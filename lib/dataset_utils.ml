open Base
open Owl

let list_tbl_append ~key ~data table =
  Hashtbl.update table key ~f:(function
    | None -> [ data ]
    | Some previous -> data :: previous)

module Infix = struct
  let ( .@![] ) table key = Hashtbl.find_exn table key

  let ( .@?[] ) table key = Hashtbl.find table key

  let ( .@[]<- ) table key data = Hashtbl.set ~key ~data table
end

open Infix

type taxon = string

type phone = string

type word = phone list

let split_fused s =
  match
    List.mem
      [ "ea"; "eo"; "ia"; "io"; "uo"; "oi"; "ie"; "ue" ]
      s ~equal:String.( = )
  with
  | true -> s |> String.to_list |> List.map ~f:String.of_char
  | false -> (
      match s with
      | "aɪ" -> [ "a"; "ɪ" ]
      | "aɪə" -> [ "a"; "ɪ"; "ə" ]
      | "aʊ" -> [ "a"; "ʊ" ]
      | "əʊ" -> [ "ə"; "ʊ" ]
      | "eɪ" -> [ "e"; "ɪ" ]
      | "ɛə" -> [ "ɛ"; "ə" ]
      | "ɪə" -> [ "ɪ"; "ə" ]
      | "ʧ" -> [ "t"; "ʃ" ]
      | "ʧː" -> [ "t"; "ʃː" ]
      | "ʧʲ" -> [ "t"; "ʃʲ" ]
      | "ʤ" -> [ "d"; "ʒ" ]
      | "ʤː" -> [ "d"; "ʒː" ]
      | "ai" -> [ "a"; "i" ]
      | "au" -> [ "a"; "u" ]
      | "auː" -> [ "a"; "uː" ]
      | "ɑɪ" -> [ "ɑ"; "ɪ" ]
      | "ɑu" -> [ "ɑ"; "u" ]
      | "ei" -> [ "e"; "i" ]
      | "əi" -> [ "ə"; "i" ]
      | "ɛɪ" -> [ "ɛ"; "ɪ" ]
      | "iɐ" -> [ "i"; "ɐ" ]
      | "iɛ" -> [ "i"; "ɛ" ]
      | "ɪɑ" -> [ "ɪ"; "ɑ" ]
      | "ɪu" -> [ "ɪ"; "u" ]
      | "œy" -> [ "œ"; "y" ]
      | "ou" -> [ "o"; "u" ]
      | "ɔy" -> [ "ɔ"; "y" ]
      | "ʦʲ" -> [ "t"; "sʲ" ]
      | "uɐ" -> [ "u"; "ɐ" ]
      | "aːə" -> [ "aː"; "ə" ]
      | "aiː" -> [ "a"; "iː" ]
      | "ɑɪ̯" -> [ "ɑ"; "ɪ̯" ]
      | "eːə" -> [ "eː"; "ə" ]
      | "eiː" -> [ "e"; "iː" ]
      | "iɛː" -> [ "i"; "ɛː" ]
      | "oːə" -> [ "oː"; "ə" ]
      | "øːʌ" -> [ "øː"; "ʌ" ]
      | "ouː" -> [ "o"; "uː" ]
      | "øyː" -> [ "ø"; "yː" ]
      | "ɔyə" -> [ "ɔ"; "y"; "ə" ]
      | "uːa" -> [ "uː"; "a" ]
      | "ɐ̃uː" -> [ "ɐ̃"; "uː" ]
      | "ɪyaː" -> [ "ɪ"; "y"; "aː" ]
      | "ʊãː" -> [ "ʊ"; "ãː" ]
      | "ʦʰ" -> [ "t"; "sʰ" ]
      | "ʧʰ" -> [ "t"; "ʃʰ" ]
      | "ʦ" -> [ "t"; "s" ]
      (* "small scripted g" :-( *)
      | "g" -> [ "ɡ" ]
      | "gʱ" -> [ "ɡʱ" ]
      | "gː" -> [ "ɡː" ]
      | "ç" -> [ "ç" ]
      | "ʣ" -> [ "d"; "z" ]
      | "ʨ" -> [ "t"; "ɕ" ]
      | "ʥ" -> [ "d"; "ʑ" ]
      | "ʧːʰ" -> [ "t"; "ʃːʰ" ]
      | "t͡l" -> [ "t"; "l" ]
      | "t͡n" -> [ "t"; "n" ]
      | "k͡s" -> [ "k"; "s" ]
      | "p͡s" -> [ "p"; "s" ]
      (* TODO - check this is right *)
      | _ -> [ s ] )

let degrade = function "ḷ" -> "l" | other -> other

type dataset = {
  phones: (string, (phone, String.comparator_witness) Set.t) Hashtbl.t;
  words: (string, word list) Hashtbl.t;
  cogs: (int, (taxon * word) list) Hashtbl.t;
  concept_to_gloss_id: (string, int) Hashtbl.t;
  phone_counts: (string, int) Hashtbl.t;
}

type dataset_row = {
  id: int;
  tokens: string list;
  taxon: string;
  gloss_id: int;
}
[@@deriving fields]

(** New function to load dataset *)
let load_rows path =
  let df = Dataframe.of_csv path in
  let rows = ref [] in
  let process_row =
    let open Dataframe in
    function
    | [|
        Int id; String taxon; _gloss; Int gloss_id; _ipa; String tokens; _cog_id;
      |] ->
        let tokens =
          String.split ~on:' ' tokens
          |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
        in
        rows := { id; tokens; taxon; gloss_id } :: !rows
    | _ -> failwith "Row in dataset has unexpected length"
  in
  Dataframe.iter_row process_row df;
  !rows |> List.rev

(** Load a dataset from a CSV at a path *)
let load_dataset ?verbose path =
  let verbose =
    match verbose with Some true -> true | Some false | None -> false
  in
  let df = Dataframe.of_csv path in

  if verbose then (
    Stdio.print_endline @@ Owl_pretty.dataframe_to_string ~max_row:10000 df;
    let x, y = Dataframe.shape df in
    Printf.sprintf "Shape: (%d, %d)" x y |> Stdio.print_endline;

    let tokenses = Dataframe.get_col_by_name df "Tokens" in
    match tokenses with
    | String_Series strings -> Array.iter ~f:Stdio.print_endline strings
    | _ -> assert false );

  let phones = Hashtbl.create (module String) in
  let words = Hashtbl.create (module String) in
  let cognate_candidates = Hashtbl.create (module Int) in
  let concept_to_gloss_id = Hashtbl.create (module String) in
  let phone_counts = Hashtbl.create (module String) in
  let process_row =
    let open Dataframe in
    function
    | [|
        _id;
        String taxon;
        String gloss;
        Int gloss_id;
        _ipa;
        String tokens;
        _cog_id;
      |] ->
        let token_list =
          String.split ~on:' ' tokens
          |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
        in
        List.iter token_list ~f:(fun t -> Hashtbl.incr phone_counts t ~by:1);
        let previous_set =
          match phones.@?[taxon] with
          | None -> Set.empty (module String)
          | Some set -> set
        in
        let new_set =
          List.fold token_list ~init:previous_set ~f:(fun set s ->
              Set.add set s)
        in
        phones.@[taxon] <- new_set;
        let previous_list = Option.value ~default:[] @@ words.@?[taxon] in
        let new_list = token_list :: previous_list in
        words.@[taxon] <- new_list;
        let previous_cognate_candidates =
          Option.value ~default:[] @@ cognate_candidates.@?[gloss_id]
        in
        let new_cognate_candidates =
          (taxon, token_list) :: previous_cognate_candidates
        in
        cognate_candidates.@[gloss_id] <- new_cognate_candidates;
        concept_to_gloss_id.@[gloss] <- gloss_id
    | other -> Stdio.print_endline (Int.to_string @@ Array.length other)
  in
  let () = Dataframe.iter_row process_row df in
  let () =
    Hashtbl.iter_keys phones ~f:(fun taxon ->
        Stdio.print_endline taxon;
        Set.to_list phones.@![taxon]
        |> String.concat ~sep:" " |> Stdio.print_endline)
  in
  {
    phones;
    words;
    cogs = cognate_candidates;
    concept_to_gloss_id;
    phone_counts;
  }

let index_exn ~equal xs element =
  match List.findi ~f:(fun _ x -> equal x element) xs with
  | Some (i, _) -> i
  | None -> failwith "Element not found"

(** Make functions to encode phones as integers.
    Encoding starts at 1 (0 represents a deletion).*)
let phone_coders phone_set =
  let as_list = Base.Set.to_list phone_set in
  let encode token = 1 + index_exn ~equal:String.equal as_list token in
  let decode i = match i with 0 -> "-" | _ -> List.nth_exn as_list (i - 1) in
  (encode, decode)

let aligned_to_string decoder tokens =
  String.concat ~sep:" " @@ List.map ~f:decoder tokens

let cognate_lists cognates first_language second_language =
  let lookup i language =
    let open String in
    List.Assoc.find ~equal cognates.@![i] language
  in
  List.map
    ~f:(fun i -> (lookup i first_language, lookup i second_language))
    (Hashtbl.keys cognates)
  |> List.unzip

let print_weights first_decoder second_decoder weights =
  let open Dense.Ndarray in
  let shape = Generic.shape weights in
  (* TODO - check correct *)
  let width = shape.(0) in
  let height = shape.(1) in
  Stdio.print_endline "oy";
  (*let max_number_width =
      1
      + ( Generic.max'
        @@ Generic.map (fun n -> (*String.length @@ Int.to_string*) n) weights )
    in*)
  let max_number_width = 4 in
  Stdio.print_endline "yo";
  let element_pattern =
    Caml.Scanf.format_from_string
      ("%" ^ Int.to_string max_number_width ^ "d")
      "%d"
  in
  let first_line =
    List.range 0 width |> List.map ~f:first_decoder |> String.concat ~sep:"  "
  in
  Stdio.print_endline @@ "  " ^ first_line;
  List.(
    iter (range 0 height) ~f:(fun y ->
        Stdio.printf "%s" (second_decoder y);
        iter (range 0 width) ~f:(fun x ->
            Stdio.printf element_pattern @@ Generic.get weights [| x; y |]);
        Stdio.print_endline ""))
