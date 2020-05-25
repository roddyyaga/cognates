open Base
open Types

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

let string_to_token_list s =
  let chars_to_collapse =
    [
      ([ '\203'; '\144' ], "ː");
      ([ '\201'; '\170' ], "ɪ");
      ([ '\202'; '\138' ], "ʊ");
      ([ '\201'; '\153' ], "ə");
      ([ '\201'; '\155' ], "ɛ");
      ([ '\201'; '\145' ], "ɑ");
      ([ '\201'; '\144' ], "ɐ");
      ([ '\202'; '\138' ], "ʊ");
      ([ '\201'; '\168' ], "ɨ");
      ([ '\201'; '\148' ], "ɔ");
      ([ '\195'; '\166' ], "æ");
    ]
  in
  let chars = String.to_list s in
  let rec iter accum chars =
    match chars with
    | [] -> accum
    | non_empty_chars -> (
        match
          List.find chars_to_collapse ~f:(fun (prefix, _prefix_as_string) ->
              Utils.is_list_prefix ~equal:Char.equal ~prefix non_empty_chars)
        with
        | Some (prefix_chars, prefix_string) ->
            iter
              (accum @ [ prefix_string ])
              (List.drop non_empty_chars (List.length prefix_chars))
        | None ->
            iter
              (accum @ [ Char.to_string (List.hd_exn non_empty_chars) ])
              (List.tl_exn chars) )
  in
  iter [] chars

let vowels =
  [
    "a";
    "e";
    "i";
    "o";
    "u";
    "ɪ";
    "ʊ";
    "ə";
    "ɛ";
    "ɑ";
    "ɐ";
    "ʊ";
    "ɨ";
    "ɔ";
    "æ";
  ]

let vowel_pairs =
  List.cartesian_product vowels vowels |> List.map ~f:(fun (s1, s2) -> s1 ^ s2)

let split_fused s =
  let slist = string_to_token_list s in
  match slist with
  | [ a; b ] when List.mem vowel_pairs (a ^ b) ~equal:String.equal -> [ a; b ]
  | [ a; b; "ː" ] when List.mem vowel_pairs (a ^ b) ~equal:String.equal ->
      [ a; b ^ "ː" ]
  | [ a; "ː"; b ] when List.mem vowel_pairs (a ^ b) ~equal:String.equal ->
      [ a ^ "ː"; b ]
  | _ -> (
      match s with
      | "aɪə" -> [ "a"; "ɪ"; "ə" ]
      | "uiu" -> [ "u"; "i"; "u" ]
      | "oie" -> [ "o"; "i"; "e" ]
      | "ʊãː" -> [ "ʊ"; "ãː" ]
      | "yi" -> [ "y"; "i" ]
      | "yɨ" -> [ "y"; "ɨ" ]
      | "ʧ" -> [ "t"; "ʃ" ]
      | "ʧː" -> [ "t"; "ʃː" ]
      | "ʧʲ" -> [ "t"; "ʃʲ" ]
      | "ʤ" -> [ "d"; "ʒ" ]
      | "ʤː" -> [ "d"; "ʒː" ]
      | "œy" -> [ "œ"; "y" ]
      | "ɔy" -> [ "ɔ"; "y" ]
      | "ʦʲ" -> [ "t"; "sʲ" ]
      | "ɑɪ̯" -> [ "ɑ"; "ɪ̯" ]
      | "eːə" -> [ "eː"; "ə" ]
      | "øːʌ" -> [ "øː"; "ʌ" ]
      | "øyː" -> [ "ø"; "yː" ]
      | "ɔyə" -> [ "ɔ"; "y"; "ə" ]
      | "ɐ̃uː" -> [ "ɐ̃"; "uː" ]
      | "ɪyaː" -> [ "ɪ"; "y"; "aː" ]
      | "ʦʰ" -> [ "t"; "sʰ" ]
      | "ʧʰ" -> [ "t"; "ʃʰ" ]
      | "ʦ" -> [ "t"; "s" ]
      | "ʦː" -> [ "t"; "sː" ]
      (* "small scripted g" :-( *)
      | "iːː" -> [ "iː" ]
      | "g" -> [ "ɡ" ]
      | "gʱ" -> [ "ɡʱ" ]
      | "gː" -> [ "ɡː" ]
      | "ç" -> [ "ç" ]
      | "ʣ" -> [ "d"; "z" ]
      | "ʨ" -> [ "t"; "ɕ" ]
      | "ʨː" -> [ "t"; "ɕː" ]
      | "ʥ" -> [ "d"; "ʑ" ]
      | "ʧːʰ" -> [ "t"; "ʃːʰ" ]
      | "t͡l" -> [ "t"; "l" ]
      | "t͡n" -> [ "t"; "n" ]
      | "k͡s" -> [ "k"; "s" ]
      | "p͡s" -> [ "p"; "s" ]
      | "b͡z" -> [ "b"; "z" ]
      (* TODO - check this is right *)
      | _ -> [ s ] )

let degrade = function "ḷ" -> "l" | other -> other

type word = Phone.t list

type dataset = {
  phones: (Taxon.t, (Phone.t, Phone.comparator_witness) Set.t) Hashtbl.t;
  words: (Taxon.t, word list) Hashtbl.t;
  cogs: (int, (Taxon.t * word) list) Hashtbl.t;
  concept_to_gloss_id: (string, int) Hashtbl.t;
  phone_counts: (Phone.t, int) Hashtbl.t;
}

let load_row column_count =
  let open Owl.Dataframe in
  match column_count with
  | 7 -> (
      function
      | [|
          Int id;
          String taxon;
          _gloss;
          Int gloss_id;
          _ipa;
          String tokens;
          _cog_id;
        |] ->
          (id, taxon, gloss_id, tokens)
      | _ -> failwith "Row in dataset has unexpected length" )
  | 8 -> (
      function
      | [|
          Int id;
          String taxon;
          _gloss;
          Int gloss_id;
          _ids_id;
          _ipa;
          String tokens;
          _cog_id;
        |] ->
          (id, taxon, gloss_id, tokens)
      | _ -> failwith "Row in dataset has unexpected length" )
  | _ -> failwith "Unsupported column count"

(** New function to load dataset *)
let load_rows path =
  let df = Owl.Dataframe.of_csv path in
  let rows = ref [] in
  let process_row =
    let open Owl.Dataframe in
    function
    | [|
        Int id; String taxon; _gloss; Int gloss_id; _ipa; String tokens; _cog_id;
      |] ->
        let tokens =
          String.split ~on:' ' tokens
          |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
          |> List.map ~f:Phone.of_string
        in
        rows :=
          { Row.id; tokens; taxon = Taxon.of_string taxon; gloss_id } :: !rows
    | _ -> failwith "Row in dataset has unexpected length"
  in
  Owl.Dataframe.iter_row process_row df;
  !rows |> List.rev

let get_missing_tokens path =
  let df = Owl.Dataframe.of_csv path in
  let process_row =
    let open Owl.Dataframe in
    function
    | [|
        Int _id;
        String _taxon;
        _gloss;
        Int _gloss_id;
        _ipa;
        String tokens;
        _cog_id;
      |] ->
        let _ =
          try
            String.split ~on:' ' tokens
            |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
            |> List.map ~f:(fun t ->
                   Phon.process_phone ~t |> fst |> fun t -> Phon.row_for_phon ~t)
          with Failure message ->
            Stdio.print_endline message;
            []
        in
        ()
    | _ -> failwith "Row in dataset has unexpected length"
  in
  Owl.Dataframe.iter_row process_row df

(** Load a dataset from a CSV at a path *)
let load_dataset ?verbose path =
  let verbose =
    match verbose with Some true -> true | Some false | None -> false
  in
  let df = Owl.Dataframe.of_csv path in

  if verbose then (
    Stdio.print_endline @@ Owl_pretty.dataframe_to_string ~max_row:10000 df;
    let x, y = Owl.Dataframe.shape df in
    Printf.sprintf "Shape: (%d, %d)" x y |> Stdio.print_endline;

    let tokenses = Owl.Dataframe.get_col_by_name df "Tokens" in
    match tokenses with
    | String_Series strings -> Array.iter ~f:Stdio.print_endline strings
    | _ -> assert false );

  let phones = Hashtbl.create (module Taxon) in
  let words = Hashtbl.create (module Taxon) in
  let cognate_candidates = Hashtbl.create (module Int) in
  let concept_to_gloss_id = Hashtbl.create (module String) in
  let phone_counts = Hashtbl.create (module Phone) in
  let process_row =
    let open Owl.Dataframe in
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
        let taxon = Taxon.of_string taxon in
        let token_list =
          String.split ~on:' ' tokens
          |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
          |> List.map ~f:Phone.of_string
        in
        List.iter token_list ~f:(fun t -> Hashtbl.incr phone_counts t ~by:1);
        let previous_set =
          match phones.@?[taxon] with
          | None -> Set.empty (module Phone)
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
  let () = Owl.Dataframe.iter_row process_row df in
  let () =
    Hashtbl.iter_keys phones ~f:(fun taxon ->
        Stdio.print_endline (Taxon.to_string taxon);
        Set.to_list phones.@![taxon]
        |> List.map ~f:Phone.to_string
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
  let encode token =
    match Phone.(token = null) with
    | true -> 0
    | false -> 1 + index_exn ~equal:Phone.( = ) as_list token
  in
  let decode i =
    match i with 0 -> Phone.null | _ -> List.nth_exn as_list (i - 1)
  in
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
  let open Owl.Dense.Ndarray in
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
