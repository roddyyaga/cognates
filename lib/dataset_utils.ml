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

let base_vowels =
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
    "y";
    "ʌ";
    "ɶ";
    "œ";
    "ø";
  ]

let string_to_token_list s =
  let chars_to_collapse =
    base_vowels |> List.map ~f:(fun s -> (String.to_list s, s))
  in
  let suffixes =
    [
      [ '\203'; '\144' ] (* "ː" *);
      [ '\204'; '\175' ] (* "̯" *);
      [ '\204'; '\131' ] (* nasalised *);
      [ '\204'; '\158' ] (* lowered *);
      [ '\204'; '\132' ] (* bar *);
      [ '\204'; '\129' ] (* acute *);
      [ '\202'; '\176' ] (* aspirated *);
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
        | None -> (
            match
              List.find suffixes ~f:(fun suffix ->
                  Utils.is_list_prefix ~equal:Char.equal ~prefix:suffix
                    non_empty_chars)
            with
            | Some suffix ->
                let accum_start, accum_last =
                  (List.drop_last_exn accum, List.last_exn accum)
                in
                let new_element = accum_last ^ String.of_char_list suffix in
                let new_accum = accum_start @ [ new_element ] in
                let new_chars =
                  List.drop non_empty_chars (List.length suffix)
                in
                iter new_accum new_chars
            | None ->
                iter
                  (accum @ [ Char.to_string (List.hd_exn non_empty_chars) ])
                  (List.tl_exn chars) ) )
  in
  iter [] chars

let vowels =
  List.concat_map base_vowels ~f:(fun v ->
      [
        v;
        v ^ "ː";
        v ^ "\204\131" (* nasalised *);
        v ^ "\204\175";
        (* lowered *)
        v ^ "\204\158";
        (* non-syllabic *)
        v ^ "\202\176";
        (* aspirated *)
      ])

let vowel_pairs =
  List.cartesian_product vowels vowels
  |> List.map ~f:(fun (s1, s2) -> s1 ^ s2)
  |> Hash_set.of_list (module String)

let vowel_triples =
  let ( * ) = List.cartesian_product in
  vowels * vowels * vowels
  |> List.map ~f:(fun ((s1, s2), s3) -> s1 ^ s2 ^ s3)
  |> Hash_set.of_list (module String)

let split_fused s =
  let slist = string_to_token_list s in
  match slist with
  | [ a; b ] when Hash_set.mem vowel_pairs (a ^ b) -> [ a; b ]
  | [ a; b; c ] when Hash_set.mem vowel_triples (a ^ b ^ c) -> [ a; b; c ]
  | _ -> (
      match s with
      (* velarised lateral approximant e.g. in Albanian (although symbol sometimes used for voiceless alveolar lateral fricative *)
      | "tˡˀ" -> [ "tˡ" ]
      | "ł" -> [ "lˤ" ]
      | "kw" -> [ "k"; "w" ]
      | "ʧ" -> [ "t"; "ʃ" ]
      | "ʧː" -> [ "t"; "ʃː" ]
      | "ʧʲ" -> [ "t"; "ʃʲ" ]
      | "ʧˀ" -> [ "t"; "ʃˀ" ]
      | "ʤ" -> [ "d"; "ʒ" ]
      | "ʤː" -> [ "d"; "ʒː" ]
      | "œy" -> [ "œ"; "y" ]
      | "ɔy" -> [ "ɔ"; "y" ]
      | "ʦʲ" -> [ "t"; "sʲ" ]
      | "ɑɪ̯" -> [ "ɑ"; "ɪ̯" ]
      | "øːʌ" -> [ "øː"; "ʌ" ]
      | "ɐ̃uː" -> [ "ɐ̃"; "uː" ]
      | "ʦʰ" -> [ "t"; "sʰ" ]
      | "ʧʰ" -> [ "t"; "ʃʰ" ]
      | "ʦ" -> [ "t"; "s" ]
      | "ʦː" -> [ "t"; "sː" ]
      | "ʦˀ" -> [ "t"; "sˀ" ]
      (* "small scripted g" :-( *)
      | "iːː" -> [ "iː" ]
      | "ç" -> [ "ç" ]
      | "ʣ" -> [ "d"; "z" ]
      | "ʤ\202\176" -> [ "d"; "ʒ\202\176" ]
      | "\202\168" -> [ "t"; "ɕ" ]
      | "\202\168ʰ" -> [ "t"; "ɕʰ" ]
      | "ʨː" -> [ "t"; "ɕː" ]
      | "ʥ" -> [ "d"; "ʑ" ]
      | "ʧːʰ" -> [ "t"; "ʃːʰ" ]
      | "t͡l" -> [ "t"; "l" ]
      | "t͡n" -> [ "t"; "n" ]
      | "k͡s" -> [ "k"; "s" ]
      | "p͡s" -> [ "p"; "s" ]
      | "b͡z" -> [ "b"; "z" ]
      | "t͡ʂ" -> [ "t"; "ʂ" ]
      | "d͡ʐ" -> [ "d"; "ʐ" ]
      | "t͡ʂʰ" -> [ "t"; "ʂʰ" ]
      | "ǝɯ" -> [ "ǝ"; "ɯ" ]
      | "iǝ" -> [ "i"; "ǝ" ] (* schwa from BAI.csv *)
      (* Probably from North American Phonetic alphabet *)
      | "č" | "c\204\140" -> [ "t"; "ʃ" ]
      | "ǰ" | "j\204\140" -> [ "d͡ʒ" ]
      | "š" | "s\204\140" -> [ "ʃ" ]
      | "ž" | "z\204\140" -> [ "ʒ" ]
      | "t͜s" -> [ "t"; "s" ]
      | "a͜iː" -> [ "a"; "i\203\144" ]
      | "ə͜uː" -> [ "ə"; "u\203\144" ]
      | "p͜f" -> [ "p"; "f" ]
      | "ə͜i" -> [ "ə"; "i" ]
      | "ø̞ːʌ" -> [ "ø\203\144"; "ʌ" ]
      | "ã̄" -> [ "a\204\131" ]
      | "gː" -> [ "ɡː" ]
      | "ʊãː" -> [ "ʊ"; "ãː" ]
      | _ -> [ s ] )

let degrade = function
  | "ḷ" -> "l"
  | "ɚ" -> "ə" (* r-coloured schwa *)
  | "ɚ̃" -> "ə\204\131" (* nasalised r-coloured schwa *)
  | "m\204\163" -> "m" (* not sure *)
  | "œ̞ː" -> "œ\203\144" (* no lowering *)
  | "t̝" -> "t"
  | "ń" -> "n"
  | "ĩ́ː" -> "i\204\131\203\144"
  | "ã́ː" -> "a\204\131\203\144"
  | "é" -> "ɛ" (* e.g. Palauan "mei" *)
  | other -> other

let shouldn't_skip = function
  | "₁₂" | "₂₂" | "₄₂" | "₂₁" | "₃₁" | "₃₅" | "₅₅"
  | "₃₃" | "₂₄" | "₄₄" | "₄₃" | "₅₃" ->
      false
  | "#" -> false
  | "#ː" -> false
  | _other -> true

type word = Phone.t list

type dataset = {
  phones: (Taxon.t, (Phone.t, Phone.comparator_witness) Set.t) Hashtbl.t;
  words: (Taxon.t, word list) Hashtbl.t;
  cogs: (int, (Taxon.t * word) list) Hashtbl.t;
  concept_to_gloss_id: (string, int) Hashtbl.t;
  phone_counts: (Phone.t, int) Hashtbl.t;
}

type row_format = Basic | Iel | Ksl | Pan

let load_row =
  let open Owl.Dataframe in
  function
  | Basic -> (
      function
      | [|
          Int id;
          String taxon;
          String gloss;
          Int gloss_id;
          _ipa;
          String tokens;
          Int cog_id;
        |] ->
          (id, taxon, gloss, gloss_id, tokens, cog_id)
      | _ -> failwith "Row in dataset has unexpected length" )
  | Pan -> (
      function
      | [|
          Int id;
          String taxon;
          String gloss;
          Int gloss_id;
          Int cog_id;
          _ipa;
          String tokens;
        |] ->
          (id, taxon, gloss, gloss_id, tokens, cog_id)
      | _ -> failwith "Row in dataset has unexpected length" )
  | Iel -> (
      function
      | [|
          Int id;
          String taxon;
          String gloss;
          Int gloss_id;
          _original_form;
          _ipa;
          String tokens;
          Int cog_id;
        |] ->
          (id, taxon, gloss, gloss_id, tokens, cog_id)
      | _ -> failwith "Row in dataset has unexpected length" )
  | Ksl -> (
      function
      | [|
          Int id;
          String taxon;
          String gloss;
          Int gloss_id;
          _orthography;
          _ipa;
          String tokens;
          Int cog_id;
        |] ->
          (id, taxon, gloss, gloss_id, tokens, cog_id)
      | _ -> failwith "Row in dataset has unexpected length" )

(** New function to load dataset *)
let load_rows ?(row_format = Basic) path =
  let df = Owl.Dataframe.of_csv path in
  let rows = ref [] in
  let process_row row =
    let id, taxon, _gloss, gloss_id, tokens, _cog_id =
      load_row row_format row
    in
    let tokens =
      String.split ~on:' ' tokens
      |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
      |> List.filter ~f:shouldn't_skip
      |> List.map ~f:Phone.of_string
    in
    rows := { Row.id; tokens; taxon = Taxon.of_string taxon; gloss_id } :: !rows
  in
  Owl.Dataframe.iter_row process_row df;
  !rows |> List.rev

let load_rows_with_cog_ids ?(row_format = Basic) path =
  let df = Owl.Dataframe.of_csv path in
  let rows = ref [] in
  let process_row row =
    let id, taxon, _gloss, gloss_id, tokens, cog_id = load_row row_format row in
    let tokens =
      String.split ~on:' ' tokens
      |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
      |> List.filter ~f:shouldn't_skip
      |> List.map ~f:Phone.of_string
    in
    rows :=
      ({ Row.id; tokens; taxon = Taxon.of_string taxon; gloss_id }, cog_id)
      :: !rows
  in
  Owl.Dataframe.iter_row process_row df;
  !rows |> List.rev

let get_missing_tokens ?(row_format = Basic) path =
  let df = Owl.Dataframe.of_csv path in
  let process_row row =
    let _id, _taxon, _gloss, _gloss_id, tokens, _cog_id =
      load_row row_format row
    in
    let _ =
      String.split ~on:' ' tokens
      |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
      |> List.filter ~f:shouldn't_skip
      |> List.map ~f:(fun t ->
             try
               let _ =
                 Phon.process_phone ~t |> fst |> fun t -> Phon.row_for_phon ~t
               in
               ()
             with Failure message ->
               Stdio.print_endline (message ^ Printf.sprintf " OG %s" t))
    in
    ()
  in
  Owl.Dataframe.iter_row process_row df

(** Load a dataset from a CSV at a path *)
let load_dataset ?verbose ?(row_format = Basic) path =
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
  let process_row row =
    let _id, taxon, gloss, gloss_id, tokens, _cog_id =
      load_row row_format row
    in
    let taxon = Taxon.of_string taxon in
    let token_list =
      String.split ~on:' ' tokens
      |> List.map ~f:split_fused |> List.concat |> List.map ~f:degrade
      |> List.filter ~f:shouldn't_skip
      |> List.map ~f:Phone.of_string
    in
    List.iter token_list ~f:(fun t -> Hashtbl.incr phone_counts t ~by:1);
    let previous_set =
      match phones.@?[taxon] with
      | None -> Set.empty (module Phone)
      | Some set -> set
    in
    let new_set =
      List.fold token_list ~init:previous_set ~f:(fun set s -> Set.add set s)
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
  (*let max_number_width =
      1
      + ( Generic.max'
        @@ Generic.map (fun n -> (*String.length @@ Int.to_string*) n) weights )
    in*)
  let max_number_width = 4 in
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
