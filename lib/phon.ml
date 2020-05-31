open Base
open Owl

let ipa_df =
  Dataframe.of_csv ~sep:','
    ~types:
      [|
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
        "s";
      |]
    "/home/roddy/iii/project/code/ipa_all.csv"

type change =
  | Nasalised
  | Voiceless
  | Aspirated
  | Syllabic
  | NonSyllabic
  | Ignored

let suffixes_and_changes =
  [
    ("\202\176", Aspirated);
    ("\202\177", Aspirated);
    ("\204\165", Voiceless);
    ("\204\131", Nasalised);
    ("\204\175", NonSyllabic);
    ("\204\129", Ignored);
  ]

let rec changes_from_suffixes (t, changes) =
  match
    List.find suffixes_and_changes ~f:(fun (suffix, _change) ->
        String.is_suffix ~suffix t)
  with
  | Some (suffix, change) ->
      changes_from_suffixes (String.chop_suffix_exn ~suffix t, change :: changes)
  | None -> (t, changes)

let one_token_change = function
  | "g" -> "ɡ"
  | "ǝ" -> "ə" (* just a different encoding? *)
  | other -> other

let process_phone ~t =
  let t, changes = changes_from_suffixes (t, []) in
  match t with
  | "ĩ" -> ("i", Nasalised :: changes)
  | "ã" -> ("a", Nasalised :: changes)
  | "õ" -> ("o", Nasalised :: changes)
  | "ẽ" -> ("e", Nasalised :: changes)
  | "ɿ" ->
      ("z", Syllabic :: changes) (* https://en.wiktionary.org/wiki/%C9%BF *)
  | other -> (other, changes)

let phone_exists ~t =
  let t, _change = process_phone ~t in
  let rows =
    Dataframe.filter_row
      (fun row ->
        match row.(0) with String s -> String.(s = t) | _ -> assert false)
      ipa_df
    |> Dataframe.to_rows
  in
  match rows with
  | [| _row |] -> true
  | [||] -> false
  | _multiple ->
      Stdio.printf "Warning: multiple feature table entries found for %s\n" t;
      true

let row_cache = Hashtbl.create (module String)

let row_for_phon ~t =
  let t = one_token_change t in
  let open Dict.Infix in
  match row_cache.@?[t] with
  | Some row -> row
  | None -> (
      let rows =
        Dataframe.filter_row
          (fun row ->
            match row.(0) with String s -> String.(s = t) | _ -> assert false)
          ipa_df
        |> Dataframe.to_rows
      in
      match rows with
      | [| row |] ->
          row_cache.@[t] <- row;
          row
      | [||] -> failwith @@ Printf.sprintf "%s not found in feature table" t
      | multiple ->
          Stdio.printf "Warning: multiple feature table entries found for %s\n"
            t;
          multiple.(0) )

let difference_count ~t1 ~t2 =
  let open Dataframe in
  let elements_equal = function
    | String s1, String s2 -> String.equal s1 s2
    | _ -> assert false
  in
  let t1, change_t1 = process_phone ~t:t1 in
  let t2, change_t2 = process_phone ~t:t2 in
  let change_count = if Stdlib.(change_t1 = change_t2) then 0 else 1 in
  let element_count =
    List.zip_exn
      (List.tl_exn @@ Array.to_list (row_for_phon ~t:t1))
      (List.tl_exn @@ Array.to_list (row_for_phon ~t:t2))
    |> List.filter ~f:(fun (x, y) -> not @@ elements_equal (x, y))
    |> List.length
  in
  element_count + change_count

let identical ~t1 ~t2 = difference_count ~t1 ~t2 = 0

let identical_bar_one ~t1 ~t2 = difference_count ~t1 ~t2 = 1

let feature_encodings =
  [
    ("voi", (Voiceless, false));
    ("sg", (Aspirated, true));
    ("nas", (Nasalised, true));
    ("syl", (Syllabic, true));
    ("syl", (NonSyllabic, false));
  ]

let feature ~name ~t =
  let t, changes = process_phone ~t in
  let answer_from_changes =
    match List.Assoc.find feature_encodings name ~equal:String.equal with
    | Some (feature, value) -> (
        match List.mem changes feature ~equal:Poly.equal with
        | true -> Some value
        | false -> None )
    | None -> None
  in
  match answer_from_changes with
  | Some answer -> Some answer
  | None -> (
      let row = row_for_phon ~t in
      let column_index = Dataframe.head_to_id ipa_df name in
      match row.(column_index) with
      | String "+" -> Some true
      | String "-" -> Some false
      | String "0" | Int 0 -> None
      | _ ->
          failwith
          @@ Printf.sprintf
               "Unexpected feature value found for string %s and feature name \
                %s"
               t name )

let syl t = feature ~name:"syl" ~t

let same_feature_value x y =
  match (x, y) with
  | Some true, Some true | Some false, Some false -> true
  | _ -> false

let feature_to_string = function
  | Some true -> "+"
  | Some false -> "-"
  | None -> "0"
