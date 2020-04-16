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

type change = WasNasalised | WasVoiceless | WasAspirated

let process_phone ~t =
  match t with
  | "ĩ" -> ("i", Some WasNasalised)
  | "ã" -> ("a", Some WasNasalised)
  | "õ" -> ("o", Some WasNasalised)
  | "b̥" -> ("b", Some WasVoiceless)
  | "d̥" -> ("d", Some WasVoiceless)
  | "aʰ" -> ("a", Some WasAspirated)
  | "ʃʰ" -> ("ʃ", Some WasAspirated)
  | "sʰ" -> ("s", Some WasAspirated)
  | "ouʰ" -> ("u", Some WasAspirated)
  | "ɔʰ" -> ("ɔ", Some WasAspirated)
  | "ɛʰ" -> ("ɛ", Some WasAspirated)
  | "ʏʰ" -> ("ʏ", Some WasAspirated)
  | "bʱ" -> ("b", Some WasAspirated)
  | "dʱ" -> ("d", Some WasAspirated)
  | "ɡʱ" -> ("ɡ", Some WasAspirated)
  | "ʃːʰ" -> ("ʃː", Some WasAspirated)
  | other -> (other, None)

let phone_exists ~t =
  let t, _change = process_phone ~t in
  let rows =
    Dataframe.filter_row
      (fun row ->
        match row.(0) with
        | String s ->
            (*             Stdio.printf "%s %s\n" s t; *)
            String.(s = t)
        | _ -> assert false)
      ipa_df
    |> Dataframe.to_rows
  in
  match rows with
  | [| _row |] -> true
  | [||] -> false
  | _ ->
      failwith @@ Printf.sprintf "Multiple feature table entries found for %s" t

let row_cache = Hashtbl.create (module String)

let feature ~name ~t =
  let t, _change = process_phone ~t in
  let open Dataset_utils.Infix in
  let row =
    match row_cache.@?[t] with
    | Some row -> row
    | None -> (
        let rows =
          Dataframe.filter_row
            (fun row ->
              match row.(0) with
              | String s -> String.(s = t)
              | _ -> assert false)
            ipa_df
          |> Dataframe.to_rows
        in
        match rows with
        | [| row |] ->
            row_cache.@[t] <- row;
            row
        | [||] -> failwith @@ Printf.sprintf "%s not found in feature table" t
        | _ ->
            failwith
            @@ Printf.sprintf "Multiple feature table entries found for %s" t )
  in
  let column_index = Dataframe.head_to_id ipa_df name in
  match row.(column_index) with
  | String "+" -> Some true
  | String "-" -> Some false
  | String "0" | Int 0 -> None
  | _ ->
      failwith
      @@ Printf.sprintf
           "Unexpected feature value found for string %s and feature name %s" t
           name

let syl t = feature ~name:"syl" ~t

let same_feature_value x y =
  match (x, y) with
  | Some true, Some true | Some false, Some false -> true
  | _ -> false

let feature_to_string = function
  | Some true -> "+"
  | Some false -> "-"
  | None -> "0"
