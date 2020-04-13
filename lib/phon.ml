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

let feature ~name ~t =
  let rows =
    Dataframe.filter_row
      (fun row ->
        match row.(0) with
        | String s ->
            let open String in
            s = t
        | _ -> false)
      ipa_df
    |> Dataframe.to_rows
  in
  let row =
    match rows with
    | [| row |] -> row
    | [||] -> failwith @@ Printf.sprintf "%s not found in feature table" t
    | _ ->
        failwith
        @@ Printf.sprintf "Multiple feature table entries found for %s" t
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
