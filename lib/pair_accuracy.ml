open Base

type row_with_gloss_id = { row: Bcubed_scores.row; gloss_id: int }

let zip4_exn xs ys zs ws =
  let rec iter accum xs ys zs ws =
    match (xs, ys, zs, ws) with
    | [], [], [], [] -> accum
    | x :: xs, y :: ys, z :: zs, w :: ws ->
        iter ((x, y, z, w) :: accum) xs ys zs ws
    | _ -> failwith "Unequal lengths"
  in
  iter [] xs ys zs ws |> List.rev

let rows_of_dataframe df ~reference_column ~given_column ~id_column =
  let open Bcubed_scores in
  zip4_exn
    (column_as_int_list df reference_column)
    (column_as_int_list df given_column)
    (column_as_int_list df id_column)
    (column_as_int_list df "GlossID")
  |> List.map ~f:(fun (reference_id, given_id, row_id, gloss_id) ->
         { row = { Bcubed_scores.reference_id; given_id; row_id }; gloss_id })

let accuracies rows =
  let gloss_groups = Dict.create (module Int) in
  List.iter rows ~f:(fun row -> Dict.cons gloss_groups row.gloss_id row);
  let cognate_accuracy =
    Dict.data gloss_groups
    |> List.map ~f:(fun gloss_group ->
           List.map (Utils.unique_pairs gloss_group)
             ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
               if p1.given_id = p2.given_id && p1.reference_id = p2.reference_id
               then 1.0
               else 0.0))
    |> List.concat |> Utils.float_mean
  in
  let non_cog_accuracy =
    Dict.data gloss_groups
    |> List.map ~f:(fun gloss_group ->
           List.map (Utils.unique_pairs gloss_group)
             ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
               if
                 p1.given_id <> p2.given_id
                 && p1.reference_id <> p2.reference_id
               then 1.0
               else 0.0))
    |> List.concat |> Utils.float_mean
  in
  (cognate_accuracy, non_cog_accuracy)

let cs rows =
  let gloss_groups = Dict.create (module Int) in
  List.iter rows ~f:(fun row -> Dict.cons gloss_groups row.gloss_id row);
  let a =
    Dict.data gloss_groups
    |> List.map ~f:(fun gloss_group ->
           List.map (Utils.unique_pairs gloss_group)
             ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
               if p1.reference_id = p2.reference_id then 1.0 else 0.0))
    |> List.concat |> Utils.float_sum
  in

  let b =
    Dict.data gloss_groups
    |> List.map ~f:(fun gloss_group ->
           List.map (Utils.unique_pairs gloss_group)
             ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
               if p1.reference_id <> p2.reference_id then 1.0 else 0.0))
    |> List.concat |> Utils.float_sum
  in
  (a, b)
