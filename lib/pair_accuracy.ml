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

let rows_of_dataframe df ~gloss_column ~reference_column ~given_column
    ~id_column =
  let open Bcubed_scores in
  zip4_exn
    (column_as_int_list df reference_column)
    (column_as_int_list df given_column)
    (column_as_int_list df id_column)
    (column_as_int_list df gloss_column)
  |> List.map ~f:(fun (reference_id, given_id, row_id, gloss_id) ->
         { row = { Bcubed_scores.reference_id; given_id; row_id }; gloss_id })

let accuracies rows =
  let gloss_groups = Dict.create (module Int) in
  List.iter rows ~f:(fun row -> Dict.cons gloss_groups row.gloss_id row);
  let cogs =
    Dict.data gloss_groups
    |> List.map ~f:(fun gloss_group ->
           List.filter (Utils.unique_pairs gloss_group)
             ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
               p1.reference_id = p2.reference_id))
    |> List.concat
  in
  let non_cogs =
    Dict.data gloss_groups
    |> List.map ~f:(fun gloss_group ->
           List.filter (Utils.unique_pairs gloss_group)
             ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
               p1.reference_id <> p2.reference_id))
    |> List.concat
  in
  Stdio.printf "Cog %d Nocog %d\n" (List.length cogs) (List.length non_cogs);
  let cognate_accuracy =
    cogs
    |> List.map ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
           if p1.given_id = p2.given_id then (
             Stdio.printf "Rite %d\n" p1.row_id;
             1.0 )
           else 0.0)
    |> Utils.float_mean
  in
  let non_cog_accuracy =
    non_cogs
    |> List.map ~f:(fun ({ row = p1; _ }, { row = p2; _ }) ->
           if p1.given_id <> p2.given_id then 1.0 else 0.0)
    |> Utils.float_mean
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
