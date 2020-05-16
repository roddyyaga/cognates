open Base

type row = { reference_id: int; given_id: int; row_id: int }

let column_as_int_list df column_name =
  let open Owl in
  Dataframe.get_col_by_name df column_name
  |> Dataframe.unpack_int_series |> Array.to_list

let zip3_exn xs ys zs = List.map3_exn xs ys zs ~f:(fun x y z -> (x, y, z))

let rows_of_dataframe df ~reference_column ~given_column ~id_column =
  zip3_exn
    (column_as_int_list df reference_column)
    (column_as_int_list df given_column)
    (column_as_int_list df id_column)
  |> List.map ~f:(fun (reference_id, given_id, row_id) ->
         { reference_id; given_id; row_id })

let cluster_sets get_id rows =
  let cognate_id_table = Hashtbl.create (module Int) in
  List.iter rows ~f:(fun row ->
      Dataset_utils.list_tbl_append cognate_id_table ~key:(get_id row)
        ~data:row.row_id);
  let cognate_id_table =
    Hashtbl.map cognate_id_table ~f:(Set.of_list (module Int))
  in
  let result = Hashtbl.create (module Int) in
  Hashtbl.iteri cognate_id_table ~f:(fun ~key ~data ->
      ignore key;
      Set.iter data ~f:(fun row_id -> Hashtbl.add_exn result ~key:row_id ~data));
  result

let reference_sets = cluster_sets (fun row -> row.reference_id)

let given_sets = cluster_sets (fun row -> row.given_id)

let word_precision ~reference_sets ~given_sets row_id =
  let open Dataset_utils.Infix in
  let numerator =
    Set.length @@ Set.inter reference_sets.@![row_id] given_sets.@![row_id]
  in
  let denominator = Set.length @@ given_sets.@![row_id] in
  Float.(of_int numerator / of_int denominator)

let word_recall ~reference_sets ~given_sets row_id =
  let open Dataset_utils.Infix in
  let numerator =
    Set.length @@ Set.inter reference_sets.@![row_id] given_sets.@![row_id]
  in
  let denominator = Set.length @@ reference_sets.@![row_id] in
  Float.(of_int numerator / of_int denominator)

let f_score precision recall =
  Float.(2.0 * (precision * recall) / (precision + recall))

let score rows =
  let reference_sets = reference_sets rows in
  let given_sets = given_sets rows in
  List.map rows ~f:(fun row ->
      let precision = word_precision ~reference_sets ~given_sets row.row_id in
      let recall = word_recall ~reference_sets ~given_sets row.row_id in
      let f_score = f_score precision recall in
      (precision, recall, f_score))

let average scores =
  let precision_sum, recall_sum, f_sum =
    List.fold ~init:(0.0, 0.0, 0.0)
      ~f:(fun (x, y, z) (a, b, c) -> Float.(x + a, y + b, z + c))
      scores
  in
  let normalise x = Float.(x / of_int (List.length scores)) in
  (normalise precision_sum, normalise recall_sum, normalise f_sum)
