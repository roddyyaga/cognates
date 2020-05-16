open Base

let array_to_string array ~to_string =
  Printf.sprintf "[| %s |]"
    (String.concat ~sep:"; " (List.map ~f:to_string @@ Array.to_list array))

(* Abbreviations to make type signatures nicer *)
type encoders_table = (string, string list -> int Array.t) Hashtbl.t

let possible_pairs rows =
  let gloss_groups = Hashtbl.create (module Int) in
  let open Dataset_utils in
  let () =
    List.iter rows ~f:(fun row ->
        list_tbl_append ~key:row.gloss_id ~data:row gloss_groups)
  in
  Hashtbl.data gloss_groups
  |> List.map ~f:(fun xs -> List.cartesian_product xs xs)
  |> List.concat

let encode encoders_table row =
  let open Dataset_utils in
  let open Dataset_utils.Infix in
  let encoder = encoders_table.@![row.taxon] in
  encoder row.tokens

let align_pair encoders_table weights_table (first_row, second_row) =
  let open Dataset_utils in
  let open Dataset_utils.Infix in
  let weights, flip =
    match weights_table.@?[(first_row.taxon, second_row.taxon)] with
    | Some weights -> (weights, false)
    | None -> (
        match weights_table.@?[(second_row.taxon, first_row.taxon)] with
        | Some weights -> (weights, true)
        | None ->
            Printf.failwithf "No weights found for %s and %s" first_row.taxon
              second_row.taxon () )
  in
  let aligned =
    let first_encoded = encode encoders_table first_row in
    let second_encoded = encode encoders_table second_row in
    if flip then Alignment.align weights second_encoded first_encoded
    else Alignment.align weights first_encoded second_encoded
  in
  if flip then ((second_row, first_row), aligned)
  else ((first_row, second_row), aligned)

let align_pairs encoders_table weights_table pairs =
  List.map ~f:(align_pair encoders_table weights_table) pairs

let score_pair encoders_table weights_table (first_row, second_row) =
  let open Dataset_utils in
  match String.(first_row.taxon = second_row.taxon) with
  | true ->
      if List.equal String.equal first_row.tokens second_row.tokens then
        (* A word is always cognate with itself *)
        Float.infinity
        (* If two distinct words have the same language and same gloss they cannot be cognate *)
      else -.Float.infinity
  | false ->
      let total_length =
        List.length first_row.tokens + List.length second_row.tokens
      in
      align_pair encoders_table weights_table (first_row, second_row)
      |> fun (_, (_, _, score)) -> score /. Float.of_int total_length

(** Make a table mapping row ids to possible cognates and associated scores. *)
let score_graph encoders_table weights_table rows =
  let open Dataset_utils in
  let result = Hashtbl.create (module Int) in
  let () =
    List.iter (possible_pairs rows) ~f:(fun (first_row, second_row) ->
        list_tbl_append ~key:first_row.id
          ~data:
            ( second_row,
              score_pair encoders_table weights_table (first_row, second_row) )
          result)
  in
  result

(** Make a list of pairs of rows that might be cognate (excluding "self-cognate" pairs),
    with their alignments as given by [weights_tables] *)
let align_rows encoders_table weights_tables rows =
  List.filter_map (possible_pairs rows) ~f:(fun (first_row, second_row) ->
      match String.(first_row.taxon = second_row.taxon) with
      | true -> None
      | false ->
          let result =
            align_pair encoders_table weights_tables (first_row, second_row)
            |> fun ((first_row, second_row), (scores, pointers, score)) ->
            let total_length =
              List.length first_row.tokens + List.length second_row.tokens
            in
            let normalised_score = score /. Float.of_int total_length in
            (first_row, second_row, (scores, pointers, normalised_score))
          in
          Some result)

(** Aligns pairs and then discards alignment information other than final score *)
let score_pairs encoders_table weights_table pairs =
  align_pairs encoders_table weights_table pairs
  |> List.map ~f:(fun (pair, (_, _, score)) -> (pair, score))

let int_list_to_string xs =
  String.concat ~sep:"; " @@ List.map xs ~f:Int.to_string

(** Given rows, a score graph and a threshold, make clusters.

    Scores must be strictly greater than the threshold. *)
let cluster threshold score_graph =
  (* Make a copy because we mutate the graph *)
  let working_graph = Hashtbl.copy score_graph in
  let open Dataset_utils in
  let open Dataset_utils.Infix in
  let rec dfs result stack =
    match stack with
    | [] -> result
    | row_id :: row_ids -> (
        match working_graph.@?[row_id] with
        | Some scored_neighbours ->
            Hashtbl.remove working_graph row_id;
            let next_nodes =
              List.filter scored_neighbours ~f:(fun (neighbour, score) ->
                  Float.(score > threshold)
                  (* Only explore nodes that haven't already been removed *)
                  && Hashtbl.mem working_graph neighbour.id)
            in
            dfs (row_id :: result)
              ( List.map next_nodes ~f:(fun (neighbour, _score) -> neighbour.id)
              @ row_ids )
        | None -> dfs result row_ids )
  in
  let rec iter clusters_so_far =
    match Hashtbl.choose working_graph with
    | Some (row_id, _neighbours) -> iter (dfs [] [ row_id ] :: clusters_so_far)
    | None -> clusters_so_far
  in
  iter []

(* Update a dataframe by setting cognate ids *)
let set_cognates_from_clusters df clusters =
  let open Dataset_utils.Infix in
  let id_to_new_cogid = Hashtbl.create (module Int) in
  List.iteri clusters ~f:(fun i cluster ->
      List.iter cluster ~f:(fun id -> id_to_new_cogid.@[id] <- i));
  let open Owl in
  (*   Dataframe.set_heads df *)
  (*     (Array.append (Dataframe.get_heads df) [| "NewCogID" |]); *)
  let new_column_reversed = ref [] in
  Dataframe.iter_row
    (function
      | [| Int id; _taxon; _gloss; _gloss_id; _ipa; _tokens; _original_cog_id |]
        ->
          new_column_reversed := id_to_new_cogid.@![id] :: !new_column_reversed
      | _ -> failwith "Unexpected row shape in dataframe")
    df;
  let new_column =
    !new_column_reversed |> List.rev |> Array.of_list
    |> Dataframe.pack_int_series
  in
  Dataframe.append_col df new_column "NewCogID";
  df

let all_finite_scores score_graph =
  score_graph |> Hashtbl.data |> List.concat
  |> List.map ~f:(fun (_row, score) -> score)
  |> List.filter ~f:Float.is_finite

let get_rows concept_to_gloss_id rows taxon gloss =
  List.filter rows ~f:(fun row ->
      let open Dataset_utils.Infix in
      String.(row.Dataset_utils.taxon = taxon)
      && row.Dataset_utils.gloss_id = concept_to_gloss_id.@![gloss])

let get_row concept_to_gloss_id rows taxon gloss =
  match get_rows concept_to_gloss_id rows taxon gloss with
  | [] -> Error "No rows found"
  | [ r ] -> Ok r
  | _ -> Error "Multiple rows found"
