open Base

let array_to_string array ~to_string =
  Printf.sprintf "[| %s |]"
    (String.concat ~sep:"; " (List.map ~f:to_string @@ Array.to_list array))

(* Abbreviations to make type signatures nicer *)
type encoders_table = (string, string list -> int Array.t) Hashtbl.t

type 'a weights_table =
  (string * string, (Int.t, 'a) Owl.Dense.Ndarray.Generic.t) Hashtbl.t

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

let align_pair encoders_table weights_table (first_row, second_row) =
  let open Dataset_utils in
  let open Dataset_utils.Infix in
  let first_encoder = encoders_table.@![first_row.taxon] in
  let second_encoder = encoders_table.@![second_row.taxon] in
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
    let first_encoded = first_encoder first_row.tokens in
    let second_encoded = second_encoder second_row.tokens in
    if flip then Alignment.align weights second_encoded first_encoded
    else Alignment.align weights first_encoded second_encoded
    (*try Alignment.align weights first_encoded second_encoded
      with Invalid_argument s ->
        let trace = Backtrace.(Exn.most_recent () |> to_string) in
        Stdio.print_endline trace;
        let weights_shape =
          array_to_string ~to_string:Int.to_string
          @@ Owl.Dense.Ndarray.Generic.shape weights
        in
        let first_enc_string =
          array_to_string ~to_string:Int.to_string first_encoded
        in
        let second_enc_string =
          array_to_string ~to_string:Int.to_string second_encoded
        in
        Printf.failwithf
          "Invalid_argument %s\nWeights shape: %s\nFirst: %s\nSecond: %s" s
          weights_shape first_enc_string second_enc_string ()*)
  in
  ((first_row, second_row), aligned)

let align_pairs encoders_table weights_table pairs =
  List.map ~f:(align_pair encoders_table weights_table) pairs

let score_pair encoders_table weights_table (first_row, second_row) =
  let open Dataset_utils in
  match String.(first_row.taxon = second_row.taxon) with
  | true -> Float.infinity
  | false ->
      let total_length =
        List.length first_row.tokens + List.length second_row.tokens
      in
      align_pair encoders_table weights_table (first_row, second_row)
      |> fun (_, (_, _, score)) ->
      Float.of_int score /. Float.of_int total_length

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
