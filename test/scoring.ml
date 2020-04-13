open Base

(** [cluster_gen [[1; 2; 3]; [4; 5]; [6]] threshold] returns a score graph
    where each sublist forms a cluster, connected by edges with scores exceeding [threshold] *)
let cluster_gen clusters threshold =
  let open QCheck.Gen in
  clusters >>= fun clusters ->
  threshold >|= fun threshold ->
  let _not_in_cluster cluster =
    let flattened_ids = List.concat clusters in
    List.filter
      ~f:(fun id ->
        let open Int in
        not (List.mem ~equal cluster id))
      flattened_ids
  in
  let open Lib.Dataset_utils in
  let graph = Hashtbl.create (module Int) in
  List.iteri clusters ~f:(fun index cluster ->
      (* Created edges for nodes that should be connected *)
      let pairs =
        ( List.cartesian_product cluster cluster
        |> List.filter ~f:(fun (x, y) -> x < y) )
        @ List.map cluster ~f:(fun x -> (x, x))
      in
      let scored_pairs =
        List.map pairs ~f:(fun (x, y) ->
            if x = y then (x, y, Float.infinity) else (x, y, threshold +. 1.0))
      in
      List.iter scored_pairs ~f:(fun (x, y, score) ->
          let x_row = { id = x; tokens = []; taxon = ""; gloss_id = index } in
          let y_row = { id = y; tokens = []; taxon = ""; gloss_id = index } in
          list_tbl_append ~key:x_row.id ~data:(y_row, score) graph;
          if x <> y then
            list_tbl_append ~key:y_row.id ~data:(x_row, score) graph);

      (* Create edges for disconnected clusters (with scores below the threshold  *)
      let other_clusters = List.drop clusters (index + 1) in
      List.iter other_clusters ~f:(fun other_cluster ->
          let pairs =
            List.cartesian_product cluster other_cluster
            |> List.filter ~f:(fun (x, y) -> x < y)
          in
          let scored_pairs =
            List.map pairs ~f:(fun (x, y) ->
                assert (x <> y);
                (x, y, threshold -. 1.0))
          in
          List.iter scored_pairs ~f:(fun (x, y, score) ->
              assert (x <> y);
              let x_row =
                { id = x; tokens = []; taxon = ""; gloss_id = index }
              in
              let y_row =
                { id = y; tokens = []; taxon = ""; gloss_id = index }
              in
              list_tbl_append ~key:x_row.id ~data:(y_row, score) graph;
              list_tbl_append ~key:y_row.id ~data:(x_row, score) graph)));

  let produced_clusters = Lib.Scoring.cluster threshold graph in
  (clusters, threshold, graph, produced_clusters)

let clusters_to_string clusters =
  String.concat ~sep:"\n"
  @@ List.map clusters ~f:(fun cluster ->
         "["
         ^ (String.concat ~sep:"; " @@ List.map cluster ~f:Int.to_string)
         ^ "]")

let normalise clusters =
  clusters
  |> List.map ~f:(List.sort ~compare:Int.compare)
  |> List.sort ~compare:(List.compare Int.compare)

let test_case_to_string (original_clusters, _threshold, graph, produced_clusters)
    =
  let open Lib.Dataset_utils in
  "Original clusters:\n"
  ^ clusters_to_string (normalise original_clusters)
  ^ "\nProduced clusters:\n"
  ^ clusters_to_string (normalise produced_clusters)
  ^ "\nGraph:\n" ^ String.concat ~sep:"\n"
  @@ List.map (Hashtbl.keys graph) ~f:(fun k ->
         let values =
           String.concat ~sep:"; "
             (List.map (Hashtbl.find_exn graph k) ~f:(fun (row, score) ->
                  Printf.sprintf "{%d; %f}" row.id score))
         in
         Printf.sprintf "%d -> [%s]" k values)

let cluster_lengths n =
  let one_cluster = [ n ] in
  let clusters_of_one = List.init n ~f:(fun _ -> 1) in
  let three_clusters =
    let length = n / 3 in
    let remainder = n - (length * 3) in
    if remainder > 0 then [ length; length; length; remainder ]
    else [ length; length; length ]
  in
  [ one_cluster; clusters_of_one; three_clusters ]
  |> List.map ~f:(List.filter ~f:(fun x -> x > 0))

let take_lengths lengths ids =
  let rec iter result remaining_ids remaining_lengths =
    match (remaining_ids, remaining_lengths) with
    | [], [] -> result
    | ids, next_length :: new_lengths ->
        let new_cluster = List.take ids next_length in
        let new_ids = List.drop ids next_length in
        iter (new_cluster :: result) new_ids new_lengths
    | _ -> failwith "Error in taking lengths"
  in
  iter [] ids lengths

let cluster_lists_gen () =
  let open QCheck.Gen in
  int_range 1 100 >|= fun n ->
  let lengths = List.random_element_exn (cluster_lengths n) in
  Stdio.printf "Lengthz: %s" (clusters_to_string [ lengths ]);
  List.range 1 (n + 1) |> List.permute |> take_lengths lengths

let make_test_case () =
  let open QCheck.Gen in
  let threshold = float in
  let clusters = cluster_lists_gen () in
  QCheck.make ~print:test_case_to_string (cluster_gen clusters threshold)

let check_clusters_equal original produced =
  Stdlib.(normalise original = normalise produced)

let test =
  QCheck.Test.make ~name:"test" ~count:10000 (make_test_case ())
    (fun (orignal_clusters, _threshold, _graph, produced_clusters) ->
      check_clusters_equal orignal_clusters produced_clusters)

let () =
  let quickcheck = QCheck_alcotest.to_alcotest test in
  let open Alcotest in
  run "Scoring" [ ("quickcheck", [ quickcheck ]) ]
