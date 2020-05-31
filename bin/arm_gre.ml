open Base

let data_path = "/home/roddy/iii/project/code/data/ARM_GRE.csv"

let get_name path =
  path |> String.split ~on:'/' |> List.last_exn |> String.split ~on:'.'
  |> List.hd_exn

let run initialiser ?row_format ?params_callback ~smoothing ~base_cognate_prob
    data_path name iterations () =
  let open Lib in
  let open Lib.Types in
  let open Dict.Infix in
  let base_df = Owl.Dataframe.of_csv data_path in
  let Dataset_utils.{ phones; _ } =
    Dataset_utils.load_dataset ?row_format ~verbose:false data_path
  in
  let rows = Dataset_utils.load_rows ?row_format data_path in
  Stdio.print_endline "Getting taxons";
  let taxons = Initialisation.all_taxons rows in

  Stdio.print_endline "Getting encoders";
  let encoders = Initialisation.all_encoders taxons phones in
  let decoders = Initialisation.all_decoders taxons phones in

  Stdio.print_endline "Getting weights tables";
  let weights_tables = Initialisation.new_weights_tables taxons phones in

  Stdio.print_endline "Getting word encoders";

  let theta = Em.Theta_family.create () in
  let alpha = Em.Alpha_family.create () in

  Em.initialise_parameters theta alpha
    (List.map taxons ~f:(fun t -> (t, Set.to_list phones.@![t])))
    initialiser;

  let base_cognate_probs =
    Dict.create (module Sorted_pair.Hashable_t (Taxon))
  in
  let row_pairs = Em.possible_pairs rows in
  let expect =
    Em.expectations ~explain:false encoders decoders row_pairs weights_tables
      ~default_base_cognate_prob:base_cognate_prob
  in
  let maximise = Em.maximise ~smoothing in
  let iterate n base_cognate_probs alpha theta =
    let dist = expect ~base_cognate_probs alpha theta in
    Em.print_dist dist;
    let base_cognate_probs = Em.infer_taxon_pair_cognate_probs dist in
    Stdio.print_endline "Maximising";
    let alpha, theta = maximise dist in
    (match params_callback with Some f -> f alpha theta | None -> ());

    Stdio.print_endline "Making score graph";
    let score_graph = Em.score_graph rows dist in
    Stdio.print_endline "Getting bcubed scores";
    List.iter (List.range 0 20) ~f:(fun i ->
        let clusters =
          Scoring.cluster
            Float.((0.01 - 0.00000001) * of_int i / 20.0)
            score_graph
        in
        let df = Owl.Dataframe.copy base_df in
        let new_df =
          Scoring.set_cognates_from_clusters ?row_format df clusters
        in
        (* CogID NewCogID  *)
        let scoring_rows =
          Pair_accuracy.rows_of_dataframe new_df ~reference_column:"CogID"
            ~given_column:"NewCogID" ~id_column:"ID"
        in
        let cog_acc, non_acc = scoring_rows |> Pair_accuracy.accuracies in
        Stdio.printf "%d %d %.3f %.3f \n" i n cog_acc non_acc;
        Owl.Dataframe.to_csv new_df (name ^ Printf.sprintf "_%d_%d.csv" i n));
    Stdio.print_endline "";
    (base_cognate_probs, alpha, theta)
  in
  let _final_base_cognate_probs, _final_alpha, _final_theta =
    List.fold
      (List.init iterations ~f:Int.succ)
      ~f:(fun (base_cognate_probs, alpha, theta) n ->
        iterate n base_cognate_probs alpha theta)
      ~init:(base_cognate_probs, alpha, theta)
  in
  ()

let () =
  Stdio.print_endline data_path;
  run ~row_format:Basic Lib.Run_em.basic_initialiser ~smoothing:0.0001
    ~base_cognate_prob:0.5 data_path (get_name data_path) 7 ()
