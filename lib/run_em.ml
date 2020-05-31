open Base
open Types
open Dict.Infix

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

let make_initialiser ~different ~null ~syl_match ~two_match ~one_match
    ~identical t1 t2 =
  if Phone.(t1 = null || t2 = null) then Float.of_int null
  else
    let t1, t2 = (Phone.to_string t1, Phone.to_string t2) in
    (*   Stdio.printf "%s %s %d\n" t1 t2 (difference_count ~t1 ~t2); *)
    if String.(t1 = t2) then Float.of_int identical
    else
      match Phon.difference_count ~t1 ~t2 with
      | 0 -> Float.of_int identical
      | 1 -> Float.of_int one_match
      | 2 -> Float.of_int two_match
      | _ ->
          if Phon.(same_feature_value (syl t1) (syl t2)) then
            Float.of_int syl_match
          else Float.of_int different

let basic_initialiser =
  make_initialiser ~different:1 ~null:10 ~syl_match:20 ~two_match:50
    ~one_match:5000 ~identical:25000

let run initialiser ?row_format ?params_callback ~smoothing ~base_cognate_prob
    data_path name iterations () =
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
    let base_cognate_probs = Em.infer_taxon_pair_cognate_probs dist in
    Stdio.print_endline "Maximising";
    let alpha, theta = maximise dist in
    (match params_callback with Some f -> f alpha theta | None -> ());

    Stdio.print_endline "Making score graph";
    let score_graph = Em.score_graph rows dist in
    Stdio.print_endline "Getting bcubed scores";
    List.iter (List.range 0 100) ~f:(fun i ->
        let clusters =
          Scoring.cluster Float.((0.99 - 0.001) * of_int i / 100.0) score_graph
        in
        let df = Owl.Dataframe.copy base_df in
        let new_df =
          Scoring.set_cognates_from_clusters ?row_format df clusters
        in
        (* CogID NewCogID  *)
        let scoring_rows =
          Bcubed_scores.rows_of_dataframe new_df ~reference_column:"CogID"
            ~given_column:"NewCogID" ~id_column:"ID"
        in
        let precision, recall, f_score =
          scoring_rows |> Bcubed_scores.score |> Bcubed_scores.average
        in
        Stdio.printf "%d %d %.3f %.3f %.3f\n" i n precision recall f_score;
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

let print_sorted_thetas taxons_to_show n _alpha theta =
  let taxons_to_show =
    List.map taxons_to_show ~f:(fun (t, t') ->
        (Taxon.of_string t, Taxon.of_string t'))
    |> List.map ~f:(fun ts ->
           ts
           |> Sorted_pair.of_tup ~compare:Taxon.compare
           |> Sorted_pair.to_derived)
  in
  let sorted = Em.sorted_thetas theta in

  Utils.take_last
    (List.filter sorted ~f:(fun (taxons, _, _) ->
         List.mem taxons_to_show taxons
           ~equal:(Sorted_pair.Derived.equal ~equal:Taxon.( = ))))
    n
  |> List.iter ~f:(fun (taxons, phones, v) ->
         let t1, t2 =
           Sorted_pair.Derived.(taxons |> map ~f:Taxon.to_string |> to_tup)
         in
         let p1, p2 =
           Sorted_pair.Derived.(phones |> map ~f:Phone.to_string |> to_tup)
         in
         Stdio.printf "%s %s %s %s %.4f\n" t1 t2 p1 p2
           (Probability.Log.to_float v));
  Stdio.print_endline ""
