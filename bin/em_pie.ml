open Base
open Lib
open Lib.Types
open Lib.Dict.Infix

let data_path = "/home/roddy/iii/project/code/data/PIE.csv"

let base_df = Owl.Dataframe.of_csv data_path

let Dataset_utils.{ phones; words; cogs; concept_to_gloss_id; phone_counts } =
  Dataset_utils.load_dataset ~verbose:false data_path

(*let () =
  List.iter (Hashtbl.keys phones) ~f:(fun taxon ->
      let open Dataset_utils.Infix in
      Stdio.print_endline taxon;
      Set.iter phones.@![taxon] ~f:(fun phone ->
          match Phon.phone_exists ~t:phone with
          | true -> ()
          | false -> Stdio.printf "%s doesn't exist\n" phone);
      Stdio.print_endline "")

let () = Stdio.print_endline "Done!"*)

let rows = Dataset_utils.load_rows data_path

let make_init_offset () = 2 ** Random.int 10


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

let basic_initialiser t1 t2 =
  if Phone.(t1 = null || t2 = null) then 10.0
  else
    let t1, t2 = (Phone.to_string t1, Phone.to_string t2) in
    let open Phon in
    (*   Stdio.printf "%s %s %d\n" t1 t2 (difference_count ~t1 ~t2); *)
    if String.(t1 = t2) then 25000.0
    else
      match difference_count ~t1 ~t2 with
      | 0 -> 25000.0
      | 1 -> 5000.0
      | 2 -> 500.0
      | _ -> if same_feature_value (syl t1) (syl t2) then 20.0 else 1.0

let () = Stdio.print_endline "Getting taxons"

let taxons = Initialisation.all_taxons rows

let () = Stdio.print_endline "Getting encoders"

let encoders = Initialisation.all_encoders taxons phones

let decoders = Initialisation.all_decoders taxons phones

let () = Stdio.print_endline "Getting weights tables"

let weights_tables = Initialisation.new_weights_tables taxons phones

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

let () = Stdio.print_endline "Getting word encoders"

let word_encoders = Hashtbl.map encoders ~f:word_encoder

let theta = Em.Theta_family.create ()

let alpha = Em.Alpha_family.create ()

let () =
  Em.initialise_parameters theta alpha
    (List.map taxons ~f:(fun t -> (t, Set.to_list phones.@![t])))
    basic_initialiser

let row_pairs = Em.possible_pairs rows

let expect =
  Em.expectations ~explain:false encoders decoders row_pairs weights_tables
    ~base_cognate_prob:(Probability.of_float 0.005)

let expect_german_spanish =
  let row_pairs =
    row_pairs
    |> List.filter ~f:(fun (r1, r2) ->
           let open Row in
           Taxon.(
             r1.taxon = of_string "Spanish" && r2.taxon = of_string "German")
           || Taxon.(
                r1.taxon = of_string "German" && r2.taxon = of_string "Spanish"))
  in
  Em.expectations ~explain:true encoders decoders row_pairs weights_tables
    ~base_cognate_prob:(Probability.of_float 0.05)

let maximise = Em.maximise ~smoothing:0.0

let filter_dist language_pairs dist =
  let language_pairs =
    List.map language_pairs ~f:(fun (t1, t2) ->
        (Taxon.of_string t1, Taxon.of_string t2)
        |> Sorted_pair.of_tup ~compare:Taxon.compare
        |> Sorted_pair.to_tup)
  in
  let open Em in
  List.filter dist ~f:(function
    | Cognate (_, (r1, r2)) ->
        let taxon_pair =
          (r1.Aligned_row.row.Row.taxon, r2.Aligned_row.row.Row.taxon)
          |> Sorted_pair.of_tup ~compare:Taxon.compare
          |> Sorted_pair.to_tup
        in
        List.mem language_pairs taxon_pair ~equal:(fun (x1, x2) (y1, y2) ->
            Taxon.(x1 = y1 && x2 = y2))
    | _ -> false)
  |> List.sort ~compare:(fun x y ->
         match (x, y) with
         | Cognate (p1, _), Cognate (p2, _) -> Probability.compare p1 p2
         | _ -> assert false)

let dist_for_gloss_id id dist =
  let open Em in
  List.filter dist ~f:(function
      | Cognate (_, (r1, r2)) | Not_cognate (_, (r1, r2)) ->
      r1.Aligned_row.row.Row.gloss_id = id
      || r2.Aligned_row.row.Row.gloss_id = id)

let taxon_pair_cognate_probs dist =
  let counts = Dict.create (module Sorted_pair.Hashable_t (Taxon)) in
  let open Em in
  List.iter dist ~f:(function
    | Cognate (p, (row1, row2)) ->
        let key =
          (row1.Aligned_row.row.Row.taxon, row2.Aligned_row.row.Row.taxon)
          |> Sorted_pair.of_tup ~compare:Taxon.compare
        in
        Dict.extend counts key [ Probability.to_float p ]
    | _ -> ());
  Dict.map counts ~f:Utils.float_mean

let iterate n alpha theta =
  let dist = expect alpha theta in
  let alpha, theta = maximise dist in
  (*let counts = taxon_pair_cognate_probs dist in
    List.iter (Dict.sorted_items counts ~compare:Float.compare) ~f:(fun (k, p) ->
        let t1, t2 = Sorted_pair.to_tup k in
        Stdio.printf "%s %s %.4f\n" (Taxon.to_string t1) (Taxon.to_string t2) p);*)
  let score_graph = Em.score_graph rows dist in
  (*   Em.print_dist (filter_dist [ ("German", "Spanish") ] dist); *)
  (*   Em.print_dist (filter_dist [ ("Dutch", "German") ] dist); *)
  (*   Em.print_dist (filter_dist [ ("French", "Italian") ] dist); *)
  (*   Em.print_dist (dist_for_gloss_id 1 dist); *)
  List.iter (List.range 0 20) ~f:(fun i ->
      (*       Stdio.printf "%f %f\n" min_score max_score; *)
      let clusters =
        Scoring.cluster
          Float.(0.01 + ((1.01 - 0.01) * of_int i / 20.0))
          score_graph
      in
      let df = Owl.Dataframe.copy base_df in
      let new_df = Scoring.set_cognates_from_clusters df clusters in
      (* CogID NewCogID  *)
      let scoring_rows =
        Bcubed_scores.rows_of_dataframe new_df ~reference_column:"CogID"
          ~given_column:"NewCogID" ~id_column:"ID"
      in
      let precision, recall, f_score =
        scoring_rows |> Bcubed_scores.score |> Bcubed_scores.average
      in
      Stdio.printf "%d %d %.3f %.3f %.3f\n" i n precision recall f_score;
      Owl.Dataframe.to_csv new_df (Printf.sprintf "PIE_scored_%d_%d.csv" i n));
  Stdio.print_endline "";
  (dist, alpha, theta)

let _dist, alpha, theta = iterate 1 alpha theta

(*let () = Em.print_dist dist *)

let _dist, alpha, theta = iterate 2 alpha theta

(*  let () = Em.print_dist dist  *)

let _dist, alpha, theta = iterate 3 alpha theta

let _dist, alpha, theta = iterate 4 alpha theta

let _dist, alpha, theta = iterate 5 alpha theta

let () = Stdio.print_endline "5 iterations"

let _dist, alpha, theta = iterate 6 alpha theta

let _dist, alpha, theta = iterate 7 alpha theta

(* let _dist, alpha, theta = iterate 8 alpha theta *)

(* let _dist, alpha, theta = iterate 9 alpha theta *)

(* let _dist, alpha, theta = iterate 10 alpha theta *)

let string_of_params
    ( different,
      null,
      syl_match,
      two_match,
      one_match,
      identical,
      smoothing,
      cog_prob ) =
  Printf.sprintf "%d %d %d %d %d %d %.4f %.4f" different null syl_match
    two_match one_match identical smoothing cog_prob

let print_params params = Stdio.print_endline @@ string_of_params params

let do_run f_scoreses () =
  (*let different = make_init_offset () in
    let null = different + make_init_offset () in
    let syl_match = null + make_init_offset () in
    let two_match = syl_match + make_init_offset () in
    let one_match = two_match + make_init_offset () in
    let identical = one_match + make_init_offset () in
  *)
  (*let different = 1 in
    let null = different + Random.int_incl 0 20 in
    let syl_match = null + Random.int_incl 0 20 in
    let two_match = syl_match + Random.int_incl 100 2000 in
    let one_match = two_match + Random.int_incl 500 20000 in
    let identical = one_match + Random.int_incl 0 20000 in*)
  let different = 1 in
  let null = 10 in
  let syl_match = 20 in
  let two_match = 500 in
  let one_match = 5000 in
  let identical = 25000 in

  let initialiser =
    make_initialiser ~different ~null ~syl_match ~two_match ~one_match
      ~identical
  in
  let smoothing =
    match Random.int 2 with
    | 0 -> Random.float_range 0.0 0.3
    | 1 -> 0.0
    | _ -> failwith "err"
  in

  let cognate_prob =
    match Random.int 2 with
    | 0 -> Random.float 1.0
    | 1 -> Random.float_range 0.0 0.1
    | _ -> failwith "err"
  in

  let expect =
    Em.expectations ~explain:false encoders decoders row_pairs weights_tables
      ~base_cognate_prob:(Probability.of_float cognate_prob)
  in
  let maximise = Em.maximise ~smoothing in

  let theta = Em.Theta_family.create () in

  let alpha = Em.Alpha_family.create () in
  let () =
    Em.initialise_parameters theta alpha
      (List.map taxons ~f:(fun t -> (t, Set.to_list phones.@![t])))
      initialiser
  in
  let params =
    ( different,
      null,
      syl_match,
      two_match,
      one_match,
      identical,
      smoothing,
      cognate_prob )
  in
  print_params params;
  let iterate f_scores alpha theta =
    let dist = expect alpha theta in
    let alpha, theta = maximise dist in
    let score_graph = Em.score_graph rows dist in
    let f_score_list =
      List.map (List.range 0 200) ~f:(fun i ->
          (*       Stdio.printf "%f %f\n" min_score max_score; *)
          let clusters =
            Scoring.cluster
              Float.(0.01 + ((1.01 - 0.01) * of_int i / 200.0))
              score_graph
          in
          let df = Owl.Dataframe.copy base_df in
          let new_df = Scoring.set_cognates_from_clusters df clusters in
          (* CogID NewCogID  *)
          let scoring_rows =
            Bcubed_scores.rows_of_dataframe new_df ~reference_column:"CogID"
              ~given_column:"NewCogID" ~id_column:"ID"
          in
          let _precision, _recall, f_score =
            scoring_rows |> Bcubed_scores.score |> Bcubed_scores.average
          in
          f_score)
    in
    f_scores.@[string_of_params params] <-
      Option.value_exn (List.max_elt f_score_list ~compare:Float.compare);
    (alpha, theta)
  in
  let _final_alpha, _final_theta =
    List.fold f_scoreses
      ~f:(fun (alpha, theta) f_scores -> iterate f_scores alpha theta)
      ~init:(alpha, theta)
  in
  ()

(*
let f_scoreses = List.init 5 ~f:(fun _ -> Dict.create (module String))

let () = List.init 50 ~f:(fun _ -> do_run f_scoreses ()) |> List.iter ~f:Fn.id

let () =
  List.iter f_scoreses ~f:(fun f_scores ->
      Stdio.print_endline "Next iteration";
      let sorted_scores = Dict.sorted_items f_scores ~compare:Float.compare in
      List.iter sorted_scores ~f:(fun (p, s) -> Stdio.printf "%s - %.4f\n" p s))*)
