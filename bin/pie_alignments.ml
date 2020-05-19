open Base
open Lib
open Lib.Types

let data_path = "/home/roddy/iii/project/code/data/PIE.csv"

let Dataset_utils.{ phones; words; cogs; concept_to_gloss_id; phone_counts } =
  Dataset_utils.load_dataset ~verbose:true data_path

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

let basic_initialiser t1 t2 =
  let t1, t2 = (Phone.to_string t1, Phone.to_string t2) in
  let open Phon in
  (*   Stdio.printf "%s %s %d\n" t1 t2 (difference_count ~t1 ~t2); *)
  if String.(t1 = t2) then 5.0
  else
    match difference_count ~t1 ~t2 with
    | 0 ->
        Stdio.printf "Identical %s %s\n" t1 t2;
        5.0
    | 1 ->
        Stdio.printf "One %s %s\n" t1 t2;
        2.0
    | 2 ->
        Stdio.printf "Two %s %s\n" t1 t2;
        1.0
    | _ -> if same_feature_value (syl t1) (syl t2) then 0.0 else -3.0

let () = Stdio.print_endline "Getting taxons"

let taxons = Initialisation.all_taxons rows

let () = Stdio.print_endline "Getting encoders"

let encoders = Initialisation.all_encoders taxons phones

let decoders = Initialisation.all_decoders taxons phones

let () = Stdio.print_endline "Getting weights tables"

let weights_tables =
  Initialisation.initialise_weights_tables taxons phones encoders
    basic_initialiser ~initial_value:(-1.0)

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

let () = Stdio.print_endline "Getting word encoders"

let word_encoders = Hashtbl.map encoders ~f:word_encoder

let () = Stdio.print_endline "Getting score graph"

let score_graph = Scoring.score_graph word_encoders weights_tables rows

let () =
  List.iter (List.range 0 40) ~f:(fun i ->
      Stdio.printf ".";
      let clusters =
        Scoring.cluster Float.(0.0 + (2.0 * of_int i / 40.0)) score_graph
      in
      let df = Owl.Dataframe.of_csv data_path in
      let new_df = Scoring.set_cognates_from_clusters df clusters in
      let scoring_rows =
        Bcubed_scores.rows_of_dataframe df ~reference_column:"CogID"
          ~given_column:"NewCogID" ~id_column:"ID"
      in
      let precision, recall, f_score =
        scoring_rows |> Bcubed_scores.score |> Bcubed_scores.average
      in
      Stdio.printf "%d %.3f %.3f %.3f\n" i precision recall f_score;
      Owl.Dataframe.to_csv new_df (Printf.sprintf "PIE_scored_%d_og.csv" i))

let () = Stdio.print_endline ""

let () =
  Updates.print_frequent_matches word_encoders decoders weights_tables rows

let iteration n =
  let open Dataset_utils.Infix in
  Stdio.printf "\nIteration %d\n" n;
  let aligned_rows = Scoring.align_rows word_encoders weights_tables rows in
  let pair_scores =
    Updates.all_taxon_phone_pair_scores word_encoders decoders phone_counts
      aligned_rows
  in
  let max_exn xs = Option.value_exn (List.max_elt xs ~compare:Float.compare) in
  let min_exn xs = Option.value_exn (List.min_elt xs ~compare:Float.compare) in
  let filtered =
    pair_scores
    |> Hashtbl.filteri ~f:(fun ~key ~data ->
           ignore data;
           let t1, t2 = key in
           Phone.(t1 <> t2 && t1 <> null && t2 <> null))
    |> Hashtbl.data
  in
  let max_score, min_score = (max_exn filtered, min_exn filtered) in
  Stdio.printf "max %f min %f\n" max_score min_score;
  List.iter (Updates.matching_tokens word_encoders aligned_rows)
    ~f:(fun ( ((taxon1, taxon2) as taxons),
              ((phone1, phone2) as phones),
              _match_score )
            ->
      let t1, t2 = (decoders.@![taxon1] phone1, decoders.@![taxon2] phone2) in
      let (t1', t2'), _flipped =
        Updates.sort_tuple (t1, t2) ~compare:Phone.compare
      in
      let score = pair_scores.@![(t1', t2')] in
      let update_function old_weight =
        if Phone.(t1 = t2 || t1 = null || t2 = null) then old_weight
        else if
          (*           Stdio.printf "%s %s %.2f    " t1' t2' scaled; *)
          Float.(score > 0.3)
        then Float.max 4.0 old_weight
        else old_weight
      in
      Updates.update_weights weights_tables taxons phones update_function);

  let score_graph = Scoring.score_graph word_encoders weights_tables rows in
  let all_scores = Scoring.all_finite_scores score_graph in
  let min_score, max_score =
    Float.
      ( Option.value_exn (List.min_elt ~compare all_scores),
        Option.value_exn (List.max_elt ~compare all_scores) )
  in
  List.iter (List.range 0 40) ~f:(fun i ->
      (*       Stdio.printf "%f %f\n" min_score max_score; *)
      let clusters =
        Scoring.cluster
          Float.(min_score + ((max_score - min_score) * of_int i / 40.0))
          score_graph
      in
      let df = Owl.Dataframe.of_csv data_path in
      let new_df = Scoring.set_cognates_from_clusters df clusters in
      (* CogID NewCogID  *)
      let scoring_rows =
        Bcubed_scores.rows_of_dataframe df ~reference_column:"CogID"
          ~given_column:"NewCogID" ~id_column:"ID"
      in
      let precision, recall, f_score =
        scoring_rows |> Bcubed_scores.score |> Bcubed_scores.average
      in
      Stdio.printf "%d %d %.3f %.3f %.3f\n" i n precision recall f_score;
      Owl.Dataframe.to_csv new_df (Printf.sprintf "PIE_scored_%d_%d.csv" i n))

let () = iteration 1

let () = iteration 2

let () = iteration 3

let () = iteration 4

(*
let () =
  List.iter (Hashtbl.keys cogs) ~f:(fun i ->
      let es_opt = List.Assoc.find ~equal:String.( = ) cogs.@![i] "Spanish" in
      let it_opt = List.Assoc.find ~equal:String.( = ) cogs.@![i] "English" in
      match (es_opt, it_opt) with
      | Some es, Some it ->
          let spanish_encoded =
            List.to_array @@ List.map ~f:encode_spanish es
          in
          let italian_encoded =
            List.to_array @@ List.map ~f:encode_italian it
          in
          let _scores, pointers, score =
            Alignment.align weights spanish_encoded italian_encoded
          in
          Stdio.print_endline (String.concat es);
          Stdio.print_endline (String.concat it);
          let alignments =
            Alignment.traceback spanish_encoded italian_encoded pointers
          in
          List.iter
            ~f:(fun (a, b) ->
              Stdio.print_endline
              @@ Dataset_utils.aligned_to_string decode_spanish a;
              Stdio.print_endline
              @@ Dataset_utils.aligned_to_string decode_italian b)
            alignments;
          Stdio.print_endline @@ Int.to_string score;
          Stdio.print_endline ""
      | _ -> ())

let () =
  Set.iter spanish_phones ~f:(fun p ->
      Stdio.print_endline
      @@ Printf.sprintf "%s - %s" p
           (Lib.Phon.feature_to_string @@ Lib.Phon.syl p))

let () = Dataset_utils.print_weights decode_spanish decode_italian weights
*)
