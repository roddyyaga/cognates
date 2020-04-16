open Base
open Lib

let data_path = "/home/roddy/iii/project/code/data/PIE.csv"

let Dataset_utils.{ phones; words; cogs } =
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
  match String.(t1 = t2) with
  | true -> 4
  | false -> (
      let open Phon in
      match same_feature_value (syl t1) (syl t2) with true -> 2 | false -> -1 )

let () = Stdio.print_endline "Getting taxons"

let taxons = Initialisation.all_taxons rows

let () = Stdio.print_endline "Getting encoders"

let encoders = Initialisation.all_encoders taxons phones

let () = Stdio.print_endline "Getting weights tables"

let weights_tables =
  Initialisation.initialise_weights_tables taxons phones encoders
    basic_initialiser ~initial_value:(-1)

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

let () = Stdio.print_endline "Getting word encoders"

let word_encoders = Hashtbl.map encoders ~f:word_encoder

let () = Stdio.print_endline "Getting score graph"

let score_graph = Scoring.score_graph word_encoders weights_tables rows

let () =
  List.iter (List.range 0 20) ~f:(fun i ->
      Stdio.print_endline "Getting clusters";
      let clusters =
        Scoring.cluster Float.(-1.0 + (5.0 * of_int i / 20.0)) score_graph
      in
      Stdio.print_endline "Writing out";
      let df = Owl.Dataframe.of_csv data_path in
      let new_df = Scoring.set_cognates_from_clusters df clusters in
      Owl.Dataframe.to_csv new_df (Printf.sprintf "PIE_scored_%d.csv" i))

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
