open Base
open Types

let data_path = "/home/roddy/iii/project/code/data/PIE.csv"

let Dataset_utils.{ phones; words; cogs; concept_to_gloss_id; phone_counts } =
  Dataset_utils.load_dataset ~verbose:true data_path

let rows = Dataset_utils.load_rows data_path

let lookup = Scoring.get_row concept_to_gloss_id rows

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

let get_rows = Scoring.get_rows concept_to_gloss_id rows

let threes =
  List.bind
    ~f:(fun taxon -> get_rows (Taxon.of_string taxon) "all")
    [ "Danish"; "English"; "German"; "Icelandic"; "Swedish" ]

let fours =
  List.bind
    ~f:(fun taxon -> get_rows (Taxon.of_string taxon) "all")
    [ "Dutch"; "Norwegian" ]

let elevens =
  List.bind
    ~f:(fun taxon -> get_rows (Taxon.of_string taxon) "all")
    [ "Romanian"; "Spanish"; "Italian" ]
  |> List.filter ~f:(fun row ->
         not @@ List.mem [ 19; 9; 22 ] row.Row.id ~equal:Int.equal)

let score = Scoring.score_pair word_encoders weights_tables

let tfs =
  List.cartesian_product threes fours
  |> List.map ~f:(fun pair -> (pair, score pair))

let tes =
  List.cartesian_product threes elevens
  |> List.map ~f:(fun pair -> (pair, score pair))

let r = lookup
