open Core
open Owl

let unique_pairs xs =
  let rec f result remaining =
    match remaining with
    | [] -> result
    | x :: new_remaining ->
        let new_result =
          List.map new_remaining ~f:(fun x' -> (x, x')) @ result
        in
        f new_result new_remaining
  in
  f [] xs

(** Get a list of which taxons are in a dataset *)
let all_taxons rows =
  let open Dataset_utils in
  rows |> List.map ~f:taxon |> List.dedup_and_sort ~compare:String.compare

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

(** Build encoders for some taxons and a table of the sets of phones for each *)
let all_encoders taxons phones_tbl =
  let open Dataset_utils.Infix in
  let result_table = Hashtbl.create (module String) in
  let () =
    List.iter taxons ~f:(fun taxon ->
        result_table.@[taxon] <-
          Tuple2.get1 @@ Dataset_utils.phone_coders phones_tbl.@![taxon])
  in
  result_table

let initialise_weights_tables taxons phones_tbl encoders_tbl ~initial_value
    initialiser =
  let open Dense.Ndarray in
  let open Dataset_utils.Infix in
  let f taxon1 taxon2 =
    let phones1, phones2 = (phones_tbl.@![taxon1], phones_tbl.@![taxon2]) in
    let weights =
      Generic.create Bigarray.Int
        [| 1 + Set.length phones1; 1 + Set.length phones2 |]
        initial_value
    in
    Set.iter phones1 ~f:(fun t1 ->
        Set.iter phones2 ~f:(fun t2 ->
            let encoder1, encoder2 =
              (encoders_tbl.@![taxon1], encoders_tbl.@![taxon2])
            in
            let weight = initialiser t1 t2 in
            Generic.set weights [| encoder1 t1; encoder2 t2 |] weight));
    weights
  in
  let result_table =
    Hashtbl.create (module Tuple.Hashable_t (String) (String))
  in
  let () =
    List.iter (unique_pairs taxons) ~f:(fun (taxon1, taxon2) ->
        result_table.@[(taxon1, taxon2)] <- f taxon1 taxon2)
  in
  result_table

let cognate_pairs taxon1 taxon2 cognates =
  List.filter_map (Hashtbl.keys cognates) ~f:(fun i ->
      let open Dataset_utils.Infix in
      let lookup = List.Assoc.find ~equal:String.equal cognates.@![i] in
      Option.both (lookup taxon1) (lookup taxon2))
