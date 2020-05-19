open Core
open Dict.Infix
open Types

(** Get a list of which taxons are in a dataset *)
let all_taxons rows =
  rows |> List.map ~f:Row.taxon |> List.dedup_and_sort ~compare:Taxon.compare

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

(** Build encoders for some taxons and a table of the sets of phones for each *)
let all_encoders taxons phones_tbl =
  let open Dataset_utils.Infix in
  let result_table = Hashtbl.create (module Taxon) in
  let () =
    List.iter taxons ~f:(fun taxon ->
        result_table.@[taxon] <-
          Tuple2.get1 @@ Dataset_utils.phone_coders phones_tbl.@![taxon])
  in
  result_table

(* TODO - combine with all_encoders *)
let all_decoders taxons phones_tbl =
  let open Dataset_utils.Infix in
  let result_table = Hashtbl.create (module Taxon) in
  let () =
    List.iter taxons ~f:(fun taxon ->
        result_table.@[taxon] <-
          Tuple2.get2 @@ Dataset_utils.phone_coders phones_tbl.@![taxon])
  in
  result_table

let initialise_weights_tables taxons phones_tbl encoders_tbl ~initial_value
    initialiser =
  let open Owl.Dense.Ndarray in
  let f taxon1 taxon2 =
    let phones1, phones2 = (phones_tbl.@![taxon1], phones_tbl.@![taxon2]) in
    let weights =
      Generic.create Bigarray.Float64
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
  let result_table = Hashtbl.create (module Tuple.Hashable_t (Taxon) (Taxon)) in
  let () =
    List.iter (Utils.unique_pairs taxons) ~f:(fun (taxon1, taxon2) ->
        result_table.@[(taxon1, taxon2)] <- f taxon1 taxon2)
  in
  result_table

let new_weights_tables taxons phones =
  let open Owl.Dense.Ndarray in
  let f (taxon1, taxon2) =
    let phones1, phones2 = (phones.@![taxon1], phones.@![taxon2]) in
    Generic.create Bigarray.Float64
      [| 1 + Set.length phones1; 1 + Set.length phones2 |]
      Float.infinity
  in
  let result_table =
    Hashtbl.create (module Sorted_pair.Derived.Hashable_t (Taxon))
  in
  let () =
    List.iter (Utils.unique_pairs taxons) ~f:(fun (taxon1, taxon2) ->
        let sorted =
          Sorted_pair.of_tup (taxon1, taxon2) ~compare:Taxon.compare
          |> Sorted_pair.to_derived
        in
        result_table.@[sorted] <- f (Sorted_pair.Derived.to_tup sorted))
  in
  result_table

let cognate_pairs taxon1 taxon2 cognates =
  List.filter_map (Hashtbl.keys cognates) ~f:(fun i ->
      let open Dataset_utils.Infix in
      let lookup = List.Assoc.find ~equal:String.equal cognates.@![i] in
      Option.both (lookup taxon1) (lookup taxon2))
