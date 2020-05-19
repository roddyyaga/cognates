open Core
open Dict.Infix
open Types

let find_pair hashtbl (x, y) =
  match Dict.find hashtbl (x, y) with
  | Some v -> (v, false)
  | None -> (Dict.find_exn hashtbl (y, x), true)

let sort_tuple (x, y) ~compare =
  if compare x y <= 0 then ((x, y), false) else ((y, x), true)

let weights_and_index weights_table taxons phones =
  let weights, flipped = find_pair weights_table taxons in
  let phone1, phone2 = if flipped then Tuple2.swap phones else phones in
  (weights, [| phone1; phone2 |])

let get_weight weights_table taxons phones =
  let weights, index = weights_and_index weights_table taxons phones in
  let open Owl.Dense.Ndarray in
  Generic.get weights index

let mean xs = List.fold ~init:0.0 ~f:( +. ) xs /. Float.of_int (List.length xs)

let sum xs = List.fold ~init:0.0 ~f:( +. ) xs

let matching_tokens encoders_table aligned_rows =
  let matches_table = Dict.create (module Tuple.Hashable_t (Taxon) (Taxon)) in
  List.iter aligned_rows
    ~f:(fun (first_row, second_row, (_scores, pointers, score)) ->
      let first_encoded = Scoring.encode encoders_table first_row in
      let second_encoded = Scoring.encode encoders_table second_row in
      let alignments =
        Alignment.traceback first_encoded second_encoded pointers
      in
      List.iter [ List.hd_exn alignments ] ~f:(fun alignment ->
          let matched = Alignment.matched_elements alignment in
          let taxon_pair_key, flipped =
            sort_tuple
              (first_row.taxon, second_row.taxon)
              ~compare:Taxon.compare
          in
          let counter =
            Dict.find_or_add matches_table taxon_pair_key ~default:(fun () ->
                Dict.create (module Tuple.Hashable_t (Int) (Int)))
          in
          List.iter matched ~f:(fun (x, y) ->
              let x, y = if flipped then (y, x) else (x, y) in
              let previous_scores =
                counter.@?[(x, y)] |> Option.value ~default:[]
              in
              counter.@[(x, y)] <- score :: previous_scores)));
  let all_counts =
    List.map (Dict.keys matches_table) ~f:(fun (first_taxon, second_taxon) ->
        let counter = matches_table.@![(first_taxon, second_taxon)] in
        List.map (Dict.keys counter) ~f:(fun (x, y) ->
            ((first_taxon, second_taxon), (x, y), counter.@![(x, y)])))
    |> List.concat
  in
  all_counts

let print_frequent_matches encoders_table decoders_table weights_tables rows =
  let aligned_rows = Scoring.align_rows encoders_table weights_tables rows in
  matching_tokens encoders_table aligned_rows
  |> List.map ~f:(fun (taxons, phones, scores) ->
         (taxons, phones, Float.of_int @@ List.length scores))
  (*let old_score = get_weight weights_tables taxons phones in
    let score =
      sum @@ List.map scores ~f:(fun s -> Float.(s - old_score))
    in
    (taxons, phones, score))*)
  |> List.sort ~compare:(fun (_, _, first_score) (_, _, second_score) ->
         Float.(compare first_score second_score))
  |> List.iter ~f:(fun ((taxon1, taxon2), (phone1, phone2), score) ->
         let phone1_string = decoders_table.@![taxon1] phone1 in
         let phone2_string = decoders_table.@![taxon2] phone2 in
         if
           (Phone.(phone1_string <> null && phone2_string <> null) || true)
           && Taxon.(
                (taxon1 = of_string "Russian" && taxon2 = of_string "Italian")
                || (taxon1 = of_string "Italian" && taxon2 = of_string "Russian"))
         then
           Stdio.printf "%s %s %s %s %f\n" (Taxon.to_string taxon1)
             (Taxon.to_string taxon2)
             (Phone.to_string phone1_string)
             (Phone.to_string phone2_string)
             score)

let all_taxon_phone_pair_scores encoders_table decoders_table phone_counts
    aligned_rows =
  let counter = Dict.create (module Tuple.Hashable_t (Phone) (Phone)) in
  let () =
    matching_tokens encoders_table aligned_rows
    |> List.iter ~f:(fun ((taxon1, taxon2), (phone1, phone2), scores) ->
           let phone1_string = decoders_table.@![taxon1] phone1 in
           let phone2_string = decoders_table.@![taxon2] phone2 in
           let phones_key, _flipped =
             sort_tuple (phone1_string, phone2_string) ~compare:Phone.compare
           in
           Dict.extend counter phones_key scores)
  in
  let counter =
    Dict.mapi counter ~f:(fun ~key ~data ->
        let t1, t2 = key in
        let get t =
          match phone_counts.@?[t] with
          | Some n -> Float.of_int n
          | None ->
              (* empty *)
              0.0
        in
        Float.(sum data / (get t1 + get t2)))
  in
  counter

let frequent_matches_all_taxons encoders_table decoders_table weights_table rows
    phone_counts =
  let aligned_rows = Scoring.align_rows encoders_table weights_table rows in
  let counter =
    all_taxon_phone_pair_scores encoders_table decoders_table phone_counts
      aligned_rows
  in
  let sorted_pairs =
    Dict.to_alist counter
    |> List.sort ~compare:(fun (_k1, v1) (_k2, v2) -> Float.compare v1 v2)
  in
  List.iter sorted_pairs ~f:(fun ((p1, p2), v) ->
      if Phone.(p1 <> p2 && p1 <> null && p2 <> null) then
        Printf.printf "%s %s %.2f\n" (Phone.to_string p1) (Phone.to_string p2) v)

let taxon_pair_total_scores encoders_table weights_tables rows =
  let aligned_rows = Scoring.align_rows encoders_table weights_tables rows in
  let taxon_pair_totals =
    Dict.create (module Tuple.Hashable_t (Taxon) (Taxon))
  in
  List.iter aligned_rows ~f:(fun (row1, row2, (_, _, score)) ->
      let (taxon1, taxon2), _flipped =
        sort_tuple (row1.taxon, row2.taxon) ~compare:Taxon.compare
      in
      Dict.incr_float taxon_pair_totals (taxon1, taxon2) ~by:score);

  Dict.keys taxon_pair_totals
  |> List.sort ~compare:(fun pair1 pair2 ->
         Float.compare taxon_pair_totals.@![pair1] taxon_pair_totals.@![pair2])
  |> List.iter ~f:(fun (taxon1, taxon2) ->
         let score = taxon_pair_totals.@![(taxon1, taxon2)] in
         Printf.printf "%s %s %.2f\n" (Taxon.to_string taxon1)
           (Taxon.to_string taxon2) score)

let update_weights weights_table taxons phones update_function =
  let _weights, flipped = find_pair weights_table taxons in
  let weights, index = weights_and_index weights_table taxons phones in
  let taxons = if flipped then Tuple2.swap taxons else taxons in
  let open Owl.Dense.Ndarray in
  let old_weight = Generic.get weights index in
  Generic.set weights index (update_function old_weight);
  Dict.set weights_table ~key:taxons ~data:weights
