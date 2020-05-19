(* Need core [String] not base for Hashable_t *)
open Core
open Dict.Infix
open Types

let get_phone_string Phone_with_taxon.{ s; _ } = s

type phone_pair = Phone_with_taxon.t Sorted_pair.t

module Aligned_row = struct
  type t = { row: Row.t; aligned: Phone.t list }
end

type word_pair_distribution =
  | Cognate of Probability.t * (Aligned_row.t * Aligned_row.t)
  | Not_cognate of Probability.t * (Aligned_row.t * Aligned_row.t)

module Theta = struct
  type 'a t = (Phone.t Sorted_pair.Derived.t, 'a) Dict.t [@@deriving sexp_of]

  let create () = Dict.create (module Sorted_pair.Derived.Hashable_t (Phone))
end

module Theta_family = struct
  type 'a t = (Taxon.t Sorted_pair.Derived.t, 'a Theta.t) Dict.t
  [@@deriving sexp_of]

  let create () = Dict.create (module Sorted_pair.Derived.Hashable_t (Taxon))

  let show f t = sexp_of_t f t |> Sexp.to_string_hum
end

module Alpha = struct
  type 'a t = (Phone.t, 'a) Dict.t [@@deriving sexp_of]

  let create () = Dict.create (module Phone)
end

module Alpha_family = struct
  type 'a inner = (Taxon.t, 'a Alpha.t) Dict.t [@@deriving sexp_of]

  type 'a t = (Taxon.t, 'a inner) Dict.t [@@deriving sexp_of]

  let create () = Dict.create (module Taxon)

  let with_respect_to taxon t =
    Dict.find_or_add t taxon ~default:(fun () -> Dict.create (module Taxon))

  let show f t = sexp_of_t f t |> Sexp.to_string_hum
end

let phones_of_rows (row1, row2) =
  let f row =
    row.Aligned_row.aligned
    |> List.map ~f:(fun phone ->
           Phone_with_taxon.{ s = phone; taxon = row.Aligned_row.row.Row.taxon })
  in
  List.concat @@ List.map ~f [ row1; row2 ]

let phone_pairs_of_aligned_rows (row1, row2) =
  let open Aligned_row in
  List.zip_exn row1.aligned row2.aligned
  |> List.map ~f:(fun (s1, s2) ->
         ( { Phone_with_taxon.taxon = row1.Aligned_row.row.Row.taxon; s = s1 },
           { Phone_with_taxon.taxon = row2.Aligned_row.row.Row.taxon; s = s2 }
         )
         |> Sorted_pair.of_tup ~compare:Phone_with_taxon.compare)

let estimate_theta ~smoothing theta_counts =
  let first_taxon_counts = Dict.create (module Phone) in
  let second_taxon_counts = Dict.create (module Phone) in
  Dict.iteri theta_counts ~f:(fun ~key:pair ~data:count ->
      let s1, s2 = Sorted_pair.Derived.to_tup pair in
      Dict.incr_float first_taxon_counts s1 ~by:count;
      Dict.incr_float second_taxon_counts s2 ~by:count);
  let first_total = first_taxon_counts |> Dict.data |> Utils.float_sum in
  let second_total = second_taxon_counts |> Dict.data |> Utils.float_sum in

  (* Subtract 1 to account for "-/-" *)
  let possible_pairs_count =
    (Dict.length first_taxon_counts * Dict.length second_taxon_counts) - 1
    |> Float.of_int
  in
  let pairs_total = theta_counts |> Dict.data |> Utils.float_sum in
  let first_all_phones = Dict.keys first_taxon_counts in
  let second_all_phones = Dict.keys second_taxon_counts in

  let result = Theta.create () in
  List.iter (List.cartesian_product first_all_phones second_all_phones)
    ~f:(fun (phone1, phone2) ->
      let theta_value =
        if Phone.(phone1 = Phone.null && phone2 = Phone.null) then
          Probability.(zero |> to_log)
        else
          let first_prob =
            Float.(first_taxon_counts.@![phone1] / first_total)
            |> Probability.of_float
          in
          let second_prob =
            Float.(second_taxon_counts.@![phone2] / second_total)
            |> Probability.of_float
          in
          let base_pair_prob =
            Probability.(first_prob * second_prob) |> Probability.to_float
          in
          let pair_count =
            match
              theta_counts.@?[Sorted_pair.Derived.of_tup_unsafe (phone1, phone2)]
            with
            | Some count -> count
            | None -> 0.0
          in
          let numerator =
            Float.(
              pair_count + (smoothing * possible_pairs_count * base_pair_prob))
            |> Float.log
          in
          let denominator =
            Float.(pairs_total + (smoothing * possible_pairs_count))
            |> Float.log
          in
          Float.(numerator - denominator) |> Probability.Log.of_float
      in
      result.@[Sorted_pair.Derived.of_tup_unsafe (phone1, phone2)] <-
        theta_value);

  result

let estimate_alpha alpha_counts =
  let total = alpha_counts |> Dict.data |> Utils.float_sum in
  Dict.map alpha_counts ~f:(fun count ->
      Float.(log count - log total) |> Probability.Log.of_float)

let maximise ~smoothing data =
  let alpha_family = Dict.create (module Taxon) in
  let theta_family = Theta_family.create () in
  List.iter data ~f:(function
    | Cognate (p, (row1, row2)) ->
        let pairs = phone_pairs_of_aligned_rows (row1, row2) in
        let key =
          List.hd_exn pairs
          |> Sorted_pair.map ~f:(fun p -> p.Phone_with_taxon.taxon)
        in
        let theta =
          Hashtbl.find_or_add theta_family key ~default:Theta.create
        in
        List.iter pairs ~f:(fun pair ->
            Dict.incr_float theta
              (Sorted_pair.map ~f:get_phone_string pair)
              ~by:(Probability.to_float p))
    | Not_cognate (p, (row1, row2)) ->
        let taxon1, taxon2 =
          (row1.Aligned_row.row.Row.taxon, row2.Aligned_row.row.Row.taxon)
        in
        let phones = phones_of_rows (row1, row2) in
        List.iter phones ~f:(fun phone ->
            let key = get_phone_string phone in
            let other_taxon =
              if Taxon.(phone.Phone_with_taxon.taxon = taxon1) then taxon2
              else (
                assert (Taxon.(phone.taxon = taxon2));
                taxon1 )
            in
            let alpha =
              let other_alphas =
                Alpha_family.with_respect_to other_taxon alpha_family
              in
              Dict.find_or_add other_alphas phone.taxon ~default:Alpha.create
            in
            Dict.incr_float alpha key ~by:(Probability.to_float p)));
  ( Dict.map ~f:(fun inner -> Dict.map inner ~f:estimate_alpha) alpha_family,
    Dict.map ~f:(estimate_theta ~smoothing) theta_family )

let word_encoder encoder word = List.to_array @@ List.map ~f:encoder word

let expectations ?(explain = false) encoders decoders row_pairs weights_table
    alphas thetas ~base_cognate_prob =
  let word_encoders = Dict.map encoders ~f:word_encoder in

  (* Put theta values in weights tables *)
  List.iter
    (Utils.unique_pairs (Dict.keys encoders))
    ~f:(fun (taxon1, taxon2) ->
      let key = Sorted_pair.of_tup (taxon1, taxon2) ~compare:Taxon.compare in
      let theta = thetas.@![key |> Sorted_pair.to_derived] in
      let weights = weights_table.@![key |> Sorted_pair.to_derived] in
      let encoder1, encoder2 =
        Sorted_pair.(key |> map ~f:(fun t -> encoders.@![t]) |> Derived.to_tup)
      in
      let open Owl.Dense.Ndarray in
      Generic.create_ ~out:weights Float.(-infinity);
      Dict.iteri theta ~f:(fun ~key:phone_pair ~data:weight ->
          let t1, t2 = Sorted_pair.Derived.to_tup phone_pair in
          let w = Probability.Log.to_float weight in
          Generic.set weights [| encoder1 t1; encoder2 t2 |] w));

  (* Compute expectations *)
  List.map row_pairs ~f:(fun (row1, row2) ->
      let row_pair =
        Sorted_pair.of_tup (row1, row2) ~compare:(fun r1 r2 ->
            Taxon.compare r1.Row.taxon r2.Row.taxon)
      in
      let taxon_pair = Sorted_pair.map ~f:Row.taxon row_pair in

      (* Align *)
      let weights = weights_table.@![taxon_pair] in
      let word_encoders_pair =
        Sorted_pair.map ~f:(fun r -> word_encoders.@![r.Row.taxon]) row_pair
      in
      let decoders' =
        Sorted_pair.map ~f:(fun r -> decoders.@![r.Row.taxon]) row_pair
      in
      let encoded1', encoded2' =
        Sorted_pair.Derived.map2 (Sorted_pair.to_derived row_pair)
          word_encoders_pair ~f:(fun row word_encoder ->
            word_encoder row.Row.tokens)
        |> Sorted_pair.Derived.to_tup
      in
      let _scores, pointers, alignment_log_lik =
        Alignment.align weights encoded1' encoded2'
      in
      let traces = Alignment.traceback encoded1' encoded2' pointers in

      let aligneds_raw =
        (* Take an arbitrary alignment with maximum probability *)
        List.hd_exn traces
      in

      let aligneds =
        aligneds_raw |> Sorted_pair.Derived.of_tup_unsafe
        |> Sorted_pair.Derived.map2 decoders' ~f:(fun dec xs ->
               List.map ~f:dec xs)
      in
      let an_alignment =
        Sorted_pair.(
          Derived.map2 (to_derived row_pair) aligneds ~f:(fun row aligned ->
              Aligned_row.{ row; aligned })
          |> Derived.to_tup)
      in
      if explain then (
        Stdio.printf "Explaining %s %s:\n"
          ((fst an_alignment).Aligned_row.aligned |> Utils.word_of_phones)
          ((snd an_alignment).Aligned_row.aligned |> Utils.word_of_phones);
        let scores =
          Alignment.scores weights (fst aligneds_raw) (snd aligneds_raw)
        in
        List.iter scores ~f:(fun x -> Stdio.printf "%.2f " x);
        Stdio.printf "\n" );

      let cognate_log_lik, cognate_data =
        (alignment_log_lik |> Probability.Log.of_float, an_alignment)
      in

      (* Compute probability of being generated separately *)
      let non_cognate_log_lik, non_cognate_data =
        let row1, row2 = an_alignment in
        let alpha_for_row1 =
          let other_alphas =
            Alpha_family.with_respect_to row2.Aligned_row.row.Row.taxon alphas
          in
          other_alphas.@![row1.Aligned_row.row.Row.taxon]
        in
        let row1_log_probs =
          List.map row1.Aligned_row.aligned ~f:(fun phone ->
              alpha_for_row1.@![phone])
        in

        let alpha_for_row2 =
          let other_alphas =
            Alpha_family.with_respect_to row1.Aligned_row.row.Row.taxon alphas
          in
          other_alphas.@![row2.Aligned_row.row.Row.taxon]
        in
        let row2_log_probs =
          List.map row2.Aligned_row.aligned ~f:(fun phone ->
              alpha_for_row2.@![phone])
        in
        if explain then (
          Stdio.printf "%s " (Utils.word_of_phones row1.Aligned_row.aligned);
          List.iter row1_log_probs ~f:(fun x ->
              Stdio.printf "%.2f " (Probability.Log.to_float x));
          Stdio.printf "\n";
          Stdio.printf "%s " (Utils.word_of_phones row2.Aligned_row.aligned);
          List.iter row2_log_probs ~f:(fun x ->
              Stdio.printf "%.2f " (Probability.Log.to_float x));
          Stdio.printf "\n" );
        (Probability.Log.sum (row1_log_probs @ row2_log_probs), an_alignment)
      in
      let base_cognate_log_prob = Probability.to_log base_cognate_prob in
      let base_non_cognate_log_prob =
        Probability.(to_log (one - base_cognate_prob))
      in
      let cond_cognate_log_lik =
        Probability.Log.(cognate_log_lik + base_cognate_log_prob)
      in
      let cond_non_cognate_log_lik =
        Probability.Log.(non_cognate_log_lik + base_non_cognate_log_prob)
      in
      let total_lik =
        Probability.Log.lse cond_cognate_log_lik cond_non_cognate_log_lik
      in
      let cognate_log_prob =
        Probability.Log.(cond_cognate_log_lik - total_lik)
      in
      let non_cognate_log_prob =
        Probability.Log.(cond_non_cognate_log_lik - total_lik)
      in
      [
        Cognate (Probability.of_log cognate_log_prob, cognate_data);
        Not_cognate (Probability.of_log non_cognate_log_prob, non_cognate_data);
      ])
  |> List.concat

let init_alpha_family_inner taxon inner =
  Dict.find_or_add inner taxon ~default:Alpha.create

let initialise_parameters theta_family alpha_family taxons_and_phoneses
    initialiser =
  List.iter (Utils.unique_pairs taxons_and_phoneses)
    ~f:(fun taxons_and_phones ->
      let sorted_pair =
        Sorted_pair.of_tup taxons_and_phones
          ~compare:(fun (taxon1, _) (taxon2, _) -> Taxon.compare taxon1 taxon2)
      in
      let taxons = sorted_pair |> Sorted_pair.map ~f:Tuple.T2.get1 in
      let taxon1, taxon2 = taxons |> Sorted_pair.Derived.to_tup in
      let phones = sorted_pair |> Sorted_pair.map ~f:Tuple.T2.get2 in
      let phones1, phones2 =
        Sorted_pair.Derived.(
          phones |> map ~f:(fun ps -> Phone.null :: ps) |> to_tup)
      in
      let theta = Theta.create () in
      let alpha1 =
        Alpha_family.with_respect_to taxon2 alpha_family
        |> init_alpha_family_inner taxon1
      in
      let alpha2 =
        Alpha_family.with_respect_to taxon1 alpha_family
        |> init_alpha_family_inner taxon2
      in
      let phone_count1, phone_count2 =
        (List.length phones1, List.length phones2)
      in
      List.iter phones1 ~f:(fun p ->
          let prob =
            Probability.(
              Float.(1.0 / of_int phone_count1) |> of_float |> to_log)
          in
          alpha1.@[p] <- prob);
      List.iter phones2 ~f:(fun p ->
          let prob =
            Probability.(
              Float.(1.0 / of_int phone_count2) |> of_float |> to_log)
          in
          alpha2.@[p] <- prob);

      List.iter
        (List.cartesian_product (Phone.null :: phones1) (Phone.null :: phones2))
        ~f:(fun (p1, p2) ->
          let unnormed = initialiser p1 p2 in
          theta.@[(p1, p2) |> Sorted_pair.Derived.of_tup_unsafe] <- unnormed);

      let theta_total = Dict.data theta |> Utils.float_sum in
      let theta =
        Dict.map
          ~f:(fun x ->
            Float.(x / theta_total)
            |> Probability.of_float |> Probability.to_log)
          theta
      in
      theta_family.@[taxons] <- theta)

let print_dist ?(non_cognates = false) dist =
  List.iter
    ~f:(function
      | Cognate (p, (r1, r2)) ->
          Stdio.printf "Cognate %.4f %s %s\n" (Probability.to_float p)
            (Utils.word_of_phones r1.Aligned_row.aligned)
            (Utils.word_of_phones r2.Aligned_row.aligned)
      | Not_cognate (p, (r1, r2)) ->
          if non_cognates then
            Stdio.printf "Not cognate %.4f %s %s\n" (Probability.to_float p)
              (Utils.word_of_phones r1.Aligned_row.row.Row.tokens)
              (Utils.word_of_phones r2.Aligned_row.row.Row.tokens))
    dist;

  Stdio.print_endline ""

let possible_pairs rows =
  let gloss_groups = Hashtbl.create (module Int) in
  let () =
    List.iter rows ~f:(fun row ->
        let open Row in
        Dataset_utils.list_tbl_append ~key:row.gloss_id ~data:row gloss_groups)
  in
  Hashtbl.data gloss_groups
  |> List.map ~f:Utils.unique_pairs
  |> List.concat
  |> List.filter ~f:(fun (row1, row2) ->
         Taxon.(row1.Row.taxon <> row2.Row.taxon))

let score_graph rows dist =
  let result = Dict.create (module Int) in
  List.iter dist ~f:(function
    | Cognate (p, (row1, row2)) ->
        (* if row1.Aligned_row.row.Row.gloss_id = 47 then
           let r1, r2 = (row1.Aligned_row.row, row2.Aligned_row.row) in
           let open Row in
           Stdio.printf "%s %s %s %s %d %d %.6f\n" (Taxon.to_string r1.taxon)
             (Taxon.to_string r2.taxon)
             (Utils.word_of_phones r1.tokens)
             (Utils.word_of_phones r2.tokens)
             r1.id r2.id (Probability.to_float p) );*)
        Dataset_utils.list_tbl_append ~key:row1.Aligned_row.row.Row.id
          ~data:(row2.Aligned_row.row, Probability.to_float p)
          result;
        Dataset_utils.list_tbl_append ~key:row2.Aligned_row.row.Row.id
          ~data:(row1.Aligned_row.row, Probability.to_float p)
          result
    | Not_cognate _ -> ());

  (* All rows are self-cognate *)
  List.iter rows ~f:(fun row ->
      Dataset_utils.list_tbl_append ~key:row.Row.id ~data:(row, Float.infinity)
        result);
  result
