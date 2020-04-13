open Base
open Owl

type alignment = int list * int list [@@deriving compare]

let print_alignment (first, second) =
  Stdio.print_endline @@ String.concat ~sep:" "
  @@ List.map ~f:Int.to_string first;
  Stdio.print_endline @@ String.concat ~sep:" "
  @@ List.map ~f:Int.to_string second;
  Stdio.print_endline ""

let weight_matrix_gen vocab_size =
  let open QCheck.Gen in
  let open Dense.Ndarray in
  map
    (Generic.of_arrays Bigarray.Int)
    (array_repeat (vocab_size + 1)
       (array_size (return (vocab_size + 1)) small_signed_int))

(* let print_weight_matrix matrix = Owl_pretty.print_dsnda matrix *)

let token_string_gen vocab_size =
  let open QCheck.Gen in
  array_size (int_range 1 50) @@ int_range 1 (vocab_size + 0)

let print_token_string tokens =
  String.concat ~sep:" " @@ Array.to_list @@ Array.map ~f:Int.to_string tokens

let test_case_gen size =
  let open QCheck.Gen in
  triple (token_string_gen size) (token_string_gen size)
    (weight_matrix_gen size)

let print_test_case (f, s, w) =
  String.concat ~sep:"\n"
    [ print_token_string f; print_token_string s; Owl_pretty.dsnda_to_string w ]

let make_test_case size =
  QCheck.make ~print:print_test_case (test_case_gen size)

let test =
  QCheck.Test.make ~name:"test" ~count:10 (make_test_case 100) (fun (f, s, w) ->
      let open Dense.Ndarray in
      let scores, pointers, returned_score = Lib.Alignment.align w f s in
      let alignment_score =
        Generic.get scores [| Array.length f; Array.length s |]
      in
      let alignments = Lib.Alignment.traceback f s pointers in
      let traceback_scores =
        List.map ~f:(fun (f', s') -> Lib.Alignment.score w f' s') alignments
      in
      let () =
        match List.length alignments > 100 with
        | true -> Stdio.print_endline @@ Int.to_string @@ List.length alignments
        | false -> ()
      in
      (* Stdio.print_endline @@ Int.to_string @@ List.hd_exn traceback_scores;
         Stdio.print_endline @@ Int.to_string @@ alignment_score;
         Owl_pretty.print_dsnda scores;
         List.iter ~f:print_alignment alignments; *)
      alignment_score = returned_score
      && List.for_all
           ~f:(fun x -> x = List.hd_exn traceback_scores)
           traceback_scores
      && Option.is_none
         @@ List.find_a_dup ~compare:compare_alignment alignments
      && List.hd_exn traceback_scores = alignment_score)

let () =
  let quickcheck = QCheck_alcotest.to_alcotest test in
  let open Alcotest in
  run "Alignments" [ ("quickcheck", [ quickcheck ]) ]
