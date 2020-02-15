open Owl
open Base
open Lib
open Lib.Utils.Infix

let Utils.{ phones; words } = Utils.load_dataset ~verbose:true "../data/PIE.csv"

let first_lang, second_lang = ("Spanish", "Italian")

let spanish_phones, italian_phones = (phones @! "Spanish", phones @! "Italian")

let encode_spanish, decode_spanish = Utils.phone_coders spanish_phones

let encode_italian, decode_italian = Utils.phone_coders italian_phones

(* ('a, 'cmp) Base.Set.t -> *)
(* ('a, 'cmp) Base.Set.t -> *)
(* f:([ `Both of 'a * 'a | `Left of 'a | `Right of 'a ] -> unit) -> unit *)
let weights =
  let open Dense.Ndarray in
  let weights =
    Generic.create Bigarray.Int
      [| 1 + Set.length spanish_phones; 1 + Set.length italian_phones |]
      (-1)
  in
  Set.iter2 spanish_phones italian_phones ~f:(function
    | `Both (t1, t2) ->
        let open String in
        assert (t1 = t2);
        Generic.set weights [| encode_spanish t1; encode_italian t1 |] 1
    | _ -> ());
  weights

let spanish_words, italian_words = (words @! "Spanish", words @! "Italian")

let () = Stdio.print_endline @@ Int.to_string @@ List.length spanish_words

let () = Stdio.print_endline @@ Int.to_string @@ List.length italian_words

let () = Stdio.print_endline "Alignments:"

(* 'a list -> *)
(* 'b list -> f:('a -> 'b -> unit) -> unit Base.List.Or_unequal_lengths.t *)
let _ =
  List.iter2 spanish_words (List.tl_exn italian_words) ~f:(fun es it ->
      let spanish_encoded = List.to_array @@ List.map ~f:encode_spanish es in
      let italian_encoded = List.to_array @@ List.map ~f:encode_italian it in
      let _scores, pointers =
        Alignment.align weights spanish_encoded italian_encoded
      in
      Stdio.print_endline (String.concat es);
      Stdio.print_endline (String.concat it);
      let alignments =
        Alignment.traceback spanish_encoded italian_encoded pointers
      in
      List.iter
        ~f:(fun (a, b) ->
          Stdio.print_endline @@ Utils.aligned_to_string decode_spanish a;
          Stdio.print_endline @@ Utils.aligned_to_string decode_italian b)
        alignments;
      Stdio.print_endline "")
