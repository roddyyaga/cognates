open Owl
open Base
open Lib
open Lib.Utils.Infix

let Utils.{ phones; words; cogs } =
  Utils.load_dataset ~verbose:true "../data/PIE.csv"

let first_lang, second_lang = ("Spanish", "Italian")

let spanish_phones, italian_phones = (phones @! "Spanish", phones @! "Italian")

let encode_spanish, decode_spanish = Utils.phone_coders spanish_phones

let encode_italian, decode_italian = Utils.phone_coders italian_phones

let weights =
  let open Dense.Ndarray in
  let weights =
    Generic.create Bigarray.Int
      [| 1 + Set.length spanish_phones; 1 + Set.length italian_phones |]
      (-1)
  in
  Set.iter spanish_phones ~f:(fun t1 ->
      Set.iter italian_phones ~f:(fun t2 ->
          let weight =
            match String.(t1 = t2) with
            | true -> 4
            | false -> (
                match Bool.(Lib.Phon.syl t1 = Lib.Phon.syl t2) with
                | true -> 2
                | false -> -1 )
          in
          Generic.set weights [| encode_spanish t1; encode_italian t2 |] weight));
  weights

let _ =
  List.iter (Hashtbl.keys cogs) ~f:(fun i ->
      let es_opt = List.Assoc.find ~equal:String.( = ) (cogs @! i) "Spanish" in
      let it_opt = List.Assoc.find ~equal:String.( = ) (cogs @! i) "Italian" in
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
              Stdio.print_endline @@ Utils.aligned_to_string decode_spanish a;
              Stdio.print_endline @@ Utils.aligned_to_string decode_italian b)
            alignments;
          Stdio.print_endline @@ Int.to_string score;
          Stdio.print_endline ""
      | _ -> ())
