open Base
open Owl

let weights =
  let open Dense.Ndarray in
  Generic.of_arrays Bigarray.Int
    [|
      [| 0; -1; -1; -1; -1; -1 |];
      [| -1; 1; -1; -1; -1; -1 |];
      [| -1; -1; 1; -1; -1; -1 |];
      [| -1; -1; -1; 1; -1; -1 |];
      [| -1; -1; -1; -1; 1; -1 |];
      [| -1; -1; -1; -1; -1; 1 |];
    |]

let () = Owl_pretty.print_dsnda ~max_col:100 ~max_row:100 weights

let token_of_char = function
  | 'G' -> 1
  | 'C' -> 2
  | 'A' -> 3
  | 'T' -> 4
  | 'U' -> 5
  | _ -> failwith "Unexpected character"

let char_of_token = function
  | 1 -> 'G'
  | 2 -> 'C'
  | 3 -> 'A'
  | 4 -> 'T'
  | 5 -> 'U'
  | 0 -> '-'
  | _ -> failwith "Unexpected character"

let print_alignment (first, second) =
  Stdio.print_endline @@ String.of_char_list @@ List.map ~f:char_of_token first;
  Stdio.print_endline @@ String.of_char_list @@ List.map ~f:char_of_token second;
  Stdio.print_endline ""

let first = Array.map ~f:token_of_char (String.to_array "GATTACA")

let second = Array.map ~f:token_of_char (String.to_array "GCATGCU")

let scores, pointers, score = Lib.Alignment.align weights first second

let () =
  List.iter ~f:print_alignment @@ Lib.Alignment.traceback first second pointers
