open Core
open Types

let float_sum = List.fold ~init:0.0 ~f:Float.( + )

let float_mean xs = Float.(float_sum xs / of_int (List.length xs))

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

let phones_of_word w =
  w |> String.to_list |> List.map ~f:Char.to_string
  |> List.map ~f:Phone.of_string

let word_of_phones ps = ps |> List.map ~f:Phone.to_string |> String.concat

let is_list_prefix ~equal ~prefix xs =
  List.equal equal prefix (List.take xs (List.length prefix))

let take_last xs n = List.drop xs (List.length xs - n)
