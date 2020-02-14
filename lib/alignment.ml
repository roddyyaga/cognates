open Owl
open Base

type token = int

module Pointers = struct
  type t = Left | Up | Diagonal [@@deriving enum]
end

let align weights first second =
  let open Dense.Ndarray in
  let first_dim = Array.length first + 1 in
  let second_dim = Array.length second + 1 in
  let scores = Generic.zeros Bigarray.Int [| first_dim; second_dim |] in
  let pointers = Generic.zeros Bigarray.Int [| first_dim; second_dim; 3 |] in
  (* Initialise *)
  List.iter (List.range 1 first_dim) ~f:(fun y ->
      let score =
        Generic.get scores [| y - 1; 0 |]
        + Generic.get weights [| first.(y - 1); 0 |]
      in
      Generic.set scores [| y; 0 |] score;
      Generic.set pointers [| y; 0; Pointers.to_enum Up |] 1);

  List.iter (List.range 1 second_dim) ~f:(fun x ->
      let score =
        Generic.get scores [| 0; x - 1 |]
        + Generic.get weights [| 0; second.(x - 1) |]
      in
      Generic.set scores [| 0; x |] score;
      Generic.set pointers [| 0; x; Pointers.to_enum Left |] 1);

  (* Align *)
  List.iter (List.range 1 first_dim) ~f:(fun y ->
      List.iter (List.range 1 second_dim) ~f:(fun x ->
          let left =
            Generic.get scores [| y; x - 1 |]
            + Generic.get weights [| 0; second.(x - 1) |]
          in
          let up =
            Generic.get scores [| y - 1; x |]
            + Generic.get weights [| first.(y - 1); 0 |]
          in
          let diagonal =
            Generic.get scores [| y - 1; x - 1 |]
            + Generic.get weights [| first.(y - 1); second.(x - 1) |]
          in
          let open Pointers in
          let previous_scores =
            [ (left, Left); (up, Up); (diagonal, Diagonal) ]
          in
          let sorted_previous =
            List.sort ~compare:(fun (a, _) (b, _) -> b - a) previous_scores
          in
          let first_optimal = fst (List.hd_exn sorted_previous) in
          let all_optimal =
            List.take_while
              ~f:(fun (score, _) -> score = first_optimal)
              sorted_previous
          in
          List.iter all_optimal ~f:(fun (score, pointer) ->
              Generic.set scores [| y; x |] score;
              Generic.set pointers [| y; x; Pointers.to_enum pointer |] 1)));
  (scores, pointers)

let traceback first second pointers =
  let open Dense.Ndarray in
  let open Pointers in
  let one_step_back ((y, x), (f, s)) =
    let f pointer =
      let present =
        Generic.get pointers [| y; x; Pointers.to_enum pointer |] <> 0
      in
      match present with
      | false -> []
      | true -> (
          match pointer with
          | Left -> [ ((y, x - 1), (0 :: f, second.(x - 1) :: s)) ]
          | Up -> [ ((y - 1, x), (first.(y - 1) :: f, 0 :: s)) ]
          | Diagonal ->
              [ ((y - 1, x - 1), (first.(y - 1) :: f, second.(x - 1) :: s)) ] )
    in
    List.concat [ f Left; f Up; f Diagonal ]
  in
  let rec iterate current finished =
    match current with
    | [] -> finished
    | _ ->
        let nexts = List.map ~f:one_step_back current in
        let new_currentses, new_finished_unfiltered =
          List.unzip
          @@ List.map2_exn nexts current ~f:(fun next (_, (f, s)) ->
                 match next with [] -> ([], Some (f, s)) | some -> (some, None))
        in
        let new_currents = List.concat_no_order new_currentses in
        let new_finished = List.filter_opt new_finished_unfiltered in
        iterate new_currents (new_finished @ finished)
  in
  iterate [ ((Array.length first, Array.length second), ([], [])) ] []

let score weights first second =
  assert (List.length first = List.length second);
  let open Dense.Ndarray in
  List.fold ~init:0 ~f:( + )
  @@ List.map ~f:(fun (f, s) -> Generic.get weights [| f; s |])
  @@ List.zip_exn first second
