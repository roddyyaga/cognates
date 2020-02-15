open Base
open Owl

module Infix = struct
  let ( @! ) x y = Hashtbl.find_exn x y

  let ( @? ) x y = Hashtbl.find x y
end

open Infix

type dataset = {
  phones: (string, (string, String.comparator_witness) Set.t) Hashtbl.t;
  words: (string, string list list) Hashtbl.t;
}

(** Load a dataset from a CSV at a path *)
let load_dataset ?verbose path =
  let verbose =
    match verbose with Some true -> true | Some false | None -> false
  in
  let df = Dataframe.of_csv path in

  if verbose then (
    let x, y = Dataframe.shape df in
    Printf.sprintf "Shape: (%d, %d)" x y |> Stdio.print_endline;

    let tokenses = Dataframe.get_col_by_name df "Tokens" in
    match tokenses with
    | String_Series strings -> Array.iter ~f:Stdio.print_endline strings
    | _ -> assert false );

  let phones = Hashtbl.create (module String) in
  let words = Hashtbl.create (module String) in
  let process_row =
    let open Dataframe in
    function
    | [| _id; String taxon; _gloss; _gloss_id; _ipa; String tokens; _cog_id |]
      ->
        let token_list = String.split ~on:' ' tokens in
        let previous_set =
          match phones @? taxon with
          | None -> Set.empty (module String)
          | Some set -> set
        in
        let new_set =
          List.fold token_list ~init:previous_set ~f:(fun set s ->
              Set.add set s)
        in
        Hashtbl.set ~key:taxon ~data:new_set phones;
        let previous_list = Option.value ~default:[] @@ words @? taxon in
        let new_list = token_list :: previous_list in
        Hashtbl.set ~key:taxon ~data:new_list words
    | other -> Stdio.print_endline (Int.to_string @@ Array.length other)
  in
  let () = Dataframe.iter_row process_row df in
  let () =
    Hashtbl.iter_keys phones ~f:(fun taxon ->
        Stdio.print_endline taxon;
        Set.to_list (phones @! taxon)
        |> String.concat ~sep:" " |> Stdio.print_endline)
  in
  { phones; words }

let index_exn ~equal xs element =
  match List.findi ~f:(fun _ x -> equal x element) xs with
  | Some (i, _) -> i
  | None -> failwith "Element not found"

(** Make functions to encode phones as integers.
    Encoding starts at 1 (0 represents a deletion).*)
let phone_coders phone_set =
  let as_list = Base.Set.to_list phone_set in
  let encode token = 1 + index_exn ~equal:String.equal as_list token in
  let decode i = match i with 0 -> "-" | _ -> List.nth_exn as_list (i - 1) in
  (encode, decode)

let aligned_to_string decoder tokens =
  String.concat @@ List.map ~f:decoder tokens
