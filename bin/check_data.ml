open Base
open Owl

let df = Dataframe.of_csv "../data/BAI.csv"

let () =
  let x, y = Dataframe.shape df in
  Printf.sprintf "Shape: (%d, %d)" x y |> Stdio.print_endline

let () =
  let tokenses = Dataframe.get_col_by_name df "Tokens" in
  match tokenses with
  | String_Series strings -> Array.iter ~f:Stdio.print_endline strings
  | _ -> assert false

let phones = Hashtbl.create (module String)

let ( @! ) x y = Hashtbl.find_exn x y

let ( @? ) x y = Hashtbl.find x y

let process_row =
  let open Dataframe in
  function
  | [| _id; String taxon; _gloss; _gloss_id; _ipa; String tokens; _cog_id |] ->
      let previous_set =
        match phones @? taxon with
        | None -> Set.empty (module String)
        | Some set -> set
      in
      let new_set =
        List.fold (String.split ~on:' ' tokens) ~init:previous_set
          ~f:(fun set s -> Set.add set s)
      in
      Hashtbl.set ~key:taxon ~data:new_set phones
  | other -> Stdio.print_endline (Int.to_string @@ Array.length other)

let () = Dataframe.iter_row process_row df

let () =
  Hashtbl.iter_keys phones ~f:(fun taxon ->
      Stdio.print_endline taxon;
      Set.to_list (phones @! taxon)
      |> String.concat ~sep:" " |> Stdio.print_endline)
