open Base
open Lib

let () = Stdio.print_endline "Heyyy"

let () =
  failwith
    "what the fucking fuck are you doing you piece of shitprint_thetas_pie"

let data_paths =
  [ (*"/home/roddy/iii/project/code/data/BAI.csv";*)
    (*     "/home/roddy/iii/project/code/data/GER.csv"; *)
    (*     "/home/roddy/iii/project/code/data/IDS.csv"; *)
    (*     "/home/roddy/iii/project/code/data/IEL.csv"; *)
    (*     "/home/roddy/iii/project/code/data/JAP.csv"; *)
    (*     "/home/roddy/iii/project/code/data/KSL.csv"; *)
    (*     "/home/roddy/iii/project/code/data/OUG.csv"; *)
    (*     "/home/roddy/iii/project/code/data/PAN.csv"; *)
    (*     "/home/roddy/iii/project/code/data/PIE.csv"; *)
    (*     "/home/roddy/iii/project/code/data/ROM.csv"; *)
    (*     "/home/roddy/iii/project/code/data/SIN.csv"; *)
    (*     "/home/roddy/iii/project/code/data/SLV.csv"; *) ]

let get_name path =
  path |> String.split ~on:'/' |> List.last_exn |> String.split ~on:'.'
  |> List.hd_exn

let () =
  List.iter data_paths ~f:(fun path ->
      Run_em.run
        ~params_callback:(Run_em.print_sorted_thetas 50)
        Run_em.basic_initialiser ~smoothing:0.0001 ~base_cognate_prob:0.005 path
        (get_name path) 8 ())

(*let () =
  List.iter data_paths ~f:(fun path ->
      Stdio.print_endline path;
      Dataset_utils.get_missing_tokens path)*)
