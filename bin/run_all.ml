open Base

let basic_data_paths =
  [
    "/home/roddy/iii/project/code/data/BAI.csv";
    "/home/roddy/iii/project/code/data/GER.csv";
    (*     "/home/roddy/iii/project/code/data/IDS.csv"; *)
    "/home/roddy/iii/project/code/data/JAP.csv";
    "/home/roddy/iii/project/code/data/OUG.csv";
    "/home/roddy/iii/project/code/data/PIE.csv";
    "/home/roddy/iii/project/code/data/ROM.csv";
    (*     "/home/roddy/iii/project/code/data/SIN.csv"; *)
    "/home/roddy/iii/project/code/data/SLV.csv";
  ]

let iel_path = "/home/roddy/iii/project/code/data/IEL.csv"

let ksl_path = "/home/roddy/iii/project/code/data/KSL.csv"

let pan_path = "/home/roddy/iii/project/code/data/PAN.csv"

let get_name path =
  path |> String.split ~on:'/' |> List.last_exn |> String.split ~on:'.'
  |> List.hd_exn

let () =
  List.iter basic_data_paths ~f:(fun path ->
      Lib.Run_em.run ~row_format:Basic Lib.Run_em.basic_initialiser
        ~smoothing:0.0001 ~base_cognate_prob:0.005 path (get_name path) 6 ())

let () =
  Stdio.print_endline iel_path;
  Lib.Run_em.run ~row_format:Iel Lib.Run_em.basic_initialiser ~smoothing:0.0001
    ~base_cognate_prob:0.005 iel_path (get_name iel_path) 6 ()

let () =
  Stdio.print_endline ksl_path;
  Lib.Run_em.run ~row_format:Ksl Lib.Run_em.basic_initialiser ~smoothing:0.0001
    ~base_cognate_prob:0.005 ksl_path (get_name ksl_path) 6 ()

let () =
  Stdio.print_endline pan_path;
  Lib.Run_em.run ~row_format:Pan Lib.Run_em.basic_initialiser ~smoothing:0.0001
    ~base_cognate_prob:0.005 pan_path (get_name pan_path) 6 ()

(*let () =
  List.iter basic_data_paths ~f:(fun path ->
      Stdio.print_endline path;
      Lib.Dataset_utils.(get_missing_tokens ~row_format:Basic path))

let () =
  Stdio.print_endline iel_path;
  Lib.Dataset_utils.(get_missing_tokens ~row_format:Iel iel_path)

let () =
  Stdio.print_endline ksl_path;
  Lib.Dataset_utils.(get_missing_tokens ~row_format:Ksl ksl_path)

let () =
  Stdio.print_endline pan_path;
  Lib.Dataset_utils.(get_missing_tokens ~row_format:Pan pan_path)*)
