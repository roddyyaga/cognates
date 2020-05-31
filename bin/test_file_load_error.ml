let data_path = "/home/roddy/iii/project/code/data/BAI.csv"

let dfs = List.init 2000 (fun _n -> Owl.Dataframe.of_csv data_path)

let () = print_endline "Done!"
