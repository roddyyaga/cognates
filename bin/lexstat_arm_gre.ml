open Base

let filenames =
  [
    "/home/roddy/iii/project/code/python/ARM_GRE_lexstat_0.csv";
    "/home/roddy/iii/project/code/python/ARM_GRE_lexstat_1.csv";
    "/home/roddy/iii/project/code/python/ARM_GRE_lexstat_2.csv";
    "/home/roddy/iii/project/code/python/ARM_GRE_lexstat_3.csv";
    "/home/roddy/iii/project/code/python/ARM_GRE_lexstat_4.csv";
    "/home/roddy/iii/project/code/python/ARM_GRE_lexstat_5.csv";
  ]

let () =
  List.iter filenames ~f:(fun filename ->
      Stdio.print_endline "yo";
      let df = Owl.Dataframe.of_csv ~sep:'\t' filename in
      let scoring_rows =
        Lib.Pair_accuracy.rows_of_dataframe df ~gloss_column:"GLOSSID"
          ~reference_column:"COGID" ~given_column:"COGNATES" ~id_column:"ID"
      in
      let cog_acc, non_acc = scoring_rows |> Lib.Pair_accuracy.accuracies in
      Stdio.printf "%.6f %.6f \n" cog_acc non_acc)
