open Lib
open Base
open Types
open Dict.Infix

let a_taxon = "a" |> Taxon.of_string

let b_taxon = "b" |> Taxon.of_string

let a_phones =
  [ "-"; "a"; "b"; "c"; "d"; "e"; "x"; "w" ] |> List.map ~f:Phone.of_string

let b_phones =
  [ "-"; "a"; "q"; "c"; "f"; "g"; "y"; "z" ] |> List.map ~f:Phone.of_string

let taxons = [ a_taxon; b_taxon ]

let phones = Dict.create (module Taxon)

let () = phones.@[a_taxon] <- Set.of_list (module Phone) a_phones

let () = phones.@[b_taxon] <- Set.of_list (module Phone) b_phones

let encoders = Initialisation.all_encoders taxons phones

let decoders = Initialisation.all_decoders taxons phones

let weights_tables = Initialisation.new_weights_tables taxons phones

let make_rows raw_data =
  List.mapi raw_data ~f:(fun i (w1, w2) ->
      ( {
          Row.id = (2 * i) + 1;
          tokens = Utils.phones_of_word w1;
          taxon = a_taxon;
          gloss_id = i + 1;
        },
        {
          Row.id = (2 * i) + 2;
          tokens = Utils.phones_of_word w2;
          taxon = b_taxon;
          gloss_id = i + 1;
        } ))

let row_pairs =
  make_rows
    [
      ("abc", "aqc");
      ("dbe", "fqg");
      ("acac", "acacfg");
      ("xwxwx", "yzyzy");
      ("wxwxw", "zyzyz");
      ("abcdexw", "zyfgqca");
      ("acde", "yzq");
    ]

let thetas = Em.Theta_family.create ()

let alphas = Em.Alpha_family.create ()

let () =
  Em.initialise_parameters thetas alphas
    [ (a_taxon, a_phones); (b_taxon, b_phones) ] (fun _ _ -> 1.0)

let () =
  Stdio.print_endline @@ Em.Theta_family.show Probability.Log.sexp_of_t thetas

let () =
  Stdio.print_endline @@ Em.Alpha_family.show Probability.Log.sexp_of_t alphas

let expect =
  Em.expectations encoders decoders row_pairs weights_tables
    ~base_cognate_prob:(Probability.of_float 0.5)

let maximise = Em.maximise ~smoothing:1.0

let dist1 = expect alphas thetas

let () = Em.print_dist dist1

let alpha2, theta2 = maximise dist1

let dist2 = expect alpha2 theta2

let () = Stdio.print_endline ""

let () =
  Stdio.print_endline @@ Em.Theta_family.show Probability.Log.sexp_of_t theta2

let () =
  Stdio.print_endline @@ Em.Alpha_family.show Probability.Log.sexp_of_t alpha2

let () = Em.print_dist dist2

let alpha, theta = maximise dist2

let () = Stdio.print_endline ""

let () =
  Stdio.print_endline @@ Em.Theta_family.show Probability.Log.sexp_of_t theta

let () =
  Stdio.print_endline @@ Em.Alpha_family.show Probability.Log.sexp_of_t alpha

let dist = expect alpha theta

let () = Em.print_dist dist

let alpha, theta = maximise dist

let () = Stdio.print_endline ""

let () =
  Stdio.print_endline @@ Em.Theta_family.show Probability.Log.sexp_of_t theta

let () =
  Stdio.print_endline @@ Em.Alpha_family.show Probability.Log.sexp_of_t alpha

let dist = expect alpha theta

let () = Em.print_dist dist

let alpha, theta = maximise dist

let () = Stdio.print_endline ""

let () =
  Stdio.print_endline @@ Em.Theta_family.show Probability.Log.sexp_of_t theta

let () =
  Stdio.print_endline @@ Em.Alpha_family.show Probability.Log.sexp_of_t alpha

let dist = expect alpha theta

let () = Em.print_dist dist
