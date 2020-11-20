Code for https://www.aclweb.org/anthology/2020.conll-1.38/

The system is written in OCaml. To run it, you should:
- Install [opam](https://opam.ocaml.org/)
- Clone the repo
- Create an opam switch with OCaml version 4.10.0
- Install dependencies with `opam install . --deps-only`
- Run relevant code with `dune exec -- ./bin/name.exe` where there is a file `name.ml` in the bin directory
