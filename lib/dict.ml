open Base

let ( .@![] ) table key = Hashtbl.find_exn table key

let ( .@?[] ) table key = Hashtbl.find table key

let ( .@[]<- ) table key data = Hashtbl.set ~key ~data table
