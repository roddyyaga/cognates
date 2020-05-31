open Base
include Base.Hashtbl

let incr_float table key ~by =
  let current = find_or_add table key ~default:(fun () -> 0.0) in
  let new_total = Float.(current + by) in
  set table ~key ~data:new_total

let extend table key new_items =
  let current = find_or_add table key ~default:(fun () -> []) in
  let new_value = current @ new_items in
  set table ~key ~data:new_value

let cons table key new_item =
  let current = find_or_add table key ~default:(fun () -> []) in
  let new_value = new_item :: current in
  set table ~key ~data:new_value

module Infix = struct
  let ( .@![] ) table key = find_exn table key

  let ( .@?[] ) table key = find table key

  let ( .@[]<- ) table key data = set ~key ~data table
end

let sorted_items t ~compare =
  let open Infix in
  let sorted_keys =
    keys t |> List.sort ~compare:(fun x y -> compare t.@![x] t.@![y])
  in
  List.map sorted_keys ~f:(fun k -> (k, t.@![k]))

let to_alist2 t =
  fold t ~init:[] ~f:(fun ~key ~data accum ->
      let new_tuples =
        to_alist data |> List.map ~f:(fun (k2, v) -> (key, k2, v))
      in
      accum @ new_tuples)

let list_partion_n (type a) (module M : Key.S with type t = a) ~f xs =
  let dict = create (module M) in
  List.iter xs ~f:(fun x -> cons dict (f x) x);
  data dict
