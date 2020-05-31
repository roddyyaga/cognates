open Core

type 'a t = 'a * 'a [@@deriving sexp]

let of_tup (x, y) ~compare = if compare x y <= 0 then (x, y) else (y, x)

let to_tup = Fn.id

module Hashable_t (S : Tuple.Hashable_sexpable) = struct
  type nonrec t = S.t t

  module M = Tuple.Hashable_t (S) (S)

  let compare x y = M.compare (to_tup x) (to_tup y)

  let sexp_of_t x = M.sexp_of_t (to_tup x)

  let hash x = M.hash (to_tup x)
end

module Derived = struct
  type 'a t = 'a * 'a [@@deriving sexp]

  let to_tup = Fn.id

  let of_tup_unsafe = Fn.id

  let map (x, y) ~f = (f x, f y)

  let map2 (x, y) (a, b) ~f = (f x a, f y b)

  let equal (x, y) (x', y') ~equal = equal x x' && equal y y'

  module Hashable_t (S : Tuple.Hashable_sexpable) = struct
    type nonrec t = S.t t

    module M = Tuple.Hashable_t (S) (S)

    let compare x y = M.compare (to_tup x) (to_tup y)

    let sexp_of_t x = M.sexp_of_t (to_tup x)

    let hash x = M.hash (to_tup x)
  end
end

let map (x, y) ~f = (f x, f y)

let to_derived = map ~f:Fn.id

let equal (x, y) (x', y') ~equal = equal x x' && equal y y'
