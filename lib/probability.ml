open Base

type t = float [@@deriving sexp_of]

module Log = struct
  type t = float [@@deriving sexp_of]

  let of_float x =
    if Float.(x <= 0.0) then x
    else failwith @@ Printf.sprintf "Invalid log probability %.4f" x

  let of_float_unsafe = Fn.id

  let to_float = Fn.id

  let ( + ) = Float.( + )

  let ( - ) = Float.( - )

  (** LogSumExp: log(e^x + e^y) computed to avoid underflow *)
  let lse x y =
    let max_val, other = (Float.max x y, Float.min x y) in
    Float.(max_val + log1p (exp (other - max_val)))

  let sum = Utils.float_sum
end

let of_float x =
  if Float.(0.0 <= x && x <= 1.0) then x
  else failwith @@ Printf.sprintf "Invalid probability %.4f" x

let of_float_unsafe = Fn.id

let to_float = Fn.id

let of_log = Float.exp

let to_log = Float.log

let ( * ) = Float.( * )

let ( + ) = Float.( + )

let ( - ) = Float.( + )

let zero = 0.0

let one = 1.0

let compare = Float.compare
