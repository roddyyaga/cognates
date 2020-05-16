open Base

type t = float

module Log = struct
  type t = float

  let of_float = Fn.id

  let to_float = Fn.id

  let ( + ) = Float.( + )

  let ( - ) = Float.( + )
end

let of_float = Fn.id

let to_float = Fn.id

let of_log = Float.exp

let to_log = Float.log

let ( * ) = Float.( * )

let ( + ) = Float.( + )

let ( - ) = Float.( + )
