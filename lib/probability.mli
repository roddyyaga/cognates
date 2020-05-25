type t [@@deriving sexp_of]

module Log : sig
  type t [@@deriving sexp_of]

  val of_float : float -> t

  val of_float_unsafe : float -> t
  (** Don't check in range [-inf, 0] *)

  val to_float : t -> float

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val lse : t -> t -> t

  val sum : t list -> t

  val compare : t -> t -> int
end

val of_float : float -> t

val of_float_unsafe : float -> t
(** Don't check in range [0, 1] *)

val to_float : t -> float

val of_log : Log.t -> t

val to_log : t -> Log.t

val ( * ) : t -> t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val zero : t

val one : t

val compare : t -> t -> int
