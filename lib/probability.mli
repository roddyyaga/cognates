type t

module Log : sig
  type t

  val of_float : float -> t

  val to_float : t -> float

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t
end

val of_float : float -> t

val to_float : t -> float

val of_log : Log.t -> t

val to_log : t -> Log.t

val ( * ) : t -> t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t
