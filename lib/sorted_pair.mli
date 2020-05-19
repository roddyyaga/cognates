type 'a t [@@deriving sexp]

val of_tup : 'a * 'a -> compare:('a -> 'a -> int) -> 'a t

val to_tup : 'a t -> 'a * 'a

module Hashable_t (S : Core.Tuple.Hashable_sexpable) :
  Base.Hashtbl.Key.S with type t = S.t t

module Derived : sig
  type 'a t [@@deriving sexp]

  val to_tup : 'a t -> 'a * 'a

  val of_tup_unsafe : 'a * 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  module Hashable_t (S : Core.Tuple.Hashable_sexpable) :
    Base.Hashtbl.Key.S with type t = S.t t
end

val map : 'a t -> f:('a -> 'b) -> 'b Derived.t

val to_derived : 'a t -> 'a Derived.t
