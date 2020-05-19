open Core
open Types

type phone_pair = Phone_with_taxon.t Sorted_pair.t

module Aligned_row : sig
  type t = { row: Row.t; aligned: Phone.t list }
end

type word_pair_distribution =
  | Cognate of Probability.t * (Aligned_row.t * Aligned_row.t)
  | Not_cognate of Probability.t * (Aligned_row.t * Aligned_row.t)

module Theta : sig
  type 'a t = (Phone.t Sorted_pair.Derived.t, 'a) Dict.t [@@deriving sexp_of]

  val create : unit -> 'a t
end

module Theta_family : sig
  type 'a t = (Taxon.t Sorted_pair.Derived.t, 'a Theta.t) Dict.t
  [@@deriving sexp_of]

  val create : unit -> 'a t

  val show : ('a -> Sexp.t) -> 'a t -> string
end

module Alpha : sig
  type 'a t = (Phone.t, 'a) Dict.t [@@deriving sexp_of]

  val create : unit -> 'a t
end

module Alpha_family : sig
  type 'a inner = (Taxon.t, 'a Alpha.t) Dict.t [@@deriving sexp_of]

  type 'a t = (Taxon.t, 'a inner) Dict.t [@@deriving sexp_of]

  val create : unit -> 'a t

  val with_respect_to : Taxon.t -> 'a t -> 'a inner

  val show : ('a -> Sexp.t) -> 'a t -> string
end

val estimate_theta :
  smoothing:float -> float Theta.t -> Probability.Log.t Theta.t

val estimate_alpha : float Alpha.t -> Probability.Log.t Alpha.t

val maximise :
  smoothing:float ->
  word_pair_distribution list ->
  Probability.Log.t Alpha_family.t * Probability.Log.t Theta_family.t

val expectations :
  ?explain:bool ->
  encoders ->
  decoders ->
  (Row.t * Row.t) list ->
  ( Taxon.t Dict.key Sorted_pair.Derived.t,
    (float, 'a) Owl.Dense.Ndarray.Generic.t )
  Dict.t ->
  Probability.Log.t Alpha_family.t ->
  Probability.Log.t Theta_family.t ->
  base_cognate_prob:Probability.t ->
  word_pair_distribution list

val initialise_parameters :
  Probability.Log.t Theta_family.t ->
  Probability.Log.t Alpha_family.t ->
  (Taxon.t * Phone.t list) list ->
  (Phone.t -> Phone.t -> float) ->
  unit

val print_dist : ?non_cognates:bool -> word_pair_distribution list -> unit

val possible_pairs : Row.t list -> (Row.t * Row.t) list

val score_graph :
  Row.t list ->
  word_pair_distribution list ->
  (int, (Row.t * float) list) Dict.t
