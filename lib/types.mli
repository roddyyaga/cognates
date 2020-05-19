open Core

module type String_alias = sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool

  include Tuple.Hashable_sexpable with type t := t

  include Comparator.S with type t := t
end

module String_alias : String_alias

module Taxon : String_alias

module Phone : sig
  include String_alias

  val null : t
end

module Phone_with_taxon : sig
  type t = { taxon: Taxon.t; s: Phone.t }

  val compare : t -> t -> int
end

module Row : sig
  type t = { id: int; tokens: Phone.t list; taxon: Taxon.t; gloss_id: int }
  [@@deriving fields]
end

type encoder = Phone.t -> int

type decoder = int -> Phone.t

type encoders = (Taxon.t, encoder) Dict.t

type decoders = (Taxon.t, decoder) Dict.t
