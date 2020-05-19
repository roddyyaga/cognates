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

module String_alias : String_alias = struct
  include String
end

module Taxon = String_alias

module Phone = struct
  include String_alias

  let null = "-" |> of_string
end

module Phone_with_taxon = struct
  type t = { taxon: Taxon.t; s: Phone.t }

  let compare x y = Taxon.compare x.taxon y.taxon
end

module Row = struct
  type t = { id: int; tokens: Phone.t list; taxon: Taxon.t; gloss_id: int }
  [@@deriving fields]
end

type encoder = Phone.t -> int

type decoder = int -> Phone.t

type encoders = (Taxon.t, encoder) Dict.t

type decoders = (Taxon.t, decoder) Dict.t
