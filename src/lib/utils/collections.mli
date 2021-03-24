module type Element = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val to_string : t -> string
end

module type WithCollections = sig
  include Element

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module Make (E : Element) : sig
  module Set : Set.S with type elt = E.t

  module Map : Map.S with type key = E.t
end
