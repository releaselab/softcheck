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

module Make (E : Element) = struct
  module Set = Set.Make (E)
  module Map = Map.Make (E)
end
