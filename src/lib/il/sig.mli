module type Ordered = sig
  include Set.OrderedType

  val equal : t -> t -> bool

  val to_string : t -> string
end

module type Expr = Utils.Collections.WithCollections
