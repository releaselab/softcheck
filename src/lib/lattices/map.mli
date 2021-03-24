module Make (D : sig
  include Utils.Collections.WithCollections

  val bottom_elems : Set.t
end)
(L : Sig.S) : sig
  include Sig.S

  val set : property -> D.t -> L.property -> property

  val get : property -> D.t -> L.property
end
