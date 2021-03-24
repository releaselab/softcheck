module Make (D : Utils.Collections.WithCollections) :
  Sig.S with type property = D.Set.t

module Make_reverse (D : sig
  include Utils.Collections.WithCollections

  val bottom : Set.t
end) : Sig.S with type property = D.Set.t
