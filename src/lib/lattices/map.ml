open Utils

module Make (D : sig
  include Utils.Collections.WithCollections

  val bottom_elems : Set.t
end)
(L : Sig.S) =
struct
  module Set = D.Set
  module Map = D.Map

  type property = L.property Map.t

  let bottom =
    Set.fold (fun x map -> Map.add x L.bottom map) D.bottom_elems Map.empty

  let lub x y =
    List.fold_left
      (fun acc (k, _) -> Map.add k (L.lub (Map.find k x) (Map.find k y)) acc)
      y (Map.bindings x)

  let equal = ( = )

  let to_string = Map.to_string ~fst:"[" ~lst:"]" ~sep:";" L.to_string

  let is_maximal _ = false

  let set x k v = Map.add k v x

  let get x k = Map.find k x
end
