module Make (D : Utils.Collections.WithCollections) = struct
  type property = D.Set.t

  let bottom = D.Set.empty

  let lub = D.Set.union

  let equal = D.Set.equal

  let to_string = D.Set.to_string ~fst:"[" ~lst:"]" ~sep:";"

  let is_maximal _ = false
end

module Make_reverse (D : sig
  include Utils.Collections.WithCollections

  val bottom : Set.t
end) =
struct
  type property = D.Set.t

  let bottom = D.bottom

  let lub = D.Set.inter

  let equal = D.Set.equal

  let to_string = D.Set.to_string ~fst:"[" ~lst:"]" ~sep:";"

  let is_maximal _ = false
end
