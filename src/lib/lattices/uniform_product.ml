open Utils

module Make
  (L : Sig.S) (N : sig
    val n : int
  end) =
struct
  type property = L.property list

  let bottom = List.init N.n (fun _ -> L.bottom)

  let lub x y = List.map2 L.lub x y

  let equal = ( = )

  let to_string = List.to_string ~fst:"[" ~sep:";" ~lst:"]" L.to_string

  let is_maximal _ = false
end
