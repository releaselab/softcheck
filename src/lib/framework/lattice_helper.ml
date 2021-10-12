module Make (L : S.Lattice) = struct
  include L

  type property = t

  let lub = join

  let equal x y = L.leq x y && L.leq y x

  let is_maximal _ = false
end
