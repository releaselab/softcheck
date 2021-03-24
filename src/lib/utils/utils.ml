let cartesian_product xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

let fst3 (x, _, _) = x

let trd (_, _, x) = x

let ( --- ) lower upper =
  let rec helper u i = if i > u then [] else i :: helper u (i + 1) in
  helper upper lower

let ( -- ) lower upper = lower --- (upper - 1)

let dualize f (xs, ys) = (f xs, f ys)

let dual_map f (xs, ys) = dualize (List.map f) (xs, ys)

let dual_fold_left f init (xs, ys) = dualize (List.fold_left f init) (xs, ys)

let pair x y = (x, y)

let rev_pair x y = (y, x)

module Collections = Collections
module Set = Set
module List = List
module Map = Map
