include Stdlib.Set

module type OrderedType = sig
  include Stdlib.Set.OrderedType

  val to_string : t -> string
end

module type S = sig
  include Stdlib.Set.S

  val to_string : fst:string -> lst:string -> sep:string -> t -> string
end

module Make (E : OrderedType) = struct
  include Make (E)

  let to_string ~fst ~lst ~sep s =
    let l = List.of_seq (to_seq s) in
    List.to_string ~fst ~lst ~sep
      (fun d -> Printf.sprintf "%a" (fun () -> E.to_string) d)
      l
end
