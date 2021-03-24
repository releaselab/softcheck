include Stdlib.Map

module type S = sig
  include S

  val to_string :
    fst:string -> lst:string -> sep:string -> ('a -> string) -> 'a t -> string
end

module Make (E : sig
  include OrderedType

  val to_string : t -> string
end) =
struct
  include Make (E)

  let to_string ~fst ~lst ~sep f s =
    let l = bindings s in
    List.to_string ~fst ~lst ~sep
      (fun (k, d) ->
        Printf.sprintf "%a: %a" (fun () -> E.to_string) k (fun () -> f) d)
      l
end
