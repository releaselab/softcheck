module Make (D : sig
  type t

  val compare : t -> t -> int

  val default : t

  val to_string : t -> string
end) =
struct
  include Map.Make (D)

  let find x m = match find_opt x m with None -> D.default | Some x -> x
end
