open Base

type t = int

include Comparable.S with type t := t

include Sexpable.S with type t := t

val to_string : t -> string
