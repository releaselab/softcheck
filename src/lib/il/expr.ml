open! Core

module type S = sig
  include Comparable.S
  include Sexpable.S with type t := t

  val to_string : t -> string
end
