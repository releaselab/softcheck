module T = struct
  type t = { var_id : int; var_name : string }

  let compare v_1 v_2 = Int.compare v_1.var_id v_2.var_id

  let to_string v = Printf.sprintf "%s^%d" v.var_name v.var_id
end

include T
module Set = Set.Make (T)
module Map = Map.Make (T)
