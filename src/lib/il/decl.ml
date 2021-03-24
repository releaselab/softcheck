module T = struct
      type t = { decl_name : string; decl_location : Label.t }
      [@@deriving ord, eq]

      let to_string x =
        let aux (name : string) (loc : int) = [%string "%{name}^%{loc#Int}"] in
        aux x.decl_name x.decl_location
    end

    include T
    include Utils.Collections.Make (T)
