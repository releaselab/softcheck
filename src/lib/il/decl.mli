type t = { decl_name : string; decl_location : Label.t }

include Utils.Collections.WithCollections with type t := t
