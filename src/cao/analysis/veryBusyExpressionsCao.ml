open Batteries
open Set.Infix

include Analysis.Very_busy_expressions.Make(Ast)(Printer)(Cfg)(struct
    let aexp_star p =
      let graph = Cfg.generate_from_program p in
      let blocks = Cfg.get_blocks graph in
      Hashtbl.fold (fun _ b acc -> AexpCao.aexp_block b ||. acc) blocks Set.empty

    let kill aexp_star = let open Flow in function
        VDecl (Ast.VarD v) when Option.is_some v.Ast.var_d_init ->
          List.fold_left (fun acc x -> Set.filter (Ast.contains_ident (Var x)) acc) aexp_star v.Ast.var_d_ids
      | VDecl (Ast.ContD c) ->
          List.fold_left (fun acc x -> Set.filter (Ast.contains_ident (Var x)) acc) aexp_star c.Ast.cont_d_ids
      | Assign a -> List.fold_left (fun acc x -> Set.filter (Ast.contains_lv x) acc) aexp_star a.Ast.assign_ids
      | Sample lvs -> List.fold_left (fun acc x -> Set.filter (Ast.contains_lv x) acc) aexp_star lvs
      | Seq s -> Set.filter (Ast.contains_ident (Var s.Ast.seqheader_var)) aexp_star
      | VDecl _
      | CDecl _
      | FCallS _
      | Ret _
      | Ite _
      | While _ -> Set.empty

    let gen b = let open Flow in
      let exprs = match b with
          VDecl (Ast.VarD v) -> (match v.Ast.var_d_init with
            Some e -> [e]
          | None -> [])
        | VDecl (Ast.ContD c) -> c.Ast.cont_d_init
        | Assign a -> a.Ast.assign_values
        | Seq s -> [s.Ast.seqheader_start_val; s.Ast.seqheader_end_val]
        | FCallS f -> f.Ast.fcalls_args
        | Ret e
        | Ite e
        | While e -> [e]
        | Sample _
        | CDecl _ -> []
      in List.fold_left AexpCao.aexp_expr Set.empty exprs
  end)

