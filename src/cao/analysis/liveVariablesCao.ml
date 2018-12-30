open Batteries
open Set.Infix

include Analysis.Live_variables.Make(Ast)(Cfg)(struct
    let free_variables e =
      let open Ast in
      let rec free_variables_rowapat acc = function
          CElem e -> free_variables_rec acc e
        | CRange c -> free_variables_rec (free_variables_rec acc c.crange_start_val) c.crange_end_val
      and free_variables_apat acc = function
          VectP v -> free_variables_rowapat acc v
        | MatP m -> free_variables_rowapat (free_variables_rowapat acc m.matp_row) m.matp_col
      and free_variables_rec acc = function
          Var v -> acc <-- v
        | Lit _ -> acc
        | FunCall f -> List.fold_left free_variables_rec acc f.funcall_args
        | StructProj s -> free_variables_rec acc s.structproj_id
        | UnaryOp uop -> free_variables_rec acc uop.unaryop_expr
        | BinaryOp binop -> free_variables_rec (free_variables_rec acc binop.binaryop_l_expr) binop.binaryop_r_expr
        | Access a -> free_variables_apat (free_variables_rec acc a.access_container_id) a.access_pattern
        | Cast c -> free_variables_rec acc c.cast_expr
      in free_variables_rec Set.empty e

    let kill = let open Flow in let open Set.Infix in function
        Assign a -> List.fold_left (fun acc lv -> Ast.lv_to_expr lv |> free_variables ||. acc) Set.empty a.Ast.assign_ids
      | Sample s -> List.fold_left (fun acc lv -> Ast.lv_to_expr lv |> free_variables ||. acc) Set.empty s
      | Seq s -> Set.singleton s.Ast.seqheader_var
      | VDecl _
      | CDecl _
      | FCallS _
      | Ret _
      | Ite _
      | While _ -> Set.empty

    let gen = let open Flow in function
        Assign a -> List.fold_left (fun acc rv -> free_variables rv ||. acc) Set.empty a.Ast.assign_values
      | VDecl (VarD d) -> (match d.Ast.var_d_init with
          None -> Set.empty
        | Some i -> free_variables i)
      | VDecl (ContD c) -> List.fold_left (fun acc x -> free_variables x ||. acc) Set.empty c.Ast.cont_d_init
      | CDecl c -> (match c.Ast.const_d_ann with
          NoConst -> Set.empty
        | ConstInit e | ConstCond e -> free_variables e)
      | FCallS f -> List.fold_left (fun acc x -> free_variables x ||. acc) Set.empty f.Ast.fcalls_args
      | Ret e
      | Ite e
      | While e -> free_variables e
      | Seq _
      | Sample _ -> Set.empty
  end)
