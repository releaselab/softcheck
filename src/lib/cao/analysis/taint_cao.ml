open Batteries

open Cao
open Cfg

include Taint.Make(Ast)(Cfg)(struct
    include ReachingDefinitionsCao.S

    let rec eval s = let open Ast in
      let open Taint_lattice in function
          Var v -> Map.find v s
        | Lit _ -> Taint_lattice.Element false
        | FunCall fc when fc.Ast.funcall_id = "read" -> Element true
        | FunCall fc -> 
            List.fold_left (fun acc arg -> join_taint acc (eval s arg))
              bottom fc.Ast.funcall_args
        | StructProj sp -> eval s sp.Ast.structproj_id
        | UnaryOp uop -> eval s uop.Ast.unaryop_expr
        | Access a -> eval s a.Ast.access_container_id
        | Cast c -> eval s c.Ast.cast_expr
        | BinaryOp bop ->
            let eval_e1 = eval s bop.Ast.binaryop_l_expr in
            let eval_e2 = eval s bop.Ast.binaryop_r_expr in
            join_taint eval_e1 eval_e2

    let rec get_id_from_lv = function
        Ast.LVVar lvv -> lvv
      | Ast.LVStruct lvs -> get_id_from_lv lvs.Ast.lvstruct_id
      | Ast.LVCont lvc -> get_id_from_lv lvc.Ast.lvcont_id

    let ta s b = let open Flow in match b with
      VDecl (VarD v) -> (match v.Ast.var_d_init with
        Some rv -> let eval_rv = eval s rv in
          List.map (fun lv -> (lv, eval_rv)) (get_idents b)
      | None -> List.map (fun lv -> (lv, Taint_lattice.bottom)) (get_idents b))
    | VDecl (ContD c) -> List.map2 (fun lv rv -> (lv, eval s rv))
                           c.Ast.cont_d_ids c.Ast.cont_d_init
    | Assign a -> List.map2 (fun lv rv -> let lv2 = get_id_from_lv lv in
                              (lv2, eval s rv)) a.Ast.assign_ids
                    a.Ast.assign_values
    | Sample lvals ->
        List.map (fun lv -> (get_id_from_lv lv, Taint_lattice.bottom)) lvals
    | Seq seq -> let eval_s = eval s seq.Ast.seqheader_start_val in
        let eval_e = eval s seq.Ast.seqheader_end_val in
        let eval_t = (match seq.Ast.seqheader_increase with
            Some e ->
              Taint_lattice.lub (Taint_lattice.lub eval_s eval_e) (eval s e)
          | None -> Taint_lattice.lub eval_s eval_e) in
        [seq.Ast.seqheader_var, eval_t]
    | CDecl _
    | FCallS _
    | Ret _
    | Ite _
    | While _ -> []
  end)