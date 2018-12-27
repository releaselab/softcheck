open Batteries
open Set.Infix
open Utils

open Cao
open Cfg

module S = struct
  type ident = Cfg.ident
  type stmt_label = Cfg.stmt_label
  type vertex = Cfg.vertex
  type blocks = (Cfg.stmt_label, Cfg.vertex) Hashtbl.t
  type definition_location = Cfg.ident * Cfg.stmt_label option

  let get_idents_of_lv = let open Ast in function
      LVVar s -> [s]
    | LVStruct _ -> []
    | LVCont _ -> []

  let get_idents = let open Flow in function
      VDecl (VarD v) -> v.Ast.var_d_ids
    | VDecl (ContD c) -> c.Ast.cont_d_ids
    | CDecl c -> c.Ast.const_d_ids
    | Assign a -> List.fold_left (fun acc x -> acc @ get_idents_of_lv x) [] a.Ast.assign_ids
    | Sample lvs -> List.fold_left (fun acc x -> acc @ get_idents_of_lv x) [] lvs
    | Seq s -> [s.Ast.seqheader_var]
    | FCallS _
    | Ret _
    | Ite _
    | While _ -> []

  let find_assignments bs x =
    let open Flow in
    let open Set.Infix in
    let aux l b acc = match b with
        VDecl (VarD v) when Option.is_some v.Ast.var_d_init && List.exists ((=) x) (get_idents b) ->
          acc <-- l
      | VDecl (ContD _)
      | Assign _
      | Sample _
      | Seq _ when List.exists ((=) x) (get_idents b) -> acc <-- l
      | CDecl _
      | FCallS _
      | Ret _
      | Ite _
      | While _
      | VDecl _
      | Assign _
      | Sample _
      | Seq _ -> acc
    in Hashtbl.fold aux bs Set.empty

  let free_variables blocks =
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
    in let aux acc = let open Flow in function
        VDecl (VarD v) -> (match v.Ast.var_d_init with
          None -> acc
        | Some e -> free_variables_rec acc e)
      | VDecl (ContD c) -> List.fold_left free_variables_rec acc c.Ast.cont_d_init
      | CDecl c -> (match c.Ast.const_d_ann with
          NoConst -> acc
        | ConstInit e | ConstCond e -> free_variables_rec acc e)
      | Assign a -> List.fold_left free_variables_rec acc a.Ast.assign_values
      | Sample _ -> acc
      | FCallS f -> List.fold_left free_variables_rec acc f.Ast.fcalls_args
      | Seq s ->
          free_variables_rec (free_variables_rec acc s.Ast.seqheader_start_val) s.Ast.seqheader_end_val
      | Ret e | Ite e | While e -> free_variables_rec acc e
    in Hashtbl.fold (fun _ b acc -> aux acc b) blocks Set.empty

  let kill bs s = let open Flow in let open Set.Infix in match s with
    VDecl (VarD v) when Option.is_some v.Ast.var_d_init ->
      List.fold_left (fun acc x -> acc ||. Set.map (pair x % Option.some)
                                     (find_assignments bs x) <-- (x,None)) Set.empty (get_idents s)
  | VDecl (ContD _)
  | CDecl _
  | Assign _
  | Sample _
  | Seq _ ->
      List.fold_left (fun acc x -> acc ||. Set.map (pair x % Option.some)
                                     (find_assignments bs x) <-- (x,None)) Set.empty (get_idents s)
  | VDecl _
  | FCallS _
  | Ret _
  | Ite _
  | While _ -> Set.empty

  let gen l b = let open Flow in let open Set.Infix in match b with
    VDecl (VarD v) when Option.is_some v.Ast.var_d_init -> List.fold_left (fun acc lv -> acc <-- (lv, Some l)) Set.empty (get_idents b)
  | VDecl (ContD _)
  | CDecl _
  | Assign _
  | Sample _
  | Seq _ -> List.fold_left (fun acc lv -> acc <-- (lv, Some l)) Set.empty (get_idents b)
  | VDecl _
  | FCallS _
  | Ret _
  | Ite _
  | While _ -> Set.empty
end

include ReachingDefinitions.Make(Ast)(Cfg)(S)
