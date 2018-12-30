open Batteries

let aexp_expr acc =
  let open Set.Infix in
  let open Ast in function
      Var _
    | Lit _
    | FunCall _
    | StructProj _
    | UnaryOp _
    | Access _
    | Cast _ -> acc
    | BinaryOp binop as e -> match binop.binaryop_op with
        ArithOp _ -> acc <-- e
      | BitOp _
      | BitsSROp _
      | Concat
      | BoolOp _
      | CmpOp _ -> acc

let contains_ident v = let open Ast in
  let rec contains_ident_rowapat = function
      CElem e -> contains_ident_rec e
    | CRange c -> contains_ident_rec c.crange_start_val || contains_ident_rec c.crange_end_val
  and contains_ident_apat = let open Ast in function
      VectP r -> contains_ident_rowapat r
    | MatP m -> contains_ident_rowapat m.matp_row || contains_ident_rowapat m.matp_col
  and contains_ident_rec = let open Ast in function
      Var _ as x -> v = x
    | FunCall f -> List.exists contains_ident_rec f.funcall_args
    | StructProj s -> contains_ident_rec s.structproj_id
    | UnaryOp unop -> contains_ident_rec unop.unaryop_expr
    | BinaryOp binop -> contains_ident_rec binop.binaryop_l_expr || contains_ident_rec binop.binaryop_r_expr
    | Access a -> contains_ident_rec a.access_container_id || contains_ident_apat a.access_pattern
    | Cast c -> contains_ident_rec c.cast_expr
    | Lit _ -> false
  in contains_ident_rec

let contains_lv lv = Ast.lv_to_expr lv |> contains_ident

let aexp_block = let open Ast in let open Set.Infix in
  let rec tydecl_aexp = function
      BitsD b -> aexp_expr Set.empty b.bits_d_size
    | ModD (ModNum e) -> aexp_expr Set.empty e
    | VectorD v -> aexp_expr Set.empty v.vector_d_size
    | MatrixD m -> aexp_expr
                     (aexp_expr (tydecl_aexp m.matrix_d_type)
                        m.matrix_d_cols) m.matrix_d_rows
    | TySynD t -> tydecl_aexp t.tysyn_d_type
    | IntD
    | RIntD
    | BoolD
    | VoidD
    | ModD _ -> Set.empty
  in let rowapat_aexp = function
      CElem e -> aexp_expr Set.empty e
    | CRange r -> aexp_expr (aexp_expr Set.empty r.crange_start_val) r.crange_end_val
  in let apat_aexp = function
      VectP r -> rowapat_aexp r
    | MatP m -> rowapat_aexp m.matp_col ||. rowapat_aexp m.matp_row
  in let rec lval_aexp = function
      LVVar _ -> Set.empty
    | LVStruct lvs -> lval_aexp lvs.lvstruct_id
    | LVCont lvc -> apat_aexp lvc.lvcont_pattern
  in function
      Flow.VDecl v -> (match v with
        VarD vd -> (match vd.var_d_init with
          None -> Set.empty
        | Some e -> aexp_expr Set.empty e)
      | ContD cd -> List.fold_left (fun acc x -> aexp_expr acc x) Set.empty cd.cont_d_init)
    | Flow.CDecl c -> Set.union (match c.const_d_ann with
        NoConst -> Set.empty
      | ConstInit e
      | ConstCond e -> aexp_expr Set.empty e)
      (tydecl_aexp c.const_d_type)
    | Flow.Assign a -> List.fold_left aexp_expr Set.empty a.assign_values
    | Flow.Sample lvs -> List.fold_left (fun acc x -> lval_aexp x ||. acc) Set.empty lvs
    | Flow.FCallS fc -> List.fold_left (fun acc x -> aexp_expr acc x) Set.empty fc.fcalls_args
    | Flow.Ret e 
    | Flow.Ite e
    | Flow.While e -> aexp_expr Set.empty e
    | Flow.Seq s -> aexp_expr
                      (aexp_expr (match s.seqheader_increase with
                           None -> Set.empty
                         | Some e -> aexp_expr Set.empty e)
                         s.seqheader_end_val) s.seqheader_start_val
