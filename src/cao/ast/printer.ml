open Batteries
open BatIO
open Ast
open Format

let sprint_list ?(first = "") ?(last = "") ?(sep = "") to_string l =
  let strout = output_string () in
  let () = List.print ~first ~last ~sep write_string strout (List.map to_string l) in
  close_out strout

type expr = Ast.expr

let uop_to_string = function
    Sym   -> "-"
  | Not   -> "~"
  | BNot  -> "!"

let arithop_to_string = function
    Plus  -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Power -> "**"
  | Div   -> "/"
  | ModOp -> "%"

let boolop_to_string = function
    And   -> "&&"
  | Or    -> "||"
  | Xor   -> "^^"

let bwop_to_string = function
    BWOr  -> "&"
  | BWAnd -> "|"
  | BWXor -> "^"

let srop_to_string = function
    SUp       -> "<<"
  | SDown     -> ">>"
  | SRUp      -> "|>"
  | SRDown    -> "<|"

let cop_to_string = function
    Eq    -> "=="
  | Neq   -> "!="
  | Lt    -> "<"
  | Let   -> "<="
  | Gt    -> ">"
  | Get   -> ">="


let binop_to_string = function
    ArithOp aOp   -> arithop_to_string aOp
  | BoolOp bOp    -> boolop_to_string bOp
  | BitOp bWOp    -> bwop_to_string bWOp
  | BitsSROp sROp -> srop_to_string sROp
  | CmpOp cOp     -> cop_to_string cOp
  | Concat        -> "@"

let iaop_to_string = function
    IPlus   -> "+"
  | IMinus  -> "-"
  | ITimes  -> "*"
  | IPower  -> "**"
  | IDiv    -> "/"
  | IModOp  -> "%"

let rec iexpr_to_string = function
    IInt n -> Num.to_string n
  | IInd s -> s
  | IArith a -> iarith_to_string a
  | ISym ie -> "-" ^ iexpr_to_string ie

and iarith_to_string ia =
  sprintf "%s %s %s" (iexpr_to_string ia.l_iexpr) (iaop_to_string ia.aop)
    (iexpr_to_string ia.r_iexpr)

let rec monomial_to_string m = match m.coef with
    CoefI expr -> iexpr_to_string expr
  | CoefP pol -> polinomial_to_string pol

and polinomial_to_string pol =
  sprint_list ~first:"" ~last:"" ~sep:"" monomial_to_string pol.monomials

and literal_to_string = function
    BLit b    -> string_of_bool b
  | ILit n    -> Num.string_of_num n
  | BSLit bsl -> bslit_to_string bsl
  | PLit pol   -> polinomial_to_string pol

and rowapat_to_string =
  function
    CElem     ce -> expr_to_string ce
  | CRange    cr -> sprintf "%s..%s" (expr_to_string cr.crange_start_val)
                      (expr_to_string cr.crange_end_val)

and apat_to_string = function
    VectP rowAPat -> rowapat_to_string rowAPat
  | MatP matP     -> sprintf "%s,%s" (rowapat_to_string matP.matp_row)
                       (rowapat_to_string matP.matp_col)

and bits_d_to_string b =
  sprintf "%s bits [%s]"
    (match b.bits_d_signal with U -> "unsigned" | S -> "signed")
    (expr_to_string b.bits_d_size)

and mod_d_to_string m =
  sprintf "mod [%s]"
    (match m with
       ModNum e -> expr_to_string e
     | ModPol m -> sprintf "%s<%s> of %s" (tydecl_to_string m.modpol_field_t)
                     m.modpol_var (polinomial_to_string m.modpol_poly))

and vector_d_to_string v =
  sprintf "vector [%s] of %s" (expr_to_string v.vector_d_size)
    (tydecl_to_string v.vector_d_type)

and matrix_d_to_string m =
  sprintf "matrix [%s,%s] of %s" (expr_to_string m.matrix_d_rows)
    (expr_to_string m.matrix_d_cols) (tydecl_to_string m.matrix_d_type)

and tydecl_to_string = function
    IntD              -> "int"
  | RIntD             -> "register int"
  | BoolD             -> "bool"
  | VoidD             -> "void"
  | BitsD bits_d      -> bits_d_to_string bits_d
  | ModD mod_d        -> mod_d_to_string mod_d
  | VectorD vector_d  -> vector_d_to_string vector_d
  | MatrixD matrix_d  -> matrix_d_to_string matrix_d
  | TySynD tysyn_d    -> tysyn_d.tysyn_d_id

and expr_to_string = function
    Var s -> s
  | Lit l -> literal_to_string l
  | FunCall fc ->
      sprint_list ~first:(fc.funcall_id ^ "(") ~last:")" ~sep:", " expr_to_string fc.funcall_args
  | StructProj sp -> sprintf "%s.%s" (expr_to_string sp.structproj_id) sp.structproj_field
  | UnaryOp uo -> sprintf "%s(%s)" (uop_to_string uo.unaryop_op)
                    (expr_to_string uo.unaryop_expr)
  | BinaryOp bo -> sprintf "(%s)%s(%s)" (expr_to_string bo.binaryop_l_expr)
                     (binop_to_string bo.binaryop_op) (expr_to_string bo.binaryop_r_expr)
  | Access access -> sprintf "%s[%s]" (expr_to_string access.access_container_id)
                       (apat_to_string access.access_pattern)
  | Cast c -> sprintf "(%s) %s" (tydecl_to_string c.cast_type)
                (expr_to_string c.cast_expr)

let vardecl_to_string vd =
  let ids,typedecl,init = match vd with
      VarD var_d ->
        let ids = sprint_list ~sep:"," identity var_d.var_d_ids in
        let typedecl = tydecl_to_string var_d.var_d_type in
        let init = match var_d.var_d_init with
            None -> ""
          | Some expr -> " := " ^ (expr_to_string expr) in
        ids,typedecl,init
    | ContD cont_d ->
        let ids = sprint_list ~sep:"," identity cont_d.cont_d_ids in
        let typedecl = tydecl_to_string cont_d.cont_d_type in
        let init = match cont_d.cont_d_init with
            [] -> ""
          | [e] -> " := " ^ (expr_to_string e)
          | l -> sprint_list ~sep:"," expr_to_string l |>
                 sprintf " := { %s }"
        in ids,typedecl,init
  in sprintf "def %s : %s%s" ids typedecl init

let const_d_to_string cd =
  let ids = sprint_list ~sep:"," identity cd.const_d_ids in
  let typedecl = tydecl_to_string cd.const_d_type in
  let init = match cd.const_d_ann with
      NoConst -> ""
    | ConstInit e -> " := " ^ (expr_to_string e)
    | ConstCond e -> expr_to_string e |> sprintf " { %s }"
  in sprintf "def const %s : %s%s" ids typedecl init

let rec lval_to_string = function
    LVVar s -> s
  | LVStruct s -> sprintf "%s.%s" (lval_to_string s.lvstruct_id) s.lvstruct_field
  | LVCont c -> sprintf "%s %s" (lval_to_string c.lvcont_id) (apat_to_string c.lvcont_pattern)

let assign_to_string a =
  let ids = sprint_list ~sep:"," lval_to_string a.assign_ids in
  let exprs = sprint_list ~sep:"," expr_to_string a.assign_values in
  sprintf "%s := %s" ids exprs

let sample_to_string l =
  let ids = sprint_list ~sep:"," lval_to_string l in
  ids ^ " := $"

let fcalls_to_string f =
  let id = f.fcalls_id in
  let args = sprint_list ~sep:"," expr_to_string f.fcalls_args in
  sprintf "%s(%s)" id args

let rec ite_to_string i =
  sprintf "if (%s) {\n%s\n}%s" (expr_to_string i.ite_condition)
    (stmt_to_string i.if_branch)
    (match i.else_branch with
       None -> ""
     | Some e -> sprintf " else {\n%s\n}" (stmt_to_string e))

and seq_to_string s =
  sprintf "seq %s := %s to %s%s {\n%s\n}" s.seq_header.seqheader_var
    (expr_to_string s.seq_header.seqheader_start_val)
    (expr_to_string s.seq_header.seqheader_end_val)
    (match s.seq_header.seqheader_increase with
       None -> ""
     | Some e -> " by " ^ (expr_to_string e)) (stmt_to_string s.seq_body)

and stmt_to_string s = match s with
    Block b -> sprint_list ~sep:"\n" stmt_to_string b
  | Stmt (_, stmt) -> match stmt with
      VDecl varDecl -> vardecl_to_string varDecl
    | CDecl const_d -> const_d_to_string const_d
    | Assign assign -> assign_to_string assign
    | Sample lVals -> sample_to_string lVals
    | FCallS fcalls -> fcalls_to_string fcalls
    | Ret lExpr -> "return " ^ expr_to_string lExpr
    | Ite ite -> ite_to_string ite
    | Seq seq -> seq_to_string seq
    | While loop -> sprintf "while (%s) {\n%s\n}" (expr_to_string loop.loop_condition)
                      (stmt_to_string loop.loop_body)
