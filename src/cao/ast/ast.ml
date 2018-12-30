(* -------------------------------------------------------------------- *)
open Num
open Batteries

type label = int

type ident = string

(* -------------------------------------------------------------------- *)
type iBOp =
  | IAnd
  | IOr
  | IXor

type iAOp =
  | IPlus
  | IMinus
  | ITimes
  | IPower
  | IDiv
  | IModOp

type iExpr =
  | IInt of num
  | IInd of string
  | IArith of iarith
  | ISym of iExpr

and iarith = {
  aop : iAOp ;
  l_iexpr : iExpr ;
  r_iexpr : iExpr ;
}

type iCond =
  | IBool of bool
  | IBInd of string
  | INot of iCond
  | IBoolOp of iboolop
  | ILeq of iExpr
  | IEq of iExpr

and iboolop = {
  bop : iBOp ;
  l_bool : iCond ;
  r_bool : iCond ;
}

(*let ( +. ) x y = IArith({ aop = IPlus ; l_iexpr = x ; r_iexpr = y })*)

let ( -. ) x y = IArith({ aop = IMinus ; l_iexpr = x ; r_iexpr = y })

(* let ( *. ) x y = IArith({ aop = ITimes ; l_iexpr = x ; r_iexpr = y }) *)

let ( **. ) x y = IArith({ aop = IPower ; l_iexpr = x ; r_iexpr = y })

let ( /. ) x y = IArith({ aop = IDiv ; l_iexpr = x ; r_iexpr = y })

let ( %. ) x y = IArith({ aop = IModOp ; l_iexpr = x ; r_iexpr = y })

let ( ==. ) x y = IEq(IArith({ aop = IMinus ; l_iexpr = x ; r_iexpr = y }))

let ( /=. ) x y = INot(x ==. y)

let ( ||. ) x y = IBoolOp({ bop = IOr ; l_bool = x ; r_bool = y })

let ( ^^. ) x y = IBoolOp({ bop = IXor ; l_bool = x ; r_bool = y })

let ( &&. ) x y = IBoolOp({ bop = IAnd ; l_bool = x ; r_bool = y })

(* -------------------------------------------------------------------- *)
type pol = { monomials : mon list }

and mon = { coef : mCoef ; base : mBase ; }

and mCoef =
  | CoefI of iExpr
  | CoefP of pol

and mBase =
  | EZero
  | MExpI of mexpi

and mexpi = {
  var : string ;
  exp : num
}

let monToPol = fun m ->
  match m.coef, m.base with
  | CoefP p, EZero -> p
  | _ -> { monomials = [m] }

let intC i = CoefI (IInt(i))

let polC p = CoefP(p)

let ( +. ) m p = { monomials = p.monomials @ [m] }

let ( *. ) coef base = { coef = coef ; base = base }

let ( ^. ) s i =
  match i with
  | Int 0 -> EZero
  | _ -> MExpI { var = s ; exp = i }

let degree p =
  let maximum xs =
    let rec maximum2 max xs =
      match xs with
      | [] -> max
      | h :: t -> if h > max then maximum2 h t else maximum2 max t
    in
    maximum2 (List.hd xs) xs
  in

  let polExp m =
    match m.base with
    | EZero -> num_of_int 0
    | MExpI e  -> e.exp
  in

  maximum (List.map polExp p.monomials)

let rec neg m =
  match m.coef with
  | CoefI(IInt i) -> { coef = CoefI(IInt(minus_num i)) ; base = m.base }
  | CoefI(i) -> { coef = CoefI(ISym(i)) ; base = m.base }
  | CoefP(p) -> { coef = CoefP( { monomials = List.map neg p.monomials } ) ; base = m.base }

let coeficient m =
  match m.coef with
  | CoefI c -> { monomials = [{ coef = CoefI(c) ; base = EZero }] }
  | CoefP p -> p

let getMonVar m =
  match m.base with
  | EZero -> None
  | MExpI(e) -> Some e.var

let getMonExp m =
  match m.base with
  | EZero -> num_of_int 0
  | MExpI(e) -> e.exp

let polyToMono s p i =
  match s,p,i with
  | _, { monomials = [ { coef = CoefI(IInt(Int 0)) ; base = EZero } ] }, _ -> None
  | Some x, { monomials = [ { coef = CoefI(IInt(c)) ; base = EZero } ] }, _ -> Some { coef = CoefI(IInt(c)) ; base = MExpI( { var = x ; exp = i } ) }
  | Some x, _, _ -> Some { coef = CoefP(p) ; base = MExpI({ var = x ; exp = i }) }
  | _, _, _ -> prerr_string ("<Language.CAO.Semantics>:<polyToMono>: unexpected input") ; exit(1)

let null p =
  match p.monomials with
  | [] -> true
  | _ -> false

let rec checkMon_ m =
  match m.coef with
  | CoefI(_) -> true
  | CoefP(p) -> isValid p.monomials

and checkPol_ s ms =
  let num_zero = num_of_int 0 in

  match s,ms with
  | ind, [m] -> (getMonVar m = None && getMonExp m = num_zero) || getMonVar m = ind
  | ind, (m1 :: m0 :: ms) -> (getMonExp m1 > getMonExp m0) && (getMonVar m1 = ind) && checkMon_ m1 && checkPol_ ind (m0::ms)
  | _, _ -> prerr_string ("<Language.CAO.Common>:<checkPol_>: unexpected empty list of monomials") ; exit(1)

and isValid = function
  | m :: ms -> checkPol_ (getMonVar m) (m :: ms)
  | _ -> false

(* -------------------------------------------------------------------- *)
type literal =
  | BLit of bool
  | ILit of num
  | BSLit of bslit
  | PLit of pol

and bslit = {
  signal : sign ;
  bitstring : bool list
}

and sign = U | S

let sign_prefix = function
  | U -> "0b"
  | S -> "1b"

let rec bool_list_to_string = function
  | [] -> ""
  | [b] -> if b then "1" else "0"
  | b :: bs -> (if b then "1" else "0") ^ bool_list_to_string bs

let bslit_to_string bs = sign_prefix bs.signal ^ bool_list_to_string bs.bitstring

(* -------------------------------------------------------------------- *)
type expr =
  | Var of string
  | Lit of literal
  | FunCall of funcall
  | StructProj of structproj
  | UnaryOp of unaryop
  | BinaryOp of binaryop
  | Access of access
  | Cast of cast

and funcall = {
  funcall_id : string ;
  funcall_args : expr list ;
}

and structproj = {
  structproj_id : expr ;
  structproj_field : string ;
}

and unaryop = {
  unaryop_op : uOp ;
  unaryop_expr : expr ;
}

and binaryop = {
  binaryop_op : binOp ;
  binaryop_l_expr : expr ;
  binaryop_r_expr : expr ;
}

and access = {
  access_container_id : expr ;
  access_pattern : aPat ;
}

and cast = {
  cast_type : tyDecl ;
  cast_expr : expr ;
}

and uOp = Sym | Not | BNot

and binOp =
  | ArithOp of aOp
  | BoolOp of bOp
  | BitOp of bWOp
  | BitsSROp of sROp
  | CmpOp of cOp
  | Concat

and aOp = Plus | Minus | Times | Power | Div | ModOp

and cOp = Eq | Neq | Lt | Let | Gt | Get

and bOp = And | Or | Xor

and bWOp = BWOr | BWAnd | BWXor

and sROp = SUp | SDown | SRUp | SRDown

and aPat =
  | VectP of rowAPat
  | MatP of matp

and rowAPat =
  | CElem of expr
  | CRange of crange

and crange = {
  crange_start_val : expr ;
  crange_end_val : expr ;
}

and colAPat = rowAPat

and matp = {
  matp_row : rowAPat ;
  matp_col : colAPat
}

(* -------------------------------------------------------------------- *)
and tyDecl =
  | IntD
  | RIntD
  | BoolD
  | VoidD
  | BitsD of bits_d
  | ModD of mod_d
  | VectorD of vector_d
  | MatrixD of matrix_d
  | TySynD of tysyn_d

and bits_d = {
  bits_d_signal : sign ;
  bits_d_size : expr ;
}

and vector_d = {
  vector_d_size : expr ;
  vector_d_type : tyDecl ;
}

and matrix_d = {
  matrix_d_rows : expr ;
  matrix_d_cols : expr ;
  matrix_d_type : tyDecl ;
}

and mod_d =
  | ModNum of expr
  | ModPol of modpol

and modpol = {
  modpol_field_t : tyDecl ;
  modpol_var : string ;
  modpol_poly : pol ;
}

and tysyn_d = {
  tysyn_d_id : string ;
  tysyn_d_type : tyDecl ;
}

(* -------------------------------------------------------------------- *)
let arith_op_to_iarith_op = function
  | Plus -> IPlus
  | Minus -> IMinus
  | Times -> ITimes
  | Power -> IPower
  | Div -> IDiv
  | ModOp -> IModOp

(* -------------------------------------------------------------------- *)
type constAnn =
  | NoConst
  | ConstInit of expr
  | ConstCond of expr

(* -------------------------------------------------------------------- *)
type arg =
  | Arg of narg
  | ArgConst of argconst

and narg = {
  narg_id : string ;
  narg_type : tyDecl ;
}

and argconst = {
  argconst_id : string ;
  argconst_type : tyDecl ;
  argconst_condition : constAnn
}

(* -------------------------------------------------------------------- *)
type lVal =
  | LVVar of string
  | LVStruct of lvstruct
  | LVCont of lvcont

and lvstruct = {
  lvstruct_id : lVal ;
  lvstruct_field : string ;
}

and lvcont = {
  lvcont_id : lVal ;
  lvcont_pattern : aPat
}

(* -------------------------------------------------------------------- *)
type stm =
  | VDecl of varDecl
  | CDecl of const_d
  | Assign of assign
  | Sample of lVal list
  | FCallS of fcalls
  | Ret of expr
  | Ite of ite
  | Seq of seq
  | While of loop

and stmt = Stmt of label * stm | Block of stmt list

and assign = {
  assign_ids : lVal list ;
  assign_values : expr list ;
}

and fcalls = {
  fcalls_id : string ;
  fcalls_args : expr list ;
}

and ite = {
  ite_condition : expr ;
  if_branch : stmt ;
  else_branch : stmt option
}

and seq = {
  seq_header : seqheader ;
  seq_body : stmt
}

and loop = {
  loop_condition : expr ;
  loop_body : stmt
}

and seqheader = {
  seqheader_var : string ;
  seqheader_start_val : expr ;
  seqheader_end_val : expr ;
  seqheader_increase : expr option ;
}

(* -------------------------------------------------------------------- *)
and def =
  | VarDef of varDecl
  | ConstDef of const_d
  | FunDef of func
  | TyDef of tyDef

and varDecl =
  | VarD of var_d
  | ContD of cont_d

and var_d = {
  var_d_ids : string list ;
  var_d_type : tyDecl ;
  var_d_init : expr option
}

and cont_d = {
  cont_d_ids : string list ;
  cont_d_type : tyDecl ;
  cont_d_init : expr list
}

and const_d = {
  const_d_ids : string list ;
  const_d_type : tyDecl ;
  const_d_ann : constAnn ;
}

and func = {
  func_id : string ;
  func_args : arg list ;
  func_ret_type : tyDecl ;
  func_body : stmt ;
}

and tyDef =
  | TySynDef of tysyn_d
  | StructDecl of struct_d

and struct_d = {
  struct_d_id : string ;
  struct_d_fields : (string * tyDecl) list
}

(* -------------------------------------------------------------------- *)
type program = { defs : def list ; main : func option }

let funcs_of_program p =
  let funcs = List.filter_map (function FunDef f -> Some f | _ -> None) p.defs in
  match p.main with
    None -> funcs
  | Some m -> m::funcs

let rec lv_to_expr = function
    LVVar s -> Var s
  | LVStruct lvs -> StructProj ({ structproj_id = lv_to_expr lvs.lvstruct_id; structproj_field = lvs.lvstruct_field })
  | LVCont lvc -> Access ({ access_container_id = lv_to_expr lvc.lvcont_id; access_pattern = lvc.lvcont_pattern })

let get_non_trivial_subexpressions acc = let open Set.Infix in function
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

let contains_ident v =
  let rec contains_ident_rowapat = function
      CElem e -> contains_ident_rec e
    | CRange c -> contains_ident_rec c.crange_start_val || contains_ident_rec c.crange_end_val
  and contains_ident_apat = function
      VectP r -> contains_ident_rowapat r
    | MatP m -> contains_ident_rowapat m.matp_row || contains_ident_rowapat m.matp_col
  and contains_ident_rec = function
      Var _ as x -> v = x
    | FunCall f -> List.exists contains_ident_rec f.funcall_args
    | StructProj s -> contains_ident_rec s.structproj_id
    | UnaryOp unop -> contains_ident_rec unop.unaryop_expr
    | BinaryOp binop -> contains_ident_rec binop.binaryop_l_expr || contains_ident_rec binop.binaryop_r_expr
    | Access a -> contains_ident_rec a.access_container_id || contains_ident_apat a.access_pattern
    | Cast c -> contains_ident_rec c.cast_expr
    | Lit _ -> false
  in contains_ident_rec

let contains_lv lv = lv_to_expr lv |> contains_ident

 let label_function i =
  let label () =
    i := !i + 1;
    !i
  in let rec aux = function
      Block b -> Block (List.map aux b)
    | Stmt (_,s) -> (match s with
        VDecl _
      | CDecl _
      | Assign _
      | Sample _
      | FCallS _
      | Ret _ -> Stmt (label(), s)
      | Ite ite -> let l = label () in
          let if_branch = aux ite.if_branch in
          let else_branch = match ite.else_branch with
              None -> None
            | Some e -> Some (aux e) in
          Stmt (l, Ite ({ ite_condition = ite.ite_condition; if_branch; else_branch }))
      | Seq seq -> let l = label () in
          let seq_body = aux seq.seq_body in
          Stmt (l, Seq ({ seq_header = seq.seq_header; seq_body }))
      | While loop -> let l = label () in
          let loop_body = aux loop.loop_body in
          Stmt (l, While ({ loop_condition = loop.loop_condition; loop_body })))
  in aux