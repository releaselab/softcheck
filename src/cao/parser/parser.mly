/* Syntactic analyser for the CAO language */

/* ------------------------------------------------------------------ */
%{
  open Ast
  open Num
  open CaoExceptions

  let bool_to_sign = function
      | true -> S
      | false -> U

  let tsyn = ref []

  let l = ref 0

  let label_function = label_function l
%}

/* ------------------------------------------------------------------ */
%token EOF
%token DEF TYPEDEF CONST OF_TYPE OF ASSIGN RETURN OSB CSB OCB CCB SEMI_COLON COMMA DOUBLE_PERIOD PERIOD
%token VOID UNSIGNED SIGNED REGISTER INT BITS BOOL VECTOR MATRIX MOD STRUCT
%token IF ELSE WHILE SEQ TO BY
%token EQ AND OR GET LET GT LT NOT NOT_EQ XOR
%token PLUS MINUS TIMES DIV POWER REMAINDER
%token BIT_NOT BIT_AND BIT_OR BIT_XOR SHIFT_LEFT SHIFT_RIGHT ROT_LEFT ROT_RIGHT CONCAT SAMPLE
%token OB CB

/* ------------------------------------------------------------------ */
%token TRUE FALSE
%token <Num.num> INT_LIT
%token <bool list> BITS_LIT
%token <bool * bool list> SIGN_BITS_LIT
%token <string> IDENTIFIER

/* ------------------------------------------------------------------ */
%left OR
%left XOR
%left AND
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQ NOT_EQ
%left LT LET GT GET
%left SHIFT_LEFT SHIFT_RIGHT ROT_LEFT ROT_RIGHT
%left PLUS MINUS
%left TIMES DIV REMAINDER CONCAT
%left POWER
%right CAST
%right NOT BIT_NOT UNARY_MINUS
%left PERIOD OSB

/* ------------------------------------------------------------------ */
/* Grammar entrance */
%start program

/* ------------------------------------------------------------------ */
%type <Ast.program> program

%%

/* ------------------------------------------------------------------ */
program:
| def* EOF { { defs = $1 ; main = None } }
;

/* ------------------------------------------------------------------ */
def:
| dv SEMI_COLON  { VarDef $1 }
| dc SEMI_COLON { ConstDef $1 }
| dt SEMI_COLON  { TyDef $1 }
| dfp { FunDef $1 }
;

identifier:
| IDENTIFIER { $1 }
;

dv:
| DEF separated_nonempty_list(COMMA, identifier) OF_TYPE typeDecl { VarD { var_d_ids = $2 ; var_d_type = $4 ; var_d_init = None } }
| DEF separated_nonempty_list(COMMA, identifier) OF_TYPE typeDecl ASSIGN expr { VarD { var_d_ids = $2 ; var_d_type = $4 ; var_d_init = Some $6 } }
| DEF separated_nonempty_list(COMMA, identifier) OF_TYPE typeDecl ASSIGN OCB separated_nonempty_list(COMMA, expr) CCB { ContD { cont_d_ids = $2 ; cont_d_type = $4 ; cont_d_init = $7 } }
;

dc:
| DEF CONST separated_nonempty_list(COMMA, identifier) OF_TYPE typeDecl { { const_d_ids = $3 ; const_d_type = $5 ; const_d_ann = NoConst } }
| DEF CONST separated_nonempty_list(COMMA, identifier) OF_TYPE typeDecl ASSIGN expr { { const_d_ids = $3 ; const_d_type = $5 ; const_d_ann = ConstInit $7 } }
| DEF CONST separated_nonempty_list(COMMA, identifier) OF_TYPE typeDecl OCB expr CCB { { const_d_ids = $3 ; const_d_type = $5 ; const_d_ann = ConstCond $7 } }
;

dt:
| TYPEDEF identifier ASSIGN typeDecl { tsyn := ($2,$4):: !tsyn ; TySynDef { tysyn_d_id = $2 ; tysyn_d_type = $4 } }
| TYPEDEF identifier ASSIGN STRUCT OSB separated_nonempty_list(SEMI_COLON, struct_field) CSB { StructDecl { struct_d_id = $2 ; struct_d_fields = $6 } }
;

struct_field:
| DEF identifier OF_TYPE typeDecl { ($2,$4)  }
;

dfp:
| DEF identifier OB separated_list(COMMA, arg) CB OF_TYPE return_type OCB lstmt+ CCB { { func_id = $2 ; func_args = $4 ; func_ret_type = $7 ; func_body = label_function (Block $9) } }
;

/* ------------------------------------------------------------------ */
arg:
| identifier OF_TYPE typeDecl { Arg { narg_id = $1 ; narg_type = $3 } }
| CONST identifier OF_TYPE typeDecl { ArgConst { argconst_id = $2 ; argconst_type = $4 ; argconst_condition = NoConst } }
| CONST identifier OF_TYPE typeDecl OCB expr CCB { ArgConst { argconst_id = $2 ; argconst_type = $4 ; argconst_condition = ConstCond $6 } }
;

/* ------------------------------------------------------------------ */
return_type:
| VOID { VoidD }
| typeDecl { $1 }
;

typeDecl:
| INT { IntD }
| REGISTER INT { RIntD }
| BOOL { BoolD }
| UNSIGNED BITS OSB expr CSB { BitsD { bits_d_signal = U ; bits_d_size = $4 } }
| SIGNED BITS OSB expr CSB { BitsD { bits_d_signal = S ; bits_d_size = $4 } }
| MOD OSB arith_expr CSB { ModD(ModNum $3) }
| MOD OSB typeDecl LT IDENTIFIER GT DIV polynomial CSB { ModD(ModPol { modpol_field_t = $3 ; modpol_var = $5 ; modpol_poly = $8 } ) }
| VECTOR OSB expr CSB OF typeDecl { VectorD { vector_d_size = $3 ; vector_d_type = $6 } }
| MATRIX OSB expr COMMA expr CSB OF typeDecl { MatrixD { matrix_d_rows = $3 ; matrix_d_cols = $5 ; matrix_d_type = $8 } }
| identifier { TySynD { tysyn_d_id = $1 ; tysyn_d_type = try List.assoc $1 !tsyn with Not_found -> unbound_val_error ("Unknown type "^$1) } }
;

/* ------------------------------------------------------------------ */
lstmt:
| stmt { Stmt (0, $1) }
;

stmt:
| dv SEMI_COLON { VDecl $1 }
| dc SEMI_COLON { CDecl $1 }
| separated_nonempty_list(COMMA, lValue) ASSIGN separated_nonempty_list(COMMA, expr) SEMI_COLON { Assign { assign_ids = $1 ; assign_values = $3 } }
| separated_nonempty_list(COMMA, lValue) ASSIGN SAMPLE SEMI_COLON { Sample $1 }
| IDENTIFIER OB separated_list(COMMA,expr) CB SEMI_COLON { FCallS { fcalls_id = $1 ; fcalls_args = $3 } }
| RETURN expr SEMI_COLON { Ret $2 }
| ifThenElseStatement { $1 }
| whileStatement { $1 }
| seqStatement { $1 }
;

ifThenElseStatement:
| IF OB expr CB OCB lstmt+ CCB { Ite { ite_condition = $3 ; if_branch = Block $6 ; else_branch = None } }
| IF OB expr CB OCB lstmt+ CCB ELSE OCB lstmt+ CCB { Ite { ite_condition = $3 ; if_branch = Block $6 ; else_branch = Some (Block $10) } }
;

whileStatement:
| WHILE OB expr CB OCB lstmt+ CCB { While { loop_condition = $3 ; loop_body = Block $6 }  }
;

seqStatement:
| SEQ identifier ASSIGN expr TO expr OCB lstmt+ CCB { Seq { seq_header = { seqheader_var = $2 ; seqheader_start_val = $4 ; seqheader_end_val = $6 ; seqheader_increase = None } ; seq_body = Block $8 } }
| SEQ identifier ASSIGN expr TO expr BY expr OCB lstmt+ CCB { Seq { seq_header = { seqheader_var = $2 ; seqheader_start_val = $4 ; seqheader_end_val = $6 ; seqheader_increase = Some $8 } ; seq_body = Block $10 } }
;

lValue:
| identifier { LVVar $1 }
| lValue PERIOD IDENTIFIER { LVStruct { lvstruct_id = $1 ; lvstruct_field = $3 } }
| lValue aPat { LVCont { lvcont_id = $1 ; lvcont_pattern = $2 } }
;

aPat:
| OSB expr CSB { VectP (CElem $2) }
| OSB expr DOUBLE_PERIOD expr CSB { VectP ( CRange { crange_start_val = $2 ; crange_end_val = $4 } ) }
| OSB expr COMMA expr CSB { MatP { matp_row = CElem $2 ;  matp_col = CElem $4 } }
| OSB expr DOUBLE_PERIOD expr COMMA expr DOUBLE_PERIOD expr CSB { MatP { matp_row = CRange { crange_start_val = $2 ; crange_end_val = $4 } ; matp_col = CRange { crange_start_val = $6 ; crange_end_val = $8 } } }
| OSB expr COMMA expr DOUBLE_PERIOD expr CSB { MatP { matp_row = CElem $2 ; matp_col = CRange { crange_start_val = $4 ; crange_end_val = $6 } } }
| OSB expr DOUBLE_PERIOD expr COMMA expr CSB { MatP { matp_row = CRange { crange_start_val = $2 ; crange_end_val = $4 } ; matp_col = CElem $6 } }
;

/* ------------------------------------------------------------------ */

expr:
| literal { Lit $1 }
| IDENTIFIER { Var $1 }
| OB expr CB { $2 }
| expr PERIOD IDENTIFIER { StructProj { structproj_id = $1 ; structproj_field = $3 } }
| identifier OB separated_list(COMMA, expr) CB { FunCall { funcall_id = $1 ; funcall_args = $3 } }
| typeDecl OF_TYPE expr %prec CAST { Cast { cast_type = $1 ; cast_expr = $3 } }
| expr aPat { Access { access_container_id = $1 ; access_pattern = $2 } }
| MINUS expr %prec UNARY_MINUS { UnaryOp { unaryop_op = Sym ; unaryop_expr = $2 } }
| uop expr { UnaryOp { unaryop_op = $1 ; unaryop_expr = $2 } }
| expr arithop expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| expr cop expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| expr bop expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| expr bitsop expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| expr srop expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
;


arith_expr:
| literal { Lit $1 }
| IDENTIFIER { Var $1 }
| OB expr CB { $2 }
| arith_expr PERIOD IDENTIFIER { StructProj { structproj_id = $1 ; structproj_field = $3 } }
| identifier OB separated_list(COMMA, arith_expr) CB { FunCall { funcall_id = $1 ; funcall_args = $3 } }
| typeDecl OF_TYPE arith_expr %prec CAST { Cast { cast_type = $1 ; cast_expr = $3 } }
| arith_expr aPat { Access { access_container_id = $1 ; access_pattern = $2 } }
| MINUS arith_expr %prec UNARY_MINUS { UnaryOp { unaryop_op = Sym ; unaryop_expr = $2 } }
| uop arith_expr { UnaryOp { unaryop_op = $1 ; unaryop_expr = $2 } }
| arith_expr arithop arith_expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| arith_expr bop arith_expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| arith_expr bitsop arith_expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
| arith_expr srop arith_expr { BinaryOp { binaryop_op = $2 ; binaryop_l_expr = $1 ; binaryop_r_expr = $3 } }
;

%inline uop:
| NOT { Not }
| BIT_NOT { BNot }
;

%inline arithop:
| PLUS { ArithOp Plus }
| MINUS { ArithOp Minus }
| TIMES { ArithOp Times }
| POWER { ArithOp Power }
| DIV { ArithOp Div }
| REMAINDER { ArithOp ModOp }
;

%inline cop:
| EQ { CmpOp Eq }
| NOT_EQ { CmpOp Neq }
| LT { CmpOp Lt }
| LET { CmpOp Let }
| GT { CmpOp Gt }
| GET { CmpOp Get }
;

%inline bop:
| OR { BoolOp Or }
| AND { BoolOp And }
| XOR { BoolOp Xor }
;

%inline bitsop:
| BIT_OR { BitOp BWOr }
| BIT_AND { BitOp BWAnd }
| BIT_XOR { BitOp BWXor }
;

%inline srop:
| SHIFT_RIGHT { BitsSROp SUp }
| SHIFT_LEFT { BitsSROp SDown }
| ROT_RIGHT { BitsSROp SRUp }
| ROT_LEFT { BitsSROp SRDown }
| CONCAT { Concat }
;

/* ------------------------------------------------------------------ */
literal:
| INT_LIT { ILit $1 }
| BITS_LIT { BSLit { signal = U ; bitstring = $1 } }
| SIGN_BITS_LIT { BSLit { signal = bool_to_sign (fst $1) ; bitstring = snd $1 } }
| OSB polynomial CSB { PLit $2 }
| TRUE { BLit true }
| FALSE { BLit false }
;

/* ------------------------------------------------------------------ */
polynomial:
| top_monomial+ { { monomials = $1 } }
;

top_monomial:
| monomial { $1 }
| PLUS monomial { $2 }
| MINUS monomial { neg $2 }
;

monomial:
| IDENTIFIER { { coef = CoefI(IInt(num_of_int 1)) ; base = MExpI { var = $1 ; exp = num_of_int 1 } } }
| IDENTIFIER POWER INT_LIT { { coef = CoefI(IInt(num_of_int 1)) ; base = MExpI { var = $1 ; exp = $3 } } }
| INT_LIT { { coef = CoefI(IInt $1) ; base = EZero } }
| INT_LIT TIMES IDENTIFIER { { coef = CoefI(IInt $1) ; base = MExpI { var = $3 ; exp = num_of_int 1 } } }
| INT_LIT TIMES IDENTIFIER POWER INT_LIT { { coef = CoefI(IInt $1) ; base = MExpI { var = $3 ; exp = $5 } } }
| OB polynomial CB { { coef = CoefP $2 ; base = EZero } }
| OB polynomial CB TIMES IDENTIFIER { { coef = CoefP $2 ; base = MExpI { var = $5 ; exp = num_of_int 1 } } }
| OB polynomial CB TIMES IDENTIFIER POWER INT_LIT { { coef = CoefP $2 ; base = MExpI { var = $5 ; exp = $7 } } }
;
