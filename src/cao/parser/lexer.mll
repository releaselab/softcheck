(** Lexical analyser for CAO (.cao) *)

(* -------------------------------------------------------------------- *)
{
  open Parser
  open CaoExceptions

  exception Lexing_error of string

  (* ------------------------------------------------------------------ *)
  let keywords = [
    (* Program instructions *)
    "def"      , DEF      ;
    "typedef"  , TYPEDEF  ;
    "const"    , CONST    ;

    (* Types *)
    "void"     , VOID     ;
    "unsigned" , UNSIGNED ;
    "signed"   , SIGNED   ;
    "register" , REGISTER ;
    "int"      , INT	  ;
    "bits"     , BITS	  ;
    "bool"     , BOOL	  ;
    "vector"   , VECTOR	  ;
    "matrix"   , MATRIX	  ;
    "mod"      , MOD	  ;
    "struct"   , STRUCT	  ;
    "of"       , OF       ;

    (* Statements *)
    "return"   , RETURN   ;
    "if"       , IF	  ;
    "else"     , ELSE	  ;
    "while"    , WHILE	  ;
    "seq"      , SEQ	  ;
    "by"       , BY	  ;
    "to"       , TO	  ;

    (* Literal *)
    "true"     , TRUE     ;
    "false"    , FALSE    ;
  ]

  (* ------------------------------------------------------------------ *)
  let s_operators = [
    (* Boolean operators *)
    "&&" , AND		 ;
    "||" , OR		 ;
    "^^" , XOR		 ;

    (* Bit-wise operators *)
    "<<" , SHIFT_LEFT	 ;
    ">>" , SHIFT_RIGHT	 ;
    "<|" , ROT_LEFT	 ;
    "|>" , ROT_RIGHT 	 ;

    (* Comparison operators *)
    "==" , EQ		 ;
    ">=" , GET		 ;
    "<=" , LET		 ;
    "!=" , NOT_EQ	 ;

    (* Arithmetic operators *)
    "**" , POWER	 ;

    (* Containers operators *)
    ".." , DOUBLE_PERIOD ;

    (* Statement operators *)
    ":=" , ASSIGN        ;
  ]

  (* ------------------------------------------------------------------ *)
  let c_operators = [
    (* Boolean operators *)
    '!'	 , NOT		 ;

    (* Bit-wise operators *)
    '&'	 , BIT_AND	 ;
    '|'	 , BIT_OR	 ;
    '^'	 , BIT_XOR	 ;
    '~'	 , BIT_NOT	 ;

    (* Comparison operators *)
    '>'	 , GT		 ;
    '<'	 , LT		 ;

    (* Arithmetic operators *)
    '+'	 , PLUS		 ;
    '-'	 , MINUS	 ;
    '*'	 , TIMES	 ;
    '/'	 , DIV		 ;
    '%'	 , REMAINDER	 ;

    (* Containers operators *)
    '.'	 , PERIOD	 ;
    ','	 , COMMA	 ;
    '['	 , OSB		 ;
    ']'	 , CSB		 ;
    '@'	 , CONCAT	 ;

    (* Statement operators *)
    '$'	 , SAMPLE	 ;
    ':'	 , OF_TYPE	 ;
    '{'	 , OCB		 ;
    '}'	 , CCB		 ;
    ';'	 , SEMI_COLON	 ;

    (* Precedence operators *)
    '('	 , OB		 ;
    ')'	 , CB		 ;
  ]

  (* ------------------------------------------------------------------ *)
  let rec string_to_bits = function
    | "" -> []
    | s -> (if s.[0] = '1' then true else false) :: string_to_bits (String.sub s 1 (String.length s - 1))
  (* Converts a 01 string into a boolean list*)

  let char_to_bool = function
    | '1' -> true
    | '0' -> false
    | _ -> exit(1)
  (* Converts a char to a boolean *)
}

(* ------------------------------------------------------------------ *)
let empty = ""
let blank = [' ' '\t' '\r']
let newline = '\n'

(* ------------------------------------------------------------------ *)
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower

(* ------------------------------------------------------------------ *)
let digit = ['0'-'9']
let decimal = digit+

(* ------------------------------------------------------------------ *)
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let hexadecimal = "0x" hex_digit+

(* ------------------------------------------------------------------ *)
let bin_digit = ['0' '1']
let bin_number = "0b" bin_digit+
let sign_bin_number = "1b" bin_digit+

(* ------------------------------------------------------------------ *)
let ichar  = (letter | digit | '_' | '\'')
let identifier = letter ichar*

(* ------------------------------------------------------------------ *)
let s_op = "&&" | "||" | "^^" | "<<" | ">>" | "<|" | "|>" | "==" | ">=" | "<=" | "!=" | "**" | ".." | ":="

(* ------------------------------------------------------------------ *)
let c_op = ':' | '[' | ']' | '{' | '}' | ';' | ',' | '.' | '>' | '<' | '!' | '+' | '-' | '*' | '/' | '%' | '~' | '&' | '|' | '^' | '@' | '(' | ')' | '=' | '\\' | '$'

(* ------------------------------------------------------------------ *)
rule next_token = parse
  | newline   { Lexing.new_line lexbuf; next_token lexbuf }
  | blank+  { next_token lexbuf }

  | identifier as id { try List.assoc id keywords with Not_found -> IDENTIFIER id }
  | decimal as i { INT_LIT (Num.num_of_string i) }
  | hexadecimal as hn { INT_LIT (Num.num_of_string hn) }
  | bin_number as bn { BITS_LIT (string_to_bits (String.sub bn 2 (String.length bn - 2))) }
  | sign_bin_number as sbn { SIGN_BITS_LIT (char_to_bool sbn.[0], string_to_bits (String.sub sbn 2 (String.length sbn - 2))) }

  | s_op as op { try List.assoc op s_operators with Not_found -> lex_error lexbuf ("illegal use of " ^ op) }
  | c_op as op { try List.assoc op c_operators with Not_found -> lex_error lexbuf ("illegal use of " ^ (String.make 1 op)) }

  | "/*" { comment lexbuf; next_token lexbuf }

  | eof     { EOF }

and comment = parse
  | "*/"        { () }
  | "/*"        { comment lexbuf; comment lexbuf }
  | newline     { Lexing.new_line lexbuf; comment lexbuf }
  | eof         { unterminated_comment () }
  | _           { comment lexbuf }
