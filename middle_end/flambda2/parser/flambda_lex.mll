
{
open Flambda_parser

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
  | No_such_primitive of string
;;

let pp_error ppf = function
  | Illegal_character c -> Format.fprintf ppf "Illegal character %c" c
  | Invalid_literal s -> Format.fprintf ppf "Invalid literal %s" s
  | No_such_primitive s -> Format.fprintf ppf "No such primitive %%%s" s

exception Error of error * location;;

let current_location lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let error ~lexbuf e = raise (Error (e, current_location lexbuf))

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable [
    "always", KWD_ALWAYS;
    "and", KWD_AND;
    "andwhere", KWD_ANDWHERE;
    "any", KWD_ANY;
    "apply", KWD_APPLY;
    "array", KWD_ARRAY;
    "asr", KWD_ASR;
    "available", KWD_AVAILABLE;
    "boxed", KWD_BOXED;
    "bswap", KWD_BSWAP;
    "ccall", KWD_CCALL;
    "closure", KWD_CLOSURE;
    "code", KWD_CODE;
    "cont", KWD_CONT;
    "default", KWD_DEFAULT;
    "define_root_symbol", KWD_DEFINE_ROOT_SYMBOL;
    "deleted", KWD_DELETED;
    "depth", KWD_DEPTH;
    "direct", KWD_DIRECT;
    "do_not_inline", KWD_DO_NOT_INLINE;
    "done", KWD_DONE;
    "end", KWD_END;
    "error", KWD_ERROR;
    "exn", KWD_EXN;
    "float", KWD_FLOAT;
    "halt_and_catch_fire", KWD_HCF;
    "heap_or_local", KWD_HEAP_OR_LOCAL;
    "hint", KWD_HINT;
    "id", KWD_ID;
    "imm", KWD_IMM;
    "immutable_unique", KWD_IMMUTABLE_UNIQUE;
    "in", KWD_IN;
    "inf", KWD_INF;
    "inline", KWD_INLINE;
    "inlined", KWD_INLINED;
    "inlining_state", KWD_INLINING_STATE;
    "int32", KWD_INT32;
    "int64", KWD_INT64;
    "invalid", KWD_INVALID;
    "land", KWD_LAND;
    "let", KWD_LET;
    "local", KWD_LOCAL;
    "loopify", KWD_LOOPIFY;
    "lor", KWD_LOR;
    "lsl", KWD_LSL;
    "lsr", KWD_LSR;
    "mutable", KWD_MUTABLE;
    "nativeint", KWD_NATIVEINT;
    "never", KWD_NEVER;
    "newer_version_of", KWD_NEWER_VERSION_OF;
    "noalloc", KWD_NOALLOC;
    "notrace", KWD_NOTRACE;
    "of", KWD_OF;
    "pop", KWD_POP;
    "push", KWD_PUSH;
    "rec", KWD_REC;
    "rec_info", KWD_REC_INFO;
    "region", KWD_REGION;
    "regular", KWD_REGULAR;
    "reraise", KWD_RERAISE;
    "set_of_closures", KWD_SET_OF_CLOSURES;
    "size", KWD_SIZE;
    "succ", KWD_SUCC;
    "switch", KWD_SWITCH;
    "tag", KWD_TAG;
    "tagged", KWD_TAGGED;
    "tailrec", KWD_TAILREC;
    "toplevel", KWD_TOPLEVEL;
    "tupled", KWD_TUPLED;
    "unit", KWD_UNIT;
    "unreachable", KWD_UNREACHABLE;
    "unroll", KWD_UNROLL;
    "unsigned", KWD_UNSIGNED;
    "val", KWD_VAL;
    "where", KWD_WHERE;
    "with", KWD_WITH;

    (* Constructors for static constants *)
    "Block", STATIC_CONST_BLOCK;
    "Float_array", STATIC_CONST_FLOAT_ARRAY;
    "Float_block", STATIC_CONST_FLOAT_BLOCK;
    "Empty_array", STATIC_CONST_EMPTY_ARRAY;
]

let ident_or_keyword str =
  try Hashtbl.find keyword_table str
  with Not_found -> IDENT str

let is_keyword str =
  Hashtbl.mem keyword_table str

let prim_table =
  create_hashtable [
    "array_length", PRIM_ARRAY_LENGTH;
    "array_load", PRIM_ARRAY_LOAD;
    "array_set", PRIM_ARRAY_SET;
    "begin_region", PRIM_BEGIN_REGION;
    "begin_try_region", PRIM_BEGIN_TRY_REGION;
    "bigstring_load", PRIM_BIGSTRING_LOAD;
    "bigstring_set", PRIM_BIGSTRING_SET;
    "Block", PRIM_BLOCK;
    "block_load", PRIM_BLOCK_LOAD;
    "block_set", PRIM_BLOCK_SET;
    "not", PRIM_BOOLEAN_NOT;
    "Box_float", PRIM_BOX_FLOAT;
    "Box_int32", PRIM_BOX_INT32;
    "Box_int64", PRIM_BOX_INT64;
    "Box_nativeint", PRIM_BOX_NATIVEINT;
    "bytes_length", PRIM_BYTES_LENGTH;
    "bytes_load", PRIM_BYTES_LOAD;
    "bytes_set", PRIM_BYTES_SET;
    "end_region", PRIM_END_REGION;
    "end_try_region", PRIM_END_TRY_REGION;
    "get_tag", PRIM_GET_TAG;
    "int_arith", PRIM_INT_ARITH;
    "int_comp", PRIM_INT_COMP;
    "int_shift", PRIM_INT_SHIFT;
    "is_flat_float_array", PRIM_IS_FLAT_FLOAT_ARRAY;
    "is_int", PRIM_IS_INT;
    "num_conv", PRIM_NUM_CONV;
    "Opaque", PRIM_OPAQUE;
    "phys_eq", PRIM_PHYS_EQ;
    "phys_ne", PRIM_PHYS_NE;
    "project_value_slot", PRIM_PROJECT_VALUE_SLOT;
    "project_function_slot", PRIM_PROJECT_FUNCTION_SLOT;
    "string_length", PRIM_STRING_LENGTH;
    "string_load", PRIM_STRING_LOAD;
    "Tag_imm", PRIM_TAG_IMM;
    "unbox_float", PRIM_UNBOX_FLOAT;
    "unbox_int32", PRIM_UNBOX_INT32;
    "unbox_int64", PRIM_UNBOX_INT64;
    "unbox_nativeint", PRIM_UNBOX_NATIVEINT;
    "unbox_vec128", PRIM_UNBOX_VEC128;
    "untag_imm", PRIM_UNTAG_IMM;
]

let prim ~lexbuf str =
  try Hashtbl.find prim_table str
  with Not_found -> error ~lexbuf (No_such_primitive str)

let unquote_ident str =
  match str with
  | "" -> ""
  | _ ->
    begin
      match String.get str 0 with
      | '`' -> String.sub str 1 (String.length str - 2)
      | _ -> str
    end

let symbol cunit_ident cunit_linkage_name ident =
  let cunit =
    Option.map (fun cunit_ident ->
      { Fexpr.ident = unquote_ident cunit_ident;
        linkage_name = Option.map unquote_ident cunit_linkage_name }
    ) cunit_ident
  in
  SYMBOL (cunit, unquote_ident ident)

}

let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let quoted_ident = '`' [^ '`' '\n']* '`'
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let sign = ['-']
let int_literal =
  sign? (decimal_literal | hex_literal | oct_literal | bin_literal)
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let int_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | "\n"
      { Lexing.new_line lexbuf; token lexbuf }
  | blank +
      { token lexbuf }
  | "(*"
      { comment 1 lexbuf;
        token lexbuf }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "."
      { DOT }
  | ";"
      { SEMICOLON }
  | "="
      { EQUAL }
  | "_"
      { BLANK }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACK }
  | "]"
      { RBRACK }
  | "[|"
      { LBRACKPIPE }
  | "|]"
      { RBRACKPIPE }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { STAR }
  | "/"  { SLASH }
  | "%"  { PERCENT }
  | "<"  { LESS }
  | ">"  { GREATER }
  | "<=" { LESSEQUAL }
  | ">=" { GREATEREQUAL }
  | "<>" { NOTEQUAL }
  | "?"  { QMARK }
  | "+." { PLUSDOT }
  | "-." { MINUSDOT }
  | "*." { STARDOT }
  | "/." { SLASHDOT }
  | "=." { EQUALDOT }
  | "<>." { NOTEQUALDOT }
  | "<." { LESSDOT }
  | "<=." { LESSEQUALDOT }
  | "?." { QMARKDOT }
  | "<-" { LESSMINUS }
  | "->" { MINUSGREATER }
  | "@" { AT }
  | "|"  { PIPE }
  | "~"  { TILDE }
  | "~-"  { TILDEMINUS }
  | "&"  { AMP }
  | "^"  { CARET }
  | "===>" { BIGARROW }
  | identstart identchar* as ident
         { ident_or_keyword ident }
  | quoted_ident as ident
         { IDENT (unquote_ident ident) }
  | '$'
    (((identchar* | quoted_ident) as cunit_ident)
     ('/' ((identchar* | quoted_ident) as cunit_linkage_name))?
     '.')?
    ((identchar* | quoted_ident) as ident)
         { symbol cunit_ident cunit_linkage_name ident }
  | '%' (identchar+ as p)
         { prim ~lexbuf p }
  | (int_literal as lit) (int_modifier as modif)?
         { INT (lit, modif) }
  | float_literal | hex_float_literal as lit
         { FLOAT (lit |> Float.of_string) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as lit
         { error ~lexbuf (Invalid_literal lit) }
  | '"' (([^ '"'] | '\\' '"')* as s) '"'
         (* CR-someday lmaurer: Escape sequences, multiline strings *)
         { STRING (Scanf.unescaped s) }
  | eof  { EOF }
  | _ as ch
         { error ~lexbuf (Illegal_character ch) }

and comment n = parse
  | "\n"
         { Lexing.new_line lexbuf; comment n lexbuf }
  | "*)"
         { if n = 1 then ()
           else comment (n-1) lexbuf }
  | "(*"
         { comment (n+1) lexbuf }
  | _
         { comment n lexbuf }
