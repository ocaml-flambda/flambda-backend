
(* The type of tokens. *)

type token = 
  | TILDE
  | SYMBOL of (Fexpr.compilation_unit option * string)
  | STRING of (string)
  | STARDOT
  | STAR
  | SLASHDOT
  | SLASH
  | SEMICOLON
  | RPAREN
  | RBRACKPIPE
  | RBRACE
  | QMARKDOT
  | QMARK
  | PRIM_UNTAG_IMM
  | PRIM_UNBOX_NATIVEINT
  | PRIM_UNBOX_INT64
  | PRIM_UNBOX_INT32
  | PRIM_UNBOX_FLOAT
  | PRIM_TAG_IMM
  | PRIM_STRING_LENGTH
  | PRIM_SELECT_CLOSURE
  | PRIM_PROJECT_VAR
  | PRIM_PHYS_NE
  | PRIM_PHYS_EQ
  | PRIM_OPAQUE
  | PRIM_NUM_CONV
  | PRIM_IS_INT
  | PRIM_INT_SHIFT
  | PRIM_INT_COMP
  | PRIM_INT_ARITH
  | PRIM_GET_TAG
  | PRIM_BYTES_LENGTH
  | PRIM_BOX_NATIVEINT
  | PRIM_BOX_INT64
  | PRIM_BOX_INT32
  | PRIM_BOX_FLOAT
  | PRIM_BLOCK_LOAD
  | PRIM_BLOCK
  | PRIM_ARRAY_SET
  | PRIM_ARRAY_LOAD
  | PRIM_ARRAY_LENGTH
  | PLUSDOT
  | PLUS
  | PIPE
  | PERCENT
  | NOTEQUALDOT
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | LPAREN
  | LESSMINUS
  | LESSEQUALDOT
  | LESSEQUAL
  | LESSDOT
  | LESS
  | LBRACKPIPE
  | LBRACE
  | KWD_WITH
  | KWD_WHERE
  | KWD_VAL
  | KWD_UNSIGNED
  | KWD_UNROLL
  | KWD_UNREACHABLE
  | KWD_UNIT
  | KWD_TUPLED
  | KWD_TAGGED
  | KWD_TAG
  | KWD_SWITCH
  | KWD_SUCC
  | KWD_SIZE
  | KWD_SET_OF_CLOSURES
  | KWD_RERAISE
  | KWD_REGULAR
  | KWD_REC_INFO
  | KWD_REC
  | KWD_PUSH
  | KWD_POP
  | KWD_NOTRACE
  | KWD_NOALLOC
  | KWD_NEWER_VERSION_OF
  | KWD_NEVER
  | KWD_NATIVEINT
  | KWD_MUTABLE
  | KWD_LXOR
  | KWD_LSR
  | KWD_LSL
  | KWD_LOR
  | KWD_LET
  | KWD_LAND
  | KWD_INT64
  | KWD_INT32
  | KWD_INLINING_STATE
  | KWD_INLINED
  | KWD_INLINE
  | KWD_INF
  | KWD_IN
  | KWD_IMMUTABLE_UNIQUE
  | KWD_IMM
  | KWD_ID
  | KWD_HINT
  | KWD_HCF
  | KWD_FLOAT_BLOCK
  | KWD_FLOAT_ARRAY
  | KWD_FLOAT
  | KWD_FABRICATED
  | KWD_EXN
  | KWD_ERROR
  | KWD_END
  | KWD_DO_NOT_INLINE
  | KWD_DONE
  | KWD_DIRECT
  | KWD_DEPTH
  | KWD_DELETED
  | KWD_DEFINE_ROOT_SYMBOL
  | KWD_DEFAULT
  | KWD_CONT
  | KWD_CODE
  | KWD_CLOSURE
  | KWD_CCALL
  | KWD_BOXED
  | KWD_BLOCK
  | KWD_AVAILABLE
  | KWD_ASR
  | KWD_APPLY
  | KWD_ANDWHERE
  | KWD_AND
  | KWD_ALWAYS
  | INT of (string * char option)
  | IDENT of (string)
  | GREATEREQUALDOT
  | GREATEREQUAL
  | GREATERDOT
  | GREATER
  | FLOAT of (float)
  | EQUALDOT
  | EQUAL
  | EOF
  | DOT
  | COMMA
  | COLON
  | BLANK
  | BIGARROW
  | AT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val flambda_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.flambda_unit)

val expect_test_spec: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.expect_test_spec)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include CamlinternalMenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val flambda_unit: Lexing.position -> (Fexpr.flambda_unit) MenhirInterpreter.checkpoint
  
  val expect_test_spec: Lexing.position -> (Fexpr.expect_test_spec) MenhirInterpreter.checkpoint
  
end
