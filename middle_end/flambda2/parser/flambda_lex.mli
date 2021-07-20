val token : Lexing.lexbuf -> Flambda_parser.token

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
  | No_such_primitive of string

exception Error of error * location

val pp_error : Format.formatter -> error -> unit

val is_keyword : string -> bool
