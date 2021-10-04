
open Lambda
open Typedtree
open Debuginfo.Scoped_location

val transl_arr_comprehension:
  transl_exp:(scopes:scopes -> expression -> lambda)
  -> loc:scoped_location -> scopes:scopes
  -> array_kind:array_kind
  -> expression -> comprehension list
  -> lambda

val transl_list_comprehension: 
  transl_exp:(scopes:scopes -> expression -> lambda)
  -> loc:scoped_location -> scopes:scopes 
  -> expression -> comprehension list 
  -> lambda