
open Lambda
open Typedtree
open Debuginfo.Scoped_location

val transl_arr_comprehension: expression -> comprehension list
    -> array_kind:array_kind -> scopes:scopes -> loc:scoped_location
    -> transl_exp:(scopes:scopes -> expression -> lambda) -> lambda

val transl_list_comprehension: expression -> comprehension list
    -> scopes:scopes -> loc:scoped_location
    -> transl_exp:(scopes:scopes -> expression -> lambda) -> lambda