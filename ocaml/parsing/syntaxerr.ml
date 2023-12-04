(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliary type for reporting syntax errors *)

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * string
  | Removed_string_set of Location.t

exception Error of error
exception Escape_error

let location_of_error = function
  | Unclosed(l,_,_,_)
  | Applicative_path l
  | Variable_in_scope(l,_)
  | Other l
  | Not_expecting (l, _)
  | Ill_formed_ast (l, _)
  | Invalid_package_type (l, _)
  | Expecting (l, _)
  | Removed_string_set l -> l


let ill_formed_ast loc s =
  raise (Error (Ill_formed_ast (loc, s)))

let prepare_error err =
  match err with
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      Location.errorf
        ~loc:closing_loc
        ~sub:[
          Location.msg ~loc:opening_loc
            "This '%s' might be unmatched" opening
        ]
        "Syntax error: '%s' expected" closing

  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s expected." nonterm
  | Not_expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s not expected." nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "In this scoped type, variable %a \
         is reserved for the local type %s."
        Printast.tyvar var var
  | Other loc ->
      Location.errorf ~loc "Syntax error"
  | Ill_formed_ast (loc, s) ->
      Location.errorf ~loc
        "broken invariant in parsetree: %s" s
  | Invalid_package_type (loc, s) ->
      Location.errorf ~loc "invalid package type: %s" s
  | Removed_string_set loc ->
    Location.errorf ~loc
      "Syntax error: strings are immutable, there is no assignment \
       syntax for them.\n\
       @{<hint>Hint@}: Mutable sequences of bytes are available in \
       the Bytes module.\n\
       @{<hint>Hint@}: Did you mean to use 'Bytes.set'?"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | _ -> None
    )
