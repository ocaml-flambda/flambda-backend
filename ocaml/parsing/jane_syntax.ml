open Asttypes
open Parsetree
open Jane_syntax_parsing

(** We carefully regulate which bindings we import from [Language_extension]
    to ensure that we can import this file into the Jane Street internal
    repo with no changes.
*)
module Language_extension = struct
  include Language_extension_kernel

  include (
    Language_extension : Language_extension_kernel.Language_extension_for_jane_syntax)
end

(* Suppress the unused module warning so it's easy to keep around the
   shadowing even if we delete use sites of the module. *)
module _ = Language_extension

(******************************************************************************)
(** Individual language extension modules *)

(* Note [Wrapping with make_entire_jane_syntax]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The topmost node in the encoded AST must always look like e.g.
   [%jane.non_erasable.comprehensions]. (More generally,
   [%jane.ERASABILITY.FEATURE] or [@jane.ERASABILITY.FEATURE].) This allows the
   decoding machinery to know what extension is being used and what function to
   call to do the decoding. Accordingly, during encoding, after doing the hard
   work of converting the extension syntax tree into e.g. Parsetree.expression,
   we need to make a final step of wrapping the result in a [%jane.*.xyz] node.
   Ideally, this step would be done by part of our general structure, like we
   separate [of_ast] and [of_ast_internal] in the decode structure; this design
   would make it structurally impossible/hard to forget taking this final step.

   However, the final step is only one line of code (a call to
   [make_entire_jane_syntax]), but yet the name of the feature varies, as does
   the type of the payload. It would thus take several lines of code to execute
   this command otherwise, along with dozens of lines to create the structure in
   the first place. And so instead we just manually call
   [make_entire_jane_syntax] and refer to this Note as a reminder to authors of
   future syntax features to remember to do this wrapping.

   Note [Outer attributes at end]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The order of attributes matters for several reasons:
   - If the user writes attributes on a Jane Street OCaml construct, where
     should those appear with respect to the Jane Syntax attribute that
     introduces the construct?
   - Some Jane Syntax embeddings use attributes, and sometimes an AST node will
     have multiple Jane Syntax-related attributes on it. Which attribute should
     Jane Syntax interpret first?

   Both of these questions are settled by a convention where attributes
   appearing later in an attribute list are considered to be "outer" to
   attributes appearing earlier. (ppxlib adopted this convention, and thus we
   need to as well for compatibility.)

   - User-written attributes appear later in the attribute list than
     a Jane Syntax attribute that introduces a syntactic construct.
   - If multiple Jane Syntax attributes appear on an AST node, the ones
     appearing later in the attribute list should be interpreted first.
*)

module Instances = struct
  type instance =
    { head : string
    ; args : (string * instance) list
    }

  type module_expr = Imod_instance of instance

  let feature : Feature.t = Language_extension Instances
  let module_expr_of_string ~loc str = Ast_helper.Mod.ident ~loc { txt = Lident str; loc }

  let rec module_expr_of_instance ~loc { head; args } =
    let head = module_expr_of_string ~loc head in
    match args with
    | [] -> head
    | _ ->
      let args =
        List.concat_map
          (fun (param, value) ->
            let param = module_expr_of_string ~loc param in
            let value = module_expr_of_instance ~loc value in
            [ param; value ])
          args
      in
      List.fold_left (Ast_helper.Mod.apply ~loc) head args
  ;;

  let module_expr_of ~loc = function
    | Imod_instance instance ->
      Module_expr.make_entire_jane_syntax ~loc feature (fun () ->
        module_expr_of_instance ~loc instance)
  ;;

  let head_of_ident (lid : Longident.t Location.loc) =
    match lid with
    | { txt = Lident s; loc = _ } -> s
    | _ -> failwith "Malformed instance identifier"
  ;;

  let gather_args mexpr =
    let rec loop mexpr rev_acc =
      match mexpr.pmod_desc with
      | Pmod_apply (f, v) ->
        (match f.pmod_desc with
         | Pmod_apply (f, n) -> loop f ((n, v) :: rev_acc)
         | _ -> failwith "Malformed instance identifier")
      | head -> head, List.rev rev_acc
    in
    loop mexpr []
  ;;

  let string_of_module_expr mexpr =
    match mexpr.pmod_desc with
    | Pmod_ident i -> head_of_ident i
    | _ -> failwith "Malformed instance identifier"
  ;;

  let rec instance_of_module_expr mexpr =
    match gather_args mexpr with
    | Pmod_ident i, args ->
      let head = head_of_ident i in
      let args = List.map instances_of_arg_pair args in
      { head; args }
    | _ -> failwith "Malformed instance identifier"

  and instances_of_arg_pair (n, v) = string_of_module_expr n, instance_of_module_expr v

  let of_module_expr mexpr = Imod_instance (instance_of_module_expr mexpr)
end

(******************************************************************************)
(** The interface to our novel syntax, which we export *)

module type AST = sig
  type t
  type ast

  val of_ast : ast -> t option
end

module Module_expr = struct
  type t = Emod_instance of Instances.module_expr

  let of_ast_internal (feat : Feature.t) sigi =
    match feat with
    | Language_extension Instances -> Some (Emod_instance (Instances.of_module_expr sigi))
    | _ -> None
  ;;

  let of_ast = Module_expr.make_of_ast ~of_ast_internal
end
