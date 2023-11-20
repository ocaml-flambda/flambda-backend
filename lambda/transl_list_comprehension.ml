open Lambda
open Typedtree
open Asttypes
open Transl_comprehension_utils
open Lambda_utils.Constants

(** List comprehensions are compiled in terms of "reversed difference lists".  A
    difference list in general is a function from lists to lists; by "reversed",
    we mean that these lists are stored backwards, and need to be reversed at
    the end.  We make both these choices for the usual efficiency reasons;
    difference lists allow for efficient concatenation; they can also be viewed
    as based on passing around accumulators, which allows us to make our
    functions tail-recursive, at the cost of building our lists up backwards.
    An additional choice we make is to build all these intermediate data
    structures on the stack (i.e., make them [local_]); again, this is for
    efficiency, as it means we don't need to get the structure of these
    difference lists involved with the garbage collector.  Since we can
    currently only generate global lists with list comprehensions (see the
    comment "What modes should comprehensions use?" in [typecore.ml]), we need a
    type that is spine-local but element-global; we thus define a custom type of
    such snoc lists, and define our difference lists in terms of that, in the
    internal module [CamlinternalComprehension]:
    {[
      type 'a rev_list =
        | Nil
        | Snoc of { init : 'a rev_list; global_ last : 'a }

      type 'a rev_dlist = local_ 'a rev_list -> local_ 'a rev_list
    ]}
    We then work exclusively in terms of [local_ 'a rev_dlist] values, reversing
    them into a global [list] only at the very end.

    We desugar each iterator of a list comprehension into the application of a
    tail-recursive higher-order function analogous to `concat_map`, whose type
    is of the following form:
    {[
      ...iterator arguments... ->
      local_ ('elt -> local_ 'res rev_dlist) ->
      local_ 'res rev_dlist
    ]}
    Here, the [...iterator arguments...] define the sequence of values to be
    iterated over (the [seq] of a [for pat in seq] iterator, or the [start] and
    [end] of a [for x = start to/downto end] iterator); the function argument is
    then to be called once for each item.  What goes in the function?  It will
    be the next iterator, desugared in the same way.  At any time, a [when]
    clause might intervene, which is simply desugared into a conditional that
    gates entering the next phase of the translation.

    Eventually, we reach the body, which is placed into the body of the
    innermost translated function; it produces the single-item reversed
    difference list (alternatively, snocs its generated value onto the
    accumulator).  Because each function is analogous to `concat_map`, this
    builds up the correct list in the end.  The whole thing is then passed into
    a reversal function, building the final list.

    For example, consider the following list comprehension:
    {[
      [x+y for x = 1 to 3 when x <> 2 for y in [10*x; 100*x]]
      (* = [11; 101; 33; 303] *)
    ]}
    This translates to the (Lambda equivalent of) the following:
    {[
      (* Convert the result to a normal list *)
      CamlinternalComprehension.rev_list_to_list (
        (* for x = 1 to 3 *)
        let start = 1 in
        let stop  = 3 in
        CamlinternalComprehension.rev_dlist_concat_iterate_up
          start stop
          (fun x acc_x -> local_
            (* when x <> 2 *)
            if x <> 2
            then
              (* for y in [10*x; 100*x] *)
              let iter_list = [10*x; 100*x] in
              CamlinternalComprehension.rev_dlist_concat_map
                iter_list
                (fun y acc_y -> local_
                  (* The body: x+y *)
                  Snoc { init = acc_y; last = x*y })
                acc_x
            else
              acc_x)
          Nil)
         ]}

    See [CamlinternalComprehension] for the types and functions we desugar to,
    along with some more documentation. *)

(** An implementation note: Many of the functions in this file need to translate
    expressions from Typedtree to Lambda; to avoid strange dependency ordering,
    we parameterize those functions by [Translcore.transl_exp], and pass it in
    as a labeled argument, along with the necessary [scopes] labeled argument
    that it requires. *)

(** The functions that are required to build the results of list comprehensions;
    see the documentation for [CamlinternalComprehension] for more details.
    Because these are being looked up in the environment, we need to wait to
    create them until that exists, hence [lazy]. *)
let ( rev_list_to_list
    , rev_dlist_concat_map
    , rev_dlist_concat_iterate_up
    , rev_dlist_concat_iterate_down )
  =
  let transl name =
    lazy (Lambda.transl_prim "CamlinternalComprehension" name)
  in
  ( transl "rev_list_to_list"
  , transl "rev_dlist_concat_map"
  , transl "rev_dlist_concat_iterate_up"
  , transl "rev_dlist_concat_iterate_down" )
;;

(** The [local_] form of the [CamlinternalComprehension.Snoc] constructor, for
    building the intermediate restults of list comprehensions; see the
    documentation for [CamlinternalComprehension.rev_list] for more details. *)
let rev_list_snoc_local ~loc ~init ~last =
  Lprim(Pmakeblock(0, Immutable, None, alloc_local), [init; last], loc)

(** The [CamlinternalComprehension.Nil] constructor, for building the
    intermediate restults of list comprehensions; see the documentation for
    [CamlinternalComprehension.rev_list] for more details. *)
let rev_list_nil = int 0

(** The information needed to translate a single iterator from a
    [for ... and ...] clause (i.e., [x = e1 (down)to e2] or [for pat in xs]). *)
type translated_iterator =
  { builder : lambda Lazy.t
  (** The function that does the appropriate iteration (counting up, counting
      down, or iterating over a list).  As discussed at the start of this file,
      this function is expected to have a type of the following form:
      {[
        ...iterator arguments... ->
        local_ ('elt -> local_ 'res rev_dlist) ->
        local_ 'res rev_dlist
      ]}
      Once the "iterator arguments", which vary depending on the iterator, are
      applied to this function (see [arg_lets]), then it is simply waiting for
      the body of the iterator (the final function argument).  Lazy because it
      holds a reference to a primitive, which has to be constructed lazily (see
      above). *)
  ; arg_lets : Let_binding.t list
  (** The first-class let bindings that bind the arguments to the [builder]
      function that actually does the iteration.  These let bindings need to be
      collected separately so that they can all be bound at once before the
      whole [for ... and ...] clause, so that iterators in such a clause don't
      have their side effects performed multiple times in relation to each
      other.  Every variable bound by one of these let bindings will be passed
      to [builder], filling in the [...iterator arguments...] in its type. *)
  ; element : Ident.t
  (** The name given to the values we're iterating over; needs to be a fresh
      name for [for]-[in] iterators in case the user specifies a complex
      pattern. *)
  ; element_kind : layout
  (** The [layout] of the values we're iterating over. *)
  ; add_bindings : lambda -> lambda
  (** Any extra bindings that should be present in the body of this iterator,
      for use by nested pieces of the translation; used if the user specifies a
      complex pattern in a [for]-[in] iterator. *)
  }

(** Translates an iterator ([Typedtree.comprehension_iterator]), one piece of a
    [for ... and ... and ...] expression, into Lambda.  This translation is into
    a [translated_iterator], not just a Lambda term, because the iterator
    desugars into a higher-order function which is applied to another function
    containing the body of the iteration; that body function can't be filled in
    until the rest of the translations have been done. *)
let iterator ~transl_exp ~scopes = function
  | Texp_comp_range { ident; pattern = _; start; stop; direction } ->
      (* We have to let-bind [start] and [stop] so that they're evaluated in the
         correct (i.e., left-to-right) order *)
      let transl_bound var bound =
        Let_binding.make
          (Immutable Strict) (Pvalue Pintval)
          var (transl_exp ~scopes Jkind.Sort.for_predef_value bound)
      in
      let start = transl_bound "start" start in
      let stop  = transl_bound "stop"  stop  in
      { builder      = (match direction with
                        | Upto   -> rev_dlist_concat_iterate_up
                        | Downto -> rev_dlist_concat_iterate_down)
      ; arg_lets     = [start; stop]
      ; element      = ident
      ; element_kind = Pvalue Pintval
      ; add_bindings = Fun.id
      }
  | Texp_comp_in { pattern; sequence } ->
      let iter_list =
        Let_binding.make (Immutable Strict) (Pvalue Pgenval)
          "iter_list" (transl_exp ~scopes Jkind.Sort.for_predef_value sequence)
      in
      (* Create a fresh variable to use as the function argument *)
      let element = Ident.create_local "element" in
      { builder      = rev_dlist_concat_map
      ; arg_lets     = [iter_list]
      ; element
      ; element_kind =
          Typeopt.layout pattern.pat_env pattern.pat_loc
            Jkind.Sort.for_list_element pattern.pat_type
      ; add_bindings =
          (* CR layouts: to change when we allow non-values in sequences *)
          Matching.for_let
            ~scopes ~arg_sort:Jkind.Sort.for_list_element
            ~return_layout:(Pvalue Pgenval) pattern.pat_loc (Lvar element)
            pattern
      }

(** Translates a list comprehension binding
    ([Typedtree.comprehension_clause_binding]) into Lambda.  At parse time,
    iterators don't include patterns and bindings do; however, in the typedtree
    representation, the patterns have been moved into the iterators (so that
    range iterators can just have an [Ident.t], for translation into for loops),
    so bindings are just like iterators with a possible annotation.  As a
    result, this function is essentially the same as [iterator], which see. *)
let binding ~transl_exp ~scopes { comp_cb_iterator; comp_cb_attributes = _ } =
  (* No attributes are meaningful here; see the definition of
     [comp_cb_attributes]. *)
  iterator ~transl_exp ~scopes comp_cb_iterator

(** Translate all the bindings of a single [for ... and ...] clause (the
    contents of a [Typedtree.Texp_comp_for]) into a pair of (1) a list of let
    bindings that are in force for the translation; and (2) a single Lambda term
    of type ['res rev_dlist], assuming we know how to translate everything that
    ought to be nested within it (the [inner_body], a function awaiting the most
    nested accumulator as a labeled argument which will produce the body of the
    iterations) and have a name for the accumulator of the current [rev_dlist]
    ([accumulator], which changes at every recursive step).  It folds together
    all the [translated_iterator]s by connecting their [body_func]tions to each
    other, and bottoms out at the [inner_body].  *)
let rec translate_bindings
          ~transl_exp ~scopes ~loc ~inner_body ~accumulator = function
  | cur_binding :: bindings ->
      let { builder; arg_lets; element; element_kind; add_bindings } =
        binding ~transl_exp ~scopes cur_binding
      in
      let inner_acc = Ident.create_local "accumulator" in
      let body_arg_lets, body =
        translate_bindings
          ~transl_exp ~scopes ~loc
          ~inner_body ~accumulator:(Lvar inner_acc) bindings
      in
      let body_func =
        Lambda.lfunction
          ~kind:(Curried { nlocal = 2 })
          (* Only the accumulator is local, but since the function itself is
             local, [nlocal] has to be equal to the number of parameters *)
          ~params:[
            {name = element;
             layout = element_kind;
             attributes = Lambda.default_param_attribute;
             (* CR ncourant: check *)
             mode = alloc_heap};
            {name = inner_acc;
             layout = Pvalue Pgenval;
             attributes = Lambda.default_param_attribute;
             mode = alloc_local}
          ]
          ~return:(Pvalue Pgenval)
          ~attr:default_function_attribute
          ~loc
          ~mode:alloc_local
          ~region:false
          ~body:(add_bindings body)
      in
      let result =
        Lambda_utils.apply
          ~loc
          ~mode:alloc_local
          (Lazy.force builder)
          (List.map (fun Let_binding.{id; _} -> Lvar id) arg_lets @
           [body_func; accumulator])
          ~result_layout:(Pvalue Pgenval)
      in
      arg_lets @ body_arg_lets, result
  | [] ->
      [], inner_body ~accumulator

(** Translate a single clause, either [for ... and ...] or [when ...]
    ([Typedtree.comprehension_clause]), into a single Lambda term of type
    ['res rev_dlist], assuming we know how to translate everything that ought to
    be nested within it (the [comprehension_body], a a function awaiting the
    most nested accumulator as a labeled argument which will produce the body of
    the iterations) and have a name for the accumulator of the current
    [rev_dlist] ([accumulator], which changes at every recursive step). *)
let rec translate_clauses
          ~transl_exp ~scopes ~loc ~comprehension_body ~accumulator = function
  | clause :: clauses ->
      let body ~accumulator =
        translate_clauses ~transl_exp ~scopes ~loc
          ~comprehension_body ~accumulator clauses
      in begin
        match clause with
        | Texp_comp_for bindings ->
            let arg_lets, bindings =
              translate_bindings
                ~transl_exp ~scopes ~loc ~inner_body:body ~accumulator bindings
            in
            Let_binding.let_all arg_lets bindings
        | Texp_comp_when cond ->
            Lifthenelse(transl_exp ~scopes Jkind.Sort.for_predef_value cond,
                        body ~accumulator,
                        accumulator,
                        (Pvalue Pgenval) (* [list]s have the standard representation *))
      end
  | [] ->
      comprehension_body ~accumulator

let comprehension ~transl_exp ~scopes ~loc { comp_body; comp_clauses } =
  let rev_comprehension =
    translate_clauses ~transl_exp ~scopes ~loc
      ~comprehension_body:(fun ~accumulator ->
        rev_list_snoc_local
          ~loc
          ~init:accumulator
          ~last:(transl_exp ~scopes Jkind.Sort.for_list_element comp_body))
      ~accumulator:rev_list_nil
      comp_clauses
  in
  Lambda_utils.apply
    ~loc
    ~mode:alloc_heap
    (Lazy.force rev_list_to_list)
    [rev_comprehension]
    ~result_layout:(Pvalue Pgenval)
