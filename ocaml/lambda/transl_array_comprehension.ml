open Lambda
open Typedtree
open Asttypes
open Transl_comprehension_utils
open Lambda_utils.Constants
open Lambda_utils.Primitive

(** Array comprehensions are compiled by turning into a nested series of loops
    that mutably update an array.  This is simple to say, but slightly tricky to
    do.  One complexity is that we want to apply an optimization to certain
    array comprehensions: if an array comprehension contains exactly one clause,
    and it’s a [for ... and ...] clause, then we can allocate an array of
    exactly the right size up front (instead of having to grow the generated
    array dynamically, as we usually do).  We call this the *fixed-size array
    comprehension optimization*.  We cannot do this with nested [for]s, as the
    sizes of iterators further to the right could depend on the values generated
    by those on the left; indeed, this is why we have [for ... and ...] instead
    of just allowing the user to nest [for]s.

    In general, there are three major sources of complexity to be aware of in
    this translation:

    1. We need to have a resizable array, as most array comprehensions have an
       unknown size (but see point (2)); however, OCaml arrays can't grow or
       shrink, so we have to do this ourselves.

    2. We need to perform the fixed-size array comprehension optimization, as
       described above; this requires handling things specially when the
       comprehension has the form [[|BODY for ITER and ITER ... and ITER|]].
       This ends up getting its tentacles throughout the entire module, as we
       want to share a lot of the code but have to parameterize it over these
       two kinds of output.

    3. We have to handle the float array optimization, so we can't simply
       allocate arrays in a uniform way; if we don't know what's in the array,
       we have to carefully handle things on the first iteration.  These details
       are more local in scope, but particularly fiddly.

    In general, the structure is: we allocate an array and a mutable index
    counter that starts at [0]; each iterator becomes a loop; [when] clauses
    become an [if] expression, same as with lists; and in the body, every time
    we generate an array element, we set it and increment the index counter by
    one.  If we're not in the fixed-size array case, then we also need the array
    to be growable, the first source of extra complexity; we keep track of the
    array size, and if we would ever exceed it, we double the size of the array.
    This means that at the end, we have to use a subarray operation to cut it
    down to the right size.

    In the fixed-size array case, the second source of extra complexity, we have
    to first compute the size of every iterator and multiply them together; in
    both cases, we have to check for overflow, in which case we simply fail.  We
    also check to see if any of the iterators would be empty (have size [0]), in
    which case we can shortcut this whole process and simply return an empty
    array.  Once we do that, though, the loop body is simpler as there's no need
    to double the array size, and we don't need to cut the list down to size at
    the end.  This has ramifications throughout the translation code, as we have
    to add a bunch of extra special-case logic to handle this: we have to store
    enough information to be able to compute iterator sizes if we need to; we
    have to be able to switch between having a resizable and a fixed-size array;
    we don't need to introduce the same number of variable bindings in each
    case; etc.  Various bits of the code make these decisions (for these
    examples: the [Iterator_bindings] module and the [initial_array] and [body]
    functions, all in this file).

    Finally, handling the float array optimization also affects the initial
    array and the element assignment (so this ends up being a locus for all the
    sources of complexity).  If the array has an unknown array kind
    ([Pgenarray]), then we can't allocate it with nonzero size without having
    the first element!  Thus, no matter whether we are in the normal case or the
    fixed-size case, we have to set the initial array to be completely empty.
    Then, on the first iteration through the loop, we can finally create the
    real array, by allocating either the initial values for a resizable array or
    precisely enough values for a fixed-size array and setting all of them to
    the newly-computed first element of the resulting array.  The initial array
    creation is done by the function [initial_array], and the index checking is
    done (among other things) by the function [body].

    To see some examples of what this translation looks like, consider the
    following array comprehension:
    {[
      [x+y for x = 1 to 3 when x <> 2 for y in [10*x; 100*x]]
      (* = [11; 101; 33; 303] *)
    ]}
    This translates to the (Lambda equivalent of) the following:
    {[
      (* Allocate the (resizable) array *)
      let array_size = ref 8 in
      let array      = ref [|0; 0; 0; 0; 0; 0; 0; 0|] in
      (* Next element to be generated *)
      let index = ref 0 in
      (* for x = 1 to 3 *)
      let start = 1 in
      let stop  = 3 in
      for x = start to stop do
        (* when x <> 2 *)
        if x <> 2 then
          (* for y in [|10*x; 100*x|] *)
          let iter_arr = [|10*x; 100*x|] in
          for iter_ix = 0 to Array.length iter_arr - 1 do
            let y = iter_arr.(iter_ix) in
            (* Resize the array if necessary *)
            if not (!index < !array_size) then begin
              array_size := 2 * !array_size;
              array      := Array.append !array !array
            end;
            (* The body: x + y *)
            !array.(!index) <- x + y;
            index := !index + 1
          done
      done;
      (* Cut the array back down to size *)
      Array.sub !array 0 !index
    ]}
    On the other hand, consider this array comprehension, which is subject to
    the fixed-size array comprehension optimization:
    {[
      [|x*y for x = 1 to 3 and y = 10 downto 8|]
      (* = [|10; 9; 8; 20; 18; 16; 30; 27; 24|] *)
    ]}
    This translates to the (Lambda equivalent of) the following rather different
    OCaml:
    {[
      (* ... = 1 to 3 *)
      let start_x = 1  in
      let stop_x  = 3  in
      (* ... = 10 downto 8 *)
      let start_y = 10 in
      let stop_y  = 8  in
      (* Check if any iterators are empty *)
      if start_x > stop_x || start_y < stop_y
      then
        (* If so, return the empty array *)
        [||]
      else
        (* Precompute the array size *)
        let array_size =
          (* Compute the size of the range [1 to 3], failing on overflow (the
             case where the range is correctly size 0 is handled by the
             emptiness check) *)
          let x_size =
            let range_size = (stop_x - start_x) + 1 in
            if range_size > 0
            then range_size
            else raise (Invalid_argument "integer overflow when precomputing \
                                          the size of an array comprehension")
          in
          (* Compute the size of the range [10 downto 8], failing on overflow
             (the case where the range is correctly size 0 is handled by the
             emptiness check) *)
          let y_size =
            let range_size = (start_y - stop_y) + 1 in
            if range_size > 0
            then range_size
            else raise (Invalid_argument "integer overflow when precomputing \
                                          the size of an array comprehension")
          in
          (* Multiplication that checks for overflow ([y_size] can't be [0]
             because we checked that above *)
          let product = x_size * y_size in
          if product / y_size = x_size
          then product
          else raise (Invalid_argument "integer overflow when precomputing \
                                        the size of an array comprehension")
        in
        (* Allocate the (nonresizable) array *)
        let array = Array.make array_size 0 in
        (* Next element to be generated *)
        let index = ref 0 in
        (* for x = 1 to 3 *)
        for x = start_x to stop_x do
          (* for y = 10 downto 8 *)
          for y = start_y downto stop_y do
            (* The body: x*y *)
            array.(!index) <- x*y;
            index := !index + 1
          done
        done;
        array
    ]}
    You can see that the loop body is tighter, but there's more up-front size
    checking work to be done. *)

(** An implementation note: Many of the functions in this file need to translate
    expressions from Typedtree to lambda; to avoid strange dependency ordering,
    we parameterize those functions by [Translcore.transl_exp], and pass it in
    as a labeled argument, along with the necessary [scopes] labeled argument
    that it requires. *)

module Precompute_array_size : sig
  (** Generates the lambda expression that throws the exception once we've
      determined that precomputing the array size has overflowed.  The check for
      overflow is done elsewhere; this just throws the exception
      unconditionally. *)
  val raise_overflow_exn : loc:scoped_location -> lambda

  (** [safe_product_pos_vals ~loc xs] generates the lambda expression that
      computes the product of all the lambda terms in [xs] assuming they are all
      strictly positive (nonzero!) integers, failing if any product overflows
      (equivalently, if the whole product would overflow).  This function must
      look at its inputs multiple times, as they are evaluated more than once
      due to the overflow check; the optional argument [variable_name]
      customizes the string used to name these variables. *)
  val safe_product_pos :
    ?variable_name:string -> loc:scoped_location -> lambda list -> lambda
end = struct
  (* Modeled after [Translcore.assert_failed] *)
  let raise_overflow_exn ~loc =
    let loc' = Debuginfo.Scoped_location.to_location loc in
    let slot =
      transl_extension_path loc (Lazy.force Env.initial)
        Predef.path_invalid_argument
    in
    (* CR-someday aspectorzabusky: We might want to raise an event here for
       debug tracing (cf. [Translcore.assert_failed] and
       [Translprim.event_after]), but it's not clear what event that would be,
       and this isn't a feature we expect to use.  We can add it when it seems
       important, or when we upstream this change. *)
    Lprim
      ( Praise Raise_regular,
        [ Lprim
            ( Pmakeblock (0, Immutable, None, alloc_heap),
              [ slot;
                string ~loc:loc'
                  "integer overflow when precomputing the size of an array \
                   comprehension" ],
              loc ) ],
        loc )

  (** [safe_mul_pos_vals ~loc x y] generates the lambda expression that computes
      the product [x * y] of two strictly positive (nonzero!) integers and fails
      if this overflowed; the inputs are required to be values, as they are
      evaluated more than once *)
  let safe_mul_pos_vals ~loc x y =
    let y = y.Let_binding.var in
    let open (val Lambda_utils.int_ops ~loc) in
    let product =
      Let_binding.make (Immutable Alias) (Pvalue Pintval) "product" (x * y)
    in
    (* [x * y] is safe, for strictly positive [x] and [y], iff you can undo the
       multiplication: [(x * y)/y = x].  We assume the inputs are values, so we
       don't have to bind them first to avoid extra computation. *)
    Let_binding.let_one product
      (Lifthenelse
         ( product.var / y = x,
           product.var,
           raise_overflow_exn ~loc,
           Pvalue Pintval ))

  (** [safe_product_pos_vals ~loc xs] generates the lambda expression that
      computes the product of all the lambda values in [xs] assuming they are
      all strictly positive (nonzero!) integers, failing if any product
      overflows; the inputs are required to be values, as they are evaluated
      more than once *)
  let safe_product_pos_vals ~loc = function
    (* This operation is associative, so the fact that [List.fold_left] brackets
       as [(((one * two) * three) * four)] shouldn't matter *)
    | x :: xs -> List.fold_left (safe_mul_pos_vals ~loc) x.Let_binding.var xs
    | [] -> int 1
  (* The empty list case can't happen with comprehensions; we could raise an
     error here instead of returning 1 *)

  (* The inputs are *not* required to be values, as we save them in variables.
     We could avoid making let-bindings for lambda-terms that are already
     variables, but we assume the optimizer can deal with that case nicely. *)
  let safe_product_pos ?(variable_name = "x") ~loc factors =
    let factors =
      List.map
        (Let_binding.make (Immutable Strict) (Pvalue Pintval) variable_name)
        factors
    in
    Let_binding.let_all factors (safe_product_pos_vals ~loc factors)
end

(** This module contains the type of bindings generated when translating array
    comprehension iterators ([Typedtree.comprehension_iterator]s).  We need more
    structure than a [Let_binding.t list] because of the fixed-size array
    optimization: if we're translating an array comprehension whose size can be
    determined ahead of time, such as
    [[|x,y for x = 1 to 10 and y in some_array|]], then we need to be able to
    precompute the sizes of the iterators.  This means that we don't just need
    the list of bindings, but we also need to know which bindings are which.
    Thus, this module, which allows you to work with a structured representation
    of the translated iterator bindings. *)
module Iterator_bindings = struct
  (** This is the type of bindings generated when translating array
      comprehension iterators ([Typedtree.comprehension_iterator]).  If we are
      in the fixed-size array case, then ['u = many], and we remember all the
      information about the right-hand sides of the iterators; if not, then
      ['u = once], and we only remember those bindings that could have side
      effects, using the other terms directly.  (This means that we remember the
      [start] and [stop] of [to] and [downto] iterators, and the array on the
      right-hand side of an [in] iterator; this last binding is also always
      referenced multiple times.) *)
  type t =
    | Range of
        { start : Let_binding.t; (* Always bound *)
          stop : Let_binding.t; (* Always bound *)
          direction : direction_flag
        }
        (** The translation of [Typedtree.Texp_comp_range], an integer iterator
        ([... = ... (down)to ...]) *)
    | Array of
        { iter_arr : Let_binding.t; (* Always bound *)
          iter_len : Let_binding.t
        }
        (** The translation of [Typedtree.Texp_comp_in], an array iterator
        ([... in ...]).  Note that we always remember the array ([iter_arr]), as
        it's indexed repeatedly no matter what. *)

  (** Get the [Let_binding.t]s out of a translated iterator *)
  let let_bindings = function
    | Range { start; stop; direction = _ } -> [start; stop]
    | Array { iter_arr; iter_len } -> [iter_arr; iter_len]

  (** Get the [Let_binding.t]s out of a list of translated iterators; this is
      the information we need to translate a full [for] comprehension clause
      ([Typedtree.Texp_comp_for]). *)
  let all_let_bindings bindings = List.concat_map let_bindings bindings

  (** Functions for use in the fixed-size array case *)
  module Fixed_size_array = struct
    (** Check if a translated iterator is empty; that is, check if this iterator
        will iterate over zero things. *)
    let is_empty ~loc t =
      let open (val Lambda_utils.int_ops ~loc) in
      match t with
      | Range { start; stop; direction } -> (
        let start = start.var in
        let stop = stop.var in
        match direction with Upto -> start > stop | Downto -> start < stop)
      | Array { iter_arr = _; iter_len } -> iter_len.var = l0

    (** Check if any of the translated iterators are empty; that is, check if
        any of these iterators will iterate over zero things, and thus check if
        iterating over all of these iterators together will actually iterate
        over zero things.  This is the information we need to optimize away
        iterating over the values at all if the result would have zero
        elements. *)
    let are_any_empty ~loc ts =
      let open (val Lambda_utils.int_ops ~loc) in
      match List.map (is_empty ~loc) ts with
      | is_empty :: are_empty ->
        (* ( || ) is associative, so the fact that [List.fold_left] brackets
           as [(((one || two) || three) || four)] shouldn't matter *)
        List.fold_left ( || ) is_empty are_empty
      | [] -> l0
    (* false *)
    (* The empty list case can't happen with comprehensions; we could
       raise an error here instead *)

    (** Compute the size of a single nonempty array iterator.  This is either
        the size of a range, which itself is either [stop - start + 1] or
        [start - stop + 1] depending on if the array is counting up ([to]) or
        down ([downto]), clamped to being nonnegative; or it is the length of
        the array being iterated over.  In the range case, we also have to check
        for overflow.  We require that the iterators be nonempty, although this
        is only important for the range case; generate Lambda code that checks
        the result of [are_any_empty] before entering [size_nonempty] to ensure
        this. *)
    let size_nonempty ~loc = function
      | Range { start; stop; direction } ->
        let open (val Lambda_utils.int_ops ~loc) in
        let start = start.var in
        let stop = stop.var in
        let low, high =
          match direction with Upto -> start, stop | Downto -> stop, start
        in
        (* We can assume that the range is nonempty, but computing its size
           still might overflow *)
        let range_size =
          Let_binding.make (Immutable Alias) (Pvalue Pintval) "range_size"
            (high - low + l1)
        in
        Let_binding.let_one range_size
          (* If the computed size of the range is positive, there was no
             overflow; if it was zero or negative, then there was overflow *)
          (Lifthenelse
             ( range_size.var > l0,
               range_size.var,
               Precompute_array_size.raise_overflow_exn ~loc,
               Pvalue Pintval ))
      | Array { iter_arr = _; iter_len } -> iter_len.var

    (** Compute the total size of an array built out of a list of translated
        iterators, as long as all the iterators are nonempty; since this forms a
        cartesian product, we take the product of the sizes (see
        [size_nonempty]).  This can overflow, in which case we will raise an
        exception.  This is the operation needed to precompute the fixed size of
        a nonempty fixed-size array; check against [are_any_empty] first to
        address the case of fixedly-empty array. *)
    let total_size_nonempty ~loc iterators =
      Precompute_array_size.safe_product_pos ~variable_name:"iterator_size" ~loc
        (List.map (size_nonempty ~loc) iterators)
  end
end

(** Machinery for working with resizable arrays for the results of an array
    comprehension: they are created at a fixed, known, small size, and are
    doubled in size when necessary.  These are the arrays that back array
    comprehensions by default, but not in the fixed-size case; in that case, we
    simply construct an array of the appropriate size directly.  We could have
    built something like this as an OCaml module instead, but doing it directly
    in Lambda was simpler, particularly around the ability to alternate freely
    with the fixed-size case. *)
module Resizable_array = struct
  (** The starting size of a resizable array.  This is guaranteed to be a small
      power of two.  Because we resize the array by doubling, using a power of
      two means that, under the assumption that [Sys.max_array_length] is of the
      form 2^x-1, the array will only grow too large one iteration before it
      would otherwise exceed the limit.  (In practice, the program will fail by
      running out of memory first.) *)
  let starting_size = 8

  (** Create a fresh resizable array: it is mutable and has [starting_size]
      elements.  We have to provide the initial value as well as the array kind,
      thanks to the float array optimization, so sometimes this will be a
      default value and sometimes it will be the first element of the
      comprehension. *)
  let make ~loc array_kind elt =
    Lprim
      ( Pmakearray (array_kind, Mutable, alloc_heap),
        Misc.replicate_list elt starting_size,
        loc )

  (** Create a new array that's twice the size of the old one.  The first half
      of the array contains the same elements, and the latter half's contents
      are unspecified.  Note that this does not update [array] itself. *)
  let double ~loc array = array_append ~loc array array
  (* Implementing array doubling in by appending an array to itself may not be
     the optimal way to do array doubling, but it's good enough for now *)
end

(** Translates an iterator ([Typedtree.comprehension_iterator]), one piece of a
    [for ... and ... and ...] expression, into Lambda.  We translate iterators
    from the "outermost" iterator inwards, so this translation is done in CPS;
    the result of the translation is actually a function that's waiting for the
    body to fill into the translated loop.  The term generated by this function
    will execute the body (which is likely made of further translated iterators
    and suchlike) once for every value being iterated over, with all the
    variables bound over by the iterator available.

    This function returns both a pair of said CPSed Lambda term and the let
    bindings generated by this term (as an [Iterator_bindings.t], which see). *)
let iterator ~transl_exp ~scopes ~loc :
    comprehension_iterator -> (lambda -> lambda) * Iterator_bindings.t =
  function
  | Texp_comp_range { ident; pattern = _; start; stop; direction } ->
    let bound name value =
      Let_binding.make (Immutable Strict) (Pvalue Pintval) name
        (transl_exp ~scopes Jkind.Sort.for_predef_value value)
    in
    let start = bound "start" start in
    let stop = bound "stop" stop in
    let mk_iterator body =
      Lfor
        { for_id = ident;
          for_loc = loc;
          for_from = start.var;
          for_to = stop.var;
          for_dir = direction;
          for_body = body
        }
    in
    mk_iterator, Range { start; stop; direction }
  | Texp_comp_in { pattern; sequence = iter_arr_exp } ->
    let iter_arr =
      Let_binding.make (Immutable Strict) (Pvalue Pgenval) "iter_arr"
        (transl_exp ~scopes Jkind.Sort.for_predef_value iter_arr_exp)
    in
    let iter_arr_kind =
      (* CR layouts v4: [~elt_sort:None] here is not ideal and
         should be fixed. To do that, we will need to store a sort
         on [Texp_comp_in]. *)
      Typeopt.array_type_kind ~elt_sort:None iter_arr_exp.exp_env
        iter_arr_exp.exp_loc iter_arr_exp.exp_type
    in
    let iter_len =
      (* Extra let-binding if we're not in the fixed-size array case; the
         middle-end will simplify this for us *)
      Let_binding.make (Immutable Alias) (Pvalue Pintval) "iter_len"
        (Lprim (Parraylength iter_arr_kind, [iter_arr.var], loc))
    in
    let iter_ix = Ident.create_local "iter_ix" in
    let mk_iterator body =
      let open (val Lambda_utils.int_ops ~loc) in
      (* for iter_ix = 0 to Array.length iter_arr - 1 ... *)
      (* CR layouts v4: will need updating when we allow non-values in arrays. *)
      Lfor
        { for_id = iter_ix;
          for_loc = loc;
          for_from = l0;
          for_to = iter_len.var - l1;
          for_dir = Upto;
          for_body =
            Matching.for_let ~scopes
              ~arg_sort:Jkind.Sort.for_array_comprehension_element
              ~return_layout:(Pvalue Pintval) pattern.pat_loc
              (Lprim
                 ( Parrayrefu
                     ( Lambda.(array_ref_kind alloc_heap iter_arr_kind),
                       Ptagged_int_index ),
                   [iter_arr.var; Lvar iter_ix],
                   loc ))
              pattern body
        }
    in
    mk_iterator, Array { iter_arr; iter_len }

(** Translates an array comprehension binding
    ([Typedtree.comprehension_clause_binding]) into Lambda.  At parse time,
    iterators don't include patterns and bindings do; however, in the typedtree
    representation, the patterns have been moved into the iterators (so that
    range iterators can just have an [Ident.t], for translation into for loops),
    so bindings are just like iterators with a possible annotation.  As a
    result, this function is essentially the same as [iterator], which see. *)
let binding ~transl_exp ~scopes ~loc
    { comp_cb_iterator; comp_cb_attributes = _ } =
  (* No attributes are meaningful here; see the definition of
     [comp_cb_attributes]. *)
  iterator ~transl_exp ~loc ~scopes comp_cb_iterator

(** Translate the contents of a single [for ... and ...] clause (the contents of
    a [Typedtree.Texp_comp_for]) into Lambda, returning both the [lambda ->
    lambda] function awaiting the body of the translated loop, and the ['u
    Iterator_bindings.t list] containing all the bindings generated by the
    individual iterators.  This function is factored out of [clause] because it
    is also used separately in the fixed-size case. *)
let for_and_clause ~transl_exp ~scopes ~loc =
  Cps_utils.compose_map_acc (binding ~transl_exp ~loc ~scopes)

(** Translate a single clause, either [for ... and ...] or [when ...]
    ([Typedtree.comprehension_clause]), into Lambda, returning the [lambda ->
    lambda] function awaiting the body of the loop or conditional corresponding
    to this clause.  The argument to that function will be executed once for
    every tuple of elements being iterated over in the [for ... and ...] case,
    or it will be executed iff the condition is true in the [when] case.

    This function is only used if we are not in the fixed-size array case; see
    [clauses] and [for_and_clause] for more details. *)
let clause ~transl_exp ~scopes ~loc = function
  | Texp_comp_for bindings ->
    let make_clause, var_bindings =
      for_and_clause ~transl_exp ~loc ~scopes bindings
    in
    fun body ->
      Let_binding.let_all
        (Iterator_bindings.all_let_bindings var_bindings)
        (make_clause body)
  | Texp_comp_when cond ->
    fun body ->
      Lifthenelse
        ( transl_exp ~scopes Jkind.Sort.for_predef_value cond,
          body,
          lambda_unit,
          Pvalue Pintval (* [unit] is immediate *) )

(** The [array_sizing] type describes whether an array comprehension has been
    translated using the fixed-size array optimization ([Fixed_size]), or it has
    not been but instead been translated using the usual dynamically-sized array
    ([Dynamic_size]).

    If an array comprehension is of the form
    {[
      [|BODY for ITER and ITER ... and ITER|]
    ]}
    then we can compute the size of the resulting array before allocating it
    ([Fixed_size]); otherwise, we cannot ([Dynamic_size]), and we have to
    dynamically grow the array as we iterate and shrink it to size at the
    end. *)

type array_sizing =
  | Fixed_size
  | Dynamic_size

(** The [array_sizing_info] type is like [array_sizing], but includes any extra
    information necessary to construct the array comprehension at the very top
    level; it does not need to be passed down into the various functions that
    create pieces, which can operate only knowing the [array_sizing]. *)
type array_sizing_info =
  | Fixed_size_info of Iterator_bindings.t list
      (** In the fixed-size case, we need to collect the lengths of the iterators
      being iterated over, which determine the size of the array; thus, the
      iterator bindings need to be available early, before we even allocate the
      array. *)
  | Dynamic_size_info
      (** In the dynamic-size case, we don't need to collect any other
      information. *)

(** The result of translating the clauses portion of an array comprehension
    (everything but the body) *)
type translated_clauses =
  { array_sizing_info : array_sizing_info;
        (** Whether the array is of a fixed size or must be grown dynamically, along
      with supporting information; see the [array_sizing] and
      [array_sizing_info] types for more details. *)
    array_size : Let_binding.t;
        (** The binding that defines the array size; comes in between any extra
      information from [array_sizing_info] and the definition of the array.  In
      the case where the array has been translated with the fixed-size array
      optimization (when [array_sizing_info] is [Fixed_size _]), the variable
      holding the size is immutable; in the usual dynamically-sized array case
      (when [array_sizing_info] is [Dynamic_size_info]), the variable holding
      the size is mutable so that the array size can be grown.*)
    make_comprehension : lambda -> lambda
        (** The translation of the comprehension's iterators, awaiting the translation
      of the comprehension's body.  All that remains to be done after this
      function is called is the creation and disposal of the array that is being
      constructed; the extra information from [array_sizing_info] must also be
      applied to the result, outside of even the creation of the array. *)
  }

(** Translate the clauses of an array comprehension (everything but the body; a
    [Typedtree.comprehension_clause list], which is the [comp_clauses] field of
    [Typedtree.comprehension]).  This function has to handle the fixed-size
    array case: if the list of clauses is a single [for ... and ...] clause,
    then the array will be preallocated at its full size and the comprehension
    will not have to resize the array (although the float array optimization
    interferes with this slightly -- see [initial_array]); this is also why we
    need the [array_kind].  In the normal case, this function simply wires
    together multiple [clause]s, and provides the variable holding the current
    array size as a binding. *)
let clauses ~transl_exp ~scopes ~loc = function
  | [Texp_comp_for bindings] ->
    let make_comprehension, var_bindings =
      for_and_clause ~transl_exp ~loc ~scopes bindings
    in
    let array_size =
      Let_binding.make (Immutable Alias) (Pvalue Pintval) "array_size"
        (Iterator_bindings.Fixed_size_array.total_size_nonempty ~loc
           var_bindings)
    in
    { array_sizing_info = Fixed_size_info var_bindings;
      array_size;
      make_comprehension
    }
  | clauses ->
    let array_size =
      Let_binding.make Mutable (Pvalue Pintval) "array_size"
        (int Resizable_array.starting_size)
    in
    let make_comprehension =
      Cps_utils.compose_map (clause ~transl_exp ~loc ~scopes) clauses
    in
    { array_sizing_info = Dynamic_size_info; array_size; make_comprehension }

(** Create the initial array that will be filled by an array comprehension,
    returning both its identifier and the let binding that binds it.  The logic
    behind how to create the array is complicated, because it lies at the
    intersection of two special cases (controlled by the two non-location
    arguments to this function):

    * The float array optimization means that we may not know the type of
      elements that go into this array, and so need to wait to actually create
      an array until we have seen the first element.  In this case, we have to
      return an empty array that will get overwritten later.

    * The fixed-size optimization means that we may want to preallocate the
      entire array all at once, instead of allocating a resizable array and
      growing it.

    Importantly, the two cases can co-occur, in which case later code needs to
    be aware of what has happened.

    The array that is returned is bound as a [Variable] in both the case where
    we're subject to the float array optimization (i.e., [array_kind] is
    [Pgenarray]) and in the case where nothing special occurs and the array is
    resizable; in the fixed-size array case, the resulting array is bound
    immutably, although it is still internally mutable.  This logic is important
    when translating comprehension bodies; see [body] for details. *)
let initial_array ~loc ~array_kind ~array_size ~array_sizing =
  (* As discussed above, there are three cases to consider for how we allocate
     the array.

     1. We are subject to the float array optimization: The array kind is
        [Pgenarray].  In this case, we create an immutable empty array as a
        [Variable], since rather than being updated it will simply be
        overwritten once we have the first element.  This is the only time a
        fixed-size array needs to be a [Variable], since it will be overwritten
        on the first iteration.
     2. The array is of fixed size and known array kind, in which case we use
        [make_(float_)vect] to create the array, and bind it as [StrictOpt]
        since it never needs to be overwritten to be resized or replaced.
     3. The array is of unknown size and known array kind, in which case we
        create a small array of default values using [Pmakearray] and bind it as
        a [Variable] so that it can be overwritten when its size needs to be
        doubled. *)
  let array_let_kind, array_value =
    let open Let_binding in
    let open Let_kind in
    match array_sizing, array_kind with
    (* Case 1: Float array optimization difficulties *)
    | (Fixed_size | Dynamic_size), Pgenarray ->
      ( Mutable,
        Lprim (Pmakearray (Pgenarray, Immutable, Lambda.alloc_heap), [], loc) )
    (* Case 2: Fixed size, known array kind *)
    | Fixed_size, (Pintarray | Paddrarray) ->
      Immutable StrictOpt, make_vect ~loc ~length:array_size.var ~init:(int 0)
    | Fixed_size, (Pfloatarray | Punboxedfloatarray Pfloat64) ->
      (* The representations of these two are the same, it's only
         accesses that differ. *)
      Immutable StrictOpt, make_float_vect ~loc array_size.var
    | Fixed_size, Punboxedfloatarray Pfloat32 ->
      Immutable StrictOpt, make_unboxed_float32_vect ~loc array_size.var
    | Fixed_size, Punboxedintarray Pint32 ->
      Immutable StrictOpt, make_unboxed_int32_vect ~loc array_size.var
    | Fixed_size, Punboxedintarray Pint64 ->
      Immutable StrictOpt, make_unboxed_int64_vect ~loc array_size.var
    | Fixed_size, Punboxedintarray Pnativeint ->
      Immutable StrictOpt, make_unboxed_nativeint_vect ~loc array_size.var
    (* Case 3: Unknown size, known array kind *)
    | Dynamic_size, (Pintarray | Paddrarray) ->
      Mutable, Resizable_array.make ~loc array_kind (int 0)
    | Dynamic_size, Pfloatarray ->
      Mutable, Resizable_array.make ~loc array_kind (float 0.)
    | Dynamic_size, Punboxedfloatarray Pfloat64 ->
      Mutable, Resizable_array.make ~loc array_kind (unboxed_float 0.)
    | Dynamic_size, Punboxedfloatarray Pfloat32 ->
      Mutable, Resizable_array.make ~loc array_kind (unboxed_float32 0.)
    | Dynamic_size, Punboxedintarray Pint32 ->
      Mutable, Resizable_array.make ~loc array_kind (unboxed_int32 0l)
    | Dynamic_size, Punboxedintarray Pint64 ->
      Mutable, Resizable_array.make ~loc array_kind (unboxed_int64 0L)
    | Dynamic_size, Punboxedintarray Pnativeint ->
      ( Mutable,
        Resizable_array.make ~loc array_kind (unboxed_nativeint Targetint.zero)
      )
    | _, (Pgcscannableproductarray _ | Pgcignorableproductarray _) ->
      Misc.fatal_error
        "Transl_array_comprehension.initial_array: unboxed product array"
  in
  Let_binding.make array_let_kind (Pvalue Pgenval) "array" array_value

(** Generate the code for the body of an array comprehension.  This involves
    translating the body expression (a [Typedtree.expression], which is the
    [comp_body] field of [Typedtree.comprehension], but also handles the logic
    of filling in the array that is being produced by the comprehension.  This
    logic varies depending on whether we are subject to the float array
    optimization or not and whether we are in the fixed size array case or not,
    so the correctness depends on getting the correct bindings from
    [initial_array] and [clauses]. *)
let body ~loc ~array_kind ~array_size ~array_sizing ~array ~index ~body =
  (* The body of an array comprehension has three jobs:
       1. Compute the next element
       2. Assign it (mutably) to the next element of the array
       3. Advance the index of the next element
     However, there are several pieces of complexity:
       (a) If the array size is not fixed, we have to check if the index has
           overflowed; if it has, we have to double the size of the array.  (The
           complex case corresponds to [array_sizing] being [Dynamic_size].)
       (b) If the array kind is not statically known, we initially created an
           empty array; we have to check if we're on the first iteration and use
           the putative first element of the array as the placeholder value for
           every element of the array.  (The complex case corresponds to
           [array_kind] being [Pgenarray].)
       (c) If both (a) and (b) hold, we shouldn't bother checking for an
           overflowed index on the first loop iteration.
     The result is that we build the "set the element" behavior in three steps:
       i.   First, we build the raw "set the element unconditionally" expression
            ([set_element_raw]).
       ii.  Then, if necessary, we precede that with the resizing check;
            otherwise, we leave the raw behavior alone
            ([set_element_in_bounds]).
       iii. Then, if necessary, we check to see if we're on the first iteration
            and create the fresh array instead if so; otherwise, we leave the
            size-safe behavior alone ([set_element_known_kind_in_bounds]).
       iv.  Finally, we take the resulting safe element-setting behavior (which
            could be equal to the result from any of stages i--iii), and follow
            it up by advancing the index of the element to update.
  *)
  let open (val Lambda_utils.int_ops ~loc) in
  let open Let_binding in
  let set_element_raw elt =
    (* array.(index) <- elt *)
    Lprim
      ( Parraysetu
          (Lambda.(array_set_kind modify_heap array_kind), Ptagged_int_index),
        [array.var; index.var; elt],
        loc )
  in
  let set_element_in_bounds elt =
    match array_sizing with
    | Fixed_size -> set_element_raw elt
    | Dynamic_size ->
      Lsequence
        ( (* Double the size of the array if it's time... *)
          Lifthenelse
            ( index.var < array_size.var,
              lambda_unit,
              Lsequence
                ( Lassign (array_size.id, i 2 * array_size.var),
                  Lassign (array.id, Resizable_array.double ~loc array.var) ),
              Pvalue Pintval (* [unit] is immediate *) ),
          (* ...and then set the element now that the array is big enough *)
          set_element_raw elt )
  in
  let set_element_known_kind_in_bounds =
    match array_kind with
    | Pgenarray ->
      let is_first_iteration = index.var = l0 in
      let elt =
        Let_binding.make (Immutable Strict) (Pvalue Pgenval) "elt" body
      in
      let make_array =
        match array_sizing with
        | Fixed_size -> make_vect ~loc ~length:array_size.var ~init:elt.var
        | Dynamic_size -> Resizable_array.make ~loc Pgenarray elt.var
      in
      Let_binding.let_one elt
        (Lifthenelse
           ( is_first_iteration,
             Lassign (array.id, make_array),
             set_element_in_bounds elt.var,
             Pvalue Pintval (* [unit] is immediate *) ))
    | Pintarray | Paddrarray | Pfloatarray
    | Punboxedfloatarray (Pfloat64 | Pfloat32)
    | Punboxedintarray _ ->
      set_element_in_bounds body
    | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
      Misc.fatal_error "Transl_array_comprehension.body: unboxed product array"
  in
  Lsequence
    (set_element_known_kind_in_bounds, Lassign (index.id, index.var + l1))

let comprehension ~transl_exp ~scopes ~loc ~(array_kind : Lambda.array_kind)
    { comp_body; comp_clauses } =
  (match array_kind with
  | Pgenarray | Paddrarray | Pintarray | Pfloatarray -> ()
  | Punboxedfloatarray _ | Punboxedintarray _ ->
    if not !Clflags.native_code
    then
      Misc.fatal_errorf
        "Array comprehensions for kind %s are not allowed in bytecode"
        (Printlambda.array_kind array_kind);
    if Targetint.size <> 64
    then
      Misc.fatal_errorf
        "Array comprehensions for kind %s can only be compiled for 64-bit \
         native targets"
        (Printlambda.array_kind array_kind)
  | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
    Misc.fatal_error
      "Transl_array_comprehension.comprehension: unboxed product array");
  let { array_sizing_info; array_size; make_comprehension } =
    clauses ~transl_exp ~scopes ~loc comp_clauses
  in
  let array_sizing =
    match array_sizing_info with
    | Fixed_size_info _ -> Fixed_size
    | Dynamic_size_info -> Dynamic_size
  in
  let array = initial_array ~loc ~array_kind ~array_size ~array_sizing in
  let index = Let_binding.make Mutable (Pvalue Pintval) "index" (int 0) in
  (* The core of the comprehension: the array, the index, and the iteration that
     fills everything in.  The translation of the clauses will produce a check
     to see if we can avoid doing the hard work of growing the array, which is
     the case when the array is known to be empty after the fixed-size array
     optimization; we also have to check again when we're done. *)
  let comprehension =
    Let_binding.let_all [array_size; array; index]
      (Lsequence
         ( (* Create the array *)
           make_comprehension
             (body ~loc ~array_kind ~array_size ~array_sizing ~array
                ~index
                  (* CR layouts v4: Ensure that the [transl_exp] here can cope
                     with non-values. *)
                ~body:
                  (transl_exp ~scopes Jkind.Sort.for_array_comprehension_element
                     comp_body)),
           (* If it was dynamically grown, cut it down to size *)
           match array_sizing with
           | Fixed_size -> array.var
           | Dynamic_size ->
             array_sub ~loc array.var ~offset:(int 0) ~length:index.var ))
  in
  (* If we're in the fixed-size array case, do the extra setup necessary. *)
  match array_sizing_info with
  | Fixed_size_info var_bindings ->
    Let_binding.let_all
      (Iterator_bindings.all_let_bindings var_bindings)
      (Lifthenelse
         ( Iterator_bindings.Fixed_size_array.are_any_empty ~loc var_bindings,
           (* If the array is known to be empty, we short-circuit and return the
              empty array; all empty arrays are identical, so we don't care
              about its kind or mutability *)
           Lprim (Pmakearray (Pgenarray, Immutable, Lambda.alloc_heap), [], loc),
           (* Otherwise, we translate it normally *)
           comprehension,
           (* (And the result has the [value_kind] of the array) *)
           Pvalue (Parrayval array_kind) ))
  | Dynamic_size_info -> comprehension
