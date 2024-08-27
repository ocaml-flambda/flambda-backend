open Lambda

(** Code-generation utilities for use when generating Lambda for
    comprehensions. *)

(** First-class let bindings (mutable and immutable); we sometimes need to
    collect these while translating array comprehension clauses and bind them
    later. *)
module Let_binding : sig
  (** Lambda distinguishes between binding forms that bind an immutable ([Llet])
      vs. mutable ([Lmutlet]) variable; however, we want to be able to abstract
      over these uniformly, as in some cases we will be collecting lists of
      variables we want to bind don't want to split the mutable and immutable
      ones apart. We thus combine the different immutable [let_kind]s with the
      option of having a mutable variable and use that to distinguish what sort
      of [let] we're generating. *)
  module Let_kind : sig
    (** What sort of variable are we binding? *)
    type t =
      | Immutable of let_kind
          (** Bind an immutable variable of the specified [let_kind]; corresponds to
          [Llet]. *)
      | Mutable  (** Bind a mutable variable; corresponds to [Lmutlet]. *)
  end

  (** The first-class (in OCaml) type of let bindings. *)
  type t = private
    { let_kind : Let_kind.t;
      layout : layout;
      id : Ident.t;
      init : lambda; (* initial value *)
      var : lambda (* occurrence of this variable *)
    }

  (** Create a fresh local identifier (with name as given by the string
      argument) to bind to an initial value given by the lambda argument. *)
  val make : Let_kind.t -> layout -> string -> lambda -> t

  (** Create a Lambda let-binding (with [Llet]) from a first-class let
      binding, providing the body. *)
  val let_one : t -> lambda -> lambda

  (** Create Lambda let-bindings (with [Llet]) from multiple first-class let
      bindings, providing the body. *)
  val let_all : t list -> lambda -> lambda
end

(** Convenience functions for working with the Lambda AST *)
module Lambda_utils : sig
  (** Creating AST fragments for constants from OCaml values *)
  module Constants : sig
    (** Lambda integer literals *)
    val int : int -> lambda

    (** Lambda float literals; be careful with unusual values, as this calls
        [Float.to_string] *)
    val float : float -> lambda

    (** Unboxed floats and ints *)
    val unboxed_float : float -> lambda

    val unboxed_float32 : float -> lambda

    val unboxed_int32 : Int32.t -> lambda

    val unboxed_int64 : Int64.t -> lambda

    val unboxed_vec128 : high:Int64.t -> low:Int64.t -> lambda

    val unboxed_nativeint : Targetint.t -> lambda

    (** Lambda string literals; these require a location, and are constructed as
        "quoted strings", not {fancy|delimited strings|fancy}. *)
    val string : loc:Location.t -> string -> lambda
  end

  (** Apply a Lambda function to some Lambda values, at a location; all the
      other information needed by [Lapply] is set to some default value. *)
  val apply :
    loc:scoped_location ->
    mode:alloc_mode ->
    lambda ->
    lambda list ->
    result_layout:layout ->
    lambda

  (** Nicer OCaml syntax for constructing Lambda ASTs that operate on integers;
      created by [int_ops], which includes the necessary location in all the
      operations *)
  module type Int_ops = sig
    (** Integer arithmetic *)

    val ( + ) : lambda -> lambda -> lambda

    val ( - ) : lambda -> lambda -> lambda

    val ( * ) : lambda -> lambda -> lambda

    val ( / ) : lambda -> lambda -> lambda

    (** Integer comparisons *)

    val ( = ) : lambda -> lambda -> lambda

    val ( <> ) : lambda -> lambda -> lambda

    val ( < ) : lambda -> lambda -> lambda

    val ( > ) : lambda -> lambda -> lambda

    val ( <= ) : lambda -> lambda -> lambda

    val ( >= ) : lambda -> lambda -> lambda

    (** Boolean logical operators *)

    val ( && ) : lambda -> lambda -> lambda

    val ( || ) : lambda -> lambda -> lambda

    (** Integer literals *)

    val l0 : lambda

    val l1 : lambda

    val i : int -> lambda
  end

  (** Construct an [Int_ops] module at the given location *)
  val int_ops : loc:scoped_location -> (module Int_ops)

  (** Expose functions to construct Lambda calls to primitives; some of their
      arguments have been given labels, but otherwise they mirror the primitives
      exactly *)
  module Primitive : sig
    (** [make_vect ~length ~init] calls the [caml_make_vect] C primitive, which
        creates an array of the given [length] containing that many copies of
        the given [init]ial value *)
    val make_vect :
      loc:scoped_location -> length:lambda -> init:lambda -> lambda

    (** [make_float_vect len] calls the [caml_make_float_vect] C primitive,
        which creates an unboxed float array of length [len] whose contents are
        uninitialized *)
    val make_float_vect : loc:scoped_location -> lambda -> lambda

    (** Like [make_float_vect] but for unboxed float32 arrays. *)
    val make_unboxed_float32_vect : loc:scoped_location -> lambda -> lambda

    (** Like [make_float_vect] but for unboxed int32 arrays. *)
    val make_unboxed_int32_vect : loc:scoped_location -> lambda -> lambda

    (** Like [make_float_vect] but for unboxed int64 arrays. *)
    val make_unboxed_int64_vect : loc:scoped_location -> lambda -> lambda

    (** Like [make_float_vect] but for unboxed nativeint arrays. *)
    val make_unboxed_nativeint_vect : loc:scoped_location -> lambda -> lambda

    (** Like [make_float_vect] but for unboxed vec128 arrays. *)
    val make_unboxed_vec128_vect : loc:scoped_location -> lambda -> lambda

    (** [array_append a1 a2] calls the [caml_array_append] C primitive, which
        creates a new array by appending [a1] and [a2] *)
    val array_append : loc:scoped_location -> lambda -> lambda -> lambda

    (** [array_sub a ~offset ~length] calls the [caml_array_sub] C primitive,
        which creates a new subarray corresponding to the subarray of [a]
        starting at the given [offset] with the given [length] *)
    val array_sub :
      loc:scoped_location -> lambda -> offset:lambda -> length:lambda -> lambda
  end
end

module Cps_utils : sig
  (** [compose_map f xs] applies [f] to every element of [xs], obtaining a list
      of functions, and then composes these functions (from left to right).
      This is useful for, e.g., combining a sequence of translated clauses into
      a single [lambda -> lambda] function. *)
  val compose_map : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

  (** [compose_map_acc f xs] is like [compose_map], but [f] returns a pair of a
      function and some extra data; the final result is then the composition (as
      in [compose_map]) paired with the list of all these extra values.  This is
      useful for combining a sequence of iterators into a single [lambda ->
      lambda] function and their list of generated bindings (a
      ['u Iterator_bindings.t list]), as the [binding] function returns this
      extra data. *)
  val compose_map_acc :
    ('a -> ('b -> 'b) * 'c) -> 'a list -> ('b -> 'b) * 'c list
end
