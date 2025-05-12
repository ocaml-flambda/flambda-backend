(** String metaprogramming used for test generation *)

(** Code assumed to be included by generated code *)
val preamble : bytecode:bool -> string

module Tree : sig
  type 'a t =
    | Branch of 'a t list  (** invariant: nonempty *)
    | Leaf of 'a

  (** Counts both leaves and branches as a node *)
  val enumerate_shapes : max_num_nodes:int -> unit t list

  (** Counts both leaves and branches-with-one-child as a node.

      [enumerate_shapes' ~max_nodes_and_singleton_branches:n] is similar to
      [enumerate_shapes ~max_num_nodes:(n + 1)] for small [n], but has
      subjectively more interesting tree shapes.

      For example, let [es4] = [enumerate_shapes ~max_num_nodes:4]
      and let [es'3] = [enumerate_shapes' ~max_leaves_and_singleton_branches:3].
      - Both contain the trees [L (L) (LL) (LLL) ((L)) ((L)L) (L(L)) ((LL))].
      - Only [es4] contains [(((L)))].
      - Only [es'3] contains [((LL)L) (L(LL))].
  *)
  val enumerate_shapes' : max_leaves_and_singleton_branches:int -> unit t list

  val enumerate : shape:unit t -> leaves:'a list -> 'a t list

  val to_string : ('a -> string) -> 'a t -> string

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

module Boxing : sig
  type t =
    | Boxed
    | Unboxed
end

module Layout : sig
  type value_kind =
    | Addr_non_float
    | Immediate
    | Float

  type t =
    | Product of t list
    | Value of value_kind
    | Float64
    | Float32
    | Bits64
    | Bits32
    | Vec128
    | Word

  val all_scannable : t -> bool

  val all_ignorable : t -> bool

  val reordered_in_block : t -> bool

  val contains_vec128 : t -> bool
end

module Type_structure : sig
  (** Treats all types as structural. Names are added via [Type_naming]. *)

  type t =
    | Record of t list * Boxing.t
    | Tuple of t list * Boxing.t
    | Option of t
    | Int
    | Int64
    | Int64_u
    | Int32
    | Int32_u
    | Nativeint
    | Nativeint_u
    | Float
    | Float_u
    | Float32
    | Float32_u
    | String
    | Int64x2_u

  val compare : t -> t -> int

  val layout : t -> Layout.t

  (** This differs from composing [layout] and [Layout.contains_vec128] because
     this function considers the fields of boxed values *)
  val contains_vec128 : t -> bool

  (** [None] if the tree is a [Leaf] (thus will always produce a boxed record *)
  val boxed_record_containing_unboxed_records : t Tree.t -> t option

  val is_flat_float_record : t -> bool

  (** [None] if block indices to arrays of [nested_unboxed_record t] are not
      supported *)
  val array_element : t Tree.t -> t option

  val to_string : t -> string

  (** Size in words, as in [Obj.size] *)
  val size : t -> bytecode:bool -> int
end

module Path : sig
  type access =
    | Field of string
    | Unboxed_field of string

  type t = access list

  val to_string : t -> string
end

module Type : sig
  type t =
    | Record of
        { name : string;
          fields : (string * t) list;
          boxing : Boxing.t
        }
    | Tuple of t list * Boxing.t
    | Option of t
    | Int
    | Int64
    | Int64_u
    | Int32
    | Int32_u
    | Nativeint
    | Nativeint_u
    | Float
    | Float_u
    | Float32
    | Float32_u
    | String
    | Int64x2_u

  val follow_path : t -> Path.t -> t

  val num_subvals_left_of_path : t -> Path.t -> int

  val compare : t -> t -> int

  val structure : t -> Type_structure.t

  (** Code for this type expression (e.g. "int option * float") *)
  val code : t -> string

  (** Given some integer seed, generate code for a value of this type. E.g.
      passing [3] gives [#(3, 4., "5")] for [#(option * float * string)]. *)
  val value_code : t -> int -> string

  (** The number of subvalues of this type, which we only use to generate
      non-overlapping values with [value_code]. E.g. [int option * #(float *
      float)] has three. We consider an [int64x2#] to have two. *)
  val num_subvals : t -> int

  (** Code that dynamically implements [value_code], creating a value from an
      integer seed bound to [i]. We should be able to generate this code:
      [let mk_value (i : int) : $ty_code = $mk_value_code] *)
  val mk_value_body_code : t -> string

  (** Code for the equality function. We should be able generate this code:
      [let eq : $ty_code @ local -> $ty_code @ local -> bool = $eq_code] *)
  val eq_code : t -> string

  (** Enumerate all paths into a nested unboxed record type. E.g. for an unboxed
      record with the structure:
      [#{ a : #{ b : int; c : int } }]
      produces:
      [(0, [<empty path>]), (1, [.#a]]); (2, [[.#a.#b]; [.#a.#c]])]
  *)
  val unboxed_paths_by_depth : t -> (int * Path.t list) list
end

module Type_naming : sig
  (** Adds names to a [Type_structure.t] to produce a [Type.t]. [t] memoizes
      this and keeps track of the next fresh name to use *)

  type t

  val empty : t

  val add_names : t -> Type_structure.t -> t * Type.t

  (** The list of type declarations for all nominal types produced *)
  val decls_code : t -> string list
end
