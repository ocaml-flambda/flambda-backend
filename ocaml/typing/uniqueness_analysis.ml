(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Uniqueness analysis, ran after type-checking *)

open Asttypes
open Types
open Mode
open Typedtree
module Uniqueness = Mode.Uniqueness
module Linearity = Mode.Linearity

module Occurrence = struct
  (** The occurrence of a potentially unique ident in the expression. Currently
  it's just the location; might add more things in the future *)
  type t = { loc : Location.t }

  let mk loc = { loc }
end

let rec iter_error f = function
  | [] -> Ok ()
  | x :: xs -> ( match f x with Ok () -> iter_error f xs | Error e -> Error e)

module Maybe_unique : sig
  (** The type representing a usage that could be either unique or aliased *)
  type t

  (** extract an arbitrary occurrence from this usage *)
  val extract_occurrence : t -> Occurrence.t

  (** construct a single usage *)
  val singleton : unique_use -> Occurrence.t -> t

  val meet : t -> t -> t

  type axis =
    | Uniqueness
    | Linearity

  (** Describes why cannot force aliased - including the failing occurrence, and
      the failing axis *)
  type cannot_force =
    { occ : Occurrence.t;
      axis : axis
    }

  (** Call this function to indicate that this is used multiple times *)
  val mark_multi_use : t -> (unit, cannot_force) result

  (** Returns the uniqueness represented by this usage. If this identifier is
      expected to be unique in any branch, it will return unique. If the current
      usage is forced, it will return aliased. *)
  val uniqueness : t -> Uniqueness.r
end = struct
  (** Occurrences with modes to be forced aliased and many in the future if
      needed. This is a list because of multiple control flows. For example, if
      a value is used aliased in one branch but unique in another branch, then
      overall the value is used uniquely (this is a "stricter" requirement).
      Therefore, techincally, the mode this list represents is the meet of all
      modes in the lists. (recall that aliased > unique). Therefore, if this
      virtual mode needs to be forced aliased, the whole list needs to be forced
      aliased. *)
  type t = (unique_use * Occurrence.t) list

  let singleton unique_use occ : t = [unique_use, occ]

  let uniqueness l = Uniqueness.meet (List.map (fun ((uniq, _), _) -> uniq) l)

  type axis =
    | Uniqueness
    | Linearity

  type cannot_force =
    { occ : Occurrence.t;
      axis : axis
    }

  let mark_multi_use l =
    let force_one ((uni, lin), occ) =
      (* values being multi-used means two things:
         - the expected mode must be higher than [aliased]
         - the access mode must be lower than [many] *)
      match Linearity.submode lin Linearity.many with
      | Error _ -> Error { occ; axis = Linearity }
      | Ok () -> (
        match Uniqueness.submode Uniqueness.aliased uni with
        | Ok () -> Ok ()
        | Error _ -> Error { occ; axis = Uniqueness })
    in
    iter_error force_one l

  let extract_occurrence = function [] -> assert false | (_, occ) :: _ -> occ

  let meet l0 l1 = l0 @ l1
end

module Maybe_aliased : sig
  type t

  type access =
    | Read of Unique_barrier.t
    | Write

  val string_of_access : access -> string

  (** The type representing a usage that could be either aliased or borrowed *)

  (** Extract an arbitrary occurrence from the usage *)
  val extract_occurrence : t -> Occurrence.t

  (** extract an arbitrary access from this usage *)
  val extract_access : t -> access

  (** Add a barrier. The uniqueness mode represents the usage immediately
      following the current usage. If that mode is Unique, the current usage
       must be Borrowed (hence no code motion); if that mode is not restricted
       to Unique, this usage can be Borrowed or Aliased (prefered). Can be called
       multiple times for multiple barriers (for different branches). *)
  val add_barrier : t -> Uniqueness.r -> unit

  val meet : t -> t -> t

  val singleton : Occurrence.t -> access -> t
end = struct
  type access =
    | Read of Unique_barrier.t
    | Write

  let string_of_access = function
    | Read _ -> "read from"
    | Write -> "written to"

  (** list of occurences together with modes to be forced as borrowed in the
  future if needed. It is a list because of multiple control flows. For
  example, if a value is used borrowed in one branch but aliased in another,
  then the overall usage is aliased. Therefore, the mode this list represents
  is the meet of all modes in the list. (recall that borrowed > aliased).
  Therefore, if this virtual mode needs to be forced borrowed, the whole list
  needs to be forced borrowed. *)
  type t = (Occurrence.t * access) list

  let meet l0 l1 = l0 @ l1

  let singleton occ access = [occ, access]

  let extract_occurrence = function [] -> assert false | (occ, _) :: _ -> occ

  let extract_access = function
    | [] -> assert false
    | (_, access) :: _ -> access

  let add_barrier t uniq =
    List.iter
      (fun (_, access) ->
        match access with
        | Read barrier -> Unique_barrier.add_upper_bound uniq barrier
        | _ -> ())
      t
end

module Aliased : sig
  type t

  type reason =
    | Forced  (** aliased because forced  *)
    | Lazy  (** aliased because it is the argument of lazy forcing *)
    | Lifted of Maybe_aliased.access
        (** aliased because lifted from implicit borrowing, carries the original
          access *)

  (** The occurrence is only for future error messages. The share_reason must
  corresponds to the occurrence *)
  val singleton : Occurrence.t -> reason -> t

  val extract_occurrence : t -> Occurrence.t

  val reason : t -> reason
end = struct
  type reason =
    | Forced
    | Lazy
    | Lifted of Maybe_aliased.access

  type t = Occurrence.t * reason

  let singleton occ reason = occ, reason

  let extract_occurrence (occ, _) = occ

  let reason (_, reason) = reason
end

module Usage : sig
  type t =
    | Unused  (** empty usage *)
    | Borrowed of Occurrence.t
        (** A borrowed usage with an arbitrary occurrence. The occurrence is
        only for future error messages. Currently not used, because we don't
        have explicit borrowing *)
    | Maybe_aliased of Maybe_aliased.t
        (** A usage that could be either borrowed or aliased. *)
    | Aliased of Aliased.t  (** A aliased usage *)
    | Maybe_unique of Maybe_unique.t
        (** A usage that could be either unique or aliased. *)

  val aliased : Occurrence.t -> Aliased.reason -> t

  val maybe_unique : unique_use -> Occurrence.t -> t

  (** Extract an arbitrary occurrence from a usage *)
  val extract_occurrence : t -> Occurrence.t option

  type first_or_second =
    | First
    | Second

  type error =
    { cannot_force : Maybe_unique.cannot_force;
      there : t;  (** The other usage  *)
      first_or_second : first_or_second
          (** Is it the first or second usage that's failing force? *)
    }

  exception Error of error

  (** Sequential composition *)
  val seq : t -> t -> t

  (** Non-deterministic choice *)
  val choose : t -> t -> t

  (** Parallel composition *)
  val par : t -> t -> t
end = struct
  (* We have Unused (top) > Borrowed > Aliased > Unique > Error (bot).

     - Unused means unused
     - Borrowed means read-only access confined to a region
     - Aliased means read-only access that may escape a region. For example,
     storing the value in a cell that can be accessed later.
     - Unique means accessing the value as if it's the only pointer. Example
     includes overwriting.
     - Error means error happens when composing usage.

     Some observations:
     - It is sound to relax mode towards Error. It grants the access more
     "capability" and usually helps performance.
       For example, relaxing borrowed to aliased allows code motion of
       projections. Relaxing aliased to unique allows in-place update.

       An example of the relaxing borrowed to aliased:

       let x = r.a in
       (a lot of codes)
       x

       In first line, r.memory_address is accessed as borrowed. But if we weaken
       it to aliased and it still mode checks, that means
       - there is no "unique" access in the "a lot of codes"
       - or equivalently, that r.memory_address stays unchanged and safe to read

       and as a result, we can delay the projection at `x`.

       The downside of relaxing is the loss of completeness: if we relax too
       much the program will fail type check. In the extreme case we relax it to
       Error which fails type check outright (and extremely sound, hehe).

     - The purpose of this uniqueness analysis is to figure out the most relaxed
     mode for each use, such that we get the best performance, while still
     type-check. Currently there are really only two choices worth figuring out,
     Namely
     - borrowed or aliased?
     - aliased or unique?

     As a result, instead of having full-range inference, we only care about the
     following ranges:
     - unused
     - borrowed (Currently not useful, because we don't have explicit borrowing)
     - borrowed or aliased
     - aliased
     - aliased or unique
     - error

     error is represented as exception which is just easier.
  *)

  type t =
    | Unused
    | Borrowed of Occurrence.t
    | Maybe_aliased of Maybe_aliased.t
    | Aliased of Aliased.t
    | Maybe_unique of Maybe_unique.t

  let aliased occ reason = Aliased (Aliased.singleton occ reason)

  let maybe_unique unique_use occ =
    Maybe_unique (Maybe_unique.singleton unique_use occ)

  let extract_occurrence = function
    | Unused -> None
    | Borrowed occ -> Some occ
    | Maybe_aliased t -> Some (Maybe_aliased.extract_occurrence t)
    | Aliased t -> Some (Aliased.extract_occurrence t)
    | Maybe_unique t -> Some (Maybe_unique.extract_occurrence t)

  let choose m0 m1 =
    match m0, m1 with
    | Unused, m | m, Unused -> m
    | Borrowed _, t | t, Borrowed _ -> t
    | Maybe_aliased l0, Maybe_aliased l1 ->
      Maybe_aliased (Maybe_aliased.meet l0 l1)
    | Maybe_aliased _, t | t, Maybe_aliased _ ->
      (* The barrier stays empty; if there is any unique after this,
         the analysis will error *)
      t
    | Aliased _, t | t, Aliased _ -> t
    | Maybe_unique l0, Maybe_unique l1 -> Maybe_unique (Maybe_unique.meet l0 l1)

  type first_or_second =
    | First
    | Second

  type error =
    { cannot_force : Maybe_unique.cannot_force;
      there : t;
      first_or_second : first_or_second
    }

  exception Error of error

  let force_aliased_multiuse t there first_or_second =
    match Maybe_unique.mark_multi_use t with
    | Ok () -> ()
    | Error cannot_force ->
      raise (Error { cannot_force; there; first_or_second })

  let par m0 m1 =
    match m0, m1 with
    | Unused, m | m, Unused -> m
    | Borrowed occ, Borrowed _ -> Borrowed occ
    | Borrowed _, Maybe_aliased t | Maybe_aliased t, Borrowed _ ->
      Maybe_aliased t
    | Borrowed _, Aliased t | Aliased t, Borrowed _ -> Aliased t
    | Borrowed occ, Maybe_unique t | Maybe_unique t, Borrowed occ ->
      force_aliased_multiuse t (Borrowed occ) First;
      aliased (Maybe_unique.extract_occurrence t) Aliased.Forced
    | Maybe_aliased t0, Maybe_aliased t1 ->
      Maybe_aliased (Maybe_aliased.meet t0 t1)
    | Maybe_aliased _, Aliased occ | Aliased occ, Maybe_aliased _ ->
      (* The barrier stays empty; if there is any unique after this,
         the analysis will error *)
      Aliased occ
    | Maybe_aliased t0, Maybe_unique t1 | Maybe_unique t1, Maybe_aliased t0 ->
      (* t1 must be aliased *)
      force_aliased_multiuse t1 (Maybe_aliased t0) First;
      (* The barrier stays empty; if there is any unique after this,
         the analysis will error *)
      aliased (Maybe_unique.extract_occurrence t1) Aliased.Forced
    | Aliased t0, Aliased _ -> Aliased t0
    | Aliased t0, Maybe_unique t1 ->
      force_aliased_multiuse t1 (Aliased t0) Second;
      Aliased t0
    | Maybe_unique t1, Aliased t0 ->
      force_aliased_multiuse t1 (Aliased t0) First;
      Aliased t0
    | Maybe_unique t0, Maybe_unique t1 ->
      force_aliased_multiuse t0 m1 First;
      force_aliased_multiuse t1 m0 Second;
      aliased (Maybe_unique.extract_occurrence t0) Aliased.Forced

  let seq m0 m1 =
    match m0, m1 with
    | Unused, m | m, Unused -> m
    | Borrowed _, t -> t
    | Maybe_aliased _, Borrowed _ -> m0
    | Maybe_aliased l0, Maybe_aliased l1 ->
      Maybe_aliased (Maybe_aliased.meet l0 l1)
    | Maybe_aliased _, Aliased _ -> m1
    | Maybe_aliased l0, Maybe_unique l1 ->
      (* Four cases (semi-colon meaning sequential composition):
          Borrowed;Aliased = Aliased
          Borrowed;Unique = Unique
          Aliased;Aliased = Aliased
          Aliased;Unique = Error

          We are in a dilemma: recall that Borrowed->Aliased allows code
          motion, and Aliased->Unique allows unique overwriting. We can't have
          both. We first note is that the first is a soft optimization, and
          the second is a hard requirement.

          A reasonable solution is thus to check if the m1 actually needs
          to use the "unique" capabilities. If not, there is no need to
          relax it to Unique, and we will make it Aliased, and make m0
          Aliased for code-motion. However, there is no good way to do that,
          because the "unique_use" in "maybe_unique" is not complete,
          because the type-checking and uniqueness analysis is performed on
          a per-top-level-expr basis.

          Our solution is to record on the m0 that it is constrained by the
          m1. I.e. if m1 is Unique, then m0 cannot be Aliased. After the type
          checking of the whole file, m1 will correctly tells whether it needs
          to be Unique, and by extension whether m0 can be Aliased. *)
      let uniq = Maybe_unique.uniqueness l1 in
      Maybe_aliased.add_barrier l0 uniq;
      m1
    | Aliased _, Borrowed _ -> m0
    | Maybe_unique l, Borrowed occ ->
      force_aliased_multiuse l m1 First;
      aliased occ Aliased.Forced
    | Aliased _, Maybe_aliased _ ->
      (* The barrier stays empty; if there is any unique after this,
         the analysis will error *)
      m0
    | Maybe_unique l0, Maybe_aliased l1 ->
      (* Four cases:
          Aliased;Borrowed = Aliased
          Aliased;Aliased = Aliased
          Unique;Borrowed = Error
          Unique;Aliased = Error

          As you can see, we need to force the m0 to Aliased, and m1 needn't
          be constrained. The result is always Aliased.
          The barrier stays empty; if there is any unique after this,
          the analysis will error.
      *)
      let occ = Maybe_aliased.extract_occurrence l1 in
      force_aliased_multiuse l0 m1 First;
      aliased occ Aliased.Forced
    | Aliased _, Aliased _ -> m0
    | Maybe_unique l, Aliased _ ->
      force_aliased_multiuse l m1 First;
      m1
    | Aliased _, Maybe_unique l ->
      force_aliased_multiuse l m0 Second;
      m0
    | Maybe_unique l0, Maybe_unique l1 ->
      force_aliased_multiuse l0 m1 First;
      force_aliased_multiuse l1 m0 Second;
      aliased (Maybe_unique.extract_occurrence l0) Aliased.Forced
end

module Projection : sig
  (** Projections from parent to child. *)
  type t =
    | Tuple_field of int
    | Record_field of string
    | Construct_field of string * int
    | Variant_field of label
    | Memory_address (* this is rendered as clubsuit in the ICFP'24 paper *)

  module Map : Map.S with type key = t
end = struct
  module T = struct
    type t =
      | Tuple_field of int
      | Record_field of string
      | Construct_field of string * int
      | Variant_field of label
      | Memory_address

    let compare t1 t2 =
      match t1, t2 with
      | Tuple_field i, Tuple_field j -> Int.compare i j
      | Record_field l1, Record_field l2 -> String.compare l1 l2
      | Construct_field (l1, i), Construct_field (l2, j) -> (
        match String.compare l1 l2 with 0 -> Int.compare i j | i -> i)
      | Variant_field l1, Variant_field l2 -> String.compare l1 l2
      | Memory_address, Memory_address -> 0
      | ( Tuple_field _,
          (Record_field _ | Construct_field _ | Variant_field _ | Memory_address)
        ) ->
        -1
      | ( (Record_field _ | Construct_field _ | Variant_field _ | Memory_address),
          Tuple_field _ ) ->
        1
      | Record_field _, (Construct_field _ | Variant_field _ | Memory_address)
        ->
        -1
      | (Construct_field _ | Variant_field _ | Memory_address), Record_field _
        ->
        1
      | Construct_field _, (Variant_field _ | Memory_address) -> -1
      | (Variant_field _ | Memory_address), Construct_field _ -> 1
      | Variant_field _, Memory_address -> -1
      | Memory_address, Variant_field _ -> 1
  end

  include T
  module Map = Map.Make (T)
end

type boundary_reason =
  | Paths_from_mod_class (* currently will never trigger *)
  | Free_var_of_mod_class (* currently will never trigger *)
  | Out_of_mod_class

(** The relation between two nodes in a usage tree. Obviously the list must be
    non-empty *)
type relation =
  | Self
  | Ancestor of Projection.t list
  | Descendant of Projection.t list

type error =
  | Usage of
      { inner : Usage.error;
            (** Describes the error concerning the two usages  *)
        first_is_of_second : relation
            (** The relation between the two usages in the tree  *)
      }
  | Boundary of
      { cannot_force : Maybe_unique.cannot_force;
        reason : boundary_reason
      }

exception Error of error

(** lifting module Usage to trees *)
module Usage_tree : sig
  module Path : sig
    (** Represents a path from the root to a node in a tree *)
    type t

    (** Constructing a child path *)
    val child : Projection.t -> t -> t

    (** The path representing the root node *)
    val root : t
  end

  (** Usage tree, lifted from [Usage.t] *)
  type t

  (** Sequential composition lifted from [Usage.seq] *)
  val seq : t -> t -> t

  (** Non-deterministic choice lifted from [Usage.choose] *)
  val choose : t -> t -> t

  (** Parallel composition lifted from [Usage.par]  *)
  val par : t -> t -> t

  (** A singleton tree containing only one leaf *)
  val singleton : Usage.t -> Path.t -> t

  (** Runs a function through the tree; the function must be monotone *)
  val mapi : (Path.t -> Usage.t -> Usage.t) -> t -> t
end = struct
  (** Represents a tree of usage. Each node records the choose on all possible
     execution paths. As a result, trees such as `S -> U` is valid, even though
     it would be invalid if it was the result of a single path: using a parent
     aliased and a child uniquely is obviously bad. However, it might be the
     result of "choos"ing multiple path: choose `S` `N -> U`, which is valid.

     INVARIANT: children >= parent. For example, having an aliased child under a
     unique parent is nonsense. The invariant is preserved because Usage.choose,
     Usage.par, and Usage.seq above are monotone, and Usage_tree.par and
     Usage_tree.seq, Usage_tree.choose here are node-wise. *)
  type t =
    { children : t Projection.Map.t;
      usage : Usage.t
    }

  module Path = struct
    type t = Projection.t list

    let child (a : Projection.t) (p : t) : t = p @ [a]

    let root : t = []
  end

  let mapi_aux projs f t =
    let rec loop projs t =
      let usage = f projs t.usage in
      let children =
        Projection.Map.mapi (fun proj t -> loop (proj :: projs) t) t.children
      in
      { usage; children }
    in
    loop projs t

  let mapi f t = mapi_aux [] f t

  let rec mapi2 f t0 t1 =
    let usage = f Self t0.usage t1.usage in
    let children =
      Projection.Map.merge
        (fun proj c0 c1 ->
          match c0, c1 with
          | None, None -> assert false
          | None, Some c1 ->
            Some
              (mapi_aux [proj]
                 (fun projs r -> f (Ancestor projs) t0.usage r)
                 c1)
          | Some c0, None ->
            Some
              (mapi_aux [proj]
                 (fun projs l -> f (Descendant projs) l t1.usage)
                 c0)
          | Some c0, Some c1 -> Some (mapi2 f c0 c1))
        t0.children t1.children
    in
    { usage; children }

  let lift f t0 t1 =
    mapi2
      (fun first_is_of_second t0 t1 ->
        try f t0 t1
        with Usage.Error error ->
          raise (Error (Usage { inner = error; first_is_of_second })))
      t0 t1

  let choose t0 t1 = lift Usage.choose t0 t1

  let seq t0 t1 = lift Usage.seq t0 t1

  let par t0 t1 = lift Usage.par t0 t1

  let rec singleton leaf = function
    | [] -> { usage = leaf; children = Projection.Map.empty }
    | proj :: path ->
      { usage = Unused;
        children = Projection.Map.singleton proj (singleton leaf path)
      }
end

(** Lift Usage_tree to forest *)
module Usage_forest : sig
  module Path : sig
    type t

    (** Construct a child path from a parent  *)
    val child : Projection.t -> t -> t

    (** Create a fresh tree in the forest *)
    val fresh_root : unit -> t
  end

  (** Represents a forest of usage. *)
  type t

  (** Similar to [Usage_tree.seq] but lifted to forests *)
  val seq : t -> t -> t

  (** Similar to [Usage_tree.choose] but lifted to forests *)
  val choose : t -> t -> t

  (** Similar to [Usage_tree.par] but lifted to forests *)
  val par : t -> t -> t

  val seqs : t list -> t

  val chooses : t list -> t

  val pars : t list -> t

  (** The empty forest *)
  val unused : t

  (** The forest with only one usage, given by the path and the usage *)
  val singleton : Usage.t -> Path.t -> t

  (** Run a function through a forest. The function must be monotone *)
  val map : (Usage.t -> Usage.t) -> t -> t
end = struct
  module Root_id = struct
    module T = struct
      (* Contains only one field but we might extend in the future *)
      type t = { id : int } [@@unboxed]

      let compare t1 t2 = t1.id - t2.id
    end

    include T
    module Map = Map.Make (T)

    let stamp = ref 0

    let fresh () =
      let id = !stamp in
      stamp := id + 1;
      { id }
  end

  type t = Usage_tree.t Root_id.Map.t

  module Path = struct
    type t = Root_id.t * Usage_tree.Path.t

    let child proj ((rootid, path) : t) : t =
      rootid, Usage_tree.Path.child proj path

    let fresh_root () : t = Root_id.fresh (), Usage_tree.Path.root
  end

  let unused = Root_id.Map.empty

  (** [f] must be monotone  *)
  let map2 f t0 t1 =
    Root_id.Map.merge
      (fun _rootid t0 t1 ->
        match t0, t1 with
        | None, None -> assert false
        | None, Some t1 -> Some t1
        | Some t0, None -> Some t0
        | Some t0, Some t1 -> Some (f t0 t1))
      t0 t1

  let choose t0 t1 = map2 Usage_tree.choose t0 t1

  let seq t0 t1 = map2 Usage_tree.seq t0 t1

  let par t0 t1 = map2 Usage_tree.par t0 t1

  let chooses l = List.fold_left choose unused l

  let seqs l = List.fold_left seq unused l

  let pars l = List.fold_left par unused l

  let singleton leaf ((rootid, path') : Path.t) =
    Root_id.Map.singleton rootid (Usage_tree.singleton leaf path')

  (** f must be monotone *)
  let map f =
    Root_id.Map.mapi (fun _root tree ->
        Usage_tree.mapi (fun _projs usage -> f usage) tree)
end

module UF = Usage_forest

module Paths : sig
  [@@@warning "-unused-value-declaration"]

  (** Represents a list of [UF.Path.t]  *)
  type t

  (** Returns the element-wise child *)
  val child : Projection.t -> t -> t

  (** Represents a value whose modes are managed by the type checker.
      It is ignored by uniqueness analysis and represented as an empty list *)
  val untracked : t

  (** [modal_child gf proj t] is [child prof t] when [gf] is [Unrestricted]
      and is [untracked] otherwise. *)
  val modal_child : Modality.Value.Const.t -> Projection.t -> t -> t

  (** [tuple_field i t] is [child (Projection.Tuple_field i) t]. *)
  val tuple_field : int -> t -> t

  (** [record_field gf s t] is
      [modal_child gf (Projection.Record_field s) t]. *)
  val record_field : Modality.Value.Const.t -> string -> t -> t

  (** [construct_field gf s i t] is
      [modal_child gf (Projection.Construct_field(s, i)) t]. *)
  val construct_field : Modality.Value.Const.t -> string -> int -> t -> t

  (** [variant_field s t] is [child (Projection.Variant_field s) t]. *)
  val variant_field : string -> t -> t

  (** [memory_address t] is [child Projection.Memory_address t]. *)
  val memory_address : t -> t

  val mark : Usage.t -> t -> UF.t

  val fresh : unit -> t

  val choose : t -> t -> t

  val mark_implicit_borrow_memory_address :
    Occurrence.t -> Maybe_aliased.access -> t -> UF.t

  val mark_aliased : Occurrence.t -> Aliased.reason -> t -> UF.t
end = struct
  type t = UF.Path.t list

  let choose a b = a @ b

  let untracked = []

  let child proj t = List.map (UF.Path.child proj) t

  let modal_child gf proj t =
    (* CR zqian: Instead of just ignoring such children, we should add modality
       to [Projection.t] and add corresponding logic in [UsageTree]. *)
    let gf = Modality.Value.Const.to_list gf in
    let l =
      List.filter
        (function
         | Atom (Monadic Uniqueness, Join_with Aliased) -> true
         | Atom (Comonadic Linearity, Meet_with Many) -> true
         | _ -> false
          : Modality.t -> _)
        gf
    in
    if List.length l = 2 then untracked else child proj t

  let tuple_field i t = child (Projection.Tuple_field i) t

  let record_field gf s t = modal_child gf (Projection.Record_field s) t

  let construct_field gf s i t =
    modal_child gf (Projection.Construct_field (s, i)) t

  let variant_field s t = child (Projection.Variant_field s) t

  let memory_address t = child Projection.Memory_address t

  let mark usage t = UF.chooses (List.map (UF.singleton usage) t)

  let fresh () = [UF.Path.fresh_root ()]

  let mark_implicit_borrow_memory_address occ access paths =
    mark
      (Maybe_aliased (Maybe_aliased.singleton occ access))
      (memory_address paths)

  let mark_aliased occ reason paths = mark (Usage.aliased occ reason) paths
end

let force_aliased_boundary unique_use occ ~reason =
  let maybe_unique = Maybe_unique.singleton unique_use occ in
  match Maybe_unique.mark_multi_use maybe_unique with
  | Ok () -> ()
  | Error cannot_force -> raise (Error (Boundary { cannot_force; reason }))

module Value : sig
  (** See [existing] for its meaning *)
  type t

  (** A value contains the list of paths it could point to, the unique_use if
      it's a variable, and its occurrence in the source code. [unique_use] could
      be None if it's not a variable (e.g. result of an application) *)
  val existing : Paths.t -> unique_use -> Occurrence.t -> t

  (** A value not yet being existing by the analysis *)
  val fresh : t

  (** The untracked value, lifted from [Paths.untracked] *)
  val untracked : unique_use -> Occurrence.t -> t

  (** [paths t] is [None] if [t] is fresh and [Some p] if [t] is
      existing where [p] are its associated paths *)
  val paths : t -> Paths.t option

  (** [implicit_record_field gf s t u] is [fresh] if [t] is [fresh],
      otherwise it is [existing (Paths.record_field gf s p) o u] where [p]
      are the paths of [t] and [o] is [t]'s occurrence. This is used for the
      implicit record field values for kept fields in a [{ foo with ... }]
      expression. *)
  val implicit_record_field :
    Modality.Value.Const.t -> string -> t -> unique_use -> t

  (** Mark the value as aliased_or_unique   *)
  val mark_maybe_unique : t -> UF.t

  (** Mark the memory_address of the value as implicitly borrowed
      (borrow_or_aliased). *)
  val mark_implicit_borrow_memory_address : Maybe_aliased.access -> t -> UF.t

  val mark_aliased : reason:boundary_reason -> t -> UF.t
end = struct
  type t =
    | Fresh
    | Existing of
        { paths : Paths.t;
          unique_use : unique_use;
          occ : Occurrence.t
        }

  let existing paths unique_use occ = Existing { paths; unique_use; occ }

  let fresh = Fresh

  let paths = function Fresh -> None | Existing { paths; _ } -> Some paths

  let untracked unique_use occ = existing Paths.untracked unique_use occ

  let implicit_record_field gf s t unique_use =
    match t with
    | Fresh -> Fresh
    | Existing { paths; occ; unique_use = _ } ->
      let paths = Paths.record_field gf s paths in
      Existing { paths; occ; unique_use }

  let mark_implicit_borrow_memory_address access = function
    | Fresh -> UF.unused
    | Existing { paths; occ; _ } ->
      Paths.mark_implicit_borrow_memory_address occ access paths

  let mark_maybe_unique = function
    | Fresh -> UF.unused
    | Existing { paths; unique_use; occ } ->
      Paths.mark (Usage.maybe_unique unique_use occ) paths

  let mark_aliased ~reason = function
    | Fresh -> UF.unused
    | Existing { paths; unique_use; occ } ->
      force_aliased_boundary unique_use occ ~reason;
      let aliased = Usage.aliased occ Aliased.Forced in
      Paths.mark aliased paths
end

module Ienv : sig
  module Extension : sig
    (** Extention to Ienv. Usually generated by a pattern *)
    type t

    (** Composition for [OR] patterns. This operation is commutative *)
    val disjunct : t -> t -> t

    (** Composition for conjunctive patterns. The two extensions must be
    disjoint. *)
    val conjunct : t -> t -> t

    (** Similar to [conjunct] but lifted to lists *)
    val conjuncts : t list -> t

    (** The empty extension *)
    val empty : t

    val singleton : Ident.t -> Paths.t -> t
    (* Constructing a mapping with only one mapping  *)
  end

  (** Mapping from identifiers to a list of possible nodes, each represented by
      a path into the forest, instead of directly ponting to the node. *)
  type t

  (** Extend a mapping with an extension *)
  val extend : t -> Extension.t -> t

  (** The empty mapping  *)
  val empty : t

  (** Find the list of paths corresponding to an identifier  *)
  val find_opt : Ident.t -> t -> Paths.t option
end = struct
  module Extension = struct
    type t = Paths.t Ident.Map.t

    let disjunct ienv0 ienv1 =
      Ident.Map.merge
        (fun _id locs0 locs1 ->
          match locs0, locs1 with
          | None, None -> None
          | Some paths0, Some paths1 -> Some (Paths.choose paths0 paths1)
          (* cannot bind variable only in one of the OR-patterns *)
          | _, _ -> assert false)
        ienv0 ienv1

    let empty = Ident.Map.empty

    let conjunct ienv0 ienv1 =
      Ident.Map.union
        (fun _id _ _ ->
          (* cannot bind variable twice in a single pattern *)
          assert false)
        ienv0 ienv1

    let conjuncts = List.fold_left conjunct empty

    let singleton id locs = Ident.Map.singleton id locs
  end

  type t = Paths.t Ident.Map.t

  let empty = Ident.Map.empty

  let extend t ex =
    Ident.Map.union
      (* the extension shadows the original *)
        (fun _id _paths0 paths1 -> Some paths1)
      t ex

  let find_opt = Ident.Map.find_opt
end

(* The fun algebraic stuff ends. Here comes the concrete mess *)

(* Forcing due to boundary is more about OCaml than the algebra, hence defined
   here (as opposed to earlier) *)

type value_to_match =
  | Match_tuple of Value.t list
      (** The value being matched is a tuple; we treat it specially so matching
  tuples against tuples merely create alias instead of uses; We need [Value.t]
  instead of [Paths.t] because the tuple could be bound to a variable, in which
    case all values in the tuple is considered used *)
  | Match_single of Paths.t  (** The value being matched is not a tuple *)

let conjuncts_pattern_match l =
  let exts, ufs = List.split l in
  Ienv.Extension.conjuncts exts, UF.pars ufs

let rec pattern_match_tuple pat values =
  match pat.pat_desc with
  | Tpat_or (pat0, pat1, _) ->
    Unique_barrier.enable pat.pat_unique_barrier;
    let ext0, uf0 = pattern_match_tuple pat0 values in
    let ext1, uf1 = pattern_match_tuple pat1 values in
    Ienv.Extension.disjunct ext0 ext1, UF.choose uf0 uf1
  | Tpat_tuple pats ->
    (* No read: the tuple does not exist in memory *)
    Unique_barrier.enable pat.pat_unique_barrier;
    List.map2
      (fun (_, pat) value ->
        let paths =
          match Value.paths value with
          | None -> Paths.fresh ()
          | Some paths -> paths
        in
        pattern_match_single pat paths)
      pats values
    |> conjuncts_pattern_match
  | _ ->
    (* Mark all values in the tuple as used, because we are binding the tuple
       to a variable *)
    let uf = UF.seqs (List.map Value.mark_maybe_unique values) in
    let paths = Paths.fresh () in
    let ext, uf' = pattern_match_single pat paths in
    ext, UF.seq uf uf'

and pattern_match_single pat paths : Ienv.Extension.t * UF.t =
  let loc = pat.pat_loc in
  let occ = Occurrence.mk loc in
  (* To read from the allocation, we need to borrow its memory cell
     and set the unique_barrier. However, we do not read in every case,
     since the user might want use a wildcard for already-consumed data. *)
  let borrow_memory_address access =
    Unique_barrier.enable pat.pat_unique_barrier;
    match access with
    | `NoRead -> UF.unused
    | `Read ->
      Paths.mark_implicit_borrow_memory_address occ
        (Read pat.pat_unique_barrier) paths
  in
  match pat.pat_desc with
  | Tpat_or (pat0, pat1, _) ->
    let uf_read = borrow_memory_address `NoRead in
    let ext0, uf0 = pattern_match_single pat0 paths in
    let ext1, uf1 = pattern_match_single pat1 paths in
    Ienv.Extension.disjunct ext0 ext1, UF.par uf_read (UF.choose uf0 uf1)
  | Tpat_any ->
    let uf_read = borrow_memory_address `NoRead in
    Ienv.Extension.empty, uf_read
  | Tpat_var (id, _, _, _) ->
    let uf_read = borrow_memory_address `NoRead in
    Ienv.Extension.singleton id paths, uf_read
  | Tpat_alias (pat', id, _, _, _) ->
    let uf_read = borrow_memory_address `NoRead in
    let ext0 = Ienv.Extension.singleton id paths in
    let ext1, uf = pattern_match_single pat' paths in
    Ienv.Extension.conjunct ext0 ext1, UF.par uf_read uf
  | Tpat_constant _ ->
    let uf_read = borrow_memory_address `Read in
    Ienv.Extension.empty, uf_read
  | Tpat_construct (lbl, cd, pats, _) ->
    let uf_read = borrow_memory_address `Read in
    let pats_args = List.combine pats cd.cstr_args in
    let ext, uf_pats =
      List.mapi
        (fun i (pat, { Types.ca_modalities = gf; _ }) ->
          let name = Longident.last lbl.txt in
          let paths = Paths.construct_field gf name i paths in
          pattern_match_single pat paths)
        pats_args
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_pats
  | Tpat_variant (lbl, arg, _) ->
    let uf_read = borrow_memory_address `Read in
    let ext, uf_arg =
      match arg with
      | Some arg ->
        let paths = Paths.variant_field lbl paths in
        pattern_match_single arg paths
      | None -> Ienv.Extension.empty, UF.unused
    in
    ext, UF.par uf_read uf_arg
  | Tpat_record (pats, _) ->
    let uf_read = borrow_memory_address `Read in
    let ext, uf_pats =
      List.map
        (fun (_, l, pat) ->
          let paths = Paths.record_field l.lbl_modalities l.lbl_name paths in
          pattern_match_single pat paths)
        pats
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_pats
  | Tpat_array (_, _, pats) ->
    let uf_read = borrow_memory_address `Read in
    let ext, uf_pats =
      List.map
        (fun pat ->
          let paths = Paths.fresh () in
          pattern_match_single pat paths)
        pats
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_pats
  | Tpat_lazy arg ->
    (* forcing a lazy expression is like calling a nullary-function *)
    let uf_read = borrow_memory_address `Read in
    let uf_force = Paths.mark_aliased occ Lazy paths in
    let paths = Paths.fresh () in
    let ext, uf_arg = pattern_match_single arg paths in
    ext, UF.pars [uf_read; uf_force; uf_arg]
  | Tpat_tuple args ->
    let uf_read = borrow_memory_address `Read in
    let ext, uf_args =
      List.mapi
        (fun i (_, arg) ->
          let paths = Paths.tuple_field i paths in
          pattern_match_single arg paths)
        args
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_args
  | Tpat_unboxed_tuple args ->
    (* No borrow since unboxed data can not be consumed. *)
    let uf_read = borrow_memory_address `NoRead in
    let ext, uf_args =
      List.mapi
        (fun i (_, arg, _) ->
          let paths = Paths.tuple_field i paths in
          pattern_match_single arg paths)
        args
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_args

let pattern_match pat = function
  | Match_tuple values -> pattern_match_tuple pat values
  | Match_single paths -> pattern_match_single pat paths

(* We ignore exceptions in uniqueness analysis. *)
let comp_pattern_match pat value =
  match split_pattern pat with
  | Some pat', _ -> pattern_match pat' value
  | None, _ -> Ienv.Extension.empty, UF.unused

let value_of_ident ienv unique_use occ path =
  match path with
  | Path.Pident id -> (
    match Ienv.find_opt id ienv with
    (* TODO: for better error message, we should record in ienv why some
       variables are not in it. *)
    | None ->
      force_aliased_boundary ~reason:Out_of_mod_class unique_use occ;
      None
    | Some paths ->
      let value = Value.existing paths unique_use occ in
      Some value)
  (* accessing a module, which is forced by typemod to be aliased and many.
     Here we force it again just to be sure *)
  | Path.Pdot _ ->
    force_aliased_boundary ~reason:Paths_from_mod_class unique_use occ;
    None
  | Path.Papply _ | Path.Pextra_ty _ -> assert false

(* TODO: replace the dirty hack.
   The following functions are dirty hacks and used for modules and classes.
   Currently we treat the boundary between modules/classes and their surrounding
   environment coarsely. To be specific, all references in the modules/classes
   pointing to the environment are treated as many and aliased. This translates
   to enforcement on both ends:
   - inside the module, those uses needs to be forced as many and aliased
   - need a UF.t which marks those uses as many and aliased, so that the
     parent expression can detect conflict if any. *)

(** Returns all open variables inside a module. *)
let open_variables ienv f =
  let ll = ref [] in
  let iter =
    { Tast_iterator.default_iterator with
      expr =
        (fun self e ->
          (match e.exp_desc with
          | Texp_ident (path, _, _, _, unique_use) -> (
            let occ = Occurrence.mk e.exp_loc in
            match value_of_ident ienv unique_use occ path with
            | None -> ()
            | Some value -> ll := value :: !ll)
          | _ -> ());
          Tast_iterator.default_iterator.expr self e)
    }
  in
  f iter;
  !ll

(** Marks all open variables in a class/module as aliased,
   as well as returning a UF reflecting all those aliased usage. *)
let mark_aliased_open_variables ienv f _loc =
  let ll = open_variables ienv f in
  let ufs =
    List.map
      (fun value -> Value.mark_aliased value ~reason:Free_var_of_mod_class)
      ll
  in
  UF.pars ufs

let lift_implicit_borrowing uf =
  UF.map
    (function
      | Maybe_aliased t ->
        (* implicit borrowing lifted. *)
        let occ = Maybe_aliased.extract_occurrence t in
        let access = Maybe_aliased.extract_access t in
        Usage.aliased occ (Aliased.Lifted access)
      | m ->
        (* other usage stays the same *)
        m)
    uf

(* There are two modes our algorithm will work at.

   In the first mode, we care about if the expression can be considered as
   alias, for example, we want `a.x.y` to return the alias of a.x.y in addition
   to the usage of borrowing a and a.x. Note that a.x.y is not included in the
   usage, and the caller is responsible to mark a.x.y if it is used.

   In the second mode, we don't care about if the expression can be considered
   as alias. Checking a.x.y will return the usage of borrowing a and a.x, and
   using a.x.y. This mode is used in most occasions. *)

(** Corresponds to the second mode *)
let rec check_uniqueness_exp (ienv : Ienv.t) exp : UF.t =
  match exp.exp_desc with
  | Texp_ident _ ->
    let value, uf = check_uniqueness_exp_as_value ienv exp in
    UF.seq uf (Value.mark_maybe_unique value)
  | Texp_constant _ -> UF.unused
  | Texp_let (_, vbs, body) ->
    let ext, uf_vbs = check_uniqueness_value_bindings ienv vbs in
    let uf_body = check_uniqueness_exp (Ienv.extend ienv ext) body in
    UF.seq uf_vbs uf_body
  | Texp_function { params; body; _ } ->
    let ienv, uf_params =
      List.fold_left_map
        (fun ienv param ->
          (* [param.fp_param] is only a hint not a binder;
             actual binding done by [param.fp_kind]'s pattern. *)
          let ext, uf_param =
            match param.fp_kind with
            | Tparam_pat pat ->
              let value = Match_single (Paths.fresh ()) in
              pattern_match pat value
            | Tparam_optional_default (pat, default, _) ->
              let value, uf_default =
                check_uniqueness_exp_for_match ienv default
              in
              let ext, uf_pat = pattern_match pat value in
              ext, UF.seq uf_default uf_pat
          in
          Ienv.extend ienv ext, uf_param)
        ienv params
    in
    let uf_body =
      match body with
      | Tfunction_body body -> check_uniqueness_exp ienv body
      | Tfunction_cases { fc_cases; fc_param = _; _ } ->
        (* [param] is only a hint not a binder; actual binding done by the
           [c_lhs] field of each of the [cases]. *)
        let value = Match_single (Paths.fresh ()) in
        check_uniqueness_cases ienv value fc_cases
    in
    let uf = UF.seq (UF.seqs uf_params) uf_body in
    (* we are constructing a closure here, and therefore any implicit
       borrowing of free variables in the closure is in fact using aliased. *)
    lift_implicit_borrowing uf
  | Texp_apply (fn, args, _, _, _) ->
    let uf_fn = check_uniqueness_exp ienv fn in
    let uf_args =
      List.map
        (fun (_, arg) ->
          match arg with
          | Arg (e, _) -> check_uniqueness_exp ienv e
          | Omitted _ -> UF.unused)
        args
    in
    UF.pars (uf_fn :: uf_args)
  | Texp_match (arg, _, cases, _) ->
    let value, uf_arg = check_uniqueness_exp_for_match ienv arg in
    let uf_cases = check_uniqueness_comp_cases ienv value cases in
    UF.seq uf_arg uf_cases
  | Texp_try (body, cases) ->
    let uf_body = check_uniqueness_exp ienv body in
    let value = Match_single (Paths.fresh ()) in
    let uf_cases = check_uniqueness_cases ienv value cases in
    (* we don't know how much of e will be run; safe to assume all of them *)
    UF.seq uf_body uf_cases
  | Texp_tuple (es, _) ->
    UF.pars (List.map (fun (_, e) -> check_uniqueness_exp ienv e) es)
  | Texp_unboxed_tuple es ->
    UF.pars (List.map (fun (_, e, _) -> check_uniqueness_exp ienv e) es)
  | Texp_construct (_, _, es, _) ->
    UF.pars (List.map (fun e -> check_uniqueness_exp ienv e) es)
  | Texp_variant (_, None) -> UF.unused
  | Texp_variant (_, Some (arg, _)) -> check_uniqueness_exp ienv arg
  | Texp_record { fields; extended_expression } ->
    let value, uf_ext =
      match extended_expression with
      | None -> Value.fresh, UF.unused
      | Some (exp, unique_barrier) ->
        let value, uf_exp = check_uniqueness_exp_as_value ienv exp in
        Unique_barrier.enable unique_barrier;
        let uf_read =
          Value.mark_implicit_borrow_memory_address (Read unique_barrier) value
        in
        value, UF.par uf_exp uf_read
    in
    let uf_fields =
      Array.map
        (fun field ->
          match field with
          | l, Kept (_, _, unique_use) ->
            let value =
              Value.implicit_record_field l.lbl_modalities l.lbl_name value
                unique_use
            in
            Value.mark_maybe_unique value
          | _, Overridden (_, e) -> check_uniqueness_exp ienv e)
        fields
    in
    UF.par uf_ext (UF.pars (Array.to_list uf_fields))
  | Texp_field _ ->
    let value, uf = check_uniqueness_exp_as_value ienv exp in
    UF.seq uf (Value.mark_maybe_unique value)
  | Texp_setfield (rcd, _, _, _, arg) ->
    let value, uf_rcd = check_uniqueness_exp_as_value ienv rcd in
    let uf_arg = check_uniqueness_exp ienv arg in
    let uf_write = Value.mark_implicit_borrow_memory_address Write value in
    UF.pars [uf_rcd; uf_arg; uf_write]
  | Texp_array (_, _, es, _) ->
    UF.pars (List.map (fun e -> check_uniqueness_exp ienv e) es)
  | Texp_ifthenelse (if_, then_, else_opt) ->
    (* if' is only borrowed, not used; but probably doesn't matter because of
       mode crossing *)
    let uf_cond = check_uniqueness_exp ienv if_ in
    let uf_then = check_uniqueness_exp ienv then_ in
    let uf_else =
      match else_opt with
      | Some else_ -> check_uniqueness_exp ienv else_
      | None -> UF.unused
    in
    UF.seq uf_cond (UF.choose uf_then uf_else)
  | Texp_sequence (e0, _, e1) ->
    let uf0 = check_uniqueness_exp ienv e0 in
    let uf1 = check_uniqueness_exp ienv e1 in
    UF.seq uf0 uf1
  | Texp_while { wh_cond; wh_body; _ } ->
    let uf_cond = check_uniqueness_exp ienv wh_cond in
    let uf_body = check_uniqueness_exp ienv wh_body in
    UF.seq uf_cond uf_body
  | Texp_list_comprehension { comp_body; comp_clauses } ->
    let uf_body = check_uniqueness_exp ienv comp_body in
    let uf_clauses = check_uniqueness_comprehensions ienv comp_clauses in
    UF.par uf_body uf_clauses
  | Texp_array_comprehension (_, _, { comp_body; comp_clauses }) ->
    let uf_body = check_uniqueness_exp ienv comp_body in
    let uf_clauses = check_uniqueness_comprehensions ienv comp_clauses in
    UF.par uf_body uf_clauses
  | Texp_for { for_from; for_to; for_body; _ } ->
    let uf_from = check_uniqueness_exp ienv for_from in
    let uf_to = check_uniqueness_exp ienv for_to in
    let uf_body = check_uniqueness_exp ienv for_body in
    UF.seq (UF.par uf_from uf_to) uf_body
  | Texp_send (e, _, _) -> check_uniqueness_exp ienv e
  | Texp_new _ -> UF.unused
  | Texp_instvar _ -> UF.unused
  | Texp_setinstvar (_, _, _, e) -> check_uniqueness_exp ienv e
  | Texp_override (_, ls) ->
    UF.pars (List.map (fun (_, _, e) -> check_uniqueness_exp ienv e) ls)
  | Texp_letmodule (_, _, _, mod_expr, body) ->
    let uf_mod =
      mark_aliased_open_variables ienv
        (fun iter -> iter.module_expr iter mod_expr)
        mod_expr.mod_loc
    in
    let uf_body = check_uniqueness_exp ienv body in
    UF.seq uf_mod uf_body
  | Texp_letexception (_, e) -> check_uniqueness_exp ienv e
  | Texp_assert (e, _) -> check_uniqueness_exp ienv e
  | Texp_lazy e ->
    let uf = check_uniqueness_exp ienv e in
    lift_implicit_borrowing uf
  | Texp_object (cls_struc, _) ->
    (* the object (methods, values) will be type-checked by Typeclass,
       which invokes uniqueness check.*)
    mark_aliased_open_variables ienv
      (fun iter -> iter.class_structure iter cls_struc)
      exp.exp_loc
  | Texp_pack mod_expr ->
    (* the module will be type-checked by Typemod which invokes uniqueness
       analysis. *)
    mark_aliased_open_variables ienv
      (fun iter -> iter.module_expr iter mod_expr)
      mod_expr.mod_loc
  | Texp_letop { let_; ands; body } ->
    let uf_let = check_uniqueness_binding_op ienv let_ in
    let uf_ands =
      List.map (fun bop -> check_uniqueness_binding_op ienv bop) ands
    in
    let uf_body =
      check_uniqueness_cases ienv (Match_single (Paths.fresh ())) [body]
    in
    let uf_body = lift_implicit_borrowing uf_body in
    UF.pars (uf_let :: (uf_ands @ [uf_body]))
  | Texp_unreachable -> UF.unused
  | Texp_extension_constructor _ -> UF.unused
  | Texp_open (open_decl, e) ->
    let uf =
      mark_aliased_open_variables ienv
        (fun iter -> iter.open_declaration iter open_decl)
        open_decl.open_loc
    in
    UF.seq uf (check_uniqueness_exp ienv e)
  | Texp_probe { handler } -> check_uniqueness_exp ienv handler
  | Texp_probe_is_enabled _ -> UF.unused
  | Texp_exclave e -> check_uniqueness_exp ienv e
  | Texp_src_pos -> UF.unused

(**
Corresponds to the first mode.

Look at exp and see if it can be treated as an alias. Currently only
[Texp_ident] and [Texp_field] (and recursively so) are treated so. If it returns
[Some Value.t], the caller is responsible to mark it as used as needed *)
and check_uniqueness_exp_as_value ienv exp : Value.t * UF.t =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Texp_ident (p, _, _, _, unique_use) ->
    let occ = Occurrence.mk loc in
    let value =
      match value_of_ident ienv unique_use occ p with
      | None ->
        (* cross module access - don't track *)
        Value.untracked unique_use occ
      | Some value -> value
    in
    value, UF.unused
  | Texp_field (e, _, l, float, unique_barrier) -> (
    let value, uf = check_uniqueness_exp_as_value ienv e in
    match Value.paths value with
    | None ->
      (* No barrier: the expression 'e' is not overwritable. *)
      Unique_barrier.enable unique_barrier;
      Value.fresh, uf
    | Some paths ->
      (* accessing the field meaning borrowing the parent record's mem
         block. Note that the field itself is not borrowed or used *)
      Unique_barrier.enable unique_barrier;
      let uf_read =
        Value.mark_implicit_borrow_memory_address (Read unique_barrier) value
      in
      let uf_boxing, value =
        let occ = Occurrence.mk loc in
        let paths = Paths.record_field l.lbl_modalities l.lbl_name paths in
        match float with
        | Non_boxing unique_use ->
          UF.unused, Value.existing paths unique_use occ
        | Boxing (_, unique_use) ->
          Paths.mark (Usage.maybe_unique unique_use occ) paths, Value.fresh
      in
      value, UF.seqs [uf; uf_read; uf_boxing])
  (* CR-someday anlorenzen: This could also support let-bindings. *)
  | _ -> Value.fresh, check_uniqueness_exp ienv exp

(** take typed expression, do some parsing and returns [value_to_match] *)
and check_uniqueness_exp_for_match ienv exp : value_to_match * UF.t =
  match exp.exp_desc with
  | Texp_tuple (es, _) ->
    let values, ufs =
      List.split
        (List.map (fun (_, e) -> check_uniqueness_exp_as_value ienv e) es)
    in
    Match_tuple values, UF.pars ufs
  | _ ->
    let value, uf = check_uniqueness_exp_as_value ienv exp in
    let paths =
      match Value.paths value with
      | None -> Paths.fresh ()
      | Some paths -> paths
    in
    Match_single paths, uf

(** Returns [ienv] and [uf].
   [ienv] is the new bindings introduced;
   [uf] is the usage forest caused by the binding
*)
and check_uniqueness_value_bindings ienv vbs =
  (* we imitate how data are accessed at runtime *)
  let exts, uf_vbs =
    List.split
      (List.map
         (fun vb ->
           let value, uf_value =
             check_uniqueness_exp_for_match ienv vb.vb_expr
           in
           let ienv, uf_pat = pattern_match vb.vb_pat value in
           ienv, UF.seq uf_value uf_pat)
         vbs)
  in
  Ienv.Extension.conjuncts exts, UF.pars uf_vbs

(* type signature needed because high-ranked *)
and check_uniqueness_cases_gen :
      'a.
      ('a Typedtree.general_pattern -> _ -> _) -> _ -> _ -> 'a case list -> _ =
 fun pat_match ienv value cases ->
  (* In the following we imitate how data are accessed at runtime for cases *)
  (* we first evaluate all LHS including all the guards, in parallel *)
  let exts, uf_pats =
    List.split
      (List.map
         (fun case ->
           let ext, uf_lhs = pat_match case.c_lhs value in
           let uf_guard =
             match case.c_guard with
             | None -> UF.unused
             | Some g -> check_uniqueness_exp (Ienv.extend ienv ext) g
           in
           ext, UF.par uf_lhs uf_guard)
         cases)
  in
  (* we then evaluate all RHS, in _parallel_ *)
  let uf_cases =
    List.map2
      (fun ext case -> check_uniqueness_exp (Ienv.extend ienv ext) case.c_rhs)
      exts cases
  in
  UF.seq (UF.pars uf_pats) (UF.chooses uf_cases)

and check_uniqueness_cases ienv value cases =
  check_uniqueness_cases_gen pattern_match ienv value cases

and check_uniqueness_comp_cases ienv value cases =
  check_uniqueness_cases_gen comp_pattern_match ienv value cases

and check_uniqueness_comprehensions ienv cs =
  UF.pars
    (List.map
       (fun c ->
         match c with
         | Texp_comp_when e -> check_uniqueness_exp ienv e
         | Texp_comp_for cbs ->
           check_uniqueness_comprehension_clause_binding ienv cbs)
       cs)

and check_uniqueness_comprehension_clause_binding ienv cbs =
  UF.pars
    (List.map
       (fun cb ->
         match cb.comp_cb_iterator with
         | Texp_comp_range { start; stop; _ } ->
           let uf_start = check_uniqueness_exp ienv start in
           let uf_stop = check_uniqueness_exp ienv stop in
           UF.par uf_start uf_stop
         | Texp_comp_in { sequence; _ } -> check_uniqueness_exp ienv sequence)
       cbs)

and check_uniqueness_binding_op ienv bo =
  let occ = Occurrence.mk bo.bop_loc in
  let uf_path =
    match value_of_ident ienv aliased_many_use occ bo.bop_op_path with
    | Some value -> Value.mark_maybe_unique value
    | None -> UF.unused
  in
  let uf_exp = check_uniqueness_exp ienv bo.bop_exp in
  UF.par uf_path uf_exp

let check_uniqueness_exp exp =
  let _ = check_uniqueness_exp Ienv.empty exp in
  ()

let check_uniqueness_value_bindings vbs =
  let _ = check_uniqueness_value_bindings Ienv.empty vbs in
  ()

let report_multi_use inner first_is_of_second =
  let { Usage.cannot_force = { occ; axis }; there; first_or_second } = inner in
  let here_usage = "used" in
  let there_usage =
    match there with
    | Usage.Maybe_aliased t ->
      Maybe_aliased.string_of_access (Maybe_aliased.extract_access t)
    | Usage.Aliased t -> (
      match Aliased.reason t with
      | Forced | Lazy -> "used"
      | Lifted access ->
        Maybe_aliased.string_of_access access
        ^ " in a closure that might be called later")
    | _ -> "used"
  in
  let first, first_usage, second, second_usage =
    match first_or_second with
    | Usage.First ->
      occ, here_usage, Option.get (Usage.extract_occurrence there), there_usage
    | Usage.Second ->
      Option.get (Usage.extract_occurrence there), there_usage, occ, here_usage
  in
  let first_is_of_second =
    match first_is_of_second with
    | Self
    | Ancestor [Projection.Memory_address]
    | Descendant [Projection.Memory_address] ->
      "it"
    | Descendant _ -> "part of it"
    | Ancestor _ -> "it is part of a value that"
  in
  (* English is sadly not very composible, we write out all four cases
     manually *)
  let error =
    match first_or_second, axis with
    | First, Uniqueness ->
      Format.dprintf
        "This value is %s here,@ but %s has already been %s as unique:"
        second_usage first_is_of_second first_usage
    | First, Linearity ->
      Format.dprintf
        "This value is %s here,@ but %s is defined as once and has already \
         been %s:"
        second_usage first_is_of_second first_usage
    | Second, Uniqueness ->
      Format.dprintf
        "This value is %s here as unique,@ but %s has already been %s:"
        second_usage first_is_of_second first_usage
    | Second, Linearity ->
      Format.dprintf
        "This value is defined as once and %s here,@ but %s has already been \
         %s:"
        second_usage first_is_of_second first_usage
  in
  let sub = [Location.msg ~loc:first.loc ""] in
  Location.errorf ~loc:second.loc ~sub "@[%t@]" error

let report_boundary cannot_force reason =
  let { Maybe_unique.occ; axis } = cannot_force in
  let reason =
    match reason with
    | Paths_from_mod_class -> "another module or class"
    | Free_var_of_mod_class -> "outside the current module or class"
    | Out_of_mod_class -> "outside the current module or class"
  in
  let error =
    match axis with
    | Uniqueness -> "This value is aliased but used as unique"
    | Linearity -> "This value is once but used as many"
  in
  Location.errorf ~loc:occ.loc "@[%s.\nHint: This value comes from %s.@]" error
    reason

let report_error err =
  Printtyp.wrap_printing_env ~error:true Env.empty (fun () ->
      match err with
      | Usage { inner; first_is_of_second } ->
        report_multi_use inner first_is_of_second
      | Boundary { cannot_force; reason } -> report_boundary cannot_force reason)

let () =
  Location.register_error_of_exn (function
    | Error e -> Some (report_error e)
    | _ -> None)
