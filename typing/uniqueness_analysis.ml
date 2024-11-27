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

(* CR uniqueness: currently printing does not work.
   The debugger just returns <abstr> for all types. *)
module Print_utils = struct
  open Format

  let list elem ppf l =
    fprintf ppf "@[[%a]@]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") elem)
      l

  module Map (M : Map.S) = struct
    let print ~key ~value ppf map =
      let open Format in
      fprintf ppf "@[{:";
      M.iter (fun k v -> fprintf ppf "@[%a :->@ %a@]" key k value v) map;
      fprintf ppf ":}@]"
  end
end

module Occurrence = struct
  (** The occurrence of a potentially unique ident in the expression. Currently
  it's just the location; might add more things in the future *)
  type t = { loc : Location.t }

  let mk loc = { loc }

  let print ppf { loc } = Location.print_loc ppf loc
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

  val print : Format.formatter -> t -> unit
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

  let print ppf t =
    let open Format in
    Print_utils.list
      (fun ppf (uu, occ) ->
        fprintf ppf "@[(%a,@ %a)@]" Typedtree.print_unique_use uu
          Occurrence.print occ)
      ppf t
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

  val print_access : Format.formatter -> access -> unit

  val print : Format.formatter -> t -> unit
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

  let print_access ppf =
    let open Format in
    function
    | Read ub -> fprintf ppf "Read(%a)" Unique_barrier.print ub
    | Write -> fprintf ppf "Write"

  let print ppf t =
    let open Format in
    Print_utils.list
      (fun ppf (occ, access) ->
        fprintf ppf "(%a,%a)" Occurrence.print occ print_access access)
      ppf t
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

  val print : Format.formatter -> t -> unit
end = struct
  type reason =
    | Forced
    | Lazy
    | Lifted of Maybe_aliased.access

  type t = Occurrence.t * reason

  let singleton occ reason = occ, reason

  let extract_occurrence (occ, _) = occ

  let reason (_, reason) = reason

  let print ppf (occ, reason) =
    let open Format in
    let print_reason ppf = function
      | Forced -> fprintf ppf "Forced"
      | Lazy -> fprintf ppf "Lazy"
      | Lifted ma -> fprintf ppf "Lifted(%a)" Maybe_aliased.print_access ma
    in
    fprintf ppf "(%a,%a)" Occurrence.print occ print_reason reason
end

(** For error messages, we keep track of whether an access was sequential or parallel *)
type access_order =
  | Seq
  | Par

(** Usage algebra

    In this file we track the usage of variables and tags throughout a source file.
    Information can be composed using three operators:

     - seq: sequential composition such as 'foo; bar'
     - par: parallel composition such as '(foo, bar)'
         where evaluation order is not specified.
     - choose: non-deterministic choice such as 'if b then foo else bar'

    subject to the following laws:

     - seq, par, choose are associative
     - par, choose are commutative
     - seq and par both distribute over choose
     - choose is idempotent (forall a. a `choose` a = a)
     - seq and par have a common unit 'empty'

    Note: These laws do not apply regarding the concrete error messages reported
    to the user if the analysis fails. However, the laws do determine whether
    the analysis may fail in the first place.

    These operations form a semiring, where choose is '+', seq is '*' and empty
    is '1'. In practice, our semirings also have an ordering that all operations
    above preserve. We write 's1 > s2' if it is sound to return 's2' whenever
    the analysis returns 's1'.

    Missing from a proper semiring is that we do not ask for a 'zero' which would
    the unit of choose and annihilate seq and par. The reason for this is that
    zero is not very useful in practice: it only applies to empty pattern matches
    but not to exceptions or 'assert false'. If 'assert false' would map to zero,
    the analysis would allow segfaulting code before an assertion failure.
    Thus we only need zero for empty pattern matches, but to avoid the hassle of
    defining it, we simply return 'empty' in that case, which is sound in every
    semiring with '0 > 1'.

    CR uniqueness: we might want to have a law relating seq and par.
    See eg. 'concurrent semiring' in Hoare, Möller, Struth, Wehrmann
    "Concurrent Kleene Algebra and its Foundations". *)

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
      first_or_second : first_or_second;
          (** Is it the first or second usage that's failing force? *)
      access_order : access_order
          (** Are the accesses in sequence or parallel? *)
    }

  exception Error of error

  (** Unused *)
  val empty : t

  (** Sequential composition *)
  val seq : t -> t -> t

  (** Non-deterministic choice *)
  val choose : t -> t -> t

  (** Parallel composition *)
  val par : t -> t -> t

  val print : Format.formatter -> t -> unit
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

     We could additionally include a zero for our semiring that sits above unused.
     However, this would have to suppress errors which prevents us from representing
     Error as an exception.
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

  let empty = Unused

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
      first_or_second : first_or_second;
      access_order : access_order
    }

  exception Error of error

  let force_aliased_multiuse t there first_or_second access_order =
    match Maybe_unique.mark_multi_use t with
    | Ok () -> ()
    | Error cannot_force ->
      raise (Error { cannot_force; there; first_or_second; access_order })

  let par m0 m1 =
    match m0, m1 with
    | Unused, m | m, Unused -> m
    | Borrowed occ, Borrowed _ -> Borrowed occ
    | Borrowed _, Maybe_aliased t | Maybe_aliased t, Borrowed _ ->
      Maybe_aliased t
    | Borrowed _, Aliased t | Aliased t, Borrowed _ -> Aliased t
    | Borrowed occ, Maybe_unique t | Maybe_unique t, Borrowed occ ->
      force_aliased_multiuse t (Borrowed occ) First Par;
      aliased (Maybe_unique.extract_occurrence t) Aliased.Forced
    | Maybe_aliased t0, Maybe_aliased t1 ->
      Maybe_aliased (Maybe_aliased.meet t0 t1)
    | Maybe_aliased _, Aliased occ | Aliased occ, Maybe_aliased _ ->
      (* The barrier stays empty; if there is any unique after this,
         the analysis will error *)
      Aliased occ
    | Maybe_aliased t0, Maybe_unique t1 | Maybe_unique t1, Maybe_aliased t0 ->
      (* t1 must be aliased *)
      force_aliased_multiuse t1 (Maybe_aliased t0) First Par;
      (* The barrier stays empty; if there is any unique after this,
         the analysis will error *)
      aliased (Maybe_unique.extract_occurrence t1) Aliased.Forced
    | Aliased t0, Aliased _ -> Aliased t0
    | Aliased t0, Maybe_unique t1 ->
      force_aliased_multiuse t1 (Aliased t0) Second Par;
      Aliased t0
    | Maybe_unique t1, Aliased t0 ->
      force_aliased_multiuse t1 (Aliased t0) First Par;
      Aliased t0
    | Maybe_unique t0, Maybe_unique t1 ->
      force_aliased_multiuse t0 m1 First Par;
      force_aliased_multiuse t1 m0 Second Par;
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
      force_aliased_multiuse l m1 First Seq;
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
      force_aliased_multiuse l0 m1 First Seq;
      aliased occ Aliased.Forced
    | Aliased _, Aliased _ -> m0
    | Maybe_unique l, Aliased _ ->
      force_aliased_multiuse l m1 First Seq;
      m1
    | Aliased _, Maybe_unique l ->
      force_aliased_multiuse l m0 Second Seq;
      m0
    | Maybe_unique l0, Maybe_unique l1 ->
      force_aliased_multiuse l0 m1 First Seq;
      force_aliased_multiuse l1 m0 Second Seq;
      aliased (Maybe_unique.extract_occurrence l0) Aliased.Forced

  let print ppf =
    let open Format in
    function
    | Unused -> fprintf ppf "Unused"
    | Borrowed occ -> fprintf ppf "Borrowed(%a)" Occurrence.print occ
    | Maybe_aliased ma -> fprintf ppf "Maybe_aliased(%a)" Maybe_aliased.print ma
    | Aliased a -> fprintf ppf "Aliased(%a)" Aliased.print a
    | Maybe_unique mu -> fprintf ppf "Maybe_unique(%a)" Maybe_unique.print mu
end

module Tag : sig
  (** This module represents the tags of constructors at runtime.
      When we overwrite a tag, we need to check that the
      tag is equal to the old tag. This is to ensure that
      the tag never changes during overwrites. Changing the
      tag during overwrites is not supported by the multicore GC. *)
  type t =
    { tag : Types.tag;
      name_for_error : Longident.t loc
    }

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val print : Format.formatter -> t -> unit
end = struct
  type t =
    { tag : Types.tag;
      name_for_error : Longident.t loc
    }

  type tags = t

  module Set = Set.Make (struct
    type t = tags

    let compare t1 t2 = Types.compare_tag t1.tag t2.tag
  end)

  module Map = Map.Make (struct
    type t = tags

    let compare t1 t2 = Types.compare_tag t1.tag t2.tag
  end)

  let print ppf { name_for_error; _ } =
    Pprintast.longident ppf name_for_error.txt
end

module Learned_tags : sig
  (** This module collects the tags of allocations which we may learn from
      pattern matches. It is always sound to forget tags we have learned

      [choose] is used to combine information in or-patterns and [par]
      is used to combine information from several pattern matches on
      the same variable. [seq] is not used (but may still be called
      while checking expressions; then the arguments are [empty]).

      Perhaps surprisingly, we allow an allocation to have multiple
      tags. This can only happen when there are two match-statements
      and we enter branches of incompatible tags. Such code can never
      run and so it does not matter which choice we make here. *)

  type t

  (** No known tag: eg. '()' pattern *)
  val empty : t

  (** Sequential composition: This is not called for patterns in general
      and we use the same implementation as for [par]. *)
  val seq : t -> t -> t

  (** Non-deterministic choice: This is called for or-patterns like
      '(TagA _ | TagB _) ->' where we learn the intersection of
      the tags in the pattern. *)
  val choose : t -> t -> t

  (** Parallel composition: If we match on the same memory cell twice
      we learn the union of the patterns. For example, in
      'match (x, x) with (TagA _, TagB _) ->' we learn both tags.
      This is a sound choice since 'x' can not possibly have two distinct
      tags and also we are now aliasing 'x' which makes it impossible to
      overwrite. Note that 'x' can have two distinct tags in the presence
      of mutation but we track that in the Overwrites module below. *)
  val par : t -> t -> t

  (** Register a tag we know (eg. from a pattern-match) *)
  val learn_tag : Tag.t -> t

  (** Extract the tags that this cell may have *)
  val extract_tags : t -> Tag.Set.t

  (** Assert that no tags were learned. *)
  val assert_empty : t -> unit

  val print : Format.formatter -> t -> unit
end = struct
  type t = Tag.Set.t

  (* If we were to define a zero for this semiring, it would be
     the set of all possible tags. *)

  let empty = Tag.Set.empty

  let choose t0 t1 = Tag.Set.inter t0 t1

  let par t0 t1 = Tag.Set.union t0 t1

  let seq t0 t1 = par t0 t1

  let learn_tag tag = Tag.Set.singleton tag

  let extract_tags t = t

  let assert_empty t = assert (Tag.Set.is_empty t)

  let print ppf t =
    let open Format in
    fprintf ppf "%a" (Format.pp_print_list Tag.print) (Tag.Set.elements t)
end

module Overwrites : sig
  (** This module collects the tags of overwrites. It is always sound
      to assume we have overwritten with additional tags.

      However, it is in general unsound if the user mutates the tag
      after we have learned its nature. Then all bets are off and we
      always fail if there is an overwrite that follows a mutation. *)

  type t

  type old_tag =
    | Old_tag_unknown
    | Old_tag_was of Tag.t
    | Old_tag_mutated of access_order

  type error =
    | Changed_tag of
        { old_tag : old_tag;
          new_tag : Tag.t
        }

  exception Error of error

  (** No overwrites as in eg. a constant expression '()'. *)
  val empty : t

  (** Overwrite using a certain tag *)
  val overwrite_tag : Tag.t -> t

  (** Indicate a mutation (which invalidates all tags we have learned
      and will learn). If there is also an overwrite, we fail. *)
  val mutate_tag : t

  (** Sequential composition: Union the overwrites that were collected
      in either argument. Error if there was a mutation in one argument
      and an overwrite in the second. Eg.
      'x <- TagA; overwrite_ x with TagB' fails
      'overwrite_ x with TagA; x <- TagB' fails
      'overwrite_ x with TagA; overwrite_ x with TagB' succeeds
      *)
  val seq : t -> t -> t

  (** Non-deterministic choice: Union the overwrites that were collected
      in either argument. Record if there was a mutation in either
      argument but do not error since the branches are independent. Eg.
      'if b then overwrite_ x with TagA else overwrite_ x with TagB' succeeds
      'if b then x <- TagA else overwrite_ x with TagB' succeeds and records
        that 'x' was mutated in a branch.
      *)
  val choose : t -> t -> t

  (** Parallel composition: Same as [seq]. *)
  val par : t -> t -> t

  (** If we find out that a tag was mutated,
      we need to promote this information to its children.
      This is because a write to a mutable field can change
      any tag that is reachable from the mutable field. *)
  val promote_mutation_to_children : t -> t

  (** We may not overwrite a tag we do not know.
      At the end of the analysis, this function should
      be called to ensure all overwrites are on known tags. *)
  val check_no_remaining_overwritten_as : t -> unit

  (** Accept the overwrites using tags that we have learned
      from a pattern-match. *)
  val match_with_learned_tags : Tag.Set.t -> t -> t

  (** Assert that no overwrites were collected. *)
  val assert_empty : t -> unit

  val print : Format.formatter -> t -> unit
end = struct
  type tags =
    { overwritten : Tag.Set.t Tag.Map.t;
          (** The keys of the map are the tags of the overwrite.
              When we learn new tags, we can remove keys from this map,
              that correspond to the new tags. We then keep the learned
              tags on the keys that did not match them to produce good
              error messages. It is always sound to add elements to this map.
          *)
      was_mutated : bool
          (** If a mutation occurs in a branch with no overwriting, we set this flag.
              It is acceptable for overwrites to occur in a different branch or
              earlier in the control flow, but it is not okay for an overwrite to
              happen later in the control flow.
          *)
    }

  type t =
    | Tags of tags
    | Tag_was_mutated
        (** We have discovered a mutable write.
        This is dangerous: We might have accepted a overwrite based on the wrong tag.
        If we detect this state, we reject all overwrites to this cell. *)

  type old_tag =
    | Old_tag_unknown
    | Old_tag_was of Tag.t
    | Old_tag_mutated of access_order

  type error =
    | Changed_tag of
        { old_tag : old_tag;
          new_tag : Tag.t
        }

  exception Error of error

  let empty = Tags { overwritten = Tag.Map.empty; was_mutated = false }

  let overwrite_tag tag =
    Tags
      { overwritten = Tag.Map.singleton tag Tag.Set.empty; was_mutated = false }

  let mutate_tag = Tag_was_mutated

  (* We union the overwrites and either intersect or union the learned tags.
     The distinction between these two functions only affects error messages.
  *)
  let union_with_inter_learned =
    Tag.Map.union (fun _ t0 t1 -> Some (Tag.Set.inter t0 t1))

  let union_with_union_learned =
    Tag.Map.union (fun _ t0 t1 -> Some (Tag.Set.union t0 t1))

  let choose t0 t1 =
    match t0, t1 with
    | Tags t0, Tags t1 ->
      Tags
        { overwritten = union_with_inter_learned t0.overwritten t1.overwritten;
          was_mutated = t0.was_mutated || t1.was_mutated
        }
    | Tags { overwritten }, Tag_was_mutated
    | Tag_was_mutated, Tags { overwritten } ->
      Tags { overwritten; was_mutated = true }
    | Tag_was_mutated, Tag_was_mutated -> Tag_was_mutated

  let seq_or_par access_order t0 t1 =
    match t0, t1 with
    | Tag_was_mutated, Tag_was_mutated -> Tag_was_mutated
    | Tags t0, Tags t1 when (not t0.was_mutated) && not t1.was_mutated ->
      Tags
        { overwritten = union_with_union_learned t0.overwritten t1.overwritten;
          was_mutated = false
        }
    | Tags { overwritten }, _ | _, Tags { overwritten } -> (
      match Tag.Map.choose_opt overwritten with
      | None -> Tag_was_mutated
      | Some (t, _) ->
        raise
          (Error
             (Changed_tag
                { old_tag = Old_tag_mutated access_order; new_tag = t })))

  let par t0 t1 = seq_or_par Par t0 t1

  let seq t0 t1 = seq_or_par Seq t0 t1

  (* This is effectively the set difference [overwrite - learned].
     However, we also store the newly learned tags on the overwrites
     that are not eliminated to give better error messages. *)
  let match_with_learned_tags newly_learned t =
    match t with
    | Tag_was_mutated -> Tag_was_mutated
    | Tags { overwritten; was_mutated } ->
      Tags
        { overwritten =
            Tag.Map.filter_map
              (fun tag learned_before ->
                if Tag.Set.mem tag newly_learned
                then None
                else Some (Tag.Set.union learned_before newly_learned))
              overwritten;
          was_mutated
        }

  let promote_mutation_to_children t =
    match t with
    | Tag_was_mutated -> Tag_was_mutated
    | Tags { was_mutated } -> if was_mutated then Tag_was_mutated else empty

  let check_no_remaining_overwritten_as t =
    match t with
    | Tags { overwritten } -> (
      match Tag.Map.choose_opt overwritten with
      | None -> ()
      | Some (tag_overwritten, have_learned) ->
        raise
          (Error
             (match Tag.Set.choose_opt have_learned with
             | None ->
               Changed_tag
                 { old_tag = Old_tag_unknown; new_tag = tag_overwritten }
             | Some tag_learned ->
               Changed_tag
                 { old_tag = Old_tag_was tag_learned;
                   new_tag = tag_overwritten
                 })))
    | Tag_was_mutated -> ()

  let assert_empty t =
    match t with
    | Tags { overwritten; was_mutated } ->
      assert (not was_mutated);
      assert (Tag.Map.is_empty overwritten)
    | _ -> assert false

  let print ppf =
    let open Format in
    function
    | Tags tag ->
      fprintf ppf "Tags { overwritten = %a; was_mutated = %a }"
        (Format.pp_print_list Tag.print)
        (List.map fst (Tag.Map.bindings tag.overwritten))
        Format.pp_print_bool tag.was_mutated
    | Tag_was_mutated -> fprintf ppf "Tag_was_mutated"
end

module Projection : sig
  (** Projections from parent to child. *)
  type t =
    | Tuple_field of int
    | Record_field of string
    | Construct_field of string * int
    | Variant_field of label
    | Array_index of int
    | Memory_address (* this is rendered as clubsuit in the ICFP'24 paper *)

  module Map : Map.S with type key = t

  val print : Format.formatter -> t -> unit

  val print_map :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Map.t -> unit
end = struct
  module T = struct
    type t =
      | Tuple_field of int
      | Record_field of string
      | Construct_field of string * int
      | Variant_field of label
      | Array_index of int
      | Memory_address

    let compare t1 t2 =
      match t1, t2 with
      | Tuple_field i, Tuple_field j -> Int.compare i j
      | Record_field l1, Record_field l2 -> String.compare l1 l2
      | Construct_field (l1, i), Construct_field (l2, j) -> (
        match String.compare l1 l2 with 0 -> Int.compare i j | i -> i)
      | Variant_field l1, Variant_field l2 -> String.compare l1 l2
      | Array_index i, Array_index j -> Int.compare i j
      | Memory_address, Memory_address -> 0
      | ( Tuple_field _,
          ( Record_field _ | Construct_field _ | Variant_field _ | Array_index _
          | Memory_address ) ) ->
        -1
      | ( ( Record_field _ | Construct_field _ | Variant_field _ | Array_index _
          | Memory_address ),
          Tuple_field _ ) ->
        1
      | ( Record_field _,
          (Construct_field _ | Variant_field _ | Array_index _ | Memory_address)
        ) ->
        -1
      | ( (Construct_field _ | Variant_field _ | Array_index _ | Memory_address),
          Record_field _ ) ->
        1
      | Construct_field _, (Variant_field _ | Array_index _ | Memory_address) ->
        -1
      | (Variant_field _ | Array_index _ | Memory_address), Construct_field _ ->
        1
      | Variant_field _, (Array_index _ | Memory_address) -> -1
      | (Array_index _ | Memory_address), Variant_field _ -> 1
      | Array_index _, Memory_address -> -1
      | Memory_address, Array_index _ -> 1
  end

  include T
  module Map = Map.Make (T)

  let print ppf =
    let open Format in
    function
    | Tuple_field n -> fprintf ppf "Tuple_field(%d)" n
    | Record_field l -> fprintf ppf "Record_field(%s)" l
    | Construct_field (s, n) -> fprintf ppf "Construct_field(%s,%d)" s n
    | Variant_field l -> fprintf ppf "Variant_field(%s)" l
    | Array_index n -> fprintf ppf "Array_index(%d)" n
    | Memory_address -> fprintf ppf "Memory_address"

  let print_map print_value ppf map =
    let module M = Print_utils.Map (Map) in
    M.print ~key:print ~value:print_value ppf map
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
  | Overwrite_changed_tag of Overwrites.error

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

    val print : Format.formatter -> t -> unit
  end

  (** Usage tree, lifted from [Usage.t] *)
  type t

  (** Sequential composition lifted from [Usage.seq] *)
  val seq : t -> t -> t

  (** Non-deterministic choice lifted from [Usage.choose] *)
  val choose : t -> t -> t

  (** Parallel composition lifted from [Usage.par]  *)
  val par : t -> t -> t

  (** An empty tree containing only the root with empty usage *)
  val empty : t

  (** A singleton tree containing only one leaf. In patterns, the
      'Overwrites.t' should be empty and in expressions the 'Learned_tags.t'
      should be empty. *)
  val singleton : Usage.t -> Learned_tags.t -> Overwrites.t -> Path.t -> t

  (** Runs a function through the tree; the function must be monotone *)
  val mapi :
    (Path.t -> Usage.t -> Usage.t) ->
    (Learned_tags.t -> Learned_tags.t) ->
    (Overwrites.t -> Overwrites.t) ->
    t ->
    t

  (** Check that all overwrites are on known tags *)
  val check_no_remaining_overwritten_as : t -> unit

  (** Split into usage and overwrites on the one side
      and learned tags on the other. It is safe to throw
      away the second component of the pair. *)
  val split_usage_tags : t -> t * t

  (** Use the learned_tags from the first argument to match with
      the overwrites of the second argument.
      Danger: this throws away the usage in its first argument and should
      only be called on the learned tags from [split_usage_tags]. *)
  val match_with_learned_tags : t -> t -> t

  val print : Format.formatter -> t -> unit
end = struct
  (** Represents a tree of usage. Each node records the choose on all possible
     execution paths. As a result, trees such as `S -> U` is valid, even though
     it would be invalid if it was the result of a single path: using a parent
     aliased and a child uniquely is obviously bad. However, it might be the
     result of "choos"ing multiple path: choose `S` `N -> U`, which is valid.

     INVARIANT: children >= parent. For example, having an aliased child under a
     unique parent is nonsense. The invariant is preserved because Usage.choose,
     Usage.par, and Usage.seq above are monotone, and Usage_tree.par and
     Usage_tree.seq, Usage_tree.choose here are node-wise.

     INVARIANT: Either the [learned] or the [overwrites] is [empty]. When
     checking patterns the latter is empty while for expression the former is
     empty. *)
  type t =
    { children : t Projection.Map.t;
      usage : Usage.t;
      learned : Learned_tags.t;
      overwrites : Overwrites.t
    }

  module Path = struct
    type t = Projection.t list

    let child (a : Projection.t) (p : t) : t = p @ [a]

    let root : t = []

    let print ppf t = Print_utils.list Projection.print ppf t
  end

  let mapi_aux projs fu fl fo t =
    let rec loop projs t =
      let usage = fu projs t.usage in
      let children =
        Projection.Map.mapi (fun proj t -> loop (proj :: projs) t) t.children
      in
      let learned = fl t.learned in
      let overwrites = fo t.overwrites in
      { usage; children; learned; overwrites }
    in
    loop projs t

  let mapi f t = mapi_aux [] f t

  let rec mapi2 fu fl fo t0 t1 =
    let usage = fu Self t0.usage t1.usage in
    let children =
      Projection.Map.merge
        (fun proj c0 c1 ->
          match c0, c1 with
          | None, None -> assert false
          | None, Some c1 ->
            Some
              (mapi_aux [proj]
                 (fun projs r -> fu (Ancestor projs) t0.usage r)
                 (fun r -> fl r Learned_tags.empty)
                 (fun r ->
                   fo (Overwrites.promote_mutation_to_children t0.overwrites) r)
                 c1)
          | Some c0, None ->
            Some
              (mapi_aux [proj]
                 (fun projs l -> fu (Descendant projs) l t1.usage)
                 (fun l -> fl l Learned_tags.empty)
                 (fun l ->
                   fo l (Overwrites.promote_mutation_to_children t1.overwrites))
                 c0)
          | Some c0, Some c1 -> Some (mapi2 fu fl fo c0 c1))
        t0.children t1.children
    in
    let learned = fl t0.learned t1.learned in
    let overwrites = fo t0.overwrites t1.overwrites in
    { usage; children; learned; overwrites }

  let lift fu fl fo t0 t1 =
    mapi2
      (fun first_is_of_second t0 t1 ->
        try fu t0 t1
        with Usage.Error error ->
          raise (Error (Usage { inner = error; first_is_of_second })))
      fl
      (fun t0 t1 ->
        try fo t0 t1
        with Overwrites.Error error ->
          raise (Error (Overwrite_changed_tag error)))
      t0 t1

  let choose t0 t1 =
    lift Usage.choose Learned_tags.choose Overwrites.choose t0 t1

  let seq t0 t1 = lift Usage.seq Learned_tags.seq Overwrites.seq t0 t1

  let par t0 t1 = lift Usage.par Learned_tags.par Overwrites.par t0 t1

  let empty =
    { children = Projection.Map.empty;
      usage = Usage.empty;
      learned = Learned_tags.empty;
      overwrites = Overwrites.empty
    }

  let rec singleton leaf learned overwrites = function
    | [] ->
      { usage = leaf; children = Projection.Map.empty; learned; overwrites }
    | proj :: path ->
      { usage = Usage.empty;
        children =
          Projection.Map.singleton proj (singleton leaf learned overwrites path);
        learned = Learned_tags.empty;
        overwrites = Overwrites.empty
      }

  let rec check_no_remaining_overwritten_as { children; overwrites } =
    Projection.Map.iter
      (fun _ t -> check_no_remaining_overwritten_as t)
      children;
    try Overwrites.check_no_remaining_overwritten_as overwrites
    with Overwrites.Error error -> raise (Error (Overwrite_changed_tag error))

  let rec split_usage_tags { children; usage; overwrites; learned } =
    let split_children = Projection.Map.map split_usage_tags children in
    let children_usages, children_tags =
      ( Projection.Map.map fst split_children,
        Projection.Map.map snd split_children )
    in
    Overwrites.assert_empty overwrites;
    ( { children = children_usages;
        usage;
        overwrites = Overwrites.empty;
        learned = Learned_tags.empty
      },
      { children = children_tags;
        usage = Usage.empty;
        overwrites = Overwrites.empty;
        learned
      } )

  let rec match_with_learned_tags learned t =
    let children =
      Projection.Map.merge
        (fun _ c0 c1 ->
          match c0, c1 with
          | None, None -> assert false
          | None, Some c1 -> Some c1
          | Some _, None -> None
          | Some c0, Some c1 -> Some (match_with_learned_tags c0 c1))
        learned.children t.children
    in
    Learned_tags.assert_empty t.learned;
    { usage = t.usage;
      children;
      learned = Learned_tags.empty;
      overwrites =
        Overwrites.match_with_learned_tags
          (Learned_tags.extract_tags learned.learned)
          t.overwrites
    }

  let rec print ppf { children; usage; learned; overwrites } =
    let open Format in
    fprintf ppf
      "@[{ children = %a;@ usage = %a;@ learned = %a;@ overwrites = %a }@]"
      (Projection.print_map print)
      children Usage.print usage Learned_tags.print learned Overwrites.print
      overwrites
end

(** Lift Usage_tree to forest *)
module Usage_forest : sig
  module Path : sig
    type t

    (** Construct a child path from a parent  *)
    val child : Projection.t -> t -> t

    (** Create a fresh tree in the forest *)
    val fresh_root : unit -> t

    val print : Format.formatter -> t -> unit
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
  val singleton : Usage.t -> Learned_tags.t -> Overwrites.t -> Path.t -> t

  (** Run a function through a forest. The function must be monotone *)
  val map :
    (Usage.t -> Usage.t) ->
    (Learned_tags.t -> Learned_tags.t) ->
    (Overwrites.t -> Overwrites.t) ->
    t ->
    t

  (** Check that all overwrites are on known tags *)
  val check_no_remaining_overwritten_as : t -> unit

  (** Split into usage and overwrites on the one side
      and learned tags on the other. It is safe to throw
      away the second component of the pair. *)
  val split_usage_tags : t -> t * t

  (** Use the learned_tags from the first argument to match with
      the overwrites of the second argument.
      Danger: this throws away the usage in its first argument and should
      only be called on the learned tags from [split_usage_tags]. *)
  val match_with_learned_tags : t -> t -> t

  val print : Format.formatter -> t -> unit
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

    let print ppf { id } = Format.fprintf ppf "{%d}" id
  end

  type t = Usage_tree.t Root_id.Map.t

  module Path = struct
    type t = Root_id.t * Usage_tree.Path.t

    let child proj ((rootid, path) : t) : t =
      rootid, Usage_tree.Path.child proj path

    let fresh_root () : t = Root_id.fresh (), Usage_tree.Path.root

    let print ppf (root_id, path) =
      let open Format in
      fprintf ppf "@[(%a,@ %a)@]" Root_id.print root_id Usage_tree.Path.print
        path
  end

  let unused = Root_id.Map.empty

  (** [f] must be monotone  *)
  let map2 f t0 t1 =
    Root_id.Map.merge
      (fun _rootid t0 t1 ->
        match t0, t1 with
        | None, None -> assert false
        | None, Some t1 -> Some (f Usage_tree.empty t1)
        | Some t0, None -> Some (f t0 Usage_tree.empty)
        | Some t0, Some t1 -> Some (f t0 t1))
      t0 t1

  let choose t0 t1 = map2 Usage_tree.choose t0 t1

  let seq t0 t1 = map2 Usage_tree.seq t0 t1

  let par t0 t1 = map2 Usage_tree.par t0 t1

  (* For convenience, our semirings do not have a zero.
     However, when we fold the [choose] operation we need to initialize the
     accumulator with zero. We avoid doing this here by skipping the initial
     accumulator altogether. When [chooses] gets passed an empty list,
     we return the '1' of the semiring which is sound in all semirings
     that have '0 > 1'. See the discussion about semirings above. *)
  let fold_left1 f = function [] -> unused | x :: l -> List.fold_left f x l

  let chooses l = fold_left1 choose l

  let seqs l = fold_left1 seq l

  let pars l = fold_left1 par l

  let singleton leaf learned overwrites ((rootid, path') : Path.t) =
    Root_id.Map.singleton rootid
      (Usage_tree.singleton leaf learned overwrites path')

  (** 'fu fl fo' all must be monotone *)
  let map fu fl fo =
    Root_id.Map.mapi (fun _root tree ->
        Usage_tree.mapi (fun _projs usage -> fu usage) fl fo tree)

  let check_no_remaining_overwritten_as t =
    Root_id.Map.iter
      (fun _ t -> Usage_tree.check_no_remaining_overwritten_as t)
      t

  let split_usage_tags t =
    ( Root_id.Map.map (fun x -> fst (Usage_tree.split_usage_tags x)) t,
      Root_id.Map.map (fun x -> snd (Usage_tree.split_usage_tags x)) t )

  let match_with_learned_tags learned t =
    Root_id.Map.merge
      (fun _rootid t0 t1 ->
        match t0, t1 with
        | None, None -> assert false
        | None, Some t1 -> Some t1
        | Some _, None -> None
        | Some t0, Some t1 -> Some (Usage_tree.match_with_learned_tags t0 t1))
      learned t

  let print ppf t =
    let open Format in
    let module M = Print_utils.Map (Root_id.Map) in
    M.print
      ~key:(fun ppf { id } -> fprintf ppf "%d" id)
      ~value:Usage_tree.print ppf t
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

  (** [array_index mut i t] is [modal_child gf (Projection.Array_index i) t]
      where [gf] is the appropriate modality for mutability [mut]. *)
  val array_index : Types.mutability -> int -> t -> t

  (** [memory_address t] is [child Projection.Memory_address t]. *)
  val memory_address : t -> t

  val mark : Usage.t -> Learned_tags.t -> Overwrites.t -> t -> UF.t

  val fresh : unit -> t

  val choose : t -> t -> t

  val mark_implicit_borrow_memory_address :
    Occurrence.t -> Maybe_aliased.access -> t -> UF.t

  val mark_aliased : Occurrence.t -> Aliased.reason -> t -> UF.t

  val invalidate_tag : t -> UF.t

  val overwrite_tag : Tag.t -> t -> UF.t

  val learn_tag : Tag.t -> t -> UF.t

  val print : Format.formatter -> t -> unit
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

  let array_index mut i t =
    let modality = Typemode.transl_modalities ~maturity:Stable mut [] [] in
    modal_child modality (Projection.Array_index i) t

  let memory_address t = child Projection.Memory_address t

  let mark usage learned overwrites t =
    UF.chooses (List.map (UF.singleton usage learned overwrites) t)

  let fresh () = [UF.Path.fresh_root ()]

  let mark_implicit_borrow_memory_address occ access paths =
    mark
      (Maybe_aliased (Maybe_aliased.singleton occ access))
      Learned_tags.empty Overwrites.empty (memory_address paths)

  let mark_aliased occ reason paths =
    mark (Usage.aliased occ reason) Learned_tags.empty Overwrites.empty paths

  let invalidate_tag paths =
    mark Usage.empty Learned_tags.empty Overwrites.mutate_tag paths

  let overwrite_tag tag paths =
    mark Usage.empty Learned_tags.empty (Overwrites.overwrite_tag tag) paths

  let learn_tag tag paths =
    mark Usage.empty (Learned_tags.learn_tag tag) Overwrites.empty paths

  let print ppf t = Print_utils.list UF.Path.print ppf t
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

  (** Mark the value's memory address as aliased_or_unique   *)
  val mark_consumed_memory_address : t -> UF.t

  (** Mark the memory_address of the value as implicitly borrowed
      (borrow_or_aliased). *)
  val mark_implicit_borrow_memory_address : Maybe_aliased.access -> t -> UF.t

  val mark_aliased : reason:boundary_reason -> t -> UF.t

  val invalidate_tag : t -> UF.t

  val overwrite_tag : Tag.t -> t -> UF.t

  val print : Format.formatter -> t -> unit
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
      Paths.mark
        (Usage.maybe_unique unique_use occ)
        Learned_tags.empty Overwrites.empty paths

  let mark_consumed_memory_address = function
    | Fresh -> UF.unused
    | Existing { paths; unique_use; occ } ->
      Paths.mark
        (Usage.maybe_unique unique_use occ)
        Learned_tags.empty Overwrites.empty
        (Paths.memory_address paths)

  let mark_aliased ~reason = function
    | Fresh -> UF.unused
    | Existing { paths; unique_use; occ } ->
      force_aliased_boundary unique_use occ ~reason;
      let aliased = Usage.aliased occ Aliased.Forced in
      Paths.mark aliased Learned_tags.empty Overwrites.empty paths

  let invalidate_tag = function
    | Fresh -> UF.unused
    | Existing { paths; _ } -> Paths.invalidate_tag paths

  let overwrite_tag tag = function
    | Fresh -> UF.unused
    | Existing { paths; _ } -> Paths.overwrite_tag tag paths

  let print ppf =
    let open Format in
    function
    | Fresh -> fprintf ppf "Fresh"
    | Existing { paths; unique_use; occ } ->
      fprintf ppf "@[{ paths = %a;@ unique_use = %a;@ occ = %a }@]" Paths.print
        paths Typedtree.print_unique_use unique_use Occurrence.print occ
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

    val print : Format.formatter -> t -> unit
  end

  (** Mapping from identifiers to a list of possible nodes, each represented by
      a path into the forest, instead of directly pointing to the node. *)
  type t

  (** Extend a mapping with an extension *)
  val extend : t -> Extension.t -> t

  (** The empty mapping  *)
  val empty : t

  (** Find the list of paths corresponding to an identifier  *)
  val find_opt : Ident.t -> t -> Paths.t option

  val print : Format.formatter -> t -> unit
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

    let print ppf t =
      let module M = Print_utils.Map (Ident.Map) in
      M.print ~key:Ident.print ~value:Paths.print ppf t
  end

  type t = Paths.t Ident.Map.t

  let empty = Ident.Map.empty

  let extend t ex =
    Ident.Map.union
      (* the extension shadows the original *)
        (fun _id _paths0 paths1 -> Some paths1)
      t ex

  let find_opt = Ident.Map.find_opt

  let print ppf t =
    let module M = Print_utils.Map (Ident.Map) in
    M.print ~key:Ident.print ~value:Paths.print ppf t
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
  let no_borrow_memory_address () =
    Unique_barrier.enable pat.pat_unique_barrier;
    ignore (Unique_barrier.resolve pat.pat_unique_barrier)
  in
  let borrow_memory_address () =
    Unique_barrier.enable pat.pat_unique_barrier;
    Paths.mark_implicit_borrow_memory_address occ (Read pat.pat_unique_barrier)
      paths
  in
  match pat.pat_desc with
  | Tpat_or (pat0, pat1, _) ->
    no_borrow_memory_address ();
    let ext0, uf0 = pattern_match_single pat0 paths in
    let ext1, uf1 = pattern_match_single pat1 paths in
    Ienv.Extension.disjunct ext0 ext1, UF.choose uf0 uf1
  | Tpat_any ->
    no_borrow_memory_address ();
    Ienv.Extension.empty, UF.unused
  | Tpat_var (id, _, _, _) ->
    no_borrow_memory_address ();
    Ienv.Extension.singleton id paths, UF.unused
  | Tpat_alias (pat', id, _, _, _) ->
    no_borrow_memory_address ();
    let ext0 = Ienv.Extension.singleton id paths in
    let ext1, uf = pattern_match_single pat' paths in
    Ienv.Extension.conjunct ext0 ext1, uf
  | Tpat_constant _ ->
    let uf_read = borrow_memory_address () in
    Ienv.Extension.empty, uf_read
  | Tpat_construct (lbl, cd, pats, _) ->
    let uf_tag =
      Paths.learn_tag { tag = cd.cstr_tag; name_for_error = lbl } paths
    in
    let uf_read = borrow_memory_address () in
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
    ext, UF.pars [uf_tag; uf_read; uf_pats]
  | Tpat_variant (lbl, arg, _) ->
    let uf_read = borrow_memory_address () in
    let ext, uf_arg =
      match arg with
      | Some arg ->
        let paths = Paths.variant_field lbl paths in
        pattern_match_single arg paths
      | None -> Ienv.Extension.empty, UF.unused
    in
    ext, UF.pars [uf_read; uf_arg]
  | Tpat_record (pats, _) ->
    let uf_read = borrow_memory_address () in
    let ext, uf_pats =
      List.map
        (fun (_, l, pat) ->
          let paths = Paths.record_field l.lbl_modalities l.lbl_name paths in
          pattern_match_single pat paths)
        pats
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_pats
  | Tpat_array (mut, _, pats) ->
    let uf_read = borrow_memory_address () in
    let ext, uf_pats =
      List.mapi
        (fun idx pat ->
          let paths = Paths.array_index mut idx paths in
          pattern_match_single pat paths)
        pats
      |> conjuncts_pattern_match
    in
    ext, UF.par uf_read uf_pats
  | Tpat_lazy arg ->
    no_borrow_memory_address ();
    (* forced below: *)
    (* forcing a lazy expression is like calling a nullary-function *)
    let uf_force = Paths.mark_aliased occ Lazy paths in
    let paths = Paths.fresh () in
    let ext, uf_arg = pattern_match_single arg paths in
    ext, UF.par uf_force uf_arg
  | Tpat_tuple args ->
    let uf_read = borrow_memory_address () in
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
    no_borrow_memory_address ();
    let ext, uf_args =
      List.mapi
        (fun i (_, arg, _) ->
          let paths = Paths.tuple_field i paths in
          pattern_match_single arg paths)
        args
      |> conjuncts_pattern_match
    in
    ext, uf_args

let pattern_match pat = function
  | Match_tuple values -> pattern_match_tuple pat values
  | Match_single paths -> pattern_match_single pat paths

let comp_pattern_match pat value =
  let vals, exns = split_pattern pat in
  (* We ignore exceptions in uniqueness analysis,
     since they can never contain unique values. *)
  (match exns with
  | Some exns ->
    let _ = pattern_match exns (Match_single Paths.untracked) in
    ()
  | None -> ());
  match vals with
  | Some pat' -> pattern_match pat' value
  | None -> Ienv.Extension.empty, UF.unused

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
    (fun t -> t)
    (fun t -> t)
    uf

let descend proj overwrite =
  match overwrite with
  | None -> None
  | Some paths -> Some (Paths.child proj paths)

(* There are two modes our algorithm will work at.

   In the first mode, we care about if the expression can be considered as
   alias, for example, we want `a.x.y` to return the alias of a.x.y in addition
   to the usage of borrowing a and a.x. Note that a.x.y is not included in the
   usage, and the caller is responsible to mark a.x.y if it is used.

   In the second mode, we don't care about if the expression can be considered
   as alias. Checking a.x.y will return the usage of borrowing a and a.x, and
   using a.x.y. This mode is used in most occasions. *)

(** Corresponds to the second mode *)
let rec check_uniqueness_exp ~overwrite (ienv : Ienv.t) exp : UF.t =
  match exp.exp_desc with
  | Texp_ident _ ->
    let value, uf = check_uniqueness_exp_as_value ienv exp in
    UF.seq uf (Value.mark_maybe_unique value)
  | Texp_constant _ -> UF.unused
  | Texp_let (_, vbs, body) ->
    let ext, uf_vbs = check_uniqueness_value_bindings ienv vbs in
    let uf_body =
      check_uniqueness_exp ~overwrite:None (Ienv.extend ienv ext) body
    in
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
              let ext, uf_pat = pattern_match pat value in
              ext, (UF.unused, uf_pat)
            | Tparam_optional_default (pat, default, _) ->
              let value, uf_default =
                check_uniqueness_exp_for_match ienv default
              in
              let ext, uf_pat = pattern_match pat value in
              ext, (uf_default, uf_pat)
          in
          Ienv.extend ienv ext, uf_param)
        ienv params
    in
    let uf_body =
      match body with
      | Tfunction_body body -> check_uniqueness_exp ~overwrite:None ienv body
      | Tfunction_cases { fc_cases; fc_param = _; _ } ->
        (* [param] is only a hint not a binder; actual binding done by the
           [c_lhs] field of each of the [cases]. *)
        let value = Match_single (Paths.fresh ()) in
        check_uniqueness_cases ienv value fc_cases
    in
    let uf =
      List.fold_right
        (fun (uf_default, uf_pat) uf_body ->
          let uf_pat, tags = UF.split_usage_tags uf_pat in
          UF.seqs [uf_default; uf_pat; UF.match_with_learned_tags tags uf_body])
        uf_params uf_body
    in
    (* we are constructing a closure here, and therefore any implicit
       borrowing of free variables in the closure is in fact using aliased. *)
    lift_implicit_borrowing uf
  | Texp_apply (fn, args, _, _, _) ->
    let uf_fn = check_uniqueness_exp ~overwrite:None ienv fn in
    let uf_args =
      List.map
        (fun (_, arg) ->
          match arg with
          | Arg (e, _) -> check_uniqueness_exp ~overwrite:None ienv e
          | Omitted _ -> UF.unused)
        args
    in
    UF.pars (uf_fn :: uf_args)
  | Texp_match (arg, _, cases, _) ->
    let value, uf_arg = check_uniqueness_exp_for_match ienv arg in
    let uf_cases = check_uniqueness_comp_cases ienv value cases in
    UF.seq uf_arg uf_cases
  | Texp_try (body, cases) ->
    let uf_body = check_uniqueness_exp ~overwrite:None ienv body in
    let value = Match_single (Paths.fresh ()) in
    let uf_cases = check_uniqueness_cases ienv value cases in
    (* we don't know how much of e will be run; safe to assume all of them *)
    UF.seq uf_body uf_cases
  | Texp_tuple (es, _) ->
    UF.pars
      (List.mapi
         (fun i (_, e) ->
           check_uniqueness_exp
             ~overwrite:(descend (Projection.Tuple_field i) overwrite)
             ienv e)
         es)
  | Texp_unboxed_tuple es ->
    UF.pars
      (List.map
         (fun (_, e, _) -> check_uniqueness_exp ~overwrite:None ienv e)
         es)
  | Texp_construct (lbl, _, es, _) ->
    let name = Longident.last lbl.txt in
    UF.pars
      (List.mapi
         (fun i e ->
           check_uniqueness_exp
             ~overwrite:
               (descend (Projection.Construct_field (name, i)) overwrite)
             ienv e)
         es)
  | Texp_variant (_, None) -> UF.unused
  | Texp_variant (_, Some (arg, _)) ->
    check_uniqueness_exp ~overwrite:None ienv arg
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
          | l, Overridden (_, e) ->
            check_uniqueness_exp
              ~overwrite:
                (descend (Projection.Record_field l.lbl_name) overwrite)
              ienv e)
        fields
    in
    UF.par uf_ext (UF.pars (Array.to_list uf_fields))
  | Texp_field _ ->
    let value, uf = check_uniqueness_exp_as_value ienv exp in
    UF.seq uf (Value.mark_maybe_unique value)
  | Texp_setfield (rcd, _, _, _, arg) ->
    let value, uf_rcd = check_uniqueness_exp_as_value ienv rcd in
    let uf_arg = check_uniqueness_exp ~overwrite:None ienv arg in
    let uf_write = Value.mark_implicit_borrow_memory_address Write value in
    let uf_tag = Value.invalidate_tag value in
    UF.pars [uf_rcd; uf_arg; uf_write; uf_tag]
  | Texp_array (_, _, es, _) ->
    UF.pars (List.map (fun e -> check_uniqueness_exp ~overwrite:None ienv e) es)
  | Texp_ifthenelse (if_, then_, else_opt) ->
    (* if' is only borrowed, not used; but probably doesn't matter because of
       mode crossing *)
    let uf_cond = check_uniqueness_exp ~overwrite:None ienv if_ in
    let uf_then = check_uniqueness_exp ~overwrite:None ienv then_ in
    let uf_else =
      match else_opt with
      | Some else_ -> check_uniqueness_exp ~overwrite:None ienv else_
      | None -> UF.unused
    in
    UF.seq uf_cond (UF.choose uf_then uf_else)
  | Texp_sequence (e0, _, e1) ->
    let uf0 = check_uniqueness_exp ~overwrite:None ienv e0 in
    let uf1 = check_uniqueness_exp ~overwrite:None ienv e1 in
    UF.seq uf0 uf1
  | Texp_while { wh_cond; wh_body; _ } ->
    let uf_cond = check_uniqueness_exp ~overwrite:None ienv wh_cond in
    let uf_body = check_uniqueness_exp ~overwrite:None ienv wh_body in
    UF.seq uf_cond uf_body
  | Texp_list_comprehension { comp_body; comp_clauses } ->
    let uf_body = check_uniqueness_exp ~overwrite:None ienv comp_body in
    let uf_clauses = check_uniqueness_comprehensions ienv comp_clauses in
    UF.par uf_body uf_clauses
  | Texp_array_comprehension (_, _, { comp_body; comp_clauses }) ->
    let uf_body = check_uniqueness_exp ~overwrite:None ienv comp_body in
    let uf_clauses = check_uniqueness_comprehensions ienv comp_clauses in
    UF.par uf_body uf_clauses
  | Texp_for { for_from; for_to; for_body; _ } ->
    let uf_from = check_uniqueness_exp ~overwrite:None ienv for_from in
    let uf_to = check_uniqueness_exp ~overwrite:None ienv for_to in
    let uf_body = check_uniqueness_exp ~overwrite:None ienv for_body in
    UF.seq (UF.par uf_from uf_to) uf_body
  | Texp_send (e, _, _) -> check_uniqueness_exp ~overwrite:None ienv e
  | Texp_new _ -> UF.unused
  | Texp_instvar _ -> UF.unused
  | Texp_setinstvar (_, _, _, e) -> check_uniqueness_exp ~overwrite:None ienv e
  | Texp_override (_, ls) ->
    UF.pars
      (List.map
         (fun (_, _, e) -> check_uniqueness_exp ~overwrite:None ienv e)
         ls)
  | Texp_letmodule (_, _, _, mod_expr, body) ->
    let uf_mod =
      mark_aliased_open_variables ienv
        (fun iter -> iter.module_expr iter mod_expr)
        mod_expr.mod_loc
    in
    let uf_body = check_uniqueness_exp ~overwrite:None ienv body in
    UF.seq uf_mod uf_body
  | Texp_letexception (_, e) -> check_uniqueness_exp ~overwrite:None ienv e
  | Texp_assert (e, _) -> check_uniqueness_exp ~overwrite:None ienv e
  | Texp_lazy e ->
    let uf = check_uniqueness_exp ~overwrite:None ienv e in
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
    UF.seq uf (check_uniqueness_exp ~overwrite:None ienv e)
  | Texp_probe { handler } -> check_uniqueness_exp ~overwrite:None ienv handler
  | Texp_probe_is_enabled _ -> UF.unused
  | Texp_exclave e -> check_uniqueness_exp ~overwrite:None ienv e
  | Texp_src_pos -> UF.unused
  | Texp_overwrite (e1, e2) ->
    let value, uf = check_uniqueness_exp_as_value ienv e1 in
    let uf_tag =
      match e2.exp_desc with
      | Texp_construct (lbl, cd, _, _) ->
        Value.overwrite_tag { tag = cd.cstr_tag; name_for_error = lbl } value
      | Texp_record _ | Texp_tuple _ -> UF.unused
      | _ ->
        Misc.fatal_error "Uniqueness analysis: overwrite of unexpected term"
    in
    let uf_body = check_uniqueness_exp ~overwrite:(Value.paths value) ienv e2 in
    (* We first evaluate the body and then perform the overwrite: *)
    UF.seqs [uf; uf_tag; uf_body; Value.mark_consumed_memory_address value]
  | Texp_hole use -> (
    match overwrite with
    | None -> assert false
    | Some p ->
      let occ = Occurrence.mk exp.exp_loc in
      Paths.mark
        (Usage.maybe_unique use occ)
        Learned_tags.empty Overwrites.empty p)

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
          ( Paths.mark
              (Usage.maybe_unique unique_use occ)
              Learned_tags.empty Overwrites.empty paths,
            Value.fresh )
      in
      value, UF.seqs [uf; uf_read; uf_boxing])
  (* CR-someday anlorenzen: This could also support let-bindings. *)
  | _ -> Value.fresh, check_uniqueness_exp ~overwrite:None ienv exp

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
           (* We ignore the tags from value bindings *)
           let uf_pat, _tags = UF.split_usage_tags uf_pat in
           ienv, UF.seq uf_value uf_pat)
         vbs)
  in
  Ienv.Extension.conjuncts exts, UF.pars uf_vbs

(* type signature needed because invoked on both value and computation patterns *)
and check_uniqueness_cases_gen :
      'a.
      ('a Typedtree.general_pattern -> _ -> _) -> _ -> _ -> 'a case list -> _ =
 fun pat_match ienv value cases ->
  (* At first, we keep the information from patterns, guards and branches
     separate. We only use the Ienv from the patterns in the guards and
     branches. *)
  let exts, uf_pats =
    List.split
      (List.map
         (fun case ->
           let ext, uf_lhs = pat_match case.c_lhs value in
           let uf_guard =
             match case.c_guard with
             | None -> UF.unused
             | Some g ->
               check_uniqueness_exp ~overwrite:None (Ienv.extend ienv ext) g
           in
           ext, (uf_lhs, uf_guard))
         cases)
  in
  let uf_cases =
    List.map2
      (fun ext case ->
        check_uniqueness_exp ~overwrite:None (Ienv.extend ienv ext) case.c_rhs)
      exts cases
  in
  let uf_lhss, uf_guards = List.split uf_pats in
  let uf_lhss_usages, uf_lhss_tags =
    List.split (List.map UF.split_usage_tags uf_lhss)
  in
  (* We combine patterns as follows: We assume that the patterns and guards
     could be evaluated in any order. This is necessary since the pattern-match
     compiler might permute them. After this stage, only one of the branches is
     evaluated. *)
  UF.seq
    (UF.pars (List.map2 UF.par uf_lhss_usages uf_guards))
    (UF.chooses (List.map2 UF.match_with_learned_tags uf_lhss_tags uf_cases))

and check_uniqueness_cases ienv value cases =
  check_uniqueness_cases_gen pattern_match ienv value cases

and check_uniqueness_comp_cases ienv value cases =
  check_uniqueness_cases_gen comp_pattern_match ienv value cases

and check_uniqueness_comprehensions ienv cs =
  UF.pars
    (List.map
       (fun c ->
         match c with
         | Texp_comp_when e -> check_uniqueness_exp ~overwrite:None ienv e
         | Texp_comp_for cbs ->
           check_uniqueness_comprehension_clause_binding ienv cbs)
       cs)

and check_uniqueness_comprehension_clause_binding ienv cbs =
  UF.pars
    (List.map
       (fun cb ->
         match cb.comp_cb_iterator with
         | Texp_comp_range { start; stop; _ } ->
           let uf_start = check_uniqueness_exp ~overwrite:None ienv start in
           let uf_stop = check_uniqueness_exp ~overwrite:None ienv stop in
           UF.par uf_start uf_stop
         | Texp_comp_in { sequence; _ } ->
           check_uniqueness_exp ~overwrite:None ienv sequence)
       cbs)

and check_uniqueness_binding_op ienv bo =
  let occ = Occurrence.mk bo.bop_loc in
  let uf_path =
    match value_of_ident ienv aliased_many_use occ bo.bop_op_path with
    | Some value -> Value.mark_maybe_unique value
    | None -> UF.unused
  in
  let uf_exp = check_uniqueness_exp ~overwrite:None ienv bo.bop_exp in
  UF.par uf_path uf_exp

let check_uniqueness_exp exp =
  let uf = check_uniqueness_exp ~overwrite:None Ienv.empty exp in
  UF.check_no_remaining_overwritten_as uf;
  ()

let check_uniqueness_value_bindings vbs =
  let _, uf = check_uniqueness_value_bindings Ienv.empty vbs in
  UF.check_no_remaining_overwritten_as uf;
  ()

let report_multi_use inner first_is_of_second =
  let { Usage.cannot_force = { occ; axis };
        there;
        first_or_second;
        access_order
      } =
    inner
  in
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
  let access_order =
    match access_order with
    | Par -> "is already being"
    | Seq -> "has already been"
  in
  (* English is sadly not very composible, we write out all four cases
     manually *)
  let error =
    match first_or_second, axis with
    | First, Uniqueness ->
      Format.dprintf "This value is %s here,@ but %s %s %s as unique:"
        second_usage first_is_of_second access_order first_usage
    | First, Linearity ->
      Format.dprintf
        "This value is %s here,@ but %s is defined as once and %s %s:"
        second_usage first_is_of_second access_order first_usage
    | Second, Uniqueness ->
      Format.dprintf "This value is %s here as unique,@ but %s %s %s:"
        second_usage first_is_of_second access_order first_usage
    | Second, Linearity ->
      Format.dprintf "This value is defined as once and %s here,@ but %s %s %s:"
        second_usage first_is_of_second access_order first_usage
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

let report_tag_change (err : Overwrites.error) =
  match err with
  | Changed_tag { old_tag; new_tag } ->
    let new_tag_txt =
      Format.dprintf "%a" Pprintast.longident new_tag.name_for_error.txt
    in
    let old_tag_txt =
      match old_tag with
      | Old_tag_unknown -> Format.dprintf "is unknown."
      | Old_tag_was l ->
        Format.dprintf "is %a." Pprintast.longident l.name_for_error.txt
      | Old_tag_mutated access_order -> (
        match access_order with
        | Par -> Format.dprintf "is being changed through mutation."
        | Seq -> Format.dprintf "was changed through mutation.")
    in
    Location.errorf ~loc:new_tag.name_for_error.loc
      "@[Overwrite may not change the tag to %t.\n\
       Hint: The old tag of this allocation %t@]" new_tag_txt old_tag_txt

let report_error err =
  Printtyp.wrap_printing_env ~error:true Env.empty (fun () ->
      match err with
      | Usage { inner; first_is_of_second } ->
        report_multi_use inner first_is_of_second
      | Boundary { cannot_force; reason } -> report_boundary cannot_force reason
      | Overwrite_changed_tag err -> report_tag_change err)

let () =
  Location.register_error_of_exn (function
    | Error e -> Some (report_error e)
    | _ -> None)
