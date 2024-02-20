include Language_extension_kernel

(* operations we want on every extension level *)
module type Extension_level = sig
  type t

  val compare : t -> t -> int

  val max : t -> t -> t

  val max_value : t

  val all : t list

  val to_command_line_suffix : t -> string
end

module Unit = struct
  type t = unit

  let compare = Unit.compare

  let max _ _ = ()

  let max_value = ()

  let all = [()]

  let to_command_line_suffix () = ""
end

module Maturity = struct
  type t = maturity =
    | Stable
    | Beta
    | Alpha

  let compare t1 t2 =
    let rank = function Stable -> 1 | Beta -> 2 | Alpha -> 3 in
    compare (rank t1) (rank t2)

  let max t1 t2 = if compare t1 t2 >= 0 then t1 else t2

  let max_value = Alpha

  let all = [Stable; Beta; Alpha]

  let to_command_line_suffix = function
    | Stable -> ""
    | Beta -> "_beta"
    | Alpha -> "_alpha"
end

let get_level_ops : type a. a t -> (module Extension_level with type t = a) =
  function
  | Comprehensions -> (module Unit)
  | Local -> (module Unit)
  | Unique -> (module Unit)
  | Include_functor -> (module Unit)
  | Polymorphic_parameters -> (module Unit)
  | Immutable_arrays -> (module Unit)
  | Module_strengthening -> (module Unit)
  | Layouts -> (module Maturity)
  | SIMD -> (module Unit)
  | Labeled_tuples -> (module Unit)
  | Instances -> (module Unit)

type extn_pair = Exist_pair.t = Pair : 'a t * 'a -> extn_pair

type exist = Exist.t = Pack : _ t -> exist

(**********************************)
(* string conversions *)

let to_command_line_string : type a. a t -> a -> string =
 fun extn level ->
  let (module Ops) = get_level_ops extn in
  to_string extn ^ Ops.to_command_line_suffix level

let pair_of_string_exn extn_name =
  match pair_of_string extn_name with
  | Some pair -> pair
  | None ->
    raise (Arg.Bad (Printf.sprintf "Extension %s is not known" extn_name))

(************************************)
(* equality *)

let equal_t (type a b) (a : a t) (b : b t) : (a, b) Misc.eq option =
  match a, b with
  | Comprehensions, Comprehensions -> Some Refl
  | Local, Local -> Some Refl
  | Unique, Unique -> Some Refl
  | Include_functor, Include_functor -> Some Refl
  | Polymorphic_parameters, Polymorphic_parameters -> Some Refl
  | Immutable_arrays, Immutable_arrays -> Some Refl
  | Module_strengthening, Module_strengthening -> Some Refl
  | Layouts, Layouts -> Some Refl
  | SIMD, SIMD -> Some Refl
  | Labeled_tuples, Labeled_tuples -> Some Refl
  | Instances, Instances -> Some Refl
  | ( ( Comprehensions | Local | Unique | Include_functor
      | Polymorphic_parameters | Immutable_arrays | Module_strengthening
      | Layouts | SIMD | Labeled_tuples | Instances ),
      _ ) ->
    None

let equal a b = Option.is_some (equal_t a b)

(*****************************)
(* extension universes *)

module Universe : sig
  val is_allowed : 'a t -> bool

  val check : 'a t -> unit

  val check_maximal : unit -> unit

  type t =
    | No_extensions
    | Only_erasable
    | Any

  val set : t -> bool
end = struct
  (** Which extensions can be enabled? *)
  type t =
    | No_extensions
    | Only_erasable
    | Any

  let compare t1 t2 =
    let rank = function No_extensions -> 1 | Only_erasable -> 2 | Any -> 3 in
    compare (rank t1) (rank t2)

  let universe = ref Any

  let compiler_options = function
    | No_extensions -> "flag -disable-all-extensions"
    | Only_erasable -> "flag -only-erasable-extensions"
    | Any -> "default options"

  let is_allowed ext =
    match !universe with
    | No_extensions -> false
    | Only_erasable -> is_erasable ext
    | Any -> true

  (* are _all_ extensions allowed? *)
  let all_allowed () =
    match !universe with Any -> true | No_extensions | Only_erasable -> false

  (* The terminating [()] argument helps protect against ignored arguments. See
     the documentation for [Base.failwithf]. *)
  let fail fmt = Format.ksprintf (fun str () -> raise (Arg.Bad str)) fmt

  let check extn =
    if not (is_allowed extn)
    then
      fail "Cannot enable extension %s: incompatible with %s" (to_string extn)
        (compiler_options !universe)
        ()

  let check_maximal () =
    if not (all_allowed ())
    then
      fail "Cannot enable all extensions: incompatible with %s"
        (compiler_options !universe)
        ()

  (* returns whether or not a change was actually made *)
  let set new_universe =
    let cmp = compare new_universe !universe in
    if cmp > 0
    then
      fail "Cannot specify %s: incompatible with %s"
        (compiler_options new_universe)
        (compiler_options !universe)
        ();
    universe := new_universe;
    cmp <> 0
end

(*****************************************)
(* enabling / disabling *)

(* Mutable state. Invariants:

   (1) [!extensions] contains at most one copy of each extension.

   (2) Every member of [!extensions] satisfies [Universe.is_allowed]. (For
   instance, [!universe = No_extensions] implies [!extensions = []]). *)

let default_extensions : extn_pair list =
  [ Pair (Local, ());
    Pair (Include_functor, ());
    Pair (Polymorphic_parameters, ());
    Pair (Immutable_arrays, ());
    Pair (Labeled_tuples, ());
    Pair (Instances, ()) ]

let extensions : extn_pair list ref = ref default_extensions

let set_worker (type a) (extn : a t) = function
  | Some value ->
    Universe.check extn;
    let (module Ops) = get_level_ops extn in
    let rec update_extensions already_seen : extn_pair list -> extn_pair list =
      function
      | [] -> Pair (extn, value) :: already_seen
      | (Pair (extn', v) as e) :: es -> (
        match equal_t extn extn' with
        | None -> update_extensions (e :: already_seen) es
        | Some Refl ->
          Pair (extn, Ops.max v value) :: List.rev_append already_seen es)
    in
    extensions := update_extensions [] !extensions
  | None ->
    extensions
      := List.filter
           (fun (Pair (extn', _) : extn_pair) -> not (equal extn extn'))
           !extensions

let set extn ~enabled = set_worker extn (if enabled then Some () else None)

let enable extn value = set_worker extn (Some value)

let disable extn = set_worker extn None

(* This is similar to [Misc.protect_refs], but we don't have values to set
   [extensions] to. *)
let with_temporary_extensions f =
  let current_extensions = !extensions in
  Fun.protect ~finally:(fun () -> extensions := current_extensions) f

(* It might make sense to ban [set], [enable], [disable],
   [only_erasable_extensions], and [disallow_extensions] inside [f], but it's
   not clear that it's worth the hassle *)
let with_set_worker extn value f =
  with_temporary_extensions (fun () ->
      set_worker extn value;
      f ())

let with_set extn ~enabled =
  with_set_worker extn (if enabled then Some () else None)

let with_enabled extn value = with_set_worker extn (Some value)

let with_disabled extn = with_set_worker extn None

let enable_of_string_exn extn_name =
  match pair_of_string_exn extn_name with
  | Pair (extn, setting) -> enable extn setting

let disable_of_string_exn extn_name =
  match pair_of_string_exn extn_name with Pair (extn, _) -> disable extn

let disable_all () = extensions := []

let unconditionally_enable_maximal_without_checks () =
  let maximal_pair (Pack extn) =
    let (module Ops) = get_level_ops extn in
    Pair (extn, Ops.max_value)
  in
  extensions := List.map maximal_pair Exist.all

let enable_maximal () =
  Universe.check_maximal ();
  (* It's safe to call this here because we've confirmed that we can. *)
  unconditionally_enable_maximal_without_checks ()

let restrict_to_erasable_extensions () =
  let changed = Universe.set Only_erasable in
  if changed
  then
    extensions
      := List.filter
           (fun (Pair (extn, _)) -> Universe.is_allowed extn)
           !extensions

let disallow_extensions () =
  ignore (Universe.set No_extensions : bool);
  disable_all ()

(********************************************)
(* checking an extension *)

let is_at_least (type a) (extn : a t) (value : a) =
  let rec check : extn_pair list -> bool = function
    | [] -> false
    | Pair (e, v) :: es -> (
      let (module Ops) = get_level_ops e in
      match equal_t e extn with
      | Some Refl -> Ops.compare v value >= 0
      | None -> check es)
  in
  check !extensions

let is_enabled extn =
  let rec check : extn_pair list -> bool = function
    | [] -> false
    | Pair (e, _) :: _ when equal e extn -> true
    | _ :: es -> check es
  in
  check !extensions

let get_command_line_string_if_enabled extn =
  let rec find = function
    | [] -> None
    | Pair (e, v) :: _ when equal e extn -> Some (to_command_line_string e v)
    | _ :: es -> find es
  in
  find !extensions

(********************************************)
(* existentially packed extension *)

module Exist = struct
  include Exist

  let to_command_line_strings (Pack extn) =
    let (module Ops) = get_level_ops extn in
    List.map (to_command_line_string extn) Ops.all

  let to_string : t -> string = function Pack extn -> to_string extn

  let is_enabled : t -> bool = function Pack extn -> is_enabled extn

  let is_erasable : t -> bool = function Pack extn -> is_erasable extn
end

(********************************************)
(* Special functionality for [Pprintast] *)

module For_pprintast = struct
  type printer_exporter =
    { print_with_maximal_extensions :
        'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
    }

  let can_still_define_printers = ref true

  let make_printer_exporter () =
    if !can_still_define_printers
    then (
      can_still_define_printers := false;
      { print_with_maximal_extensions =
          (fun pp fmt item ->
            with_temporary_extensions (fun () ->
                (* It's safe to call this here without validating that the
                   extensions are enabled, because the [Pprintast] printers
                   should always print Jane syntax. *)
                unconditionally_enable_maximal_without_checks ();
                pp fmt item))
      })
    else
      Misc.fatal_error
        "Only Pprintast may use [Language_extension.For_pprintast]"
end
