include Language_extension_kernel

type 'a language_extension = 'a t

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

let maturity_to_string = function
  | Alpha -> "alpha"
  | Beta -> "beta"
  | Stable -> "stable"

let get_level_ops : type a. a t -> (module Extension_level with type t = a) =
  function
  | Comprehensions -> (module Unit)
  | Mode -> (module Maturity)
  | Unique -> (module Maturity)
  | Overwriting -> (module Unit)
  | Include_functor -> (module Unit)
  | Polymorphic_parameters -> (module Unit)
  | Immutable_arrays -> (module Unit)
  | Module_strengthening -> (module Unit)
  | Layouts -> (module Maturity)
  | SIMD -> (module Maturity)
  | Labeled_tuples -> (module Unit)
  | Small_numbers -> (module Maturity)
  | Instances -> (module Unit)
  | Separability -> (module Unit)
  | Let_mutable -> (module Unit)

(* We'll do this in a more principled way later. *)
(* CR layouts: Note that layouts is only "mostly" erasable, because of annoying
   interactions with the pre-layouts [@@immediate] attribute like:

     type ('a : immediate) t = 'a [@@immediate]

   But we've decided to punt on this issue in the short term.
*)
let is_erasable : type a. a t -> bool = function
  | Mode | Unique | Overwriting | Layouts -> true
  | Comprehensions | Include_functor | Polymorphic_parameters | Immutable_arrays
  | Module_strengthening | SIMD | Labeled_tuples | Small_numbers | Instances
  | Separability | Let_mutable ->
    false

let maturity_of_unique_for_drf = Stable

let maturity_of_unique_for_destruction = Alpha

module Exist_pair = struct
  type t = Pair : 'a language_extension * 'a -> t

  let maturity : t -> Maturity.t = function
    | Pair (Comprehensions, ()) -> Beta
    | Pair (Mode, m) -> m
    | Pair (Unique, m) -> m
    | Pair (Overwriting, ()) -> Alpha
    | Pair (Include_functor, ()) -> Stable
    | Pair (Polymorphic_parameters, ()) -> Stable
    | Pair (Immutable_arrays, ()) -> Stable
    | Pair (Module_strengthening, ()) -> Stable
    | Pair (Layouts, m) -> m
    | Pair (SIMD, m) -> m
    | Pair (Labeled_tuples, ()) -> Stable
    | Pair (Small_numbers, m) -> m
    | Pair (Instances, ()) -> Stable
    | Pair (Separability, ()) -> Stable
    | Pair (Let_mutable, ()) -> Stable

  let is_erasable : t -> bool = function Pair (ext, _) -> is_erasable ext

  let to_string = function
    | Pair (Layouts, m) -> to_string Layouts ^ "_" ^ maturity_to_string m
    | Pair (Mode, m) -> to_string Mode ^ "_" ^ maturity_to_string m
    | Pair (Unique, m) -> to_string Unique ^ "_" ^ maturity_to_string m
    | Pair (Small_numbers, m) ->
      to_string Small_numbers ^ "_" ^ maturity_to_string m
    | Pair (SIMD, m) -> to_string SIMD ^ "_" ^ maturity_to_string m
    | Pair
        ( (( Comprehensions | Include_functor | Polymorphic_parameters
           | Immutable_arrays | Module_strengthening | Labeled_tuples
           | Instances | Overwriting | Separability | Let_mutable ) as ext),
          _ ) ->
      to_string ext

  (* converts full extension names, like "layouts_alpha" to a pair of
     an extension and its maturity. For extensions that don't take an
     argument, the conversion is just [Language_extension_kernel.of_string].
  *)
  let of_string extn_name : t option =
    match String.lowercase_ascii extn_name with
    | "comprehensions" -> Some (Pair (Comprehensions, ()))
    | "mode" -> Some (Pair (Mode, Stable))
    | "mode_beta" -> Some (Pair (Mode, Beta))
    | "mode_alpha" -> Some (Pair (Mode, Alpha))
    | "unique" -> Some (Pair (Unique, Stable))
    | "unique_beta" -> Some (Pair (Unique, Beta))
    | "unique_alpha" -> Some (Pair (Unique, Alpha))
    | "overwriting" -> Some (Pair (Overwriting, ()))
    | "include_functor" -> Some (Pair (Include_functor, ()))
    | "polymorphic_parameters" -> Some (Pair (Polymorphic_parameters, ()))
    | "immutable_arrays" -> Some (Pair (Immutable_arrays, ()))
    | "module_strengthening" -> Some (Pair (Module_strengthening, ()))
    | "layouts" -> Some (Pair (Layouts, Stable))
    | "layouts_alpha" -> Some (Pair (Layouts, Alpha))
    | "layouts_beta" -> Some (Pair (Layouts, Beta))
    | "simd" -> Some (Pair (SIMD, Stable))
    | "simd_beta" -> Some (Pair (SIMD, Beta))
    | "simd_alpha" -> Some (Pair (SIMD, Alpha))
    | "labeled_tuples" -> Some (Pair (Labeled_tuples, ()))
    | "small_numbers" -> Some (Pair (Small_numbers, Stable))
    | "small_numbers_beta" -> Some (Pair (Small_numbers, Beta))
    | "instances" -> Some (Pair (Instances, ()))
    | "separability" -> Some (Pair (Separability, ()))
    | "let_mutable" -> Some (Pair (Let_mutable, ()))
    | _ -> None
end

type extn_pair = Exist_pair.t = Pair : 'a t * 'a -> extn_pair

type exist = Pack : _ t -> exist

let all_extensions =
  [ Pack Comprehensions;
    Pack Mode;
    Pack Unique;
    Pack Overwriting;
    Pack Include_functor;
    Pack Polymorphic_parameters;
    Pack Immutable_arrays;
    Pack Module_strengthening;
    Pack Layouts;
    Pack SIMD;
    Pack Labeled_tuples;
    Pack Small_numbers;
    Pack Instances;
    Pack Separability;
    Pack Let_mutable ]

(**********************************)
(* string conversions *)

let to_command_line_string : type a. a t -> a -> string =
 fun extn level ->
  let (module Ops) = get_level_ops extn in
  to_string extn ^ Ops.to_command_line_suffix level

let pair_of_string_exn extn_name =
  match Exist_pair.of_string extn_name with
  | Some pair -> pair
  | None ->
    raise (Arg.Bad (Printf.sprintf "Extension %s is not known" extn_name))

let of_string extn_name : exist option =
  match Exist_pair.of_string extn_name with
  | Some (Pair (ext, _)) -> Some (Pack ext)
  | None -> None

(************************************)
(* equality *)

let equal_t (type a b) (a : a t) (b : b t) : (a, b) Misc.eq option =
  match a, b with
  | Comprehensions, Comprehensions -> Some Refl
  | Mode, Mode -> Some Refl
  | Unique, Unique -> Some Refl
  | Overwriting, Overwriting -> Some Refl
  | Include_functor, Include_functor -> Some Refl
  | Polymorphic_parameters, Polymorphic_parameters -> Some Refl
  | Immutable_arrays, Immutable_arrays -> Some Refl
  | Module_strengthening, Module_strengthening -> Some Refl
  | Layouts, Layouts -> Some Refl
  | SIMD, SIMD -> Some Refl
  | Labeled_tuples, Labeled_tuples -> Some Refl
  | Small_numbers, Small_numbers -> Some Refl
  | Instances, Instances -> Some Refl
  | Separability, Separability -> Some Refl
  | Let_mutable, Let_mutable -> Some Refl
  | ( ( Comprehensions | Mode | Unique | Overwriting | Include_functor
      | Polymorphic_parameters | Immutable_arrays | Module_strengthening
      | Layouts | SIMD | Labeled_tuples | Small_numbers | Instances
      | Separability | Let_mutable ),
      _ ) ->
    None

let equal a b = Option.is_some (equal_t a b)

(*****************************)
(* extension universes *)

module Universe : sig
  type t =
    | No_extensions
    | Upstream_compatible
    | Stable
    | Beta
    | Alpha

  val all : t list

  val maximal : t

  val to_string : t -> string

  val of_string : string -> t option

  val get : unit -> t

  val set : t -> unit

  val is : t -> bool

  val check : extn_pair -> unit

  (* Allowed extensions, each with the greatest allowed level. *)
  val allowed_extensions_in : t -> extn_pair list
end = struct
  (** Which extensions can be enabled? *)
  type t =
    | No_extensions
    | Upstream_compatible
    | Stable
    | Beta
    | Alpha
  (* If you add a constructor, you should also add it to [all]. *)

  let all = [No_extensions; Upstream_compatible; Stable; Beta; Alpha]

  let maximal = Alpha

  let to_string = function
    | No_extensions -> "no_extensions"
    | Upstream_compatible -> "upstream_compatible"
    | Stable -> "stable"
    | Beta -> "beta"
    | Alpha -> "alpha"

  let of_string = function
    | "no_extensions" -> Some No_extensions
    | "upstream_compatible" -> Some Upstream_compatible
    | "stable" -> Some Stable
    | "beta" -> Some Beta
    | "alpha" -> Some Alpha
    | _ -> None

  let compare t1 t2 =
    let rank = function
      | No_extensions -> 0
      | Upstream_compatible -> 1
      | Stable -> 2
      | Beta -> 3
      | Alpha -> 4
    in
    compare (rank t1) (rank t2)

  (* For now, the default universe is set to [Alpha] but only a limited set of
     extensions is enabled. After the migration to extension universes, the
     default will be [No_extensions]. *)
  let universe = ref Alpha

  let get () = !universe

  let set new_universe = universe := new_universe

  let is u = compare u !universe = 0

  let compiler_options = function
    | No_extensions -> "flag -extension-universe no_extensions"
    | Upstream_compatible -> "flag -extension-universe upstream_compatible"
    | Stable -> "flag -extension-universe stable"
    | Beta -> "flag -extension-universe beta"
    | Alpha -> "flag -extension-universe alpha (default CLI option)"

  let is_allowed_in t extn_pair =
    match t with
    | No_extensions -> false
    | Upstream_compatible ->
      Exist_pair.is_erasable extn_pair
      && Maturity.compare (Exist_pair.maturity extn_pair) Stable <= 0
    | Stable -> Maturity.compare (Exist_pair.maturity extn_pair) Stable <= 0
    | Beta -> Maturity.compare (Exist_pair.maturity extn_pair) Beta <= 0
    | Alpha -> true

  let is_allowed extn_pair = is_allowed_in !universe extn_pair

  (* The terminating [()] argument helps protect against ignored arguments. See
     the documentation for [Base.failwithf]. *)
  let fail fmt = Format.ksprintf (fun str () -> raise (Arg.Bad str)) fmt

  let check extn_pair =
    if not (is_allowed extn_pair)
    then
      fail "Cannot enable extension %s: incompatible with %s"
        (Exist_pair.to_string extn_pair)
        (compiler_options !universe)
        ()

  let allowed_extensions_in t =
    let maximal_in_universe (Pack extn) =
      let (module Ops) = get_level_ops extn in
      let allowed_levels =
        Ops.all |> List.filter (fun lvl -> is_allowed_in t (Pair (extn, lvl)))
      in
      match allowed_levels with
      | [] -> None
      | lvl :: lvls ->
        let max_allowed_lvl = List.fold_left Ops.max lvl lvls in
        Some (Pair (extn, max_allowed_lvl))
    in
    List.filter_map maximal_in_universe all_extensions
end

(*****************************************)
(* enabling / disabling *)

(* Mutable state. Invariants:

   (1) [!extensions] contains at most one copy of each extension.

   (2) Every member of [!extensions] satisfies [Universe.is_allowed]. (For
   instance, [!universe = No_extensions] implies [!extensions = []]). *)

(* After the migration to extension universes, this will be an empty list. *)
let legacy_default_extensions : extn_pair list =
  Universe.allowed_extensions_in Stable

let extensions : extn_pair list ref = ref legacy_default_extensions

let set_worker (type a) (extn : a t) = function
  | Some value ->
    Universe.check (Pair (extn, value));
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
  extensions := List.map maximal_pair all_extensions

let erasable_extensions_only () =
  Universe.is No_extensions || Universe.is Upstream_compatible

let set_universe_and_enable_all u =
  Universe.set u;
  extensions := Universe.allowed_extensions_in (Universe.get ())

let set_universe_and_enable_all_of_string_exn univ_name =
  match Universe.of_string univ_name with
  | Some u -> set_universe_and_enable_all u
  | None ->
    raise (Arg.Bad (Printf.sprintf "Universe %s is not known" univ_name))

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
  type t = exist = Pack : _ language_extension -> t

  let all = all_extensions

  let to_command_line_strings (Pack extn) =
    let (module Ops) = get_level_ops extn in
    List.map (to_command_line_string extn) Ops.all

  let to_string : t -> string = function Pack extn -> to_string extn

  let is_enabled : t -> bool = function Pack extn -> is_enabled extn

  let is_erasable : t -> bool = function Pack extn -> is_erasable extn
end

module Error = struct
  type error =
    | Disabled_extension :
        { ext : _ t;
          maturity : maturity option
        }
        -> error

  let report_error ~loc = function
    | Disabled_extension { ext; maturity } -> (
      (* CR layouts: The [maturity] special case is a bit ad-hoc, but the
         layouts error message would be much worse without it. It also would be
         nice to mention the language construct in the error message. *)
      match maturity with
      | None ->
        Location.errorf ~loc
          "The extension \"%s\" is disabled and cannot be used" (to_string ext)
      | Some maturity ->
        Location.errorf ~loc
          "This construct requires the %s version of the extension \"%s\", \
           which is disabled and cannot be used"
          (maturity_to_string maturity)
          (to_string ext))

  exception Error of Location.t * error

  let () =
    Location.register_error_of_exn (function
      | Error (loc, err) -> Some (report_error ~loc err)
      | _ -> None)
end

let assert_enabled (type a) ~loc (t : a t) (setting : a) =
  if not (is_at_least t setting)
  then
    let maturity : maturity option =
      match t with Layouts -> Some (setting : maturity) | _ -> None
    in
    raise (Error.Error (loc, Disabled_extension { ext = t; maturity }))

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
