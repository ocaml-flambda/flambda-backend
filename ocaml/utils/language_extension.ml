type maturity = Stable | Beta | Alpha

let equal_maturity (a : maturity) (b : maturity) = (a = b)

type t =
  | Comprehensions
  | Local
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays
  | Module_strengthening
  | Layouts of maturity

let equal (a : t) (b : t) = (a = b)

let all =
  [ Comprehensions
  ; Local
  ; Include_functor
  ; Polymorphic_parameters
  ; Immutable_arrays
  ; Module_strengthening
  ; Layouts Alpha
  ; Layouts Beta
  ; Layouts Stable
  ]

let max_compatible =
  [ Comprehensions
  ; Local
  ; Include_functor
  ; Polymorphic_parameters
  ; Immutable_arrays
  ; Module_strengthening
  ; Layouts Alpha
  ]

let default_extensions =
  [ Local
  ; Include_functor
  ; Polymorphic_parameters
  ]

let to_string = function
  | Comprehensions -> "comprehensions"
  | Local -> "local"
  | Include_functor -> "include_functor"
  | Polymorphic_parameters -> "polymorphic_parameters"
  | Immutable_arrays -> "immutable_arrays"
  | Module_strengthening -> "module_strengthening"
  | Layouts Alpha -> "layouts_alpha"
  | Layouts Beta -> "layouts_beta"
  | Layouts Stable -> "layouts"

let of_string extn = match String.lowercase_ascii extn with
  | "comprehensions" -> Some Comprehensions
  | "local" -> Some Local
  | "include_functor" -> Some Include_functor
  | "polymorphic_parameters" -> Some Polymorphic_parameters
  | "immutable_arrays" -> Some Immutable_arrays
  | "strengthening" -> Some Module_strengthening
  | "layouts_alpha" -> Some (Layouts Alpha)
  | "layouts_beta" -> Some (Layouts Beta)
  | "layouts" -> Some (Layouts Stable)
  | _ -> None

let of_string_exn extn =
  match of_string extn with
  | Some extn -> extn
  | None -> raise (Arg.Bad(Printf.sprintf "Extension %s is not known" extn))

(* We'll do this in a more principled way later. *)
(* CR layouts: Note that layouts is only "mostly" erasable, because of annoying
   interactions with the pre-layouts [@@immediate] attribute like:

     type ('a : immediate) t = 'a [@@immediate]

   But we've decided to punt on this issue in the short term.
*)
let is_erasable = function
  | Local
  | Layouts Alpha
  | Layouts Beta
  | Layouts Stable ->
      true
  | Comprehensions
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays
  | Module_strengthening ->
      false

module Universe = struct
  (** Which extensions can be enabled? *)
  type t =
    | No_extensions
    | Only_erasable
    | Any

  let compiler_options = function
    | No_extensions -> "flag -disable-all-extensions"
    | Only_erasable -> "flag -only-erasable-extensions"
    | Any           -> "default options"

  let is_allowed t ext = match t with
    | No_extensions -> false
    | Only_erasable -> is_erasable ext
    | Any           -> true
end

(* Mutable state.  Invariants:

   (1) [!extensions] contains at most one copy of each extension.

   (2) Every member of [!extensions] satisfies [Universe.is_allowed !universe].
       (For instance, [!universe = No_extensions] implies
       [!extensions = []]). *)
let extensions = ref default_extensions (* -extension *)
let universe   = ref Universe.Any       (* -only-erasable-extensions,
                                           -disable-all-extensions *)

type compatibility = Compatible | Duplicate | Incompatible of string

let check_conflicts t1 =
  let layouts_err =
    "Invalid extensions: Please enable at most one of 'layouts', \
     'layouts_beta', and 'layouts_alpha'."
  in
  let c = List.find_map (fun t2 ->
    if equal t1 t2 then Some Duplicate else
      match t1, t2 with
      | Layouts _, Layouts _ -> Some (Incompatible layouts_err)
      | _, _ -> None)
    !extensions
  in
  Option.value c ~default:Compatible

let set extn ~enabled =
  if enabled then begin
    if not (Universe.is_allowed !universe extn) then
      raise (Arg.Bad(Printf.sprintf
        "Cannot %s extension %s: incompatible with %s"
        (if enabled then "enable" else "disable")
        (to_string extn)
        (Universe.compiler_options !universe)));
    match check_conflicts extn with
    | Duplicate -> ()
    | Compatible -> extensions := extn :: !extensions
    | Incompatible err -> raise (Arg.Bad err)
  end else
    extensions :=
      List.filter (fun extn' ->
        match extn, extn' with
        | Layouts m1, Layouts m2 when not (equal_maturity m1 m2) ->
            raise (Arg.Bad(Printf.sprintf
              "Cannot disable extension %s because extension %s is enabled. \
               Please enable or disable at most one of the layouts extensions."
              (to_string extn) (to_string extn')))
        | _, _ ->
            not (equal extn extn'))
        !extensions

let enable  = set ~enabled:true
let disable = set ~enabled:false

let is_enabled extn = List.mem extn !extensions

(* It might make sense to ban [set], [enable], [disable],
   [only_erasable_extensions], and [disallow_extensions] inside [f], but it's
   not clear that it's worth the hassle *)
let with_set extn ~enabled f =
  (* This is similar to [Misc.protect_refs], but we don't have values to set
     [extensions] and [universe] to. *)
  let current_extensions = !extensions in
  let current_universe   = !universe   in
  Fun.protect
    ~finally:(fun () ->
      extensions := current_extensions;
      universe   := current_universe)
    (fun () ->
       set extn ~enabled;
       f ())

let with_enabled  = with_set ~enabled:true
let with_disabled = with_set ~enabled:false

let restrict_to_erasable_extensions () =
  match !universe with
  | Any ->
      extensions := List.filter is_erasable !extensions;
      universe   := Universe.Only_erasable
  | Only_erasable ->
      () (* Idempotent *)
  | No_extensions ->
      raise (Arg.Bad(Printf.sprintf
        "Cannot specify %s: incompatible with %s"
        (Universe.compiler_options Only_erasable)
        (Universe.compiler_options No_extensions)))

let disallow_extensions () =
  (* The strictest option, so no internal checks needed *)
  extensions := [];
  universe   := Universe.No_extensions
