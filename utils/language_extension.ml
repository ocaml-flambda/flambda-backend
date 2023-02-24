type t =
  | Comprehensions
  | Local
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays

let equal (a : t) (b : t) = (a = b)

let all =
  [ Comprehensions
  ; Local
  ; Include_functor
  ; Polymorphic_parameters
  ; Immutable_arrays
  ]

let default_extensions =
  [ Local
  ; Include_functor
  ; Polymorphic_parameters
  ]

let to_string = function
  | Comprehensions -> "comprehensions_experimental"
  | Local -> "local"
  | Include_functor -> "include_functor"
  | Polymorphic_parameters -> "polymorphic_parameters"
  | Immutable_arrays -> "immutable_arrays_experimental"

let of_string extn = match String.lowercase_ascii extn with
  | "comprehensions_experimental" -> Some Comprehensions
  | "local" -> Some Local
  | "include_functor" -> Some Include_functor
  | "polymorphic_parameters" -> Some Polymorphic_parameters
  | "immutable_arrays_experimental" -> Some Immutable_arrays
  | _ -> None

let of_string_exn extn =
  match of_string extn with
  | Some extn -> extn
  | None -> raise (Arg.Bad(Printf.sprintf "Extension %s is not known" extn))

(* We'll do this in a more principled way later *)
let is_erasable = function
  | Local ->
      true
  | Comprehensions
  | Include_functor
  | Polymorphic_parameters
  | Immutable_arrays ->
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

let set extn ~enabled =
  if enabled then begin
    if not (Universe.is_allowed !universe extn) then
      raise (Arg.Bad(Printf.sprintf
        "Cannot %s extension %s: incompatible with %s"
        (if enabled then "enable" else "disable")
        (to_string extn)
        (Universe.compiler_options !universe)));
    if not (List.exists (equal extn) !extensions) then
      extensions := extn :: !extensions
  end else
    extensions :=
      List.filter (fun extn' -> not (equal extn extn')) !extensions

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
