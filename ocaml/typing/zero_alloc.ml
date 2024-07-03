module ZA = Zero_alloc_utils

type const = Builtin_attributes.zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of { strict: bool;
               opt: bool;
               arity: int;
               loc: Location.t;
             }
  | Assume of { strict: bool;
                never_returns_normally: bool;
                never_raises: bool;
                arity: int;
                loc: Location.t;
              }

type desc = { strict : bool; opt : bool }

type var =
  { loc : Location.t;
    arity : int;
    mutable desc : desc option;
    (* None indicates the default case (no check will be done).  If the desc is
       present this var has been constrained by some signature that requires a
       check. *)
  }

type t =
  | Const of const
  | Var of var

(* For backtracking *)
type change = desc option * var
let undo_change (d, v) = v.desc <- d
let log_change = ref (fun _ -> ())
let set_change_log f = log_change := f

let create x = Const x
let create_var loc arity = Var { loc; arity; desc = None }
let default = Const Default_zero_alloc

let get (t : t) =
  match t with
  | Const c -> c
  | Var { loc; arity; desc } ->
    match desc with
    | None -> Default_zero_alloc
    | Some { strict; opt } ->
      Check { loc; arity; strict; opt }

type error =
  | Less_general of { missing_entirely : bool }
  | Arity_mismatch of int * int

exception Error of error

let print_error ppf error =
  let pr fmt = Format.fprintf ppf fmt in
  match error with
  | Less_general { missing_entirely } ->
    pr "The former provides a weaker \"zero_alloc\" guarantee than the latter.";
    if missing_entirely then
      pr "@ Hint: Add a \"zero_alloc\" attribute to the implementation."
  | Arity_mismatch (n1, n2) ->
    pr "zero_alloc arity mismatch:@ \
        When using \"zero_alloc\" in a signature, the syntactic arity of@ \
        the implementation must match the function type in the interface.@ \
        Here the former is %d and the latter is %d."
      n1 n2

let sub_const_const_exn za1 za2 =
  (* The core of the check here is that we translate both attributes into the
     abstract domain and use the existing inclusion check from there, ensuring
     what we do in the typechecker matches the backend.

     There are a few additional details:

     - [opt] is not captured by the abstract domain, so we need a special check
       for it.  But it doesn't interact at all with the abstract domain - it's
       just about whether or not the check happens - so this special check can
       be fully separate.
     - [arity] is also not captured by the abstract domain - it exists only for
       use here, in typechecking.  If the arities do not match, we issue an
       error. It's essential for the soundness of the way we (will, in the next
       PR) use zero_alloc in signatures that the apparent arity of the type in
       the signature matches the syntactic arity of the function.
     - [ignore] can not appear in zero_alloc attributes in signatures, and is
       erased from structure items when computing their signature, so we don't
       need to consider it here.
     *)
  let open Builtin_attributes in
  (* abstract domain check *)
  let abstract_value za =
    match za with
    | Default_zero_alloc | Ignore_assert_all -> ZA.Assume_info.Value.top ()
    | Check { strict; _ } ->
      ZA.Assume_info.Value.of_annotation ~strict ~never_returns_normally:false
        ~never_raises:false
    | Assume { strict; never_returns_normally; never_raises; } ->
      ZA.Assume_info.Value.of_annotation ~strict ~never_returns_normally
        ~never_raises
  in
  let v1 = abstract_value za1 in
  let v2 = abstract_value za2 in
  if not (ZA.Assume_info.Value.lessequal v1 v2) then
    begin let missing_entirely =
        match za1 with
        | Default_zero_alloc -> true
        | Ignore_assert_all | Check _ | Assume _ -> false
      in
      raise (Error (Less_general {missing_entirely}))
    end;
  (* opt check *)
  begin match za1, za2 with
  | Check { opt = opt1; _ }, Check { opt = opt2; _ } ->
    if opt1 && not opt2 then
      raise (Error (Less_general {missing_entirely = false}))
  | (Check _ | Default_zero_alloc | Assume _ | Ignore_assert_all), _ -> ()
  end;
  (* arity check *)
  let get_arity = function
    | Check { arity; _ } | Assume { arity; _ } -> Some arity
    | Default_zero_alloc | Ignore_assert_all -> None
  in
  match get_arity za1, get_arity za2 with
  | Some arity1, Some arity2 ->
    (* Check *)
    if not (arity1 = arity2) then
      raise (Error (Arity_mismatch (arity1, arity2)))
  | Some _, None -> ()
    (* Forgetting zero_alloc info is fine *)
  | None, Some _ ->
    (* Fabricating it is not, but earlier cases should have ruled this out *)
    Misc.fatal_error "Zero_alloc: sub_const_exn"
  | None, None -> ()

let sub_var_const_exn v c =
  (* This can only fail due to an arity mismatch. We have a linear order and can
     always constrain the var lower to make the sub succeed. *)
  match v, c with
  | _, (Default_zero_alloc | Ignore_assert_all | Assume _) -> assert false
  | { arity = arity1; _ }, Check { arity = arity2; _ }
    when arity1 <> arity2 ->
    raise (Error (Arity_mismatch (arity1, arity2)))
  | { desc = None; _ }, Check { strict; opt; _ } ->
    !log_change (None, v);
    v.desc <- Some { strict; opt }
  | { desc = (Some { strict = strict1; opt = opt1 } as desc); _ },
    Check { strict = strict2; opt = opt2 } ->
    let strict = strict1 || strict2 in
    let opt = opt1 && opt2 in
    if strict <> strict1 || opt <> opt1 then begin
      !log_change (desc, v);
      v.desc <- Some { strict; opt }
    end

let sub_exn za1 za2 =
  match za1, za2 with
  | _, Var _ ->
    (* A fully inferred signature will never have a variable in it, so we almost
       never have to constrain by a variable, but there is one special case:

       The typing of modules (e.g., the [Pmod_structure] case of
       [Typemod.type_module_aux]) works by (1) computing a naive signature
       containing every definition in the module, (2) constructing a simplified
       signature that, for example, removes shadowed things, and (3)
       constraining the original signature by the simplfied signature.  These
       signatures _do_ have variables in them, so we allow the special case of
       constraining a variable by itself (which is obviously sound in any
       event).
    *)
    if not (za1 == za2) then
      Misc.fatal_error "zero_alloc: variable constraint"
  | _, Const (Ignore_assert_all | Assume _) ->
    Misc.fatal_error "zero_alloc: invalid constraint"
  | _, (Const Default_zero_alloc) -> ()
  | Var v, Const c -> sub_var_const_exn v c
  | Const c1, Const c2 -> sub_const_const_exn c1 c2
