(* TEST
   modules = "cstubs.c"
   include ocamlcommon
   * native *)

external local_stack_offset : unit -> int = "caml_local_stack_offset"
external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
let[@inline never] ignore_local (local_ x) = let _ = opaque_identity x in ()
let last_offset = ref 0

let check_empty name =
  let offs = local_stack_offset () in
  if offs <> !last_offset then begin
    Printf.printf "%25s: %d bytes leaked\n%!" name (offs - !last_offset)
  end else begin
    Printf.printf "%25s: OK\n%!" name
  end;
  last_offset := offs

let check_not_empty name =
  let offs = local_stack_offset ()in
  if offs <> !last_offset then begin
    Printf.printf "%25s: OK\n%!" name
  end else begin
    Printf.printf "%25s: not leaking while expected so\n%!" name
  end;
  last_offset := offs

let () = check_empty "startup"

let allocate_in_current_region x = local_
  ignore_local (Some x);
  ()

let leak_in_current_region : 'a -> unit = Obj.magic allocate_in_current_region

external make_dumb_external_block : unit -> int = "make_dumb_external_block"
external follow : int -> ('a [@local_opt]) = "%int_as_pointer"

let ext : int = make_dumb_external_block ()

let[@inline never] int_as_pointer_local x =
  leak_in_current_region x;
  (* The region should be preserved by the following call; hence no stack space
     leaking *)
  let _ = opaque_identity (follow ext) in
  ()

let[@inline never] int_as_pointer_global x =
  (* The current function region will be eliminated, because the following two
     function calls don't allocate on the region (superficially). The first call
     secretly does, hence stack space leaking. *)
  leak_in_current_region x;
  let _ = Sys.opaque_identity (follow ext) in
  ()

let () =
  int_as_pointer_global 42;
  check_not_empty "int_as_pointer (global)"

let () =
  int_as_pointer_local 42;
  check_empty "int_as_pointer (local)"

let[@inline never] uses_local x =
  let local_ r = ref x in
  let _ = opaque_identity r in
  ()
let () =
  uses_local 42;
  check_empty "function call"

let[@inline never] uses_exclave x =
  let local_ r = ref x in
  let _ = opaque_identity r in
  [%exclave] (
    check_empty "cleanup upon exclave"
  )
let () =
  uses_exclave 42


let[@inline never] uses_local_try x =
  try
    let r = local_ ref x in
    if opaque_identity false then raise Exit;
    let _ = opaque_identity r in
    ()
  with Exit -> ()
let () =
  uses_local_try 42;
  check_empty "exn function call"

let[@inline never][@specialise never][@local never] do_tailcall f =
  let local_ r = ref 42 in
  let _ = opaque_identity r in
  f ()
let () =
  do_tailcall (fun () -> check_empty "during indirect tailcall");
  check_empty "after indirect tailcall"


let[@inline always] tailcalled_function () =
  check_empty "during direct tailcall"
let[@inline never] do_direct_tailcall () =
  let local_ r = ref 42 in
  let _ = opaque_identity r in
  tailcalled_function ()
let () =
  do_direct_tailcall ();
  check_empty "after direct tailcall"


let[@inline never][@specialise never][@local never] do_overtailcall f =
  let local_ r = ref 42 in
  let _ = opaque_identity r in
  f () ()
let () =
  do_overtailcall (fun () ->
    let local_ r = ref 42 in
    let _ = opaque_identity r in
    fun () ->
      check_empty "during indirect overtail");
  check_empty "after indirect overtail"

let[@inline always] overtailcalled_function () =
  let local_ r = ref 42 in
  let _ = opaque_identity r in
  fun () ->
    check_empty "during direct overtail"
let[@inline never] do_direct_overtailcall () =
  let local_ r = ref 42 in
  let _ = opaque_identity r in
  overtailcalled_function () ()
let () =
  do_direct_overtailcall ();
  check_empty "after direct overtail"


let[@inline always] do_inlined_tailcall g =
  let local_ r = ref 42 in
  let _ = opaque_identity r in
  g ()

let () =
  do_inlined_tailcall (fun () ->
    check_empty "during inlined tailcall");
  check_empty "after inlined tailcall"


let[@inline never] local_ret a b = local_ ref (a + b)
let[@inline never] calls_local_ret () =
  let local_ r = (local_ret 1) 2 in
  let () = ignore_local r in
  ()
let () =
  calls_local_ret ();
  check_empty "apply merging"


let () =
  let local_ z = ref 1000 in
  let _ = opaque_identity z in
  ()
let () = check_empty "toplevel binding"


let rec foo = 1 :: bar
and bar =
  let local_ z = ref 1000 in
  let _ = opaque_identity z in
  1 :: foo
let () = check_empty "toplevel rec binding"


;;
(let local_ z = ref 1000 in
 let _ = opaque_identity z in
 ());;
let () = check_empty "toplevel eval"

let () =
  let f x b g =
    let local_ p = x, x in
    if b then () else (g p; ())
  in f 0 true (fun _ -> ())
let () = check_empty "constant switch arm"

module type T = sig val x : int end
let _ =
  let module M : T =
    (val (let local_ r = ref 42 in
          let _ = opaque_identity r in
          ((module struct let x = !r + 1 end) : (module T)))) in
  M.x
let () = check_empty "first class mod"

class d x =
  let z =
    let r = local_ ref 1000 in
    let _ = opaque_identity r in
    !r + 1
  in
  object
    val p =
      let r = opaque_identity (local_ ref 42) in
      !r
    initializer
      let r = local_ ref 42 in
      let _ = opaque_identity r in
      ()
    method getd =
      z + p + x
  end

let () = check_empty "class d definition"

class c =
  let z =
    let r = local_ ref 1000 in
    let _ = opaque_identity r in
    !r + 1
  in
  object
    initializer
      let r = local_ ref 42 in
      let _ = opaque_identity r in
      ()
    val q =
      let r = opaque_identity (local_ ref 42) in
      !r
    inherit d (
      let r = local_ ref 42 in
      let _ = opaque_identity r in
      42)
    method getc =
      z + p + q
  end

class e = d (
  let r = local_ ref 42 in
  let _ = opaque_identity r in
  !r)

let () = check_empty "class definitions"

let o1 = new c
let o2 = new d 42
let o3 = new e
let () = check_empty "class instantiation"


let glob = ref 0
let[@inline never] local_fn_ret () s = local_
  incr glob;
  fun x -> Gc.minor (); string_of_int x ^ s

let globstr = ref ""
let unknown_fn = ref local_fn_ret
let gpart_fn = ref (local_fn_ret ())
let obj = ref (object
  method local_ret s = local_
    incr glob;
    fun x -> Gc.minor (); string_of_int x ^ s
  end)
let check s =
  globstr := s; assert (s = "5!")
let () =
  check (local_fn_ret () "!" 5);
  check_empty "static overapply";
  check (!unknown_fn () "!" 5);
  check_empty "dynamic overapply";
  let part_fn = local_fn_ret () in
  check (part_fn "!" 5);
  check_empty "static/partial overapply";
  gpart_fn := local_fn_ret ();
  check (!gpart_fn "!" 5);
  check_empty "dynamic/partial overapply";
  check (!obj#local_ret "!" 5);
  check_empty "method overapply"

type t = { x : int } [@@unboxed]
let[@inline never] create_local () =
  let local_ _extra = opaque_identity (Some (opaque_identity ())) in
  local_ { x = opaque_identity 0 }
let create_and_ignore () =
  let x = create_local () in
  ignore (Sys.opaque_identity x : t)

let () =
  create_and_ignore ();
  check_empty "mode-crossed region"

let[@inline never] allocate_in_exclave a =
  (* This needs to be a [while] so that we're allowed to have an [exclave_] *)
  while
    let pair = local_ opaque_identity (a, a) in
    let b, _ = pair in
    let b : int = b in
    exclave_ (
      (* This should allocate in the function's region - in particular, the
         function needs to have one *)
      let pair = local_ opaque_identity (b, b) in
      let c, _ = pair in
      c = 42)
  do
    ()
  done

let () =
  let () =
    (* Temporarily disable this test for flambda1 until it's fixed there *)
    if not Config.flambda then
      allocate_in_exclave 1
  in
  (* If [allocate_in_exclave] had its region elided, the allocation will have
     happened in our region instead *)
  check_empty "allocation from exclave"

let () = Gc.compact ()
