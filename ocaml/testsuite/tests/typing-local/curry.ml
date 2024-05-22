(* TEST
 {
   reference = "${test_source_directory}/curry.heap.reference";
   bytecode;
 }{
   stack-allocation;
   reference = "${test_source_directory}/curry.stack.reference";
   native;
 }{
   no-stack-allocation;
   reference = "${test_source_directory}/curry.heap.reference";
   native;
 }
*)

module M : sig
  (* explicit signature to force return modes *)
  val part_local : int -> int -> local_ string -> int -> int -> int -> int list
  val all_heap : int -> int -> string -> int -> int -> int -> int list
end = struct
  let part_local a b (local_ c) d e f =
    let _ = c in
    [a; b; d; e; f]
  let all_heap a b c d e f = [a;b;int_of_string c;d;e;f]
end


external is_stack : local_ 'a -> bool = "caml_obj_is_stack"
let loc (local_ x) =
  if is_stack x then 1 else 0

let[@inline never] flocal (local_ arg) =
  let g = M.part_local in
  let a = g 1 in
  let b = a 2 in
  let c = b arg in
  let d = c 4 in
  let e = d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "specialise local"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let _ = flocal "asdf"

let fopaque () =
  let g = Sys.opaque_identity M.part_local in
  let a = g 1 in
  let b = a 2 in
  let c = b (local_ "asdf") in
  let d = c 4 in
  let e = d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "caml_curry local"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let _ = fopaque ()


let[@inline never] fheap () =
  let g = local_ M.all_heap in
  let a = local_ g 1 in
  let b = local_ a 2 in
  let c = local_ b "asdf" in
  let d = local_ c 4 in
  let e = local_ d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "all heap"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let () = fheap ()

(* partial application of a local function *)
let[@inline never] f z =
  let local_ g a b c d e f = [a;b;c;d;e;f;z] in
  let a = g 1 in
  let b = a 2 in
  let c = b 3 in
  let d = c 4 in
  let e = d 5 in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d%d%d\n"
    "all local"
    (loc g) (loc a) (loc b) (loc c) (loc d) (loc e)

let () = f 42

let[@inline never] tupled : (string*string) -> local_ string =
  fun (a,b) -> local_ (a^b)
let () =
  match tupled ("a","b") with
  | "ab" -> Printf.printf "%20s: ok\n" "tupled"
  | _ -> assert false

let globalref = ref 42
type 'a glob = { global_ g : 'a }
let escape x = { g = x }.g
let[@inline never] prim () =
  let r = local_ ref 42 in
  (* currying of primitives (opaque goes via caml_curry) *)
  let a,b,c,d =
    ((Sys.opaque_identity (:=)) r,
     ((:=) r),
     escape ((Sys.opaque_identity (:=)) globalref),
     escape ((:=) globalref)) in
  Gc.minor ();
  Printf.printf "%20s: %d%d%d%d\n" "primcurry"
    (loc a) (loc b) (loc c) (loc d)

let () = prim ()

let curried (local_ x) = { g = fun () -> 42 }
let () =
  let _ = (Sys.opaque_identity curried) (local_ ref 42) in
  Gc.minor ()
