(* TEST
 modules = "replace_caml_modify.c";
 {
   flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify_local";
   native;
 }
*)

(* This test verifies that external_ and external64 prevent calls to caml_modify when
   appropriate. This is done by utilizing the --wrap argument to the C compiler, which
   allows us to wrap caml_modify and caml_modify_local. replace_caml_modify.c defines
   these wrappers, which track whether caml_modify/caml_modify_local has been called.

   Note: caml_modify is always called in bytecode, so this test is only performed on
   native *)

external called_caml_modify : unit -> bool = "replace_caml_modify_called_modify" [@@noalloc]
external reset : unit -> unit = "replace_caml_modify_reset" [@@noalloc]

(* Test whether executing f results in caml_modify being called *)
let test ~(call_pos : [%call_pos]) ~expect_caml_modify_call f =
  reset ();
  f ();
  let error =
    match expect_caml_modify_call, called_caml_modify () with
    | true, false -> Some "Expected a call to caml_modify but got none"
    | false, true -> Some "Expected no call to caml_modify but got one"
    | true, true | false, false -> None
  in
  match error with
  | Some error ->
    failwith @@ Format.sprintf "%s on line %d" error call_pos.pos_lnum
  | None -> ()

(* Validate testing technique *)

let () =
  test ~expect_caml_modify_call:true (fun () ->
    let foo = Array.make 1 "hello" in
    foo.(0) <- "world")

let () =
  test ~expect_caml_modify_call:false (fun () ->
    let foo = Array.make 1 0 in
    foo.(0) <- 0)

(* Some type definitions for below tests *)

type 'a boxed_variant = Boxed of 'a
type 'a unboxed_variant = Unboxed of 'a [@@unboxed]
type external_variant : value mod external_ = External
type 'a unboxed_record = { unboxed : 'a } [@@unboxed]
type 'a internal_record = { boxed : 'a }

(* Internal values result in caml_modify calls *)
let () =
  let f (type t) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let expect_caml_modify_call = true in
  test ~expect_caml_modify_call (fun () -> f 10);
  test ~expect_caml_modify_call (fun () -> f "hello");
  test ~expect_caml_modify_call (fun () -> f true);
  test ~expect_caml_modify_call (fun () -> f { boxed = 10 });
  test ~expect_caml_modify_call (fun () -> f { boxed = "hello" });
  test ~expect_caml_modify_call (fun () -> f { unboxed = 10 });
  test ~expect_caml_modify_call (fun () -> f { unboxed = "hello" });
  test ~expect_caml_modify_call (fun () -> f External);
  test ~expect_caml_modify_call (fun () -> f (Boxed 10));
  test ~expect_caml_modify_call (fun () -> f (Boxed "hello"));
  test ~expect_caml_modify_call (fun () -> f (Unboxed 10));
  test ~expect_caml_modify_call (fun () -> f (Unboxed "hello"))

(* External values result in no caml_modify calls *)
let () =
  let f (type (t : value mod external_)) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let expect_caml_modify_call = false in
  test ~expect_caml_modify_call (fun () -> f 10);
  test ~expect_caml_modify_call (fun () -> f true);
  test ~expect_caml_modify_call (fun () -> f { unboxed = 10 });
  test ~expect_caml_modify_call (fun () -> f External);
  test ~expect_caml_modify_call (fun () -> f (Unboxed 10))

(* External64 values result in no caml_modify calls iff the system is 64 bit *)
let () =
  let f (type (t : value mod external_)) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let is_64_bit =
    match Sys.word_size with
    | 64 -> true
    | 32 ->
      (* This case is never excersized because native tests are never run on a 32-bit
         system *)
      false
    | _ -> failwith "Expected word size of 32 or 64"
  in
  let expect_caml_modify_call = not is_64_bit in
  test ~expect_caml_modify_call (fun () -> f 10);
  test ~expect_caml_modify_call (fun () -> f true);
  test ~expect_caml_modify_call (fun () -> f { unboxed = 10 });
  test ~expect_caml_modify_call (fun () -> f External);
  test ~expect_caml_modify_call (fun () -> f (Unboxed 10))
