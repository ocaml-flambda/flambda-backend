let run_test func_name ~test ~validate =
  Gc.minor ();
  let read_fd, write_fd = Unix.pipe () in
  match Unix.fork () with
  | 0 -> (
    match (
      Unix.close read_fd;
      Unix.dup2 write_fd Unix.stderr;
      Unix.dup2 write_fd Unix.stdout;
      test ()
    ) with
    | () -> exit 0
    | exception _ -> exit 1
  )
  | child_pid ->
    (try
       Unix.close write_fd;
       ignore (Unix.waitpid [] child_pid);
       let child_output = Unix.in_channel_of_descr read_fd |> In_channel.input_all in
       validate child_output
     with
     | exn ->
       Printf.eprintf
         "Error encountered while running '%s':\n%s\n\n"
         func_name
         (Printexc.to_string exn))
;;

external alloc : int -> 'a = "ocaml_address_sanitizer_test_alloc" [@@noalloc]
external free : 'a -> unit = "ocaml_address_sanitizer_test_free" [@@noalloc]

let use_after_free_regex = Str.regexp_string "AddressSanitizer: heap-use-after-free"

let out_of_bounds_access_regex =
  Str.regexp_string "AddressSanitizer: heap-buffer-overflow"
;;

let read_regex = Str.regexp "^READ of size "
let write_regex = Str.regexp "^WRITE of size "

let assert_asan_detected_read_use_after_free test_output =
  ignore (Str.search_forward use_after_free_regex test_output 0);
  ignore (Str.search_forward read_regex test_output 0)
;;

let assert_asan_detected_write_use_after_free test_output =
  ignore (Str.search_forward use_after_free_regex test_output 0);
  ignore (Str.search_forward write_regex test_output 0)
;;

let assert_asan_detected_out_of_bounds_read ?(access_size = 8) test_output =
  ignore (Str.search_forward out_of_bounds_access_regex test_output 0);
  let read_regex = Str.regexp ("^READ of size " ^ Int.to_string access_size ^ " ") in
  ignore (Str.search_forward read_regex test_output 0)
;;

let assert_asan_detected_out_of_bounds_write ?(access_size = 8) test_output =
  ignore (Str.search_forward out_of_bounds_access_regex test_output 0);
  let write_regex = Str.regexp ("^WRITE of size " ^ Int.to_string access_size ^ " ") in
  ignore (Str.search_forward write_regex test_output 0)
;;

type t0 = { mutable x : int }

let test_use_after_free_field_get_immediate () =
  let test () =
    let t = alloc 1 in
    t.x <- 0;
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

let test_use_after_free_field_set_immediate () =
  let test () =
    let t = alloc 1 in
    t.x <- 0;
    free t;
    t.x <- 1;
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_write_use_after_free
;;

(* Storing pointers to the OCaml heap in out-of-heap allocations like this is
   not actually safe, but it's fine for the sake of what this test is trying
   to demonstrate. *)
type t1 = { mutable x : int array }

let test_use_after_free_field_get_value () =
  let test () =
    let t = alloc 1 in
    t.x <- [||];
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

let test_use_after_free_field_set_value () =
  let test () =
    let t = alloc 1 in
    t.x <- [||];
    free t;
    t.x <- Sys.opaque_identity [| 1 |];
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_write_use_after_free
;;

type t2 = { mutable x : int64# }

let test_use_after_free_field_get_i64 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0L;
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

let test_use_after_free_field_set_i64 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0L;
    free t;
    t.x <- #1L;
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_write_use_after_free
;;

type t3 = { mutable x : int32# }

let test_use_after_free_field_get_i32 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0l;
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

let test_use_after_free_field_set_i32 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0l;
    free t;
    t.x <- #1l;
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_write_use_after_free
;;

type t4 =
  { mutable x : float
  ; mutable y : int
  }

let test_use_after_free_field_get_float () =
  let test () =
    let t = alloc 2 in
    t.x <- 0.;
    t.y <- 0;
    let _ = Sys.opaque_identity t.y in
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

type t5 = { mutable x : float# }

let test_use_after_free_field_get_f64 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0.;
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

let test_use_after_free_field_set_f64 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0.;
    free t;
    t.x <- #1.;
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_write_use_after_free
;;

type t6 = { mutable x : float32# }

let test_use_after_free_field_get_f32 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0.s;
    free t;
    let _ = Sys.opaque_identity t.x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_read_use_after_free
;;

let test_use_after_free_field_set_f32 () =
  let test () =
    let t = alloc 1 in
    t.x <- #0.s;
    free t;
    t.x <- #1.s;
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_write_use_after_free
;;

let test_valid_accesses_are_unaffected () =
  let test () =
    let t0 : t0 = alloc 1 in
    t0.x <- 0;
    let t1 : t1 = alloc 1 in
    t1.x <- [||];
    let t2 : t2 = alloc 1 in
    t2.x <- #0L;
    let t3 : t3 = alloc 1 in
    t3.x <- #0l;
    let t4 : t4 = alloc 2 in
    t4.x <- 0.0;
    t4.y <- 0;
    let t5 : t5 = alloc 1 in
    t5.x <- #0.0;
    let t6 = alloc 1 in
    t6.x <- #0.0s;
    let _t0_x = Sys.opaque_identity t0.x in
    let _t1_x = Sys.opaque_identity t1.x in
    let _t2_x = Sys.opaque_identity t2.x in
    let _t3_x = Sys.opaque_identity t3.x in
    let _t4_x = Sys.opaque_identity t4.x in
    let _t5_x = Sys.opaque_identity t5.x in
    let _t6_x = Sys.opaque_identity t6.x in
    free t0;
    free t1;
    free t2;
    free t3;
    free t4;
    free t5;
    free t6;
    ()
  in
  run_test __FUNCTION__ ~test ~validate:(fun test_output ->
    assert (String.equal test_output ""))
;;

let test_out_of_bounds_read_int_array () =
  let test () =
    let len = 8 in
    let t : int array = alloc len in
    let x = Array.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_read
;;

let test_out_of_bounds_write_int_array () =
  let test () =
    let len = 8 in
    let t : int array = alloc len in
    let () = Array.unsafe_set t len 0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_write
;;

let test_out_of_bounds_read_obj_array () =
  let test () =
    let len = 8 in
    let t : Obj.t array = alloc len in
    let x = Array.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_read
;;

let test_out_of_bounds_write_obj_array () =
  let test () =
    let len = 8 in
    let t : Obj.t array = alloc len in
    let () = Array.unsafe_set t len (Obj.repr None) in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_write
;;

let test_out_of_bounds_read_float_array () =
  let test () =
    let len = 8 in
    let t : float array = alloc len in
    let x = Array.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_read
;;

let test_out_of_bounds_write_float_array () =
  let test () =
    let len = 8 in
    let t : float array = alloc len in
    let () = Array.unsafe_set t len 0.0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_write
;;

let test_out_of_bounds_read_int_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_read
;;

let test_out_of_bounds_write_int_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_write
;;

let test_out_of_bounds_read_int64_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int64 C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_read
;;

let test_out_of_bounds_write_int64_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int64 C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0L in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_write
;;

let test_out_of_bounds_read_float_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Float64 C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_read
;;

let test_out_of_bounds_write_float_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Float64 C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0.0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test __FUNCTION__ ~test ~validate:assert_asan_detected_out_of_bounds_write
;;

let test_out_of_bounds_read_complex64_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Complex64 C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_read ~access_size:16)
;;

let test_out_of_bounds_write_complex64_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Complex64 C_layout len in
    let () = Bigarray.Array1.unsafe_set t len Complex.zero in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_write ~access_size:16)
;;

let test_out_of_bounds_read_int32_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int32 C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_read ~access_size:4)
;;

let test_out_of_bounds_write_int32_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int32 C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0l in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_write ~access_size:4)
;;

let test_out_of_bounds_read_int16_signed_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int16_signed C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_read ~access_size:2)
;;

let test_out_of_bounds_write_int16_signed_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int16_signed C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_write ~access_size:2)
;;

let test_out_of_bounds_read_int16_unsigned_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int16_unsigned C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_read ~access_size:2)
;;

let test_out_of_bounds_write_int16_unsigned_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int16_unsigned C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_write ~access_size:2)
;;

let test_out_of_bounds_read_int8_signed_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int8_signed C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_read ~access_size:1)
;;

let test_out_of_bounds_write_int8_signed_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int8_signed C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_write ~access_size:1)
;;

let test_out_of_bounds_read_int8_unsigned_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int8_unsigned C_layout len in
    let x = Bigarray.Array1.unsafe_get t len in
    let _ = Sys.opaque_identity x in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_read ~access_size:1)
;;

let test_out_of_bounds_write_int8_unsigned_bigarray () =
  let test () =
    let len = 8 in
    let t = Bigarray.Array1.create Int8_unsigned C_layout len in
    let () = Bigarray.Array1.unsafe_set t len 0 in
    let _ = Sys.opaque_identity t in
    ()
  in
  run_test
    __FUNCTION__
    ~test
    ~validate:(assert_asan_detected_out_of_bounds_write ~access_size:1)
;;

(* Main entry point *)
let () =
  (* We print *something* out regardless of whether or not this test is enabled
     so that if we somehow fail to run this test at all, it will cause a
     visible failure. *)
  print_endline "Possibly running AddressSanitizer tests";
  (* These tests are only enabled when the compiler was built with AddressSanitizer support *)
  let should_run_tests =
    String.equal "true\n" (In_channel.with_open_text Sys.argv.(1) In_channel.input_all)
  in
  if should_run_tests
  then (
    (* Ensure that we aren't producing false-positives *)
    test_valid_accesses_are_unaffected ();
    (* Record use-after-free tests *)
    test_use_after_free_field_get_immediate ();
    test_use_after_free_field_set_immediate ();
    test_use_after_free_field_get_value ();
    test_use_after_free_field_set_value ();
    test_use_after_free_field_get_i64 ();
    test_use_after_free_field_set_i64 ();
    test_use_after_free_field_get_i32 ();
    test_use_after_free_field_set_i32 ();
    test_use_after_free_field_get_float ();
    test_use_after_free_field_get_f64 ();
    test_use_after_free_field_set_f64 ();
    test_use_after_free_field_get_f32 ();
    test_use_after_free_field_set_f32 ();
    (* Out-of-bounds array access tests *)
    test_out_of_bounds_read_int_array ();
    test_out_of_bounds_write_int_array ();
    test_out_of_bounds_read_obj_array ();
    test_out_of_bounds_write_obj_array ();
    test_out_of_bounds_read_float_array ();
    test_out_of_bounds_write_float_array ();
    (* Out-of-bounds bigarray access tests *)
    test_out_of_bounds_read_int_bigarray ();
    test_out_of_bounds_write_int_bigarray ();
    test_out_of_bounds_read_int64_bigarray ();
    test_out_of_bounds_write_int64_bigarray ();
    test_out_of_bounds_read_float_bigarray ();
    test_out_of_bounds_write_float_bigarray ();
    test_out_of_bounds_read_complex64_bigarray ();
    test_out_of_bounds_write_complex64_bigarray ();
    test_out_of_bounds_read_int32_bigarray ();
    test_out_of_bounds_write_int32_bigarray ();
    test_out_of_bounds_read_int16_signed_bigarray ();
    test_out_of_bounds_write_int16_signed_bigarray ();
    test_out_of_bounds_read_int16_unsigned_bigarray ();
    test_out_of_bounds_write_int16_unsigned_bigarray ();
    test_out_of_bounds_read_int8_signed_bigarray ();
    test_out_of_bounds_write_int8_signed_bigarray ();
    test_out_of_bounds_read_int8_unsigned_bigarray ();
    test_out_of_bounds_write_int8_unsigned_bigarray ())
;;
