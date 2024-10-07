open Cmm

type memory_access =
  | Load
  | Store

let max_supported_log2size = 4

let is_enabled = ref Config.with_address_sanitizer

let command_line_options =
  [ ( "-fno-asan",
      Arg.Clear is_enabled,
      "Disable AddressSanitizer. This is only meaningful if the compiler was \
       built with AddressSanitizer support enabled." ) ]

(* Checking [Config.with_address_sanitizer] is redundant, but we do it because
   it's a compile-time constant, so it enables the compiler to completely
   optimize-out the AddressSanitizer code when the compiler was configured
   without it. *)
let[@inline always] is_enabled () = Config.with_address_sanitizer && !is_enabled

let[@inline always] asan_report_extcall func =
  Cextcall
    { func;
      ty = typ_void;
      builtin = false;
      returns = true;
      effects = Arbitrary_effects;
      coeffects = Has_coeffects;
      alloc = false;
      ty_args = []
    }

let asan_report_extcall memory_access log2size =
  let index =
    (log2size lsl 1) + match memory_access with Load -> 0 | Store -> 1
  in
  (* [Cextcall] values are relatively heavyweight, and when ASAN is enabled we
     emit *a lot* of them, so we take extra care to structure our code such that
     they are statically allocated as manifest constants in a flat array. *)
  match index with
  | 0 -> asan_report_extcall "caml_asan_report_load1_noabort"
  | 1 -> asan_report_extcall "caml_asan_report_store1_noabort"
  | 2 -> asan_report_extcall "caml_asan_report_load2_noabort"
  | 3 -> asan_report_extcall "caml_asan_report_store2_noabort"
  | 4 -> asan_report_extcall "caml_asan_report_load4_noabort"
  | 5 -> asan_report_extcall "caml_asan_report_store4_noabort"
  | 6 -> asan_report_extcall "caml_asan_report_load8_noabort"
  | 7 -> asan_report_extcall "caml_asan_report_store8_noabort"
  | 8 -> asan_report_extcall "caml_asan_report_load16_noabort"
  | 9 -> asan_report_extcall "caml_asan_report_store16_noabort"
  | _ ->
    (* Larger loads and stores can be reported using
       [__asan_report_load_n_noabort], but we don't support this yet. *)
    assert false

let slow_path_check memory_access ~log2size ~field_address ~shadow_value dbg =
  let asan_report_invalid_access =
    Cmm.Csequence
      ( Cop (asan_report_extcall memory_access log2size, [field_address], dbg),
        Cconst_int (1, dbg) )
  in
  match log2size with
  | 3 | 4 ->
    (* There is no slow-path check for word-sized and larger accesses *)
    asan_report_invalid_access
  | 0 | 1 | 2 ->
    let access_size = 1 lsl log2size in
    let last_accessed_byte =
      Cop
        ( Caddi,
          [ Cconst_int (access_size - 1, dbg);
            Cop (Cand, [field_address; Cconst_int (7, dbg)], dbg) ],
          dbg )
    in
    let is_valid = Cop (Ccmpi Clt, [last_accessed_byte; shadow_value], dbg) in
    Cifthenelse
      ( is_valid,
        dbg,
        Cconst_int (1, dbg),
        dbg,
        asan_report_invalid_access,
        dbg,
        Any )
  | log2size ->
    Misc.fatal_errorf
      "Attempted to instrument a %s of size %d bytes with AddressSanitizer, \
       but this is not supported"
      (match memory_access with Load -> "load" | Store -> "store")
      (1 lsl log2size)

(** Implements [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm#mapping]. *)
let check memory_access ~log2size field_address dbg =
  let field_address_tmp_var = Backend_var.create_local "field_address_tmp" in
  Clet
    ( Backend_var.With_provenance.create field_address_tmp_var,
      field_address,
      let field_address = Cvar field_address_tmp_var in
      let shadow_value =
        let shadow_address =
          Cop
            (* Shadow memory is by definition outside of the normal heap, hence
               why this is unconditionally a [Caddi]. *)
            ( Caddi
              (* These constants come from
                 [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm#64-bit]. *),
              [ Cconst_int (0x7FFF8000, dbg);
                Cop (Clsr, [field_address; Cconst_int (3, dbg)], dbg) ],
              dbg )
        in
        Cop
          ( Cload
              { memory_chunk = Byte_signed;
                mutability = Mutable;
                is_atomic = false
              },
            [shadow_address],
            dbg )
      in
      let shadow_value_tmp_var = Backend_var.create_local "shadow_value_tmp" in
      Clet
        ( Backend_var.With_provenance.create shadow_value_tmp_var,
          shadow_value,
          let shadow_value = Cvar shadow_value_tmp_var in
          let is_valid =
            Cop (Ccmpi Ceq, [shadow_value; Cconst_int (0, dbg)], dbg)
          in
          Cifthenelse
            ( is_valid,
              dbg,
              Cconst_int (1, dbg),
              dbg,
              slow_path_check memory_access ~log2size ~field_address
                ~shadow_value dbg,
              dbg,
              Any ) ) )
