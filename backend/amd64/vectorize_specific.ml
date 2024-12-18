# 2 "backend/amd64/vectorize_specific.ml"

(* Keep in sync with [Arch.operation_is_pure], [Arch.operation_can_raise],
   [Arch.operation_allocates]. *)
module Memory_access = Vectorize_utils.Memory_access

let memory_access : Arch.specific_operation -> Memory_access.t option =
  fun op ->
  let create ?first_memory_arg_index desc =
    Some (Memory_access.create ?first_memory_arg_index desc)
  in
  match op with
  | Istore_int (_n, addressing_mode, is_assignment) ->
    let desc =
      Memory_access.Write { width_in_bits = W64;
              addressing_mode;
              init_or_assign = if is_assignment then Assignment else Initialization
            }
    in
    create ~first_memory_arg_index:0 desc
  | Ifloatarithmem (float_width, _float_op, addressing_mode) ->
    let width_in_bits : Vectorize_utils.Width_in_bits.t =
      match float_width with
      | Float64 -> W64
      | Float32 -> W32
    in
    let is_mutable =
      (* CR-someday gyorsh: conservative, propagate mutability of Ifloatarithmem from
         selection to make it precise. *)
      true
    in
    let desc =
      Memory_access.Read {
        width_in_bits; addressing_mode; is_mutable; is_atomic = false;
      }
    in
    create ~first_memory_arg_index:1 desc
  | Ioffset_loc (_n, addressing_mode) ->
    let desc =
      Memory_access.Read_and_write { width_in_bits = W64;
                       addressing_mode;
                       is_atomic = false;
                     }
    in
    create desc
  | Iprefetch  { is_write = _; locality = _; addr = _ } ->
    (* Conservative, to prevent reordering anything around this instruction.
       Using [addressing_mode] is tricky because it need not be the start of the
       prefetch cache line and the interval would depend on cache line size. *)
    create Memory_access.Arbitrary
  | Icldemote _
  | Irdtsc
  | Irdpmc
  | Ilfence | Isfence | Imfence | Ipause ->
    (* Conservative, don't reorder around timing or ordering instructions. *)
    create Memory_access.Arbitrary
  | Isimd op ->
    (* Conservative. we don't have any simd operations with memory operations
       at the moment. *)
    if Simd.is_pure op
    then None
    else create Memory_access.Arbitrary
  | Ilea _ | Ibswap _ | Isextend32 | Izextend32 -> None
