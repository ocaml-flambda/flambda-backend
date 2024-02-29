(** This module implements a validator for register allocation. The algorithm is
    based on a paper by Silvain Rideau and Xavier Leroy titled "Validating
    Register Allocation and Spilling" which can be found here

    [1] https://xavierleroy.org/publi/validation-regalloc.pdf

    The solution is adapted to the different representation of CFG that is used
    in this compiler, including exception handling. Also, the arguments for a
    function call are specified as preassigned registers instead of
    reconstructing the argument locations from the function type. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
include Cfg_intf.S

module Location : sig
  type t

  val of_reg : Reg.t -> t option

  val of_reg_exn : Reg.t -> t

  val of_regs_exn : Reg.t array -> t array

  val to_loc_lossy : t -> Reg.location

  val print : Cmm.machtype_component -> Format.formatter -> t -> unit

  val equal : t -> t -> bool

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end = struct
  module Stack = struct
    (** This type is based on [Reg.stack_location]. The first difference is that
        for [Stack (Local index)] this types additionally stores [stack_class]
        because local stacks are separate for different stack slot classes.
        Secondly for all stacks it stores index in words and not byte offset.
        That gives the guarantee that if indices are different then the
        locations do not overlap. *)
    type t =
      | Local of
          { index : int;
            stack_class : Stack_class.t
          }
      | Incoming of { index : int }
      | Outgoing of { index : int }
      | Domainstate of { index : int }

    let byte_bits = 8

    let word_size =
      (* CR-someday azewierzejew: The current implementation is limited to 64
         bit architecture because then all values are of the same size and
         alignment. With that assumption two stack locations can have
         overlapping ranges of bytes only if the have exactly the same byte
         offset.

         On 32 bit architecture floats are still 64 bits but other values are 32
         bits. Then for [DomainState], [Incoming] and [Outgoing] there can be
         two locations with different offsets that overlap. An example is a slot
         for an integer with offset 4 and a slot for a float with offset 0. The
         ranges they refer to are respectively [4..8] and [0..8] so they overlap
         but the offsets are different. *)
      let word_size = 8 in
      if Sys.word_size <> word_size * byte_bits
      then
        Regalloc_utils.fatal
          "regalloc validation only supports 64 bit architecture, got word \
           size %d"
          Sys.word_size;
      word_size

    let byte_offset_to_word_index offset =
      if offset mod word_size <> 0
      then
        Regalloc_utils.fatal
          "regalloc validation expects aligned offsets, got offset %d with \
           remainder %d"
          offset (offset mod word_size);
      offset / word_size

    let word_index_to_byte_offset index = index * word_size

    let of_stack_loc ~stack_class loc =
      match loc with
      | Reg.Local index -> Local { index; stack_class }
      | Reg.Incoming offset ->
        Incoming { index = byte_offset_to_word_index offset }
      | Reg.Outgoing offset ->
        Outgoing { index = byte_offset_to_word_index offset }
      | Reg.Domainstate offset ->
        Domainstate { index = byte_offset_to_word_index offset }

    let to_stack_loc_lossy t =
      match t with
      | Local { index; _ } -> Reg.Local index
      | Incoming { index } -> Reg.Incoming (word_index_to_byte_offset index)
      | Outgoing { index } -> Reg.Outgoing (word_index_to_byte_offset index)
      | Domainstate { index } ->
        Reg.Domainstate (word_index_to_byte_offset index)
  end

  type t =
    | Reg of int
    | Stack of Stack.t

  let of_reg reg =
    match reg.Reg.loc with
    | Reg.Unknown -> None
    | Reg.Reg idx -> Some (Reg idx)
    | Reg.Stack stack ->
      Some
        (Stack
           (Stack.of_stack_loc
              ~stack_class:(Stack_class.of_machtype reg.Reg.typ)
              stack))

  let of_reg_exn reg = of_reg reg |> Option.get

  let of_regs_exn loc_arr = Array.map of_reg_exn loc_arr

  let to_loc_lossy t =
    match t with
    | Reg idx -> Reg.Reg idx
    | Stack stack -> Reg.Stack (Stack.to_stack_loc_lossy stack)

  let print typ ppf t =
    Printmach.loc ~unknown:(fun _ -> assert false) ppf (to_loc_lossy t) typ

  let compare (t1 : t) (t2 : t) : int =
    (* CR-someday azewierzejew: Implement proper comparison. *)
    Stdlib.compare t1 t2

  let equal (t1 : t) (t2 : t) : bool = compare t1 t2 = 0

  module T = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module Reg_id : sig
  type t =
    | Preassigned of { location : Location.t }
    | Named of { stamp : int }

  val compare : t -> t -> int

  val of_reg : Reg.t -> t

  val to_loc_lossy : t -> Reg.location
end = struct
  type t =
    | Preassigned of { location : Location.t }
    | Named of { stamp : int }

  let of_reg (reg : Reg.t) =
    let loc = Location.of_reg reg in
    if Option.is_some loc <> Reg.is_preassigned reg
    then
      Regalloc_utils.fatal
        "Mismatch between register having location (%b) and register being a \
         preassigned register (%b)"
        (Option.is_some loc) (Reg.is_preassigned reg);
    match loc with
    | Some location -> Preassigned { location }
    | None -> Named { stamp = reg.stamp }

  let to_loc_lossy t =
    match t with
    | Preassigned { location } -> Location.to_loc_lossy location
    | Named _ -> Reg.Unknown

  let compare (t1 : t) (t2 : t) =
    (* CR-someday azewierzejew: Implement proper comparison. *)
    Stdlib.compare t1 t2
end

module Register : sig
  module For_print : sig
    type t
  end

  type t =
    { reg_id : Reg_id.t;
      for_print : For_print.t
    }

  val create : Reg.t -> t

  val to_dummy_reg : t -> Reg.t

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val typ : t -> Cmm.machtype_component

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end = struct
  module For_print = struct
    type t =
      { raw_name : Reg.Raw_name.t;
        stamp : int;
        typ : Cmm.machtype_component
      }
  end

  type t =
    { reg_id : Reg_id.t;
      for_print : For_print.t
    }

  let create (reg : Reg.t) : t =
    { reg_id = Reg_id.of_reg reg;
      for_print = { raw_name = reg.raw_name; stamp = reg.stamp; typ = reg.typ }
    }

  let to_dummy_reg (t : t) : Reg.t =
    { Reg.dummy with
      raw_name = t.for_print.raw_name;
      typ = t.for_print.typ;
      stamp = t.for_print.stamp;
      loc = Reg_id.to_loc_lossy t.reg_id
    }

  let typ (t : t) = t.for_print.typ

  let print (ppf : Format.formatter) (t : t) : unit =
    match t.reg_id with
    | Preassigned { location } ->
      Format.fprintf ppf "R[%a]" (Location.print t.for_print.typ) location
    | Named _ -> Printmach.reg ppf (to_dummy_reg t)

  let compare (t1 : t) (t2 : t) : int = Reg_id.compare t1.reg_id t2.reg_id

  let equal (t1 : t) (t2 : t) : bool = compare t1 t2 = 0

  module T = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module Instruction = struct
  type 'a t =
    { desc : 'a;
      arg : Register.t array;
      res : Register.t array
    }

  module Kind = struct
    type 'a t =
      | Terminator : terminator t
      | Basic : basic t
  end

  let to_prealloc (type a) ~(alloced : a instruction) (t : a t) : a instruction
      =
    { alloced with
      arg = Array.map Register.to_dummy_reg t.arg;
      res = Array.map Register.to_dummy_reg t.res
    }
end

module Description : sig
  (** A snapshot of the [desc], [arg] and [res] fields of all instructions in
      the CFG and [fun_args] of the CFG. It is used by the validator to record
      information about the CFG before register allocation, instead of making a
      deep copy of the (mutable) CFG. The description provides a type-safe and
      quick access to instructions based on their IDs.

      Currently, the validator assumes (without checking) that the register
      allocator does not change the structure of the CFG and does not reorder
      instructions within or between basic blocks.

      The validator checks that the register allocator does not remove
      instructions except Prologue (whenever it's allowed to do so), and does
      not add any new instructions except Spill and Reload. The unique IDs of
      instructions are sufficient to determine this, and the description does
      not need to record the block an instruction belongs to. It is possible to
      reconstruct some information about the CFG structure from the description.
      For example, successors of a block can be reconstructed from the labels
      that appear in terminator's [desc]. It also checks that all [fun_args]
      were preassigned before allocation and that they haven't changed after. *)
  type t

  (** Will return [Some _] for the instructions that existed in the CFG before
      allocation and [None] otherwise. Currently, only instructions that
      register allocation can add are [Spill] and [Reload]. *)
  val find_basic : t -> basic instruction -> basic Instruction.t option

  (** Will return [Some _] for the terminators that existed in CFG before
      allocation and [None] otherwise. Currently, only terminators that register
      allocation can add is [Always]. *)
  val find_terminator :
    t -> terminator instruction -> terminator Instruction.t option

  val create : Cfg_with_layout.t -> t option

  val verify : t -> Cfg_with_layout.t -> unit

  val reg_fun_args : t -> Register.t array
end = struct
  type basic_info =
    { successor_id : int;
      instr : basic Instruction.t
    }

  type t =
    { instructions : (int, basic_info) Hashtbl.t;
      terminators : (int, terminator Instruction.t) Hashtbl.t;
      first_instruction_in_block : int Label.Tbl.t;
      reg_fun_args : Register.t array
    }

  let find_basic t instr =
    Hashtbl.find_opt t.instructions instr.id
    |> Option.map (fun info -> info.instr)

  let find_terminator t instr = Hashtbl.find_opt t.terminators instr.id

  let reg_fun_args t = t.reg_fun_args

  let is_regalloc_specific_basic (desc : Cfg.basic) =
    match desc with Op (Reload | Spill) -> true | _ -> false

  let add_instr_id ~seen_ids ~context id =
    if Hashtbl.mem seen_ids id
    then Regalloc_utils.fatal "Duplicate instruction no. %d while %s" id context;
    Hashtbl.add seen_ids id ()

  let add_basic ~seen_ids ~successor_id t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"adding a basic instruction to the description" id;
    if is_regalloc_specific_basic instr.desc
    then
      Regalloc_utils.fatal
        "Instruction no. %d is specific to the regalloc phase while creating \
         pre-allocation description"
        id;
    Hashtbl.add t.instructions id
      { successor_id;
        instr =
          { Instruction.desc = instr.desc;
            arg = Array.map Register.create instr.arg;
            res = Array.map Register.create instr.res
          }
      }

  let add_terminator ~seen_ids t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"adding a terminator instruction to the description" id;
    Hashtbl.add t.terminators id
      { Instruction.desc = instr.desc;
        arg = Array.map Register.create instr.arg;
        res = Array.map Register.create instr.res
      }

  let do_create cfg =
    Regalloc_invariants.precondition cfg;
    if Lazy.force Regalloc_utils.validator_debug
    then
      (* CR-someday: We don't save the file with [fun_name] in the filename
         because there is an appended stamp that is fragile and is annoying when
         testing. Currently it's not a problem because we abort the build
         whenever register allocation fails but if there was a fallback mode
         then the interesting files would be instantly overwritten. *)
      Cfg_with_layout.save_as_dot ~filename:"before.dot" cfg
        "before_allocation_before_validation";
    let basic_count, terminator_count =
      Cfg_with_layout.fold_instructions cfg
        ~instruction:(fun (basic_count, terminator_count) _ ->
          basic_count + 1, terminator_count)
        ~terminator:(fun (basic_count, terminator_count) _ ->
          basic_count, terminator_count + 1)
        ~init:(0, 0)
    in
    let seen_ids = Hashtbl.create (basic_count + terminator_count) in
    let reg_fun_args =
      (Cfg_with_layout.cfg cfg).fun_args
      |> Array.map (fun reg ->
             let reg = Register.create reg in
             (* Assert that [fun_args] are preassigned. *)
             (match reg.reg_id with
             | Preassigned _ -> ()
             | Named _ ->
               Regalloc_utils.fatal
                 "Register in function arguments that isn't preassigned: %a"
                 Register.print reg);
             reg)
    in
    let t =
      { instructions = Hashtbl.create basic_count;
        terminators = Hashtbl.create terminator_count;
        first_instruction_in_block = Label.Tbl.create terminator_count;
        reg_fun_args
      }
    in
    Label.Tbl.iter
      (fun _ (block : Cfg.basic_block) ->
        add_terminator ~seen_ids t block.terminator;
        let first_instruction_id =
          DLL.fold_right
            ~f:(fun instr successor_id ->
              add_basic ~seen_ids ~successor_id t instr;
              instr.id)
            block.body ~init:block.terminator.id
        in
        Label.Tbl.add t.first_instruction_in_block block.start
          first_instruction_id)
      (Cfg_with_layout.cfg cfg).Cfg.blocks;
    t

  let create cfg =
    match !Flambda_backend_flags.regalloc_validate with
    | false -> None
    | true -> Some (do_create cfg)

  let verify_reg_array ~context ~reg_arr ~loc_arr =
    if Array.length reg_arr <> Array.length loc_arr
    then
      Regalloc_utils.fatal
        "%s: register array length has changed. Before: %d. Now: %d." context
        (Array.length reg_arr) (Array.length loc_arr);
    Array.iter2
      (fun (reg_desc : Register.t) loc_reg ->
        match reg_desc.reg_id, Location.of_reg loc_reg with
        | _, None ->
          Regalloc_utils.fatal "%s: location is still unknown after allocation"
            context
        | Named { stamp = _ }, _ -> ()
        | Preassigned { location = l1 }, Some l2 when Location.equal l1 l2 -> ()
        | Preassigned { location = prev_loc }, Some new_loc ->
          Regalloc_utils.fatal
            "%s: changed preassigned register's location from %a to %a" context
            (Location.print (Register.typ reg_desc))
            prev_loc
            (Location.print loc_reg.Reg.typ)
            new_loc)
      reg_arr loc_arr;
    ()

  let verify_reg_arrays (type a) ~id (instr : a Cfg.instruction)
      (old_instr : a Instruction.t) =
    verify_reg_array
      ~context:(Printf.sprintf "In instruction's no %d arguments" id)
      ~reg_arr:old_instr.arg ~loc_arr:instr.arg;
    verify_reg_array
      ~context:(Printf.sprintf "In instruction's no %d results" id)
      ~reg_arr:old_instr.res ~loc_arr:instr.res

  let verify_basic ~seen_ids ~successor_id t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"checking a basic instruction in the new CFG" id;
    match
      Hashtbl.find_opt t.instructions id, is_regalloc_specific_basic instr.desc
    with
    (* The instruction was present before. *)
    | Some { instr = old_instr; successor_id = old_successor_id }, false ->
      if not (Int.equal old_successor_id successor_id)
      then
        Regalloc_utils.fatal
          "The instruction's no. %d successor id has changed. Before \
           allocation: %d. After allocation (ignoring instructions added by \
           allocation): %d."
          id old_successor_id successor_id;
      (* CR-someday azewierzejew: Avoid using polymrphic compare. *)
      (match instr.desc, old_instr.desc with
      | Op (Name_for_debugger _), Op (Name_for_debugger _) ->
        (* IRC uses `Reg.interf` to represent the adjacency lists for the
           interference graph, which can lead to cycles. *)
        ()
      | _ ->
        if instr.desc <> old_instr.desc
        then
          Regalloc_utils.fatal "The desc of instruction with id %d changed" id);
      verify_reg_arrays ~id instr old_instr;
      (* Return new successor id which is the id of the current instruction. *)
      id
    | None, true ->
      (* Added regalloc specific instruction that wasn't before. The new
         successor is the same as old one because this instruction is
         ignored. *)
      successor_id
    | Some _, true ->
      Regalloc_utils.fatal
        "Register allocation changed existing instruction no. %d into a \
         register allocation specific instruction"
        id
    | None, false -> (
      match instr.desc with
      | Op Move ->
        (* A move instruction, while no regalloc-specific, can be inserted
           because of phi moves in split/rename. *)
        successor_id
      | _ ->
        Regalloc_utils.fatal
          "Register allocation added non-regalloc specific instruction no. %d"
          id)

  let compare_terminators ~successor_ids ~id (old_instr : terminator)
      (instr : terminator) =
    let compare_label l1 l2 =
      let s1 = Label.Tbl.find successor_ids l1 in
      let s2 = Label.Tbl.find successor_ids l2 in
      if not (Int.equal s1 s2)
      then
        Regalloc_utils.fatal
          "When checking equivalence of labels before and after allocation got \
           different successor id's. Successor (label, instr id) before: (%d, \
           %d). Successor (label, instr id) after: (%d, %d)."
          l1 s1 l2 s2
    in
    match old_instr, instr with
    | Never, Never -> ()
    | Always l1, Always l2 -> compare_label l1 l2
    | ( Parity_test { ifso = ifso1; ifnot = ifnot1 },
        Parity_test { ifso = ifso2; ifnot = ifnot2 } ) ->
      compare_label ifso1 ifso2;
      compare_label ifnot1 ifnot2
    | ( Truth_test { ifso = ifso1; ifnot = ifnot1 },
        Truth_test { ifso = ifso2; ifnot = ifnot2 } ) ->
      compare_label ifso1 ifso2;
      compare_label ifnot1 ifnot2
    | ( Float_test { lt = lt1; eq = eq1; gt = gt1; uo = uo1 },
        Float_test { lt = lt2; eq = eq2; gt = gt2; uo = uo2 } ) ->
      compare_label lt1 lt2;
      compare_label eq1 eq2;
      compare_label gt1 gt2;
      compare_label uo1 uo2
    | ( Int_test { lt = lt1; eq = eq1; gt = gt1; is_signed = sign1; imm = imm1 },
        Int_test { lt = lt2; eq = eq2; gt = gt2; is_signed = sign2; imm = imm2 }
      )
      when Bool.equal sign1 sign2 && Option.equal Int.equal imm1 imm2 ->
      compare_label lt1 lt2;
      compare_label eq1 eq2;
      compare_label gt1 gt2
    | Switch labels1, Switch labels2 ->
      Array.iter2 (fun l1 l2 -> compare_label l1 l2) labels1 labels2
    | Return, Return -> ()
    | Raise rk1, Raise rk2
    (* CR-someday azewierzejew: Avoid using polymorphic comparison. *)
      when Stdlib.compare rk1 rk2 = 0 ->
      ()
    | Tailcall_self { destination = l1 }, Tailcall_self { destination = l2 } ->
      compare_label l1 l2
    | Tailcall_func call1, Tailcall_func call2
    (* CR-someday azewierzejew: Avoid using polymorphic comparison. *)
      when Stdlib.compare call1 call2 = 0 ->
      ()
    | Call_no_return call1, Call_no_return call2
    (* CR-someday azewierzejew: Avoid using polymorphic comparison. *)
      when Stdlib.compare call1 call2 = 0 ->
      ()
    | ( Call { op = call1; label_after = l1 },
        Call { op = call2; label_after = l2 } )
    (* CR-someday azewierzejew: Avoid using polymorphic comparison. *)
      when Stdlib.compare call1 call2 = 0 ->
      compare_label l1 l2
    | ( Prim { op = prim1; label_after = l1 },
        Prim { op = prim2; label_after = l2 } )
    (* CR-someday azewierzejew: Avoid using polymorphic comparison. *)
      when Stdlib.compare prim1 prim2 = 0 ->
      compare_label l1 l2
    | ( Specific_can_raise { op = op1; label_after = l1 },
        Specific_can_raise { op = op2; label_after = l2 } )
    (* CR-someday azewierzejew: Avoid using polymorphic comparison. *)
      when Stdlib.compare op1 op2 = 0 ->
      compare_label l1 l2
    | _ ->
      Regalloc_utils.fatal
        "The desc of terminator with id %d changed, before: %a, after: %a." id
        (Cfg.dump_terminator ~sep:", ")
        old_instr
        (Cfg.dump_terminator ~sep:", ")
        instr

  let verify_terminator ~seen_ids ~successor_ids t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"checking a terminator instruction in the new CFG" id;
    match Hashtbl.find_opt t.terminators id with
    (* The instruction was present before. *)
    | Some old_instr ->
      verify_reg_arrays ~id instr old_instr;
      compare_terminators ~successor_ids ~id old_instr.desc instr.desc;
      id
    | None -> (
      match instr.desc with
      | Always successor ->
        (* A terminator added by the register allocator. The successor
           instruction can be found in the next block. *)
        Label.Tbl.find successor_ids successor
      | _ ->
        Regalloc_utils.fatal
          "Register allocation added a terminator no. %d but that's not \
           allowed for this type of terminator: %a"
          id Cfg.print_terminator instr)

  let compute_successor_ids t (cfg : Cfg.t) =
    let visited_labels = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    let successor_ids = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    (* Finds and stores successor id for a given block. *)
    let rec get_id (block : Cfg.basic_block) =
      match Label.Tbl.find_opt successor_ids block.start with
      | Some id -> id
      | None ->
        if Label.Tbl.mem visited_labels block.start
        then
          Misc.fatal_errorf
            "Visiting the same block %d without knowing the successor \
             instruction's id. That means there's a loop consisting of only \
             instructions added by the register allocator."
            block.start;
        Label.Tbl.add visited_labels block.start ();
        let first_id = get_first_non_regalloc_id t block in
        Label.Tbl.add successor_ids block.start first_id;
        first_id
    (* Finds successor id in or after the given block. *)
    and get_first_non_regalloc_id t (block : Cfg.basic_block) =
      let res : Cfg.basic Cfg.instruction option =
        DLL.fold_left
          ~f:(fun acc instr ->
            match acc with
            | Some _ -> acc
            | None ->
              if Hashtbl.mem t.instructions instr.id then Some instr else None)
          block.body ~init:None
      in
      match res with
      | Some instr -> instr.id
      | None -> (
        match
          block.terminator.desc, Hashtbl.mem t.terminators block.terminator.id
        with
        | _, true -> block.terminator.id
        | Always label, false -> get_id (Cfg.get_block_exn cfg label)
        | _, false ->
          Regalloc_utils.fatal
            "Register allocation added a terminator no. %d but that's not \
             allowed for this type of terminator: %a"
            block.terminator.id Cfg.print_terminator block.terminator)
    in
    Label.Tbl.iter
      (fun _ block ->
        (* Force compuatation of the given id. *)
        let (_ : int) = get_id block in
        ())
      cfg.blocks;
    successor_ids

  let verify t cfg =
    Regalloc_invariants.postcondition_layout cfg;
    verify_reg_array ~reg_arr:t.reg_fun_args ~context:"In function arguments"
      ~loc_arr:(Cfg_with_layout.cfg cfg).fun_args;
    let seen_ids =
      Hashtbl.create
        (Hashtbl.length t.instructions + Hashtbl.length t.terminators)
    in
    let successor_ids = compute_successor_ids t (Cfg_with_layout.cfg cfg) in
    Label.Tbl.iter
      (fun _ (block : Cfg.basic_block) ->
        let successor_id =
          verify_terminator ~seen_ids ~successor_ids t block.terminator
        in
        let first_instruction_id =
          DLL.fold_right
            ~f:(fun instr successor_id ->
              verify_basic ~seen_ids ~successor_id t instr)
            block.body ~init:successor_id
        in
        ignore (first_instruction_id : int))
      (Cfg_with_layout.cfg cfg).Cfg.blocks;
    Hashtbl.iter
      (fun id { instr; _ } ->
        let can_be_removed =
          match instr.Instruction.desc with
          | Prologue ->
            let ({ fun_contains_calls; fun_num_stack_slots; _ } : Cfg.t) =
              Cfg_with_layout.cfg cfg
            in
            not
              (Proc.prologue_required ~fun_contains_calls ~fun_num_stack_slots)
          | _ -> false
        in
        if (not (Hashtbl.mem seen_ids id)) && not can_be_removed
        then
          Regalloc_utils.fatal
            "Instruction no. %d was deleted by register allocator" id)
      t.instructions;
    Hashtbl.iter
      (fun id _ ->
        if not (Hashtbl.mem seen_ids id)
        then
          Regalloc_utils.fatal
            "Terminator no. %d was deleted by register allocator" id)
      t.terminators
end

module Equation_set : sig
  (** This corresponds to the set of equations defined in section 3.2 of in the
      paper [1]. The definition is simplified substantially because here none of
      the locations (e.g. registers or stack) are allowed to overlap. *)
  type t

  val empty : t

  val union : t -> t -> t

  val subset : t -> t -> bool

  (** This corresponds to case (10) in Fig. 1 of the paper [1]. *)
  val rename_loc : arg:Location.t -> res:Location.t -> t -> t

  (** This corresponds to case (3) in Fig. 1 of the paper [1]. *)
  val rename_reg : arg:Register.t -> res:Register.t -> t -> t

  (** Calling [remove_result], [verify_destoyed_locations] and [add_argument] in
      this order corresponds to case (7) in Fig. 1 of the paper [1]. This
      implementation is also generalized for all cases not handled by
      [rename_loc] or [rename_reg]. *)
  val remove_result :
    reg_res:Register.t array ->
    loc_res:Location.t array ->
    t ->
    (t, string) Result.t

  val verify_destroyed_locations :
    destroyed:Location.t array -> t -> (t, string) Result.t

  val add_argument :
    reg_arg:Register.t array -> loc_arg:Location.t array -> t -> t

  val is_empty : t -> bool

  val print : Format.formatter -> t -> unit
end = struct
  module Equation = struct
    (** The equations here contain a bit more than the paper [1] has. In the
        said paper the equation is a named variable on one side and location on
        the other side because moving arguments to the correct position before
        call is done implicitly.

        In our representation of CFG before the call [foo %rax] there is
        explicit instruction [%rax := x]. Therefore the left-hand side of the
        equation in our case is either [Named] or [Preassigned]. The equations
        for [Preassigned { location }] are always expected to have exactly
        [location] on the right-hand side.

        It's possible to have very specific code with only no-op moves, spills
        and reloads that breaks the previous assumption but there are multiple
        soft assumptions that prevent existence of such code. Result of
        validation on such code is subject to change and shouldn't be relied
        upon.

        For that reason equations of form [Preassigned { location } = location]
        give us implicitly a set of live preassigned locations. That verifies
        that none of the preassigned locations are destroyed or assigning to the
        preassigned location doesn't destroy a [Named] variable. *)
    type t = Register.t * Location.t

    let print ppf (r, l) =
      Format.fprintf ppf "%a=%a" Register.print r
        (Location.print (Register.typ r))
        l
  end

  exception Verification_failed of string

  type t =
    { for_loc : Register.Set.t Location.Map.t;
      for_reg : Location.Set.t Register.Map.t
    }

  let empty = { for_loc = Location.Map.empty; for_reg = Register.Map.empty }

  let remove ((eq_reg, eq_loc) : Equation.t) t =
    { for_loc =
        Location.Map.update eq_loc
          (function
            | None -> None
            | Some set ->
              let set = Register.Set.remove eq_reg set in
              if Register.Set.is_empty set then None else Some set)
          t.for_loc;
      for_reg =
        Register.Map.update eq_reg
          (function
            | None -> None
            | Some set ->
              let set = Location.Set.remove eq_loc set in
              if Location.Set.is_empty set then None else Some set)
          t.for_reg
    }

  let add ((eq_reg, eq_loc) : Equation.t) t =
    { for_loc =
        Location.Map.update eq_loc
          (fun set ->
            match set with
            | None -> Some (Register.Set.singleton eq_reg)
            | Some set -> Some (Register.Set.add eq_reg set))
          t.for_loc;
      for_reg =
        Register.Map.update eq_reg
          (function
            | None -> Some (Location.Set.singleton eq_loc)
            | Some set -> Some (Location.Set.add eq_loc set))
          t.for_reg
    }

  let is_empty t =
    let loc_res = Location.Map.is_empty t.for_loc in
    let reg_res = Register.Map.is_empty t.for_reg in
    assert (loc_res = reg_res);
    loc_res

  let subset t1 t2 =
    Location.Map.for_all
      (fun loc regs1 ->
        assert (not (Register.Set.is_empty regs1));
        match Location.Map.find_opt loc t2.for_loc with
        | None -> false
        | Some regs2 -> Register.Set.subset regs1 regs2)
      t1.for_loc

  let union t1 t2 =
    { for_loc =
        Location.Map.merge
          (fun _loc regs1 regs2 ->
            match regs1, regs2 with
            | None, None -> None
            | Some r, None | None, Some r -> Some r
            | Some r1, Some r2 -> Some (Register.Set.union r1 r2))
          t1.for_loc t2.for_loc;
      for_reg =
        Register.Map.merge
          (fun _reg locs1 locs2 ->
            match locs1, locs2 with
            | None, None -> None
            | Some l, None | None, Some l -> Some l
            | Some l1, Some l2 -> Some (Location.Set.union l1 l2))
          t1.for_reg t2.for_reg
    }

  let array_fold2 f acc arr1 arr2 =
    let acc = ref acc in
    Array.iter2 (fun v1 v2 -> acc := f !acc v1 v2) arr1 arr2;
    !acc

  let compatible_one ~reg ~loc t =
    let check_equation eq_reg eq_loc =
      (* This check corresponds to simplified check that "(x, l) is compatible
         with E" from chapter 3.2 section "Unsatisfiability and Overlap" from
         the paper [1], where "x" is [reg] and "l" is [loc]. Because we don't
         consider overlap at all, the condition simplifies to [(x' = x && l' =
         l) || (x' <> x && l' <> l)]. *)
      let reg_eq = Register.equal eq_reg reg in
      let loc_eq = Location.equal eq_loc loc in
      if not (Bool.equal reg_eq loc_eq)
      then (
        Format.fprintf Format.str_formatter
          "Unsatisfiable equations when removing result equations.\n\
           Existing equation has to agree on 0 or 2 sides (cannot be exactly \
           1) with the removed equation.\n\
           Existing equation %a.\n\
           Removed equation: %a." Equation.print (eq_reg, eq_loc) Equation.print
          (reg, loc);
        let message = Format.flush_str_formatter () in
        raise (Verification_failed message))
    in
    (* Check equations that have the location the same. *)
    (match Location.Map.find_opt loc t.for_loc with
    | None -> ()
    | Some regs ->
      let eq_loc = loc in
      Register.Set.iter (fun eq_reg -> check_equation eq_reg eq_loc) regs);
    (* Check equations that have the register the same. *)
    (match Register.Map.find_opt reg t.for_reg with
    | None -> ()
    | Some locs ->
      let eq_reg = reg in
      Location.Set.iter (fun eq_loc -> check_equation eq_reg eq_loc) locs);
    ()

  let remove_result ~reg_res ~loc_res t =
    try
      Array.iter2 (fun reg loc -> compatible_one ~reg ~loc t) reg_res loc_res;
      let t =
        array_fold2 (fun t reg loc -> remove (reg, loc) t) t reg_res loc_res
      in
      Ok t
    with Verification_failed message -> Error message

  let verify_destroyed_locations ~destroyed t =
    (* CR azewierzejew for azewierzejew: Add checking stack for stack_location
       other than Local. *)
    try
      Array.iter
        (fun destroyed_loc ->
          match Location.Map.find_opt destroyed_loc t.for_loc with
          | None -> ()
          | Some regs ->
            assert (not (Register.Set.is_empty regs));
            let typ = Register.Set.choose regs |> Register.typ in
            Format.fprintf Format.str_formatter
              "Destroying a location %a in which live registers %a are stored"
              (Location.print typ) destroyed_loc
              (Format.pp_print_seq
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                 Register.print)
              (Register.Set.to_seq regs);
            let message = Format.flush_str_formatter () in
            raise (Verification_failed message))
        destroyed;
      Ok t
    with Verification_failed message -> Error message

  let add_argument ~reg_arg ~loc_arg t =
    array_fold2 (fun t reg loc -> add (reg, loc) t) t reg_arg loc_arg

  let rename_loc ~arg ~res t =
    let regs =
      Location.Map.find_opt res t.for_loc
      |> Option.value ~default:Register.Set.empty
    in
    Register.Set.fold
      (fun reg acc -> acc |> remove (reg, res) |> add (reg, arg))
      regs t

  let rename_reg ~arg ~res t =
    let locs =
      Register.Map.find_opt res t.for_reg
      |> Option.value ~default:Location.Set.empty
    in
    Location.Set.fold
      (fun loc acc -> acc |> remove (res, loc) |> add (arg, loc))
      locs t

  let print ppf t =
    let first = ref true in
    let print_eq eq_reg eq_loc =
      if !first then first := false else Format.fprintf ppf " ";
      Format.fprintf ppf "%a" Equation.print (eq_reg, eq_loc)
    in
    Location.Map.iter
      (fun eq_loc regs ->
        Register.Set.iter (fun eq_reg -> print_eq eq_reg eq_loc) regs)
      t.for_loc
end

module type Description_value = sig
  val description : Description.t
end

let print_reg_as_loc ppf reg =
  Printmach.loc
    ~unknown:(fun ppf -> Format.fprintf ppf "<Unknown>")
    ppf reg.Reg.loc reg.Reg.typ

module Domain : Cfg_dataflow.Domain_S with type t = Equation_set.t = struct
  (** This type corresponds to the set of equations in the dataflow from the
      paper [1]. The value corresponing to "Top" is not represented. The
      transfer function return [(domain, error) result] and the "Top" element
      corresponds to [Error _] for which dataflow is terminated immediately
      without propagating the error value all the way to the entrypoint. A
      side-effect of this is that an erroreous but unreachable code will make
      the validation fail (where the algorithm in [1] will allow such code). *)

  type t = Equation_set.t

  let bot = Equation_set.empty

  let join t1 t2 = Equation_set.union t1 t2

  let less_equal t1 t2 = Equation_set.subset t1 t2
end

module Transfer_error : sig
  type t

  val create :
    Equation_set.t ->
    instr_kind:'a Instruction.Kind.t ->
    exn:Equation_set.t option ->
    reg_instr:'a Instruction.t ->
    loc_instr:'a instruction ->
    string ->
    t

  val print : Format.formatter -> t -> unit
end = struct
  type 'a unpacked =
    { instr_kind : 'a Instruction.Kind.t;
      equations : Equation_set.t;
      exn_equations : Equation_set.t option;
      reg_instr : 'a Instruction.t;
      loc_instr : 'a instruction;
      message : string
    }

  type t = T : _ unpacked -> t

  let create (type a) equations ~(instr_kind : a Instruction.Kind.t) ~exn
      ~(reg_instr : a Instruction.t) ~(loc_instr : a instruction) message =
    T
      { instr_kind;
        message;
        equations;
        reg_instr;
        loc_instr;
        exn_equations = exn
      }

  let print_unpacked (type a) ppf (t : a unpacked) =
    let reg_instr, loc_instr =
      match t.instr_kind with
      | Basic ->
        let t : basic unpacked = t in
        ( `Basic (Instruction.to_prealloc ~alloced:t.loc_instr t.reg_instr),
          `Basic t.loc_instr )
      | Terminator ->
        let t : terminator unpacked = t in
        ( `Terminator (Instruction.to_prealloc ~alloced:t.loc_instr t.reg_instr),
          `Terminator t.loc_instr )
    in
    Format.fprintf ppf "CFG REGALLOC Check failed in instr %d:\n" t.loc_instr.id;
    Format.fprintf ppf "Instruction's description before allocation: %a\n"
      Cfg.print_instruction reg_instr;
    Format.fprintf ppf "Instruction's description after allocation: %a\n"
      (Cfg.print_instruction' ~print_reg:print_reg_as_loc)
      loc_instr;
    Format.fprintf ppf "Message: %s\n" t.message;
    Format.fprintf ppf "Live equations for the normal successor: [%a]\n"
      Equation_set.print t.equations;
    Option.iter
      (fun exn_equations ->
        Format.fprintf ppf
          "Live equations for the exceptional successor: [%a]\n"
          Equation_set.print exn_equations)
      t.exn_equations;
    ()

  let print ppf (T t) = print_unpacked ppf t
end

module Transfer (Desc_val : Description_value) :
  Cfg_dataflow.Backward_transfer
    with type domain = Domain.t
     and type error = Transfer_error.t = struct
  type domain = Domain.t

  type error = Transfer_error.t

  let description = Desc_val.description

  (** This corresponds to case (10) in Fig. 1 of the paper [1]. *)
  let rename_location equations ~loc_instr =
    assert (Array.length loc_instr.arg = 1);
    assert (Array.length loc_instr.res = 1);
    Equation_set.rename_loc
      ~arg:(Location.of_reg_exn loc_instr.arg.(0))
      ~res:(Location.of_reg_exn loc_instr.res.(0))
      equations

  (** This corresponds to case (3) in Fig. 1 of the paper [1]. *)
  let rename_register equations ~(reg_instr : _ Instruction.t) =
    assert (Array.length reg_instr.arg = 1);
    assert (Array.length reg_instr.res = 1);
    Equation_set.rename_reg ~arg:reg_instr.arg.(0) ~res:reg_instr.res.(0)
      equations

  (** For equations coming from exceptional path remove the expected equations. *)
  let remove_exn_bucket equations =
    let phys_reg = Proc.loc_exn_bucket in
    let reg = Register.create phys_reg in
    let loc =
      match reg.reg_id with
      | Preassigned { location } -> location
      | Named _ -> assert false
    in
    Equation_set.remove_result equations ~reg_res:[| reg |] ~loc_res:[| loc |]
    |> Result.map_error (fun message ->
           Printf.sprintf "While removing exn bucket: %s" message)

  (** This corresponds to case (7) in Fig. 1 of the paper [1] generalized for
      all other cases not handled by [rename_location] or [rename_register]. We
      have an additional parameter which is the equations for the exceptional
      path successor which is not present in the paper [1] because exceptions
      are not considered there. *)
  let append_equations (type a) equations ~(instr_kind : a Instruction.Kind.t)
      ~exn ~(reg_instr : a Instruction.t) ~(loc_instr : a instruction)
      ~destroyed =
    let bind f res = Result.bind res f in
    let wrap_error res =
      Result.map_error
        (fun message ->
          Transfer_error.create equations ~instr_kind ~exn ~reg_instr ~loc_instr
            message)
        res
    in
    let exn =
      exn
      |> Option.map (fun exn ->
             (* Handle the exceptional path specific conversions here because in
                [exception_] we don't have enough information in order to give a
                meaningful error message. *)
             exn
             (* Remove the equality for [exn_bucket] if it exists. *)
             |> remove_exn_bucket
             |> wrap_error
             |> bind (fun equations ->
                    (* Verify the destroyed registers for exceptional path
                       only. *)
                    equations
                    |> Equation_set.verify_destroyed_locations
                         ~destroyed:
                           (Location.of_regs_exn (Proc.destroyed_at_raise ()))
                    |> Result.map_error (fun message ->
                           Printf.sprintf
                             "While verifying locations destroyed at raise: %s"
                             message)
                    |> wrap_error))
      (* If instruction can't raise [Option.is_none exn] then use empty set of
         equations as that's the same as skipping the step. *)
      |> Option.value ~default:(Ok Domain.bot)
    in
    equations
    |> (* First remove the result equations. *)
    Equation_set.remove_result ~reg_res:reg_instr.Instruction.res
      ~loc_res:(Location.of_regs_exn loc_instr.res)
    |> wrap_error
    |> bind (fun equations ->
           (* Join the exceptional path equations. *)
           exn
           |> Result.map (fun exn_equations ->
                  Equation_set.union equations exn_equations))
    |> bind (fun equations ->
           (* Verify the destroyed registers (including the exceptional
              path). *)
           Equation_set.verify_destroyed_locations ~destroyed equations
           |> wrap_error)
    |> Result.map (fun equations ->
           (* Add all eqations for the arguments. *)
           Equation_set.add_argument ~reg_arg:reg_instr.Instruction.arg
             ~loc_arg:(Location.of_regs_exn loc_instr.arg)
             equations)

  let basic t instr : (domain, error) result =
    match Description.find_basic description instr with
    | None ->
      (match instr.desc with
      | Op (Spill | Reload | Move) -> ()
      | _ -> assert false);
      Result.ok @@ rename_location t ~loc_instr:instr
    | Some instr_before -> (
      match instr.desc with
      | Op Move
        when Array.length instr.arg = 1
             && Array.length instr.res = 1
             && Reg.same_loc instr.arg.(0) instr.res.(0) ->
        (* This corresponds to a noop move where the source and target registers
           have the same locations. *)
        Result.ok @@ rename_register t ~reg_instr:instr_before
      | _ ->
        append_equations t ~instr_kind:Instruction.Kind.Basic ~exn:None
          ~reg_instr:instr_before ~loc_instr:instr
          ~destroyed:(Proc.destroyed_at_basic instr.desc |> Location.of_regs_exn)
      )

  let terminator t ~exn instr =
    match Description.find_terminator description instr with
    | Some instr_before ->
      (* CR-soon azewierzejew: This is kind of fragile for [Tailcall (Self _)]
         because that instruction doesn't strictly adhere to generic
         semantics. *)
      let exn =
        if Cfg.can_raise_terminator instr.desc then Some exn else None
      in
      append_equations t ~instr_kind:Instruction.Kind.Terminator ~exn
        ~reg_instr:instr_before ~loc_instr:instr
        ~destroyed:
          (Proc.destroyed_at_terminator instr.desc |> Location.of_regs_exn)
    | None -> (
      Result.ok
      @@
      match instr.desc with
      | Always _ ->
        (* [Always] is always an identity. *)
        assert (not (Cfg.can_raise_terminator instr.desc));
        assert (Array.length instr.arg = 0);
        assert (Array.length instr.res = 0);
        t
      | _ ->
        Regalloc_utils.fatal
          "Register allocation added a terminator no. %d but that's not \
           allowed for this type of terminator: %a"
          instr.id Cfg.print_terminator instr)

  (* This should remove the equations for the exception value, but we do that in
     [Domain.append_equations] because there we have more information to give if
     there's an error. *)
  let exception_ t = Ok t
end

module Check_backwards (Desc_val : Description_value) =
  Cfg_dataflow.Backward (Domain) (Transfer (Desc_val))

let save_as_dot_with_equations ~desc ~res_instr ~res_block ?filename cfg msg =
  Cfg_with_layout.save_as_dot
    ~annotate_instr:
      [ (fun ppf instr ->
          let id =
            match instr with
            | `Basic instr -> instr.id
            | `Terminator instr -> instr.id
          in
          Cfg_dataflow.Instr.Tbl.find_opt res_instr id
          |> Format.pp_print_option
               ~none:(fun ppf () -> Format.fprintf ppf "Unknown")
               Equation_set.print ppf);
        Cfg.print_instruction' ~print_reg:print_reg_as_loc;
        (fun ppf instr ->
          let print printer find instr =
            match find desc instr with
            | Some prev_instr ->
              let instr = Instruction.to_prealloc ~alloced:instr prev_instr in
              printer ppf instr
            | None -> ()
          in
          match instr with
          | `Basic instr -> print Cfg.print_basic Description.find_basic instr
          | `Terminator ti ->
            print Cfg.print_terminator Description.find_terminator ti) ]
    ~annotate_block_end:(fun ppf block ->
      Label.Tbl.find_opt res_block block.start
      |> Format.pp_print_option
           ~none:(fun ppf () -> Format.fprintf ppf "Unknown")
           Equation_set.print ppf)
    ?filename cfg msg;
  ()

module Error : sig
  module At_entrypoint : sig
    type t =
      { message : string;
        equations : Equation_set.t;
        reg_fun_args : Register.t array;
        loc_fun_args : Location.t array
      }
  end

  module Source : sig
    type t =
      | At_instruction of Transfer_error.t
      | At_entrypoint of At_entrypoint.t
  end

  type t =
    { source : Source.t;
      res_instr : Domain.t Cfg_dataflow.Instr.Tbl.t;
      res_block : Domain.t Label.Tbl.t;
      desc : Description.t;
      cfg : Cfg_with_layout.t
    }

  val dump : Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end = struct
  module At_entrypoint = struct
    type t =
      { message : string;
        equations : Equation_set.t;
        reg_fun_args : Register.t array;
        loc_fun_args : Location.t array
      }
  end

  module Source = struct
    type t =
      | At_instruction of Transfer_error.t
      | At_entrypoint of At_entrypoint.t

    let print (ppf : Format.formatter) (t : t) : unit =
      match t with
      | At_instruction error -> Transfer_error.print ppf error
      | At_entrypoint { message; equations; reg_fun_args; loc_fun_args } ->
        Format.fprintf ppf "Bad equations at entry point, reason: %s\n" message;
        Format.fprintf ppf "Equations: %a\n" Equation_set.print equations;
        Format.fprintf ppf "Function argument descriptions: %a\n"
          (Format.pp_print_seq
             ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
             Register.print)
          (Array.to_seq reg_fun_args);
        Format.fprintf ppf "Function argument locations: %a\n"
          (Format.pp_print_seq
             ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
             (fun ppf (reg, loc) -> Location.print (Register.typ reg) ppf loc))
          (Array.to_seq (Array.combine reg_fun_args loc_fun_args));
        ()
  end

  type t =
    { source : Source.t;
      res_instr : Domain.t Cfg_dataflow.Instr.Tbl.t;
      res_block : Domain.t Label.Tbl.t;
      desc : Description.t;
      cfg : Cfg_with_layout.t
    }

  let print (ppf : Format.formatter) ({ source; _ } : t) : unit =
    Source.print ppf source

  let dump (ppf : Format.formatter)
      ({ source; res_instr; res_block; desc; cfg } : t) : unit =
    Source.print ppf source;
    let filename =
      Filename.temp_file
        (X86_proc.string_of_symbol "" (Cfg_with_layout.cfg cfg).fun_name ^ "_")
        ".dot"
    in
    Format.fprintf ppf "Dumping cfg into %s ...\n" filename;
    save_as_dot_with_equations ~desc ~res_instr ~res_block ~filename cfg
      "validation_error";
    Format.fprintf ppf "Dumped cfg into: %s\n" filename;
    ()
end

let verify_entrypoint (equations : Equation_set.t) (desc : Description.t)
    (cfg : Cfg_with_layout.t) :
    (Cfg_with_layout.t, Error.At_entrypoint.t) Result.t =
  let reg_fun_args = Description.reg_fun_args desc in
  let loc_fun_args = Location.of_regs_exn (Cfg_with_layout.cfg cfg).fun_args in
  let bind f r = Result.bind r f in
  Equation_set.remove_result ~reg_res:reg_fun_args ~loc_res:loc_fun_args
    equations
  |> bind (fun equations ->
         (* This check is stronger than the one in the paper [1]. That because C
            allows to start with uninitialized variables as it's explained in
            chapter 3.2 and of section "Dataflow Analysis and Its Uses". Such a
            thing is not allowed in OCaml. Therefore after removing all
            equations for arguments the should be no additional equations
            left. *)
         if Equation_set.is_empty equations
         then Ok cfg
         else (
           Format.fprintf Format.str_formatter
             "Some equations still present at entrypoint after removing \
              parameter equations: [%a]"
             Equation_set.print equations;
           let message = Format.flush_str_formatter () in
           Error message))
  |> Result.map_error (fun message : Error.At_entrypoint.t ->
         { message; equations; reg_fun_args; loc_fun_args })

let test (desc : Description.t) (cfg : Cfg_with_layout.t) :
    (Cfg_with_layout.t, Error.t) Result.t =
  if Lazy.force Regalloc_utils.validator_debug
  then
    (* CR-someday: We don't save the file with [fun_name] in the filename
       because there is an appended stamp that is fragile and is annoying when
       testing. Currently it's not a problem because we abort the build whenever
       register allocation fails but if there was a fallback mode then the
       interesting files would be instantly overwritten. *)
    Cfg_with_layout.save_as_dot
      ~annotate_instr:[Cfg.print_instruction' ~print_reg:print_reg_as_loc]
      ~filename:"after.dot" cfg "after_allocation_before_validation";
  Description.verify desc cfg;
  let module Check_backwards = Check_backwards (struct
    let description = desc
  end) in
  let res_instr, res_block, result =
    match
      Check_backwards.run (Cfg_with_layout.cfg cfg) ~init:Domain.bot
        ~map:Check_backwards.Both ()
    with
    | Ok (res_instr, res_block) -> res_instr, res_block, Ok ()
    | Aborted ((res_instr, res_block), error) ->
      res_instr, res_block, Error error
    | Max_iterations_reached ->
      Regalloc_utils.fatal
        "Unable to compute validation equation sets from CFG for function %s@."
        (Cfg_with_layout.cfg cfg).fun_name
  in
  if Lazy.force Regalloc_utils.validator_debug
  then
    (* CR-someday: We don't save the file with [fun_name] in the filename
       because there is an appended stamp that is fragile and is annoying when
       testing. Currently it's not a problem because we abort the build whenever
       register allocation fails but if there was a fallback mode then the
       interesting files would be instantly overwritten. *)
    save_as_dot_with_equations ~desc ~res_instr ~res_block ~filename:"annot.dot"
      cfg "after_allocation_after_validation";
  match result with
  | Ok () ->
    let entrypoint_equations =
      let cfg = Cfg_with_layout.cfg cfg in
      let entry_block = Cfg.entry_label cfg |> Cfg.get_block_exn cfg in
      let entry_id = Cfg.first_instruction_id entry_block in
      Cfg_dataflow.Instr.Tbl.find res_instr entry_id
    in
    verify_entrypoint entrypoint_equations desc cfg
    |> Result.map_error (fun (error : Error.At_entrypoint.t) : Error.t ->
           { source = At_entrypoint error; res_instr; res_block; desc; cfg })
  | Error error ->
    Error { source = At_instruction error; res_instr; res_block; desc; cfg }

let run desc cfg =
  match desc with
  | None -> cfg
  | Some desc -> (
    match test desc cfg with
    | Ok cfg -> cfg
    | Error error -> Regalloc_utils.fatal "%a%!" Error.dump error)
