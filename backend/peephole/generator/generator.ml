[@@@ocaml.warning "+a-30-40-41-42"]

module Operand : sig
  type t

  val make_ignored : unit -> t

  val make_register : name:string -> t

  val make_int_imm : name:string -> t

  val get_name_exn : t -> string

  val is_register : t -> bool

  val is_int_imm : t -> bool
end = struct
  type t =
    | Ignored
    | Register of string
    | Int_imm of string

  let make_ignored () = Ignored

  let make_register ~name = Register name

  let make_int_imm ~name = Int_imm name

  let get_name_exn = function
    | Ignored -> assert false
    | Register name | Int_imm name -> name

  let is_register = function Register _ -> true | Int_imm _ | Ignored -> false

  let is_int_imm = function Int_imm _ -> true | Register _ | Ignored -> false
end

module Operator : sig
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Asr

  val to_string : t -> string

  val to_ocaml_op : t -> string
end = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Asr

  let to_string = function
    | Add -> "Mach.Iadd"
    | Sub -> "Mach.Isub"
    | Mul -> "Mach.Imul"
    | Div -> "Mach.Idiv"
    | Mod -> "Mach.Imod"
    | And -> "Mach.Iand"
    | Or -> "Mach.Ior"
    | Xor -> "Mach.Ixor"
    | Shl -> "Mach.Ilsl"
    | Shr -> "Mach.Ilsr"
    | Asr -> "Mach.Iasr"

  let to_ocaml_op = function
    | Add -> "( + )"
    | Sub -> "( - )"
    | Mul -> "( * )"
    | Div -> "( / )"
    | Mod -> "( mod )"
    | And -> "( land )"
    | Or -> "( lor )"
    | Xor -> "( lxor )"
    | Shl -> "( lsl )"
    | Shr -> "( lsr )"
    | Asr -> "( lsr )"
end

module Instruction : sig
  type t

  val make_move : fst:string -> snd:string -> t

  val make_int_operation :
    fst:string -> snd:string -> imm:string -> op:Operator.t -> t

  val get_match : t -> string

  val get_fst : t -> Operand.t option

  val get_snd : t -> Operand.t option

  val get_imm : t -> Operand.t option

  val is_move : t -> bool
end = struct
  type t =
    | Move of
        { fst : Operand.t;
          snd : Operand.t
        }
    | Intop_imm of
        { fst : Operand.t;
          snd : Operand.t;
          imm : Operand.t;
          op : Operator.t
        }

  let make_move ~fst ~snd =
    Move
      { fst = Operand.make_register ~name:fst;
        snd = Operand.make_register ~name:snd
      }

  let make_int_operation ~fst ~snd ~imm ~op =
    Intop_imm
      { fst = Operand.make_register ~name:fst;
        snd = Operand.make_register ~name:snd;
        imm = Operand.make_int_imm ~name:imm;
        op
      }

  let get_match = function
    | Move _ -> "Op (Move | Spill | Reload)"
    | Intop_imm { fst = _; snd = _; imm; op } ->
      String.concat " "
        [ "Op (Intop_imm (";
          Operator.to_string op ^ ",";
          Operand.get_name_exn imm;
          "))" ]

  let get_fst = function
    | Move { fst; snd = _ } -> Some fst
    | Intop_imm { fst; snd = _; imm = _; op = _ } -> Some fst

  let get_snd = function
    | Move { fst = _; snd } -> Some snd
    | Intop_imm { fst = _; snd; imm = _; op = _ } -> Some snd

  let get_imm = function
    | Move _ -> None
    | Intop_imm { fst = _; snd = _; imm; op = _ } -> Some imm

  let is_move = function Move _ -> true | Intop_imm _ -> false
end

module Condition : sig
  type t

  val make_are_equal : fst:string -> snd:string -> t

  val make_no_add_overflow : fst:string -> snd:string -> t

  val make_no_sub_overflow : fst:string -> snd:string -> t

  val make_no_mul_overflow : fst:string -> snd:string -> t

  val make_no_shift_overflow : fst:string -> snd:string -> t

  val make_no_bitwise_overflow_assert :
    fst:string -> snd:string -> op:Operator.t -> t

  val make_is_lsl_imm32 : fst:string -> t

  val combine_all : t list -> t

  val combine_any : t list -> t

  val to_string : t -> string
end = struct
  type t =
    | Are_equal of
        { fst : string;
          snd : string
        }
    | No_add_overflow of
        { fst : string;
          snd : string
        }
    | No_sub_overflow of
        { fst : string;
          snd : string
        }
    | No_mul_overflow of
        { fst : string;
          snd : string
        }
    | No_shift_overflow of
        { fst : string;
          snd : string
        }
    | No_bitwise_overflow_assert of
        { fst : string;
          snd : string;
          op : Operator.t
        }
    | Is_lsl_imm32 of string
    | And of t list
    | Or of t list

  let make_are_equal ~fst ~snd = Are_equal { fst; snd }

  let make_no_add_overflow ~fst ~snd = No_add_overflow { fst; snd }

  let make_no_sub_overflow ~fst ~snd = No_sub_overflow { fst; snd }

  let make_no_mul_overflow ~fst ~snd = No_mul_overflow { fst; snd }

  let make_no_shift_overflow ~fst ~snd = No_shift_overflow { fst; snd }

  let make_no_bitwise_overflow_assert ~fst ~snd ~op =
    No_bitwise_overflow_assert { fst; snd; op }

  let make_is_lsl_imm32 ~fst = Is_lsl_imm32 fst

  let combine_all t_lst = And t_lst

  let combine_any t_lst = Or t_lst

  let rec to_string = function
    | Are_equal { fst; snd } -> String.concat " " ["are_equal_regs"; fst; snd]
    | No_add_overflow { fst; snd } ->
      String.concat " "
        [ "Misc.no_overflow_add";
          fst;
          snd;
          "&& no_32_bit_overflow";
          fst;
          snd;
          "( + )" ]
    | No_sub_overflow { fst; snd } ->
      String.concat " "
        [ "Misc.no_overflow_sub";
          fst;
          snd;
          "&& no_32_bit_overflow";
          fst;
          snd;
          "( - )" ]
    | No_mul_overflow { fst; snd } ->
      String.concat " "
        [ "Misc.no_overflow_mul";
          fst;
          snd;
          "&& no_32_bit_overflow";
          fst;
          snd;
          "( * )" ]
    | No_shift_overflow { fst; snd } ->
      String.concat " "
        [ "Misc.no_overflow_add";
          fst;
          snd;
          "&&";
          fst;
          "+";
          snd;
          "<= Sys.int_size" ]
    | No_bitwise_overflow_assert { fst; snd; op } ->
      String.concat " "
        ["bitwise_overflow_assert"; fst; snd; Operator.to_ocaml_op op]
    | Is_lsl_imm32 fst -> String.concat " " [fst; ">= 0 &&"; fst; "< 31"]
    | And lst -> "(" ^ String.concat "&&" (List.map to_string lst) ^ ")"
    | Or lst -> "(" ^ String.concat "||" (List.map to_string lst) ^ ")"
end

module Action : sig
  type t

  val make_delete : instr:string -> t

  val make_add_after_return :
    curr_instr:string -> new_instr_name:string -> cell_constructor:string -> t

  val make_add_before_return :
    curr_instr:string -> new_instr_name:string -> cell_constructor:string -> t

  val make_add_after : curr_instr:string -> cell_constructor:string -> t

  val make_add_before : curr_instr:string -> cell_constructor:string -> t

  val to_string : t -> string
end = struct
  type t =
    | Delete of string
    | Add_after_return of
        { curr_instr : string;
          new_instr_name : string;
          cell_constructor : string
        }
    | Add_before_return of
        { curr_instr : string;
          new_instr_name : string;
          cell_constructor : string
        }
    | Add_after of
        { curr_instr : string;
          cell_constructor : string
        }
    | Add_before of
        { curr_instr : string;
          cell_constructor : string
        }

  let make_delete ~instr = Delete instr

  let make_add_after_return ~curr_instr ~new_instr_name ~cell_constructor =
    Add_after_return { curr_instr; new_instr_name; cell_constructor }

  let make_add_before_return ~curr_instr ~new_instr_name ~cell_constructor =
    Add_before_return { curr_instr; new_instr_name; cell_constructor }

  let make_add_after ~curr_instr ~cell_constructor =
    Add_after { curr_instr; cell_constructor }

  let make_add_before ~curr_instr ~cell_constructor =
    Add_before { curr_instr; cell_constructor }

  let to_string = function
    | Delete name -> String.concat "" ["DLL.delete_curr "; name; ";"]
    | Add_before_return { curr_instr; new_instr_name; cell_constructor } ->
      String.concat " "
        [ "let";
          new_instr_name;
          "= DLL.insert_and_return_before";
          curr_instr;
          cell_constructor;
          "in" ]
    | Add_after_return { curr_instr; new_instr_name; cell_constructor } ->
      String.concat " "
        [ "let";
          new_instr_name;
          "= DLL.insert_and_return_after";
          curr_instr;
          cell_constructor;
          "in" ]
    | Add_before { curr_instr; cell_constructor } ->
      String.concat " " ["DLL.insert_before"; curr_instr; cell_constructor; ";"]
    | Add_after { curr_instr; cell_constructor } ->
      String.concat " " ["DLL.insert_after"; curr_instr; cell_constructor; ";"]
end

module Case = struct
  type t =
    { instrs : Instruction.t list;
      condition : Condition.t;
      actions : Action.t list;
      return_cell : string
    }
end

type t =
  { name : string;
    instr_names : string list;
    cases : Case.t list
  }

let get_instr_arg idx instr_name =
  String.concat ""
    ["Array.unsafe_get "; instr_name; "_val.arg "; Int.to_string idx]

let get_instr_res idx instr_name =
  String.concat ""
    ["Array.unsafe_get "; instr_name; "_val.res "; Int.to_string idx]

let get_cell_list slide_back name instr_names =
  [ String.concat " "
      ["let"; name; "(cell : Cfg.basic Cfg.instruction DLL.cell) ="];
    String.concat " " ["let slide_back ="; Int.to_string slide_back; "in"];
    String.concat " "
      [ "  match get_cells cell";
        List.length instr_names |> Int.to_string;
        "with" ];
    "  | None -> None";
    String.concat " " ["  | Some ["; String.concat "; " instr_names; "] -> ("]
  ]

let define_cell_vals instr_names =
  List.map
    (fun instr_name ->
      String.concat " "
        ["let"; instr_name ^ "_val"; "= DLL.value"; instr_name; "in"])
    instr_names

let create_match_condition instrs =
  [ String.concat " "
      ["|"; String.concat ", " (List.map Instruction.get_match instrs); "->"] ]

let define_registers instr_names instrs =
  let named_instrs = List.combine instr_names instrs in
  [ "if not (";
    String.concat "&&"
      (List.map
         (fun instr_name ->
           String.concat ""
             [ "Array.length ";
               instr_name;
               "_val.arg = 1 && Array.length ";
               instr_name;
               "_val.res = 1" ])
         instr_names);
    ") then None else (" ]
  @ List.fold_left
      (fun str_lst (name, instr) ->
        let fst_name =
          Instruction.get_fst instr |> Option.get |> Operand.get_name_exn
        in
        let snd_name =
          Instruction.get_snd instr |> Option.get |> Operand.get_name_exn
        in
        [ String.concat " " ["let"; fst_name; "="; get_instr_arg 0 name; "in"];
          String.concat " " ["let"; snd_name; "="; get_instr_res 0 name; "in"]
        ]
        @ str_lst)
      [] named_instrs

let check_condition condition =
  ["if not ("; Condition.to_string condition; ") then None else ("]

let apply_actions replacer =
  [ String.concat "\n"
      (List.map (fun action -> Action.to_string action) replacer) ]

let generate_case name instr_names
    ({ instrs; condition; actions; return_cell } : Case.t) =
  create_match_condition instrs
  @ define_registers instr_names instrs
  @ check_condition condition @ apply_actions actions
  @ [ String.concat ""
        [ "if Option.is_some \
           !Flambda_backend_flags.cfg_peephole_optimize_track then update_csv \
           \"";
          name;
          "\";" ];
      String.concat "" ["Some ((prev_at_most slide_back) "; return_cell; ")"];
      "))"
      (* closing `(` from check_condition and closing `(` from
         define_registers)*) ]

let match_cell_cases name instr_names cases =
  [ String.concat " "
      [ "match";
        String.concat ", "
          (List.map (fun instr_name -> instr_name ^ "_val.desc") instr_names);
        "with" ] ]
  @ List.flatten (List.map (generate_case name instr_names) cases)
  @ ["| _ -> None)"]

let generate_func slide_back { name; instr_names; cases } =
  get_cell_list slide_back name instr_names
  @ define_cell_vals instr_names
  @ match_cell_cases name instr_names cases
  @ ["| _ -> None"]
  |> String.concat "\n"

let construct_func_name_list funcs =
  String.concat " "
    (["let generated_rule_names = ["]
    @ List.map (fun func -> "\"" ^ func.name ^ "\";") funcs
    @ ["]"])

let construct_func_match_pref func =
  [
    String.concat " " ["match"; func.name; "cell with"];
    "| None -> ("
  ]

let construct_func_match_suff =
  [
    "| res -> res)"
  ]

let rec construct_generated_func' funcs lines_pref lines_suff =
  match funcs with
  | [] -> lines_pref @ lines_suff
  | hd :: tl -> 
    construct_generated_func' 
    tl 
    (construct_func_match_pref hd @ lines_pref) 
    (lines_suff @ construct_func_match_suff)

let construct_generated_func funcs =
  let rev_funcs = List.rev funcs in
  let rev_funcs_tl = List.tl rev_funcs in
  let rev_funcs_hd = List.hd rev_funcs in
  let lines = [
    String.concat " " ["match"; rev_funcs_hd.name; "cell with"];
    "| None -> None";
    "| res -> res)";
  ] in
  String.concat "\n" (["let generated_rules cell = ("] @ (construct_generated_func' rev_funcs_tl lines [] ))

let useles_movs =
  { name = "useless_movs";
    instr_names = ["mov1"; "mov2"];
    cases =
      [ { instrs =
            [ Instruction.make_move ~fst:"reg1" ~snd:"reg2";
              Instruction.make_move ~fst:"reg3" ~snd:"reg4" ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg2" ~snd:"reg3" ];
          actions = [Action.make_delete ~instr:"mov2"];
          return_cell = "mov1"
        } ]
  }

let fold_intop_imm_bitwise =
  { name = "fold_intop_imm_bitwise";
    instr_names = ["op1"; "op2"];
    cases =
      [ { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Shl;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Shl ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_shift_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Ilsl, imm1 + \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Shr;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Shr ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_shift_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Ilsr, imm1 + \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Asr;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Asr ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_shift_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Iasr, imm1 + \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.And;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.And ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_bitwise_overflow_assert ~fst:"imm1"
                  ~snd:"imm2" ~op:Operator.And ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Iand, imm1 \
                   land imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Or;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Or ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_bitwise_overflow_assert ~fst:"imm1"
                  ~snd:"imm2" ~op:Operator.Or ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Ior, imm1 lor \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Xor;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Xor ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_bitwise_overflow_assert ~fst:"imm1"
                  ~snd:"imm2" ~op:Operator.Xor ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Ixor, imm1 \
                   lxor imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        } ]
  }

let fold_intop_imm_arith =
  { name = "fold_intop_imm_arith";
    instr_names = ["op1"; "op2"];
    cases =
      [ { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Add;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Add ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_add_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Iadd, imm1 + \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Sub;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Sub ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_add_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Isub, imm1 + \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Add;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Sub ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_sub_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Iadd, imm1 - \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Sub;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Add ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_sub_overflow ~fst:"imm2" ~snd:"imm1" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Iadd, imm2 - \
                   imm1)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Mul;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Mul ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_no_mul_overflow ~fst:"imm1" ~snd:"imm2" ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Imul, imm1 * \
                   imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Shl;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Mul ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_is_lsl_imm32 ~fst:"imm1";
                Condition.make_no_mul_overflow ~fst:"(1 lsl imm1)" ~snd:"imm2"
              ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Imul, (1 lsl \
                   imm1) * imm2)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        };
        { instrs =
            [ Instruction.make_int_operation ~fst:"reg1" ~snd:"reg2" ~imm:"imm1"
                ~op:Operator.Mul;
              Instruction.make_int_operation ~fst:"reg3" ~snd:"reg4" ~imm:"imm2"
                ~op:Operator.Shl ];
          condition =
            Condition.combine_all
              [ Condition.make_are_equal ~fst:"reg1" ~snd:"reg2";
                Condition.make_are_equal ~fst:"reg3" ~snd:"reg4";
                Condition.make_are_equal ~fst:"reg1" ~snd:"reg3";
                Condition.make_is_lsl_imm32 ~fst:"imm2";
                Condition.make_no_mul_overflow ~fst:"(1 lsl imm2)" ~snd:"imm1"
              ];
          actions =
            [ Action.make_add_before_return ~curr_instr:"op1"
                ~new_instr_name:"op3"
                ~cell_constructor:
                  "{ op1_val with desc = Cfg.Op (Intop_imm (Mach.Imul, (1 lsl \
                   imm2) * imm1)) }";
              Action.make_delete ~instr:"op1";
              Action.make_delete ~instr:"op2" ];
          return_cell = "op3"
        } ]
  }

let funcs = [useles_movs; fold_intop_imm_arith; fold_intop_imm_bitwise]

let () =
  let slide_back =
    List.fold_left max 0
      (List.map (fun func -> List.length func.instr_names) funcs)
    - 1
  in
  "[@@@ocaml.warning \"+a-30-40-41-42-4\"]" |> print_endline;
  "open! Peephole_utils" |> print_endline;
  List.iter
    (fun func ->
      generate_func slide_back func |> print_endline;
      print_endline "")
    funcs;
  construct_generated_func funcs |> print_endline;
  construct_func_name_list funcs |> print_endline
