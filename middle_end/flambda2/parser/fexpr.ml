type location = Lambda.scoped_location

type 'a located =
  { txt : 'a;
    loc : location
  }

type variable = string located

type continuation_id = string located

type code_id = string located

type function_slot = string located

type value_slot = string located

type compilation_unit =
  { ident : string;
    linkage_name : string option (* defaults to same as ident *)
  }

type symbol = (compilation_unit option * string) located

type immediate = string

type targetint = int64

type special_continuation =
  | Done
  (* top-level normal continuation *)
  | Error
(* top-level exception continuation *)

type continuation =
  | Named of continuation_id
  | Special of special_continuation

type result_continuation =
  | Return of continuation
  | Never_returns

type continuation_sort =
  | Normal
  | Exn
  | Define_root_symbol
(* There's also [Return] and [Toplevel_return], but those don't need to be
 * specified explicitly *)

type region =
  | Named of variable
  | Toplevel

type const =
  | Naked_immediate of immediate
  | Tagged_immediate of immediate
  | Naked_float of float
  | Naked_int32 of int32
  | Naked_int64 of int64
  | Naked_vec128 of Vector_types.Vec128.Bit_pattern.bits
  | Naked_nativeint of targetint

type field_of_block =
  | Symbol of symbol
  | Tagged_immediate of immediate
  | Dynamically_computed of variable

type is_recursive =
  | Nonrecursive
  | Recursive

type tag_scannable = int

type mutability = Mutability.t =
  | Mutable
  | Immutable
  | Immutable_unique

type 'a or_variable =
  | Const of 'a
  | Var of variable

type static_data =
  | Block of
      { tag : tag_scannable;
        mutability : mutability;
        elements : field_of_block list
      }
  | Boxed_float of float or_variable
  | Boxed_int32 of int32 or_variable
  | Boxed_int64 of int64 or_variable
  | Boxed_nativeint of targetint or_variable
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.bits or_variable
  | Immutable_float_block of float or_variable list
  | Immutable_float_array of float or_variable list
  | Immutable_value_array of field_of_block list
  | Empty_array
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

type kind = Flambda_kind.t

type subkind =
  | Anything
  | Boxed_float
  | Boxed_int32
  | Boxed_int64
  | Boxed_nativeint
  | Boxed_vec128
  | Tagged_immediate
  | Variant of
      { consts : targetint list;
        non_consts : (tag_scannable * kind_with_subkind list) list
      }
  | Float_block of { num_fields : int }
  | Float_array
  | Immediate_array
  | Value_array
  | Generic_array

and kind_with_subkind =
  | Value of subkind
  | Naked_number of Flambda_kind.Naked_number_kind.t
  | Region
  | Rec_info

type static_data_binding =
  { symbol : symbol;
    defining_expr : static_data
  }

type raise_kind = Trap_action.Raise_kind.t =
  | Regular
  | Reraise
  | No_trace

type trap_action =
  | Push of { exn_handler : continuation }
  | Pop of
      { exn_handler : continuation;
        raise_kind : raise_kind option
      }

type rec_info =
  | Depth of int
  | Infinity
  | Do_not_inline
  | Var of variable
  | Succ of rec_info
  | Unroll of int * rec_info

type coercion =
  | Id
  | Change_depth of
      { from : rec_info;
        to_ : rec_info
      }

type kinded_parameter =
  { param : variable;
    kind : kind_with_subkind option
  }

type name =
  | Var of variable
  | Symbol of symbol

type simple =
  | Var of variable
  | Symbol of symbol
  | Const of const
  | Coerce of simple * coercion

type array_kind = Flambda_primitive.Array_kind.t =
  | Immediates
  | Values
  | Naked_floats

type box_kind = Flambda_kind.Boxable_number.t =
  | Naked_float
  | Naked_int32
  | Naked_int64
  | Naked_nativeint
  | Naked_vec128

type generic_array_specialisation =
  | No_specialisation
  | Full_of_naked_floats
  | Full_of_immediates
  | Full_of_arbitrary_values_but_not_floats

type block_access_field_kind = Flambda_primitive.Block_access_field_kind.t =
  | Any_value
  | Immediate

type block_access_kind =
  | Values of
      { tag : tag_scannable option;
        size : targetint option;
        field_kind : block_access_field_kind
      }
  | Naked_floats of { size : targetint option }

type standard_int = Flambda_kind.Standard_int.t =
  | Tagged_immediate
  | Naked_immediate
  | Naked_int32
  | Naked_int64
  | Naked_nativeint

type standard_int_or_float = Flambda_kind.Standard_int_or_float.t =
  | Tagged_immediate
  | Naked_immediate
  | Naked_float
  | Naked_int32
  | Naked_int64
  | Naked_nativeint

type string_or_bytes = Flambda_primitive.string_or_bytes =
  | String
  | Bytes

type alloc_mode_for_allocations =
  | Heap
  | Local of { region : region }

type alloc_mode_for_types =
  | Heap
  | Heap_or_local
  | Local

type alloc_mode_for_assignments =
  | Heap
  | Local

type init_or_assign =
  | Initialization
  | Assignment of alloc_mode_for_assignments

type 'signed_or_unsigned comparison =
      'signed_or_unsigned Flambda_primitive.comparison =
  | Eq
  | Neq
  | Lt of 'signed_or_unsigned
  | Gt of 'signed_or_unsigned
  | Le of 'signed_or_unsigned
  | Ge of 'signed_or_unsigned

type equality_comparison = Flambda_primitive.equality_comparison =
  | Eq
  | Neq

type signed_or_unsigned = Flambda_primitive.signed_or_unsigned =
  | Signed
  | Unsigned

type nullop = Begin_region

type unary_int_arith_op = Flambda_primitive.unary_int_arith_op =
  | Neg
  | Swap_byte_endianness

type unop =
  | Array_length
  | Begin_try_region
  | Boolean_not
  | Box_number of box_kind * alloc_mode_for_allocations
  | End_region
  | Get_tag
  | Int_arith of standard_int * unary_int_arith_op
  | Is_flat_float_array
  | Is_int
  | Num_conv of
      { src : standard_int_or_float;
        dst : standard_int_or_float
      }
  | Opaque_identity
  | Project_value_slot of
      { project_from : function_slot;
        value_slot : value_slot
      }
  | Project_function_slot of
      { move_from : function_slot;
        move_to : function_slot
      }
  | String_length of string_or_bytes
  | Unbox_number of box_kind
  | Untag_immediate
  | Tag_immediate

type 'signed_or_unsigned comparison_behaviour =
      'signed_or_unsigned Flambda_primitive.comparison_behaviour =
  | Yielding_bool of 'signed_or_unsigned comparison
  | Yielding_int_like_compare_functions of 'signed_or_unsigned

type binary_int_arith_op = Flambda_primitive.binary_int_arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor

type int_shift_op = Flambda_primitive.int_shift_op =
  | Lsl
  | Lsr
  | Asr

type binary_float_arith_op = Flambda_primitive.binary_float_arith_op =
  | Add
  | Sub
  | Mul
  | Div

type string_accessor_width = Flambda_primitive.string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Sixty_four
  | One_twenty_eight of { aligned : bool }

type string_like_value = Flambda_primitive.string_like_value =
  | String
  | Bytes
  | Bigstring

type bytes_like_value = Flambda_primitive.bytes_like_value =
  | Bytes
  | Bigstring

type infix_binop =
  | Int_arith of binary_int_arith_op (* on tagged immediates *)
  | Int_shift of int_shift_op (* on tagged immediates *)
  | Int_comp of signed_or_unsigned comparison_behaviour (* on tagged imms *)
  | Float_arith of binary_float_arith_op
  | Float_comp of unit comparison_behaviour

type binop =
  | Array_load of array_kind * mutability
  | Block_load of block_access_kind * mutability
  | Phys_equal of equality_comparison
  | Int_arith of standard_int * binary_int_arith_op
  | Int_comp of standard_int * signed_or_unsigned comparison_behaviour
  | Int_shift of standard_int * int_shift_op
  | Infix of infix_binop
  | String_or_bigstring_load of string_like_value * string_accessor_width
  | Bigarray_get_alignment of int

type ternop =
  | Array_set of array_kind * init_or_assign
  | Block_set of block_access_kind * init_or_assign
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width

type varop =
  | Make_block of tag_scannable * mutability * alloc_mode_for_allocations

type prim =
  | Nullary of nullop
  | Unary of unop * simple
  | Binary of binop * simple * simple
  | Ternary of ternop * simple * simple * simple
  | Variadic of varop * simple list

type arity = kind_with_subkind list

type function_call =
  | Direct of
      { code_id : code_id;
        function_slot : function_slot option;
        alloc : alloc_mode_for_types
      }
  | Indirect of alloc_mode_for_types
(* Will translate to indirect_known_arity or indirect_unknown_arity depending on
   whether the apply record's arities field has a value *)

type method_kind =
  | Self
  | Public
  | Cached

type call_kind =
  | Function of function_call
  (* | Method of { kind : method_kind; obj : simple; } *)
  | C_call of { alloc : bool }

type function_arities =
  { params_arity : arity option;
    ret_arity : arity
  }

type inline_attribute = Inline_attribute.t =
  | Always_inline
  | Available_inline
  | Never_inline
  | Unroll of int
  | Default_inline

type inlined_attribute =
  | Always_inlined
  | Hint_inlined
  | Never_inlined
  | Unroll of int
  | Default_inlined

type inlining_state = { depth : int (* CR lmaurer: Add inlining arguments *) }

type loopify_attribute = Loopify_attribute.t =
  | Always_loopify
  | Never_loopify
  | Already_loopified
  | Default_loopify_and_tailrec
  | Default_loopify_and_not_tailrec

type apply =
  { func : simple;
    continuation : result_continuation;
    exn_continuation : continuation;
    args : simple list;
    call_kind : call_kind;
    arities : function_arities option;
    inlined : inlined_attribute option;
    inlining_state : inlining_state option;
    region : region
  }

type size = int

type apply_cont =
  { cont : continuation;
    trap_action : trap_action option;
    args : simple list
  }

type expr =
  | Let of let_
  | Let_cont of let_cont
  | Let_symbol of let_symbol
  | Apply of apply
  | Apply_cont of apply_cont
  | Switch of
      { scrutinee : simple;
        cases : (int * apply_cont) list
      }
  | Invalid of { message : string }

and value_slots = one_value_slot list

and one_value_slot =
  { var : value_slot;
    value : simple
  }

and let_ =
  { bindings : let_binding list;
    value_slots : value_slots option;
    body : expr
  }

and let_binding =
  { var : variable;
    defining_expr : named
  }

and named =
  | Simple of simple
  | Prim of prim
  | Closure of fun_decl
  | Rec_info of rec_info

and fun_decl =
  { code_id : code_id;
    function_slot : function_slot option (* defaults to same name as code id *);
    alloc : alloc_mode_for_allocations
        (* alloc mode for set of closures (ignored except on first binding) *)
  }

and let_cont =
  { recursive : is_recursive;
    body : expr;
    bindings : continuation_binding list
  }

and continuation_binding =
  { name : continuation_id;
    params : kinded_parameter list;
    sort : continuation_sort option;
    handler : expr
  }

and let_symbol =
  { bindings : symbol_binding list;
    (* Only used if there's no [Set_of_closures] in the list *)
    value_slots : value_slots option;
    body : expr
  }

and symbol_binding =
  | Data of static_data_binding
  | Code of code
  | Deleted_code of code_id
  | Closure of static_closure_binding
  | Set_of_closures of static_set_of_closures

and static_set_of_closures =
  { bindings : static_closure_binding list;
    elements : value_slots option
  }

and code =
  { id : code_id;
    newer_version_of : code_id option;
    param_arity : arity option;
    ret_arity : arity option;
    recursive : is_recursive;
    inline : inline_attribute option;
    params_and_body : params_and_body;
    code_size : code_size;
    is_tupled : bool;
    loopify : loopify_attribute option
  }

and code_size = int

and params_and_body =
  { params : kinded_parameter list;
    closure_var : variable;
    region_var : variable;
    depth_var : variable;
    ret_cont : continuation_id;
    exn_cont : continuation_id;
    body : expr
  }

and static_closure_binding =
  { symbol : symbol;
    fun_decl : fun_decl
  }

type flambda_unit = { body : expr }

type expect_test_spec =
  { before : flambda_unit;
    after : flambda_unit
  }

type markdown_node =
  | Text of string
  | Expect of expect_test_spec

type markdown_doc = markdown_node list
