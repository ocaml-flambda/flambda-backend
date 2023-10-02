%{
open! Fexpr

let make_loc (startpos, endpos) =
  Debuginfo.Scoped_location.of_location
    ~scopes:Debuginfo.Scoped_location.empty_scopes
    {
      Location.loc_start = startpos;
      Location.loc_end = endpos;
      Location.loc_ghost = false;
    }

let make_located txt (startpos, endpos) =
  let loc = make_loc (startpos, endpos) in
  { txt; loc }

let make_plain_int = function
  | s, None -> Int64.of_string s |> Int64.to_int
  | _, Some _ ->
    Misc.fatal_errorf "No modifier expected here"

let make_targetint = function
  | s, None -> Int64.of_string s
  | _, Some _ ->
    Misc.fatal_errorf "No modifier expected here"

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    Misc.fatal_errorf "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, None -> s
  | _, _ ->
    Misc.fatal_errorf "Must be a tagged immediate"

let make_const_int (i, m) : const =
  match m with
  | None -> Tagged_immediate i
  | Some 'i' -> Naked_immediate i
  | Some 'n' -> Naked_nativeint (Int64.of_string i)
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> Misc.fatal_errorf "Unknown int modifier: %c" c

let make_boxed_const_int (i, m) : static_data =
  match m with
  | None -> Misc.fatal_errorf "Need int modifier for static data"
  | Some 'n' -> Boxed_nativeint (Const (Int64.of_string i))
  | Some 'l' -> Boxed_int32 (Const (Int32.of_string i))
  | Some 'L' -> Boxed_int64 (Const (Int64.of_string i))
  | Some c -> Misc.fatal_errorf "Bad int modifier for static data: %c" c

%}

/* Tokens */

%token AMP   [@symbol "&"]
%token AT    [@symbol "@"]
%token BIGARROW [@symbol "===>"]
%token BLANK [@symbol "_"]
%token CARET [@symbol "^"]
%token COLON  [@symbol ":"]
%token COMMA [@symbol ","]
%token DOT   [@symbol "."]
%token EQUAL [@symbol "="]
%token EQUALDOT [@symbol "=."]
%token <float> FLOAT
%token GREATER [@symbol ">"]
%token GREATEREQUAL [@symbol ">="]
%token GREATERDOT [@symbol ">."]
%token GREATEREQUALDOT [@symbol ">"]
%token <string> IDENT
%token <string * char option> INT
%token LBRACE [@symbol "{"]
%token LBRACK [@symbol "["]
%token LBRACKPIPE [@symbol "[|"]
%token LESS   [@symbol "<"]
%token LESSDOT [@symbol "<."]
%token LESSEQUAL [@symbol "<="]
%token LESSEQUALDOT [@symbol "<=."]
%token LESSMINUS [@symbol "<-"]
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token NOTEQUAL [@symbol "<>"]
%token NOTEQUALDOT [@symbol "<>."]
%token QMARK [@symbol "?"]
%token QMARKDOT [@symbol "?."]
%token PIPE [@symbol "|"]
%token PERCENT [@symbol "%"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token RBRACE [@symbol "}"]
%token RBRACK [@symbol "]"]
%token RBRACKPIPE [@symbol "|]"]
%token RPAREN [@symbol ")"]
%token SEMICOLON [@symbol ";"]
%token SLASH  [@symbol "/"]
%token SLASHDOT [@symbol "/."]
%token STAR   [@symbol "*"]
%token STARDOT [@symbol "*."]
%token<string> STRING
%token<Fexpr.compilation_unit option * string> SYMBOL
%token TILDE [@symbol "~"]
%token TILDEMINUS [@symbol "~-"]
%token EOF

%token KWD_ALWAYS [@symbol "always"]
%token KWD_AND   [@symbol "and"]
%token KWD_ANDWHERE [@symbol "andwhere"]
%token KWD_ANY   [@symbol "any"]
%token KWD_APPLY [@symbol "apply"]
%token KWD_ARRAY [@symbol "array"]
%token KWD_ASR   [@symbol "asr"]
%token KWD_AVAILABLE [@symbol "available"]
%token KWD_BOXED [@symbol "boxed"]
%token KWD_BSWAP [@symbol "bswap"]
%token KWD_CCALL  [@symbol "ccall"]
%token KWD_CLOSURE  [@symbol "closure"]
%token KWD_CODE  [@symbol "code"]
%token KWD_CONT  [@symbol "cont"]
%token KWD_DEFAULT [@symbol "default"]
%token KWD_DEFINE_ROOT_SYMBOL [@symbol "define_root_symbol"]
%token KWD_DELETED [@symbol "deleted"]
%token KWD_DEPTH [@symbol "depth"]
%token KWD_DIRECT [@symbol "direct"]
%token KWD_DO_NOT_INLINE [@symbol "do_not_inline"]
%token KWD_DONE  [@symbol "done"]
%token KWD_END   [@symbol "end"]
%token KWD_ERROR [@symbol "error"]
%token KWD_EXN   [@symbol "exn"]
%token KWD_REGION [@symbol "region"]
%token KWD_FLOAT [@symbol "float"]
%token KWD_HCF   [@symbol "halt_and_catch_fire"]
%token KWD_HEAP_OR_LOCAL [@symbol "heap_or_local"]
%token KWD_HINT  [@symbol "hint"]
%token KWD_ID    [@symbol "id"]
%token KWD_IMM   [@symbol "imm" ]
%token KWD_IMMUTABLE_UNIQUE [@symbol "immutable_unique"]
%token KWD_IN    [@symbol "in"]
%token KWD_INF   [@symbol "inf"]
%token KWD_INLINE [@symbol "inline"]
%token KWD_INLINED [@symbol "inlined"]
%token KWD_INLINING_STATE [@symbol "inlining_state"]
%token KWD_INT32 [@symbol "int32"]
%token KWD_INT64 [@symbol "int64"]
%token KWD_INVALID [@symbol "invalid"]
%token KWD_LAND  [@symbol "land"]
%token KWD_LET   [@symbol "let"]
%token KWD_LOCAL [@symbol "local"]
%token KWD_LOOPIFY [@symbol "loopify"]
%token KWD_LOR   [@symbol "lor"]
%token KWD_LSL   [@symbol "lsl"]
%token KWD_LSR   [@symbol "lsr"]
%token KWD_LXOR  [@symbol "lxor"]
%token KWD_MUTABLE [@symbol "mutable"]
%token KWD_NATIVEINT [@symbol "nativeint"]
%token KWD_NEVER  [@symbol "never"]
%token KWD_NEWER_VERSION_OF [@symbol "newer_version_of"]
%token KWD_NOALLOC [@symbol "noalloc"]
%token KWD_NOTRACE [@symbol "notrace"]
%token KWD_OF     [@symbol "of"]
%token KWD_POP    [@symbol "pop"]
%token KWD_PUSH   [@symbol "push"]
%token KWD_REC    [@symbol "rec"]
%token KWD_REC_INFO [@symbol "rec_info"]
%token KWD_REGULAR [@symbol "regular"]
%token KWD_RERAISE [@symbol "reraise"]
%token KWD_SET_OF_CLOSURES [@symbol "set_of_closures"]
%token KWD_SIZE   [@symbol "size"]
%token KWD_SUCC   [@symbol "succ"]
%token KWD_SWITCH [@symbol "switch"]
%token KWD_TAG    [@symbol "tag"]
%token KWD_TAGGED [@symbol "tagged"]
%token KWD_TAILREC [@symbol "tailrec"]
%token KWD_TOPLEVEL [@symbol "toplevel"]
%token KWD_TUPLED [@symbol "tupled"]
%token KWD_UNIT   [@symbol "unit"]
%token KWD_UNREACHABLE [@symbol "unreachable"]
%token KWD_UNROLL [@symbol "unroll"]
%token KWD_UNSIGNED [@symbol "unsigned"]
%token KWD_VAL    [@symbol "val"]
%token KWD_WHERE  [@symbol "where"]
%token KWD_WITH   [@symbol "with"]

%token PRIM_ARRAY_LENGTH [@symbol "%array_length"]
%token PRIM_ARRAY_LOAD [@symbol "%array_load"]
%token PRIM_ARRAY_SET [@symbol "%array_set"]
%token PRIM_BEGIN_REGION [@symbol "%begin_region"]
%token PRIM_BEGIN_TRY_REGION [@symbol "%begin_try_region"]
%token PRIM_BIGSTRING_LOAD [@symbol "%bigstring_load"]
%token PRIM_BIGSTRING_SET [@symbol "%bigstring_set"]
%token PRIM_BLOCK [@symbol "%Block"]
%token PRIM_BLOCK_LOAD [@symbol "%block_load"]
%token PRIM_BLOCK_SET [@symbol "%block_set"]
%token PRIM_BOOLEAN_NOT [@symbol "%not"]
%token PRIM_BOX_FLOAT [@symbol "%Box_float"]
%token PRIM_BOX_INT32 [@symbol "%Box_int32"]
%token PRIM_BOX_INT64 [@symbol "%Box_int64"]
%token PRIM_BOX_NATIVEINT [@symbol "%Box_nativeint"]
%token PRIM_BYTES_LENGTH [@symbol "%bytes_length"]
%token PRIM_BYTES_LOAD [@symbol "%bytes_load"]
%token PRIM_BYTES_SET [@symbol "%bytes_set"]
%token PRIM_END_REGION [@symbol "%end_region"]
%token PRIM_GET_TAG [@symbol "%get_tag"]
%token PRIM_INT_ARITH [@symbol "%int_arith"]
%token PRIM_INT_COMP [@symbol "%int_comp"]
%token PRIM_INT_SHIFT [@symbol "%int_shift"]
%token PRIM_IS_FLAT_FLOAT_ARRAY [@symbol "%is_flat_float_array"]
%token PRIM_IS_INT [@symbol "%is_int"]
%token PRIM_NUM_CONV [@symbol "%num_conv"]
%token PRIM_OPAQUE [@symbol "%Opaque"]
%token PRIM_PHYS_EQ [@symbol "%phys_eq"]
%token PRIM_PHYS_NE [@symbol "%phys_ne"]
%token PRIM_PROJECT_VALUE_SLOT [@symbol "%project_value_slot"]
%token PRIM_PROJECT_FUNCTION_SLOT [@symbol "%project_function_slot"]
%token PRIM_STRING_LENGTH [@symbol "%string_length"]
%token PRIM_STRING_LOAD [@symbol "%string_load"]
%token PRIM_TAG_IMM [@symbol "%tag_imm"]
%token PRIM_UNBOX_FLOAT [@symbol "%unbox_float"]
%token PRIM_UNBOX_INT32 [@symbol "%unbox_int32"]
%token PRIM_UNBOX_INT64 [@symbol "%unbox_int64"]
%token PRIM_UNBOX_NATIVEINT [@symbol "%unbox_nativeint"]
%token PRIM_UNBOX_VEC128 [@symbol "%unbox_vec128"]
%token PRIM_UNTAG_IMM [@symbol "%untag_imm"]

%token STATIC_CONST_BLOCK [@symbol "Block"]
%token STATIC_CONST_FLOAT_ARRAY [@symbol "Float_array"]
%token STATIC_CONST_FLOAT_BLOCK [@symbol "Float_block"]
%token STATIC_CONST_EMPTY_ARRAY [@symbol "Empty_array"]

%start flambda_unit expect_test_spec
%type <Fexpr.region -> Fexpr.alloc_mode_for_allocations> alloc_mode_for_types_opt
%type <Fexpr.alloc_mode_for_allocations> alloc_mode_for_allocations_opt
%type <Fexpr.array_kind> array_kind
%type <Fexpr.binary_float_arith_op> binary_float_arith_op
%type <Fexpr.binary_int_arith_op> binary_int_arith_op
%type <Fexpr.block_access_field_kind> block_access_field_kind
%type <Fexpr.const> const
%type <Fexpr.continuation> continuation
%type <Fexpr.standard_int_or_float> convertible_type
%type <Fexpr.expect_test_spec> expect_test_spec
%type <Fexpr.field_of_block> field_of_block
%type <Fexpr.flambda_unit> flambda_unit
%type <unit Fexpr.comparison_behaviour> float_comp
%type <Fexpr.continuation_sort option> continuation_sort
%type <float Fexpr.or_variable> float_or_variable
%type <Fexpr.infix_binop> infix_binop
%type <Fexpr.signed_or_unsigned -> Fexpr.signed_or_unsigned Fexpr.comparison_behaviour> int_comp
(* %type <Fexpr.kind> kind *)
%type <Fexpr.kind_with_subkind> kind_with_subkind
%type <Fexpr.kind_with_subkind list> kinds_with_subkinds
%type <Fexpr.loopify_attribute> loopify
%type <Fexpr.mutability> mutability
%type <Flambda_kind.Naked_number_kind.t> naked_number_kind
%type <Fexpr.named> named
%type <Fexpr.rec_info> rec_info
%type <Fexpr.rec_info> rec_info_atom
%type <Fexpr.region> region
%type <Fexpr.special_continuation> special_continuation
%type <Fexpr.standard_int> standard_int
%type <Fexpr.static_data> static_data
%type <Fexpr.static_data_binding> static_data_binding
%type <Fexpr.subkind> subkind
%type <Fexpr.kind_with_subkind list> kinds_with_subkinds_nonempty
%type <Fexpr.variable -> Fexpr.static_data> static_data_kind
%type <Fexpr.symbol_binding> symbol_binding
%type <Fexpr.unary_int_arith_op> unary_int_arith_op
%type <Fexpr.unop> unop
%%

(* CR-someday lmaurer: Modularize and generally clean up *)

flambda_unit:
  | body = module_
    EOF
    { body }
;

expect_test_spec:
  | before = module_; BIGARROW; after = module_; EOF
    { { before; after } }
;

(* XCR lwhite: Probably easier to just use some default names for these
   continuations

   lmaurer: Makes sense. I went with "done" and "error" for the names. *)
module_:
  | body = expr
    { { body } }
;

exn_continuation:
  | STAR cont = continuation { cont }

exn_continuation_id:
  | STAR cont = continuation_id { cont }
;

let_symbol(body):
  | KWD_LET;
    bindings = separated_nonempty_list(KWD_AND, symbol_binding);
    value_slots = with_value_slots_opt;
    KWD_IN; body = body; { { bindings; value_slots; body } }
;

symbol_binding:
  | s = static_data_binding { Data s }
  | code = code { Code code }
  | code_id = deleted_code { Deleted_code code_id }
  | s = static_closure_binding { Closure s }
  | s = static_set_of_closures { Set_of_closures s }
;

deleted_code:
  | KWD_CODE; code_id = code_id; KWD_DELETED { code_id }
;

code:
  | header = code_header;
    params = kinded_args;
    closure_var = variable;
    region_var = variable;
    depth_var = variable;
    MINUSGREATER; ret_cont = continuation_id;
    exn_cont = exn_continuation_id;
    ret_arity = return_arity;
    EQUAL; body = expr;
    { let
        recursive, inline, loopify, id, newer_version_of, code_size, is_tupled
        =
        header
      in
      { id; newer_version_of; param_arity = None; ret_arity; recursive; inline;
        params_and_body = { params; closure_var; region_var; depth_var;
                            ret_cont; exn_cont; body };
        code_size; is_tupled; loopify; } }
;

code_header:
  | KWD_CODE;
    recursive = recursive;
    inline = option(inline);
    loopify = loopify_opt;
    KWD_SIZE LPAREN; code_size = code_size; RPAREN;
    newer_version_of = option(newer_version_of);
    is_tupled = boption(KWD_TUPLED);
    id = code_id;
    { recursive, inline, loopify, id, newer_version_of, code_size, is_tupled }
;

newer_version_of:
  | KWD_NEWER_VERSION_OF LPAREN; id = code_id; RPAREN { id };

static_closure_binding:
  | symbol = symbol; EQUAL; fun_decl = fun_decl;
    { { symbol; fun_decl } }
;

static_set_of_closures:
  | KWD_SET_OF_CLOSURES;
    bindings = separated_nonempty_list(KWD_AND, static_closure_binding);
    elements = with_value_slots_opt;
    KWD_END
    { { bindings; elements } }

recursive:
  | { Nonrecursive }
  | KWD_REC { Recursive }
;

nullop:
  | PRIM_BEGIN_REGION { Begin_region }
;

unary_int_arith_op:
  | KWD_BSWAP { Swap_byte_endianness }
  | TILDEMINUS { Neg }

unop:
  | PRIM_ARRAY_LENGTH { Array_length }
  | PRIM_BEGIN_TRY_REGION { Begin_try_region }
  | PRIM_BOOLEAN_NOT { Boolean_not }
  | PRIM_BOX_FLOAT; alloc = alloc_mode_for_allocations_opt
    { Box_number (Naked_float, alloc) }
  | PRIM_BOX_INT32; alloc = alloc_mode_for_allocations_opt
    { Box_number (Naked_int32, alloc) }
  | PRIM_BOX_INT64; alloc = alloc_mode_for_allocations_opt
    { Box_number (Naked_int64, alloc) }
  | PRIM_BOX_NATIVEINT; alloc = alloc_mode_for_allocations_opt
    { Box_number (Naked_nativeint, alloc) }
  | PRIM_BYTES_LENGTH { String_length Bytes }
  | PRIM_END_REGION { End_region }
  | PRIM_GET_TAG { Get_tag }
  | PRIM_IS_FLAT_FLOAT_ARRAY { Is_flat_float_array }
  | PRIM_IS_INT { Is_int }
  | PRIM_INT_ARITH; i = standard_int; o = unary_int_arith_op
    { Int_arith (i, o) }
  | PRIM_NUM_CONV; LPAREN;
      src = convertible_type; MINUSGREATER; dst = convertible_type;
    RPAREN
    { Num_conv { src; dst } }
  | PRIM_OPAQUE { Opaque_identity }
  | PRIM_PROJECT_VALUE_SLOT; project_from = function_slot; DOT; value_slot = value_slot_for_projection
    { Project_value_slot { project_from; value_slot } }
  | PRIM_PROJECT_FUNCTION_SLOT; LPAREN;
      move_from = function_slot; MINUSGREATER; move_to = function_slot;
    RPAREN
    { Project_function_slot { move_from; move_to } }
  | PRIM_STRING_LENGTH { String_length String }
  | PRIM_TAG_IMM { Tag_immediate }
  | PRIM_UNBOX_FLOAT { Unbox_number Naked_float }
  | PRIM_UNBOX_INT32 { Unbox_number Naked_int32 }
  | PRIM_UNBOX_INT64 { Unbox_number Naked_int64 }
  | PRIM_UNBOX_NATIVEINT { Unbox_number Naked_nativeint }
  | PRIM_UNBOX_VEC128 { Unbox_number Naked_vec128 }
  | PRIM_UNTAG_IMM { Untag_immediate }

infix_binop:
  | o = binary_int_arith_op { Int_arith o }
  | c = int_comp { Int_comp (c Signed) }
  | s = int_shift { Int_shift s }
  | o = binary_float_arith_op { Float_arith o }
  | c = float_comp { Float_comp c }
;

prefix_binop:
  | PRIM_BLOCK_LOAD;
    mutability = mutability;
    kind = block_access_kind;
    { Block_load (kind, mutability) }
  | PRIM_BIGSTRING_LOAD;
    saw = string_accessor_width;
    { String_or_bigstring_load (Bigstring, saw) }
  | PRIM_BYTES_LOAD;
    saw = string_accessor_width;
    { String_or_bigstring_load (Bytes, saw) }
  | PRIM_STRING_LOAD;
    saw = string_accessor_width;
    { String_or_bigstring_load (String, saw) }
  | PRIM_PHYS_EQ { Phys_equal Eq }
  | PRIM_PHYS_NE { Phys_equal Neq }

mutability:
  | KWD_MUTABLE { Mutable }
  | KWD_IMMUTABLE_UNIQUE { Immutable_unique }
  | { Immutable }

string_accessor_width:
  | i = INT
    { let (i,c) = i in
      match int_of_string i, c with
      | 8, None -> Eight
      | 16, None -> Sixteen
      | 32, None -> Thirty_two
      | 64, None -> Sixty_four
      | 128, Some 'a' -> One_twenty_eight {aligned = true}
      | 128, Some 'u' -> One_twenty_eight {aligned = false}
      | _, _ -> Misc.fatal_error "invalid string accessor width" }

array_kind:
  | { Values }
  | KWD_IMM { Immediates }
  | KWD_FLOAT { Naked_floats }

block_access_kind:
  | field_kind = block_access_field_kind; tag = tag_opt; size = size_opt
    { Values { field_kind; tag; size } }
  | KWD_FLOAT; size = size_opt
    { Naked_floats { size } }
;

block_access_field_kind:
  | { Any_value }
  | KWD_IMM { Immediate }

size_opt:
  | { None }
  | KWD_SIZE; LPAREN; size = targetint; RPAREN { Some size }

standard_int:
  | { Tagged_immediate }
  | KWD_IMM { Naked_immediate }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }

convertible_type:
  | KWD_IMM KWD_TAGGED { Tagged_immediate }
  | KWD_IMM { Naked_immediate }
  | KWD_FLOAT { Naked_float }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }

init_or_assign:
  | EQUAL { Initialization }
  | LESSMINUS { Assignment Heap }
  | LESSMINUS AMP { Assignment Local }

alloc_mode_for_types_opt:
  | { fun _ -> Heap }
  | KWD_HEAP_OR_LOCAL
  | KWD_LOCAL { fun region -> Local { region } }

alloc_mode_for_allocations_opt:
  | { Heap }
  | AMP; region = region { Local { region } }

signed_or_unsigned:
  | { Signed }
  | KWD_UNSIGNED { Unsigned }

binary_int_arith_op:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
  | PERCENT { Mod }
  | KWD_LAND { And }
  | KWD_LOR { Or }
  | KWD_LXOR { Xor }

binary_float_arith_op:
  | PLUSDOT { Add }
  | MINUSDOT { Sub }
  | STARDOT { Mul }
  | SLASHDOT { Div }

int_comp:
  | LESS { fun s -> Yielding_bool (Lt s) }
  | GREATER { fun s -> Yielding_bool (Gt s) }
  | LESSEQUAL { fun s -> Yielding_bool (Le s) }
  | GREATEREQUAL { fun s -> Yielding_bool (Ge s) }
  | EQUAL { fun _ -> Yielding_bool Eq }
  | NOTEQUAL { fun _ -> Yielding_bool Neq }
  | QMARK { fun s -> Yielding_int_like_compare_functions s }

float_comp:
  | EQUALDOT { Yielding_bool Eq }
  | NOTEQUALDOT { Yielding_bool Neq }
  | LESSDOT { Yielding_bool ( Lt ()) }
  | GREATERDOT { Yielding_bool ( Gt ()) }
  | LESSEQUALDOT { Yielding_bool (Le()) }
  | GREATEREQUALDOT { Yielding_bool (Ge ()) }
  | QMARKDOT { (Yielding_int_like_compare_functions ()) }
;

int_shift:
  | KWD_LSL { Lsl }
  | KWD_LSR { Lsr }
  | KWD_ASR { Asr }
;

binop_app:
  | op = prefix_binop; LPAREN; arg1 = simple; COMMA; arg2 = simple; RPAREN
    { Binary (op, arg1, arg2) }
  | arg1 = simple; op = infix_binop; arg2 = simple
    { Binary (Infix op, arg1, arg2) }
  | PRIM_ARRAY_LOAD; ak = array_kind; mut = mutability;
    arg1 = simple; DOT; LPAREN; arg2 = simple; RPAREN
    { Binary (Array_load (ak, mut), arg1, arg2) }
  | PRIM_INT_ARITH; i = standard_int;
    arg1 = simple; c = binary_int_arith_op; arg2 = simple
    { Binary (Int_arith (i, c), arg1, arg2) }
  | PRIM_INT_COMP;
    i = standard_int; s = signed_or_unsigned;
    arg1 = simple; c = int_comp; arg2 = simple
    { Binary (Int_comp (i, c s), arg1, arg2) }
  | PRIM_INT_SHIFT;
    i = standard_int; arg1 = simple; s = int_shift; arg2 = simple
    { Binary (Int_shift (i, s), arg1, arg2) }
;

bytes_or_bigstring_set:
  | PRIM_BYTES_SET { Bytes }
  | PRIM_BIGSTRING_SET { Bigstring }

ternop_app:
  | PRIM_ARRAY_SET; ak = array_kind;
    arr = simple; DOT LPAREN; ix = simple; RPAREN; ia = init_or_assign;
    v = simple
    { Ternary (Array_set (ak, ia), arr, ix, v) }
  | PRIM_BLOCK_SET; kind = block_access_kind;
    block = simple; DOT LPAREN; ix = simple; RPAREN; ia = init_or_assign;
    v = simple
    { Ternary (Block_set (kind, ia), block, ix, v) }
  | blv = bytes_or_bigstring_set; saw = string_accessor_width;
    block = simple; DOT LPAREN; ix = simple; RPAREN; v = simple
    { Ternary (Bytes_or_bigstring_set (blv, saw), block, ix, v) }
;

block:
  | PRIM_BLOCK; m = mutability; t = tag; alloc = alloc_mode_for_allocations_opt;
    LPAREN; elts = separated_list(COMMA, simple); RPAREN
    { Variadic (Make_block (t, m, alloc), elts) }
;

named:
  | s = simple { Simple s }
  | n = nullop { Prim (Nullary n) }
  | u = unop a = simple { Prim (Unary (u, a)) }
  | b = binop_app { Prim b }
  | t = ternop_app { Prim t }
  | b = block { Prim b }
  | c = fun_decl { Closure c }
  (* CR lmaurer: This strikes me as a bit ugly, but it's necessary because
     [let x = y] could be assigning either a simple or a rec_info. We could
     also give a prefix to depth variables, but there are enough (too many?)
     magic prefixes in the language as is. Could also leave it ambiguous and
     infer from the kind of the variable. *)
  | KWD_REC_INFO; ri = rec_info_atom { Rec_info ri }
;

switch_case:
  | i = tag; MINUSGREATER; ac = apply_cont_expr { i,ac }
;

switch:
  | option(PIPE); cs = separated_list(PIPE, switch_case) { cs }
;
naked_number_kind:
  | KWD_IMM { Naked_immediate }
  | KWD_FLOAT { Naked_float }
  | KWD_INT32 { Naked_int32 }
  | KWD_INT64 { Naked_int64 }
  | KWD_NATIVEINT { Naked_nativeint }
;
(*
kind:
  | KWD_VAL { Flambda_kind.value }
  | nnk = naked_number_kind { Flambda_kind.naked_number nnk }
  | KWD_REGION { Flambda_kind.region }
  | KWD_REC_INFO { Flambda_kind.rec_info }
;
*)
kind_with_subkind:
  | nnk = naked_number_kind { Naked_number nnk }
  | subkind = subkind { Value subkind }
  | KWD_REGION { Region }
  | KWD_REC_INFO { Rec_info }
;
kinds_with_subkinds :
  | KWD_UNIT { [] }
  | ks = separated_nonempty_list(STAR, kind_with_subkind) { ks }
;
subkind:
  | KWD_VAL { Anything }
  | KWD_FLOAT KWD_BOXED { Boxed_float }
  | KWD_INT32 KWD_BOXED { Boxed_int32 }
  | KWD_INT64 KWD_BOXED { Boxed_int64 }
  | KWD_NATIVEINT KWD_BOXED { Boxed_nativeint }
  | KWD_IMM KWD_TAGGED { Tagged_immediate }
  | KWD_FLOAT; CARET; num_fields = plain_int { Float_block { num_fields } }
  | LBRACK; ctors = ctors; RBRACK
    { let consts, non_consts = ctors in Variant { consts; non_consts; }}
  | KWD_FLOAT KWD_ARRAY { Float_array }
  | KWD_IMM KWD_ARRAY { Immediate_array }
  | KWD_VAL KWD_ARRAY { Value_array }
  | KWD_ANY KWD_ARRAY { Generic_array }
;
kinds_with_subkinds_nonempty:
  | sks = separated_nonempty_list(STAR, kind_with_subkind) { sks }
;
(* LR(1) restrictions make this a bit awkward to write *)
ctors:
  | { [], [] }
  | ctors = ctors_nonempty { ctors }
ctors_nonempty:
  | tag = targetint { [ tag ], [] }
  | tag = targetint; PIPE; ctors = ctors_nonempty
      { let (c, nc) = ctors in (tag :: c, nc) }
  | nonconsts = nonconst_ctors_nonempty { [], nonconsts }
nonconst_ctors_nonempty:
  | ctors = separated_nonempty_list(PIPE, nonconst_ctor) { ctors }
;
nonconst_ctor:
  | tag = tag; KWD_OF; kinds = kinds_with_subkinds_nonempty { tag, kinds }
;
return_arity:
  | { None }
  | COLON k = kinds_with_subkinds { Some k }
;

/* expr is staged so that let and where play nicely together. In particular, in
   let ... in ... where, we want the where to be on the inside so that the
   continuation can refer to the let-bound variables (and the defining
   expressions can't refer to continuations anyway); and in where ... where, we
   want both wheres to be at the same level (as it's easier to use parens to
   force them to nest than it is force them to un-nest). The straightforward way
   to achieve this is to have an expression be a let expression or an inner
   expression, and to have where be an inner expression. Then we say that the
   body of a continuation can have let but can't have where (without
   parentheses, that is). Unfortunately, it's hard to say "a let whose body is
   not a where" in a grammar, but we can get close by parameterizing let_expr by
   what nonterminal its body should be. */

expr:
  | l = let_expr(expr) { l }
  | i = inner_expr { i }
;

let_expr(body):
  | KWD_LET l = let_(body) { Let l }
  | ls = let_symbol(body) { Let_symbol ls }
;

inner_expr:
  | w = where_expr { w }
  | a = atomic_expr { a }
;

where_expr:
  | body = inner_expr; KWD_WHERE; recursive = recursive;
    bindings = separated_list(KWD_ANDWHERE, continuation_binding)
    { Let_cont { recursive; body; bindings } }
;

continuation_body:
  | l = let_expr(continuation_body) { l }
  | a = atomic_expr { a }
;

atomic_expr:
  | KWD_HCF { Invalid { message = "halt-and-catch-fire" } }
  | KWD_UNREACHABLE { Invalid { message =  "treat-as-unreachable" } }
  | KWD_INVALID; message = STRING { Invalid { message } }
  | KWD_CONT; ac = apply_cont_expr { Apply_cont ac }
  | KWD_SWITCH; scrutinee = simple; cases = switch { Switch {scrutinee; cases} }
  | KWD_APPLY e = apply_expr { Apply e }
  | LPAREN; e = expr; RPAREN { e }
;

let_(body):
  | bindings = separated_nonempty_list(KWD_AND, let_binding);
(*  CR lwhite: I think this closure elements stuff is a bit of a hangover from
    when closures definitions contained the code as well. I imagine the closures
    used to look like:

    let f a b c =
      ...
    and g x y z =
      ...
    with { i = j; ... } in
    ...

    but now they should probably just look something like:

      let (f', g') = closure({f, g}, {i = j; ...}) in
      ...

    lmaurer: Let_symbol_expr.t still allows code and closure definitions to be
    mutually recursive, though, so we need some syntax that bundles them
    together. Also, several sets of closures can share the same value slots.
 *)
    value_slots = with_value_slots_opt;
    KWD_IN body = body;
    { ({ bindings; value_slots; body } : let_) }
;

let_binding:
  | var = variable EQUAL defining_expr = named
    { { var; defining_expr } }
;

with_value_slots_opt:
  | { None }
  | KWD_WITH LBRACE;
      elements = separated_list(SEMICOLON, value_slot);
    RBRACE;
    { Some elements }
;

value_slot:
  | var = value_slot_for_projection; EQUAL; value = simple; { { var; value; } }
;

fun_decl:
  | KWD_CLOSURE; code_id = code_id;
    function_slot = function_slot_opt;
    alloc = alloc_mode_for_allocations_opt;
    { { code_id; function_slot; alloc; } }
;

apply_expr:
  | call_kind = call_kind;
    inlined = option(inlined);
    inlining_state = option(inlining_state);
    func = func_name_with_optional_arities;
    args = simple_args;
    AMP region = region;
    MINUSGREATER
    r = result_continuation e = exn_continuation
     { let (func, arities) = func in {
       func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = call_kind region;
          inlined;
          inlining_state;
          arities;
     } }
;

call_kind:
  | alloc = alloc_mode_for_types_opt; { fun region -> Function (Indirect (alloc region)) }
  | KWD_DIRECT; LPAREN;
      code_id = code_id;
      function_slot = function_slot_opt;
      alloc = alloc_mode_for_types_opt;
    RPAREN
    { fun region -> Function (Direct { code_id; function_slot; alloc = alloc region }) }
  | KWD_CCALL; noalloc = boption(KWD_NOALLOC)
    { fun _ -> C_call { alloc = not noalloc } }
;

inline:
  | KWD_INLINE LPAREN KWD_ALWAYS RPAREN { Always_inline }
  | KWD_INLINE LPAREN KWD_AVAILABLE RPAREN { Available_inline }
  | KWD_INLINE LPAREN KWD_NEVER RPAREN { Never_inline }
  | KWD_UNROLL LPAREN; i = plain_int; RPAREN { Inline_attribute.Unroll i }
  | KWD_INLINE LPAREN KWD_DEFAULT RPAREN { Default_inline }

inlined:
  | KWD_INLINED LPAREN KWD_ALWAYS RPAREN { Always_inlined }
  | KWD_INLINED LPAREN KWD_HINT RPAREN { Hint_inlined }
  | KWD_INLINED LPAREN KWD_NEVER RPAREN { Never_inlined }
  | KWD_UNROLL LPAREN; i = plain_int; RPAREN { Unroll i }
  | KWD_INLINED LPAREN KWD_DEFAULT RPAREN { Default_inlined }

inlining_state:
  | KWD_INLINING_STATE LPAREN; depth = inlining_state_depth; RPAREN
    {
      (* CR poechsel: Parse the inlining arguments *)
      { depth }
    }

inlining_state_depth:
  | KWD_DEPTH LPAREN; i = plain_int; RPAREN { i }
;

loopify_opt:
  | { None }
  | KWD_LOOPIFY LPAREN; l = loopify; RPAREN { Some l }
;

loopify:
  | KWD_ALWAYS { Always_loopify }
  | KWD_NEVER { Never_loopify }
  | KWD_DONE { Already_loopified }
  | KWD_DEFAULT KWD_TAILREC { Default_loopify_and_tailrec }
  | KWD_DEFAULT { Default_loopify_and_not_tailrec }
;

region:
  | v = variable { Named v }
  | KWD_TOPLEVEL { Toplevel }
;

result_continuation:
  | c = continuation { Return c }
  | KWD_NEVER { Never_returns }
;

apply_cont_expr:
  | cont = continuation; trap_action = option(trap_action); args = simple_args
    { { cont; args; trap_action } }
;

trap_action:
  | KWD_PUSH; LPAREN; exn_handler = continuation; RPAREN { Push { exn_handler } }
  | KWD_POP; LPAREN;
      raise_kind = option(raise_kind); exn_handler = continuation;
    RPAREN
    { Pop { exn_handler; raise_kind } }
;

raise_kind:
  | KWD_REGULAR { Regular }
  | KWD_RERAISE { Reraise }
  | KWD_NOTRACE { No_trace }

continuation_sort:
  | { None }
  | KWD_EXN { Some Exn }
  | KWD_DEFINE_ROOT_SYMBOL { Some Define_root_symbol }
;

continuation_binding:
  | name = continuation_id; sort = continuation_sort;
    params = kinded_args; EQUAL; handler = continuation_body
    { { name; params; handler; sort } }
;

kinded_args:
  | { [] }
  | LPAREN; vs = separated_nonempty_list(COMMA, kinded_variable); RPAREN { vs }
;

static_data_binding:
  | s = symbol EQUAL sp = static_data
    { { symbol = s; defining_expr = sp } }
;

static_data:
  | STATIC_CONST_BLOCK; m = mutability; tag = tag; LPAREN;
    elements = separated_list(COMMA, field_of_block); RPAREN
    { (Block { tag; mutability = m; elements } : static_data) }
  | f = FLOAT { Boxed_float (Const f) }
  | i = INT { make_boxed_const_int i }
  | v = variable; COLON; k = static_data_kind { k v }
  | STATIC_CONST_FLOAT_BLOCK; LPAREN;
    fs = separated_list(COMMA, float_or_variable);
    RPAREN
    { Immutable_float_block fs }
  | STATIC_CONST_FLOAT_ARRAY; LBRACKPIPE;
    fs = separated_list(SEMICOLON, float_or_variable);
    RBRACKPIPE
    { Immutable_float_array fs }
  | STATIC_CONST_EMPTY_ARRAY { Empty_array }
  | KWD_MUTABLE; s = STRING { Mutable_string { initial_value = s } }
  | s = STRING { Immutable_string s }
;

static_data_kind:
  | KWD_FLOAT KWD_BOXED { fun v -> Boxed_float (Var v) }
  | KWD_INT32 KWD_BOXED { fun v -> Boxed_int32 (Var v) }
  | KWD_INT64 KWD_BOXED { fun v -> Boxed_int64 (Var v) }
  | KWD_NATIVEINT KWD_BOXED { fun v -> Boxed_nativeint (Var v) }

float_or_variable:
  | f = FLOAT { Const f }
  | v = variable { Var v }

targetint:
  i = INT { make_targetint i }

tag:
  tag = INT { make_tag ~loc:(make_loc ($startpos, $endpos)) tag }
;

tag_opt:
  | KWD_TAG LPAREN; tag = tag; RPAREN { Some tag }
  | { None }
;

plain_int:
  i = INT { make_plain_int i }
;

field_of_block:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

kinded_variable:
  | param = variable; kind = kind_with_subkind_opt { { param; kind } }
;

kind_with_subkind_opt:
  | { None }
  | COLON; kind = kind_with_subkind { Some kind }

simple_args:
  | { [] }
  | LPAREN s = separated_nonempty_list(COMMA, simple); RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { Naked_float c }
;

func_name_with_optional_arities:
  | s = simple { s, None }
  | LPAREN;
      s = simple; COLON; params_arity = blank_or(kinds_with_subkinds);
      MINUSGREATER; ret_arity = kinds_with_subkinds;
    RPAREN
    { s, Some ({ params_arity; ret_arity } : function_arities) }
;

blank_or(a):
  | BLANK { None }
  | a = a { Some a }
;

simple:
  | s = symbol { Symbol s }
  | v = variable { Var v }
  | c = const { Const c }
  | s = simple; TILDE; c = coercion { Coerce (s, c) }
;

coercion:
  | KWD_ID { Id }
  | KWD_DEPTH; from = rec_info; MINUSGREATER; to_ = rec_info
    { Change_depth { from; to_; } }
;

rec_info_atom:
  | i = plain_int { Depth i }
  | KWD_INF { Infinity }
  | KWD_DO_NOT_INLINE { Do_not_inline }
  | dv = variable { Var dv }
  | LPAREN; ri = rec_info; RPAREN { ri }
;

rec_info:
  | ri = rec_info_atom { ri }
  | KWD_SUCC; ri = rec_info_atom { Succ ri }
  | KWD_UNROLL; d = plain_int; ri = rec_info_atom { Unroll (d, ri) }
;

code_id:
  | v = variable { v }
;

code_size:
  | i = plain_int { i }

function_slot:
  | v = variable { v }
;

function_slot_opt :
  | { None }
  | AT; cid = function_slot { Some cid }
;

symbol:
  | e = SYMBOL { make_located e ($startpos, $endpos) }
;

variable:
  | e = IDENT { make_located e ($startpos, $endpos) }
;

continuation_id :
  | e = IDENT { make_located e ($startpos, $endpos) }
;

continuation:
  | e = continuation_id { Named e }
  | s = special_continuation { Special s }
;

special_continuation:
  | KWD_DONE { Done }
  | KWD_ERROR { Error }
;

value_slot_for_projection:
  | e = IDENT { make_located e ($startpos, $endpos) }
;

%%
