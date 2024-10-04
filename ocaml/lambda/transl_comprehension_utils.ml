open Lambda

module Let_binding = struct
  module Let_kind = struct
    type t =
      | Immutable of let_kind
      | Mutable
  end

  type t =
    { let_kind : Let_kind.t;
      layout : layout;
      id : Ident.t;
      init : lambda;
      var : lambda
    }

  let make (let_kind : Let_kind.t) layout name init =
    let id = Ident.create_local name in
    let var =
      match let_kind with Mutable -> Lmutvar id | Immutable _ -> Lvar id
    in
    { let_kind; layout; id; init; var }

  let let_one { let_kind; layout; id; init } body =
    match let_kind with
    | Immutable let_kind -> Llet (let_kind, layout, id, init, body)
    | Mutable -> Lmutlet (layout, id, init, body)

  let let_all = List.fold_right let_one
end

module Lambda_utils = struct
  module Constants = struct
    let int n = Lconst (const_int n)

    let float f = Lconst (Const_base (Const_float (Float.to_string f)))

    let unboxed_float f =
      Lconst (Const_base (Const_unboxed_float (Float.to_string f)))

    let unboxed_float32 f =
      Lconst (Const_base (Const_unboxed_float32 (Float.to_string f)))

    let unboxed_int32 i = Lconst (Const_base (Const_unboxed_int32 i))

    let unboxed_int64 i = Lconst (Const_base (Const_unboxed_int64 i))

    let unboxed_nativeint i =
      (* See CR in typedtree.mli *)
      let i = i |> Targetint.to_int64 |> Int64.to_nativeint in
      Lconst (Const_base (Const_unboxed_nativeint i))

    let string ~loc s = Lconst (Const_base (Const_string (s, loc, None)))
  end

  let apply ~loc ~mode func args ~result_layout =
    (* These defaultscould be promoted to optional arguments if they were more
       widely used *)
    let region_close = Rc_normal in
    let tailcall = Default_tailcall in
    let inlined = Default_inlined in
    let specialised = Default_specialise in
    let probe = None in
    Lapply
      { ap_loc = loc;
        ap_func = func;
        ap_args = args;
        ap_region_close = region_close;
        ap_mode = mode;
        ap_tailcall = tailcall;
        ap_inlined = inlined;
        ap_specialised = specialised;
        ap_probe = probe;
        ap_result_layout = result_layout
      }

  module type Int_ops = sig
    (* We want to expose all the operators so we don't have to think about which
       ones to add and remove as we change the rest of the file *)

    val ( + ) : lambda -> lambda -> lambda

    val ( - ) : lambda -> lambda -> lambda

    val ( * ) : lambda -> lambda -> lambda

    val ( / ) : lambda -> lambda -> lambda

    val ( = ) : lambda -> lambda -> lambda

    val ( <> ) : lambda -> lambda -> lambda

    val ( < ) : lambda -> lambda -> lambda

    val ( > ) : lambda -> lambda -> lambda

    val ( <= ) : lambda -> lambda -> lambda

    val ( >= ) : lambda -> lambda -> lambda

    val ( && ) : lambda -> lambda -> lambda

    val ( || ) : lambda -> lambda -> lambda

    val l0 : lambda

    val l1 : lambda

    val i : int -> lambda
  end

  let int_ops ~loc : (module Int_ops) =
    (module struct
      let binop prim l r = Lprim (prim, [l; r], loc)

      let ( + ) = binop Paddint

      let ( - ) = binop Psubint

      let ( * ) = binop Pmulint

      let ( / ) = binop (Pdivint Unsafe)

      let ( = ) = binop (Pintcomp Ceq)

      let ( <> ) = binop (Pintcomp Cne)

      let ( < ) = binop (Pintcomp Clt)

      let ( > ) = binop (Pintcomp Cgt)

      let ( <= ) = binop (Pintcomp Cle)

      let ( >= ) = binop (Pintcomp Cge)

      let ( && ) = binop Psequor

      let ( || ) = binop Psequor

      let i = Constants.int

      let l0 = i 0

      let l1 = i 1
    end)

  module Primitive = struct
    (* CR layouts v4: To change when non-values are allowed in arrays. *)
    (** The Lambda primitive for calling a simple C primitive *)
    let c_prim name arity =
      Pccall (Lambda.simple_prim_on_values ~name ~arity ~alloc:true)

    (** Create a function that produces the Lambda representation for a
        one-argument C primitive when provided with a Lambda argument *)
    let unary name =
      let prim = c_prim name 1 in
      fun ~loc x -> Lprim (prim, [x], loc)

    (** Create a function that produces the Lambda representation for a
        two-argument C primitive when provided with two Lambda arguments *)
    let binary name =
      let prim = c_prim name 2 in
      fun ~loc x y -> Lprim (prim, [x; y], loc)

    (** Create a function that produces the Lambda representation for a
        three-argument C primitive when provided with three Lambda arguments *)
    let ternary name =
      let prim = c_prim name 3 in
      fun ~loc x y z -> Lprim (prim, [x; y; z], loc)

    let make_vect =
      let make_vect = binary "caml_make_vect" in
      fun ~loc ~length ~init -> make_vect ~loc length init

    let make_float_vect = unary "caml_make_float_vect"

    let make_unboxed_float32_vect = unary "caml_make_unboxed_float32_vect"

    let make_unboxed_int32_vect = unary "caml_make_unboxed_int32_vect"

    let make_unboxed_int64_vect = unary "caml_make_unboxed_int64_vect"

    let make_unboxed_nativeint_vect = unary "caml_make_unboxed_nativeint_vect"

    let make_unboxed_vec128_vect = unary "caml_make_unboxed_vec128_vect"

    let array_append = binary "caml_array_append"

    let array_sub =
      let array_sub = ternary "caml_array_sub" in
      fun ~loc a ~offset ~length -> array_sub ~loc a offset length
  end
end

module Cps_utils = struct
  (** Function composition *)
  let compose f g x = f (g x)

  (** Apply a function to the first part of a tuple *)
  let first f (x, y) = f x, y

  let compose_map f = List.fold_left (fun k x -> compose k (f x)) Fun.id

  let compose_map_acc f =
    List.fold_left_map (fun k x -> first (compose k) (f x)) Fun.id
end
