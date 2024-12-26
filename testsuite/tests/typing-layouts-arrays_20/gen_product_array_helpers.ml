module type Element_intf = Test_gen_u_array.Element_intf

type 'a elem =
  | Number : { ops : (module Element_intf with type t = 'a) } -> 'a elem
  | Option : 'a elem -> ('a option) elem
  | Tup2 : 'a1 elem * 'a2 elem -> ('a1 * 'a2) elem
  | Tup3 : 'a1 elem * 'a2 elem * 'a3 elem -> ('a1 * 'a2 * 'a3) elem
  | Tup4 : 'a1 elem * 'a2 elem * 'a3 elem * 'a4 elem
      -> ('a1 * 'a2 * 'a3 * 'a4) elem
  | Tup5 : 'a1 elem * 'a2 elem * 'a3 elem * 'a4 elem * 'a5 elem
      -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) elem
  | Tup6 : 'a1 elem * 'a2 elem * 'a3 elem * 'a4 elem * 'a5 elem * 'a6 elem
      -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) elem

module Int_elem : Element_intf with type t = int =
struct
  include Int
  let of_int x = x
  let max_val = max_int
  let min_val = min_int
  let rand = Random.full_int
  let print i = Printf.printf "%d" i
end

let int_elem = Number { ops = (module Int_elem) }

module Int32_elem : Element_intf with type t = int32 =
struct
  include Int32
  let max_val = max_int
  let min_val = min_int
  let rand = Random.int32
  let print i = Printf.printf "%ld" i
end

let int32_elem = Number { ops = (module Int32_elem) }

module Int64_elem : Element_intf with type t = int64 =
struct
  include Int64
  let max_val = max_int
  let min_val = min_int
  let rand = Random.int64
  let print i = Printf.printf "%Ld" i
end

let int64_elem = Number { ops = (module Int64_elem) }

module Nativeint_elem : Element_intf with type t = nativeint =
struct
  include Nativeint
  let max_val = max_int
  let min_val = min_int
  let rand = Random.nativeint
  let print i = Printf.printf "%nd" i
end

let nativeint_elem = Number { ops = (module Nativeint_elem) }

module Float_elem : Element_intf with type t = float =
struct
  include Float
  let max_val = max_float
  let min_val = min_float
  let rand = Random.float
  let print i = Printf.printf "%f" i
end

let float_elem = Number { ops = (module Float_elem) }

module Float32_elem : Element_intf with type t = float32 =
struct
  include Stdlib_stable.Float32
  let max_val = max_float
  let min_val = min_float
  let rand x = of_float (Random.float (to_float x))
  let print i = Printf.printf "%f" (to_float i)
end

let float32_elem = Number { ops = (module Float32_elem) }

let traverse0 (f : 'a. (module Element_intf with type t = 'a) -> 'a) =
  let rec go : type a . a elem -> a =
    fun (elem : a elem) ->
      match elem with
      | Number {ops} -> f ops
      | Option elem -> Some (go elem)
      | Tup2 (e1, e2) -> (go e1, go e2)
      | Tup3 (e1, e2, e3) -> (go e1, go e2, go e3)
      | Tup4 (e1, e2, e3, e4) -> (go e1, go e2, go e3, go e4)
      | Tup5 (e1, e2, e3, e4, e5) -> (go e1, go e2, go e3, go e4, go e5)
      | Tup6 (e1, e2, e3, e4, e5, e6) ->
        (go e1, go e2, go e3, go e4, go e5, go e6)
  in
  go

let traverse1 (f : 'a. (module Element_intf with type t = 'a) -> 'a -> 'a) =
  let rec go : type a . a elem -> a -> a =
    fun (elem : a elem) (a : a) ->
      match elem with
      | Number {ops} -> f ops a
      | Option elem -> Option.map (go elem) a
      | Tup2 (e1, e2) ->
        let a1, a2 = a in
        (go e1 a1, go e2 a2)
      | Tup3 (e1, e2, e3) ->
        let a1, a2, a3 = a in
        (go e1 a1, go e2 a2, go e3 a3)
      | Tup4 (e1, e2, e3, e4) ->
        let a1, a2, a3, a4 = a in
        (go e1 a1, go e2 a2, go e3 a3, go e4 a4)
      | Tup5 (e1, e2, e3, e4, e5) ->
        let a1, a2, a3, a4, a5 = a in
        (go e1 a1, go e2 a2, go e3 a3, go e4 a4, go e5 a5)
      | Tup6 (e1, e2, e3, e4, e5, e6) ->
        let a1, a2, a3, a4, a5, a6 = a in
        (go e1 a1, go e2 a2, go e3 a3, go e4 a4, go e5 a5, go e6 a6)
  in
  go

let traverse2
      (f : 'a. (module Element_intf with type t = 'a) -> 'a -> 'a -> 'a) =
  let rec go : type a . a elem -> a -> a -> a =
    fun (elem : a elem) (a1 : a) (a2 : a) ->
      match elem with
      | Number {ops} -> f ops a1 a2
      | Option elem ->
        begin match a1, a2 with
        | None, _ | _, None -> None
        | Some a1, Some a2 -> Some (go elem a1 a2)
        end
      | Tup2 (e1, e2) ->
        let a11, a12 = a1 in
        let a21, a22 = a2 in
        (go e1 a11 a21, go e2 a12 a22)
      | Tup3 (e1, e2, e3) ->
        let a11, a12, a13 = a1 in
        let a21, a22, a23 = a2 in
        (go e1 a11 a21, go e2 a12 a22, go e3 a13 a23)
      | Tup4 (e1, e2, e3, e4) ->
        let a11, a12, a13, a14 = a1 in
        let a21, a22, a23, a24 = a2 in
        (go e1 a11 a21, go e2 a12 a22, go e3 a13 a23, go e4 a14 a24)
      | Tup5 (e1, e2, e3, e4, e5) ->
        let a11, a12, a13, a14, a15 = a1 in
        let a21, a22, a23, a24, a25 = a2 in
        (go e1 a11 a21, go e2 a12 a22, go e3 a13 a23, go e4 a14 a24,
         go e5 a15 a25)
      | Tup6 (e1, e2, e3, e4, e5, e6) ->
        let a11, a12, a13, a14, a15, a16 = a1 in
        let a21, a22, a23, a24, a25, a26 = a2 in
        (go e1 a11 a21, go e2 a12 a22, go e3 a13 a23, go e4 a14 a24,
         go e5 a15 a25, go e6 a16 a26)
  in
  go

let rec of_int : type a . a elem -> int -> a =
  fun elem i ->
    match elem with
    | Number {ops} ->
      let module O = (val ops) in
      O.of_int i
    | Option elem -> Some (of_int elem i)
    | Tup2 (e1, e2) -> (of_int e1 i, of_int e2 i)
    | Tup3 (e1, e2, e3) -> (of_int e1 i, of_int e2 i, of_int e3 i)
    | Tup4 (e1, e2, e3, e4) ->
      (of_int e1 i, of_int e2 i, of_int e3 i, of_int e4 i)
    | Tup5 (e1, e2, e3, e4, e5) ->
      (of_int e1 i, of_int e2 i, of_int e3 i, of_int e4 i, of_int e5 i)
    | Tup6 (e1, e2, e3, e4, e5, e6) ->
      (of_int e1 i, of_int e2 i, of_int e3 i, of_int e4 i, of_int e5 i,
       of_int e6 i)

let add elem a1 a2 =
  let f (type a) (module E : Element_intf with type t = a) (a1 : a) (a2 : a) =
    E.add a1 a2
  in
  traverse2 f elem a1 a2

let sub elem a1 a2 =
  let f (type a) (module E : Element_intf with type t = a) (a1 : a) (a2 : a) =
    E.sub a1 a2
  in
  traverse2 f elem a1 a2

let mul elem a1 a2 =
  let f (type a) (module E : Element_intf with type t = a) (a1 : a) (a2 : a) =
    E.mul a1 a2
  in
  traverse2 f elem a1 a2

let neg elem a =
  let f (type a) (module E : Element_intf with type t = a) (a : a) =
    E.neg a
  in
  traverse1 f elem a

let max_val elem =
  let f (type a) (module E : Element_intf with type t = a) =
    E.max_val
  in
  traverse0 f elem

let min_val elem =
  let rec go : type a . a elem -> a =
    fun (elem : a elem) ->
      match elem with
      | Number {ops} ->
        let module E = (val ops) in
        E.min_val
      | Option elem -> None
      | Tup2 (e1, e2) -> (go e1, go e2)
      | Tup3 (e1, e2, e3) -> (go e1, go e2, go e3)
      | Tup4 (e1, e2, e3, e4) -> (go e1, go e2, go e3, go e4)
      | Tup5 (e1, e2, e3, e4, e5) -> (go e1, go e2, go e3, go e4, go e5)
      | Tup6 (e1, e2, e3, e4, e5, e6) ->
        (go e1, go e2, go e3, go e4, go e5, go e6)
  in
  go elem

let rand elem a =
  let f (type a) (module E : Element_intf with type t = a) (a : a) =
    E.rand a
  in
  traverse1 f elem a

let rec compare : type a . a elem -> a -> a -> int =
  fun elem a1 a2 ->
    match elem with
    | Number {ops} ->
      let module E = (val ops) in
      E.compare a1 a2
    | Option elem -> Option.compare (compare elem) a1 a2
    | Tup2 (e1, e2) ->
      let a11, a12 = a1 in
      let a21, a22 = a2 in
      let x = compare e1 a11 a21 in
      if x <> 0 then x
      else compare e2 a12 a22
    | Tup3 (e1, e2, e3) ->
      let a11, a12, a13 = a1 in
      let a21, a22, a23 = a2 in
      let x = compare e1 a11 a21 in
      if x <> 0 then x
      else
        let x = compare e2 a12 a22 in
        if x <> 0 then x else compare e3 a13 a23
    | Tup4 (e1, e2, e3, e4) ->
      let a11, a12, a13, a14 = a1 in
      let a21, a22, a23, a24 = a2 in
      let x = compare e1 a11 a21 in
      if x <> 0 then x
      else
        let x = compare e2 a12 a22 in
        if x <> 0 then x else
          let x = compare e3 a13 a23 in
          if x <> 0 then x else compare e4 a14 a24
    | Tup5 (e1, e2, e3, e4, e5) ->
      let a11, a12, a13, a14, a15 = a1 in
      let a21, a22, a23, a24, a25 = a2 in
      let x = compare e1 a11 a21 in
      if x <> 0 then x
      else
        let x = compare e2 a12 a22 in
        if x <> 0 then x else
          let x = compare e3 a13 a23 in
          if x <> 0 then x else
            let x = compare e4 a14 a24 in
            if x <> 0 then x else compare e5 a15 a25
    | Tup6 (e1, e2, e3, e4, e5, e6) ->
      let a11, a12, a13, a14, a15, a16 = a1 in
      let a21, a22, a23, a24, a25, a26 = a2 in
      let x = compare e1 a11 a21 in
      if x <> 0 then x
      else
        let x = compare e2 a12 a22 in
        if x <> 0 then x else
          let x = compare e3 a13 a23 in
          if x <> 0 then x else
            let x = compare e4 a14 a24 in
            if x <> 0 then x else
              let x = compare e5 a15 a25 in
              if x <> 0 then x else
                compare e6 a16 a26

let rec print : type a . a elem -> a -> unit =
  let open struct
    type packed = P : 'a elem * 'a -> packed

    let print_comma_sep l =
      Printf.printf "(";
      let rec go l =
        match l with
        | [] -> assert false
        | [P (e,a)] ->
          print e a;
          Printf.printf ")"
        | (P (e,a)) :: l ->
          print e a;
          Printf.printf ", ";
          go l
      in
      go l
  end
  in
  fun elem a ->
    match elem with
    | Number {ops} ->
      let module E = (val ops) in
      E.print a
    | Option elem ->
      begin match a with
      | None -> Printf.printf "None"
      | Some a -> begin
          Printf.printf "Some ";
          print elem a
        end
      end
    | Tup2 (e1, e2) ->
      let a1, a2 = a in
      print_comma_sep [P (e1, a1); P (e2, a2)]
    | Tup3 (e1, e2, e3) ->
      let a1, a2, a3 = a in
      print_comma_sep [P (e1, a1); P (e2, a2); P (e3, a3)]
    | Tup4 (e1, e2, e3, e4) ->
      let a1, a2, a3, a4 = a in
      print_comma_sep [P (e1, a1); P (e2, a2); P (e3, a3); P (e4, a4)]
    | Tup5 (e1, e2, e3, e4, e5) ->
      let a1, a2, a3, a4, a5 = a in
      print_comma_sep
        [P (e1, a1); P (e2, a2); P (e3, a3); P (e4, a4); P (e5, a5)]
    | Tup6 (e1, e2, e3, e4, e5, e6) ->
      let a1, a2, a3, a4, a5, a6 = a in
      print_comma_sep
        [P (e1, a1); P (e2, a2); P (e3, a3); P (e4, a4); P (e5, a5);
         P (e6, a6)]

let make_element_ops (type a) (elem : a elem)
  : (module Element_intf with type t = a) =
  (module struct
    type t = a

    let of_int i = of_int elem i
    let add t1 t2 = add elem t1 t2
    let sub t1 t2 = sub elem t1 t2
    let mul t1 t2 = mul elem t1 t2
    let neg t = neg elem t
    let max_val = max_val elem
    let min_val = min_val elem
    let rand t = rand elem t
    let compare t1 t2 = compare elem t1 t2
    let print t = print elem t
  end)
