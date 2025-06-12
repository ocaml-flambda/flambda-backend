(* Cannot vectorize this example because different candidate computations use
   the same register as both address and non-address arguments. *)
type s =
  | A
  | B

type fn = int -> int

type r =
  { c1 : fn;
    c2 : fn
  }

type t =
  { d1 : int;
    d2 : int;
    d3 : int;
    d4 : r;
    d5 : r;
    d6 : int
  }

type r' =
  { b0 : s;
    b1 : r;
    b2 : r
  }

type t' =
  { a1 : fn;
    a2 : fn;
    a3 : fn;
    a4 : fn;
    a5 : s;
    a6 : r;
    a7 : r;
    a8 : r'
  }

let b0 = Sys.opaque_identity A

let[@inline never] [@local never] [@specialize never] make t =
  let d4 = t.d4 in
  let d5 = t.d5 in
  let r' = { b1 = d4; b2 = d5; b0 } in
  { a1 = d4.c1;
    a2 = d4.c2;
    a3 = d5.c1;
    a4 = d5.c2;
    a5 = Sys.opaque_identity A;
    a6 = d4;
    a7 = d5;
    a8 = r'
  }

let print ppf t' = Format.fprintf ppf "%d %d"

let () =
  let t =
    { d1 = 1;
      d2 = 2;
      d3 = 3;
      d4 = { c1 = Int.add 1; c2 = Int.mul 3 };
      d5 = { c1 = Int.add 2; c2 = Int.mul 4 };
      d6 = 6
    }
  in
  let res = make t in
  let i = Sys.opaque_identity 7 in
  Format.printf "make %d %d\n" (res.a1 i) (res.a6.c1 i)
