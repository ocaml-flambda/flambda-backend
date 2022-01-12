type foo =
  { a : int;
    b : string
  }

type bar = { f : float }

type _ t =
  | Foo : foo -> foo t
  | Bar : bar -> bar t

let aux (type x) (x : x t) =
  match x with
  | Foo _ -> 3.
  | Bar bar ->
    (* This block load meets bar (aliased to foo (from f, after inlining) here),
       with a block with fields of kind float which should result on Bottom *)
    bar.f
  [@@inline]

let f (x : foo t) =
  let (Foo foo) = x in
  (* This is a block load without any annotation on the block foo: the variable
     foo is a row like of tag 'others' *)
  let n = foo.a in
  (* This ensures that foo is known to have fields of kind value *)
  aux x, n
