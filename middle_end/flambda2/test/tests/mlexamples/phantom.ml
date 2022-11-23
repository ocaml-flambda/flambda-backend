(* Example reduced from code in the `msat` package.

   when compiled with '-g', used to produce a bug with unbound variables (see
   flambda-backend/PR#176) *)

module type Foo = sig end

module Make (F : Foo) = struct
  exception UndecidedLit

  type var =
    { vid : int;
      pa : atom;
      na : atom;
      level : int
    }

  and atom =
    { aid : int;
      var : var;
      neg : atom;
      mutable is_true : bool
    }

  let[@inline] is_true a = a.is_true

  let[@inline] is_false a = a.neg.is_true

  let[@inline] eval_level _st (a : atom) =
    let lvl = a.var.level in
    if is_true a
    then (
      assert (lvl >= 0);
      true, lvl)
    else if is_false a
    then false, lvl
    else raise UndecidedLit

  let true_at_level0 st a =
    try
      let b, lev = (eval_level [@inlined]) st a in
      b && lev = 0
    with UndecidedLit -> false
end
[@@inline] [@@specialise]

module Make2 (F : Foo) = Make (struct
  include F
end)
[@@inline] [@@specialise]
