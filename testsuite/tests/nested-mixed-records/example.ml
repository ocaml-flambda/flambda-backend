(* TEST
   flambda2;
   native;
*)

(* Example from the pull request (#3687). *)

type sub = #{ a : int32#; b : string; c : float#; }

type sub' = #{ d : char; e : int64#; f : int; }

type record = { x : sub; y : sub'; }

let f r =
  r.x

let g r =
  let y = r.y in
  y.#f

external box_int32 : int32# -> (int32[@local_opt]) = "%box_int32"
external box_float : float# -> (float[@local_opt]) = "%box_float"

let () =
  let v = Sys.opaque_identity {
    x = #{
      a = #12l;
      b = "abc";
      c = #3.14;
    };
    y = #{
      d = 'p';
      e = #34L;
      f = 56;
    };
  } in
  let sub = f v in
  let z = g v in
  Printf.printf "%ld %S %g -- %d\n%!"
    (box_int32 sub.#a)
    sub.#b
    (box_float sub.#c)
    z
