(* After [Base.Hashtbl.fold]. This example produces the following lambda code:

   (let (f/9 = (function x/11[int] init/12[int] : int (let (m/13 =v[int]
   init/12) (catch (try (exit 1 (for i/14 0 to x/11 (assign m/13 (+ m/13
   i/14)))) with exn/23 0) with (1 val/22[int]) m/13)))) (makeblock 0 f/9))

   which is interesting because of the for-loop as an argument to [exit], in
   conjunction with the mutable variable [m]. *)

type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

external ( ! ) : 'a ref -> 'a = "%field0"

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

external ( + ) : int -> int -> int = "%addint"

let f x init =
  let m = ref init in
  match
    for i = 0 to x do
      m := !m + i
    done
  with
  | () -> !m
  | exception _ -> 0
