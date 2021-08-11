(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let
  (*match*/271 = 3
   *match*/272 = 2
   *match*/273 = 1
   *match*/274 = *match*/271
   *match*/275 = *match*/272
   *match*/276 = *match*/273)
  (catch
    (catch
      (catch (if (!= *match*/275 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/274 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let
  (*match*/279 = 3
   *match*/280 = 2
   *match*/281 = 1
   *match*/282 = (makeblock 0 *match*/279 *match*/280 *match*/281))
  (catch
    (catch
      (let (*match*/283 =a (field 0 *match*/282))
        (catch
          (let (*match*/284 =a (field 1 *match*/282))
            (if (!= *match*/284 3) (exit 7)
              (let (*match*/285 =a (field 2 *match*/282))
                (exit 5 *match*/282))))
         with (7)
          (if (!= *match*/283 1) (exit 6)
            (let
              (*match*/287 =a (field 2 *match*/282)
               *match*/286 =a (field 1 *match*/282))
              (exit 5 *match*/282)))))
     with (6) 0)
   with (5 x/277[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/277) 1)))
- : bool = false
|}];;
