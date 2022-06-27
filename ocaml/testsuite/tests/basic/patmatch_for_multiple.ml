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
  (*match*/268 = 3
   *match*/269 = 2
   *match*/270 = 1
   *match*/271 = *match*/268
   *match*/272 = *match*/269
   *match*/273 = *match*/270)
  (catch
    (catch
      (catch (if (!= *match*/272 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/271 1) (exit 2) (exit 1)))
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
  (*match*/276 = 3
   *match*/277 = 2
   *match*/278 = 1
   *match*/279 = (makeblock 0 *match*/276 *match*/277 *match*/278))
  (catch
    (catch
      (let (*match*/280 =a (field 0 *match*/279))
        (catch
          (let (*match*/281 =a (field 1 *match*/279))
            (if (!= *match*/281 3) (exit 7)
              (let (*match*/282 =a (field 2 *match*/279))
                (exit 5 *match*/279))))
         with (7)
          (if (!= *match*/280 1) (exit 6)
            (let
              (*match*/284 =a (field 2 *match*/279)
               *match*/283 =a (field 1 *match*/279))
              (exit 5 *match*/279)))))
     with (6) 0)
   with (5 x/274[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/274) 1)))
- : bool = false
|}];;
