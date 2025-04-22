(* TEST
 flags = "-nostdlib -nopervasives -dlambda";
 expect;
*)

(******************************************************************************)

(* Check that the extra split indeed happens when the last row is made of
   "variables" only *)

let last_is_anys = function
  | true, false -> 1
  | _, false -> 2
  | _, _ -> 3
;;
[%%expect{|
(let
  (last_is_anys/14 =
     (function {nlocal = 0}
       param/16[(consts ()) (non_consts ([0: [int], [int]]))] : int
       (catch
         (if (field_imm 0 param/16) (if (field_imm 1 param/16) (exit 1) 1)
           (if (field_imm 1 param/16) (exit 1) 2))
        with (1) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_anys" last_is_anys/14))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/21 =
     (function {nlocal = 0}
       param/25[(consts ()) (non_consts ([0: [int], [int]]))] : int
       (catch
         (if (field_imm 0 param/25) (if (field_imm 1 param/25) (exit 3) 1)
           (if (field_imm 1 param/25) (exit 3) 2))
        with (3) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_vars" last_is_vars/21))
val last_is_vars : bool * bool -> int = <fun>
|}]

(******************************************************************************)

(* Check that the [| _, false, true -> 12] gets raised. *)

type t = ..
type t += A | B of unit | C of bool * int;;
[%%expect{|
0
type t = ..
(let
  (A/29 = (makeblock_unique 248 "A" (caml_fresh_oo_id 0))
   B/30 = (makeblock_unique 248 "B" (caml_fresh_oo_id 0))
   C/31 = (makeblock_unique 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_imm 1 (global Toploop!)) "A/29" A/29)
    (apply (field_imm 1 (global Toploop!)) "B/30" B/30)
    (apply (field_imm 1 (global Toploop!)) "C/31" C/31)))
type t += A | B of unit | C of bool * int
|}]

let f = function
  | A, true, _ -> 1
  | _, false, false -> 11
  | B _, true, _ -> 2
  | C _, true, _ -> 3
  | _, false, true -> 12
  | _ -> 4
;;
[%%expect{|
(let
  (C/31 = (apply (field_imm 0 (global Toploop!)) "C/31")
   B/30 = (apply (field_imm 0 (global Toploop!)) "B/30")
   A/29 = (apply (field_imm 0 (global Toploop!)) "A/29")
   f/32 =
     (function {nlocal = 0}
       param/34[(consts ()) (non_consts ([0: *, [int], [int]]))] : int
       (let (*match*/35 =a (field_imm 0 param/34))
         (catch
           (if (%eq *match*/35 A/29) (if (field_imm 1 param/34) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/34)
             (if (%eq (field_imm 0 *match*/35) B/30) 2
               (if (%eq (field_imm 0 *match*/35) C/31) 3 4))
             (if (field_imm 2 param/34) 12 11))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/32))
val f : t * bool * bool -> int = <fun>
|}]
