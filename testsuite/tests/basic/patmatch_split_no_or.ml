(* TEST
 flags = "-nostdlib -nopervasives -dlambda"
 * expect
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
<<<<<<< HEAD
  (last_is_anys/10 =
     (function param/12[(consts ()) (non_consts ([0: [int], [int]]))] : int
||||||| 24dbb0976a
  (last_is_anys/10 =
     (function param/12 : int
=======
  (last_is_anys/11 =
     (function param/13 : int
>>>>>>> ocaml/4.14
       (catch
         (if (field 0 param/13) (if (field 1 param/13) (exit 1) 1)
           (if (field 1 param/13) (exit 1) 2))
        with (1) 3)))
  (apply (field 1 (global Toploop!)) "last_is_anys" last_is_anys/11))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
<<<<<<< HEAD
  (last_is_vars/17 =
     (function param/21[(consts ()) (non_consts ([0: [int], [int]]))] : int
||||||| 24dbb0976a
  (last_is_vars/17 =
     (function param/21 : int
=======
  (last_is_vars/18 =
     (function param/22 : int
>>>>>>> ocaml/4.14
       (catch
         (if (field 0 param/22) (if (field 1 param/22) (exit 3) 1)
           (if (field 1 param/22) (exit 3) 2))
        with (3) 3)))
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/18))
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
<<<<<<< HEAD
  (A/25 = (makeblock_unique 248 "A" (caml_fresh_oo_id 0))
   B/26 = (makeblock_unique 248 "B" (caml_fresh_oo_id 0))
   C/27 = (makeblock_unique 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/25" A/25)
    (apply (field 1 (global Toploop!)) "B/26" B/26)
    (apply (field 1 (global Toploop!)) "C/27" C/27)))
||||||| 24dbb0976a
  (A/25 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/26 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/27 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/25" A/25)
    (apply (field 1 (global Toploop!)) "B/26" B/26)
    (apply (field 1 (global Toploop!)) "C/27" C/27)))
=======
  (A/26 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/27 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/28 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/26" A/26)
    (apply (field 1 (global Toploop!)) "B/27" B/27)
    (apply (field 1 (global Toploop!)) "C/28" C/28)))
>>>>>>> ocaml/4.14
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
<<<<<<< HEAD
  (C/27 = (apply (field 0 (global Toploop!)) "C/27")
   B/26 = (apply (field 0 (global Toploop!)) "B/26")
   A/25 = (apply (field 0 (global Toploop!)) "A/25")
   f/28 =
     (function param/30[(consts ()) (non_consts ([0: *, [int], [int]]))]
       : int
       (let (*match*/31 =a (field 0 param/30))
||||||| 24dbb0976a
  (C/27 = (apply (field 0 (global Toploop!)) "C/27")
   B/26 = (apply (field 0 (global Toploop!)) "B/26")
   A/25 = (apply (field 0 (global Toploop!)) "A/25")
   f/28 =
     (function param/30 : int
       (let (*match*/31 =a (field 0 param/30))
=======
  (C/28 = (apply (field 0 (global Toploop!)) "C/28")
   B/27 = (apply (field 0 (global Toploop!)) "B/27")
   A/26 = (apply (field 0 (global Toploop!)) "A/26")
   f/29 =
     (function param/31 : int
       (let (*match*/32 =a (field 0 param/31))
>>>>>>> ocaml/4.14
         (catch
           (if (== *match*/32 A/26) (if (field 1 param/31) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field 1 param/31)
             (if (== (field 0 *match*/32) B/27) 2
               (if (== (field 0 *match*/32) C/28) 3 4))
             (if (field 2 param/31) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/29))
val f : t * bool * bool -> int = <fun>
|}]
