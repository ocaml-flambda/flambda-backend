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
  (last_is_anys/11 =
<<<<<<< HEAD
     (function {nlocal = 0}
       param/13[(consts ()) (non_consts ([0: [int], [int]]))] : int
||||||| merged common ancestors
  (last_is_anys/10 =
     (function param/12 : int
=======
     (function param/13 : int
>>>>>>> ocaml/5.1
       (catch
<<<<<<< HEAD
         (if (field 0 param/13) (if (field 1 param/13) (exit 1) 1)
           (if (field 1 param/13) (exit 1) 2))
||||||| merged common ancestors
         (if (field 0 param/12) (if (field 1 param/12) (exit 1) 1)
           (if (field 1 param/12) (exit 1) 2))
=======
         (if (field_imm 0 param/13) (if (field_imm 1 param/13) (exit 1) 1)
           (if (field_imm 1 param/13) (exit 1) 2))
>>>>>>> ocaml/5.1
        with (1) 3)))
<<<<<<< HEAD
  (apply (field 1 (global Toploop!)) "last_is_anys" last_is_anys/11))
||||||| merged common ancestors
  (apply (field 1 (global Toploop!)) "last_is_anys" last_is_anys/10))
=======
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/11))
>>>>>>> ocaml/5.1
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/18 =
<<<<<<< HEAD
     (function {nlocal = 0}
       param/22[(consts ()) (non_consts ([0: [int], [int]]))] : int
||||||| merged common ancestors
  (last_is_vars/17 =
     (function param/21 : int
=======
     (function param/22 : int
>>>>>>> ocaml/5.1
       (catch
<<<<<<< HEAD
         (if (field 0 param/22) (if (field 1 param/22) (exit 3) 1)
           (if (field 1 param/22) (exit 3) 2))
||||||| merged common ancestors
         (if (field 0 param/21) (if (field 1 param/21) (exit 3) 1)
           (if (field 1 param/21) (exit 3) 2))
=======
         (if (field_imm 0 param/22) (if (field_imm 1 param/22) (exit 3) 1)
           (if (field_imm 1 param/22) (exit 3) 2))
>>>>>>> ocaml/5.1
        with (3) 3)))
<<<<<<< HEAD
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/18))
||||||| merged common ancestors
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/17))
=======
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/18))
>>>>>>> ocaml/5.1
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
  (A/26 = (makeblock_unique 248 "A" (caml_fresh_oo_id 0))
   B/27 = (makeblock_unique 248 "B" (caml_fresh_oo_id 0))
   C/28 = (makeblock_unique 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/26" A/26)
    (apply (field 1 (global Toploop!)) "B/27" B/27)
    (apply (field 1 (global Toploop!)) "C/28" C/28)))
||||||| merged common ancestors
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
  (seq (apply (field_mut 1 (global Toploop!)) "A/26" A/26)
    (apply (field_mut 1 (global Toploop!)) "B/27" B/27)
    (apply (field_mut 1 (global Toploop!)) "C/28" C/28)))
>>>>>>> ocaml/5.1
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
  (C/28 = (apply (field 0 (global Toploop!)) "C/28")
   B/27 = (apply (field 0 (global Toploop!)) "B/27")
   A/26 = (apply (field 0 (global Toploop!)) "A/26")
   f/29 =
     (function {nlocal = 0}
       param/31[(consts ()) (non_consts ([0: *, [int], [int]]))] : int
       (let (*match*/32 =a (field 0 param/31))
||||||| merged common ancestors
  (C/27 = (apply (field 0 (global Toploop!)) "C/27")
   B/26 = (apply (field 0 (global Toploop!)) "B/26")
   A/25 = (apply (field 0 (global Toploop!)) "A/25")
   f/28 =
     (function param/30 : int
       (let (*match*/31 =a (field 0 param/30))
=======
  (C/28 = (apply (field_mut 0 (global Toploop!)) "C/28")
   B/27 = (apply (field_mut 0 (global Toploop!)) "B/27")
   A/26 = (apply (field_mut 0 (global Toploop!)) "A/26")
   f/29 =
     (function param/31 : int
       (let (*match*/32 =a (field_imm 0 param/31))
>>>>>>> ocaml/5.1
         (catch
<<<<<<< HEAD
           (if (== *match*/32 A/26) (if (field 1 param/31) 1 (exit 8))
||||||| merged common ancestors
           (if (== *match*/31 A/25) (if (field 1 param/30) 1 (exit 8))
=======
           (if (== *match*/32 A/26) (if (field_imm 1 param/31) 1 (exit 8))
>>>>>>> ocaml/5.1
             (exit 8))
          with (8)
<<<<<<< HEAD
           (if (field 1 param/31)
             (if (== (field 0 *match*/32) B/27) 2
               (if (== (field 0 *match*/32) C/28) 3 4))
             (if (field 2 param/31) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/29))
||||||| merged common ancestors
           (if (field 1 param/30)
             (if (== (field 0 *match*/31) B/26) 2
               (if (== (field 0 *match*/31) C/27) 3 4))
             (if (field 2 param/30) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/28))
=======
           (if (field_imm 1 param/31)
             (if (== (field_imm 0 *match*/32) B/27) 2
               (if (== (field_imm 0 *match*/32) C/28) 3 4))
             (if (field_imm 2 param/31) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/29))
>>>>>>> ocaml/5.1
val f : t * bool * bool -> int = <fun>
|}]
