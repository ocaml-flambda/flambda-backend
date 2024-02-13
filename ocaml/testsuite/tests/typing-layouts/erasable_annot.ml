(* TEST
   * expect
   flags = "-extension layouts -only-erasable-extensions"
*)

(* Upstream compatible usages of immediate/immediate64 are allowed *)
module type S1 = sig
  type t_immediate : immediate
  type t_immediate64 : immediate64
end;;
[%%expect {|
module type S1 =
  sig type t_immediate : immediate type t_immediate64 : immediate64 end
|}];;

(* Same is not true when constraining type vars *)
(* immediate *)
module type S = sig
  val f_immediate : ('a : immediate). 'a -> 'a -> 'a
end;;
[%%expect {|
Line 2, characters 2-52:
2 |   val f_immediate : ('a : immediate). 'a -> 'a -> 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in f_immediate can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

module type S = sig
  val f_immediate : ('a : immediate) -> 'a -> 'a
end;;
[%%expect {|
Line 2, characters 2-48:
2 |   val f_immediate : ('a : immediate) -> 'a -> 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in f_immediate can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

module type S = sig
  type ('a : immediate) t
end;;
[%%expect {|
Line 2, characters 2-25:
2 |   type ('a : immediate) t
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in t can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

module type S = sig
  type _ g = | MkG : ('a : immediate). 'a g
end;;
[%%expect {|
Line 2, characters 2-43:
2 |   type _ g = | MkG : ('a : immediate). 'a g
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in g can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

let f (type a : immediate): a -> a = fun x -> x
[%%expect {|
Line 1, characters 4-5:
1 | let f (type a : immediate): a -> a = fun x -> x
        ^
Error: Usage of layout immediate/immediate64 in f can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

let f x = (x : (_ : immediate))
[%%expect {|
Line 1, characters 4-5:
1 | let f x = (x : (_ : immediate))
        ^
Error: Usage of layout immediate/immediate64 in f can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

let f v: ((_ : immediate)[@error_message "Custom message"]) = v
[%%expect {|
Line 1, characters 4-5:
1 | let f v: ((_ : immediate)[@error_message "Custom message"]) = v
        ^
Error: Usage of layout immediate/immediate64 in f can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

(* immediate64 *)
module type S = sig
  val f_immediate64 : ('a : immediate64). 'a -> 'a -> 'a
end;;
[%%expect {|
Line 2, characters 2-56:
2 |   val f_immediate64 : ('a : immediate64). 'a -> 'a -> 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in f_immediate64 can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

module type S = sig
  val f_immediate64 : ('a : immediate64) -> 'a -> 'a
end;;
[%%expect {|
Line 2, characters 2-52:
2 |   val f_immediate64 : ('a : immediate64) -> 'a -> 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in f_immediate64 can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

module type S = sig
  type ('a : immediate64) t
end;;
[%%expect {|
Line 2, characters 2-27:
2 |   type ('a : immediate64) t
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in t can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

module type S = sig
  type _ g = | MkG : ('a : immediate64). 'a g
end;;
[%%expect {|
Line 2, characters 2-45:
2 |   type _ g = | MkG : ('a : immediate64). 'a g
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Usage of layout immediate/immediate64 in g can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

let f (type a : immediate64): a -> a = fun x -> x
[%%expect {|
Line 1, characters 4-5:
1 | let f (type a : immediate64): a -> a = fun x -> x
        ^
Error: Usage of layout immediate/immediate64 in f can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

let f x = (x : (_ : immediate64))
[%%expect {|
Line 1, characters 4-5:
1 | let f x = (x : (_ : immediate64))
        ^
Error: Usage of layout immediate/immediate64 in f can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

let f v: ((_ : immediate64)[@error_message "Custom message"]) = v
[%%expect {|
Line 1, characters 4-5:
1 | let f v: ((_ : immediate64)[@error_message "Custom message"]) = v
        ^
Error: Usage of layout immediate/immediate64 in f can't be erased.
       This error is produced due to the use of -only-erasable-extensions.
|}];;

(* CR layouts: This message should change after we fix the package hack.
   But it should still be an error under [-only-erasable-extensions]. *)
module type S = sig
  type t[@@immediate64]
end

module type K = sig
  val f : 'a -> (module S with type t = 'a) -> 'a
end

[%%expect {|
module type S = sig type t : immediate64 end
Line 6, characters 16-43:
6 |   val f : 'a -> (module S with type t = 'a) -> 'a
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate64
       The layout of the first is value, because
         it's a type declaration in a first-class module.
       But the layout of the first must be a sublayout of immediate64, because
         of the definition of t at line 2, characters 2-23.
|}];;

(* Other annotations are not effected by this flag *)
module type S = sig
  val f_any : ('a : any). ('a : any) -> (('a : any)[@error_message ""])
  type ('a : any) t_any : any
  type (_ : any) t_any_ = MkG : ('a : any). 'a t_any_
  val f_bits64 : ('a : bits64). ('a : bits64) -> (('a : bits64)[@error_message ""])
  type ('a : bits64) t_bits64 : bits64
  type (_ : bits64) t_bits64_ = MkG : ('a : bits64). 'a t_bits64_
  val f_bits32 : ('a : bits32). ('a : bits32) -> (('a : bits32)[@error_message ""])
  type ('a : bits32) t_bits32 : bits32
  type (_ : bits32) t_bits32_ = MkG : ('a : bits32). 'a t_bits32_
  val f_float64 : ('a : float64). ('a : float64) -> (('a : float64)[@error_message ""])
  type ('a : float64) t_float64 : float64
  type (_ : float64) t_float64_ = MkG : ('a : float64). 'a t_float64_
  val f_word : ('a : word). ('a : word) -> (('a : word)[@error_message ""])
  type ('a : word) t_word : word
  type (_ : word) t_word_ = MkG : ('a : word). 'a t_word_
end

module M = struct
  let f_any (type a : any) = ()
  let f_bits64 (type a : bits64) = ()
  let f_bits32 (type a : bits32) = ()
  let f_float64 (type a : float64) = ()
  let f_word (type a : word) = ()
end
[%%expect {|
module type S =
  sig
    val f_any : ('a : any). 'a -> 'a
    type ('a : any) t_any : any
    type (_ : any) t_any_ = MkG : ('a : any). 'a t_any_
    val f_bits64 : ('a : bits64). 'a -> 'a
    type ('a : bits64) t_bits64 : bits64
    type (_ : bits64) t_bits64_ = MkG : ('a : bits64). 'a t_bits64_
    val f_bits32 : ('a : bits32). 'a -> 'a
    type ('a : bits32) t_bits32 : bits32
    type (_ : bits32) t_bits32_ = MkG : ('a : bits32). 'a t_bits32_
    val f_float64 : ('a : float64). 'a -> 'a
    type ('a : float64) t_float64 : float64
    type (_ : float64) t_float64_ = MkG : ('a : float64). 'a t_float64_
    val f_word : ('a : word). 'a -> 'a
    type ('a : word) t_word : word
    type (_ : word) t_word_ = MkG : ('a : word). 'a t_word_
  end
module M :
  sig
    val f_any : unit
    val f_bits64 : unit
    val f_bits32 : unit
    val f_float64 : unit
    val f_word : unit
  end
|}];;
