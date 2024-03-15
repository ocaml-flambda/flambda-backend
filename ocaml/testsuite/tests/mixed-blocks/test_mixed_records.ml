(* TEST
  flags = "-extension layouts"
  * expect
 *)

(* Mixed float-float# blocks are always OK. *)
type ok1 =
  { a : float;
    b : float#;
  }

[%%expect{|
type ok1 = { a : float; b : float#; }
|}];;

(* Mixed float-float# blocks are always OK. *)
type ok2 =
  { a : float#;
    b : float;
  }

[%%expect{|
type ok2 = { a : float#; b : float; }
|}];;

(* When a non-float/float# field appears, [float]
   fields are no longer considered flat. *)
type err1 =
  { a : float#;
    b : float;
    c : int;
  }

[%%expect{|
Line 3, characters 4-14:
3 |     b : float;
        ^^^^^^^^^^
Error: Expected all flat fields after non-value field, a,
        but found boxed field, b.
|}];;

(* [float] appearing as a non-flat field in the value prefix. *)
type ok3 =
  { a : float;
    b : float#;
    c : int;
  }

[%%expect{|
type ok3 = { a : float; b : float#; c : int; }
|}];;

(* The field [c] can't be flat because a non-float/float# field [d] appears. *)
type err2 =
  { a : float;
    b : float#;
    c : float;
    d : int;
  }

[%%expect{|
Line 4, characters 4-14:
4 |     c : float;
        ^^^^^^^^^^
Error: Expected all flat fields after non-value field, b,
        but found boxed field, c.
|}];;

(* String can't appear in the flat suffix *)
type err3 =
  { a : float#;
    b : string;
  }

[%%expect{|
Line 3, characters 4-15:
3 |     b : string;
        ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, a,
        but found boxed field, b.
|}];;

(* [f3] can be flat because all other fields are float/float#,
   so it can appear in the flat suffix.
 *)
type ok4 =
  { f1 : float#;
    f2 : float#;
    f3 : float;
  }

[%%expect{|
type ok4 = { f1 : float#; f2 : float#; f3 : float; }
|}];;

(* The string [f3] can't appear in the flat suffix. *)
type err5 =
  { f1 : float#;
    f2 : float#;
    f3 : string;
  }

[%%expect{|
Line 4, characters 4-16:
4 |     f3 : string;
        ^^^^^^^^^^^^
Error: Expected all flat fields after non-value field, f1,
        but found boxed field, f3.
|}];;

(* The int [c] can appear in the flat suffix. *)
type ok5 =
  { a : float#;
    b : float#;
    c : int;
  }

[%%expect{|
type ok5 = { a : float#; b : float#; c : int; }
|}];;
