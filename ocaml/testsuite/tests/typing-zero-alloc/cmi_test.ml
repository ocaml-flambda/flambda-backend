(* TEST
   readonly_files = "cmi_test_lib.ml";
   setup-ocamlc.byte-build-env;
   module = "cmi_test_lib.ml";
   ocamlc.byte;
   flags += "-I ocamlc.byte";
   expect;
*)

(* Here we show the signatures of [cmi_test_a] and the modules within it do not
   have zero_alloc variables - we can't add further zero_alloc constraints. *)
module M1 : sig
  val[@zero_alloc] f_unconstrained_variable : int -> int
end = Cmi_test_lib
[%%expect{|
Line 3, characters 6-18:
3 | end = Cmi_test_lib
          ^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           val f_unconstrained_variable : int -> int
           module M_constrained_variable =
             Cmi_test_lib.M_constrained_variable
           module M_no_variable = Cmi_test_lib.M_no_variable
         end
       is not included in
         sig val f_unconstrained_variable : int -> int [@@zero_alloc] end
       Values do not match:
         val f_unconstrained_variable : int -> int
       is not included in
         val f_unconstrained_variable : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
       File "cmi_test_lib.ml", line 4, characters 4-28: Actual declaration
|}]

module M2 : sig
  val[@zero_alloc strict] f : int -> int
end = Cmi_test_lib.M_constrained_variable
[%%expect{|
Line 3, characters 6-41:
3 | end = Cmi_test_lib.M_constrained_variable
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int [@@zero_alloc] end
       is not included in
         sig val f : int -> int [@@zero_alloc strict] end
       Values do not match:
         val f : int -> int [@@zero_alloc]
       is not included in
         val f : int -> int [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       File "cmi_test_lib.ml", line 7, characters 6-7: Actual declaration
|}]

module M3 : sig
  val[@zero_alloc] f : int -> int
end = Cmi_test_lib.M_no_variable
[%%expect{|
Line 3, characters 6-32:
3 | end = Cmi_test_lib.M_no_variable
          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> int [@@zero_alloc] end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
       File "cmi_test_lib.ml", line 13, characters 2-20: Actual declaration
|}]
