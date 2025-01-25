(* TEST
   flags = "-disable-all-extensions";
   expect;
*)

(* This file is a good place to dump tests that ensure that we check
   for extension enabledness before allowing a construct. We haven't been very
   organized about this -- some constructs have their own dedicated test for
   this, and some constructs don't have a test at all.
 *)

let iarray = [: :]


[%%expect{|
Line 1, characters 13-18:
1 | let iarray = [: :]
                 ^^^^^
Error: The extension "immutable_arrays" is disabled and cannot be used
|}]

let iarray_pattern x =
  match x with
  | [: :] -> ()

[%%expect{|
Line 3, characters 4-9:
3 |   | [: :] -> ()
        ^^^^^
Error: The extension "immutable_arrays" is disabled and cannot be used
|}]


module Module_instance = A(B)(C) [@jane.non_erasable.instances]

[%%expect{|
Line 1, characters 25-63:
1 | module Module_instance = A(B)(C) [@jane.non_erasable.instances]
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "instances" is disabled and cannot be used
|}]

module type Module_strengthening = sig end with Int

[%%expect{|
Line 1, characters 35-51:
1 | module type Module_strengthening = sig end with Int
                                       ^^^^^^^^^^^^^^^^
Error: The extension "module_strengthening" is disabled and cannot be used
|}]

let list_comprehensions = [ x for x in [] ]
[%%expect{|
Line 1, characters 26-43:
1 | let list_comprehensions = [ x for x in [] ]
                              ^^^^^^^^^^^^^^^^^
Error: The extension "comprehensions" is disabled and cannot be used
|}]

let array_comprehensions = [| x for x in [||] |]
[%%expect{|
Line 1, characters 27-48:
1 | let array_comprehensions = [| x for x in [||] |]
                               ^^^^^^^^^^^^^^^^^^^^^
Error: The extension "comprehensions" is disabled and cannot be used
|}]

let iarray_comprehensions = [: x for x in [::] :]
[%%expect{|
Line 1, characters 28-49:
1 | let iarray_comprehensions = [: x for x in [::] :]
                                ^^^^^^^^^^^^^^^^^^^^^
Error: The extension "comprehensions" is disabled and cannot be used
|}]
