(* TEST
 include stable;
 {
   flags = "-extension layouts";
   expect;
 }
*)

module M = struct
  let id = fun x -> x
  let () = Format.printf "%f %s\n" (Stable.Float_u.to_float (id #1.)) (id "abc")
end
[%%expect{|
Line 3, characters 74-79:
3 |   let () = Format.printf "%f %s\n" (Stable.Float_u.to_float (id #1.)) (id "abc")
                                                                              ^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : float64 mod internal once shared local)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of float64, because
         of the definition of id at line 2, characters 11-21.
|}]
