(* TEST
 flags = "-keep-original-error-size";
 expect;
*)
 flags = "-keep-original-error-size";
 expect;
*)


module A = struct
  type a and b and c and d
end

module type S = sig
  module B = A
end

module C : S = struct
  module B = struct
    type a and b and c and d and e and f and g and h
  end
end
[%%expect {|
module A : sig type a and b and c and d end
module type S = sig module B = A end
Lines 9-13, characters 15-3:
 9 | ...............struct
10 |   module B = struct
11 |     type a and b and c and d and e and f and g and h
12 |   end
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module B :
             sig type a and b and c and d and e and f and g and h end
         end
       is not included in
         S
       In module B:
       Modules do not match:
         sig
           type a = B.a
           and b = B.b
           and c = B.c
           and d = B.d
           and e = B.e
           and f = B.f
           and g = B.g
           and h = B.h
         end
       is not included in
         (module A)
|}]

module C : S = struct
  module B = struct
    type a and b and c and d and e and f and g and h
    and a_type_with_extremely_long_long_long_long_long_long_long_long_name
    and a_type_with_extremely_long_long_long_long_long_long_long_long_name0
  end
end
[%%expect {|
Lines 1-7, characters 15-3:
1 | ...............struct
2 |   module B = struct
3 |     type a and b and c and d and e and f and g and h
4 |     and a_type_with_extremely_long_long_long_long_long_long_long_long_name
5 |     and a_type_with_extremely_long_long_long_long_long_long_long_long_name0
6 |   end
7 | end
Error: Signature mismatch:
       ...
       In module B:
       Modules do not match:
         sig
           type a = B.a
           and b = B.b
           and c = B.c
           and d = B.d
           and e = B.e
           and f = B.f
           and g = B.g
           and h = B.h
           and a_type_with_extremely_long_long_long_long_long_long_long_long_name =
               B.a_type_with_extremely_long_long_long_long_long_long_long_long_name
           and a_type_with_extremely_long_long_long_long_long_long_long_long_name0 =
               B.a_type_with_extremely_long_long_long_long_long_long_long_long_name0
         end
       is not included in
         (module A)
|}]

module A = struct
  type a and b and c and d
end

module type S = sig
  module type B = sig
    module C = A
  end
end

module D : S = struct
  module type B = sig
    module C: sig
      type a and b and c and d and e and f and g and h
    end
  end
end
[%%expect{|
module A : sig type a and b and c and d end
module type S = sig module type B = sig module C = A end end
Lines 11-17, characters 15-3:
11 | ...............struct
12 |   module type B = sig
13 |     module C: sig
14 |       type a and b and c and d and e and f and g and h
15 |     end
16 |   end
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type B =
             sig
               module C :
                 sig type a and b and c and d and e and f and g and h end
             end
         end
       is not included in
         S
       Module type declarations do not match:
         module type B =
           sig
             module C :
               sig type a and b and c and d and e and f and g and h end
           end
       does not match
         module type B = sig module C = A end
       At position module type B = <here>
       Module types do not match:
         sig
           module C :
             sig type a and b and c and d and e and f and g and h end
         end
       is not equal to
         sig module C = A end
       At position module type B = sig module C : <here> end
       Modules do not match:
         sig
           type a = C.a
           and b = C.b
           and c = C.c
           and d = C.d
           and e = C.e
           and f = C.f
           and g = C.g
           and h = C.h
         end
       is not included in
         (module A)
|}]

module D : S = struct
  module type B = sig
    module C: sig
      type a and b and c and d and e and f and g and h
      and a_type_with_extremely_long_long_long_long_long_long_long_long_name
      and a_type_with_extremely_long_long_long_long_long_long_long_long_name0
    end
  end
end
[%%expect{|
Lines 1-9, characters 15-3:
1 | ...............struct
2 |   module type B = sig
3 |     module C: sig
4 |       type a and b and c and d and e and f and g and h
5 |       and a_type_with_extremely_long_long_long_long_long_long_long_long_name
6 |       and a_type_with_extremely_long_long_long_long_long_long_long_long_name0
7 |     end
8 |   end
9 | end
Error: Signature mismatch:
       ...
       ...
       ...
       At position module type B = sig module C : <here> end
       Modules do not match:
         sig
           type a = C.a
           and b = C.b
           and c = C.c
           and d = C.d
           and e = C.e
           and f = C.f
           and g = C.g
           and h = C.h
           and a_type_with_extremely_long_long_long_long_long_long_long_long_name =
               C.a_type_with_extremely_long_long_long_long_long_long_long_long_name
           and a_type_with_extremely_long_long_long_long_long_long_long_long_name0 =
               C.a_type_with_extremely_long_long_long_long_long_long_long_long_name0
         end
       is not included in
         (module A)
|}]
