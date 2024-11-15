(* TEST
 flags = "-dshape";
 expect;
*)

(* Everything that couldn't go anywhere else. *)

open struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{}
module M : sig type t = A end
|}]

include M
[%%expect{|
{
 "t"[type] -> {<.2>
               "A"[constructor] -> {<.3>};
               };
 }
type t = M.t = A
|}]

module N = M
[%%expect{|
{
 "N"[module] ->
   Alias(<.5>
         {<.4>
          "t"[type] -> {<.2>
                        "A"[constructor] -> {<.3>};
                        };
          });
 }
module N = M
|}]

(* Not open structs, but the code handling the following is currently very
   similar to the one for open struct (i.e. calls [Env.enter_signature]), and
   so we are likely to encounter the same bugs, if any. *)

include struct
  module M' = struct
    type t = A
  end
end
[%%expect{|
{
 "M'"[module] -> {<.8>
                  "t"[type] -> {<.6>
                                "A"[constructor] -> {<.7>};
                                };
                  };
 }
module M' : sig type t = A end
|}]

module N' = M'
[%%expect{|
{
 "N'"[module] ->
   Alias(<.9>
         {<.8>
          "t"[type] -> {<.6>
                        "A"[constructor] -> {<.7>};
                        };
          });
 }
module N' = M'
|}]

module Test = struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 "Test"[module] ->
   {<.13>
    "M"[module] -> {<.12>
                    "t"[type] -> {<.10>
                                  "A"[constructor] -> {<.11>};
                                  };
                    };
    };
 }
module Test : sig module M : sig type t = A end end
|}]

include Test
[%%expect{|
{
 "M"[module] -> {<.12>
                 "t"[type] -> {<.10>
                               "A"[constructor] -> {<.11>};
                               };
                 };
 }
module M = Test.M
|}]

module N = M
[%%expect{|
{
 "N"[module] ->
   Alias(<.14>
         {<.12>
          "t"[type] -> {<.10>
                        "A"[constructor] -> {<.11>};
                        };
          });
 }
module N = M
|}]
