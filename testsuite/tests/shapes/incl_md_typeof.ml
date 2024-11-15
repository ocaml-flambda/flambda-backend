(* TEST
 flags = "-dshape";
 expect;
*)

module Foo : sig
  module Bar : sig
  end
end = struct
  module Bar = struct
  end
end
;;
[%%expect{|
{
 "Foo"[module] -> {<.4>
                   "Bar"[module] -> {<.2>};
                   };
 }
module Foo : sig module Bar : sig end end
|}]

module type Extended = sig
  include module type of struct include Foo end
  module Bar : sig
    include module type of struct include Bar end
  end
end
;;
[%%expect{|
{
 "Extended"[module type] -> <.6>;
 }
module type Extended = sig module Bar : sig end end
|}]

module E : Extended = struct
  module Bar = struct end
end

[%%expect{|
{
 "E"[module] -> {<.8>
                 "Bar"[module] -> {<.7>};
                 };
 }
module E : Extended
|}]
