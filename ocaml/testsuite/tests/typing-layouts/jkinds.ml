(* TEST
   * expect
*)


type 'a list : immutable_data with 'a

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type ('a, 'b) either : immutable_data with 'a * 'b

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type 'a gel : kind_of_ 'a mod global

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type 'a t : _

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immediate = value mod global unique many sync uncontended

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immutable_data = value mod sync uncontended many

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ immutable = value mod uncontended

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

kind_abbrev_ data = value mod sync many

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]

module type S = sig
  type 'a list : immutable_data with 'a
  type ('a, 'b) either : immutable_data with 'a * 'b
  type 'a gel : kind_of_ 'a mod global
  type 'a t : _
  kind_abbrev_ immediate = value mod global unique many sync uncontended
  kind_abbrev_ immutable_data = value mod sync uncontended many
  kind_abbrev_ immutable = value mod uncontended
  kind_abbrev_ data = value mod sync many
end

[%%expect{|
>> Fatal error: XXX unimplemented
Uncaught exception: Misc.Fatal_error

|}]




