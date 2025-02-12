(* TEST
 expect;
*)

(* Test bytecode printing of values *)

(* All-float *)
type t = { flt : float; uflt : float# }

let t = { flt = 4.0; uflt = #5.0 }

[%%expect {|
type t = { flt : float; uflt : float#; }
val t : t = {flt = 4.; uflt = <abstr>}
|}];;

(* Non-empty value prefix *)
type t = { uflt : float#; str : string }

let t = { uflt = #5.0; str = "str" }

[%%expect {|
type t = { uflt : float#; str : string; }
val t : t = {uflt = <abstr>; str = "str"}
|}];;

(* Flat suffix mixes float# and imm *)
type t = { str : string; uflt : float#; imm : int }

let t = { str = "str"; uflt = #5.0; imm = 5 }

[%%expect {|
type t = { str : string; uflt : float#; imm : int; }
val t : t = {str = "str"; uflt = <abstr>; imm = 5}
|}];;
