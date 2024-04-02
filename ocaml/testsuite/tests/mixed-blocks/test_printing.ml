(* TEST
   flags = "-extension layouts_alpha"

   * runtime5
   ** expect
 *)

(* Test bytecode printing of values *)

(* All-float *)
type t = { flt : float; uflt : float# }

let t = { flt = 4.0; uflt = #5.0 }

[%%expect {|
|}];;

(* Non-empty value prefix *)
type t = { str : string; uflt : float# }

let t = { str = "str"; uflt = #5.0 }

[%%expect {|
|}];;

(* Flat suffix mixes float# and imm *)
type t = { str : string; uflt : float#; imm : int }

let t = { str = "str"; uflt = #5.0; imm = 5 }

[%%expect {|
|}];;
