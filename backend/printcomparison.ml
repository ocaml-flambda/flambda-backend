open Format

let integer_comparison : Comparison.Integer.t -> string = function
  | Ceq -> "=="
  | Cne -> "!="
  | Clt -> "<"
  | Cle -> "<="
  | Cgt -> ">"
  | Cge -> ">="

let float_comparison : Comparison.Float.t -> string = function
  | CFeq -> "=="
  | CFneq -> "!="
  | CFlt -> "<"
  | CFnlt -> "!<"
  | CFle -> "<="
  | CFnle -> "!<="
  | CFgt -> ">"
  | CFngt -> "!>"
  | CFge -> ">="
  | CFnge -> "!>="

let intcomp : Comparison.Integer.With_signedness.t -> string = function
  | Isigned c -> Printf.sprintf " %ss " (integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (integer_comparison c)

let floatcomp c =
    Printf.sprintf " %sf " (float_comparison c)

let test reg tst ppf arg =
  match (tst : Comparison.Test.t) with
  | Itruetest -> reg ppf arg.(0)
  | Ifalsetest -> fprintf ppf "not %a" reg arg.(0)
  | Iinttest cmp -> fprintf ppf "%a%s%a" reg arg.(0) (intcomp cmp) reg arg.(1)
  | Iinttest_imm(cmp, n) -> fprintf ppf "%a%s%i" reg arg.(0) (intcomp cmp) n
  | Ifloattest cmp ->
      fprintf ppf "%a%s%a"
       reg arg.(0) (floatcomp cmp) reg arg.(1)
  | Ieventest -> fprintf ppf "%a & 1 == 0" reg arg.(0)
  | Ioddtest -> fprintf ppf "%a & 1 == 1" reg arg.(0)
