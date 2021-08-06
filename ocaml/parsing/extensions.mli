open Parsetree

type extension_expr = 
| Eexp_list_comprehension of expression * comprehension list 
| Eexp_arr_comprehension of  expression * comprehension list 

and comprehension =
{ 
   clauses: comprehension_clause list;
   guard : expression option 
}

and comprehension_clause = 
(*[ body for i = E2 to E3 ]      (flag = Upto)
 [ body for i = E2 downto E3 ]  (flag = downto)*)
| From_to of pattern * expression *
   expression * Asttypes.direction_flag
(*[ body for i in E3 ]      *)
| In of pattern * expression

val payload_of_extension_expr: loc:Warnings.loc -> extension_expr -> extension
val extension_expr_of_payload: loc:Warnings.loc -> extension -> extension_expr
