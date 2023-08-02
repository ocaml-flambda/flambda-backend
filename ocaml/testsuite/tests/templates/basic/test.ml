(* TEST

readonly_files =
  "category.ml category.mli \
   category_of_monoid.ml category_of_monoid.mli \
   category_utils.ml category_utils.mli \
   chain.ml chain.mli \
   import.ml \
   list_element.mli \
   list_monoid.ml list_monoid.mli \
   main.ml main.mli main.reference \
   monoid.mli \
   monoid_of_semigroup.ml monoid_of_semigroup.mli \
   monoid_utils.ml monoid_utils.mli \
   semigroup.mli"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-as-parameter"
module = "monoid.mli"
*** ocamlc.byte
module = "semigroup.mli"
**** ocamlc.byte
module = "category.mli"
***** ocamlc.byte
flags = "-parameter Semigroup -as-argument-for Monoid"
module = "monoid_of_semigroup.mli"
****** ocamlc.byte
module = "monoid_of_semigroup.ml"
******* ocamlc.byte
flags = "-as-parameter"
module = "list_element.mli"
******** ocamlc.byte
flags = "-parameter List_element -as-argument-for Monoid"
module = "list_monoid.mli"
********* ocamlc.byte
module = "list_monoid.ml"
********** ocamlc.byte
flags = "-parameter Monoid"
module = "monoid_utils.mli"
*********** ocamlc.byte
module = "monoid_utils.ml"
************ ocamlc.byte
flags = "-parameter Category"
module = "chain.mli"
************* ocamlc.byte
module = "chain.ml"
************** ocamlc.byte
flags = "-parameter Category"
module = "category_utils.mli"
*************** ocamlc.byte
module = "category_utils.ml"
**************** ocamlc.byte
flags = "-parameter Monoid -as-argument-for Category"
module = "category_of_monoid.mli"
***************** ocamlc.byte
module = "category_of_monoid.ml"
****************** ocamlc.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "import.ml"
******************* ocamlc.byte
flags = "-parameter Semigroup -parameter List_element -w -misplaced-attribute"
module = "main.mli"
******************** ocamlc.byte
flags += " -i"
module = "main.ml"
********************* check-ocamlc.byte-output
compiler_reference = "main.reference"
******************** ocamlc.byte
module = "main.ml"
********************* ocamlobjinfo
program = "main.cmo"
********************** ocamlobjinfo
program = "main.cmi"
*********************** check-program-output
*)
