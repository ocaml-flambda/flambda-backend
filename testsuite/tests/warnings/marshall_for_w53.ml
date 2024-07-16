<<<<<<< HEAD
let w53_ast =
  Pparse.parse_implementation ~tool_name:"w53_test" "w53.ml"

let () = Pparse.write_ast Pparse.Structure "w53.marshalled.ml" w53_ast

let w53_zero_alloc_all_ast =
  Pparse.parse_implementation ~tool_name:"w53_zero_alloc_all_test"
    "w53_zero_alloc_all.ml"

let () = Pparse.write_ast Pparse.Structure "w53_zero_alloc_all.marshalled.ml"
           w53_zero_alloc_all_ast
||||||| 121bedcfd2
=======
let w53_ast =
  Pparse.parse_implementation ~tool_name:"w53_test" "w53.ml"

let () = Pparse.write_ast Pparse.Structure "w53.marshalled.ml" w53_ast
>>>>>>> 5.2.0
