
(* This file was auto-generated based on "flambda_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Expected an identifier for a continuation.\n"
    | 3 ->
        "Expected either * followed by an identifier for an exception continuation, or\nan expression.\n"
    | 232 ->
        "Expected end of file.\n"
    | 157 | 168 ->
        "Expected an infix binary operator or one of: .( ; with in and \n"
    | 169 ->
        "Expected an expression.\n"
    | 166 ->
        "Expected a simple value (variable, symbol, or literal).\n"
    | 162 ->
        "Expected (.\nExample of a block load:\n  var.(2)\n"
    | 163 ->
        "Expected a simple value (variable, symbol, or literal).\n"
    | 164 ->
        "Expected ).\n"
    | 15 ->
        "Expected one or more switch cases, separated by semicolons, surrounded by {}.\nExample:\n  { 1 -> k1; 2 -> k2 }\n"
    | 16 ->
        "Expected one or more switch cases, separated by semicolons, followed by }.\nExample of a switch statement:\n  switch x { 1 -> k1; 2 -> k2 }\n"
    | 26 ->
        "Expected a swich case.\nExample of a switch case:\n  2 -> k2\n"
    | 19 ->
        "Expected -> followed by the identifier of a continuation.\n"
    | 20 ->
        "Expected the identifier of a continuation.\n"
    | 22 ->
        "Expected either ; and a switch case or } to end the list.\n"
    | 23 ->
        "Expected a switch case.\nExample of a switch case:\n  2 -> k2\n"
    | 9 ->
        "Expected a simple value (variable, symbol, or literal).\n"
    | 4 ->
        "Expected an identifier for an exception continuation.\n"
    | 6 ->
        "Expected a top-level expression.\n"
    | 29 ->
        "Expected a variable within a closure.\n"
    | 155 ->
        "Expected a simple value (variable, symbol, or literal).\n"
    | 33 ->
        "Expected an optional \"rec\" followed by a continuation handler.\n"
    | 36 ->
        "Expected \"exn\" or an identifier for a continuation.\n"
    | 40 ->
        "Expected an identifier for a continuation.\n"
    | 35 ->
        "Expected a continuation handler.\nExamples of continuation handlers:\n  k (arg1 (arg2: imm)) { cont k0 (arg1) }\n  exn k1 { cont k3 () }\n"
    | 41 ->
        "Expected an argument list, or an expression surrounded by {}.\nExamples of continuation handlers:\n  k (arg1 (arg2: imm)) { cont k0 (arg1) }\n  k1 { cont k3 () }\n"
    | 42 ->
        "Expected zero or more parameter declarations followed by ).\nExamples of parameter delarations:\n  x\n  (y : val)\n"
    | 60 ->
        "Expected an expression within {}.\n"
    | 43 ->
        "Expected an identifier for a parameter.\n"
    | 44 ->
        "Expected : followed by a kind.\n"
    | 45 ->
        "Expected a kind.\n"
    | 53 ->
        "Expected ).\n"
    | 58 ->
        "Expected a parameter declaration.\nExamples of parameter declarations:\n  x\n  (y : val)\n"
    | 61 ->
        "Expected an expression.\n"
    | 223 ->
        "Expected }.\n"
    | 225 ->
        "Expected \"and\" or \"in\".\n"
    | 230 ->
        "Expected an expression.\n"
    | 226 ->
        "Expected a continuation handler.\nExample of a continuation handler:\n  k (arg1) { cont k2 (arg1) }\n"
    | 227 ->
        "Expected \"and\" or \"in\".\n"
    | 38 ->
        "Expected \"stub\" or an identifier for a continuation.\n"
    | 62 ->
        "Expected a definition.\nExamples of let expressions:\n  let x = 42 in cont k (x)\n  let code f (arg1) my_closure -> k : val * val = cont k (arg1 arg1)\n    and symbol F = closure f in ...\n"
    | 219 ->
        "Expected = followed by a named expression.\n"
    | 220 ->
        "Expected a named expression.\nExamples of named expressions:\n  42\n  project_var var1 my_closure\n  Block 2 (var1 Symbol2 42)\n"
    | 178 ->
        "Expected a closure environment delimited by {}.\nExample of a closure environment:\n  { var1 = Symbol2; var2 = 42 }\n"
    | 179 ->
        "Expected a variable within a closure, followed by = and a simple value.\nExample:\n  var1 = Symbol2\n"
    | 208 ->
        "Expected \"in\" and an expression.\n"
    | 180 ->
        "Expected = and a simple value (variable, symbol, or literal).\n"
    | 181 ->
        "Expected a simple value (variable, symbol, or literal).\n"
    | 186 ->
        "Expected either ; and another closure element or } to end the list of closure\nelements.\n"
    | 187 ->
        "Expected a variable-within-closure binding.\nExample of a closure environment:\n  { var1 = 42; var2 = Symbol2 }\n"
    | 216 ->
        "Expected \"with\", \"in\", or \"and\".\n"
    | 209 ->
        "Expected an expression.\n"
    | 217 ->
        "Expected a variable declaration.\nExamples of variable declarations:\n  _\n  x\n  (_ : val)\n  (x : imm)\n"
    | 64 ->
        "Expected a symbol.\n"
    | 65 ->
        "Expected = followed by a closure expression or a block expression.\nExample of a closure expression:\n  closure code_id\nExample of a block expression:\n  Block 2 (var1 Symbol2 42)\n"
    | 66 ->
        "Expected a closure expression or a block expression.\nExample of a closure expression:\n  closure code_id\nExample of a block expression:\n  Block 2 (var1 Symbol2 42)\n"
    | 192 ->
        "Expected \"with\", \"in\", \"end\", or \"and\".\n"
    | 204 ->
        "Expected an expression.\n"
    | 203 ->
        "Expected \"and\", \"with\", or \"in\".\n"
    | 193 ->
        "Expected \"code\" or \"symbol\".\n"
    | 70 ->
        "Expected a tag.\nExample of a block expression:\n  Block 2 (var1 Symbol2 42)\n"
    | 71 ->
        "Expected a list of simple values, in parentheses.\nExample:\n  (var1 Symbol2 42)\n"
    | 76 | 72 ->
        "Expected a simple value (variable, symbol, or literal).\n"
    | 82 ->
        "Expected \"code\", \"symbol\", \"with\", or \"end\".\nExample of a let code expression with explicit segments:\n  let segment\n    code ident (arg) my_closure -> k = cont k arg\n    and symbol Ident = closure ident\n  end and segment\n    code forty_two (arg1) my_closure <var1> -> k : val =\n      let ans = project_var var1 my_closure in cont k (ans)\n    and symbol Forty_two = closure forty_two\n    with { var1 = 42 }\n  end in ...\n"
    | 83 ->
        "Expected a symbol identifier.\n"
    | 84 ->
        "Expected = followed by a closure expression.\nExample of a let symbol expression:\n  let symbol F_clo = closure f in ...\n"
    | 85 ->
        "Expected a closure expression.\nExample of a closure expression:\n  closure code_id\n"
    | 190 ->
        "Expected \"and\", \"with\", or \"in\".\nExample of a let code expression with explicit segments:\n  let segment\n    code ident (arg) my_closure -> k = cont k arg\n    and symbol ident_clo = closure ident\n  end and segment\n    code forty_two (arg1) my_closure <var1> -> k : val =\n      let ans = project_var var1 my_closure in cont k (ans)\n    and symbol g_clo = closure g\n    with { var1 = 42 }\n  end in ...\n"
    | 213 ->
        "Expected \"and\" or \"in\".\n"
    | 214 ->
        "Expected \"segment\".\n"
    | 196 ->
        "Expected an identifier.\nExamples of kinded variable/argument declarations:\n  arg1\n  (arg2 : val)\n"
    | 198 ->
        "Expected : and a kind.\nExamples of kinded variable/argument declarations:\n  arg1\n  (arg2 : val)\n"
    | 199 ->
        "Expected a kind.\nExamples of kinds:\n  val\n  imm\n"
    | 200 ->
        "Expected ).\n"
    | 86 ->
        "Expected rec or a code id.\nExamples of code definitions:\n  code f my_closure -> k : () = cont k ()\n  code rec f my_closure -> k : () = cont k ()\n"
    | 87 ->
        "Expected a code id.\n"
    | 88 ->
        "Expected a closure id declaration or a newer_version_of declaration or an\nargument list or an identifier for a closure argument.\nExample of a closure id declaration:\n  @ closure_id\nExample of a newer_version_of declaration:\n  newer_version_of code_id\nExamples of argument lists:\n  ()\n  (var1 (var2: imm))\n"
    | 93 ->
        "Expected a code id as an argument to newer_version_of.\nExample:\n  newer_version_of f0\n"
    | 95 ->
        "Expected an argument list in parentheses or an identifier for a closure argument.\nExamples of code definitions:\n  code f2 newer_version_of f1 (arg1 arg2) my_closure -> k = cont k arg1\n  code f2 newer_version_of f1 my_closure -> k : () = cont k ()\n"
    | 96 ->
        "Expected an identifier for a closure argument.\nExample of a code definition:\n  code f my_closure -> k : () = cont k ()\n"
    | 97 ->
        "Expected < followed by variables within a closure, or -> followed by a\ncontinuation.\nExamples of code definitions:\n  code f (arg) my_closure -> k : val = cont k (arg)\n  code f (arg1 (arg2 : imm)) my_closure -> k : val * imm = cont k (arg1 arg2)\n  code f my_closure <var1, (var2 : imm)> -> k : () = cont k ()\n"
    | 110 ->
        "Expected a name for a return continuation.\nExample of a code definition:\n  code f my_closure -> k : () = cont k ()\n"
    | 111 ->
        "Expected * followed by a name for an exception continuation, or : followed by a\nreturn arity, or = followed by an expression. \nExamples of code definitions:\n  code f my_closure -> k * e : () = cont k ()\n  code f my_closure -> k : val * val = cont k (23 42)\n  code f my_closure -> k = cont k (42)\n"
    | 112 ->
        "Expected : followed by a return arity, or = followed by an expression.\nExamples of code definitions:\n  code f my_closure -> k * e = cont k (42)\n  code f my_closure -> k * e : () = cont k ()\n  code f my_closure -> k * e : val * val = cont k (23 42)\n"
    | 122 ->
        "Expected an expression.\nExample of a code definition:\n  code f my_closure -> k = cont k (42)\n"
    | 121 ->
        "Expected = followed by an expression.\nExample of a code definition:\n  code f my_closure -> k : val = cont k (42)\n"
    | 98 ->
        "Expected a declaration of a variable within a closure, or >.\nExamples of lists of variables within a closure:\n  <>\n  <var1 (var2 : val)>\n"
    | 109 ->
        "Expected -> followed by a name for a continuation.\nExample of a code definition:\n  code f my_closure -> k * e = cont k arg1\n"
    | 99 ->
        "Expected an identifier for a variable within a closure.\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 100 ->
        "Expected : followed by a kind.\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 101 ->
        "Expected a kind.\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 102 ->
        "Expected ).\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 107 ->
        "Expected a declaration of a variable within a closure or > to end the list.\nExamples of lists of declarations of variables within a closure:\n  <>\n  <var1 (var2 : val)>\n"
    | 89 ->
        "Expected a closure id.\nExample of a code definition specifying a closure id:\n  code f @closure_id my_closure -> k * e = cont k arg1\n"
    | 92 ->
        "Expected a new_version_of declaration, an argument list, or a closure argument.\nExamples of code definitions:\n  code f2 @closure_id newer_version_of f1 arg1 arg2 my_closure <var1 var2>\n    -> k * e : val * val = cont k (arg1 arg2)\n  code f2 my_closure -> k * e = cont k (arg1)\n"
    | 124 ->
        "Expected a continuation to call.\nExample of a continuation call:\n  cont k\n"
    | 125 ->
        "Expected an argument list in parentheses, or end of file, or one of: with } in \nExamples of continuation calls:\n  cont k\n  cont k ()\n  cont k (arg1 arg2)\n"
    | 126 ->
        "Expected a simple value (a symbol, variable, or literal) as an argument to\na continuation.\nExamples of continuation calls:\n  cont k\n  cont k ()\n  cont k (arg1 arg2)\n"
    | 127 ->
        "Expected a simple value (a symbol, variable, or literal).\n"
    | 67 ->
        "Expected a code id.\nExample of a closure:\n  closure f\n"
    | 132 ->
        "Expected a C identifier in [].\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 133 ->
        "Expecting an identifier for a C function.\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 135 ->
        "Expecting ].\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 136 ->
        "Expecting an argument list, a return arity, or a continuation specification.\nExamples of C calls:\n  ccall [f] -> cont * exn_cont\n  ccall [f] (arg1 arg2) : , -> cont * exn_cont\n"
    | 139 ->
        "Expected a continuation.\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 140 ->
        "Expected * followed by an exception continuation.\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 137 ->
        "Expected : followed by an arity, or -> followed by a continuation.\nExamples of C calls:\n  ccall [f] -> cont * exn_cont\n  ccall [f] () -> cont * exn_cont\n  ccall [f] (arg1 arg2) : , -> cont * exn_cont\n"
    | 113 ->
        "Expected an arity.\nExamples of arities:\n  ()\n  val\n  int32, val, imm\n"
    | 118 ->
        "Expected one of the following: * -> =\nExamples of arities:\n  ()\n  val\n  int32, val, imm\n"
    | 119 ->
        "Expected a kind.\n"
    | 114 ->
        "Expected ).\nExamples of arities:\n    ()\n    val\n    int32, val, imm\n"
    | 138 ->
        "Expected -> followed by a continuation.\nExample of a C call:\n  ccall [f] : val -> cont * exn_cont\n"
    | 142 ->
        "Expected block tag.\nExample of a block:\n  Block 0 (field1 field2) \n"
    | 143 ->
        "Expected value(s) for block, in parentheses.\nExample of a block:\n  Block 0 (field1 field2) \n"
    | 144 ->
        "Expected value(s) for block.\nExample of a block:\n  Block 0 (field1 field2) \n"
    | 147 ->
        "Expected the name of a function to apply.\nExample of an application:\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 150 ->
        "Expected an argument list in parentheses, or -> followed by a continuation.\nExamples of applications:\n  apply f -> cont * exn_cont\n  apply f () -> cont * exn_cont\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 152 ->
        "Expected a continuation.\nExample of an application:\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 153 ->
        "Expected * followed by an exception continuation.\nExample of an application: \n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 151 ->
        "Expected -> followed by a continuation.\nExample of an application:\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | _ ->
        raise Not_found
