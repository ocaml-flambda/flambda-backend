let[@noalloc_strict assume][@inline never] test1 n = (n,n)
let[@noalloc_strict] test2 n = test1 (n+1)
let[@noalloc_strict] test3 n = T5.test n
let no_annotations x y = x * y

let[@noalloc] test4 x = if x > 0 then x
  else raise (Failure ("be postitive, not "^(string_of_int x)))

let[@noalloc] test5 x = T5.test4 x
