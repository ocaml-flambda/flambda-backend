let[@zero_alloc strict assume][@inline never][@specialise never] test1 n = (n,n)
let[@zero_alloc strict] test2 n = test1 (n+1)
let[@zero_alloc strict] test3 n = T5.test n
let no_annotations x y = x * y

let[@zero_alloc] test4 x = if x > 0 then x
  else raise (Failure ("be postitive, not "^(string_of_int x)))

let[@zero_alloc] test5 x = T5.test4 x
