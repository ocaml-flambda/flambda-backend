let[@zero_alloc strict][@zero_alloc strict] test1 x = x,x
let[@inline never][@specialise never][@local never][@zero_alloc strict assume][@zero_alloc strict] test2 x = x,x
let[@zero_alloc strict all] test3 x = x,x
let[@zero_alloc toplevel] test4 x = x,x
let[@zero_alloc on] test5 x = x,x
let[@zero_alloc all] test6 x = x,x
let[@zero_alloc check] test6 x = x,x
