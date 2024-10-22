let[@zero_alloc strict][@zero_alloc strict] test1 x = x,x
let[@inline never][@specialise never][@zero_alloc strict assume][@zero_alloc strict] test2 x = x,x
let[@zero_alloc strict check] test3 x = x,x
let[@zero_alloc check] test4 x = x,x
let[@zero_alloc all] test5 x = x,x
