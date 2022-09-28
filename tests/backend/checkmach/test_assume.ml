let[@noalloc assume][@inline never] test1 n = (n,n)
let[@noalloc] test2 n = test1 (n+1)
let[@noalloc] test3 n = T5.test n
