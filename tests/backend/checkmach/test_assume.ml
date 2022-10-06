let[@noalloc_strict assume][@inline never] test1 n = (n,n)
let[@noalloc_strict] test2 n = test1 (n+1)
let[@noalloc_strict] test3 n = T5.test n
