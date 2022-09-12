let[@assume noalloc][@inline never] test1 n = (n,n)
let[@assert noalloc] test2 n = test1 (n+1)
