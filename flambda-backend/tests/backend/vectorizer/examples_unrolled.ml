let big_n = 1024
let a = Array.make (big_n * 2) 0
let b = Array.make (big_n * 2) 0
let c = Array.make (big_n * 2) 0

let example1 () =
  for i = 0 to 127 do
    Array.set a (i * 2) (Array.get b (i * 2) + Array.get c (i * 2));
    Array.set a (i * 2 + 1) (Array.get b (i * 2 + 1) + Array.get c (i * 2 + 1))
  done
