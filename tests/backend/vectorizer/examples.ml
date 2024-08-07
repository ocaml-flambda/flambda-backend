let big_n = 1024
let a = Array.make (big_n * 2) 0
let b = Array.make (big_n * 2) 0
let c = Array.make (big_n * 2) 0

let example1 () =
  for i = 0 to 255 do
    Array.set a i (Array.get b i + Array.get c i)
