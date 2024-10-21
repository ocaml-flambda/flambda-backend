(* TEST *)

Random.init 3;;
for i = 0 to 1000 do
  ignore (Bytes.create (Random.int 1_000_000))
done;;
Printf.printf "hello world\n";;
