let () =
  List.iter (fun i -> (print_int [@inlined never]) i) [1; 2; 3]
