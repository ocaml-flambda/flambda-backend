let foo x =
  [%probe "name" (print_int x)];
  x
