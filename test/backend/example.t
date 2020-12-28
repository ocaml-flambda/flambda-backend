Example cram test for checking assembly output
  $ cat > example.ml <<EOF
  > external my_c_function : unit -> int = "my_c_function"
  > let f () = my_c_function () + 1
  > EOF
  $ ../../flambda_backend_main_native.exe -nostdlib \
  > -I ../../ocaml/stdlib/.stdlib.objs/byte \
  > -I ../../ocaml/stdlib/.stdlib.objs/native \
  > -I ../../ocaml/stdlib \
  > -c -S example.ml
  $ grep my_c_function example.s > /dev/null
