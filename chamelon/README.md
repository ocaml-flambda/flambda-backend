Usage
=====

Basic usage is:
`chamelon -c "[compile command]" -e "[error]" input.ml`

This will execute the command on `input.ml`, and produce a minimized output `input_min.ml`
such that the output of the command still contains `[error]`.

The following options are also available:
- `-o [file]`: this will use `[file]` as the ouput instead of the default - that is, suffixing the
  input file with `_min`. Inplace minimization can be achieved by setting the output file
  to the input file.
- `-t [command]`: this uses `[command] input.ml` to produce a `.cmt` file. This is necessary
 when the given command is not a compilation command. This is also useful if you want to 
 minimize a file when the compilation command  produces `.cmt` files incompatible with the
  version of OCaml chamelon is compiled with.
- `-m [minimizers]`: this runs the minimizers from the comma-separated list of minimizers given
  as arguments instead of the default iteration order.
