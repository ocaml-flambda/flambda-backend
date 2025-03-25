# Externals Extraction

The externals extraction is a small tool to extract type information about external functions from `.cmt` files.
Upon installation of the compiler, it is installed as the binary `extract_externals.opt`.


## Extracting externals

After successfully building and installing the compiler, you can extract external declarations as follows:

```
extract_externals.opt -I import/path/one -I import/path/two -H hidden/import/path -open Foo -output-file externals_out.txt file1.cmt file2.cmt
```

In other words, you can provide import flags `-I`, hidden import flags `-H`, and open flags `-open` as for the compiler.
In addition, you can provide an output file to store the externals in with `-output-file` and you should provide a list of at least one `.cmt` file. If the `-output-file` flag is not present, the result will be printed to stdout.

The output of the extraction is a sequence of external functions with their type information in [sexp format](https://github.com/janestreet/sexplib).


## Shape information

To understand the shape information that is provided in the output, see the file `shapes.mli` in the `extract_externals` directory. In short, for arguments and return values in external declarations, the following shapes are currently supported:

```ocaml
type value_shape =
  | Value  (** anything of C type [value] *)
  | Imm  (** immediate, tagged with a one at the end *)
  | Nativeint
      (** block of a native word integer, e.g., 64-bit integer on amd64 target *)
  | Double  (** block of a native double *)
  | Int64  (** block of a 64-bit integer *)
  | Int32  (** block of a 32-bit integer *)
  | String
      (** block of a char pointer with a size, representing both Bytes.t and String.t *)
  | FloatArray  (** block containing native doubles *)
  | Block of (int * value_shape list) option
      (** Block whose tag is below no-scan tag (i.e., a normal ocaml block value). If the
      argment is [None], then the block could have any tag and any elements. If the
      argument is [Some (t, shs)], then [t] is the tag of the block and [shs] contains the
      shapes of its fields. In the case of [Some (t, shs)], the number of
      fields is known statically (i.e., the length of the list [shs]).

      To represent arrays (which are blocks with tag 0 at run time, but whose size is not
      statically known), there is a separate construtor, [Array sh], which keeps track of
      the shapes of the elements. *)
  | Array of value_shape
      (** Block with tag 0 and a fixed size (not known statically). The shape of the
          elements is given by the argument. *)
  | Closure  (** Block with closure tag. *)
  | Obj  (** Block with object tag. *)
  | Or of value_shape * value_shape
      (** Disjunction between two shapes for, e.g., variant types. *)
```

Note that these shapes can be overlapping (e.g., `Value` covers all other shapes).
