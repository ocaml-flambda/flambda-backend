let linker_dir_sep = '\001'

let mangle_linker_dirs dirs =
  String.concat (Printf.sprintf "%c" linker_dir_sep) dirs

let demangle_linker_dirs mangled_dirs =
  String.split_on_char linker_dir_sep mangled_dirs