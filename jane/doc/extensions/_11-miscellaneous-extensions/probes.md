---
layout: documentation-page
collectionName: Miscellaneous extensions
title: Tracing probes
---

## Tracing OCaml native programs in user-space

Tracing probes can be defined using OCaml's extension point syntax as follows:

    [%probe <name> <handler>]

where `<name>` is a string literal without spaces or special
characters and `<handler>` is an arbitrary
OCaml expression of type `unit`.

For example:

    let foo x y =
        [%probe "my_first_probe" (myprint "from foo" x y)]
          bar x y

By default, all probes are disabled when the program starts. A
disabled probe does not do anything and does not cost almost anything
at runtime, and in particular, the handler is not evaluated.

If a probe is enabled, whenever the execution reaches it, the
corresponding handler is evaluated.

A probe can be enabled and disabled during program execution using `ocaml-probes` tool
and library.

[See ocaml-probes documentation or details](https://github.com/janestreet/ocaml-probes/blob/master/README.md)
