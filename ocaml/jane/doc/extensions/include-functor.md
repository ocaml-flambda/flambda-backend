# The `include functor` extension

The `include functor` extension eliminates a common source of boilerplate when
defining modules that include the results of functors.  It adds the module item
form `include functor F`, where `F` must be a functor whose parameter can be
"filled in" with the previous contents of the module.  For example, you can now
write this:

```ocaml
module M = struct
  type t = ...
  [@@deriving compare, sexp]

  include functor Comparable.Make
end
```

Traditionally, this would have required defining an inner structure `T` just to
have something the functor can be applied to:

```ocaml
module M = struct
  module T = struct
    type t = ...
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make(T)
end
```

These two code fragments behave identically, except that in the first case the
module `M` won't have a submodule `T`.

The feature can also be used in signatures:

```ocaml
module type F = functor (T : sig ... end) -> Comparable.S with type t = T.t

module type S = sig
  type t
  [@@deriving compare, sexp]

  include functor F
end
```

This behaves as if we had written:

```ocaml
module type S = sig
  type t
  [@@deriving compare, sexp]

  include Comparable.S with type t := t
end
```

Currently it's uncommon to define functor module types like `F` (there's no such
module type in `Comparable`, for example).  However, you can get the module type
of a functor directly with `module type of`, so the previous signature could
equivalently be written:

```ocaml
module type S = sig
  type t
  [@@deriving compare, sexp]

  include functor module type of Comparable.Make
end
```

## Details and Limitations

This extension is not available in the upstream compiler, so publicly
released code should not use it.  We plan to upstream it in the
future.

To include a functor `F`, it must have a module type of the form:

```ocaml
  F : S1 -> S2
```

or

```ocaml
  F : S1 -> () -> S2
```

where `S1` and `S2` are signatures.

Currently, `include functor` cannot be used in the signatures of recursive
modules.  It may be possible to lift this restriction in the future, if there is
sufficient demand.