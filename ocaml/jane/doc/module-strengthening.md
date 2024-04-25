# The `module strengthening` extension

What is the type of module `M` in this tiny program?

```ocaml
module type S = sig type t end

module M : S
```

If you said `S` then you are only partially correct: we also need to keep track
of the fact that all type declarations inside it (just `t` in this example) come
from `M`. So the type that the compiler infers is actually stronger:

```ocaml
sig type t = M.t end
```

We call this type "`S` strengthened with `M`". It can be written out explicitly
as above and this is, in fact, the only way to write it without module strengthening.
The new extension allows this type to be written as `S with M`.

The main motivation for this work are compiler performance improvements, which
use this new form internally.
