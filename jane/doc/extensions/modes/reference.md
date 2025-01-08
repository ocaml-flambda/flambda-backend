# Lazy
`lazy e` contains a thunk that evaluates `e`, as well as a mutable cell to store the
result of `e`. Upon construction, `lazy e` follows the mode of `e` and the comonadic mode
of the thunk. Upon destruction (forcing a lazy value), the result follows the mode of lazy
value. Currently, the thunk, the result and the `lazy` are all heap-allocated.

Additionally, forcing a lazy value involves accessing the mutable cell and thus requires
the lazy value to be `uncontended`. Therefore, one can construct a lazy value at
`portable` even if the thunk is `nonportable` (e.g., closing over `uncontended` or
`nonportable` values). Similarly, the thunk is guaranteed to run at most once even if the
lazy value is forced multiple times. Therefore, one can construct the lazy value at `many`
even if the thunk is `once` (e.g., closing over `unique` or `once` values).
