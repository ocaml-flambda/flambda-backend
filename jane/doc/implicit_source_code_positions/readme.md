# Implicit Source Code Positions [%call_pos] - Who is calling my function?

<div class="hbox">
  <div class="title">
let f ~(here : <span class="red">[%call_pos]</span>) () = ...
  </div>
  <div class="subbox">
`[%call_pos]` is similar to `[%here]`. It allows a function to know the location of its
callers ___without___ the callers providing the location.
  </div>
</div>

You may have seen functions with types like:

```ocaml
val require_does_raise : Source_code_position.t -> (unit -> 'a) -> unit
```

that callers then provide the location with something like:

```ocaml
require_does_raise [%here] (fun () -> ...)
```

With `[%call_pos]`, callers do not need to provide the location argument as the compiler
will automatically supply it for your callers.

You can define a function with an implicit source code position argument by annotating
a labelled argument with `[%call_pos]`:

```ocaml
let require_does_raise ~(here : [%call_pos]) (f : unit -> 'a) = ...

(* Note that [%call_pos] appears in the signature - different from [Source_code_position.t] *)
val require_does_raise : here:[%call_pos] -> (unit -> 'a) -> unit
```

Your callers can then call your function without needing to provide `[%here]`:

```ocaml
require_does_raise (fun () -> ...)
```

Alternatively, if (say) you are building a helper around `require_does_raise` for your tests,
but would like `require_does_raise` to get the position of the callers of your helper, you
can still send in in the location:

```ocaml
let require_does_raise_helper ~(outer_call_pos : [%call_pos]) f =
  require_does_raise ~here:outer_call_pos f
```

Type checking / how does this work?
-----------------------------------
You can think of `[%call_pos]` as an optional parameter. It is type checked and behaves in
a very similar way:

- When an __optional__ argument is not applied, its "default" value / `None` is applied.

- When an __implicit source code position__ (`[%call_pos]`) argument is not applied, its
  "caller's location" is applied.

This does mean that you need at least one "positional" (unlabelled) argument after your `[%call_pos]`
argument as otherwise you will get an unerasable argument warning - similar to optional
arguments.

[%src_pos]
----------
`[%src_pos]` is the same as ppx_here's `[%here]` except that it is implemented directly on
the compiler.

<style>
.title {
  text-align: center;
  font-family: sans-serif;
  font-size: 1.5rem;
  letter-spacing: 0.15rem;
  color: #fff;
  margin-left: 3rem; 
  font-family: monospace;
}

.hbox {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  border-radius: 8px;
  margin-top: 1rem;
  gap: 1rem;
  color: #cdd6f4;
  background-color: #1e1e2e;
  padding: 1rem;
}

.hbox code {
  color: #f38ba8;
}

code {
  color: #e64553;
}

pre {
  border-radius: 8px;
  background-color: #1e1e2e;
}

.hbox p {
  margin: 0;
}

.subbox {
  text-wrap: balance;
  text-align: center;
}

.red {
  color: #f38ba8;
}
</style>