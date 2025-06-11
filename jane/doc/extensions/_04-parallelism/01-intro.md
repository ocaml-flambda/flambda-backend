---
layout: documentation-page
collectionName: Parallelism
title: Intro
---

# Introduction to Parallelism

OCaml 5 introduces multicore, which allows parallel execution in a single process.
Based on that, the OCaml Language team developed a collection of compiler features and libraries:
- Extending the [mode system](../../modes/intro) to track values' concurrent
  usages, so they can be used concurrently safely.
- Higher-level parallelism primitives to allow users to fully expose
  opportunities of parallelism in their programs, without worrying low-level
  details such as overhead.
- Integration to existing concurrency frameworks such as [Async].
- ..and more.

This is a stub, with more complete documentation to follow. For now, please see
our parallelism tutorials, beginning with [part
1](../../tutorials/01-intro-to-parallelism-part-1).
