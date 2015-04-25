# Build system semantics

What are the semantics of phony and order-only. We're going to think about the semantics in the context of Ninja, which has both, but little else.

The semantics of phony can be expressed in two ways:

* A rule that generates the file, but where the file is deleted before running.
* An alias for the things the phony depends on.

https://groups.google.com/d/msg/ninja-build/2KMtxaN6tgk/vQ_f1kMP6hoJ
