# A Rust self-ownership lifetime trick (that doesn't work)

_Summary: I came up with a clever trick to encode lifetimes of allocated values in Rust. It doesn't work._

Let's imagine we are using Rust to implement some kind of container that can allocate values, and a special value can be associated with the container. It's a bug if the allocated value gets freed while it is the special value of a container. We might hope to use lifetimes to encode that relationship:

```rust
struct Value<'v> {...}
struct Container {...}

impl Container {
    fn alloc<'v>(&'v self) -> Value<'v> {...}
    fn set_special<'v>(&'v self, x: Value<'v>) {...}
}
```

Here we have a `Container` (which has no lifetime arguments), and a `Value<'v>` (where `'v` ties it to the right container). Within our container we can implement `alloc` and `set_special`. In both cases, we take `&'v self` and then work with a `Value<'v>`, which ensures that the lifetime of the `Container` and `Value` match. (We ignore details of how to implement these functions - it's possible but requires `unsafe`).

Unfortunately, the following code compiles:

```rust
fn set_cheat<'v1, 'v2>(to: &'v1 Container, x: Value<'v2>) {
    to.set_special(x);
}
```

The Rust compiler has taken advantage of the fact that `Container` can be reborrowed, and that [`Value` is variant](https://doc.rust-lang.org/nomicon/subtyping.html), and rewritten the code to:

```rust
fn set_cheat<'v1, 'v2>(to: &'v1 Container, x: Value<'v2>) {
    'v3: {
        let x : Value<'v3> = x; // Value is variant, 'v2 : 'v3
        let to : &'v3 Container = &*to;
        to.set_special(x);
    }
}
```

The code with lifetime annotations doesn't actually compile, it's just what the compiler did under the hood. But we can stop `Value` being variant by making it contain `PhantomData<Cell<&'v ()>>`, since lifetimes under `Cell` are invariant. Now the above code no longer compiles. Unfortunately, there is a closely related variant which does compile:

```rust
fn set_cheat_alloc<'v1, 'v2>(to: &'v1 Container, from: &'v2 Container) {
    let x = from.alloc();
    to.set_special(x);
}
```

While `Value` isn't variant, `&Container` is, so the compiler has rewritten this code as:

```rust
fn set_cheat<'v1, 'v2>(to: &'v1 Container, from: &'v2 Container) {
    'v3: {
        let from = &'v3 Container = &*from;
        let x : Value<'v3> = from.alloc();
        let to : &'v3 Container = &*to;
        to.set_special(x);
    }
}
```

Since lifetimes on `&` are always variant, I don't think there is a trick to make this work safely. Much of the information in this post was gleaned from [this StackOverflow question](https://stackoverflow.com/a/62661100/160673).
