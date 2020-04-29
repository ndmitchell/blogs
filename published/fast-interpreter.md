# Writing a fast interpreter

_Summary: Interpretation by closure is a lot faster than I expected._

Let's imagine we have an imperative language (expressions, assignments, loops etc.) and we want to write an interpreter for it. What styles of interpreter are there? And how fast do they perform? I was curious, so I wrote a demo with some benchmarks. The full code, in Rust, [is available here](https://gist.github.com/ndmitchell/084bb1a8dd30188492fcd0b8b8c70c6e).

First, let's get a taste of the the mini-language we want to interpret:

```rust
x = 100;
y = 12;
while x != 0 {
    y = y + 1 + y + 3;
    x = x - 1;
}
```

We can immediately translate `x` and `y` from being named variables to indices in an array, namely `0` and `1`. Once we've done that there are four interpretation techniques that spring to mind (and in brackets their performance on my benchmark):

1. Interpret the AST directly (2.1s).
2. Compile from the AST to closures (1.4s).
3. Compile from the AST to a stream of instructions (1.5s).
4. Encode those instructions as bytes (1.5s).
5. Compile to assembly or JIT - I didn't try this approach (it's a lot more work).

All of these are vastly slower than my version written directly in Rust (which takes a mere 0.003s) -- but my benchmark didn't have any real operations in it, so this comparison will be the absolute worst case.

Let's go through the approaches.

**Style 1: AST evaluation**

One option is to directly interpret the AST. Given a vector named `slots` representing the variables by index, we need to change the `slots` as we go. A fragment of the interpreter might look like:

```rust
fn f(x: &Expr, slots: &mut Vec<i64>) -> i64 {
    match x {
        Expr::Lit(i) => *i,
        Expr::Var(u) => slots[*u],
        Expr::Add(x, y) => f(x, slots) + f(y, slots),
        ...
    }
}
```

It's as simple as the options come. Given the expression and the slots we need to write to, we do whatever the instruction tells us. But the low simplicity leads to low performance.

**Style 2: Conversion to closure**

Instead of traversing the AST at runtime, we can traverse it once, and produce a closure/function that performs the action when run (e.g. see [this blog post](https://blog.cloudflare.com/building-fast-interpreters-in-rust/)). Given that we access the slots at runtime, we make them an argument to the closure. In Rust, the type of our closure is:

```rust
type Compiled = Box<dyn Fn(&mut Vec<i64>) -> i64>;
```

Here we are defining a `Fn` (a closure -- function plus captured data) that goes from the slots to a result. Because these functions vary in how much data they capture, we have to wrap them in `Box`. With that type we can now define our evaluation function:

```rust
fn f(x: &Expr) -> Compiled {
    match x {
        Expr::Lit(i) => {
            let i = *i;
            box move |_| i
        }
        Expr::Var(u) => {
            let u = *u;
            box move |slots| slots[u]
        }
        Expr::Add(x, y) => {
            let x = compile(x);
            let y = compile(y);
            box move |slots| x(slots) + y(slots)
        }
        ...
    }
}
```

Instead of taking the AST (compile-time information) and the slot data (runtime information) we use the compile-time information to produce a function that can then be applied to the run-time information. We trade matching on the AST for an indirect function call at runtime. Rust is able to turn tail calls on dynamic functions into jumps and the processor is able to accurately predict the jumps/calls, leading to reasonable performance.

One large advantage of the closure approach is that adding specialised variants, e.g. compiling a nested `Add` differently, can be done locally and with no additional runtime cost.

**Style 3: Fixed sized instructions**

Instead of interpreting an AST, or jumping via indirect functions, we can define a set of instructions and interpret an array of them them using a stack of intermediate values. We are effectively virtualising a CPU, including program counter. We can define a bytecode with instructions such as:

```rust
enum Bytecode {
    Assign(u32),  // assign the value at the top of the stack to a slot
    Var(u32),     // push the value in slot to the top of the stack
    Lit(i32),     // push a literal on the stack
    Add,          // Add the top two items on the stack
    Jump(u32),
    ...
}
```

We now interpret these instructions:

```rust
let mut pc = 0;
let mut slots = vec![0; 10];
let mut stack = Stack::new();

loop {
    match xs[pc] {
        Assign(x) => slots[x as usize] = stack.pop(),
        Var(x) => stack.push(slots[x as usize]),
        Lit(i) => stack.push(i as i64),
        Add => {
            let x = stack.pop();
            let y = stack.pop();
            stack.push(x + y)
        }
        Jump(pc2) => pc = pc2 as usize - 1,
        ...
    }
    pc = pc + 1;
}
```

Most of these operations work against the stack. I found that if I used checked array accesses on the stack (the default in Rust) it went about the same speed as AST interpretation. Moving to unchecked access made it similar in performance (slightly worse) than the closure version.

The bytecode approach is much harder to implement, requiring a compiler to the bytecode. It's also much harder to add specialised variants for certain combinations of instructions. To get good performance via the branch predictor probably requires further tricks beyond what I've shown here (e.g. [direct threading](http://www.cs.toronto.edu/~matz/dissertation/matzDissertation-latex2html/node6.html)).

There are advantages to a bytecode though -- it's easier to capture all the program state, which is useful for garbage collection and other operations.

**Style 4: Byte encoded instructions**

Instead of having a Rust `enum` to represent the values, we can instead use bytes, so instead of:

```rust
Lit(38)
Lit(4)
Add
Assign(0)
```

We would have a series of bytes `[0,38,0,4,1,2,0]` (where `0` = `Lit`, `1` = `Add`, `2` = `Assign`). This approach gives a more compact bytecode, and might have an impact on the instruction cache, but in my benchmarks performed the same as style 3.
