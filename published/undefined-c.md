# Undefined Behaviour in C

_Summary: I tripped over undefined behaviour in C. It's annoying._

I've recently been writing some C code to [parse XML quickly](https://github.com/ndmitchell/hexml#readme). While working on that project, I inadvertently wrote some code which is undefined according to the C language standard. The code compiled and ran fine using Visual Studio, but under `gcc` (even at `-O0`) it corrupted memory, sometimes leading to a segfault, but usually just leading to a wrong answer. The code in question was ([see context at GitHub](https://github.com/ndmitchell/hexml/commit/90a9675dbab69fc3a4734e6e8141f1324bb61522)):

    d->nodes.nodes[0].nodes = parse_content(d);

To give some context, `d` is a structure that contains various pieces of state - what the string to be parsed is, how much we have parsed, along with a pointer to the output `nodes`. The `parse_content` function parses the bit inside an XML tag, returning the indicies in `nodes` which it used.

The complication comes from `nodes` not being a fixed size, but dynamically resized if the number of nodes exceeds the capacity. For big documents that means `parse_content` will reallocate `d->nodes.nodes`.

According to the C spec, the compiler can evaluate the LHS and RHS of an assignment in any order. Since `gcc` computes the location of `d->nodes.nodes[0]` before calling `parse_content` it uses the address of the node before reallocation. After reallocation the address will have changed, and the assignment will be made to the wrong location.

I spotted the bug by inserting `printf` statements, and in doing so, I had to rewrite the code to:

    str content = parse_content(d);
    d->nodes.nodes[0].nodes = content;

That fixes the issue, since now the evaluation order is strictly defined. As a simplified example of the same issue:

    char* array;

    char f() {
        array = malloc(42);
        return 'x';    
    }

    void test() {
        array = malloc(0);
        array[0] = f();
    }

Here the line `array[0] = f()` might assign to either the result of `malloc(0)` or `malloc(42)`, at the compilers discretion.

I manually checked if I had made any other such mistakes, and I couldn't find any. Naturally, I wanted to find a static checker that could detect such a mistake, so I tried a bunch of them. I wasn't very successful:

* Visual Studio 2015 code analysis made me write `assert` after each `malloc`, but nothing further.
* PVS Studio found nothing.
* Clang undefined behaviour found nothing, and seemingly doesn't work on Windows.
* GCC undefined behaviour found nothing, and seemingly doesn't work on Windows.
* RV-Match hit a stack-overflow when running the program.
