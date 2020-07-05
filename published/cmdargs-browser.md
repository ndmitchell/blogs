# Automatic UI's for Command Lines with `cmdargs`

_Summary: Run `cmdargs-browser hlint` and you can fill out arguments easily._

The Haskell [command line parsing library `cmdargs`](https://hackage.haskell.org/package/cmdargs) contains a [data type that represents a command line](http://hackage.haskell.org/package/cmdargs/docs/System-Console-CmdArgs-Explicit.html#t:Mode). I always thought it would be a neat trick to transform that into a web page, to make it easier to explore command line options interactively - similar to how the custom-written [wget::gui](http://www.martin-achern.de/wgetgui/) wraps [`wget`](https://www.gnu.org/software/wget/).

I wrote a demo to do just that, named [`cmdargs-browser`](http://hackage.haskell.org/package/cmdargs-browser). Given any program that uses `cmdargs` (e.g. [`hlint`](https://github.com/ndmitchell/hlint)), you can install `cmdargs-browser` (with `cabal install cmdargs-browser`) and run:

```
cmdargs-browser hlint
```

And it will pop up:

<INSERT PICTURE HERE>

As we can see, the HLint modes are listed on the left (you can use `lint`, `grep` or `test`), the possible options on the right (e.g. normal arguments and `--color`) and the command line it produces at the bottom. As you change mode or add/remove flags, the command line updates. If you hit `OK` it then runs the program with the command line. The help is included next to the argument, and if you make a mistake (e.g. write `foo` for the `--color` flag) it tells you immediately. It could be more polished (e.g. browse buttons for file selections, better styling) but the basic concepts works well.

## Technical implementation

I wanted every `cmdargs`-using program to support this automatic UI, but also didn't want to increase the dependency footprint or compile-time overhead for `cmdargs`. I didn't want to tie `cmdargs` to this particular approach to a UI - I wanted a flexible mechanism that anyone could use for other purposes.

To that end, I built out a [`Helper` module](https://hackage.haskell.org/package/cmdargs-0.10.20/docs/System-Console-CmdArgs-Helper.html) that is included in `cmdargs`. That API provides the full power and capabilities on which `cmdargs-browser` is written. The `Helper` module is only 350 lines.

If you run `cmdargs` with either `$CMDARGS_HELPER` or `$CMDARGS_HELPER_HLINT` set (in the case of HLint) then `cmdargs` will run the command line you specify, passing over the explicit `Mode` data type on the stdin. That `Mode` data type includes functions, and using a simplistic communication channel on the stdin/stdout, the helper process can invoke those functions. As an example, when `cmdargs-browser` wants to validate the `--color` flag, it does so by calling a function in `Mode`, that secretly talks back to `hlint` to validate it.

At the end, the helper program can choose to either give an error message (to stop the program, e.g. if you press Cancel), or give some command lines to use to run the program.

## Future plans

This demo was a cool project, which may turn out to be useful for some, but I have no intention to develop it further. I think something along these lines should be universally available for all command line tools, and built into all command line parsing libraries.

## Historical context

All the code that makes this approach work was written over seven years ago. Specifically, it was my hacking project in the hospital while [waiting for my son to be born](https://ndmitchell.com/elements/henry-photo-big.jpg). Having a little baby is a hectic time of life, so I never got round to telling anyone about its existence.

This weekend I resurrected the code and published an updated version to Hackage, deliberately making as few changes as possible. The three necessary changes were:

1. jQuery deprecated the `live` function replacing it with `on`, meaning the code didn't work.
2. I had originally put an upper bound of `0.4` for the `transformers` library. Deleting the upper bound made it work.
3. Hackage now requires that all your uploaded `.cabal` files declare that they require a version of 1.10 or above of Cabal itself, even if they don't.

Overall, to recover a project that is over 7 years old, it was surprisingly little effort.
