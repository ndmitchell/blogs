# From Command Lines to Shake Build System

_Summary: This post explains how to go from command lines to a Shake build system, aimed at non-Haskellers._

A build system, at its heart, runs command lines in a dependency-satisfying order. Here I give an example of how to produce a simple [Shake](http://shakebuild.com) build system starting from those command lines. Much of the code is copied shamelessly from the [Shake user manual](http://shakebuild.com/manual). So, let's imagine we want to compile files with:

    g++ -dynamic -g -c -arch i386 x86_64 -Wall -fPIC -I/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/include/darwin -I./lib SourceFile.cpp -o SourceFile.o

And link them with:

	g++ -dynamiclib -arch i386 x86_64 -Wl,-soname,libFoo.so, --no-undefined -L./lib -o mylib.dylib File1.o File2.o

First, let's define a bunch of useful constants so we can simplify the logic and make it easier to change pieces:

    compileFlags = "g++ -dynamic -g -c -arch i386 x86_64 -Wall -fPIC -I/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/include/darwin -I./lib"
    linkFlags = "g++ -dynamiclib -arch i386 x86_64 -Wl,-soname,libFoo.so, --no-undefined -L./lib"
	linkFiles = "File1.o File2.o"

Here we are using a single Haskell string for each constant, and using the convention that it is space separated. That fits quite nicely with the `cmd` function in Shake. Now we write the minimum viable build system:

    -- PART 1
	import Development.Shake
	import Development.Shake.FilePath
	import Development.Shake.Util

	main :: IO ()
	main = shakeArgs shakeOptions $ do

    -- PART 2
	    want ["mylib.dylib"]

    -- PART 3
	    "*.dylib" %> \out -> do
	        need $ words linkFiles
	        cmd linkFlags linkFiles "-o" [out]

    -- PART 4
	    "*.o" %> \out -> do
	        let m = out -<.> "m"
	        unit $ cmd compileFlags [out -<.> "cpp"] "-o" [out] "-MMD -MF" [m]
	        neededMakefileDependencies m

You can view it as having 4 parts.

1. Just a few imports and the main declaration. Ignore as boilerplate.
2. What you want to produce at the end, tell the build system to build it.
3. How to link. Uses the `linkFlags` and `linkFiles` to build it. This is where the linking command line comes from.
4. How to compile. Uses the `compileFlags` plus the `-<.>` operator to change the extension of the output file. It also uses `-MMD -MF` to get the header dependencies of the C++ file, which it then uses in the call to `neededMakefileDependencies`. This ensures that the build will be updated properly if the header files changes.  

### Making it multiplatform

Now let's imagine that on Linux we want to use `-fPIC` but on other platforms we don't. We can add an `import System.Info` at the top, then redefine `compileFlags` as:

    compileFlags = "g++ -dynamic -g -c -arch i386 x86_64 -Wall -fPIC -I/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/include/darwin -I./lib" ++
        (if os == "linux" then " -fPIC" else "")
 