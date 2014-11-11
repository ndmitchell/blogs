# Monadic Build Systems are more Compositional

I've been in favour of builds systems with monadic dependencies for a while. But why. They're more compositional. Imagine:

    cat encrypted.txt | rot13 > main.c
    gcc -c main.c 

How can we model the dependencies. In Shake we can write:

    "main.o" *> \out -> do
        need ["encrypted.txt"]
        cmd Shell "cat encrypted.txt | rot13 > main.c"
        Stdout out <- cmd "gcc -c -M main.c"
        needMakefile out

All easy enough. Writing that same snippet in an applicative build system is difficult. You have two options:

* Guess the include files from `encrypted.txt` in advance.
* Run it in two phases.

Now it turns out there are almost no purely applicative build systems - they all have slightly more power than that. For example, picking Ninja we can write:

    main.o: encrypted.txt
        cmd = (cat encrypted.txt | rot13 > main.c) && gcc -c -M main.c
        deps = afterwards

What I'd argue is that only a monadic build system can be structured so it's compositional. With fairly local changes we can generalise.