# File Access Tracing

_Summary: It is useful to trace files accessed by a command. Shake and FSATrace provide some tools to do that._

When writing a build system, it's useful to see which files a command accesses. In [the Shake build system](https://shakebuild.com/), we use that information for [linting](https://shakebuild.com/lint), an [auto-deps feature](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:AutoDeps) and a [forward build mode](https://hackage.haskell.org/package/shake/docs/Development-Shake-Forward.html). What we'd like is a primitive which when applied to a command execution:

1. Reports which files are read/written.
2. Reports the start and end time for when the files were accessed.
3. Reports what file metadata is accessed, e.g. modification times and directory listing.
4. Lets us pause a file access (so the dependencies can be built) or deny a file access (so dependency violations can be rejected early).
5. Is computationally cheap.
6. Doesn't require us to write/maintain too much low-level code.
7. Works on all major OSs (Linux, Mac, Windows).
8. Doesn't require `sudo` or elevated privilege levels.

While there are lots of approaches to tracing that get some of those features, it is currently impossible to get them all. Therefore, Shake has to make compromises. The first fours bullet points are about features -- we give up on 2 (timestamps) and 4 (pause/deny); 1 (read/writes) is essential, and we make 3 (metadata) optional, using the imperfect information when its available and tolerating its absence. The last four bullet points are about how it works -- we demand 7 (compatibility) and 8 (no sudo) because Shake must be easily available to its users. We strive for 5 (cheap) and 6 (easy), but are willing to compromise a bit on both.

Shake abstracts the result behind the [`cmd` function](https://hackage.haskell.org/package/shake/docs/Development-Shake-Command.html#v:cmd) with the [`FSATrace` return type](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:FSATrace). As an example I ran in GHCi:

```haskell
traced :: [FSATrace] <- cmd "gcc -c main.c"
print traced
```

Which compiles `main.c` with `gcc`, and on my machine prints 71 entries, including:

```
[ FSARead "C:\\ghc\\ghc-8.6.3\\mingw\\bin\\gcc.exe"
, FSARead "C:\\Neil\\temp\\main.c"
, FSAWrite "C:\\Users\\ndmit_000\\AppData\\Local\\Temp\\ccAadCiR.s"
, FSARead "C:\\ghc\\ghc-8.6.3\\mingw\\bin\\as.exe"
, FSARead "C:\\Users\\ndmit_000\\AppData\\Local\\Temp\\ccAadCiR.s"
, FSAWrite "C:\\Neil\\temp\\main.o"
, ...
]
```

Most of the remaining entries are dlls that `gcc.exe` uses, typically from the Windows directory. I've reordered the list to show the flow more clearly. First the process reads `gcc.exe` (so it can execute it), which reads `main.c` and writes a temporary file `ccAadCiR.s`. It then reads `as.exe` (the assembler) so it can run it, which in turn reads `ccAadCiR.s` and writes `main.o`.

Under the hood, Shake currently uses [FSATrace](https://github.com/jacereda/fsatrace), but that is an implementation detail -- in particular the [BigBro](https://github.com/droundy/bigbro) library might one day [also be supported](https://github.com/droundy/bigbro/issues/6). In order to understand the limitations of the above API, it's useful to understand the different approaches to file system tracing, and which ones FSATrace uses.

**Syscall tracing** On Linux, [`ptrace`](https://www.linuxjournal.com/article/6100) allows tracing every system call made, examining the arguments, and thus recording the files accessed. Moreover, by tracing  the `stat` system call even file queries can be recorded. The syscall tracking approach can be made complete, but because _every_ syscall must be hooked, can end up imposing high overhead. This approach is used by BigBro as well as numerous other debugging and instrumentation tools.

**Library preload** On both Linux and Mac most programs use a dynamically linked C library to make file accesses. By using `LD_LIBRARY_PRELOAD` it is possible to inject a different library into the program memory which intercepts the relevant C library calls, recording which files are read and written. This approach is simpler than hooking syscalls, but only works if all syscall access is made through the C library. While normally true, that isn't the case for [Go programs](https://golang.org/) (syscalls are invoked directly) or statically linked programs (the C library cannot be replaced).

While the technique works on a Mac, from Mac OS X 1.10 onwards system binaries can't be traced due to [System Integrity Protection](https://developer.apple.com/library/content/documentation/Security/Conceptual/System_Integrity_Protection_Guide/ConfiguringSystemIntegrityProtection/ConfiguringSystemIntegrityProtection.html). As an example, the C compiler is typically installed as a system binary. It is possible to disable System Integrity Protection (but not recommended by Apple); or to use non-system binaries (e.g. those supplied by [Nix](https://nixos.org/nix/)); or to copy the system binary to a temporary directory (which works provided the binary does not afterwards invoke another system binary). The library preload mechanism is implemented by FSATrace and the copying system binaries trick on Mac is implemented in Shake.

**File system tracing** An alternative approach is to implement a custom file system and have that report which files are accessed. One such implementation for Linux is [TracedFS](https://github.com/jacereda/traced-fs), which is unfortunately not yet complete. Such an approach can track all accesses, but may require administrator privileges to mount a file system.

**Custom Linux tracing** On Linux, thanks to the open-source nature of the kernel, there are many custom file systems (e.g [FUSE](https://github.com/libfuse/libfuse)) and tracing mechanisms (e.g. [eBPF](http://www.brendangregg.com/ebpf.html)), many of which can be used/configured/extended to perform some kind of system tracing. Unfortunately, most of these are restricted to Linux only.

**Custom Mac tracing** [BuildXL](https://github.com/Microsoft/BuildXL/) uses a [Mac sandbox](https://github.com/Microsoft/BuildXL/blob/master/Documentation/Specs/Sandboxing.md#macos-sandboxing) based on [KAuth](https://flylib.com/books/en/3.126.1.140/1/) combined with [TrustedBSD Mandatory Access Control (MAC)](http://www.trustedbsd.org/mac.html) to both detect which files are accessed and also block access to specific files. The approach is based on internal Mac OS X details which have been reversed engineered, some of which are deprecated and scheduled for removal.

**Windows Kernel API hooking** On Windows it is possible to hook the Kernel API, which can be used to detect when any files are accessed. Implementing such a hook [is difficult](https://github.com/jacereda/fsatrace/blob/master/src/win/patch.c), particularly around 32bit v 64bit differences, as [custom assembly language trampolines must be used](https://stackoverflow.com/questions/494284/createremotethread-32-64-and-or-64-32). Furthermore, some antivirus products (incorrectly) detect such programs as viruses. Windows kernel hooking is available in both FSATrace and BigBro (sharing the same source code), although without support for 32bit processes that spawn 64bit processes.

**Current State**

Shake currently uses FSATrace, meaning it uses library preloading on Linux/Mac and kernel hooking on Windows. The biggest practical limitations vary by OS:

* On **Linux** it can't trace into Go programs (or other programs that use system calls directly) and statically linked binaries. Integrating BigBro as an alternative would address these issues.
* On **Mac** it can't trace into system binaries called from other system binaries, most commonly the system C/C++ compiler. Using your own C/C++ installation, via [Homebrew](https://brew.sh/) or Nix, is a workaround.
* On **Windows** it can't trace 64bit programs spawned by 32bit programs. In most cases the 32bit binaries can easily be replaced by 64bit binaries. The only problem I've seen was caused by a five year-old version of `sh` hiding out in my `C:\bin` directory, which was easily remedied with a newer version. The code to fix this issue [is available](https://github.com/rapid7/metasploit-payloads/blob/master/c/meterpreter/source/metsrv/base_inject.c), but scares me too much to try integrating.

Overall, the tracing available in Shake has a simple API, is very useful for Shake, and has been repurposed in [other build systems](https://blogs.ncl.ac.uk/andreymokhov/stroll/). But I do dearly wish such functionality could be both powerful and standardised!
