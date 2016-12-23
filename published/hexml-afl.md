# Fuzz testing Hexml with AFL

_Summary: Hexml 0.1 could read past the end of the buffer for malformed documents. Fuzz testing detected that and I fixed it in Hexml 0.2._

I released Hexml, my fast DOM-based XML parser, and immediately [Austin Seipp](https://github.com/thoughtpolice) got suspicious. Here was a moderately large piece of C code, taking untrusted inputs, and poking around in the buffer with `memcpy` and `memchr`. He used [American Fuzzy Lop](http://lcamtuf.coredump.cx/afl/) (AFL) to fuzz test the Hexml C code, and came up with a number of issues, notably a buffer read overrun on the fragment:

	<a b=:fallback

With a lot of help from Austin I setup AFL, fixed some issues with Hexml and with how AFL was being run, released Hexml 0.2 fixing these issues and incorporated AFL into my [Travis CI](https://travis-ci.org/ndmitchell/hexml/) builds.

If you want to actually follow all the steps on your computer, I recommend reading the original [GitHub issue](https://github.com/ndmitchell/hexml/issues/6) from Austin. Alternatively, checkout [Hexml](https://github.com/ndmitchell/hexml) and run `sh afl.sh`.

## Building and installing AFL

The first step was to build and install AFL from the tarball, including the LLVM pieces and libdislocator. The LLVM mode allows faster fuzzing, and the libdislocator library provides a library that makes all allocations next to a page boundary - ensuring that if there is a buffer read overrun it results in a segfault than AFL can detect.

## An AFL test case

To run AFL you write a program that takes a filename as an argument and "processes" it. In my case that involves calling `hexml_document_parse` - the full version is [online](https://github.com/ndmitchell/hexml/blob/master/cbits/fuzz.c), but the salient bits are:

	#include "hexml.c"
	... other imports ...
	
	int main(int argc, char** argv)
	{
	    __AFL_INIT();
	    ... read file from argv[0] ...
	    document *doc = hexml_document_parse(contents, length);
	    hexml_document_free(doc);
	    return 0;
	}

Here I statically `#include` the `hexml.c` codebase and have a `main` function that calls `__AFL_INIT` (to make testing go faster), reads from the file, then parses/frees the document. If this code crashes, I want to know about it.

The original AFL driver code used `__AFL_LOOP` to speed things up further, but that results in a huge number of spurious failures, so I removed it.

## Running AFL

To run AFL on my code requires compiling it with one AFL tool, then running it through another. The steps are:

	AFL_HARDEN=1 afl-clang-fast -O2 -Icbits cbits/fuzz.c -o $PWD/hexml-fuzz
	AFL_PRELOAD=/usr/local/lib/afl/libdislocator.so afl-fuzz -T hexml -x /usr/local/share/afl/dictionaries/xml.dict -i $PWD/xml -o $PWD/afl-results -- $PWD/hexml-fuzz @@

I compile with `AFL_HARDEN` to detect more bugs, producing `hexml-fuzz`. I run with `libdislocator` loaded so that my small buffer overrun turns into a fatal segfault. I give `afl-fuzz` a dictionary of common XML fragments and a few simple XML documents, then let it run over `hexml-fuzz`. The interactive UI shows bugs as they occur.

## Fixing the bugs

Running AFL on Hexml 0.1 produced lots of bugs within a few seconds. Each bug produces an input file which I then ran through a debugger. While there were a few distinct bug locations, they all shared a common pattern. Hexml parses a NUL-terminated string, and in some cases I looked at a character that was potentially NUL and consumed it in the parsing. That might consume the final character, meaning that any further parsing was reading past the end of the string. I audited all such occurrences, fixed them, and reran AFL. Since then I have been unable to find an AFL bug despite lots of compute time.

## Running on CI

I run all my code on [Travis CI](https://travis-ci.org/) to ensure I don't introduce bugs, and to make accepting pull requests easier (I don't even need to build the code most of the time). Fortunately, running on Travis isn't too hard:

	AFL_PRELOAD=/usr/local/lib/afl/libdislocator.so timeout 5m afl-fuzz -T hexml -x /usr/local/share/afl/dictionaries/xml.dict -i $PWD/xml -o $PWD/afl-results -- $PWD/hexml-fuzz @@ > /dev/null || true
	cat afl-results/fuzzer_stats
	grep "unique_crashes *: 0" afl-results/fuzzer_stats

I pipe the output of AFL to `/dev/null` since it's very long. I run for 5 minutes with `timeout`. After the timeout hits, I display the `fuzzer_stats` file and then grep for 0 crashes, failing if it isn't there.

## Conclusions

Writing C code is hard, especially if it's performance orientated, and if it's not performance orientated you might want to consider a different language. Even if you don't want to use your code on untrusted input, sooner or later someone else will, and even tiny bugs can result in [complete exploits](https://googleprojectzero.blogspot.co.uk/2016/12/chrome-os-exploit-one-byte-overflow-and.html). AFL does a remarkable job at detecting such issues and has made Hexml the better for it.
