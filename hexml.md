# New XML Parser, Hexml

_Summary: I've released a new library, Hexml, which is an incomplete-but-fast XML parser._

I've just released [Hexml](https://github.com/ndmitchell/hexml), a new C/Haskell library for DOM-style XML parsing that is fast, but incomplete. To unpack that a bit:

* Hexml is an XML parser that you give a string representing an XML document, it parses that string, and returns either a parse error or a representation of that document. Once you have the document, you can get the child nodes/attributes, walk around the document, and extract the text.

* Hexml is really [a C library](todo: link to hexml.c), which has been designed to be easy to wrap in Haskell, and then a Haskell wrapper on top. It should be easy to use Hexml directly from C if desired.

* Hexml has been designed for speed. In the very limited benchmarks I've done it is typically just over 2x faster at parsing than [Pugixml](todo: link to pugi), where pugixml is the gold standard for fast XML DOM parsers. In my uses it has turned XML parsing from a bottleneck to an irrelevance, so it works for me.

* To gain that speed, Hexml cheats. Primarily it doesn't do entity expansion, so `&amp;` remains as `&amp;` in the output. It also doesn't handle `CData` sections (but that's because I'm lazy) and comment locations are not remembered. It also doesn't deal with most of the XML standard, ignoring the `DOCTYPE` stuff, much like pugixml too.

If you want a more robust version of Hexml then the [Haskell pugixml binding](todo: link) on Hackage is a reasonable place to start, but be warned that it [has memory issues, that can cause segfaults](todo: link to ticket). It also requires C++ which makes use through GHCi more challenging.

## Speed techniques

To make Hexml fast I first read the [chapter on fast parsing with Pugixml](todo: link to book), and stole all those techniques. After that, I introduced a number of my own.

* I only work on UTF8, which for the bits of UTF8 I care about, is the same as ASCII - I don't need to do any character decoding.

* Since I don't do entity expansion, all strings are available in the source document, so everything simply provides offsets into the input string. In the Haskell API I use constant-time bytestring slices into the source string to present a nice API.

* The memory model for a document is an array of attributes, an array of nodes, and a root node from the list of nodes. To make sure that scanning a document is fast, each node describes their attributes and direct child nodes in terms of a start and length within the attribute and node arrays. For example, the root node might have attributes `1..5` in the attribute array, and direct children `4..19` in the node array. When scanning the child nodes there are no linked-list operations and everything is cache friendly.

* To keep the memory compact for attributes, I just have an array and reallocate/copy as necessary. By always doubling the number of attributes on exhaustion I ensure a worst-case of 1-copy per attribute on average.

* To keep the memory compact for nodes is a bit more complex, as the direct child nodes are not necessarily allocated consecutively, as child nodes may themselves have child nodes. The solution is to have an array of nodes, with contiguous allocation of used child nodes starting at the beginning. To ensure the child nodes are continguous I first put the nodes at the end of the array, then copy them after a child is complete -- in effect using the end of the array as a stack. By always doubling the number of nodes on exhaustion I ensure a worst-case of 2-copies per node on average.

* When parsing the text in the body of a document, since I don't care about `&`, the only character that is of any interest is `<`. That allows me to process much of the document with the highly-optimised `memchr`.

* I initially allocate a single buffer that contains the document, a small number of attributes and a small number of nodes, in a single call to `malloc`. If more attributes/nodes are required they allocate a fresh buffer and just ignore the initially provided one. That ensures that for small documents they don't pay for multiple `malloc` calls, at the cost of wasting the initial attribute/node allocation on larger documents (which are more memory heavy anyway - so it doesn't matter).

* I'm pretty sure Hexml could be optimised further. Specifically, I have a recursive descent parser, and it should be a single function with goto. I also process some characters multiple times, mostly to ensure predictable abstraction barriers around the parsing functions, but that could be elimiated with a goto-based approach.
