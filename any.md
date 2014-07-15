BLOG IDEA: Any
BLOG IDEA: How capture works

ANY

CmdArgs 0.1 was a big user of Data.Data (from the SYB framework). It tweaks things a lot, gets fields, set fields, and generally manipulates runtime values. Every time I manage to get something working with Data.Data I always get a feeling of great acomplishment - it's a great framework, but even the simple things feel complex.

My personal feeling is that Data.Data has two entirely separate purposes. In reality it's a reflection facility for Haskell, to construct and deconstruct runtime values. On top of this reflection layer people have written a query/transformation library. During my PhD I wrote Uniplate, which is a query/transformation library, and which I think 


During my PhD I wrote Uniplate, which performs generic queries and traversals, which is probably the most common use for SYB. However, Uniplate can't deal with the 


I also wrote a new SYB framework, which I will probably make available in Uniplate. Instead of the confusing bits it is based around:

    Any :: Data a => a -> Any
    fromAny :: Data a => Any -> a
    compose0 :: Any -> CtorName -> Any
    recompose :: Any -> [Any] -> Any
    ctor :: Any -> Ctor
    children :: Any -> [Any]
    ctors :: Any -> [Ctor]

These operations 
