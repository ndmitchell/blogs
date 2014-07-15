Haskell Records

* Defining a record:

data Foo = Foo {bar :: Int, foo :: String}

* Record accessors

bar and foo come for free

* Record setters

can set them with s{foo=1} and S{foo=1}. undefined records remain undefined. S{foo=1,bar=1}. this pattern is often used for default arguments.

* Record pattern matching

Foo{bar=1}

Also do:

Foo{bar=bar}

* Record punning

For both pattern matching and setting.

Foo{bar}

Foo{..}

* Multiconstructor records

Left til last because they aren't that powerful, and I find the standard use case is a single constructor.

data Foo = Foo {bar :: Int, foo :: String}
         | Foo2 {bar :: Int, baz :: Maybe Int}

Most things still work.
