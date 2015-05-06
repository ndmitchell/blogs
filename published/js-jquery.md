# Announcing js-jquery Haskell Library

_Summary: The library js-jquery makes it easy to get at the jQuery Javascript code from Haskell. I've just released a new version._

I've just released the Haskell library [js-jquery 1.11.3](https://hackage.haskell.org/package/js-jquery), following the announcement of [jQuery 1.11.3](http://blog.jquery.com/2015/04/28/jquery-1-11-3-and-2-1-4-released-ios-fail-safe-edition/). This package bundles the minified [jQuery](http://jquery.com/) code into a Haskell package, so it can be depended upon by Cabal packages. The version number matches the upstream jQuery version. It's easy to grab the jQuery code from Haskell using this library, as an example:

    import qualified Language.Javascript.JQuery as JQuery

    main = do
        putStrLn $ "jQuery version " ++ show JQuery.version ++ " source:"
        putStrLn =<< readFile =<< JQuery.file

There are two goals behind this library:

* **Make it easier for jQuery users** to use and upgrade jQuery in Haskell packages. You can upgrade jQuery without huge diffs and use it without messing around with extra-source-files.
* **Make it easier for upstream packagers** like Debian. The addition of a jQuery file into a Haskell package means you are mixing licenses, authors, and distributions like Debian also require the source (unminified) version of jQuery to be distributed as well. By having one package provide jQuery they only have to do that work once, and the package has been designed to meet their needs.

It's pretty easy to convert something that has bundled jQuery to use the library, as some examples:

* [Shake converted](https://github.com/ndmitchell/shake/commit/5bb9c038333b46db3636c62c76f239e899ef5d0a) in version 0.14.
* I wrote a pull request to convert [criterion](https://github.com/bos/criterion/pull/72) (not yet applied, or commented on).
* [Iustin Pop](http://k1024.org/~iustin/) wrote a pull request to convert [ekg](https://github.com/tibbe/ekg/pull/39) (being discussed [here](https://github.com/tibbe/ekg/issues/38)).

The library only depends on the `base` library so it shouldn't cause any version hassles, although (as per all Cabal packages) you can't mix and match libraries with incompatible `js-jquery` version constraints in one project. 

As a companion, there's also [`js-flot`](https://hackage.haskell.org/package/js-flot), which follows the same ideas for the [Flot](http://www.flotcharts.org/) library.
