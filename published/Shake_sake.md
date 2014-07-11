# Shake as a dependency library

_Summary: You can use Shake as a library to implement other build tools._

The [Shake build tool](https://github.com/ndmitchell/shake#readme) is often used to define a specific build system, as an alternative to Make. But Shake is really [a library](http://hackage.haskell.org/package/shake), and can be used to implement other build tools. In this post I'm going to show a rough implementation of the [Sake build tool](http://tonyfischetti.github.io/sake/) using Shake.

**What is Sake?**

Extracted from the [Sake documentation](http://tonyfischetti.github.io/sake/sake-doc.html):

> Sake is a way to easily design, share, build, and visualize workflows with intricate interdependencies. Sake is a simple and self-documenting build system, targeted at scientists, data analysts and business teams.

The Sake build rules are defined in [YAML](http://www.yaml.org/), and a simple example is:

    create the input:
        help: create the input file
        formula: echo test > input.txt
        output:
            - input.txt
    convert to uppercase:
        help: change the input file to uppercase
        dependencies:
            - input.txt
        formula: cat input.txt | tr '[a-z]' '[A-Z]' > output.txt
        output:
            - output.txt

Sake build rules are simple, contain lots of help text, and are quite explicit. I can see why some users would prefer it to Shake or Make (especially as the Sake tool also produces nice visualisations and help information).

**Sake on top of Shake**

This section contains an implementation of Sake that can execute the file above, along with [tests from the Sake repo](https://github.com/tonyfischetti/sake/blob/master/tests/test2/Sakefile.yaml). I'm going to intersperse the implementation along with some notes. First we give language extensions and imports:

    {-# LANGUAGE OverloadedStrings #-}
    import Control.Applicative
    import Control.Exception
    import Development.Shake
    import Data.Yaml
    import qualified Data.HashMap.Strict as Map
    import qualified Data.Vector as Vector
    import qualified Data.Text as Text

The interesting imports are `Shake` (the build system) and [`Yaml`](http://hackage.haskell.org/package/yaml) (the parser for YAML files). Our `main` function loads the Sake YAML file, then defers to Shake:

    main = do
        build <- either throw id <$> decodeFileEither "Sakefile.yaml"
        shakeArgs shakeOptions $ elaborate build

We are using `shakeArgs` to get Shake to provide command line handling for our tool. The interesting part is `elaborate`, which translates the Sake rules into Shake rules. We define `elaborate` as:

    elaborate (Object x) | Map.member "formula" x = do
        let formula = fromString $ x Map.! "formula"
        let dependencies = map fromString . fromArray <$> Map.lookup "dependencies" x
        let output = map fromString . fromArray <$> Map.lookup "output" x
        let act = do
                maybe alwaysRerun need dependencies
                command_ [] "sh" ["-c",formula]
        case output of
            Nothing -> action act
            Just output -> do want output; output *>> \_ -> act
    elaborate (Object x) = mapM_ elaborate $ Map.elems x
    elaborate _ = return ()

The first case is the interesting one. We look for `formula` fields which indicate build rules. We extract out the fields `formula`, `dependencies` and `output`. We then define `act` which is the action Shake will run:

    maybe alwaysRerun need dependencies
    command_ [] "sh" ["-c",formula]

If there were no dependencies, we always rerun the rule, otherwise we require the dependencies using `need`. Next we run the `formula` command using `sh`. Then we define the rules:

    case output of
        Nothing -> action act
        Just output -> do want output; output *>> \_ -> act

If a Sake rule has no output field, then it is always run, which Shake specifies with `action`. Otherwise we `want` the output (since all Sake outputs are always built) and define a rule producing multiple outputs (the `*>>` function) which runs `act`. Finally, we have a few helpers to extract the fields from the YAML:

    fromString (String x) = Text.unpack x
    fromArray (Array x) = Vector.toList x
    fromArray Null = []

Note that the full Sake implementation contains additional features and error checking. However, I think it is quite nice that a reimplementation of the basics can be done in only 16 lines of Haskell. The reimplementation also supports several features that the original Sake does not, including profiling, [progress reporting](http://neilmitchell.blogspot.co.uk/2013/12/progress-reporting-in-shake.html) and staunch mode.

**Conclusions**

Shake is capable of implementing other build tools, and can be used as a build system in its own right, or a library supplying dependency tracking. I believe there is plenty scope for higher-level build specifications ([Cabal](http://www.haskell.org/cabal/) is one example), and hope that these tools can delegate their dependency logic to Shake.
