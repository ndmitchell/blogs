# Downloading all of Hackage

_Summary: I wanted to download the latest version of every package in Hackage. Here's a script and explanation of how to do it._

Imagine you want the latest version of every package on [Hackage](https://hackage.haskell.org/). I found two tools that mirror every version of every package:

* Using [`hackage-mirror`](https://github.com/fpco/hackage-mirror) you can do `hackage-mirror --from="http://hackage.haskell.org" --to="C:/hackage-mirror"`. But this project is long deprecated and doesn't actually work anymore.
* Using [`hackage-mirror-tool`](https://github.com/haskell-hvr/hackage-mirror-tool/) you might be able to do it, but it requires a new Cabal, isn't on Hackage, doesn't seem to work on Windows and doesn't say whether it downloads to disk or not.

Given it's a fairly simple problem, after investigating these options for an hour, I decided to cut my losses and write a script myself. Writing the script took a lot less than an hour, and I even wrote this blog post while the download was running. The complete script is at the bottom of this post, but I thought it might be instructive to explain how I went about developing it.

**Step 0: Set up my working environment**

I created a file named `Download.hs` where I was writing the source code, used `ghcid Download.hs` in a VS Code terminal to get fast error feedback using [Ghcid](https://github.com/ndmitchell/ghcid), and opened another terminal to execute `runhaskell Download.hs` for testing.

**Step 1: Find where a download link is**

You can download a package from Hackage at `http://hackage.haskell.org/package/shake/shake-0.17.tar.gz`. You can also use `https`, but for my purposes and bulk downloading I figured `http` was fine. I hunted around to find a link which didn't contain the version number (as then I wouldn't have to compute the version number), but failed.

**Step 2: Find a list of package versions**

Looking at the `cabal` tool I found the `cabal list --simple` command, which prints a big list of packages in the form:

```
foo 1.0
foo 2.1
bar 1.0
```

For each package on Hackage I get all versions sequentially, with the highest version number last. I can execute this command using `systemOutput_ "cabal list --simple"` (where [`systemOutput_`](https://hackage.haskell.org/package/extra/docs/Extra.html#v:systemOutput_) comes from the [`extra` library](https://hackage.haskell.org/package/extra)).

**Step 3: Generate the list of URLs**

Now I have the data as a big string I want to convert it into a list of URL's. The full pipeline is:

```
map (toUrl . last) . groupOn fst .  map word1 . lines
```

Reading from right to left, I split the output into a list of lines with `lines`, then split each line on its first space (using [`word1`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:word1) from the [`extra` library](https://hackage.haskell.org/package/extra)). I then use `groupOn fst` so that I get consecutive runs of each package (no points for guessing where [`groupOn`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:groupOn) comes from). For each list of versions for a package I take the `last` (since I know that's the highest one) and transform it into the URL using:

```
let toUrl (name, ver) = "http://hackage.haskell.org/package/" ++ name ++ "/" ++ name ++ "-" ++ ver ++ ".tar.gz"
```

**Step 4: Download the URLs**

I could make multiple calls to `wget`, but that's very slow, so instead I write them to a file and make a single call:

```
writeFile "_urls.txt" $ unlines urls
system_ "wget --input-file=_urls.txt"
```

I use the name `_urls.txt` so I can spot that special file in amongst all the `.tar.gz` files this command produces.

**Step 5: Putting it all together**

The complete script is:

```
import Data.List.Extra
import System.Process.Extra

main :: IO ()
main = do
    let toUrl (name, ver) = "http://hackage.haskell.org/package/" ++ name ++ "/" ++ name ++ "-" ++ ver ++ ".tar.gz"
    urls <- map (toUrl . last) . groupOn fst .  map word1 . lines <$> systemOutput_ "cabal list --simple"
    writeFile "_urls.txt" $ unlines urls
    system_ "wget --input-file=_urls.txt"
```

After waiting 46 minutes I had 13,258 packages weighing in at 861Mb.
