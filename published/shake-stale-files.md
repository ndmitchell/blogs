# Cleaning stale files with Shake

_Summary: Sometimes source files get deleted, and build products become stale. Using Shake, you can automatically delete them._

Imagine you have a build system that compiles Markdown files into HTML files for your blog. Sometimes you rename a Markdown file, which means the corresponding HTML will change name too. Typically, this will result in a _stale_ HTML file being left, one that was previously produced by the build system, but will never be updated again. You can remove that file by cleaning all outputs and running the build again, but with the [Shake build system](http://shakebuild.com) you can do better. You can ask for a list of all _live_ files, and delete the build products not on that list.

**A basic Markdown to HTML converter**

Let's start with a simple website generator. For each Markdown file, with the extension `.md`, we generate an HTML file. We can write that as:

	import Development.Shake
	import Development.Shake.FilePath
	
	main :: IO ()
	main = shakeArgs shakeOptions $ do
	    action $ do
	        mds <- getDirectoryFiles "." ["//*.md"]
	        need ["output" </> x -<.> "html" | x <- mds]
	
	    "output//*.html" %> \out -> do
	        let src = dropDirectory1 out -<.> "md"
            need [src]
	        cmd "pandoc -s -o" [out, src]
	
	    phony "clean" $ do
	        removeFilesAfter "output" ["//*.html"]

Nothing too interesting here. There are three parts:

* Search for all `.md` files, and for each file `foo/bar.md` require `output/foo/bar.html`.
* To generate an `.html` file, depend on the source file then run [`pandoc`](http://pandoc.org/README.html).
* To clean everything, delete all `.html` files in `output`.

Using a new feature in Shake 0.15, we can name save this script as `Shakefile.hs` and then:

* `shake` will build all the HTML files.
* `shake -j0` will build all the files, using one thread for each processor on our system.
* `shake output/foo.html` will build just that one HTML file.
* `shake clean` will delete all the HTML files.

**Removing stale files**

Now let's imagine we've added a blog post `using-pipes.md`. Before publishing we decide to rename our post to `using-conduit.md`. If we've already run `shake` then there will be a stale file `output/using-pipes.html`. Since there is no source `.md` file, Shake will not attempt to rebuild the file, and it won't be automatically deleted. We can do `shake clean` to get rid of it, but that will also wipe all the other HTML files.

We can run `shake --live=live.txt` to produce a file `live.txt` listing all the live files - those that Shake knows about, and has built. If we run that after deleting `using-pipes.md` it will tell us that `using-conduit.md` and `output/using-conduit.md` are both "live". If we delete all files in `output` that are not mentioned as being live, that will clean away all our stale files.

Using Shake 0.15.1 (released in the last hour) you can write:

	import Development.Shake
	import Development.Shake.FilePath
	import Development.Shake.Util
	import System.Directory.Extra
	import Data.List
	import System.IO
	
	pruner :: [FilePath] -> IO ()
	pruner live = do
	    present <- listFilesRecursive "output"
	    mapM_ removeFile $ map toStandard present \\ map toStandard live
	
	main :: IO ()
	main = shakeArgsPrune shakeOptions pruner $ do
         ... as before ...

Now when running `shake --prune` it will build all files, then delete  all stale files, such as `output/using-pipes.html`. We are using the `shakePrune` function (just sugar over `--live`) which lets us pass a `pruner` function. This function gets called after the build completes with a list of all the live files. We use [`listFilesRecursive`](https://hackage.haskell.org/package/extra/docs/System-Directory-Extra.html#v:listFilesRecursive) from the [`extra` package](https://hackage.haskell.org/package/extra) to get a list of all files in `output`, then do list difference (`\\`) to delete all the files which are present but not live. To deal with the `/` vs `\` path separator issue on Windows, we apply `toStandard` to all files to ensure they match.

A few words of warning:

* If you run `shake output/foo.html --prune` then it will only pass `output/foo.html` and `foo.md` as live files, since they are the only ones that are live as you have asked for a subset of the files to be built. Generally, you want to enable all sensible targets (typically no file arguments) when passing `--prune`.
* Sometimes a rule will generate something you care about, and a few files you don't really bother tracking. As an example, building a GHC DLL on Windows generates a `.dll` and a `.dll.a` file. While the `.dll.a` file may not be known to Shake, it probably doesn't want to get pruned. The pruning function may need a few special cases, like not deleting the `.dll.a` file if the `.dll` is live.
