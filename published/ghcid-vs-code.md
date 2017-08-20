# Ghcid and VS Code

_Summary: There's now a Ghcid VS Code addin that gives you red squiggles._

I've been using [Ghcid](http://hackage.haskell.org/package/ghcid) for about 3 years, and [VS Code](https://code.visualstudio.com/) for about 6 months. Ghcid alone is able to display the errors and warnings, and update them whenever you save. In this post I'll show how VS Code users can also click on errors to jump to them, and how to get red squiggles in the text buffer for errors.

**Clicking on errors**

Using a recent VS Code, if you run `ghcid` from the terminal window, hold `Ctrl` and click the filename and it jumps to the right location in the erroneous file.

**Red squiggles**

Red squiggles are now possible using the [haskell-ghcid](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid) addin. To get it working:

* Run `ghcid -o ghcid.txt` which will produce a file `ghcid.txt` which updates every time `ghcid` updates. Running the underlying `ghci` with `-ferror-spans` will significantly improve the errors reported.
* Open `ghcid.txt` in VS Code as the active editor. Run the VS Code command (`Ctrl+Shift+P`) named "Watch Ghcid output".

These steps cause the `ghcid` errors to appear in the VS Code Problems pane, and have red squiggles in the editor. Even though the errors are in the proper problems pane, I still prefer the output provided by the `ghcid` terminal, so still look at that.

The VS Code addin is not very well polished - but I'm using it on a daily basis.
