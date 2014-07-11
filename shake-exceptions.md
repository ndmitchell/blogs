# Exception handling in Shake

In order to make use of the 'temporary' package with shake, as well as a few other packages, could you add a deriving clause for MonadThrow and MonadCatch from the 'exceptions' package?  That package is becoming a very handy way to abstract over exceptions. I see that Action is just a newtype wrapper around Strict.StateT, so it should be pretty formulaic.

Thanks for the pointer to exceptions. My only hesitation is that in
Shake I deliberately don't provide a way to catch exceptions - there
is onException and finally, but no catch. That ensures that you can't
write a build rule that tries a dependent build rule, and if it
throws, does something else which doesn't raise an exception. If I
switched to exceptions, I lose that guarantee. That guarantee is nice,
but I haven't ever taken the time to figure out for sure whether that
guarantee is required for correct semantics of build systems (I
suspect not, but it's not totally obvious either way).

On the other hand, I can see why you want to use things like temporary
or other packages with Shake, so perhaps it's a price worth paying...
Was this a theoretical issue, or are you actually practically wanting
to use temporary with Shake?

Have you seen actionFinally?

    ghcBin *> \_out -> do
        let ghcTarball' = "deps" </> ghcTarball
        need [ appRoot, ghcTarball' ]
        tmpDir <- liftIO getTemporaryDirectory
        path <- liftIO $ createTempDirectory tmpDir "ghc"
        actionFinally (removeDirectoryRecursive path) $ do
