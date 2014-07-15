# Shake Configurations

* For arch dependent directories, i'd put them in a prefix. e.g. have
x86/myfiles/output.crate, arm/myfiles/output.crate, and
fat/myfiles/output.crate. That way you can reuse most of the logic.

data Variant = Release | Debug deriving Show 

main = shakeArgs shakeOptions{shakeChange = ChangeDigest} $ do 
   usingConfigFile "build.cfg" 
   forM_ [Debug, Release] $ \v -> 
      let lv = map toLower $ show v 
      let uv = map toUpper $ show v 
      want [lv] 
      phony lv $ 
          need ["build/" ++ lv ++ "/target.o"] 
      ("build/" ++ lv ++ "/target.o" *> \out -> do 
          cxx <- fromJust <$> getConfig "CXX" 
          cxxFlags <- fromMaybe "" <$> getConfig ("CXX_FLAGS_" ++ uv) 
          cmd cxx cxxFlags "-c" "source/target.cpp" "-o" [out] 

Extending that to have more variants, or have Variant be a pair of 
mode (Debug/Release) and architecture shouldn't be too hard. 
