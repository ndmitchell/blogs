# Optimising Haskell: a tight inner loop

_Summary: I walk through optimising a Haskell string splitter to get a nice short inner loop in assembly. We look at the Haskell code, the generated Core, C-- and assembly._

Let's start with some simple code:

    break (`elem` " \r\n$") src

This code scans a string looking for a space, newline or `$` and returns the string before and the string after. Our goal is to make this code faster - by the end we'll get down to 6 assembly instructions per input character. Before making things go faster we should write test cases (so we don't break anything), profile (so we are optimising the right thing) and write benchmarks (to check our changes make things go faster). For this article, we're going to skip all that, and only look at the generated Core, C-- and assembly - making hypothesises about what _should_ go faster. The complete code is [available online](https://github.com/ndmitchell/blogs/blob/master/inner-loop/InnerLoop.hs), along with the [Core/C--/assembly for each step](https://github.com/ndmitchell/blogs/tree/master/inner-loop).

** Version 1**

To turn our example into a complete program, we can write:

    module InnerLoop(innerLoop) where

    innerLoop :: FilePath -> IO (String, String)
    innerLoop file = do
        src <- readFile file
        return $ break test src

    test x = x `elem` " \r\n$"

We can save this code as `InnerLoop.hs` and compile it with:

    ghc -c -O2 InnerLoop.hs -ddump-simpl -ddump-cmm -ddump-asm > log.txt

The full output of `log.txt` is available [here](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log1.txt). It contains the GHC Core (which looks a bit like Haskell), then the C-- (which looks a bit like C) and finally the assembly code (which looks exactly like assembly). When optimising we usually look at the Core, then at the C--, then at the assembly - stopping whenever our profiling says we are done. Let's take a look at the inner loop in Core (with some light editing):

    innerLoop_3 = GHC.CString.unpackCString# " \r\n\$"

    test_1 = \ (x :: GHC.Types.Char) ->
        GHC.List.elem @ GHC.Types.Char GHC.Classes.$fEqChar x innerLoop_3

    innerLoop_2 =
        ...
        case GHC.List.$wbreak @ GHC.Types.Char test_1 x of _
            (# a, b #) -> (a, b)
        ...

The best way to read the Core is by looking for what you can understand, and ignoring the rest - it contains a lot of boring detail. We can see that a lot of things are fully qualified, e.g. `GHC.List.elem`. Some things have also been a bit mangled, e.g. `$wbreak`, which is roughly `break`. The interesting thing here is that `break` is being passed `test_1`. Looking at `test_1` (which will be called on each character), we can see we are passing `GHC.Classes.$fEqChar` - a pair containing a function of how to perform equality on characters - to the `elem` function. For each character we are going to end up looping through a 4 element list (`innerLoop_3`) and each comparison will be going through a higher order function. Clearly we need to improve our `test` function.

**Version 2**

We can unroll the `elem` in `test` to give:

    test x = x == ' ' || x == '\r' || x == '\n' || x == '$'

Compiling again and looking at [the Core](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log2.txt) we see:

    test_2 =
      \ (x :: GHC.Types.Char) ->
        case x of _ { GHC.Types.C# c ->
        case c of _ {
          __DEFAULT -> GHC.Types.False;
          '\n' -> GHC.Types.True;
          '\r' -> GHC.Types.True;
          ' ' -> GHC.Types.True;
          '$' -> GHC.Types.True
        }
        }

Now for each character we extract the raw character (pattern matching against `C#`) then test it against the possibilities. GHC has optimised our repeated `==`/`||` into a nice `case` expression. It looks quite nice. Now the bottleneck is the `break` function.

**Version 3**

The `break` function is working on a `String`, which is stored as a linked list of characters. To get better performance we can move to `ByteString`, writing:

    innerLoop :: FilePath -> IO (ByteString, ByteString)
    innerLoop file = do
        src <- BS.readFile file
        return $ BS.break test src

For many people this is the reasonable-performance version they should stick with. However, let's look at [the Core](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log3.txt) once more:

    go = \ (a :: Addr#) (i :: Int#) (w :: State# RealWorld) ->
        case i >=# len of _ {
          GHC.Types.False ->
            case readWord8OffAddr# @ GHC.Prim.RealWorld a 0 w
            of _ { (# w, c #) ->
            case chr# (word2Int# c) of _ {
              __DEFAULT -> go (plusAddr# a 1) (i +# 1) w;
              '\n' -> (# w, GHC.Types.I# i #);
              '\r' -> (# w, GHC.Types.I# i #);
              ' ' -> (# w, GHC.Types.I# i #);
              '$' -> (# w, GHC.Types.I# i #)
            }
            };
          GHC.Types.True -> (# w, l_a1J9 #)
        }

The first thing that should strike you is the large number of `#` symbols. In Core, a `#` means you are doing strict primitive operations on unboxed values, so if the optimiser has managed to get down to `#` that is good. You'll also notice values of type `State# RealWorld` which I've renamed `w` - these are an encoding of the `IO` monad, but have zero runtime cost, so can be ignored. Looking at the rest of the code, we have a loop with a pointer to the current character (`a :: Addr#`) and an index of how far through the buffer we are (`i :: Int#`). At each character we first test if the index exceeds the length, and if it doesn't, read a character and match it against the options. If it doesn't match we continue by adding 1 to the address and 1 to the index. Of course, having to loop over two values is a bit unfortunate.

**Version 4**

A `ByteString` needs an explicit length so it knows when it has come to the end of the buffer, so needs to keep comparing against explicit lengths (and for efficiency reasons, also maintaining those lengths). Looking to C for inspiration, typically strings are terminated by a `\0` character, which allows looping without comparing against a length. We can define our own null-terminated `ByteString` type with a `break` operation:

    newtype ByteString0 = BS0 ByteString
    
    readFile0 :: FilePath -> IO ByteString0
    readFile0 x = do
        src <- BS.readFile x
        return $ BS0 $ src `BS.snoc` '\0'

We define a `newtype` wrapper around `ByteString` so we gain some type safety. We also define a `readFile0` that reads a file as a `ByteString0`, by explicitly calling `snoc` with `\0`. We can now define our own `break0` function (this is the only big chunk of Haskell in this article):

    break0 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
    break0 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
        where
            i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
                let start = castPtr ptr :: Ptr Word8
                let end = go start
                return $! end `minusPtr` start
    
            go s | c == '\0' || f c = s
                 | otherwise = go $ inc s
                where c = chr s
    
    chr :: Ptr Word8 -> Char
    chr x = Internal.w2c $ Internal.inlinePerformIO $ peek x
    
    inc :: Ptr Word8 -> Ptr Word8
    inc x = x `plusPtr` 1
    
We define `break0` by finding the position at which the condition stops being true (`i`) and calling `unsafeTake`/`unsafeDrop` to slice out the relevant pieces. Because we know the second part is still null terminated we can rewrap in ` ByteString0`. To find the index, we mostly use code copied from the bytestring library and modified. We convert the `ByteString` to a `Ptr CChar` using `unsafeUseAsCString` which just lets us look at the internals of the `ByteString`. We then loop over the pointer with `go` until we get to the first character that passes `f` and find how far we travelled. The function `go` looks at the current character using `chr`, and if it's `\0` (the end) or the function `f` passes, returns the address at this point. Otherwise it increments the pointer. We use `chr` to peek at the pointer directly, and `inlinePerformIO` to do so purely and fast - since we know these buffers are never modified, the `inlinePerformIO` is morally defensible (we could have put `chr` in `IO` but that breaks a future optimisation we'll need to do).

Compiling [to Core](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log1.txt) we see:

    go = \ (x :: GHC.Prim.Addr#) ->
        case readWord8OffAddr# @ RealWorld x 0 realWorld#
        of _ { (# _, c #) ->
        case GHC.Prim.chr# (GHC.Prim.word2Int# c) of _ {
          __DEFAULT -> go (GHC.Prim.plusAddr# x 1);
          '\NUL' -> GHC.Ptr.Ptr @ GHC.Word.Word8 x;
          '\n' -> GHC.Ptr.Ptr @ GHC.Word.Word8 x;
          '\r' -> GHC.Ptr.Ptr @ GHC.Word.Word8 x;
          ' ' -> GHC.Ptr.Ptr @ GHC.Word.Word8 x;
          '$' -> GHC.Ptr.Ptr @ GHC.Word.Word8 x
        }

Now we have a Core inner loop to be proud of. We loop round with a single pointer, peek at a byte, and compare it to our options. Time to look onwards to the C--, where I've included just the inner loop:

    InnerLoop.$wgo_info()
        c1Tt:
            Hp = Hp + 8;
            if (Hp > HpLim) goto c1Tx;
            _s1RN::I32 = %MO_UU_Conv_W8_W32(I8[I32[Sp + 0]]);
            _s1T5::I32 = _s1RN::I32;
            _s1T6::I32 = _s1T5::I32;
            if (_s1T6::I32 < 13) goto c1TG;
            if (_s1T6::I32 < 32) goto c1TH;
            if (_s1T6::I32 < 36) goto c1TI;
            if (_s1T6::I32 != 36) goto c1TJ;
            ...
        ...
        c1TJ:
            _s1T4::I32 = I32[Sp + 0] + 1;
            I32[Sp + 0] = _s1T4::I32;
            Hp = Hp - 8;
            jump InnerLoop.$wgo_info; // []
        ...    

Reading the code, we first mess around with `Hp`, then pull a value out of the array and into `_s1RN`, then do some comparisons, and if they don't match jump to `c1TJ`, mess around with `Hp` again and jump back to start again.

There are three obvious problems with the code: 1) we mess around with `Hp`; 2) we are doing too many tests to get to the default case; 3) there is a jump in the middle of the loop.

**Version 5**

Let's start with the `Hp` variable. `Hp` is the heap pointer, which says how much heap GHC is using - if the heap gets above a certain limit, it triggers a garbage collection. The `Hp = Hp + 8` reserves 8 bytes of heap for this function, `Hp > HpLim` checks if we need to garbage collect, and `Hp = Hp - 8` at the bottom of the loop gives back that heap space. Why do we allocate 8 bytes, only to give it back at the end? The reason is that in the return path after the loop we do allocation. It's a [long standing performance issue](https://ghc.haskell.org/trac/ghc/ticket/1498) that GHC doesn't push the heap test down to the exit path, but we can fix it ourselves. Looking at the Core, we saw:

    case GHC.Prim.chr# (GHC.Prim.word2Int# c) of _ {
      __DEFAULT -> go (GHC.Prim.plusAddr# x 1);
      '\NUL' -> GHC.Ptr.Ptr @ GHC.Word.Word8 x;
 
The expression `GHC.Ptr.Ptr @ GHC.Word8 x` is allocating a constructor around the pointer to return. Looking at the `Ptr` type we discover:

    data Ptr a = Ptr Addr#

So `Ptr` is simply a constructor wrapping our address. To avoid the `Ptr` in the inner loop, we can switch to returning `Addr#` from `go`:

    i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
        let start = castPtr ptr :: Ptr Word8
        let end = go start
        return $! Ptr end `minusPtr` start

    go s@(Ptr a) | c == '\0' || f c = a
                 | otherwise = go $ inc s
        where c = chr s

We also add back the `Ptr` around `end` do call `minusPtr`. Looking at [the Core](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log5.txt) we now see a very simple return path:

    case GHC.Prim.chr# (GHC.Prim.word2Int# ipv1_a1D0) of _ {
      __DEFAULT -> InnerLoop.$wgo (GHC.Prim.plusAddr# ww_s1PR 1);
      '\NUL' -> ww_s1PR;

And dropping down to C-- we see:

     c1Td:
         _s1Ry::I32 = %MO_UU_Conv_W8_W32(I8[I32[Sp + 0]]);
         _s1SP::I32 = _s1Ry::I32;
         _s1SQ::I32 = _s1SP::I32;
         if (_s1SQ::I32 < 13) goto c1Tn;
         if (_s1SQ::I32 < 32) goto c1To;
         if (_s1SQ::I32 < 36) goto c1Tp;
         if (_s1SQ::I32 != 36) goto c1Tq;
         R1 = I32[Sp + 0];
         Sp = Sp + 4;
         jump (I32[Sp + 0]); // [R1]
     c1Tq:
         _s1SO::I32 = I32[Sp + 0] + 1;
         I32[Sp + 0] = _s1SO::I32;
         jump InnerLoop.$wgo_info; // []

Not a single mention of `Hp`. We still have a lot more tests than we'd like though.

**Version 6**

The current code to check for our 5 terminating characters compares each character one by one. This entire example is based on lexing [Ninja source files](http://martine.github.io/ninja/manual.html#_lexical_syntax), so we know that most characters will be alphanumeric. Using this information, we can instead test if the character is less than or equal to `$`, if it is we can test for the different possibilities, otherwise continue on the fast path. We can write:

    test x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$')

Now looking at [the Core](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log1.txt) we see:

    go = \ (ww_s1Qt :: GHC.Prim.Addr#) ->
        case GHC.Prim.readWord8OffAddr#
               @ GHC.Prim.RealWorld ww_s1Qt 0 GHC.Prim.realWorld#
        of _ { (# _, ipv1_a1Dr #) ->
        case GHC.Prim.chr# (GHC.Prim.word2Int# ipv1_a1Dr) of wild_XH {
          __DEFAULT ->
            case GHC.Prim.leChar# wild_XH '$' of _ {
              GHC.Types.False -> go (GHC.Prim.plusAddr# ww_s1Qt 1);
              GHC.Types.True ->
                case wild_XH of _ {
                  __DEFAULT -> go (GHC.Prim.plusAddr# ww_s1Qt 1);
                  '\n' -> ww_s1Qt;
                  '\r' -> ww_s1Qt;
                  ' ' -> ww_s1Qt;
                  '$' -> ww_s1Qt
                }
            };
          '\NUL' -> ww_s1Qt
        }
        }

The code looks reasonable, but the final `\NUL` indicates that the code first checks if the character is `\NUL` (or `\0`) and only then does our fast `< $` test.

** Version 7**

To perform our `< $` test before checking for `\0` we need to modify `go`. We require that the argument predicate must return `False` on `\0` (otherwise we'll run off the end of the string) and can then write:

    go s@(Ptr a) | f c = a
                 | otherwise = go $ inc s
        where c = chr s

[The Core](https://github.com/ndmitchell/blogs/blob/master/inner-loop/log7.txt) reads:

    InnerLoop.$wgo =
      \ (ww_s1Qq :: GHC.Prim.Addr#) ->
        case GHC.Prim.readWord8OffAddr#
               @ GHC.Prim.RealWorld ww_s1Qq 0 GHC.Prim.realWorld#
        of _ { (# _, ipv1_a1Dr #) ->
        let {
          c1_a1uU [Dmd=Just L] :: GHC.Prim.Char#
          [LclId, Str=DmdType]
          c1_a1uU = GHC.Prim.chr# (GHC.Prim.word2Int# ipv1_a1Dr) } in
        case GHC.Prim.leChar# c1_a1uU '$' of _ {
          GHC.Types.False -> InnerLoop.$wgo (GHC.Prim.plusAddr# ww_s1Qq 1);
          GHC.Types.True ->
            case c1_a1uU of _ {
              __DEFAULT -> InnerLoop.$wgo (GHC.Prim.plusAddr# ww_s1Qq 1);
              '\NUL' -> ww_s1Qq;
              '\n' -> ww_s1Qq;
              '\r' -> ww_s1Qq;
              ' ' -> ww_s1Qq;
              '$' -> ww_s1Qq
            }
        }
        }

The C-- reads:

    InnerLoop.$wgo_info()
    c1Uf:
         _s1Se::I32 = %MO_UU_Conv_W8_W32(I8[I32[Sp + 0]]);
         _s1Sh::I32 = _s1Se::I32;
         _s1Sg::I32 = _s1Sh::I32;
         _c1TZ::I32 = _s1Sg::I32 <= 36;
         ;
         if (_c1TZ::I32 >= 1) goto c1Ui;
         _s1Ty::I32 = I32[Sp + 0] + 1;
         I32[Sp + 0] = _s1Ty::I32;
         jump InnerLoop.$wgo_info; // []

And the assembly reads:

    InnerLoop.$wgo_info:
    _c1Uf:
    	movl 0(%ebp),%eax
    	movzbl (%eax),%eax
    	cmpl $36,%eax
    	jbe _c1Ui
    	incl 0(%ebp)
    	jmp InnerLoop.$wgo_info

We have ended up with a fairly small 6 instruction loop.

**Version 8**

We've now exhausted my Haskell bag of tricks, and have to stop. But the assembly code could still be improved. In each loop we read the contents of the memory at `%ebp` into `%eax`, and increment the contents of the memory at `%ebp` at the end - we're manipulating the value on the top of the stack (which is pointed to by `%ebp`). We could instead cache that value in `%ebx`, and write:

    _c1Uf:
    	movzbl (%ebx),%eax
    	cmpl $36,%eax
    	jbe _c1Ui
    	incl %ebx
    	jmp _c1Uf

One less instruction, two less memory accesses. I tried the LLVM backend, but it generated significantly [worse assembly code](https://github.com/ndmitchell/blogs/blob/master/inner-loop/InnerLoop.s). I don't know how to optimise any further without dropping down to the C FFI, but I'm sure one day GHC/LLVM will automatically produce the shorter assembly code.
