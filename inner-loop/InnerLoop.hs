{-# OPTIONS_GHC -O2 #-}

module InnerLoop(innerLoop_7) where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as Internal
import Foreign
import GHC.Exts


test_1 x = x `elem` " \r\n$"

test_2 x = x == ' ' || x == '\r' || x == '\n' || x == '$'

test_3 x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$')

test_4 x = x <= '$' && (x == ' ' || x == '\r' || x == '\n' || x == '$' || x == '\0')

chr :: Ptr Word8 -> Char
chr x = Internal.w2c $ Internal.inlinePerformIO $ peek x

inc :: Ptr Word8 -> Ptr Word8
inc x = x `plusPtr` 1


findIndex0_1 :: (Char -> Bool) -> ByteString0 -> Int
findIndex0_1 f (BS0 bs) = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> go 


break0_1 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0_1 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! end `minusPtr` start

        go s | c == '\0' || f c = s
             | otherwise = go $ inc s
            where c = chr s

break0_2 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break0_2 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | c == '\0' || f c = a
                     | otherwise = go $ inc s
            where c = chr s

newtype ByteString0 = BS0 BS.ByteString

readFile0 :: FilePath -> IO ByteString0
readFile0 x = do
    src <- BS.readFile x
    return $ BS0 $ src `BS.snoc` '\0'


break00_1 :: (Char -> Bool) -> ByteString0 -> (ByteString, ByteString0)
break00_1 f (BS0 bs) = (BS.unsafeTake i bs, BS0 $ BS.unsafeDrop i bs)
    where
        i = Internal.inlinePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
            let start = castPtr ptr :: Ptr Word8
            let end = go start
            return $! Ptr end `minusPtr` start

        go s@(Ptr a) | f c = a
                     | otherwise = go $ inc s
            where c = chr s


innerLoop_1 file = do
    src <- readFile file
    return $ break test_1 src

innerLoop_2 file = do
    src <- readFile file
    return $ break test_2 src

innerLoop_3 file = do
    src <- BS.readFile file
    return $ BS.break test_2 src

innerLoop_4 file = do
    src <- readFile0 file
    return $ break0_1 test_2 src

innerLoop_5 file = do
    src <- readFile0 file
    return $ break0_2 test_2 src

innerLoop_6 file = do
    src <- readFile0 file
    return $ break0_2 test_3 src

innerLoop_7 file = do
    src <- readFile0 file
    return $ break00_1 test_4 src
