{-# LANGUAGE ScopedTypeVariables #-}

import Safe.Exact
import Control.Monad
import Numeric.Extra
import Data.Maybe
import Data.List
import System.Random


data Net = Mul Double Net
         | Add Net Net
         | Sigmoid Net
         | Relu Net
         | Input Int
         | One
           deriving Show

jiggle :: Net -> IO Net
jiggle (Mul _ x) = Mul <$> randomRIO (-1,1) <*> jiggle x
jiggle (Add x y) = Add <$> jiggle x <*> jiggle y
jiggle (Sigmoid x) = Sigmoid <$> jiggle x
jiggle (Relu x) = Relu <$> jiggle x
jiggle x = return x

data Layer a = Layer [a] a deriving Show

type Weights = [Double]
type Input = [Double]
type Output = Double
type Error = Double

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (negate x))

sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

relu x = max 0 x
relu' x = if x <= 0 then 0 else 1

softplus x = log (1 + exp x)
softplus' x = 1 / (1 + exp (negate x))

eval :: Weights -> Input -> Output
eval ws is = sigmoid $ sum $ zipWithExact (*) ws is

eval2 :: Layer Weights -> Input -> Layer Output
eval2 (Layer ws1 ws2) is = Layer o1 $ eval ws2 o1
    where o1 = map (`eval` is) ws1

adjust :: Weights -> Input -> Output -> Weights
adjust ws is actual = zipWithExact (+) ws adjust
    where output = eval ws is
          err = actual - output
          delta = err * sigmoid' output
          adjust = map (* delta) is

adjust3 :: Net -> Input -> Output -> Net
adjust3 n i actual = up $ actual - o
    where (o, up) = evalN n i

evalN :: Net -> Input -> (Output, Error -> Net)
evalN (Mul c x) is = (c * o, \e -> Mul (c + (e * o)) $ up $ e * c)
    where (o, up) = evalN x is
evalN (Add x1 x2) is = (o1 + o2, \e -> Add (up1 e) (up2 e))
    where (o1, up1) = evalN x1 is
          (o2, up2) = evalN x2 is
evalN (Sigmoid x) is = (r, \e -> Sigmoid $ up $ e * sigmoid' r)
    where (o, up) = evalN x is
          r = sigmoid o
evalN (Relu x) is = (r, \e -> Relu $ up $ e * softplus' r)
    where (o, up) = evalN x is
          r = softplus o
evalN One is = (1, const One)
evalN (Input i) is = (is !! i, const $ Input i)

adjust2 :: Layer Weights -> Input -> Output -> Layer Weights
adjust2 ws@(Layer ws1 ws2) is actual = Layer (zipWithExact (zipWithExact (+)) ws1 layer1_adjustment) (zipWithExact (+) ws2 layer2_adjustment)
    where
        Layer output_from_layer_1 output_from_layer_2 = eval2 ws is
        layer2_error :: Double = actual - output_from_layer_2
        layer2_delta :: Double = layer2_error * sigmoid' output_from_layer_2
        layer2_adjustment :: [Double] = map (* layer2_delta) output_from_layer_1

        layer1_error :: [Double] = map (* layer2_delta) ws2
        layer1_delta :: [Double] = zipWithExact (\e o -> e * sigmoid' o) layer1_error output_from_layer_1
        layer1_adjustment :: [[Double]] = map (\i -> map (* i) is) layer1_delta

adjuster :: (w -> Input -> Output -> w) -> [(Input, Output)] -> w -> w
adjuster f [] ws = ws
adjuster f ((i,o):ios) ws = adjuster f ios $ f ws i o

main1 :: IO ()
main1 = do
    -- fst3
    let examples = [([0,0,1],0),([1,1,1],1),([1,0,1],1),([0,1,1],0)]
    let hidden = [([1,0,0],1)]
    let weights = [0.4, 0.3, 0.6]
    let weights' = adjuster adjust (concat $ replicate 10000 examples) weights
    print weights'
    forM_ (examples ++ hidden) $ \i -> print (i, showDP 5 $ eval weights' $ fst i)

main1' :: IO ()
main1' = do
    -- fst3
    let examples = [([0,0,1],0),([1,1,1],1),([1,0,1],1),([0,1,1],0)]
    let hidden = [([1,0,0],1)]
    let weights = Sigmoid $ Mul 0.4 (Input 0) `Add` Mul 0.3 (Input 1) `Add` Mul 0.6 (Input 2)
    let weights' = adjuster adjust3 (concat $ replicate 10000 examples) weights
    print weights'
    forM_ (examples ++ hidden) $ \i -> print (i, showDP 5 $ fst $ evalN weights' $ fst i)

main2 :: IO ()
main2 = do
    -- fst3 `xor` snd3
    let examples = [([0,0,1],0),([0,1,1],1),([1,0,1],1),([0,1,0],1),([1,0,0],1),([1,1,1],0),([0,0,0],0)]
    let hidden = [([1,1,0],0)]
    let weights = Layer [[0.5,-0.5,0.4],[-1,0.9,-0.2],[-0.9,0.3,0.5],[0.5,0,0.6]] [-0.5,-0.2,0.6,-0.7]
    let weights' = adjuster adjust2 (concat $ replicate 60000 examples) weights
    print weights'
    forM_ (examples ++ hidden) $ \i -> print (i, let Layer _ r = eval2 weights' $ fst i in showDP 5 r)

main :: IO ()
main = do
    -- fst3 `xor` snd3
    let examples = [([0,0,1],0),([0,1,1],1),([1,0,1],1),([0,1,0],1),([1,0,0],1),([1,1,1],0),([0,0,0],0)]
    let hidden = [([1,1,0],0)]
    let inputs = map Input [0..2]
    let layer ws is = Sigmoid $ foldl1 Add $ zipWithExact Mul ws is
    let ilayer ws = layer ws $ map Input [0..2]
    let weights = layer [-0.5,-0.2,0.6,-0.7] $ map ilayer [[0.5,-0.5,0.4],[-1,0.9,-0.2],[-0.9,0.3,0.5],[0.5,0,0.6]]
    let weights' = adjuster adjust3 (concat $ replicate 60000 examples) weights
    print weights'
    forM_ (examples ++ hidden) $ \i -> print (i, showDP 5 $ fst $ evalN weights' $ fst i)
