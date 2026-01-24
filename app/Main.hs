{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Multivector
import Polynomial (toExpr)
import Signature
import Data.String (IsString(fromString))
import Util (toSubscript)


bivectorBase :: Num a => [a] -> [a]
bivectorBase [] = []
bivectorBase (a : as) = ( (a*) <$> as) <> bivectorBase as

trivectorBase :: Num a => [a] -> [a]
trivectorBase (a:as) = ((a*) <$> bivectorBase as) <> trivectorBase as
trivectorBase [] = []

quadvectorBase :: Num a => [a] -> [a]
quadvectorBase (a:as) = ((a*) <$> trivectorBase as) <> quadvectorBase as
quadvectorBase [] = []

var :: IsString (SymbolicMV sig String) => String -> SymbolicMV sig String
var = fromString

generic :: (IsString (SymbolicMV sig String), Num (SymbolicMV sig String)) => [SymbolicMV sig String] -> [String] -> SymbolicMV sig String
generic basis syms = sum ((\(b,v) -> var v * b) <$> zip basis syms)

subscripted :: String -> [String]
subscripted c = [c <> fmap toSubscript (show i) | i <- [(1::Int)..]]

subscripted0 :: String -> [String]
subscripted0 c = [c <> fmap toSubscript (show i) | i <- [(0::Int)..]]

main :: IO ()
main = do
  let [x,y,z,w] = basis :: [SymbolicMV PGA3D String]

      -- pointbasis = [y*z*w, x*z*w, x*y*w, x*y*z]
      pointbasis = [y*z*w, x*z*w, x*y*w]
      pointvars  = ["yzw", "xzw", "xyz", "xyz"]

      linebasis = [x*y, x*z, y*z, x*w, y*w, z*w]
      linevars  = ["xy", "xz", "yz", "xw", "yw", "zw"]

      motorbasis = [1, x*y, x* z, y* z, x*w, y*w, z*w, x*y*z*w]
      motorvars = ["s", "xy", "xz", "yz", "xw", "yw", "zw", "xyzw"]

      -- [xy,xz,xw,yz,yw,zw] = pga2

      -- p1 = generic pga1 (subscripted "p1")
      -- p2 = generic pga1 (subscripted "p2")

      -- l = generic pga2 (subscripted "l")
      -- lr = generic [xy,xz,yz] (subscripted "lr")
      -- l1 = generic pga2 (subscripted "l1")
      -- l2 = generic pga2 (subscripted "l2")

      -- t1 = generic pga3 (subscripted "t1")
      -- t2 = generic pga3 (subscripted "t2")

      m = generic motorbasis (("m." <>) <$> motorvars)
      l = generic linebasis (("l." <>) <$> linevars)
      t = generic pointbasis (("t." <>) <$> pointvars)
      -- m2 = generic motorbasis (("rhs." <>) <$> motorvars)

  -- print $ mExpr $ normSquared p1
  print . mvExpr $  m * t * rev m
