{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Data.String (IsString (fromString))
import Multivector
import Polynomial (toExpr)
import Signature
import Util (toSubscript)

bivectorBase :: (Num a) => [a] -> [a]
bivectorBase [] = []
bivectorBase (a : as) = ((a *) <$> as) <> bivectorBase as

trivectorBase :: (Num a) => [a] -> [a]
trivectorBase (a : as) = ((a *) <$> bivectorBase as) <> trivectorBase as
trivectorBase [] = []

quadvectorBase :: (Num a) => [a] -> [a]
quadvectorBase (a : as) = ((a *) <$> trivectorBase as) <> quadvectorBase as
quadvectorBase [] = []

var :: (IsString (SymbolicMV sig String)) => String -> SymbolicMV sig String
var = fromString

generic :: (IsString (SymbolicMV sig String), Num (SymbolicMV sig String)) => [SymbolicMV sig String] -> [String] -> SymbolicMV sig String
generic basis syms = sum ((\(b, v) -> var v * b) <$> zip basis syms)

subscripted :: String -> [String]
subscripted c = [c <> fmap toSubscript (show i) | i <- [(1 :: Int) ..]]

subscripted0 :: String -> [String]
subscripted0 c = [c <> fmap toSubscript (show i) | i <- [(0 :: Int) ..]]

main :: IO ()
main = do
  let [x, y, z, w] = basis :: [SymbolicMV PGA3D String]

      -- pointbasis = [y*z*w, x*z*w, x*y*w, x*y*z]
      pointbasis = [w * y * z, w * x * z, x * y * w, x * y * z]
      pointvars = ["yzw", "xzw", "xyz", "xyz"]

      linebasis = [x * y, x * z, y * z, w * x, w * y, w * z]
      linevars = ["xy", "xz", "yz", "wx", "wy", "wz"]

      motorbasis = [1, y * z, z * x, x * y, w * x, w * y, w * z, w * x * y * z]
      motorvars = ["s", "yz", "zx", "xy", "wx", "wy", "wz", "wxyz"]

      m = generic motorbasis (("m." <>) <$> motorvars)
      l = generic linebasis (("l." <>) <$> linevars)
      t = generic pointbasis (("t." <>) <$> pointvars)
      u = generic (take 3 linebasis) (subscripted "b")

      linebasis' = [y * z, z * x, x * y, w * x, w * y, w * z]
      b = generic linebasis' (subscripted "b")

      m1 = generic motorbasis (subscripted0 "b")
      m2 = generic motorbasis (subscripted0 "a")

  -- print $ mExpr $ normSquared p1
  print . mvExpr $ m2 * m1

-- print . mvExpr $ b * b
-- mapM_ (print . mvExpr) pointbasis

-- print . mvExpr $
