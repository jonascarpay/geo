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

main :: IO ()
main = do
  let pga0 = 1 :: SymbolicMV PGA3D String
      pga1 = basis :: [SymbolicMV PGA3D String]
      pga2 = bivectorBase pga1
      pga3 = trivectorBase pga1
      pga4 = quadvectorBase pga1
      vga = basis :: [SymbolicMV VGA3D String]

      p1 = generic pga1 (subscripted "p1")
      p2 = generic pga1 (subscripted "p2")

      l1 = generic pga2 (subscripted "l1")
      l2 = generic pga2 (subscripted "l2")

      t1 = generic pga3 (subscripted "t1")
      t2 = generic pga3 (subscripted "t2")

      m = generic ((pga0 : pga2) <> pga4) (subscripted "m")

  -- print $ mExpr $ normSquared p1
  -- print . mvExpr $ p1
  -- print . mvExpr $ l1
  print . mvExpr $ rev m * l1 * m
