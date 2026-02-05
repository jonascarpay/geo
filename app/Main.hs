{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import Multivector
import Polynomial (showPoly)
import Signature
import Util (toSubscript)

var :: (IsString (SymbolicMV sig String)) => String -> SymbolicMV sig String
var = fromString

generic :: (IsString (SymbolicMV sig String), Num (SymbolicMV sig String)) => [SymbolicMV sig String] -> [String] -> SymbolicMV sig String
generic base syms = sum ((\(b, v) -> var v * b) <$> zip base syms)

subscripted :: String -> [String]
subscripted c = [c <> fmap toSubscript (show i) | i <- [(1 :: Int) ..]]

subscripted0 :: String -> [String]
subscripted0 c = [c <> fmap toSubscript (show i) | i <- [(0 :: Int) ..]]

prefixed :: String -> [String] -> [String]
prefixed prefix = fmap (prefix <>)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : as) = ((,) a <$> as) <> pairs as

grades :: (Eq a, Num a) => Multivector PGA3D a -> [Multivector PGA3D a]
grades mv = filter (/= 0) [mapGrade (\g a -> if d == g then a else 0) mv | d <- [0 .. 4]]

format :: SymbolicMV PGA3D String -> String
format = unlines . fmap (\(dims, term) -> showDims dims <> ":\t" <> showPoly const show term) . filter ((/= 0) . snd) . decompose
  where
    showDims :: [Int] -> String
    showDims [] = "s"
    showDims ds = fmap (\i -> "xyzw" !! (i - 1)) ds

main :: IO ()
main = do
  let [x, y, z, w] = basis :: [SymbolicMV PGA3D String]

      planebasis = [w, x, y, z]
      planevars = ["w", "x", "y", "z"]

      pointbasis = [w * z * y, w * x * z, w * y * x, x * y * z]
      pointvars = ["wzy", "wxz", "wyx", "xyz"]

      bivectorbasis = [y * z, z * x, x * y]
      bivectorvars = ["yz", "zx", "xy"]

      linebasis = [y * z, z * x, x * y, w * x, w * y, w * z]
      linevars = ["yz", "zx", "xy", "wx", "wy", "wz"]

      rotorbasis = [1, y * z, z * x, x * y]
      rotorvars = ["s", "yz", "zx", "xy"]

      motorbasis = [1, y * z, z * x, x * y, w * x, w * y, w * z, w * x * y * z]
      motorvars = ["s", "yz", "zx", "xy", "wx", "wy", "wz", "wxyz"]

      i = w * x * y * z

      pl1 = generic planebasis (subscripted "pl1")
      pl2 = generic planebasis (subscripted "pl2")

      l1 = generic linebasis (subscripted "l1")
      l2 = generic linebasis (subscripted "l2")

      pt1 = generic pointbasis (subscripted "pt1")
      pt2 = generic pointbasis (subscripted "pt2")

      primitives p =
        [ ("plane", generic planebasis (prefixed p planevars)),
          ("line", generic linebasis (prefixed p linevars)),
          ("point", generic pointbasis (prefixed p pointvars))
        ]

  forM_ (liftA2 (,) (primitives "a.") (primitives "b.")) $ \((na, a), (nb, b)) -> do
    putStrLn ""
    putStrLn $ "=== " <> na <> " x " <> nb <> " ==="
    putStrLn ""
    putStrLn . format $ (a * b)
    putStrLn ""
    print . mvExpr $ a * b

-- putStrLn ""
-- putStrLn $ na <> " * " <> nb <> ":"
-- mapM_ putStrLn $ intersperse "+" $ fmap (show . mvExpr) (grades $ a * b)
-- putStrLn ""
-- putStrLn $ na <> " \\/ " <> nb <> ":"
-- print . mvExpr $ regressive a b
