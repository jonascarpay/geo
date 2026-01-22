{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Multivector
import Polynomial (toExpr)
import Signature

main :: IO ()
main = do
  let [px, py, pz, pw] = basis :: [SymbolicMV PGA3D String]
      [vx, vy, vz] = basis :: [SymbolicMV VGA3D String]
      va = "va1" * vx + "va2" * vy + "va3" * vz
      vb = "vb1" * vx + "vb2" * vy + "vb3" * vz
      ba = "ba1" * vy * vz + "ba2" * vx * vz + "ba3" * vx * vy
      bb = "bb1" * vy * vz + "bb2" * vx * vz + "bb3" * vx * vy
  -- print $ toExpr _ _ $ normSquared va
  print $ mExpr $ normSquared ba
