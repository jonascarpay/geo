{-# LANGUAGE OverloadedStrings #-}

module Main where

import Multivector
import Signature

main :: IO ()
main =
  let [px, py, pz, pw] = basis :: [SymbolicMV PGA3D String]
      [vx, vy, vz] = basis :: [SymbolicMV VGA3D String]
      va = "va_x " * vx + "va_y" * vy + "va_z" * vz
      vb = "b1" * vx + "b2" * vy + "b3" * vz
      bva = "bva_yz " * vy * vz + "bva_xz " * vx * vz + "bva_xy " * vx * vy
   in pretty $ va * bva
