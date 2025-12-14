module Weird where

data Pos a = Pos
  { variant :: a,
    invariant :: a
  }
