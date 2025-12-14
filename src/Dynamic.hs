{-# LANGUAGE RankNTypes #-}

module Dynamic where

import Data.Vector (Vector)

newtype Vecvec a = Vecvec (Vector a)

data Metric = Positive | Null | Negative

mul :: Signature -> Vecvec a -> Vecvec a -> Vecvec a
mul sig a b = go sig
  where
    go = undefined

type Signature = [Metric]

class Basis a

class (Applicative f) => Multivector f where
  (.+) :: f a -> f a -> f a
  scalar :: a -> f a

runGeo :: Signature -> (forall v. (Multivector v) => [v a] -> r) -> r
runGeo _ _ = undefined
