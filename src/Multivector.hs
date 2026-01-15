{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Multivector where

import Data.Bool (bool)
import Data.Data (Proxy (..))
import Data.String (IsString)
import GHC.Exts (IsString (..))
import Polynomial (Poly (Poly), var)
import qualified Polynomial as Poly
import Signature
import qualified Util

data Multivector (sig :: Signature) a where
  -- A grade-0 multivector
  Scalar :: a -> Multivector 'Empty a
  -- A grade-(n+1) multivector of the form (a*e_(n+1) + b), where a and b are grade-n multivectors
  Dim :: (KnownMetric metric) => Multivector l a -> Multivector l a -> Multivector (Extend metric l) a

deriving instance Functor (Multivector sig)

deriving instance Foldable (Multivector sig)

deriving instance Traversable (Multivector sig)

liftMV2 :: forall sig a b c. (a -> b -> c) -> Multivector sig a -> Multivector sig b -> Multivector sig c
liftMV2 f = go
  where
    go :: forall sig. Multivector sig a -> Multivector sig b -> Multivector sig c
    go (Scalar a) (Scalar b) = Scalar (f a b)
    go (Dim a b) (Dim c d) = Dim (go a c) (go b d)

plus :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
plus = liftMV2 (+)

instance (Num a) => Num (Multivector 'Empty a) where
  Scalar a + Scalar x = Scalar (a + x)
  Scalar a - Scalar x = Scalar (a - x)
  Scalar a * Scalar x = Scalar (a * x)
  negate (Scalar x) = Scalar (negate x)
  fromInteger x = Scalar (fromInteger x)
  abs = error "not implemented"
  signum = error "not implemented"

instance (Num a, KnownMetric m, Num (Multivector sig a)) => Num (Multivector (Extend m sig) a) where
  Dim a b + Dim x y = Dim (a + x) (b + y)

  Dim a b - Dim x y = Dim (a - x) (b - y)

  Dim a b * Dim c d =
    let m = fromInteger (metricD (metric $ Proxy @m))
     in Dim (a * hat d + b * c) (m * a * hat c + b * d)

  negate (Dim a b) = Dim (negate a) (negate b)
  fromInteger x = Dim 0 (fromInteger x)

  abs = error "not implemented"
  signum = error "not implemented"

metricD :: Metric -> Integer
metricD Positive = 1
metricD Null = 0
metricD Negative = -1

class WMetric sig where
  dimSig :: proxy sig -> Int
  basisRev :: (Num a) => [Multivector sig a]
  zero :: (Num a) => Multivector sig a
  one :: (Num a) => Multivector sig a
  scalarMV :: (Num a) => a -> Multivector sig a

instance WMetric Empty where
  dimSig _ = 0
  basisRev = []
  zero = Scalar 0
  one = Scalar 1
  scalarMV = Scalar

instance (KnownMetric m, WMetric sig) => WMetric (Extend m sig) where
  dimSig _ = 1 + dimSig (Proxy @sig)
  basisRev = Dim one zero : (Dim zero <$> basisRev)
  zero = Dim zero zero
  one = Dim zero one
  scalarMV a = Dim zero (scalarMV a)

dimMV :: forall sig a. (WMetric sig) => Multivector sig a -> Int
dimMV _ = dimSig (Proxy @sig)

basis :: (Num a, WMetric sig) => [Multivector sig a]
basis = reverse basisRev

instance (IsString coeff) => IsString (Multivector 'Empty coeff) where fromString a = Scalar (fromString a)

instance (Num coeff, IsString (Multivector sig coeff), KnownMetric m, WMetric sig) => IsString (Multivector (Extend m sig) coeff) where
  fromString a = Dim zero (fromString a)

toPoly :: forall sig a. (Num a, Ord a, WMetric sig) => Multivector sig a -> Poly Int a
toPoly mv = go (dimMV mv) mv
  where
    go :: forall sig. Int -> Multivector sig a -> Poly Int a
    go _ (Scalar a) = Poly.scalar a
    go dim (Dim a b) = Poly.var dim * go (dim - 1) a + go (dim - 1) b

instance (Eq a, Show a, Num a, Ord a, WMetric sig) => Show (Multivector sig a) where
  show a = Poly.showPoly showVar show $ toPoly a

showVar :: Int -> Int -> String
showVar n 1 = "e" <> fmap Util.toSubscript (show n)
showVar _ _ = error "impossible"

showAlgebra :: (WMetric sig) => Multivector sig (Poly String Int) -> String
showAlgebra = Poly.showPoly showVar Poly.showTerm . toPoly

pretty :: (WMetric sig) => Multivector sig (Poly String Int) -> IO ()
pretty mv = putStrLn (showAlgebra mv)

mapGrade :: forall sig a b. (Int -> a -> b) -> Multivector sig a -> Multivector sig b
mapGrade f = go 0
  where
    go :: forall sig. Int -> Multivector sig a -> Multivector sig b
    go dim (Scalar a) = Scalar (f dim a)
    go dim (Dim a b) = Dim (go (dim + 1) a) (go dim b)

-- | Grade projection: extract the grade-k component of a multivector
grade :: (Num a) => Int -> Multivector sig a -> Multivector sig a
grade k = mapGrade $ \dim a -> if dim == k then a else 0

hat :: (Num a) => Multivector sig a -> Multivector sig a
hat = mapGrade (bool id negate . even)

-- | Reversion (†): reverses the order of basis vectors in each blade
-- For a grade-k blade, multiplies by (-1)^(k(k-1)/2)
rev :: forall sig a. (Num a) => Multivector sig a -> Multivector sig a
rev = mapGrade (bool id negate . even . (`div` 2))

-- | Hodge dual
dual :: (Fractional a) => Multivector sig a -> Multivector sig a
dual (Scalar a) = Scalar a
dual (Dim a b) = Dim (dual b) (hat $ dual a)

-- | Inner (dot) product
inner :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
inner = error "TODO: inner product"

-- | Outer (wedge) product
wedge :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
wedge (Scalar a) (Scalar b) = Scalar (a * b)
wedge (Dim a b) (Dim c d) = Dim (plus (wedge a d) (hat (wedge b c))) (wedge b d)

-- | Infix wedge product
(∧) :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
(∧) = wedge

-- | Infix inner product
(·) :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
(·) = inner
