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

import Control.Applicative (liftA)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (State, evalState)
import Data.Bool (bool)
import Data.Data (Proxy (..))
import Data.String (IsString)
import Expr
import GHC.Exts (IsString (..))
import Polynomial (Poly (Poly), toExpr, var)
import qualified Polynomial as Poly
import Signature
import qualified Util

data Multivector (sig :: Signature) a where
  -- A grade-0 multivector
  Scalar :: a -> Multivector 'Empty a
  -- A grade-(n+1) multivector of the form (a*e_(n+1) + b), where a and b are grade-n multivectors
  -- TODO can we drop this constraint? what is it for?
  Dim :: (KnownMetric metric) => Multivector l a -> Multivector l a -> Multivector (Extend metric l) a

deriving instance Functor (Multivector sig)

deriving instance Foldable (Multivector sig)

deriving instance Traversable (Multivector sig)

deriving instance (Eq a) => Eq (Multivector sig a)

liftMV2 :: forall sig a b c. (a -> b -> c) -> Multivector sig a -> Multivector sig b -> Multivector sig c
liftMV2 f = go
  where
    go :: forall sig. Multivector sig a -> Multivector sig b -> Multivector sig c
    go (Scalar a) (Scalar b) = Scalar (f a b)
    go (Dim a b) (Dim c d) = Dim (go a c) (go b d)

maxGrade :: (Num a, Eq a) => Multivector sig a -> Maybe Int
maxGrade (Scalar 0) = Nothing
maxGrade (Scalar _) = Just 0
maxGrade (Dim l r) = case ((+ 1) <$> maxGrade l, maxGrade r) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just a
  (Nothing, Just b) -> Just b
  (Just a, Just b) -> Just (max a b)

instance Applicative (Multivector Empty) where
  pure = Scalar
  liftA2 f (Scalar a) (Scalar b) = Scalar (f a b)

instance (KnownMetric m, Applicative (Multivector sig)) => Applicative (Multivector (Extend m sig)) where
  pure a = Dim (pure a) (pure a)
  liftA2 f (Dim a b) (Dim c d) = Dim (liftA2 f a c) (liftA2 f b d)

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
  nullMV :: Multivector sig ()
  dual :: (Num a) => Multivector sig a -> Multivector sig a
  withMetric :: Multivector sig a -> Multivector sig (a, [Metric])

-- TODO test aB = left a B + wedge a B
-- leftContraction :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a

instance WMetric Empty where
  dimSig _ = 0
  basisRev = []
  zero = Scalar 0
  one = Scalar 1
  scalarMV = Scalar
  nullMV = Scalar ()

  -- leftContraction (Scalar a) (Scalar b) = Scalar (a * b)
  dual (Scalar s) = Scalar s

instance (KnownMetric m, WMetric sig) => WMetric (Extend m sig) where
  dimSig _ = 1 + dimSig (Proxy @sig)
  basisRev = Dim one zero : (Dim zero <$> basisRev)
  zero = Dim zero zero
  one = Dim zero one
  scalarMV a = Dim zero (scalarMV a)
  nullMV = Dim nullMV nullMV

  -- leftContraction (Dim a b) (Dim c d) =
  --   let m = fromInteger (metricD (metric $ Proxy @m))
  --    in Dim (leftContraction b c) (plus (leftContraction a ((m *) <$> hat c)) (leftContraction b d))
  dual (Dim a b) =
    let m = fromInteger (metricD (metric $ Proxy @m))
     in Dim (dual b) (hat $ (m *) <$> a)

type SymbolicMV sig sym = Multivector sig (Poly sym Int)

dimMV :: forall sig a. (WMetric sig) => Multivector sig a -> Int
dimMV _ = dimSig (Proxy @sig)

basis :: (Num a, WMetric sig) => [Multivector sig a]
basis = reverse basisRev

instance (IsString coeff) => IsString (Multivector 'Empty coeff) where fromString a = Scalar (fromString a)

instance (Num coeff, IsString (Multivector sig coeff), KnownMetric m, WMetric sig) => IsString (Multivector (Extend m sig) coeff) where
  fromString a = Dim zero (fromString a)

-- exp :: forall sig a. Multivector sig a -> Multivector sig a
-- exp = go 0 0
--   where
--     go :: forall sig. Int -> Int -> Multivector sig a -> Multivector sig a
--     go grade sign d@(Dim l r) =
--       let m = fromInteger $ metricD $ getMetric d
--           sign' = sign * m
--        in Dim (go (grade + 1))

getMetric :: forall metric sig a. (KnownMetric metric) => Multivector (Extend metric sig) a -> Metric
getMetric _ = metric (Proxy @metric)

-- | Convert a multivector to a polynomial.
-- The exponent will necessarily always be 1.
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

mvExpr :: (WMetric sig) => SymbolicMV sig String -> Expr
mvExpr = toExpr (\n -> Var $ "e" <> fmap Util.toSubscript (show n)) mExpr . toPoly

mExpr :: Poly String Int -> Expr
mExpr = toExpr Var intLit

pretty :: (WMetric sig) => SymbolicMV sig String -> IO ()
pretty mv = putStrLn (showExpr $ mvExpr mv)

mapGrade :: forall sig a b. (Int -> a -> b) -> Multivector sig a -> Multivector sig b
mapGrade f = go 0
  where
    go :: forall sig. Int -> Multivector sig a -> Multivector sig b
    go dim (Scalar a) = Scalar (f dim a)
    go dim (Dim a b) = Dim (go (dim + 1) a) (go dim b)

-- | Grade projection: extract the grade-k component of a multivector
grade :: (Num a) => Int -> Multivector sig a -> Multivector sig a
grade k = mapGrade $ \dim a -> if dim == k then a else 0

grade0 :: Multivector sig a -> a
grade0 (Scalar a) = a
grade0 (Dim _ b) = grade0 b

hat :: (Num a) => Multivector sig a -> Multivector sig a
hat = mapGrade (bool negate id . even)

-- | Reversion (†): reverses the order of basis vectors in each blade
-- For a grade-k blade, multiplies by (-1)^(k(k-1)/2)
rev :: forall sig a. (Num a) => Multivector sig a -> Multivector sig a
rev = mapGrade (bool negate id . even . (`div` 2))

-- -- | Hodge dual
-- dual :: (Num a) => Multivector sig a -> Multivector sig a
-- dual (Scalar a) = Scalar a
-- dual (Dim a b) = Dim (dual b) (hat $ dual a)

-- | Inner (dot) product
inner :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
inner = error "TODO: inner product"

-- | Outer (wedge) product
-- "Smallest subspace containing the inputs"
wedge :: (Num a) => Multivector sig a -> Multivector sig a -> Multivector sig a
wedge (Scalar a) (Scalar b) = Scalar (a * b)
wedge (Dim a b) (Dim c d) = Dim (plus (wedge a (hat d)) (wedge b c)) (wedge b d)

normSquared :: (Num a, Num (Multivector sig a)) => Multivector sig a -> a
normSquared a = grade0 $ rev a * a
