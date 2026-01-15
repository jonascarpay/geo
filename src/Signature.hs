{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Signature
  ( Metric (..),
    Cl,
    KnownMetric (..),
    Signature (..),
    -- Common algebras
    VGA2D,
    VGA3D,
    PGA2D,
    PGA3D,
    CGA2D,
    CGA3D,
    STA,
  )
where

import GHC.TypeNats (Natural, type (-))

data Metric = Positive | Null | Negative

class KnownMetric (s :: Metric) where
  metric :: p s -> Metric

instance KnownMetric Positive where metric _ = Positive

instance KnownMetric Null where metric _ = Null

instance KnownMetric Negative where metric _ = Negative

data Signature = Extend Metric Signature | Empty

-- Cl(p, q, r): p positive (e²=+1), q negative (e²=-1), r null (e²=0)
-- Built so that e[1..p] are positive, e[p+1..p+q] are negative, e[p+q+1..p+q+r] are null
type family Cl (p :: Natural) (q :: Natural) (r :: Natural) :: Signature where
  Cl p q 0 = ClNeg p q
  Cl p q r = Extend Null (Cl p q (r - 1))

type family ClNeg (p :: Natural) (q :: Natural) :: Signature where
  ClNeg p 0 = ClPos p
  ClNeg p q = Extend Negative (ClNeg p (q - 1))

type family ClPos (p :: Natural) :: Signature where
  ClPos 0 = Empty
  ClPos p = Extend Positive (ClPos (p - 1))

-- Vanilla Geometric Algebra (Euclidean)
type VGA2D = Cl 2 0 0
type VGA3D = Cl 3 0 0

-- Projective Geometric Algebra
type PGA2D = Cl 2 0 1
type PGA3D = Cl 3 0 1

-- Conformal Geometric Algebra
type CGA2D = Cl 3 1 0
type CGA3D = Cl 4 1 0

-- Spacetime Algebra (Minkowski, +---)
type STA = Cl 1 3 0
