{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Signature
  ( Metric (..),
    Cl,
    KnownMetric (..),
    Signature (..),
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

type family Cl (pos :: Natural) zero neg :: Signature where
  Cl 0 zero neg = Clifford1 zero neg
  Cl pos zero neg = Extend Positive (Cl (pos - 1) zero neg)

type family Clifford1 zero neg :: Signature where
  Clifford1 0 neg = Clifford2 neg
  Clifford1 zero neg = Extend Negative (Clifford1 (zero - 1) neg)

type family Clifford2 neg :: Signature where
  Clifford2 0 = Empty
  Clifford2 neg = Extend Null (Clifford2 (neg - 1))
