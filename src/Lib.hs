{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

data Nat = Zero | Succ Nat

{- ORMOLU_DISABLE -}
type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

type Euclid1d = Extend Positive Empty
type Euclid2d = Extend Positive Euclid1d
type Euclid3d = Extend Positive Euclid2d

type PGA1D = Extend Positive (Extend Null Empty)
type PGA2D = Extend Positive PGA1D
type PGA3D = Extend Positive PGA2D
{- ORMOLU_ENABLE -}

type family Clifford pos zero neg :: Signature where
  Clifford Zero zero neg = Clifford1 zero neg
  Clifford (Succ pos) zero neg = Extend Positive (Clifford1 zero neg)

type family Clifford1 zero neg :: Signature where
  Clifford1 Zero neg = Clifford2 neg
  Clifford1 (Succ zero) neg = Extend Null (Clifford1 zero neg)

type family Clifford2 neg :: Signature where
  Clifford2 Zero = Empty
  Clifford2 (Succ neg) = Extend Negative (Clifford2 neg)

-- C(n+1, k) = C(n, k-1) + C(n, k)

-- C(s n, s k) = C(n, k) + C(n, s k)

{-
data Pseudoscalar (grade :: Nat) (sig :: Signature) where
  P0 :: Double -> Pseudoscalar Zero Empty
  PS :: Pseudoscalar k sig -> Pseudoscalar (Succ k) (Extend e sig)

data KVector (grade :: Nat) (sig :: Signature) where
  KV :: KVector k sig -> KVector (Succ k) sig -> KVector (Succ k) (Extend e sig)
  K0 :: Double -> KVector Zero e
  KNil :: KVector (Succ k) Empty

-- KVoid :: KVector (Succ k) Empty

scalar2d :: Double -> KVector Zero Euclid2d
scalar2d = K0

vector2d :: Double -> Double -> KVector One Euclid2d
vector2d x y = KV (K0 x) (KV (K0 y) KNil)

bivector2d :: Double -> KVector Two Euclid2d
bivector2d x = KV (KV (K0 x) KNil) (KV KNil KNil)

-- e1, e2 :: KVector One Euclid2d
-- e1 = KSkip (KDim KEmpty)
-- e2 = KDim (KSkip KEmpty)

-- e12 :: KVector Two Euclid2d
-- e12 = KSkip _

data KVector k sig where
  KEmpty :: KVector Zero Empty
  KDim :: KVector k sig -> KVector (Succ k) (Extend m sig)
  KSkip :: KVector k sig -> KVector k (Extend m sig)

k0 :: KVector Zero Euclid2d
k0 = KSkip (KSkip KEmpty)

e1, e2 :: KVector One Euclid2d
e1 = KSkip (KDim KEmpty)
e2 = KDim (KSkip KEmpty)

e12 :: KVector Two Euclid2d
e12 = KSkip _
--
 -}
