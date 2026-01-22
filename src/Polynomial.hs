{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Polynomial
  ( Poly (..),
    var,
    scalar,
    showPoly,
    showTerm,
  )
where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import qualified Util

-- | A polynomial: maps monomials (variable -> exponent) to coefficients.
-- A univariate polynomial, i.e. with powers of one variable, would be Poly () a.
newtype Poly var coeff = Poly (Map (Map var Int) coeff)
  deriving stock (Eq, Ord)
  deriving newtype (Show)

poly :: (Eq coeff, Num coeff) => Map (Map var Int) coeff -> Poly var coeff
poly = Poly . Map.filter (/= 0)

instance (Num coeff, Eq coeff, Ord var) => Num (Poly var coeff) where
  Poly a + Poly b = poly $ Map.unionWith (+) a b

  Poly a * Poly b =
    poly $
      Map.fromListWith
        (+)
        [ (Map.unionWith (+) m1 m2, c1 * c2)
          | (m1, c1) <- Map.toList a,
            (m2, c2) <- Map.toList b
        ]

  negate (Poly a) = Poly $ fmap negate a
  fromInteger 0 = Poly mempty
  fromInteger n = scalar (fromInteger n)
  abs = error "not implemented"
  signum = error "not implemented"

instance (Ord var, Eq coeff, Num coeff) => Semigroup (Poly var coeff) where
  (<>) = (+)

instance (Ord var, Eq coeff, Num coeff) => Monoid (Poly var coeff) where
  mempty = Poly mempty

showPoly :: (Ord coeff, Num coeff) => (var -> Int -> String) -> (coeff -> String) -> Poly var coeff -> String
showPoly fVar fCoeff (Poly m) = case Map.toAscList m of
  [] -> "0"
  h : t -> showHead h <> concatMap showTail t
  where
    showHead t = case fTerm t of
      (t', True) -> t'
      (t', False) -> "-" <> t'
    showTail t = case fTerm t of
      (t', True) -> " + " <> t'
      (t', False) -> " - " <> t'
    fVars = concatMap (uncurry fVar)
    fTerm (vars', coeff') = case (Map.toAscList vars', coeff') of
      (_, 0) -> error "impossible"
      ([], 1) -> ("1", True)
      ([], -1) -> ("1", False)
      (vars, 1) -> (fVars vars, True)
      (vars, -1) -> (fVars vars, False)
      (vars, coeff) -> (fCoeff coeff <> fVars vars, coeff > 0)

showTerm :: (Ord a, Num a, Show a) => Poly String a -> String
showTerm = showPoly showVar show
  where
    showVar v 1 = v
    showVar v e = v <> fmap Util.toSuperscript (show e)

instance (Num coeff) => IsString (Poly String coeff) where
  fromString = var

scalar :: coeff -> Poly a coeff
scalar a = Poly (Map.singleton Map.empty a)

-- Helper to create a variable term
var :: (Num coeff) => var -> Poly var coeff
var x = Poly $ Map.singleton (Map.singleton x 1) 1
