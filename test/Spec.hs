{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad.Trans.RWS.CPS
import Data.Bool (bool)
import Data.Proxy
import Multivector
import qualified Polynomial as Poly
import Signature
import Test.Hspec

type GenMV (sig :: Signature) = RWS (Proxy sig) () Int

fresh :: GenMV sig (Poly.Poly Int Int)
fresh = state $ \n -> (Poly.var n, succ n)

-- rename to scalar
freshMV :: (WMetric sig) => GenMV sig (SymbolicMV sig Int)
freshMV = scalarMV <$> fresh

symbolicBasis :: (WMetric sig) => GenMV sig [SymbolicMV sig Int]
symbolicBasis = pure $ (fmap . fmap) Poly.scalar basis

mk1 :: (WMetric sig) => GenMV sig (SymbolicMV sig Int)
mk1 = traverse (\() -> fresh) nullMV

mk2 :: (WMetric sig) => GenMV sig (SymbolicMV sig Int, SymbolicMV sig Int)
mk2 = liftA2 (,) mk1 mk1

mk3 :: (WMetric sig) => GenMV sig (SymbolicMV sig Int, SymbolicMV sig Int, SymbolicMV sig Int)
mk3 = liftA3 (,,) mk1 mk1 mk1

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : as) = ((,) a <$> as) <> pairs as

propG :: (Example a) => String -> (forall sig. (Num (SymbolicMV sig Int), WMetric sig) => GenMV sig a) -> SpecWith (Arg a)
propG str p = describe str $ do
  it "vga0d" $ fst3 $ runRWS p (Proxy @VGA0D) 0
  it "vga1d" $ fst3 $ runRWS p (Proxy @VGA1D) 0
  it "vga2d" $ fst3 $ runRWS p (Proxy @VGA2D) 0
  it "vga3d" $ fst3 $ runRWS p (Proxy @VGA3D) 0
  it "pga2d" $ fst3 $ runRWS p (Proxy @PGA2D) 0
  it "pga3d" $ fst3 $ runRWS p (Proxy @PGA3D) 0
  it "cga2d" $ fst3 $ runRWS p (Proxy @CGA2D) 0
  it "cga3d" $ fst3 $ runRWS p (Proxy @CGA3D) 0
  it "sta3d" $ fst3 $ runRWS p (Proxy @STA) 0
  where
    fst3 (a, _, _) = a

with :: (Functor f) => f a -> (a -> b) -> f b
with = flip fmap

-- (===) :: SymbolicMV sig Int -> SymbolicMV sig Int -> Bool
-- a === b = if a == b then True else False

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    propG "commutativity" $ with mk2 $ \(a, b) -> a + b == b + a
    propG "associativity" $ with mk3 $ \(a, b, c) -> (a + b) + c == a + (b + c)
    propG "identity" $ with mk1 $ \a -> a + 0 == a
    propG "inverse" $ with mk1 $ \a -> (a - a) == 0

  describe "geometric product" $ do
    propG "associativity" $ with mk3 $ \(a, b, c) -> (a * b) * c == a * (b * c)
    propG "left distributivity" $ with mk3 $ \(a, b, c) -> a * (b + c) == a * b + a * c
    propG "right distributivity" $ with mk3 $ \(a, b, c) -> (a + b) * c == a * c + b * c
    propG "left identity" $ with mk1 $ \a -> 1 * a == a
    propG "right identity" $ with mk1 $ \a -> (a * 1) == a
    propG "scalar multiplication" $ do
      s <- freshMV
      (a, b) <- mk2
      pure . all (uncurry (==)) . pairs $ [(s * a) * b, s * (a * b), a * (s * b)]
    propG "basis vector squares" $ with symbolicBasis $ all (\a -> maxGrade (a * a) <= Just 0)
    propG "basis vector squares" $ with symbolicBasis $ all (\a -> (a * a) `elem` [-1, 0, 1])
    propG "anticommutativity" $ with symbolicBasis $ all (\(a, b) -> a * b == -(b * a)) . pairs

  describe "grade involution" $ do
    propG "involution" $ with mk1 $ \a -> hat (hat a) == a
    propG "homomorphism" $ with mk2 $ \(a, b) -> hat (a * b) == hat a * hat b
    propG "linearity" $ with mk2 $ \(a, b) -> hat (a + b) == hat a + hat b
    propG "grade action" $ do
      a <- mk1
      n <- asks dimSig
      pure $ all (\k -> hat (grade k a) == bool (-1) 1 (even k) * grade k a) [0 .. n]

  describe "reversion" $ do
    propG "involution" $ with mk1 $ \a -> rev (rev a) == a
    propG "anti-homomorphism" $ with mk2 $ \(a, b) -> rev (a * b) == rev b * rev a
    propG "linearity" $ with mk2 $ \(a, b) -> rev (a + b) == rev a + rev b
    propG "grade action" $ do
      a <- mk1
      n <- asks dimSig
      -- rev(A_k) = (-1)^(k(k-1)/2) * A_k
      -- sign pattern: +1, +1, -1, -1, +1, +1, -1, -1, ...
      let revSign k = bool (-1) 1 (even (k `div` 2))
      pure $ all (\k -> rev (grade k a) == revSign k * grade k a) [0 .. n]

  describe "grade projection" $ do
    propG "idempotent" $ do
      a <- mk1
      n <- asks dimSig
      pure $ all (\k -> grade k (grade k a) == grade k a) [0 .. n]
    propG "orthogonality" $ do
      a <- mk1
      n <- asks dimSig
      pure $ all (\(j, k) -> grade j (grade k a) == 0) [(j, k) | j <- [0 .. n], k <- [0 .. n], j /= k]
    propG "completeness" $ do
      a <- mk1
      n <- asks dimSig
      pure $ sum [grade k a | k <- [0 .. n]] == a
    propG "linearity" $ do
      (a, b) <- mk2
      n <- asks dimSig
      pure $ all (\k -> grade k (a + b) == grade k a + grade k b) [0 .. n]
    propG "scalar extraction" $ with freshMV $ \s -> grade 0 s == s

  -- -----------------------------------------------------------------------------
  -- Dual (dual)
  -- -----------------------------------------------------------------------------
  -- 1. Double dual: dual(dual(a)) = +/-a (sign depends on dimension and signature)
  -- 2. Grade swap: dual maps grade-k to grade-(n-k)
  -- 3. Linearity: dual(a + b) = dual(a) + dual(b)

  describe "dual" $ do
    propG "involution" $ with mk1 $ \a -> dual (dual a) == a
  -- todo

  describe "exterior product" $ do
    propG "antisymmetry" $ with mk2 $ \(a, b) ->
      let a' = grade 1 a
          b' = grade 1 b
       in wedge a' b' == -wedge b' a'
    propG "associativity" $ with mk3 $ \(a, b, c) -> wedge (wedge a b) c == wedge a (wedge b c)
    propG "nilpotency" $ with mk1 $ \a -> let a' = grade 1 a in wedge a' a' == 0
    propG "left distributivity" $ with mk3 $ \(a, b, c) -> wedge a (b + c) == wedge a b + wedge a c
    propG "right distributivity" $ with mk3 $ \(a, b, c) -> wedge (a + b) c == wedge a c + wedge b c
    propG "scalar factor" $ do
      s <- freshMV
      (a, b) <- mk2
      pure . all (uncurry (==)) $
        pairs
          [ wedge (s * a) b,
            s * wedge a b,
            wedge a (s * b)
          ]
    propG "geometric product" $ with mk2 $ \(a, b) -> let a' = grade 1 a; b' = grade 1 b in 2 * wedge a' b' == a' * b' - b' * a'

-- -----------------------------------------------------------------------------
-- Inner Product (inner) [TODO]
-- -----------------------------------------------------------------------------
-- 1. Symmetry for vectors: u `inner` v = v `inner` u
-- 2. Grade lowering: grade(A_r `inner` B_s) = |r - s| (when nonzero)
-- 3. Left distributivity: a `inner` (b + c) = a `inner` b + a `inner` c
-- 4. Right distributivity: (a + b) `inner` c = a `inner` c + b `inner` c
-- 5. Scalar extraction: u `inner` v = grade 0 (u * v) for vectors
-- 6. Relation to geometric product: u `inner` v = (1/2)(uv + vu) for vectors
-- 7. Orthogonality: ei `inner` ej = 0 for i /= j

-- -----------------------------------------------------------------------------
-- Combined Properties
-- -----------------------------------------------------------------------------
-- 1. Geometric product decomposition: a * b = a `inner` b + a `wedge` b (for vectors)
-- 2. Scalar product: grade 0 (a * rev(b)) is symmetric
-- 3. Norm squared: v * rev(v) = |v|^2 for vectors (in Euclidean signature)
