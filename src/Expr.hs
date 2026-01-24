{-# LANGUAGE OverloadedStrings #-}
module Expr where

import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Util (toSuperscript)

data Expr
  = Sum Expr Expr
  | Product Expr Expr
  | Neg Expr
  | Pow Expr Int
  | Lit Word
  | Var String
  deriving (Eq)

instance Show Expr where
  show e = renderString $ layoutPretty (LayoutOptions (AvailablePerLine 200 1.0)) (pretty e)

instance Pretty Expr where
  pretty = go 0 . simplify
    where
      go _ (Lit n) = pretty n
      go _ (Var s) = pretty s
      go p s@(Sum _ _) =
        let terms = flattenSum s
            formatTerm i (isNeg, e) =
              let op = if i == 0
                       then if isNeg then "-" else mempty
                       else if isNeg then "- " else "+ "
              in op <> go 0 e
            docs = zipWith formatTerm [0..] terms
        in parens' (p > 0) $ group $ align $ sep docs
      go p (Product a b) = parens' (p > 1) $ go 1 a <+> go 1 b
      go p (Neg e) = parens' (p > 2) $ "-" <> go 2 e
      go p (Pow e n) = go 4 e <> pretty (fmap toSuperscript (show n))

      parens' True d = parens d
      parens' False d = d

      flattenSum (Sum a b) = flattenSum a ++ flattenSum b
      flattenSum (Neg e) = [(True, e)]
      flattenSum e = [(False, e)]

intLit :: Int -> Expr
intLit n = if n >= 0 then Lit (fromIntegral n) else Neg (Lit (fromIntegral (-n)))

showExpr :: Expr -> String
showExpr exp = go 0 $ simplify exp
  where
    -- Precedence levels: 0 = Sum, 1 = Product, 2 = Neg, 3 = Pow, 4 = Atom
    go :: Int -> Expr -> String
    go _ (Lit n) = show n
    go _ (Var s) = s
    go p (Sum (Neg a) b) = parens (p > 0) $ go 0 b ++ " - " ++ go 0 a
    go p (Sum a (Neg b)) = parens (p > 0) $ go 0 a ++ " - " ++ go 0 b
    go p (Sum a b) = parens (p > 0) $ go 0 a ++ " + " ++ go 0 b
    go p (Product a b) = parens (p > 1) $ go 1 a ++ " " ++ go 1 b
    go p (Neg e) = parens (p > 2) $ "-" ++ go 2 e
    go p (Pow e n) = go 4 e ++ fmap toSuperscript (show n)

    parens :: Bool -> String -> String
    parens True s = "(" ++ s ++ ")"
    parens False s = s


foldSum :: Expr -> Expr -> Expr
foldSum a (Sum b c) | Just a' <- simplifySum a b = Sum a' c
foldSum a (Sum b c) | Just a' <- simplifySum a c = Sum a' b
foldSum a b = Sum a b

simplifySum :: Expr -> Expr -> Maybe Expr
simplifySum a b | a == b                                       = Just $ Product (Lit 2) a
simplifySum a (Product (Lit n) b) | a == b                     = Just $ Product (Lit (n + 1)) a
simplifySum (Product (Lit n) a) b | a == b                     = Just $ Product (Lit (n + 1)) a
simplifySum (Product (Lit na) a) (Product (Lit nb) b) | a == b = Just $ Product (Lit (na + nb)) a
simplifySum a b = Nothing

simplify :: Expr -> Expr
simplify e0 = let e' = go e0 in if e0 == e' then e0 else simplify e'
  where
    go :: Expr -> Expr
    --
    go (Product (Lit 0) _) = Lit 0
    go (Product _ (Lit 0)) = Lit 0
    go (Product (Lit 1) b) = go b
    go (Product a (Lit 1)) = go a
    go (Product a (Lit b)) = Product (Lit b) (go a)
    go (Product (Neg a) b) = Neg (Product (go a) (go b))
    go (Product a (Neg b)) = Neg (Product (go a) (go b))
    go (Product (Product a b) c) = Product a (Product b c)
    go (Product a b) = Product (go a) (go b)
    --
    go (Sum (Neg a) (Neg b)) = Neg (Sum (go a) (go b))
    go (Sum (Lit 0) b) = go b
    go (Sum a (Lit 0)) = go a
    go (Sum (Lit a) (Lit b)) = Lit (a + b)
    go (Sum (Sum a b) c) = Sum a (Sum b c)
    go (Sum a b) = foldSum (go a) (go b)
    --
    go (Neg (Neg a)) = go a
    go (Neg a) = Neg (go a)
    --
    go (Pow e 1) = go e
    go (Pow _ 0) = Lit 1
    go (Pow e n) = Pow (go e) n
    --
    go (Var v) = Var v
    go (Lit l) = Lit l
