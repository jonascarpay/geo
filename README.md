# geo
[![geo on hackage](https://img.shields.io/hackage/v/geo)](http://hackage.haskell.org/package/geo)
[![geo on Stackage Nightly](https://stackage.org/package/geo/badge/nightly)](https://stackage.org/nightly/package/geo)


WIP Haskell library for geometric/Clifford algebra.

```haskell
ghci> [x,y,z,w] = basis :: [Multivector (Cl 3 0 1) Double]
ghci> x
1.0 e[1]
ghci> i = x * y
ghci> i*i
-1.0
```

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)
