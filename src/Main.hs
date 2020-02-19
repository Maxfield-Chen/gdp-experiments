{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE RoleAnnotations     #-}



module Main where

import           Prelude
import           Theory.Lists
import           Theory.Named
import           Theory.Equality
import           Logic.Proof
import           Data.The
import           Data.Refined
import           Data.Coerce
import           Logic.Implicit

  {-
newtype SortedBy comp a = SortedBy a
instance The (SortedBy comp a) a

sortBy :: ((a -> a -> Ordering) ~~ comp) -> [a] -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

mergeBy
  :: Ord a
  => ((a -> a -> Ordering) ~~ comp)
  -> SortedBy comp [a]
  -> SortedBy comp [a]
  -> SortedBy comp [a]
mergeBy comp xs ys = coerce $ foldr
  (\(x, y) ret -> if the comp x y == LT then x : y : ret else y : x : ret)
  []
  (zip (the xs) (the ys))

gdpHead :: Fact (IsCons xs) => ([a] ~~ xs) -> a
gdpHead xs = L.head (the xs)
-}

-- Predicates about the possible shapes of positions
data IsBound n
data IsUnbound n

data BoundedCase n val =
  IsBound_ (Proof (IsBound val)) (n ~~ Valid val) |
  IsUnbound_ (Proof (IsUnbound val)) 

newtype Valid pos = Valid Defn
type role Valid nominal


boundClassify :: (Num n, Ord n) => (n ~~ val) -> BoundedCase n val
boundClassify val
   | the val < 10 && the val > 0 = IsBound_ axiom (defn (the val))
   | otherwise = IsUnbound_ axiom 

pattern IsBound :: Proof (IsBound val) -> (Int ~~ Valid val) -> (Int ~~ val)
pattern IsBound proof val <- (boundClassify -> IsBound_ proof val)

pattern IsUnbound :: Proof (IsUnbound val) -> (Int ~~ val)
pattern IsUnbound proof <- (boundClassify -> IsUnbound_ proof)

data BoundedCase' n val where
  Bound :: Fact (IsBound val) => (n ~~ Valid val) -> BoundedCase' n val
  UnBound :: Fact (IsUnbound val) => BoundedCase' n val

boundClassify' :: forall n val . (n ~~ val) -> BoundedCase' n val
boundClassify' val = note (axiom :: Proof (IsUnbound val)) UnBound

main :: IO ()
main = print "hello"

--testImplicit = do
  --xs <- readLn :: IO [Int]
  --name xs $ \xs -> case xs of
    --Cons h t -> pure (gdpHead xs)
    --Nil      -> testImplicit
    --Nil      -> testImplicit
