{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ExistentialQuantification     #-}


module Main where

import           Prelude
import           Theory.Lists
import           Theory.Named
import           Theory.Equality
import           Logic.Proof
import           Data.The
import           Data.Refined
import           Data.Coerce
import qualified Data.List                     as L
import           Data.Ord
import           Logic.Implicit

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

newtype Rev xs = Rev Defn

revRev :: Proof (Rev (Rev xs) == xs)
revRev = axiom

main :: IO ()
main = print "hello"

testImplicit = do
  xs <- readLn :: IO [Int]
  name xs $ \xs -> case xs of
    Cons h t -> pure (gdpHead xs)
    Nil      -> testImplicit
