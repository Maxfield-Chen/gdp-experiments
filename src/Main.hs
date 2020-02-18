{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import Prelude hiding (head)
import Theory.Lists
import Theory.Named
import Logic.Proof
import Data.The
import Data.Coerce
import qualified Data.List as L
import Data.Ord

newtype SortedBy comp a = SortedBy a
instance The (SortedBy comp a) a

sortBy :: ((a -> a -> Ordering) ~~ comp)
       -> [a]
       -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

mergeBy :: Ord a => ((a -> a -> Ordering) ~~ comp) 
         -> SortedBy comp [a]
         -> SortedBy comp [a]
         -> SortedBy comp [a]
mergeBy comp xs ys = coerce $ foldr (\(x,y) ret ->
    if the comp x y == LT then x:y:ret else y:x:ret) [] (zip (the xs) (the ys))

main :: IO ()
main = do
  xs <- readLn :: IO [Int]
  ys <- readLn
  name (comparing Down) $ \gt -> do 
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
    print (the (mergeBy gt xs' ys'))

