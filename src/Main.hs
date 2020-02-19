{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- Predicates about the possible shapes of positions
data IsBound n
data IsUnbound n

data BoundedCase n val =
  IsBound_ (Proof (IsBound val)) (n ~~  val) |
  IsUnbound_ (Proof (IsUnbound val))

boundClassify :: (Num n, Ord n) => (n ~~ val) -> BoundedCase n val
boundClassify val | the val < 10 && the val > 0 = IsBound_ axiom val
                  | otherwise                   = IsUnbound_ axiom

pattern IsBound :: Proof (IsBound val) -> (Int ~~  val) -> (Int ~~ val)
--brittany-disable-next-binding
pattern IsBound proof val <- (boundClassify -> IsBound_ proof val)

pattern IsUnbound :: Proof (IsUnbound val) -> (Int ~~ val)
--brittany-disable-next-binding
pattern IsUnbound proof <- (boundClassify -> IsUnbound_ proof)

data BoundedCase' n val where
  Bound_ ::Fact (IsBound val) => (n ~~  val) -> BoundedCase' n val
  Unbound_ ::Fact (IsUnbound val) => BoundedCase' n val

pattern Bound :: (Num n, Ord n) => Fact (IsBound val) => (n ~~ val) -> (n ~~ val)
--brittany-disable-next-binding
pattern Bound val <- (boundClassify' -> Bound_ val)

pattern Unbound :: (Num n, Ord n) => Fact (IsUnbound val) => (n ~~ val)
--brittany-disable-next-binding
pattern Unbound <- (boundClassify' -> Unbound_ )

boundClassify'
  :: forall  n val . (Num n, Ord n) => (n ~~ val) -> BoundedCase' n val
boundClassify' val
  | the val < 10 && the val > 0 = note (axiom :: Proof (IsBound val))
                                       (Bound_ val)
  | otherwise = note (axiom :: Proof (IsUnbound val)) Unbound_

add22Bound :: (Num n, Ord n, Fact (IsBound val)) => (n ~~  val) -> n
add22Bound val = the val + 2

main :: IO ()
main = do
  putStrLn "Enter a number > 0 and < 10."
  xs <- readLn :: IO Int
  name xs $ \case
    Bound val -> print $ add22Bound val
    Unbound   -> main

