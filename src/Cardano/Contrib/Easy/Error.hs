{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Contrib.Easy.Error
where
    
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import GHC.Exception.Type (Exception)
import Control.Exception (throw)

newtype SomeError =  SomeError String

instance Show SomeError where
  show   (SomeError m) = m

instance IsString SomeError where
  fromString v = SomeError v

instance Exception SomeError


unMaybe :: Applicative f =>  SomeError -> Maybe a  -> f a
unMaybe  e m= case m of
  Just v -> pure v
  Nothing -> throw  e

unEither :: Applicative f =>   Either SomeError a  -> f a
unEither e=case e of
    Left v -> throw v
    Right r -> pure r

maybeToEither ::  SomeError ->Maybe  a-> Either SomeError a
maybeToEither e m = case m of
  Nothing -> Left e
  Just a -> Right a