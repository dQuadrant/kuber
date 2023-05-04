{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
module Cardano.Kuber.Core.Kontract
where
import Cardano.Api
import Cardano.Api.Shelley
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Set (Set)
import Cardano.Kuber.Utility.QueryHelper (queryProtocolParam, querySystemStart, queryAddressInEraUtxos, queryUtxos, queryTxins, queryChainPoint, queryGenesesisParams)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch(..))
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception (try, Exception)
-- import Cardano.Kuber.Api (FrameworkError)
import Cardano.Slotting.Time (SystemStart)
import Cardano.Kuber.Error (FrameworkError (..), ErrorType (..))


data  Exception e =>  Kontract api  w e r  =
      KResult !r
  |   KError !e
  |   KLift  ( api -> IO (Either e r))

eitherToKontract e = case e of
  Left e  -> KError e
  Right r ->  KResult r

instance  Exception e => Functor (Kontract api w e ) where
  fmap f (KResult x) = KResult (f x)
  fmap _ (KError x) = KError x
  fmap f (KLift x) = KLift $ \api -> (fmap . fmap ) f (x api )

instance Exception e => Applicative (Kontract api w e ) where
  pure = KResult


  (<*>) ::  Kontract api w e (a -> b) -> Kontract api w e a -> Kontract api w e b
  (<*>)  (KResult f) a =  fmap f a
  (<*>)  (KError e) _ =  KError e
  (<*>)   _ (KError e) =  KError e
  (<*>)  (KLift far) (KResult a) =  KLift $ \ api -> do
          result <- far api
          case result of
            Left e -> pure $ Left  e
            Right fab -> pure $ Right $ fab a
  (<*>)  (KLift lf) (KLift lf') =  KLift $ \api -> do
        fe <- lf api
        ve <- lf' api
        pure $ do
          f <- fe
          v <- ve
          pure $ f v

instance Exception e =>  Monad (Kontract api w e ) where

  (>>=) :: Kontract api w e a -> (a -> Kontract api w e b) -> Kontract api w e b
  (>>=) (KResult r) f =  f r
  (>>=) (KError r) _ = KError r
  (>>=) v@(KLift ior) f = KLift $ \api -> do
            result <- ior api
            case result of
              Left e -> pure $ Left  e
              Right a -> evaluateKontract api ( f a)

instance  Exception e =>  MonadIO (Kontract api w e) where
    liftIO :: IO r -> Kontract api w e r
    liftIO io = KLift $  \x -> try io


evaluateKontract    :: Exception e => a ->  Kontract a w e r  -> IO (Either e r )
evaluateKontract api  contract = do
  case contract of
    KResult r -> pure $ pure r
    KError e -> pure $ Left e
    KLift f -> f api

kGetBackend ::  Kontract  a w FrameworkError a
kGetBackend = KLift $ \api  -> pure (pure api)

kError :: ErrorType -> String -> Kontract api w FrameworkError r
kError t msg= KError (FrameworkError t msg)


kWrapParser ::  Either String  r -> Kontract api w FrameworkError r
kWrapParser m = case m  of 
  Left msg -> kError  ParserError msg
  Right v -> KResult v
