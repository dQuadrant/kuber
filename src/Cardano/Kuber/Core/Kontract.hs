{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Kuber.Core.Kontract
where
import Cardano.Api
import Control.Exception (try, Exception, SomeException, catch)
import Cardano.Kuber.Error (FrameworkError (..), ErrorType (..))
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Exception.Base (throw)


data  Exception e =>  Kontract api  w e r  =
      KResult !r
  |   KError !e
  |   KLift  ( api -> IO (Either e r))

eitherToKontract :: Exception e => Either e r -> Kontract api w e r
eitherToKontract e = case e of
  Left err  -> KError err
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

instance  Exception e => Alternative (Kontract api w e) where
    empty ::  Kontract api w e a
    empty = liftIO $ throw ( FrameworkError LibraryError "Empty Alternative for Kontract" )

    (<|>) ::  Kontract api w e a -> Kontract api w e a -> Kontract api w e a
    (KError _) <|> b = b
    a <|> _ = a

instance Exception e =>  Monad (Kontract api w e ) where

  (>>=) :: Kontract api w e a -> (a -> Kontract api w e b) -> Kontract api w e b
  (>>=) (KResult r) f =  f r
  (>>=) (KError r) _ = KError r
  (>>=) (KLift ior) f = KLift $ \api -> do
            result <- ior api
            case result of
              Left e -> pure $ Left  e
              Right a -> evaluateKontract' api ( f a)

instance  Exception e =>  MonadIO (Kontract api w e) where
    liftIO :: IO r -> Kontract api w e r
    liftIO io = KLift $  \_ -> try io


evaluateKontract'    :: Exception e => a ->  Kontract a w e r  -> IO (Either e r )
evaluateKontract' api  contract = do
  case contract of
    KResult r -> pure $ pure r
    KError e -> pure $ Left e
    KLift f ->  f api

evaluateKontract    ::  a ->  Kontract a w FrameworkError r  -> IO (Either FrameworkError r )
evaluateKontract api  contract = do
  case contract of
    KResult r -> pure $ pure r
    KError e -> pure $ Left e
    KLift f -> mapException $ f api
  where
    mapException  action = catch  action someExHandler

kGetBackend :: Exception e =>  Kontract  a w e a
kGetBackend = KLift $ \api  -> pure (pure api)

kError :: ErrorType -> String -> Kontract api w FrameworkError r
kError t msg= KError (FrameworkError t msg)


kWrapParser ::  Either String  r -> Kontract api w FrameworkError r
kWrapParser m = case m  of
  Left msg -> kError  ParserError msg
  Right v -> KResult v


instance  MonadError FrameworkError  (Kontract api w FrameworkError) where
    throwError = KError
    catchError :: Kontract api w FrameworkError a -> (FrameworkError -> Kontract api w FrameworkError a) -> Kontract api w FrameworkError a
    catchError (KResult r) _ = KResult r
    catchError (KError e) handler = handler e
    catchError (KLift action) handler = KLift $ \api -> do
        result <- try @SomeException  (action api)
        let handlerResult err = case  handler err of
                KResult r -> return (Right r)
                KError e' -> return (Left e')
                KLift action' -> action' api
        case result of
          Left e -> handlerResult $ unHandledError e
          Right (Left err) -> handlerResult err
          Right(Right r) -> return (Right r)

unHandledError :: SomeException -> FrameworkError
unHandledError e = FrameworkError LibraryError ("Unhandled : " ++ show e)

someExHandler :: Applicative f => SomeException -> f (Either FrameworkError b)
someExHandler (e::SomeException ) = pure $ Left $  unHandledError e
