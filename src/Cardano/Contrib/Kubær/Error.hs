{-# LANGUAGE OverloadedStrings  #-}
module Cardano.Contrib.Kubær.Error
where
    
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import GHC.Exception.Type (Exception)
import Control.Exception (throw)
import Cardano.Api (ToJSON)
import Data.Aeson (object, ToJSON (toJSON), KeyValue ((.=)))
import Cardano.Api

data ErrorType =    ConnectionError
                  | BalancingError
                  | InsufficientInput 
                  | EraMisMatch
                  | NodeQueryError
                  | LibraryError
                  | ParserError
                  | PlutusScriptEror
                  | ExUnitCalculationError
                  | FeatureNotSupported
                  | WrongScriptType deriving Show



data FrameworkError =  FrameworkError{
                    feType:: ErrorType,
                    feMessage :: String
                  } 
        | FrameworkErrors [FrameworkError]

instance Show FrameworkError where
  show  (FrameworkError t m)= "FrameworkError: "++show t ++ ": "++show m
  
instance ToJSON FrameworkError where
  toJSON (FrameworkError t m) = object ["type" .= show t, "message" .= m]

instance Exception FrameworkError where

