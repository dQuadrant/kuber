{-# LANGUAGE OverloadedStrings  #-}
module Cardano.Kuber.Error
where
    
import GHC.Exception.Type (Exception)
import Data.Aeson (object, ToJSON (toJSON), KeyValue ((.=)))

data ErrorType =  ConnectionError
                | BalancingError
                | InsufficientInput 
                | EraMisMatch
                | NodeQueryError
                | LibraryError
                | ParserError
                | PlutusScriptError
                | ExUnitCalculationError
                | FeatureNotSupported
                | BadMetadata
                | TxSubmissionError
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

instance Exception FrameworkError 


