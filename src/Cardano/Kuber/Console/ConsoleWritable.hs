{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
module Cardano.Kuber.Console.ConsoleWritable
where

import Cardano.Api
import qualified Data.Map as Map
import Cardano.Api.Shelley (Lovelace(Lovelace), TxBody (ShelleyTxBody), ReferenceScript (ReferenceScriptNone, ReferenceScript))
import GHC.Real
import Data.List
import qualified Data.Set as Set
import Control.Monad (join)
import qualified Cardano.Ledger.Alonzo.Tx as LedgerBody
import qualified Data.Text as T
-- import Cardano.Ledger.Alonzo.TxBody (ppTxBody)
-- import Cardano.Ledger.Alonzo.Scripts (ppScript)
-- import qualified Shelley.Spec.Ledger.TxBody as LedgerBody (TxIn (TxIn))
class ConsoleWritable v where
  -- ^ toConsoleText prefix -> object -> Printable text
  toConsoleText ::   String-> v -> String

  toConsoleTextNoPrefix :: v -> String
  toConsoleTextNoPrefix v = toConsoleText "" v


instance ConsoleWritable TxIn where
  toConsoleText prefix txin = prefix ++ toConsoleTextNoPrefix txin
  toConsoleTextNoPrefix txin = T.unpack (renderTxIn txin)

instance IsCardanoEra era =>  ConsoleWritable (UTxO era) where
  toConsoleText prefix (UTxO utxoMap) =  prefix ++ intercalate ( "\n" ++ prefix) (map toStrings $ Map.toList utxoMap)
    where
      toStrings (TxIn txId (TxIx index),TxOut addr value hash refScript )=    showStr txId ++ "#" ++  show index ++" : " ++ (case value of
       TxOutAdaOnly oasie (Lovelace v) -> show v
       TxOutValue masie va ->  intercalate " +" (map vToString $valueToList va ) ) ++ showRefScript refScript
      vToString (AssetId policy asset,Quantity v)=show v ++ " " ++ showStr  policy ++ "." ++ showStr  asset
      vToString (AdaAssetId, Quantity v) = if v >99999
        then(
          let _rem= v `rem` 1_000_000
              _quot= v `quot` 1_000_000
          in
          case _rem of
                0 -> show _quot ++ " Ada"
                v-> show _quot ++"." ++ show _rem++ " Ada"
        )
        else show v ++ " Lovelace"

instance ConsoleWritable Value where
  toConsoleText prefix value= prefix ++ renderBalance value
    where
      renderBalance  balance =intercalate ("\n"++prefix) $  map renderAsset (valueToList balance)
      renderAsset (ass,q)=case ass of
        AdaAssetId  -> renderAda q
        AssetId p n        -> show q ++ "\t"++  showStr p  ++ "." ++ showStr n
      toTxOut (UTxO a) = Map.elems  a
      renderAda (Quantity q)= show ((fromIntegral q::Double)/1e6) ++ " Ada"

      toValue (TxOut _ v _ _) = case v of
        TxOutAdaOnly oasie lo -> lovelaceToValue lo
        TxOutValue masie va -> va

instance IsCardanoEra era => ConsoleWritable (TxOut ctx era ) where
 toConsoleTextNoPrefix  t@(TxOut aie val datum refScript) = T.unpack (serialiseAddress aie) ++ toConsoleText " : "  (toValue t ) ++ showRefScript refScript
  where
        toValue (TxOut _ v _ _) = case v of
          TxOutAdaOnly oasie lo -> lovelaceToValue lo
          TxOutValue masie va -> va
    
 toConsoleText prefix txout = prefix ++ toConsoleTextNoPrefix txout

showStr x = init $ tail $ show x

showRefScript refScript = case refScript of 
      ReferenceScript rtisidsie sial ->  case sial of { ScriptInAnyLang sl sc -> (case sl of
                                                          SimpleScriptLanguage ssv -> " + SimpleScript("
                                                          PlutusScriptLanguage psv -> " + PlutusScript(" ) ++ show (  hashScript sc) ++ ")" } 
      ReferenceScriptNone -> ""