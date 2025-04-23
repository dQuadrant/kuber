{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Websocket.TxBuilder where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Websocket.Forwarder
import Websocket.Utils (textToJSON)

newtype HydraGetUTxOResponse = HydraGetUTxOResponse
  { utxo :: M.Map T.Text UTXOEntry
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GroupedUTXO = GroupedUTXO
  { address :: T.Text,
    utxos :: [T.Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype UTXOEntry = UTXOEntry
  { address :: T.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

buildHydraDecommitTx utxos sk = do
  allUTxOs <- sendCommandToHydraNodeSocket GetUTxO
  let decoded = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOs) :: Maybe HydraGetUTxOResponse
  utxos <- case decoded of
    Just getUTxOResponse -> pure $ utxo getUTxOResponse
    Nothing -> error "buildHydraDecommitTx: Error parsing UTxO field"
  let groupedUTxOs = groupUtxosByAddress utxos
  pure groupedUTxOs

groupUtxosByAddress :: M.Map T.Text UTXOEntry -> [GroupedUTXO]
groupUtxosByAddress utxoMap =
  let grouped = foldl' insertUTXO M.empty (M.toList utxoMap)
   in map (\(addr, utxos) -> GroupedUTXO {address = addr, utxos = utxos}) (M.toList grouped)
  where
    insertUTXO acc (utxoId, UTXOEntry addr) =
      M.insertWith (++) addr [utxoId] acc
