{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Kuber.Data.TxBuilderAeson where

import Cardano.Api hiding (txFee, txMetadata)
import Cardano.Api.Ledger (Coin (Coin), ConwayTxCert (..), ShelleyTxCert (..), StrictMaybe (..), unboundRational)
import qualified Cardano.Api.Ledger as Ledger
import Cardano.Api.Shelley
  ( Proposal (unProposal),
    ReferenceScript (ReferenceScript, ReferenceScriptNone),
    ShelleyLedgerEra,
    VotingProcedure (unVotingProcedure),
    VotingProcedures (VotingProcedures, unVotingProcedures),
    fromLedgerPParamsUpdate,
    fromShelleyStakeAddr,
    scriptDataToJsonDetailedSchema,
    toShelleyAddr,
  )
import qualified Cardano.Api.Shelley as CApi
import Cardano.Binary (serializeEncoding)
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable (toConsoleText, toConsoleTextNoPrefix))
import Cardano.Kuber.Core.TxBuilder
import Cardano.Kuber.Data.EraUpdate (updateAddressEra)
import Cardano.Kuber.Data.Models (CertificateModal (CertificateModal), GovActionModal (GovActionModal), ProposalModal (ProposalModal), Wrapper (unWrap), unAddressModal)
import Cardano.Kuber.Data.Parsers (anyScriptParser, parseAddress, parseAddressBech32, parseAddressBinary, parseAddressRaw, parseAnyScript, parseAssetId, parseAssetNQuantity, parseAssetName, parseCbor, parseCborHex, parseHexString, parseScriptData, parseSignKey, parseTxIn, parseUtxo, parseUtxoCbor, parseValueText, parseValueToAsset, scriptDataParser, txInParser, txinOrUtxoParser)
import Cardano.Kuber.Error
import Cardano.Kuber.Util
import Cardano.Kuber.Utility.DataTransformation (addressInEraToAddressAny, fromLedgerAddress, pkhToPaymentKeyHash)
import Cardano.Kuber.Utility.ScriptUtil
import Cardano.Ledger.Api (Anchor (Anchor), GovAction (..), ProposalProcedure (..), VotingProcedures, serialiseRewardAcnt)
import Cardano.Ledger.Binary
  ( Annotator (..),
    DecCBOR (decCBOR),
    EncCBOR (encCBOR),
    ToCBOR (..),
    serialize,
    shelleyProtVer,
  )
import Cardano.Ledger.Core (PParamsUpdate (..))
import Cardano.Slotting.Time
import Codec.Serialise (serialise)
import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), ToJSONKey (toJSONKey), (.!=), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.Types (FromJSON (parseJSON), Parser, parseEither, parseMaybe, (.:))
import qualified Data.Aeson.Types as A
import Data.Bifunctor (second)
import qualified Data.ByteString as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SBS
import Data.Either
import qualified Data.Foldable as Foldable
import Data.Functor (($>), (<&>))
import qualified Data.HashMap.Internal.Strict as H
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.Ratio (denominator, numerator)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Internal.Fusion.CaseMapping (upperMapping)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import Data.Word (Word64)
import qualified Debug.Trace as Debug
import PlutusLedgerApi.V2 (CurrencySymbol, PubKeyHash (PubKeyHash, getPubKeyHash), ToData (toBuiltinData), TxOut, fromBuiltin, toBuiltin)
import PlutusTx (ToData)
import PlutusTx.IsData (toData)
import qualified Cardano.Ledger.Conway.PParams



instance
  (IsTxBuilderEra era) =>
  FromJSON (TxBuilder_ era)
  where
  parseJSON (A.Object v) =
    let objectKeys = Set.fromList $ map A.toText $ A.keys v
        pluralKeys :: [T.Text] =
          [ "selection",
            "input",
            "referenceInput",
            "output",
            "collateral",
            "mint",
            "signature",
            "proposal",
            "vote",
            "cert",
            "certificate"
          ]
        nonPluralKeys =
          [ "changeAddress",
            "metadata",
            "fee",
            "validityStart",
            "validityEnd",
            "validityStartSlot",
            "validityEndSlot"
          ]
        extraKeys = Set.difference objectKeys posisibleKeys
        posisibleKeys = Set.fromList $ pluralKeys ++ map (`T.append` "s") pluralKeys ++ nonPluralKeys
        requireAbsent k onPresent = do
          obj :: [A.Value] <- v .?< k
          if null obj then pure () else onPresent
        eraBasedParser :: ShelleyBasedEra era -> Parser (TxBuilder_ era)
        eraBasedParser sbera = case sbera of
          ShelleyBasedEraConway -> do
            -- TODO: Implement parsing of proposals
            proposals <- (do
                res <- v .?< "proposal" 
                pure $ map (\(ppm@(ProposalProcedureModal m)) -> (TxProposal ppm )) res
              )
            certs <-
              ( do
                  res <- v .?< "certificate"
                  pure $ map (\(CertificateModal m) -> m) res
                )
            votes <- v .?< "vote"
            commonParser proposals votes certs
          ShelleyBasedEraBabbage -> do
            requireAbsent "certificate" $ fail ".certificate[s] not supported. Kuber is in Babbage era"
            requireAbsent "proposal" $ fail ".proposal[s] not supported. Kuber is in Babbage era"
            requireAbsent "vote" $ fail ".vote[s] not supported. Kuber is in Babbage era"
            commonParser [] [] []
          _ -> fail "Unexpected era"
     in do
          unless (null extraKeys) (fail $ "Invalid fields in txBuilder :" ++ show extraKeys)
          eraBasedParser bShelleyBasedEra
    where
      commonParser :: [TxProposal era]
        -> [TxVote era] -> [Certificate era] -> Parser (TxBuilder_ era)  
      commonParser proposals votes certs = do
        TxBuilder_
          <$> (v .?< "selection")
          <*> v .?< "input"
          <*> v .?< "referenceInput"
          <*> v .?< "output"
          <*> v .?< "collateral"
          <*> v `parseValidity` "validityStart"
          <*> v `parseValidity` "validityEnd"
          <*> v .?< "mint"
          <*> v .?< "signature"
          <*> pure proposals
          <*> pure votes
          <*> pure certs
          <*> v .:? "fee"
          <*> (v .:? "changeAddress" <&> fmap (updateAddressEra . unAddressModal))
          <*> (v .:? "metadata" .!= Map.empty)

      parseValidity obj key = do
        mPosixTime <- obj .:? key
        case mPosixTime of
          Just posixTime -> pure $ ValidityPosixTime posixTime
          Nothing -> do
            mSlot <- obj .:? (key <> "Slot")
            case mSlot of
              Just slot -> pure $ ValiditySlot $ SlotNo $ slot
              _ -> pure $ NoValidityTime

      (.?<) ::  (FromJSON v) => A.Object -> T.Text -> Parser [v]
      (.?<) obj key = do
        mVal1 <- obj .:? A.fromText key
        case mVal1 of
          Nothing -> do
            mVal2 <- obj .:? A.fromText (key <> "s")
            case mVal2 of
              Nothing -> pure []
              Just any -> doReturn any $ A.fromText (key <> "s")
          Just val -> doReturn val $ A.fromText key
        where
          doReturn v resultKey =
            if isArray v
              then obj .: resultKey
              else do
                v <- obj .: resultKey
                pure [v]
      isArray val = case val of
        A.Array vec -> True
        _ -> False
  parseJSON _ = fail "TxBuilder must be an object"

instance FromJSON (TxMintData TxMintingScriptSource) where
  parseJSON (A.Object v) = do
    mintAmountm <- v .:? "amount"
    mintAmount_ <- case mintAmountm of
      Nothing -> v .: "tokens"
      Just any -> pure any

    exUnitsM <- v .:? "executionUnits"
    mintAmount <-
      mapM
        ( \(n, a :: A.Value) ->
            ( do
                v <- parseAssetName n
                (q, meta) <- case a of
                  A.Object km -> do
                    mintAmount <- km .:? "amount" .!= 1
                    meta_ <- km .:? "meta"
                    meta <- case meta_ of
                      Nothing -> km .:? "metadata" .!= mempty
                      Just any -> pure any
                    pure (mintAmount, meta)
                  A.Number sci ->
                    pure (Quantity $ round sci, mempty)
                  _ -> fail "expected token value to be mint amount or object"
                pure (v, q, meta)
            )
        )
        $ Map.toList mintAmount_
    let quantities = map (\(t, q, _) -> (t, q)) mintAmount
        metaList :: [Map Word64 (Map AssetName A.Value)]
        metaList = map (\(t, _, m) -> morphMetaMap t m) mintAmount
        metaMap = foldl (Map.unionWith (<>)) mempty metaList
        morphMetaMap token mp = Map.fromList $ map (second (Map.singleton token)) (Map.toList mp)
    let doReturn c = pure $ TxMintData c quantities metaMap
    scriptAny <- v .: "script"

    case scriptAny of
      A.String s -> do
        txin <- parseTxIn s
        mintRedeemerObj <- v .:? "redeemer"
        mintRedeemer <- case mintRedeemerObj of
          Nothing -> pure Nothing
          Just any -> do
            d <- scriptDataParser any
            pure $ pure d
        doReturn $ TxMintingReferenceScript txin exUnitsM mintRedeemer
      A.Object obj -> do
        txScript <- parseJSON scriptAny
        case txScript of
          TxScriptPlutus psc -> do
            mintRedeemer <- v .: "redeemer" >>= scriptDataParser
            doReturn $ TxMintingPlutusScript psc exUnitsM mintRedeemer
          TxScriptSimple simpleSc -> doReturn $ TxMintingSimpleScript simpleSc
      _ -> fail "Either ReferenceInput or Script Object expected "
  parseJSON _ = fail "TxMintData must be an object"

instance IsString a => MonadFail (Either a) where
  fail msg = Left $ fromString msg

instance IsTxBuilderEra era => ToJSON (TxBuilder_ era) where
  toJSON (TxBuilder_ selections inputs refInputs outputs collaterals validityStart validityEnd mintData signatures proposals votes certs fee defaultChangeAddr metadata) =
    A.object nonEmpyPair
    where
      -- appendNonEmpty :: (Foldable t, KeyValue a1, ToJSON (t a2)) => A.Key -> t a2 -> [a1] -> [a1]
      appendNonEmpty key val obj = if null val then obj else (key .= val) : obj

      appendValidity key val obj = case val of
        NoValidityTime -> obj
        ValidityPosixTime ndt -> (key .= ndt) : obj
        ValiditySlot sn -> ((key <> "Slot") .= sn) : obj

      nonEmpyPair :: [A.Pair]
      nonEmpyPair =
        "selections" >= concatMap collectSelection selections
          <+> "inputs" >= concatMap collectInputs inputs
          <+> "referenceInputs" >= concatMap collectRefInputs refInputs
          <+> "collaterals" >= concatMap collectColalteral collaterals
          <+> "mint" >= mintData
          <+> "outputs" >= outputs
          <+> "validityStart" `appendValidity` validityStart
          <+> "validityEnd" `appendValidity` validityEnd
          <+> "signatures" >= signatures
          <+> "proposals" >= map (translateProposal bConwayOnward) proposals --todo
          <+> "votes" >= map (transformVote bShelleyBasedEra) votes
          <+> "certificates" >= map (transformCerts bShelleyBasedEra) certs
          <+> "fee" >= fee
          <+> "changeAddress" >= defaultChangeAddr
          <#> "metadata" >= metadata
      infixl 8 >=
      (>=) a b = appendNonEmpty a b
      infixr 7 <#>
      (<#>) f f2 = f $ f2 []
      infixr 6 <+>
      (<+>) f v = f v

translateProposal :: Maybe (ConwayEraOnwards era) -> TxProposal era -> A.Value
translateProposal sbera (proposal) = 
  case sbera of
    Nothing -> A.Null
    Just ceo -> case ceo of 
      ConwayEraOnwardsConway -> case proposal of 
        TxProposal  ppm -> toJSON $  ppm
        TxProposalScript ppm -> A.Null

transformCerts :: ShelleyBasedEra era -> Certificate era -> A.Value
transformCerts sbera c = case sbera of
  ShelleyBasedEraShelley -> toJSON certModal
  ShelleyBasedEraAllegra -> toJSON certModal
  ShelleyBasedEraMary -> toJSON certModal
  ShelleyBasedEraAlonzo -> toJSON certModal
  ShelleyBasedEraBabbage -> toJSON certModal
  ShelleyBasedEraConway -> toJSON certModal
  where
    certModal = CertificateModal c

transformVote :: ShelleyBasedEra era -> TxVote era -> A.Value
transformVote sbera vote = case sbera of
  ShelleyBasedEraShelley -> toJSON vote
  ShelleyBasedEraAllegra -> toJSON vote
  ShelleyBasedEraMary -> toJSON vote
  ShelleyBasedEraAlonzo -> toJSON vote
  ShelleyBasedEraBabbage -> toJSON vote
  ShelleyBasedEraConway -> toJSON vote

emptyHashableScriptData = unsafeHashableScriptData $ fromPlutusData $ toData $ toBuiltinData ()

instance ToJSON (TxMintData TxMintingScriptSource) where
  toJSON (TxMintData script value metadata) =
    A.object $
      [ "script"
          .= ( case script of
                 TxMintingPlutusScript tps _m_eu _sd -> toJSON tps
                 TxMintingReferenceScript ti _m_eu _m_sd -> A.String $ renderTxIn ti
                 TxMintingSimpleScript tss -> toJSON tss
             ),
        "amount" .= Map.fromList value,
        "metadata" .= metadata
      ]
        ++ ( case script of
               TxMintingPlutusScript _tps _m_eu hsd -> ["redeemer" .= scriptDataToJsonDetailedSchema hsd]
               TxMintingReferenceScript _ti _m_eu m_hsd -> case m_hsd of
                 Nothing -> []
                 Just hsd -> ["redeemer" .= scriptDataToJsonDetailedSchema hsd]
               TxMintingSimpleScript _tss -> []
           )

instance ToJSON (TxInputSelection era) where
  toJSON (TxSelectableAddresses v) = A.Array $ V.fromList $ Prelude.map toJSON v
  toJSON (TxSelectableTxIn v) = A.Array $ V.fromList $ Prelude.map toJSON v
  toJSON (TxSelectableUtxos utxo) = case utxo of UTxO map_ -> A.Array $ V.fromList $ map (toJSON . fst) (Map.toList map_)
  toJSON (TxSelectableSkey sk) = A.Array $ V.fromList $ map (A.String . serialiseToBech32) sk

collectSelection :: TxInputSelection era -> [Aeson.Value]
collectSelection (TxSelectableAddresses v) = map (A.String . serialiseAddress @(AddressInEra ConwayEra) . fromLedgerAddress) v
collectSelection (TxSelectableTxIn v) = map (A.String . renderTxIn) v
collectSelection (TxSelectableUtxos utxo) = utxoToAeson utxo
collectSelection (TxSelectableSkey sk) = map (A.String . serialiseToBech32) sk

instance ToJSON (TxInput era) where
  toJSON (TxInputUnResolved txInputTxin) = toJSON txInputTxin
  toJSON (TxInputResolved val) = toJSON val

collectInputs (TxInputUnResolved txInputTxin) = [toJSON txInputTxin]
collectInputs (TxInputResolved val) = collectResolvedInputs val

collectRefInputs (TxInputReferenceTxin txInputTxin) = [toJSON $ renderTxIn txInputTxin]
collectRefInputs (TxInputReferenceUtxo utxo) = utxoToAeson utxo

renderTxId :: TxIn -> T.Text
renderTxId (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId

renderTxIx :: TxIn -> Integer
renderTxIx (TxIn txId (TxIx ix)) = toInteger ix

instance ToJSON (TxInputUnResolved_ era) where
  toJSON (TxInputTxin txin) = A.String $ renderTxIn txin
  toJSON (TxInputReferenceScriptTxin _refScript _mData _redeemer _exUnits _txin) =
    A.object $
      [ "script" .= renderTxIn _refScript,
        "inlineDatum" .= isNothing _mData,
        "txid" .= renderTxId _txin,
        "index" .= renderTxIx _txin,
        "redeemer" .= scriptDataToJsonDetailedSchema _redeemer
      ]
        ++ ( case _exUnits of
               Nothing -> []
               Just e@(ExecutionUnits mem step) -> ["exUnits" .= e]
           )
        ++ ( case _mData of
               Nothing -> []
               Just sd -> ["datum" .= scriptDataToJsonDetailedSchema sd]
           )
  toJSON (TxInputScriptTxin _script _data _redeemer _exUnits _txin) =
    A.object $
      [ "script" .= _script,
        "redeemer" .= scriptDataToJsonDetailedSchema _redeemer,
        "txid" .= renderTxId _txin,
        "index" .= renderTxIx _txin
      ]
        ++ ( case _data of
               Nothing -> []
               Just sd -> ["data" .= scriptDataToJsonDetailedSchema sd]
           )
        ++ ( case _exUnits of
               Nothing -> []
               Just e@(ExecutionUnits mem step) -> ["exUnits" .= e]
           )
  toJSON (TxInputSkey skey) = A.String $ serialiseToBech32 skey
  toJSON (TxInputAddr _addr) = A.String $ serialiseAddress $ addressInEraToAddressAny _addr

instance ToJSON (TxInputResolved_ era) where
  toJSON v = A.Array $ V.fromList $ collectResolvedInputs v


collectResolvedInputs :: TxInputResolved_ era -> [Aeson.Value]
collectResolvedInputs v = case v of
  TxInputUtxo umap -> map (A.Object . A.fromList) (collectUtxoPair umap)
  TxInputScriptUtxo tvs sd sd' m_eu uto -> 
    map
      ( \v ->
          A.Object $
            A.fromList $
              ["utxo" .= A.Object (A.fromList v)]
                ++ map (\sd -> "datum" .= scriptDataToJsonDetailedSchema sd) (maybeToList sd)
                ++ ["redeemer" .= scriptDataToJsonDetailedSchema sd']
                ++ ["script" .= tvs]
      )
      (collectUtxoPair $ UTxO $ uncurry Map.singleton uto)
  TxInputReferenceScriptUtxo refSc sd sd' m_eu uto ->
    map
      ( \v ->
          A.Object $
            A.fromList $
              ["utxo" .= A.Object (A.fromList v)]
                ++ map (\sd -> "datum" .= scriptDataToJsonDetailedSchema sd) (maybeToList sd)
                ++ ["redeemer" .= scriptDataToJsonDetailedSchema sd']
                ++ ["referenceScript" .= renderTxIn refSc]
      )
      (collectUtxoPair $ UTxO $ uncurry Map.singleton uto)

instance ToJSON TxPlutusScript where
  toJSON tps = case tps of
    TxPlutusScriptV1 ps -> toJSON $ serialiseToTextEnvelope Nothing ps
    TxPlutusScriptV2 ps -> toJSON $ serialiseToTextEnvelope Nothing ps
    TxPlutusScriptV3 ps -> toJSON $ serialiseToTextEnvelope Nothing ps

instance FromJSON TxPlutusScript where
  parseJSON (A.Object o) = do
    _type :: T.Text <- o .: "type"
    case _type of
      "PlutusScriptV1" -> o .: "cborHex" >>= parseCborHex @T.Text <&> TxPlutusScriptV1
      "PlutusScriptV2" -> o .: "cborHex" >>= parseCborHex @T.Text <&> TxPlutusScriptV2
      "PlutusScriptV3" -> o .: "cborHex" >>= parseCborHex @T.Text <&> TxPlutusScriptV3
      _ -> fail "Expected either PlutsScriptV1 or PlutusScriptV2 or PlutusScriptV3 type"
  parseJSON _ = error "Expected Object"

instance FromJSON TxScript where
  parseJSON v@(A.Object o) = do
    tryPlutus <|> trySimple
    where
      tryPlutus = parseJSON v <&> TxScriptPlutus
      trySimple = do
        v :: SimpleScript <- parseJSON v
        pure $ TxScriptSimple $ TxSimpleScript v
  parseJSON _ = error "Expected Object"

instance ToJSON TxSimpleScript where
  toJSON (TxSimpleScript ss) = toJSON ss

instance ToJSON TxScript where
  toJSON script = case script of
    TxScriptSimple tss -> toJSON tss
    TxScriptPlutus tps -> toJSON tps

instance ToJSON HashableScriptData where
  toJSON scriptData = scriptDataToJson ScriptDataJsonNoSchema scriptData

instance IsCardanoEra era => ToJSON (TxOutput (TxOutputContent era)) where
  toJSON (TxOutput _content _deductFee _addChange onMinAda) =
    A.object $
      outputContentJsonPair _content
        ++ ["addChange" .= _addChange | _addChange]
        ++ (["deductFee" .= _deductFee | _deductFee])
        ++ ( case onMinAda of
               DropOnUtxoInsufficientUtxoAda -> ["insuffientUtxoAda" .= A.fromString "drop"]
               IncreaseOnUtxoInsufficientUtxoAda -> ["insuffientUtxoAda" .= A.fromString "increase"]
               ErrorOnInsufficientUtxoAda -> ["insuffientUtxoAda" .= A.fromString "error"]
               OnInsufficientUtxoAdaUnset -> []
           )

-- outputContentJsonPair :: IsCardanoEra era => KeyValue a => TxOutputContent era -> [a]
outputContentJsonPair v = case v of
  TxOutPkh pkh va ->
    [ "pkh" .= show pkh,
      "value" .= va
    ]
  TxOutScript tps va ha ->
    [ "script" .= tps,
      "value" .= va,
      "datumHash" .= serialiseToRawBytesHexText ha
    ]
  TxOutScriptInline tps va ha ->
    [ "script" .= tps,
      "value" .= va,
      "datumHash" .= serialiseToRawBytesHexText ha
    ]
  TxOutScriptWithScript tps va ha ts ->
    [ "address" .= tps,
      "script" .= ts,
      "value" .= va,
      "datumHash" .= serialiseToRawBytesHexText ha,
      "inlineScript" .= ts
    ]
  TxOutScriptWithData script va sd ->
    [ "address" .= script,
      "value" .= va,
      "datum" .= scriptDataToJsonDetailedSchema sd
    ]
  TxOutScriptWithDataAndScript tps va sd ts ->
    [ "address" .= tps,
      "inlineScript" .= ts,
      "value" .= va,
      "datum" .= scriptDataToJsonDetailedSchema sd
    ]
  TxOutScriptWithDataAndReference tps va sd ->
    [ "script" .= tps,
      "value" .= va,
      "datum" .= scriptDataToJsonDetailedSchema sd
    ]
  TxOutNative (TxOut addr (TxOutValueShelleyBased sbe v) txoutData refScript) ->
    ["address" .= addr]
      ++ ["value" .= fromLedgerValue sbe v]
      ++ ( case txoutData of
             TxOutDatumNone -> []
             TxOutDatumHash aeo ha -> ["datumHash" .= serialiseToRawBytesHexText ha]
             TxOutDatumInTx aeo hsd -> ["datum" .= scriptDataToJsonDetailedSchema hsd, "inlineDatum" .= False]
             TxOutDatumInline beo hsd -> ["datum" .= scriptDataToJsonDetailedSchema hsd]
             --  TxOutDatumHash sdsie ha -> ["datumHash" .= serialiseToRawBytesHexText ha]
             --  TxOutDatumInline rtisidsie sd -> ["datum" .= scriptDataToJsonDetailedSchema sd]
             --  TxOutDatumInTx
             --  _ -> []
         )
      ++ ( case refScript of
             ReferenceScript rtisidsie sial -> ["inlineScript" .= sial]
             ReferenceScriptNone -> []
         )
  TxOutNative _ -> error "Unexpected"

instance ToJSON (TxCollateral era) where
  toJSON (TxCollateralTxin _collateral) = toJSON _collateral
  toJSON (TxCollateralUtxo utxo) = toJSON $ utxoToAeson utxo

collectColalteral (TxCollateralTxin _collateral) = [A.String $ renderTxIn _collateral]
collectColalteral (TxCollateralUtxo utxo) = utxoToAeson utxo

instance (IsCardanoEra era) => ToJSON (TxSignature era) where
  toJSON (TxSignatureAddr _sigAddr) = toJSON _sigAddr
  toJSON (TxSignaturePkh pkh) = toJSON $ toHexString @T.Text $ fromBuiltin $ getPubKeyHash pkh
  toJSON (TxSignatureSkey skey) = toJSON $ serialiseToBech32 skey

instance (IsTxBuilderEra era) => FromJSON (TxInputSelection era) where
  parseJSON v@(A.String txt) = do
    case parseHexString txt of
      Just str -> case parseAddressRaw @ConwayEra (LBS.toStrict str) of
        Nothing -> case parseUtxoCbor str of
          Just utxo -> pure $ TxSelectableUtxos utxo
          Nothing -> fail "Invalid InputSelection Hex:  It must be  address, txHash#index or  utxoCbor"
        Just addr -> pure $ TxSelectableAddresses [toShelleyAddr addr]
      Nothing -> case parseAddressBech32 @ConwayEra txt of
        Just addr -> pure $ TxSelectableAddresses [toShelleyAddr addr]
        Nothing -> case parseTxIn txt of
          Just txin -> pure $ TxSelectableTxIn [txin]
          Nothing -> case parseSignKey txt of
            Just sk -> pure $ TxSelectableSkey [sk]
            Nothing -> fail "Invalid InputSelection String : It must be  address, txHash#index,  or  utxoCbor"
  parseJSON v@(A.Object o) = do
    _type :: Maybe String <- o .:? "type"
    case _type of
      Nothing -> txinOrUtxoParser' (\x -> TxSelectableTxIn [x]) TxSelectableUtxos v
      Just envelopeType -> do
        envelope <- parseJSON v
        case deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey) envelope of
          Left tee -> fail $ ".selections " ++ "Got type=\"" ++ envelopeType ++ "\" But parse failed."
          Right sk -> pure $ TxSelectableSkey [sk]
  parseJSON _ = fail "Expected json or object"

instance IsTxBuilderEra era => FromJSON (TxInput era) where
  parseJSON o@(A.Object v) = do
    utxo <- v .: "utxo" <|> v .: "txIn" <|> v .: "txin"
    utxoOrTxin <- txinOrUtxoParser utxo
    mScript :: (Maybe A.Value) <- v .:? "script"
    case mScript of
      Nothing ->
        pure
          ( case utxoOrTxin of
              Left ti -> TxInputUnResolved $ TxInputTxin ti
              Right uto -> TxInputResolved $ TxInputUtxo uto
          )
      Just scriptJson -> do
        mData <- v .:? "datum"
        redeemer <- v .: "redeemer" >>= scriptDataParser
        _exUnits <- v .:? "executionUnits"
        exUnits <- case _exUnits of
          Nothing -> v .:? "exUnits"
          Just eu -> pure eu
        mParsedData <- case mData of
          Nothing -> pure Nothing
          Just datum -> do
            sd <- scriptDataParser datum
            pure $ Just sd
        case utxoOrTxin of
          Left ti -> do
            inputFunc <- case scriptJson of
              A.String txt -> parseTxIn txt <&> TxInputReferenceScriptTxin
              _ ->
                parseJSON scriptJson
                  <&> TxInputScriptTxin
            pure $ TxInputUnResolved $ inputFunc mParsedData redeemer exUnits ti
          Right (UTxO uto) -> do
            inputFunc' <- case scriptJson of
              A.String txt -> parseTxIn txt <&> TxInputReferenceScriptUtxo
              _ ->
                parseJSON scriptJson
                  <&> TxInputScriptUtxo
            pure $ TxInputResolved $ inputFunc' mParsedData redeemer exUnits (head $ Map.toList uto)
  parseJSON v@(A.String s) = do
    case parseHexString s of
      Just str -> case parseAddressRaw $ LBS.toStrict str of
        Nothing -> case parseUtxoCbor str of
          Just utxo -> pure $ TxInputResolved $ TxInputUtxo utxo
          Nothing -> do
            case parseCbor str of
              Nothing -> fail $ "Invalid Input HexString : It must be  address, txHash#index or  utxoCbor"
              Just sk -> pure $ TxInputUnResolved $ TxInputSkey sk
        Just addr -> pure $ TxInputUnResolved $ TxInputAddr addr
      Nothing -> case parseAddressBech32 s of
        Just addr -> pure $ TxInputUnResolved $ TxInputAddr addr
        Nothing -> case parseTxIn s of
          Just txin -> pure $ TxInputUnResolved $ TxInputTxin txin
          Nothing -> case parseSignKey s of
            Just sk -> pure $ TxInputUnResolved $ TxInputSkey sk
            Nothing -> fail "Invalid Input String : It must be  address, txHash#index or  utxoCbor"
  parseJSON _ = fail "TxInput must be an object or string"

instance IsTxBuilderEra era => FromJSON (TxInputReference era) where
  parseJSON v@(A.String _) = txinOrUtxoParser' TxInputReferenceTxin TxInputReferenceUtxo v
  parseJSON v@(A.Object _) = txinOrUtxoParser' TxInputReferenceTxin TxInputReferenceUtxo v
  parseJSON _ = fail "Expected InputReference string or object"

instance FromJSON InsufficientUtxoAdaAction where
  parseJSON (A.String _v)
    | v == "drop" = pure DropOnUtxoInsufficientUtxoAda
    | v == "increase" || v == "fix" = pure IncreaseOnUtxoInsufficientUtxoAda
    | v == "error" = pure ErrorOnInsufficientUtxoAda
    | otherwise = fail $ "Expected one of drop, increase or error, got: " ++ T.unpack v
    where
      v = T.toLower _v
  parseJSON A.Null = pure OnInsufficientUtxoAdaUnset
  parseJSON _ = fail "Expected InsufficientUtxoAdaAction string : 'drop', 'increase' or 'error'"

instance (IsTxBuilderEra era) => FromJSON (TxOutput (TxOutputContent era)) where
  parseJSON json@(A.Object v) = do
    -- Parse TxOutput according to address type if simple
    -- then use address with value TxOutAddress otherwise use TxOutScriptAddress or TxOutScript
    -- which has script and data if there is no script given then take address
    -- as script address and get data hash from the datum or directly from dataHash
    -- if there is script given then use that script and datahash
    addChange' <- v .:? "addChange" .!= False
    deductFee' <- v .:? "deductFee" .!= False
    insuffientUtxoAda <- v .:? "insuffientUtxoAda" .!= OnInsufficientUtxoAdaUnset
    addressTextM :: Maybe A.Value <- v .:? "address"

    destination <- case addressTextM of
      Nothing -> pure Nothing
      Just v@(A.Object o) -> do
        v <- parseJSON v
        pure $ pure $ pure v
      Just (A.String addrTxt) -> do
        val <- parseAddress addrTxt
        pure $ pure $ Left val
      _ -> error "Unexpected value"
    maybeVal :: Maybe A.Value <- v .:? "value"

    value <- case maybeVal of
      Just (A.String txt) -> parseValueText txt
      Just (A.Number sci) -> pure $ valueFromList [(AdaAssetId, Quantity $ round sci)]
      Just val -> parseJSON val
      _ -> pure $ valueFromList []
    datumHashE <- parseData
    shouldEmbed <- v .:? "inlineDatum" .!= True
    let txOutDatum =
          if shouldEmbed
            then case datumHashE of
              Nothing -> TxOutDatumNone
              Just (Left sd) -> TxOutDatumInline bBabbageOnward sd
              Just (Right dh) -> TxOutDatumHash bAlonzoOnward dh
            else
              ( case datumHashE of
                  Just (Left datum) -> TxOutDatumInTx bAlonzoOnward datum
                  Just (Right dh) -> TxOutDatumHash bAlonzoOnward dh
                  _ -> TxOutDatumNone
              )
    mScript_ <- v .:? "script"
    mScript <- case mScript_ of
      Nothing -> v .:? "inlineScript"
      Just va -> pure mScript_
    -- based on outputAddress and inlineScript determine what to do. (Automatic calculation  of destination address
    -- from script code has increased the code drastically )
    output <- case destination of
      Nothing -> case mScript of
        Nothing -> fail "missing both inlineScript and address"
        Just obj -> do
          sc <- parseJSON obj
          case txOutDatum of
            TxOutDatumNone -> fail "Missing datum in script output"
            TxOutDatumHash sdsie ha -> pure $ TxOutScriptInline sc value ha
            TxOutDatumInline rtisidsie sd -> pure $ TxOutScriptWithDataAndReference sc value sd
            _ -> error "Unexpected"
      Just (Left addr) -> case mScript of
        Nothing -> pure $ TxOutNative $ TxOut addr (valueToRequiredEra bCardanoEra value) txOutDatum ReferenceScriptNone
        Just (A.Bool _) -> pure $ TxOutNative $ TxOut addr (valueToRequiredEra bCardanoEra value) txOutDatum ReferenceScriptNone
        Just obj -> do
          sc <- anyScriptParser obj
          pure $ TxOutNative $ TxOut addr (valueToRequiredEra bCardanoEra value) txOutDatum (ReferenceScript bBabbageOnward sc)
      Just (Right script) ->
        let defaultAct = case txOutDatum of
              TxOutDatumNone -> fail "Missing datum in script output"
              TxOutDatumHash sdsie ha -> pure $ TxOutScript script value ha
              TxOutDatumInline rtisidsie sd -> pure $ TxOutScriptWithData script value sd
              _ -> error "Unexpected"
         in case mScript of
              Nothing -> defaultAct
              Just (A.Bool False) -> defaultAct
              Just (A.Bool True) -> case txOutDatum of
                TxOutDatumNone -> fail "Missing datum in script output"
                TxOutDatumHash sdsie ha -> pure $ TxOutScriptInline script value ha
                TxOutDatumInline rtisidsie sd -> pure $ TxOutScriptWithDataAndReference script value sd
                _ -> error "Unexpected"
              Just va -> do
                sc <- parseJSON va
                case txOutDatum of
                  TxOutDatumNone -> fail "Missing datum in script output"
                  TxOutDatumHash sdsie ha -> pure $ TxOutScriptWithScript script value ha sc
                  TxOutDatumInline rtisidsie sd -> pure $ TxOutScriptWithDataAndScript script value sd sc
                  _ -> error "Unexpected"
    pure $ TxOutput output deductFee' addChange' insuffientUtxoAda
    where
      valueToRequiredEra :: CardanoEra era -> Value -> TxOutValue era
      valueToRequiredEra era v = case era of
        MaryEra -> TxOutValueShelleyBased ShelleyBasedEraMary (toLedgerValue MaryEraOnwardsMary v)
        AlonzoEra -> TxOutValueShelleyBased ShelleyBasedEraAlonzo (toLedgerValue MaryEraOnwardsAlonzo v)
        BabbageEra -> TxOutValueShelleyBased ShelleyBasedEraBabbage (toLedgerValue MaryEraOnwardsBabbage v)
        ConwayEra -> TxOutValueShelleyBased ShelleyBasedEraConway (toLedgerValue MaryEraOnwardsConway v)
        _ -> error "unexpected era"

      parseData :: Parser (Maybe (Either HashableScriptData (Hash ScriptData)))
      parseData = do
        mDatum <- v .:? "datum"
        case mDatum of
          Just datum -> do
            val <- scriptDataParser datum
            pure $ pure $ Left $ val
          Nothing -> do
            datumHashM_ <- v .:? "datumHash"
            datumHashM <- case datumHashM_ of
              Nothing -> v .:? "dataHash"
              Just any -> pure datumHashM_
            case datumHashM of
              Just dHash -> case deserialiseFromRawBytesHex (AsHash AsScriptData) (T.encodeUtf8 dHash) of
                Left e -> fail "Expected hex string "
                Right dh -> pure $ pure $ pure dh
              Nothing -> pure Nothing
  parseJSON _ = fail "TxOutput must be an object"

instance IsTxBuilderEra era => FromJSON (TxCollateral era) where
  parseJSON v@(A.Object o) = txinOrUtxoParser' TxCollateralTxin TxCollateralUtxo v
  parseJSON v@(A.String s) = txinOrUtxoParser' TxCollateralTxin TxCollateralUtxo v
  parseJSON _ = fail "Expected Collateral to be TxHash#index or utxoCbor or txIn object"

instance IsTxBuilderEra era => FromJSON (TxSignature era) where
  parseJSON (A.String v) = case parseHexString v of
    Just uhHexStr ->
      if BS.length uhHexStr == 28
        then pure $ TxSignaturePkh (PubKeyHash $toBuiltin uhHexStr)
        else parseAddressBinary uhHexStr <&> TxSignatureAddr
    Nothing ->
      case parseAddressBech32 v of
        Nothing -> case parseSignKey v of
          Just sk -> pure $ TxSignatureSkey sk
          Nothing -> fail $ "Invalid address string: " ++ T.unpack v
        Just aie -> pure $ TxSignatureAddr aie
  parseJSON _ = fail "TxSignature must be an String Address or PubKeyHash"

-- collectUtxoPair :: KeyValue a => UTxO era -> [[a]]
collectUtxoPair (UTxO uMap) = map txOutToKeyVal $ Map.toList uMap
  where
    txOutToKeyVal (TxIn hash index, TxOut addr val datum sc) =
      [ "txIn"
          .= A.object
            [ "hash" .= hash,
              "index" .= index
            ],
        "address" .= serialiseAddress (addressInEraToAddressAny addr),
        "value" .= toVal val
      ]
        ++ scriptToPair sc
        ++ datumToPair datum

    toVal (TxOutValueShelleyBased sbe v) = toJSON $ fromLedgerValue sbe v
    toVal (TxOutValueByron l) = toJSON $ lovelaceToValue l

    scriptToPair sc = case sc of
      ReferenceScript rtisidsie sial -> ["inlineScript" .= toHexString @T.Text (case sial of ScriptInAnyLang sl sc' -> serialiseToRawBytes $ hashScript sc')]
      ReferenceScriptNone -> []

    -- datumToPair :: KeyValue a => TxOutDatum ctx era -> [a]
    datumToPair datum = case datum of
      TxOutDatumNone -> []
      TxOutDatumHash sdsie ha -> ["datumHash" .= toHexString @T.Text (serialiseToRawBytes ha)]
      TxOutDatumInline rtisidsie sd -> ["datum" .= scriptDataToJsonDetailedSchema sd]
      _ -> error "Unexpected"

utxoToAeson :: UTxO era -> [Aeson.Value]
utxoToAeson uMap = map (A.Object . A.fromList) $ collectUtxoPair uMap

txinOrUtxoParser' f1 f2 o =
  txinOrUtxoParser o
    >>= ( \case
            Left ti -> pure $ f1 ti
            Right uto -> pure $ f2 uto
        )
