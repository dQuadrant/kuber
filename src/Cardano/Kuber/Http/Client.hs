{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Kuber.Http.Client where

import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Kuber.Core.ChainAPI (HasChainQueryAPI (..), HasSubmitApi (..))
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Core.KuberAPI (HasKuberAPI (..))
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (bBabbageOnward, bCardanoEra), TxBuilder_ (TxBuilder_), TxCollateral (TxCollateralTxin, TxCollateralUtxo), TxInput (TxInputResolved, TxInputUnResolved), TxInputReference (TxInputReferenceTxin, TxInputReferenceUtxo), TxInputResolved_ (TxInputReferenceScriptUtxo, TxInputScriptUtxo, TxInputUtxo), TxInputSelection (TxSelectableAddresses, TxSelectableSkey, TxSelectableTxIn, TxSelectableUtxos), TxInputUnResolved_ (TxInputAddr, TxInputReferenceScriptTxin, TxInputScriptTxin, TxInputSkey, TxInputTxin), TxOutput (TxOutput), TxOutputContent (TxOutNative, TxOutPkh, TxOutScript, TxOutScriptInline, TxOutScriptWithData, TxOutScriptWithDataAndReference, TxOutScriptWithDataAndScript, TxOutScriptWithScript), TxSignature (TxSignatureAddr, TxSignaturePkh, TxSignatureSkey))
import Cardano.Kuber.Data.EraUpdate (updateAddressEra, updatePParamEra, updateTxOutInEra, updateUtxoEra, updateTxOutInEra')
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Error
import Cardano.Kuber.Http.Spec (KuberServerApi)
import Control.Exception (fromException)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Data (Proxy (Proxy))
import Data.Functor (($>), (<&>))
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), ManagerSettings (managerModifyRequest), Request (requestHeaders), Response (responseStatus), defaultManagerSettings, getUri, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (Status))
import Network.URI (URI (uriAuthority, uriPath, uriScheme), URIAuth (uriPort, uriRegName), parseURI)
import Servant.API.Alternative
import Servant.Client hiding (baseUrl)
import Text.Read (readMaybe)

cQueryPParams :: ClientM (LedgerProtocolParameters ConwayEra)
cQueryChainPoint :: ClientM ChainPointModal
cQueryUtxos :: [Text] -> [Text] -> ClientM (UtxoModal ConwayEra)
cQuerySystemStart :: ClientM SystemStartModal
cQueryGenesisParams :: ClientM (GenesisParamModal ShelleyEra)
cBuildTx :: Maybe Bool -> TxBuilder_ ConwayEra -> ClientM TxModal
cSubmitTx :: SubmitTxModal -> ClientM TxModal
cQueryTime :: ClientM TranslationResponse
cTimeToSlot :: TimeTranslationReq -> ClientM TranslationResponse
cTimeFromSlot :: SlotTranslationReq -> ClientM TranslationResponse
cCalculateFee :: TxModal -> ClientM Lovelace
cEvaluateExUnits :: TxModal -> ClientM ExUnitsResponseModal
( cQueryPParams
    :<|> cQueryChainPoint
    :<|> cQueryCurrentEra
    :<|> cQueryUtxos
    :<|> cQuerySystemStart
    :<|> cQueryGenesisParams
  )
  :<|> ( cBuildTx
           :<|> cSubmitTx
           :<|> cQueryTime
           :<|> cTimeToSlot
           :<|> cTimeFromSlot
         )
  :<|> ( cCalculateFee
           :<|> cEvaluateExUnits
         ) = client (Proxy :: Proxy (KuberServerApi era))

data RemoteKuberConnection = RemoteKuberConnection NetworkId ClientEnv

instance HasChainQueryAPI RemoteKuberConnection where
  kGetNetworkId = KLift $ \(RemoteKuberConnection net c) -> pure $ pure net

  kQueryProtocolParams = liftHttpReq (cQueryPParams <&> updatePParamEra bCardanoEra)
  kQuerySystemStart = liftHttpReq cQuerySystemStart <&> unWrap
  kQueryGenesisParams = liftHttpReq cQueryGenesisParams <&> unWrap
  kQueryUtxoByAddress ::
    IsTxBuilderEra era =>
    Set.Set AddressAny ->
    Kontract RemoteKuberConnection w FrameworkError (UTxO era)
  kQueryUtxoByAddress addrs = liftHttpReq (cQueryUtxos (map serialiseAddress $ Set.toList addrs) [] <&> (\(UtxoModal utxo) -> updateUtxoEra utxo))
  kQueryUtxoByTxin txins = liftHttpReq (cQueryUtxos [] (map renderTxIn $ Set.toList txins) <&> (\(UtxoModal utxo) -> updateUtxoEra utxo))
  kQueryChainPoint = liftHttpReq cQueryChainPoint <&> unWrap
  kQueryCurrentEra = liftHttpReq cQueryCurrentEra <&> unWrap
  kQueryGovState = error "TODO Cardano.Kuber.Http.Client.RemoteKuberConnection.queryGovState"

deserialiseTxToEra :: IsCardanoEra era => InAnyCardanoEra Tx -> AsType era -> Either FrameworkError (Tx era)
deserialiseTxToEra (InAnyCardanoEra ce tx) asType = do
  let serialised = serialiseToCBOR tx
      deserialsied = deserialiseFromCBOR (AsTx asType) serialised
  case deserialsied of
    Left sarbe -> Left $ FrameworkError EraMisMatch "Failed to Deserialise to ConwayEra"
    Right tx' -> Right tx'

parseToConwayTxBuilder :: TxBuilder_ BabbageEra -> TxBuilder_ ConwayEra
parseToConwayTxBuilder txbBabbage = case txbBabbage of
  TxBuilder_ tiss tis tirs tos tcs vt vt' tmds tss pros tvs cers m_n m_aie map ->
    TxBuilder_
      (Prelude.map updateTxInputSelectionEra tiss)
      (Prelude.map updateTxInputEra tis)
      (Prelude.map updateTxInputReferenceEra tirs)
      (Prelude.map updateTxOutputEra tos)
      (Prelude.map updateTxCollateralEra tcs)
      vt
      vt'
      tmds
      (Prelude.map updateTxSignatureEra tss)
      []
      []
      []
      m_n
      (updateMaybeAddressEra m_aie)
      map

-- updateShelleyLedgerEra :: IsTxBuilderEra era => ShelleyLedgerEra era1 -> ShelleyLedgerEra era
-- updateShelleyLedgerEra shelleyLedger = shelleyLedger

updateMaybeAddressEra :: IsTxBuilderEra era => Maybe (AddressInEra era1) -> Maybe (AddressInEra era)
updateMaybeAddressEra maybeAddress = case maybeAddress of
  Nothing -> Nothing
  Just aie -> Just $ updateAddressEra aie

-- updateTxVoteEra ::IsTxBuilderEra era => TxVote era1 -> TxVote era
-- updateTxVoteEra txVote = case txVote of { TxVote tvl ->TxVote $ (case tvl of { TxVoteL gai vp vo -> (case gai of { GovActionId ti gai' -> case ti of { TxId sh - > _ } })})  }

-- updateProposalEra :: IsTxBuilderEra era => Proposal era1 -> Proposal era
-- updateProposalEra proposal = case proposal of { Proposal pp -> Proposal $ (case pp of { ProposalProcedure co ra ga an -> transa }) }

updateTxSignatureEra :: IsTxBuilderEra era => TxSignature era1 -> TxSignature era
updateTxSignatureEra txSignature = case txSignature of
  TxSignatureAddr aie -> TxSignatureAddr (updateAddressEra aie)
  TxSignaturePkh pkh -> TxSignaturePkh pkh
  TxSignatureSkey sk -> TxSignatureSkey sk

updateTxCollateralEra :: IsTxBuilderEra era => TxCollateral era1 -> TxCollateral era
updateTxCollateralEra txCollateral = case txCollateral of
  TxCollateralTxin ti -> TxCollateralTxin ti
  TxCollateralUtxo uto -> TxCollateralUtxo (updateUtxoEra uto)

updateTxOutptputContentEra :: IsTxBuilderEra era => TxOutputContent era1 -> TxOutputContent era
updateTxOutptputContentEra txOutputContent = case txOutputContent of
  TxOutPkh pkh va -> TxOutPkh pkh va
  TxOutScript tps va ha -> TxOutScript tps va ha
  TxOutScriptInline tps va ha -> TxOutScriptInline tps va ha
  TxOutScriptWithScript tps va ha ts -> TxOutScriptWithScript tps va ha ts
  TxOutScriptWithData tps va hsd -> TxOutScriptWithData tps va hsd
  TxOutScriptWithDataAndScript tps va hsd ts -> TxOutScriptWithDataAndScript tps va hsd ts
  TxOutScriptWithDataAndReference tps va hsd -> TxOutScriptWithDataAndReference tps va hsd
  TxOutNative to -> TxOutNative (updateTxOutInEra' to)

updateTxOutputEra :: IsTxBuilderEra era => TxOutput (TxOutputContent era1) -> TxOutput (TxOutputContent era)
updateTxOutputEra txOutput = case txOutput of TxOutput toc b b' iuaa -> TxOutput (updateTxOutptputContentEra toc) b b' iuaa

updateTxInputReferenceEra :: IsTxBuilderEra era => TxInputReference era1 -> TxInputReference era
updateTxInputReferenceEra txInputReference = case txInputReference of
  TxInputReferenceTxin ti -> TxInputReferenceTxin ti
  TxInputReferenceUtxo uto -> TxInputReferenceUtxo (updateUtxoEra uto)

updateTxOutEra :: IsTxBuilderEra era => (TxIn, TxOut CtxUTxO era1) -> (TxIn, TxOut CtxUTxO era)
updateTxOutEra utxo = case utxo of (ti, to) -> (ti, updateTxOutInEra to)

updateTxInputEra :: IsTxBuilderEra era => TxInput era1 -> TxInput era
updateTxInputEra txInputs = case txInputs of
  TxInputResolved tir -> TxInputResolved $ case tir of
    TxInputUtxo uto -> TxInputUtxo (updateUtxoEra uto)
    TxInputScriptUtxo tps m_hsd hsd m_eu x0 -> TxInputScriptUtxo tps m_hsd hsd m_eu (updateTxOutEra x0)
    TxInputReferenceScriptUtxo ti m_hsd hsd m_eu x0 -> TxInputReferenceScriptUtxo ti m_hsd hsd m_eu (updateTxOutEra x0)
  TxInputUnResolved tiur -> TxInputUnResolved $ case tiur of
    TxInputTxin ti -> TxInputTxin ti
    TxInputSkey sk -> TxInputSkey sk
    TxInputAddr aie -> TxInputAddr (updateAddressEra aie)
    TxInputScriptTxin tps m_hsd hsd m_eu ti -> TxInputScriptTxin tps m_hsd hsd m_eu ti
    TxInputReferenceScriptTxin ti m_hsd hsd m_eu ti' -> TxInputReferenceScriptTxin ti m_hsd hsd m_eu ti'

updateTxInputSelectionEra :: IsTxBuilderEra era => TxInputSelection era1 -> TxInputSelection era
updateTxInputSelectionEra txInputSelections = case txInputSelections of
  TxSelectableAddresses ads -> TxSelectableAddresses ads
  TxSelectableUtxos uto -> TxSelectableUtxos (updateUtxoEra uto)
  TxSelectableTxIn tis -> TxSelectableTxIn tis
  TxSelectableSkey sks -> TxSelectableSkey sks

instance HasKuberAPI RemoteKuberConnection where
  kTxBuildTxBody :: IsTxBuilderEra era => TxBuilder_ era -> Kontract RemoteKuberConnection w FrameworkError (TxBody era)
  kTxBuildTxBody builder = doBuildTxBody bBabbageOnward builder
    where
      doBuildTxBody :: BabbageEraOnwards era -> TxBuilder_ era -> Kontract RemoteKuberConnection w FrameworkError (TxBody era)
      doBuildTxBody babbageOnward _builder = case babbageOnward of
        BabbageEraOnwardsBabbage -> do
          result <- liftHttpReq (cBuildTx (Just False) (parseToConwayTxBuilder _builder))
          pure $ parseModalInAnyEraToBodyInConwayEra result AsBabbageEra
        BabbageEraOnwardsConway -> do
          result <- liftHttpReq (cBuildTx (Just False) _builder)
          pure $ parseModalInAnyEraToBodyInConwayEra result AsConwayEra

      parseModalInAnyEraToBodyInConwayEra :: IsCardanoEra era =>TxModal -> AsType era -> TxBody era
      parseModalInAnyEraToBodyInConwayEra modalInAnyEra era = case modalInAnyEra of
        TxModal iace -> case deserialiseTxToEra iace era of
          Left fe -> error $ "Sad Error: " ++ show fe
          Right tx -> getTxBody tx

  kBuildTx :: IsTxBuilderEra era => TxBuilder_ era -> Kontract RemoteKuberConnection w FrameworkError (Tx era)
  kBuildTx builder = doBuildTx bBabbageOnward builder
    where
      doBuildTx :: BabbageEraOnwards era -> TxBuilder_ era -> Kontract RemoteKuberConnection w FrameworkError (Tx era)
      doBuildTx babbageOnward _builder = case babbageOnward of
        BabbageEraOnwardsBabbage -> do
          result <- liftHttpReq (cBuildTx (Just False) (parseToConwayTxBuilder _builder))
          pure $ parseModalInAnyEraToTxInConwayEra result AsBabbageEra
        BabbageEraOnwardsConway -> do
          result <- liftHttpReq (cBuildTx (Just False) _builder)
          pure $ parseModalInAnyEraToTxInConwayEra result AsConwayEra

      parseModalInAnyEraToTxInConwayEra :: IsCardanoEra era => TxModal -> AsType era -> Tx era
      parseModalInAnyEraToTxInConwayEra modalInAnyEra era = case modalInAnyEra of
        TxModal iace -> case deserialiseTxToEra iace era of
          Left fe -> error $ "Sad Error: " ++ show fe
          Right tx -> tx

  kTimeToSlot :: POSIXTime -> Kontract RemoteKuberConnection w FrameworkError SlotNo
  kTimeToSlot slot = liftHttpReq (cTimeToSlot (TimeTranslationReq slot)) <&> tResSlotNo
  kSlotToTime :: SlotNo -> Kontract RemoteKuberConnection w FrameworkError POSIXTime
  kSlotToTime time = liftHttpReq (cTimeFromSlot (SlotTranslationReq time)) <&> tResTimestamp

  kEvaluateExUnits :: IsTxBuilderEra era => Tx era -> Kontract RemoteKuberConnection w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
  kEvaluateExUnits tx = liftHttpReq (cEvaluateExUnits (TxModal $ InAnyCardanoEra bCardanoEra tx)) <&> unWrap

  --   kEvaluateExUnits' ::    TxBody ConwayEra -> UTxO ConwayEra -> Kontract a  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))

  kCalculateMinFee :: IsTxBuilderEra era => Tx era -> Kontract RemoteKuberConnection w FrameworkError Lovelace
  kCalculateMinFee tx = liftHttpReq (cCalculateFee (TxModal $ InAnyCardanoEra bCardanoEra tx))

  --   kCalculateMinFee' :: TxBody ConwayEra ->  ShelleyWitCount ->  ByronWitCount-> Kontract a  w FrameworkError  Lovelace
  kBuildAndSubmit :: IsTxBuilderEra era => TxBuilder_ era -> Kontract RemoteKuberConnection w FrameworkError (Tx era)
  kBuildAndSubmit builder = kError FeatureNotSupported "BuildTx Over Http is not supported"

-- TODO FIX THIS
--liftHttpReq (cBuildTx Nothing builder ) <&> unWrap

instance HasSubmitApi RemoteKuberConnection where
  kSubmitTx :: InAnyCardanoEra Tx -> Kontract RemoteKuberConnection w FrameworkError ()
  kSubmitTx tx = liftHttpReq (cSubmitTx (SubmitTxModal tx Nothing)) $> ()

liftHttpReq q = KLift $ \(RemoteKuberConnection net c) -> runClientM q c <&> mapClientError

mapClientError = \case
  Left (e :: ClientError) ->
    Left
      ( case e of
          FailureResponse rf rf' -> case decode @FrameworkError (responseBody rf') of
            Nothing -> FrameworkError LibraryError $ "Server Responded with failure [" ++ (case responseStatusCode rf' of Status n bs -> show n ++ " " ++ BS.unpack bs) ++ "] " ++ BSL8.unpack (responseBody rf')
            Just fe -> fe
          DecodeFailure txt rf -> FrameworkError LibraryError "Failed to Decode response from Kuber Backend Server"
          UnsupportedContentType mt rf -> FrameworkError LibraryError "Kuber Backend server responded with Unknown Content Type"
          InvalidContentTypeHeader rf -> FrameworkError LibraryError $ "Kuber Backend server responded with invalid contentType header  " ++ show (responseHeaders rf)
          Servant.Client.ConnectionError se -> case fromException se of
            Nothing -> FrameworkError Cardano.Kuber.Error.ConnectionError (show se)
            Just ex -> mapHttpException ex
      )
  Right v -> pure v
  where
    mapHttpException htException = case htException of
      Network.HTTP.Client.HttpExceptionRequest re hec -> mapHttpExceptionContent re hec
      Network.HTTP.Client.InvalidUrlException url reason -> FrameworkError ParserError $ "InvalidUrl url=" ++ url ++ " reason=" ++ reason
    mapHttpExceptionContent re hec = case hec of
      Network.HTTP.Client.StatusCodeException re' bs -> FrameworkError LibraryError $ "Server Responsed with failure [" ++ (case Network.HTTP.Client.responseStatus re' of Status n bs -> show n ++ " " ++ BS.unpack bs) ++ "] " -- ++ BSL8.unpack (responseBody re)
      Network.HTTP.Client.TooManyRedirects res -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Too Many Redirects"
      Network.HTTP.Client.OverlongHeaders -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Responded with veryLarge header"
      Network.HTTP.Client.ResponseTimeout -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Response Timeout"
      Network.HTTP.Client.ConnectionTimeout -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Server [" ++ authority ++ "] Connection Timeout"
      Network.HTTP.Client.ConnectionFailure se' -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Server [" ++ authority ++ "] ConnectionFailure " ++ show se'
      Network.HTTP.Client.InvalidStatusLine bs -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : Invalid Status Line in response : " ++ BS8.unpack bs
      Network.HTTP.Client.InvalidHeader bs -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : Responded with invalid Headers " ++ BS8.unpack bs
      Network.HTTP.Client.InvalidRequestHeader bs -> FrameworkError Cardano.Kuber.Error.ParserError $ "Invalid Request Header :" ++ BS8.unpack bs
      Network.HTTP.Client.InternalException se' -> FrameworkError LibraryError $ "Unknown Interlnal Error :" ++ show se'
      Network.HTTP.Client.ProxyConnectException bs n sta -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Server Connection via Proxy Failed [" ++ show n ++ "] :" ++ BS8.unpack bs
      Network.HTTP.Client.NoResponseDataReceived -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : No Response Data Received "
      Network.HTTP.Client.TlsNotSupported -> FrameworkError LibraryError $ "TlsNotSupported : HTTPS connection is not supported in http-client"
      Network.HTTP.Client.WrongRequestBodyStreamSize wo wo' -> FrameworkError LibraryError $ "WrongRequestBodyStreamSize expected=" ++ show wo ++ "got=" ++ show wo'
      Network.HTTP.Client.ResponseBodyTooShort wo wo' -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server :  Response Body Too Short expected=" ++ show wo ++ "got=" ++ show wo'
      Network.HTTP.Client.InvalidChunkHeaders -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : Responded with invalid Chunk Headers "
      Network.HTTP.Client.IncompleteHeaders -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Responsed with incomplete Headers"
      Network.HTTP.Client.InvalidDestinationHost bs -> FrameworkError LibraryError $ "Invalid Destination host :" ++ BS8.unpack bs
      Network.HTTP.Client.HttpZlibException ze -> FrameworkError LibraryError $ "Error reading response HttpZlibException : " ++ show ze
      Network.HTTP.Client.InvalidProxyEnvironmentVariable txt txt' -> FrameworkError LibraryError $ "SetupError InvalidProxyEnvironmentVariable expected=" ++ T.unpack txt ++ "got=" ++ T.unpack txt'
      Network.HTTP.Client.ConnectionClosed -> FrameworkError Cardano.Kuber.Error.ConnectionError "KuberBackend Connection was Closed"
      Network.HTTP.Client.InvalidProxySettings txt -> FrameworkError LibraryError $ "SetupError InvalidProxySettings expected=" ++ T.unpack txt
      where
        uri = Network.HTTP.Client.getUri re
        scheme = uriScheme uri
        authority = case uriAuthority uri of
          Nothing -> show uri
          Just ua -> (if null scheme then "" else scheme ++ "://") ++ uriRegName ua ++ uriPort ua

type UrlString = String

type ApiKeyString = String

createRemoteKuberConnection :: NetworkId -> UrlString -> Maybe ApiKeyString -> IO RemoteKuberConnection
createRemoteKuberConnection net urlStr apiKey = do
  url <- baseUrl urlStr
  createRemoteKuberConnection' net url apiKey

createRemoteKuberConnection' :: NetworkId -> BaseUrl -> Maybe ApiKeyString -> IO RemoteKuberConnection
createRemoteKuberConnection' net baseUrl apiKey = do
  managerSettings <- case baseUrlScheme baseUrl of
    Http -> case apiKey of
      Nothing -> pure Network.HTTP.Client.defaultManagerSettings
      Just s -> putStrLn "[Connection-Manager] Warn: Api-Key won't be not be added over http connection" $> Network.HTTP.Client.defaultManagerSettings
    Https -> pure managerSettings
  manager' <- Network.HTTP.Client.newManager managerSettings
  pure $ RemoteKuberConnection net (mkClientEnv manager' baseUrl)
  where
    managerSettings = case apiKey of
      Nothing -> tlsManagerSettings
      Just s ->
        tlsManagerSettings
          { managerModifyRequest = addApiKeyHeader s
          }
    addApiKeyHeader key req = pure $ req {requestHeaders = ("api-key", BS.pack key) : Network.HTTP.Client.requestHeaders req}

baseUrl :: MonadFail m => String -> m BaseUrl
baseUrl urlString = do
  -- Parse the URL string to create a URI
  uri <- case parseURI urlString of
    Nothing -> fail "Invalid URL"
    Just u -> return u

  -- Create a BaseUrl from the URI
  scheme <- case uriScheme uri of
    "https:" -> pure Https
    "http:" -> pure Http
    scheme -> fail $ "Invalid URL scheme " ++ scheme

  host <- case uriAuthority uri of
    Just auth -> pure $ uriRegName auth
    Nothing -> fail "Invalid URL"
  port <- case uriAuthority uri of
    Just auth -> case uriPort auth of
      "" -> case uriScheme uri of
        "https:" -> pure 443
        "http:" -> pure 80
        scheme -> fail $ "Invalid URL scheme " ++ scheme
      _ -> maybe (fail "Invalid port number") pure (readMaybe (tail (uriPort auth)))
    Nothing -> fail "Invalid URL"

  return
    BaseUrl
      { baseUrlScheme = scheme,
        baseUrlHost = host,
        baseUrlPort = port,
        baseUrlPath = dropWhile (== '/') (uriPath uri)
      }