{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}



module Cardano.Kuber.Http.Client where
import Network.URI (parseURI, URI (uriAuthority, uriScheme, uriPath), URIAuth (uriRegName, uriPort))
import Servant.Client hiding (baseUrl)
import Cardano.Api.Shelley
import Cardano.Kuber.Data.Models
import Data.Text (Text)
import Cardano.Kuber.Core.TxBuilder (TxBuilder)
import Servant.API.Alternative
import Cardano.Kuber.Http.Spec (kuberApiServerProxy, KuberServerApi)
import Cardano.Kuber.Data.TxBuilderAeson
import Cardano.Kuber.Core.ChainAPI (HasChainQueryAPI(..), HasSubmitApi(..))
import Cardano.Kuber.Error
import Cardano.Kuber.Core.Kontract
import Servant.Client.Internal.HttpClient (performRequest)
import Data.Functor ((<&>), ($>))
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Map (Map)
import Cardano.Kuber.Core.KuberAPI (ByronWitCount, ShelleyWitCount, HasKuberAPI(..))
import Cardano.Kuber.Core.ChainInfo (KuberConnectInfo(KuberConnectInfo))
import Network.HTTP.Client (parseUrl, newManager, Request (requestHeaders), ManagerSettings (managerModifyRequest), defaultManagerSettings, HttpExceptionContent (..), HttpException (..), Response (responseStatus ), getUri)
import qualified Data.Text as T
import Data.String (fromString)
import Text.Read (readMaybe)
import Data.Aeson (decode)
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Network.HTTP.Types (Status(Status))
import qualified Data.ByteString.Char8 as BS
import Control.Exception (fromException)
import qualified Network.HTTP.Client  as Network (responseBody)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Char8 as BS8



cQueryPParams         :: ClientM ProtocolParameters
cQueryChainPoint      :: ClientM ChainPointModal
cQueryUtxos           :: [Text]     -> [Text]     -> ClientM UtxoModal
cQuerySystemStart     :: ClientM SystemStartModal
cQueryGenesisParams   :: ClientM GenesisParamModal
cBuildTx              :: Maybe Bool     -> TxBuilder    -> ClientM TxModal
cSubmitTx             :: SubmitTxModal     -> ClientM TxModal
cQueryTime            :: ClientM TranslationResponse
cTimeToSlot           :: TimeTranslationReq     -> ClientM TranslationResponse
cTimeFromSlot         :: SlotTranslationReq     -> ClientM TranslationResponse
cCalculateFee         :: TxModal      -> ClientM Lovelace
cEvaluateExUnits      :: TxModal    -> ClientM ExUnitsResponseModal


(cQueryPParams :<|> cQueryChainPoint :<|> cQueryUtxos
  :<|> cQuerySystemStart :<|> cQueryGenesisParams )
  :<|> (cBuildTx :<|> cSubmitTx :<|> cQueryTime
  :<|> cTimeToSlot :<|> cTimeFromSlot ) :<|> (cCalculateFee
  :<|> cEvaluateExUnits) = client kuberApiServerProxy


data RemoteKuberConnection = RemoteKuberConnection  NetworkId ClientEnv


instance HasChainQueryAPI   RemoteKuberConnection where
  kGetNetworkId = KLift $ \(RemoteKuberConnection net c)  -> pure $ pure net

  kQueryProtocolParams = liftHttpReq cQueryPParams
  kQuerySystemStart = liftHttpReq cQuerySystemStart <&>  unWrap
  kQueryGenesisParams = liftHttpReq cQueryGenesisParams <&>  unWrap
  kQueryUtxoByAddress addrs  = liftHttpReq (cQueryUtxos  (map serialiseAddress  $  Set.toList addrs ) []  <&> unWrap)
  kQueryUtxoByTxin txins = liftHttpReq (cQueryUtxos  [] (map renderTxIn  $  Set.toList txins) <&> unWrap )
  kQueryChainPoint  = liftHttpReq cQueryChainPoint <&> unWrap

instance {-# OVERLAPS #-}  HasKuberAPI RemoteKuberConnection where
  kTxBuildTxBody    :: TxBuilder ->  Kontract RemoteKuberConnection w FrameworkError (TxBody BabbageEra)
  kTxBuildTxBody builder = liftHttpReq (cBuildTx (Just True) builder ) <&> (getTxBody .  unWrap )

  kBuildTx       :: TxBuilder -> Kontract RemoteKuberConnection w FrameworkError (Tx BabbageEra)
  kBuildTx  builder = liftHttpReq (cBuildTx (Just True) builder ) <&> unWrap

  kTimeToSlot           :: POSIXTime -> Kontract RemoteKuberConnection w FrameworkError SlotNo
  kTimeToSlot slot = liftHttpReq ( cTimeToSlot (TimeTranslationReq slot) ) <&> tResSlotNo
  kSlotToTime           ::  SlotNo    -> Kontract RemoteKuberConnection  w FrameworkError POSIXTime
  kSlotToTime time = liftHttpReq ( cTimeFromSlot (SlotTranslationReq time) ) <&> tResTimestamp

  kEvaluateExUnits :: Tx BabbageEra -> Kontract RemoteKuberConnection  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
  kEvaluateExUnits tx = liftHttpReq (cEvaluateExUnits (TxModal tx)) <&> unWrap
--   kEvaluateExUnits' ::    TxBody BabbageEra -> UTxO BabbageEra -> Kontract a  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))

  kCalculateMinFee :: Tx BabbageEra -> Kontract RemoteKuberConnection  w FrameworkError  Lovelace
  kCalculateMinFee tx = liftHttpReq (cCalculateFee (TxModal tx))

--   kCalculateMinFee' :: TxBody BabbageEra ->  ShelleyWitCount ->  ByronWitCount-> Kontract a  w FrameworkError  Lovelace
  kBuildAndSubmit :: TxBuilder -> Kontract RemoteKuberConnection w FrameworkError (Tx BabbageEra)
  kBuildAndSubmit  builder = liftHttpReq (cBuildTx Nothing builder ) <&> unWrap


instance HasSubmitApi RemoteKuberConnection where
    kSubmitTx :: Tx BabbageEra -> Kontract RemoteKuberConnection w FrameworkError ()
    kSubmitTx tx = liftHttpReq (cSubmitTx (SubmitTxModal tx Nothing) ) $> ()


liftHttpReq q =  KLift $ \(RemoteKuberConnection net c)  -> runClientM q c <&> mapClientError

mapClientError = \case
    Left (e ::ClientError) -> Left (case e of
       FailureResponse rf rf' -> case decode @FrameworkError (responseBody rf') of
         Nothing ->  FrameworkError LibraryError   $  "Server Responsed with failure ["++  (case responseStatusCode rf' of { Status n bs -> show n ++ " " ++  BS.unpack bs } ) ++  "] "  ++ BSL8.unpack (responseBody rf')
         Just fe -> fe
       DecodeFailure txt rf -> FrameworkError LibraryError "Failed to Decode response from Kuber Backend Server"
       UnsupportedContentType mt rf -> FrameworkError LibraryError "Kuber Backend server responded with Unknown Content Type"
       InvalidContentTypeHeader rf -> FrameworkError LibraryError  $ "Kuber Backend server responded with invalid contentType header  " ++  show (responseHeaders rf)
       Servant.Client.ConnectionError se -> case fromException  se of
          Nothing -> FrameworkError Cardano.Kuber.Error.ConnectionError (show se)
          Just ex -> mapHttpException  ex
      )
      
    Right v -> pure v
    where
      mapHttpException  htException= case htException of
              HttpExceptionRequest re hec ->  mapHttpExceptionContent re hec
              InvalidUrlException url reason -> FrameworkError ParserError $ "InvalidUrl url=" ++ url ++ " reason=" ++ reason
      mapHttpExceptionContent re hec = case hec of
                StatusCodeException re' bs ->  FrameworkError LibraryError $ "Server Responsed with failure ["++  (case responseStatus re' of { Status n bs -> show n ++ " " ++  BS.unpack bs } ) ++  "] "  -- ++ BSL8.unpack (responseBody re)
                TooManyRedirects res -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Too Many Redirects"
                OverlongHeaders -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Responded with veryLarge header"
                ResponseTimeout -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Response Timeout"
                ConnectionTimeout -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Server ["++ authority ++  "] Connection Timeout"
                ConnectionFailure se' -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Server ["++ authority ++  "] ConnectionFailure " ++ show se'
                InvalidStatusLine bs -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : Invalid Status Line in response : "++BS8.unpack  bs
                InvalidHeader bs -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : Responded with invalid Headers "++BS8.unpack  bs
                InvalidRequestHeader bs -> FrameworkError Cardano.Kuber.Error.ParserError $ "Invalid Request Header :"++BS8.unpack  bs
                InternalException se' -> FrameworkError LibraryError $ "Unknown Interlnal Error :"++show se'
                ProxyConnectException bs n sta -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Server Connection via Proxy Failed [" ++ show n ++"] :" ++BS8.unpack  bs
                NoResponseDataReceived -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : No Response Data Received "
                TlsNotSupported ->  FrameworkError LibraryError $ "TlsNotSupported : HTTPS connection is not supported in http-client"
                WrongRequestBodyStreamSize wo wo' -> FrameworkError LibraryError $ "WrongRequestBodyStreamSize expected="++show wo ++ "got=" ++ show wo'
                ResponseBodyTooShort wo wo' -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server :  Response Body Too Short expected="++show wo ++ "got=" ++ show wo'
                InvalidChunkHeaders -> FrameworkError Cardano.Kuber.Error.ConnectionError $ "Kuber Backend Server : Responded with invalid Chunk Headers "
                IncompleteHeaders -> FrameworkError Cardano.Kuber.Error.ConnectionError "Kuber Backend Server : Responsed with incomplete Headers"
                InvalidDestinationHost bs -> FrameworkError LibraryError $ "Invalid Destination host :"++BS8.unpack  bs
                HttpZlibException ze -> FrameworkError LibraryError $ "Error reading response HttpZlibException : " ++ show ze
                InvalidProxyEnvironmentVariable txt txt' ->  FrameworkError LibraryError $ "SetupError InvalidProxyEnvironmentVariable expected="++T.unpack txt ++ "got="++ T.unpack txt'
                ConnectionClosed -> FrameworkError Cardano.Kuber.Error.ConnectionError "KuberBackend Connection was Closed"
                InvalidProxySettings txt -> FrameworkError LibraryError $ "SetupError InvalidProxySettings expected="++T.unpack txt
            where
              uri = getUri re
              scheme = uriScheme uri
              authority = case uriAuthority uri of
                Nothing -> show uri
                Just ua ->  (if null scheme then "" else scheme ++ "://") ++ uriRegName ua ++ uriPort ua

type UrlString= String
type ApiKeyString = String
createRemoeKuberConnection :: NetworkId -> UrlString -> Maybe ApiKeyString -> IO  RemoteKuberConnection
createRemoeKuberConnection net urlStr  apiKey= do
    url <-  baseUrl urlStr
    createRemoeKuberConnection' net  url apiKey

createRemoeKuberConnection' :: NetworkId -> BaseUrl -> Maybe ApiKeyString -> IO RemoteKuberConnection
createRemoeKuberConnection' net baseUrl apiKey = do
    managerSettings <- case baseUrlScheme baseUrl of
            Http -> case apiKey of
              Nothing -> pure defaultManagerSettings
              Just s -> putStrLn "[Connection-Manager] Warn: Api-Key won't be not be added over http connection" $> defaultManagerSettings
            Https -> pure managerSettings
    manager' <- newManager managerSettings
    pure $ RemoteKuberConnection  net (mkClientEnv manager' baseUrl)

  where
    managerSettings= case apiKey of
      Nothing -> tlsManagerSettings
      Just s -> tlsManagerSettings{
            managerModifyRequest   =  addApiKeyHeader s
          }
    addApiKeyHeader key req  = pure $ req{requestHeaders =("api-key", BS.pack key) : requestHeaders req }


baseUrl :: MonadFail m => String -> m BaseUrl
baseUrl urlString = do
  -- Parse the URL string to create a URI
  uri <- case parseURI urlString of
    Nothing -> fail "Invalid URL"
    Just u -> return u

  -- Create a BaseUrl from the URI
  scheme <-  case uriScheme uri of
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
        scheme -> fail$  "Invalid URL scheme " ++ scheme 
      _ -> maybe (fail "Invalid port number") pure (readMaybe (tail (uriPort auth)))
    Nothing -> fail "Invalid URL"

  return BaseUrl
    { baseUrlScheme = scheme
    , baseUrlHost = host
    , baseUrlPort = port
    , baseUrlPath = dropWhile (== '/') (uriPath uri)
    }