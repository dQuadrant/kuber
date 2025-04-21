-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Websocket.Aeson where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)


-- import GHC.Generics
-- import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)
-- import Data.ByteString.Lazy qualified as LBS
-- import Control.Lens ((.~))
-- import Data.Aeson (Value (..), defaultOptions, encode, genericParseJSON, genericToJSON, omitNothingFields, withObject, (.:))
-- import Data.Aeson.KeyMap qualified as KeyMap
-- import Data.Aeson.Lens (atKey, key)
-- import GHC.Natural
-- import Data.Time
-- import Test.QuickCheck.Arbitrary
-- import Test.QuickCheck.Gen
-- import PlutusLedgerApi.V3 (fromBuiltin)
-- import Formatting.Buildable (build)
-- -- import Cardano.Api.UTxO qualified as UTxO
-- import Cardano.Ledger.Shelley.UTxO qualified as Ledger
-- import Codec.CBOR.Decoding qualified as CBOR
-- import Codec.CBOR.Encoding qualified as CBOR
-- import Data.Aeson (FromJSONKey, ToJSONKey, (.:), (.:?))
-- import Data.Aeson qualified as Aeson
-- import Data.Aeson.KeyMap qualified as KeyMap
-- import Data.Aeson.Types (withObject)
-- import Data.Text.Lazy.Builder (toLazyText)
-- import Data.Data
-- import Data.Aeson.Types
-- import Data.ByteString
-- import Cardano.Api
-- import Cardano.Binary
-- import Cardano.Api.Experimental
-- import Data.Text
-- import Data.String
-- import Network.Socket (PortNumber, close)

data WSMessage = WSMessage
  { tag :: T.Text,
    seq :: Maybe Int,
    timestamp :: UTCTime
  }
  deriving (Show)

instance FromJSON WSMessage where
  parseJSON = withObject "WSMessage" $ \v -> do
    tagVal <- v .: "tag"
    seqVal <- v Aeson..:? "seq"
    tsVal <- v .: "timestamp"
    ts <- iso8601ParseM tsVal
    return $ WSMessage tagVal seqVal ts

-- class
--   ( Eq tx
--   , Show tx
--   , Typeable tx
--   , FromCBOR tx
--   , ToCBOR tx
--   , FromJSON tx
--   , ToJSON tx
--   , --
--     Eq (TxIdType tx)
--   , Ord (TxIdType tx)
--   , Show (TxIdType tx)
--   , Typeable (TxIdType tx)
--   , FromJSON (TxIdType tx)
--   , ToJSON (TxIdType tx)
--   , FromCBOR (TxIdType tx)
--   , ToCBOR (TxIdType tx)
--   , FromJSONKey (TxIdType tx)
--   , ToJSONKey (TxIdType tx)
--   , --
--     Eq (TxOutType tx)
--   , Show (TxOutType tx)
--   , ToJSON (TxOutType tx)
--   , FromJSON (TxOutType tx)
--   , --
--     Eq (UTxOType tx)
--   , Show (UTxOType tx)
--   , Monoid (UTxOType tx)
--   , FromJSON (UTxOType tx)
--   , ToJSON (UTxOType tx)
--   , FromCBOR (UTxOType tx)
--   , ToCBOR (UTxOType tx)
--   ) =>
--   IsTx tx
--   where
--   -- | Type which identifies a transaction
--   type TxIdType tx

--   -- | Type for individual transaction outputs.
--   type TxOutType tx = out | out -> tx

--   -- | Type for a set of unspent transaction outputs.
--   type UTxOType tx = utxo | utxo -> tx

--   -- | Type representing a value on the ledger.
--   type ValueType tx

--   -- XXX(SN): this name easily conflicts
--   txId :: tx -> TxIdType tx

--   -- XXX: Is this even used?
--   balance :: UTxOType tx -> ValueType tx

--   -- | Hash a utxo set to be able to sign (off-chain) and verify it (on-chain).
--   hashUTxO :: UTxOType tx -> ByteString

--   txSpendingUTxO :: UTxOType tx -> tx

--   -- | Get the UTxO produced by given transaction.
--   utxoFromTx :: tx -> UTxOType tx

--   -- | Get only the outputs in given UTxO.
--   outputsOfUTxO :: UTxOType tx -> [TxOutType tx]

--   -- | Return the left-hand side without the right-hand side.
--   withoutUTxO :: UTxOType tx -> UTxOType tx -> UTxOType tx

-- -- * Constraint synonyms

-- type ArbitraryIsTx tx =
--   ( IsTx tx
--   , Arbitrary tx
--   , Arbitrary (UTxOType tx)
--   , Arbitrary (TxIdType tx)
--   , Arbitrary (TxOutType tx)
--   )

-- instance FromJSON Tx where
--   parseJSON =
--     withObject "Tx" $ \o -> do
--       hexText <- o .: "cborHex"
--       -- NOTE: We deliberately ingore the "type" to be backwards compatible
--       bytes <- decodeBase16 hexText
--       case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
--         Left e -> fail $ show e
--         Right tx -> do
--           -- NOTE: Check txId equivalence only if present.
--           (o .:? "txId") >>= \case
--             Just txid'
--               | txid' /= txId tx -> fail "txId not matching"
--             _ -> pure tx

-- -- XXX: Double CBOR encoding?
-- instance FromCBOR Tx where
--   fromCBOR = do
--     bs <- CBOR.decodeBytes
--     decodeFullAnnotator ledgerEraVersion "Tx" decCBOR (fromStrict bs)
--       & either
--         (fail . toString . toLazyText . build)
--         (pure . fromLedgerTx)

-- instance ToCBOR UTxO where
--   toCBOR = toCBOR . toLedgerUTxO
--   encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

-- instance FromCBOR UTxO where
--   fromCBOR = fromLedgerUTxO <$> fromCBOR
--   label _ = label (Proxy @(Ledger.UTxO LedgerEra))

-- -- | The type of messages sent to clients by the 'Hydra.API.Server'.
-- data TimedServerOutput tx = TimedServerOutput
--   { output :: ServerOutput tx
--   , seq :: Natural
--   , time :: UTCTime
--   }
--   deriving stock (Eq, Show, Generic)

-- instance Arbitrary (ServerOutput tx) => Arbitrary (TimedServerOutput tx) where
--   arbitrary = genericArbitrary

-- -- | Generate a random timed server output given a normal server output.
-- genTimedServerOutput :: ServerOutput tx -> Gen (TimedServerOutput tx)
-- genTimedServerOutput o =
--   TimedServerOutput o <$> arbitrary <*> arbitrary

-- data ValidationResult
--   = Valid
--   | Invalid ValidationError
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)

-- newtype ValidationError = ValidationError {reason :: Text}
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)

-- instance Arbitrary ValidationError where
--   arbitrary = genericArbitrary

-- data DecommitInvalidReason tx
--   = DecommitTxInvalid {localUTxO :: UTxOType tx, validationError :: ValidationError}
--   | DecommitAlreadyInFlight {otherDecommitTxId :: TxIdType tx}
--   deriving stock (Generic)

-- deriving stock instance (Eq (TxIdType tx), Eq (UTxOType tx)) => Eq (DecommitInvalidReason tx)
-- deriving stock instance (Show (TxIdType tx), Show (UTxOType tx)) => Show (DecommitInvalidReason tx)

-- instance (ToJSON (TxIdType tx), ToJSON (UTxOType tx)) => ToJSON (DecommitInvalidReason tx) where
--   toJSON = genericToJSON defaultOptions

-- instance (FromJSON (TxIdType tx), FromJSON (UTxOType tx)) => FromJSON (DecommitInvalidReason tx) where
--   parseJSON = genericParseJSON defaultOptions

-- instance ArbitraryIsTx tx => Arbitrary (DecommitInvalidReason tx) where
--   arbitrary = genericArbitrary

-- -- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- -- the 'ClientEffect'.
-- newtype NodeId = NodeId {nodeId :: Text}
--   deriving newtype (Eq, Show, IsString, Read, Ord, ToJSON, FromJSON)

-- data Host = Host
--   { hostname :: Text
--   , port :: PortNumber
--   }
--   deriving stock (Ord, Generic, Eq)
--   deriving anyclass (ToJSON, FromJSON)

-- newtype HeadId = UnsafeHeadId ByteString
--   deriving stock (Show, Eq, Ord, Generic)
--   deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadId)
--   deriving newtype (FromCBOR, ToCBOR)

-- instance SerialiseAsRawBytes HeadId where
--   serialiseToRawBytes (UnsafeHeadId bytes) = bytes
--   deserialiseFromRawBytes _ = Right . UnsafeHeadId

-- instance HasTypeProxy HeadId where
--   data AsType HeadId = AsHeadId
--   proxyToAsType _ = AsHeadId

-- instance Arbitrary HeadId where
--   arbitrary = UnsafeHeadId . BS.pack <$> vectorOf 16 arbitrary

-- data HydraKey

-- newtype Party = Party {vkey :: VerificationKey HydraKey}
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
--   deriving newtype (Arbitrary)

-- instance ToJSONKey Party where
--   toJSONKey = toJSONKeyText (serialiseToRawBytesHexText . vkey)

-- instance FromJSONKey Party where
--   fromJSONKey = FromJSONKeyTextParser partyFromHexText
--    where
--     partyFromHexText :: MonadFail m => Text -> m Party
--     partyFromHexText t =
--       case deserialiseFromRawBytesHex (AsVerificationKey AsHydraKey) (encodeUtf8 t) of
--         Left err -> fail $ "failed to decode Party: " <> show err
--         Right vkey -> pure $ Party{vkey}

-- -- REVIEW: Do we really want to define Ord or also use unordered-containers
-- -- based on Hashable?
-- instance Ord Party where
--   Party{vkey = a} <= Party{vkey = b} =
--     verificationKeyHash a <= verificationKeyHash b

-- instance FromCBOR Party where
--   fromCBOR = Party <$> fromCBOR

-- instance ToCBOR Party where
--   toCBOR Party{vkey} = toCBOR vkey

-- newtype SnapshotNumber
--   = UnsafeSnapshotNumber Natural
--   deriving stock (Eq, Ord, Generic)
--   deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral, Arbitrary)

-- data ClientInput tx
--   = Init
--   | Abort
--   | NewTx {transaction :: tx}
--   | GetUTxO
--   | Recover {recoverTxId :: TxIdType tx}
--   | Decommit {decommitTx :: tx}
--   | Close
--   | Contest
--   | Fanout
--   deriving stock (Generic)

-- newtype IdleState tx = IdleState {chainState :: ChainStateType tx}
--   deriving stock (Generic)

-- deriving stock instance Eq (ChainStateType tx) => Eq (IdleState tx)
-- deriving stock instance Show (ChainStateType tx) => Show (IdleState tx)
-- deriving anyclass instance ToJSON (ChainStateType tx) => ToJSON (IdleState tx)
-- deriving anyclass instance FromJSON (ChainStateType tx) => FromJSON (IdleState tx)

-- instance Arbitrary (ChainStateType tx) => Arbitrary (IdleState tx) where
--   arbitrary = genericArbitrary

-- data HeadState tx
--   = Idle (IdleState tx)
--   | Initial (InitialState tx)
--   | Open (OpenState tx)
--   | Closed (ClosedState tx)
--   deriving stock (Generic)

-- instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HeadState tx) where
--   arbitrary = genericArbitrary

-- deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (HeadState tx)
-- deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (HeadState tx)
-- deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (HeadState tx)
-- deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (HeadState tx)

-- data ServerOutput tx
--   = PeerConnected {peer :: NodeId}
--   | PeerDisconnected {peer :: NodeId}
--   | PeerHandshakeFailure
--       { remoteHost :: Host
--       , ourVersion :: Natural
--       , theirVersions :: [Natural]
--       }
--   | HeadIsInitializing {headId :: HeadId, parties :: [Party]}
--   | Committed {headId :: HeadId, party :: Party, utxo :: UTxOType tx}
--   | HeadIsOpen {headId :: HeadId, utxo :: UTxOType tx}
--   | HeadIsClosed
--       { headId :: HeadId
--       , snapshotNumber :: SnapshotNumber
--       , contestationDeadline :: UTCTime
--       -- ^ Nominal deadline until which contest can be submitted and after
--       -- which fanout is possible. NOTE: Use this only for informational
--       -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
--       -- as the ledger of our cardano-node might not have progressed
--       -- sufficiently in time yet and we do not re-submit transactions (yet).
--       }
--   | HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber, contestationDeadline :: UTCTime}
--   | ReadyToFanout {headId :: HeadId}
--   | HeadIsAborted {headId :: HeadId, utxo :: UTxOType tx}
--   | HeadIsFinalized {headId :: HeadId, utxo :: UTxOType tx}
--   | CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
--   | -- | Given transaction has been seen as valid in the Head. It is expected to
--     -- eventually be part of a 'SnapshotConfirmed'.
--     TxValid {headId :: HeadId, transactionId :: TxIdType tx, transaction :: tx}
--   | -- | Given transaction was not not applicable to the given UTxO in time and
--     -- has been dropped.
--     TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
--   | -- | Given snapshot was confirmed and included transactions can be
--     -- considered final.
--     SnapshotConfirmed
--       { headId :: HeadId
--       , snapshot :: Snapshot tx
--       , signatures :: MultiSignature (Snapshot tx)
--       }
--   | GetUTxOResponse {headId :: HeadId, utxo :: UTxOType tx}
--   | InvalidInput {reason :: String, input :: Text}
--   | -- | A friendly welcome message which tells a client something about the
--     -- node. Currently used for knowing what signing key the server uses (it
--     -- only knows one), 'HeadStatus' and optionally (if 'HeadIsOpen' or
--     -- 'SnapshotConfirmed' message is emitted) UTxO's present in the Hydra Head.
--     Greetings
--       { me :: Party
--       , headStatus :: HeadStatus
--       , hydraHeadId :: Maybe HeadId
--       , snapshotUtxo :: Maybe (UTxOType tx)
--       , hydraNodeVersion :: String
--       }
--   | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
--   | IgnoredHeadInitializing
--       { headId :: HeadId
--       , contestationPeriod :: ContestationPeriod
--       , parties :: [Party]
--       , participants :: [OnChainId]
--       }
--   | DecommitRequested {headId :: HeadId, decommitTx :: tx, utxoToDecommit :: UTxOType tx}
--   | DecommitInvalid {headId :: HeadId, decommitTx :: tx, decommitInvalidReason :: DecommitInvalidReason tx}
--   | DecommitApproved {headId :: HeadId, decommitTxId :: TxIdType tx, utxoToDecommit :: UTxOType tx}
--   | DecommitFinalized {headId :: HeadId, decommitTxId :: TxIdType tx}
--   | CommitRecorded {headId :: HeadId, utxoToCommit :: UTxOType tx, pendingDeposit :: TxIdType tx, deadline :: UTCTime}
--   | CommitApproved {headId :: HeadId, utxoToCommit :: UTxOType tx}
--   | CommitFinalized {headId :: HeadId, theDeposit :: TxIdType tx}
--   | CommitRecovered {headId :: HeadId, recoveredUTxO :: UTxOType tx, recoveredTxId :: TxIdType tx}
--   | CommitIgnored {headId :: HeadId, depositUTxO :: [UTxOType tx], snapshotUTxO :: Maybe (UTxOType tx)}
--   deriving stock (Generic)

-- deriving stock instance IsChainState tx => Eq (ServerOutput tx)
-- deriving stock instance IsChainState tx => Show (ServerOutput tx)

-- instance IsChainState tx => ToJSON (ServerOutput tx) where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { omitNothingFields = True
--         }

-- instance IsChainState tx => FromJSON (ServerOutput tx) where
--   parseJSON =
--     genericParseJSON
--       defaultOptions
--         { omitNothingFields = True
--         }

-- instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (ServerOutput tx) where
--   arbitrary = genericArbitrary

--   -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
--   -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
--   -- should be only one 'UTxOType tx'
--   shrink = \case
--     PeerConnected p -> PeerConnected <$> shrink p
--     PeerDisconnected p -> PeerDisconnected <$> shrink p
--     PeerHandshakeFailure rh ov tv -> PeerHandshakeFailure <$> shrink rh <*> shrink ov <*> shrink tv
--     HeadIsInitializing headId xs -> HeadIsInitializing <$> shrink headId <*> shrink xs
--     Committed headId p u -> Committed <$> shrink headId <*> shrink p <*> shrink u
--     HeadIsOpen headId u -> HeadIsOpen <$> shrink headId <*> shrink u
--     HeadIsClosed headId s t -> HeadIsClosed <$> shrink headId <*> shrink s <*> shrink t
--     HeadIsContested headId sn dl -> HeadIsContested <$> shrink headId <*> shrink sn <*> shrink dl
--     ReadyToFanout headId -> ReadyToFanout <$> shrink headId
--     HeadIsFinalized headId u -> HeadIsFinalized <$> shrink headId <*> shrink u
--     HeadIsAborted headId u -> HeadIsAborted <$> shrink headId <*> shrink u
--     CommandFailed i s -> CommandFailed <$> shrink i <*> shrink s
--     TxValid headId i tx -> TxValid <$> shrink headId <*> shrink i <*> shrink tx
--     TxInvalid headId u tx err -> TxInvalid <$> shrink headId <*> shrink u <*> shrink tx <*> shrink err
--     SnapshotConfirmed headId s ms -> SnapshotConfirmed <$> shrink headId <*> shrink s <*> shrink ms
--     GetUTxOResponse headId u -> GetUTxOResponse <$> shrink headId <*> shrink u
--     InvalidInput r i -> InvalidInput <$> shrink r <*> shrink i
--     Greetings me headStatus hydraHeadId snapshotUtxo hydraNodeVersion ->
--       Greetings
--         <$> shrink me
--         <*> shrink headStatus
--         <*> shrink hydraHeadId
--         <*> shrink snapshotUtxo
--         <*> shrink hydraNodeVersion
--     PostTxOnChainFailed p e -> PostTxOnChainFailed <$> shrink p <*> shrink e
--     IgnoredHeadInitializing{} -> []
--     DecommitRequested headId txid u -> DecommitRequested headId txid <$> shrink u
--     DecommitInvalid headId decommitTx decommitInvalidReason -> DecommitInvalid headId <$> shrink decommitTx <*> shrink decommitInvalidReason
--     DecommitApproved headId txid u -> DecommitApproved headId txid <$> shrink u
--     DecommitFinalized headId decommitTxId -> DecommitFinalized headId <$> shrink decommitTxId
--     CommitRecorded headId u i d -> CommitRecorded headId <$> shrink u <*> shrink i <*> shrink d
--     CommitApproved headId u -> CommitApproved headId <$> shrink u
--     CommitRecovered headId u rid -> CommitRecovered headId <$> shrink u <*> shrink rid
--     CommitFinalized headId theDeposit -> CommitFinalized headId <$> shrink theDeposit
--     CommitIgnored headId depositUTxO snapshotUTxO -> CommitIgnored headId <$> shrink depositUTxO <*> shrink snapshotUTxO

-- instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (ServerOutput tx)

-- -- | Whether or not to include full UTxO in server outputs.
-- data WithUTxO = WithUTxO | WithoutUTxO
--   deriving stock (Eq, Show)

-- -- | Whether or not to filter transaction server outputs by given address.
-- data WithAddressedTx = WithAddressedTx Text | WithoutAddressedTx
--   deriving stock (Eq, Show)

-- data ServerOutputConfig = ServerOutputConfig
--   { utxoInSnapshot :: WithUTxO
--   , addressInTx :: WithAddressedTx
--   }
--   deriving stock (Eq, Show)

-- -- | Replaces the json encoded tx field with it's cbor representation.
-- --
-- -- NOTE: we deliberately pattern match on all 'ServerOutput' constructors in
-- -- 'handleTxOutput' so that we don't forget to update this function if they
-- -- change.
-- prepareServerOutput ::
--   IsChainState tx =>
--   -- | Decide on tx representation
--   ServerOutputConfig ->
--   -- | Server output
--   TimedServerOutput tx ->
--   -- | Final output
--   LBS.ByteString
-- prepareServerOutput ServerOutputConfig{utxoInSnapshot} response =
--   case output response of
--     PeerConnected{} -> encodedResponse
--     PeerDisconnected{} -> encodedResponse
--     PeerHandshakeFailure{} -> encodedResponse
--     HeadIsInitializing{} -> encodedResponse
--     Committed{} -> encodedResponse
--     HeadIsOpen{} -> encodedResponse
--     HeadIsClosed{} -> encodedResponse
--     HeadIsContested{} -> encodedResponse
--     ReadyToFanout{} -> encodedResponse
--     HeadIsAborted{} -> encodedResponse
--     HeadIsFinalized{} -> encodedResponse
--     CommandFailed{} -> encodedResponse
--     TxValid{} -> encodedResponse
--     TxInvalid{} -> encodedResponse
--     SnapshotConfirmed{} ->
--       handleUtxoInclusion (key "snapshot" . atKey "utxo" .~ Nothing) encodedResponse
--     GetUTxOResponse{} -> encodedResponse
--     InvalidInput{} -> encodedResponse
--     Greetings{} -> encodedResponse
--     PostTxOnChainFailed{} -> encodedResponse
--     IgnoredHeadInitializing{} -> encodedResponse
--     DecommitRequested{} -> encodedResponse
--     DecommitApproved{} -> encodedResponse
--     DecommitFinalized{} -> encodedResponse
--     DecommitInvalid{} -> encodedResponse
--     CommitRecorded{} -> encodedResponse
--     CommitApproved{} -> encodedResponse
--     CommitFinalized{} -> encodedResponse
--     CommitRecovered{} -> encodedResponse
--     CommitIgnored{} -> encodedResponse
--  where
--   handleUtxoInclusion f bs =
--     case utxoInSnapshot of
--       WithUTxO -> bs
--       WithoutUTxO -> bs & f

--   encodedResponse = encode response

-- -- | All possible Hydra states displayed in the API server outputs.
-- -- data HeadStatus
-- --   = Idle
-- --   | Initializing
-- --   | Open
-- --   | Closed
-- --   | FanoutPossible
-- --   | Final
-- --   deriving stock (Eq, Show, Generic)
-- --   deriving anyclass (ToJSON, FromJSON)

-- -- instance Arbitrary HeadStatus where
-- --   arbitrary = genericArbitrary

-- -- | All information needed to distinguish behavior of the commit endpoint.
-- data CommitInfo
--   = CannotCommit
--   | NormalCommit HeadId
--   | IncrementalCommit HeadId
