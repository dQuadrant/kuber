module Cardano.Kuber.Api(
    
    -- * `TxBuilder` and companion functions

    -- |Transaction Builder Object
         TxBuilder

    -- *** Transaction Inputs
    -- | Add the txin to  transaction input
    ,   txConsumeTxIn
    ,   txConsumeUtxo
    ,   txConsumeUtxos
    ,   txRedeemTxin
    ,   txRedeemUtxo
    ,   txRedeemUtxoWithDatum
    ,   txRedeemUtxoWithReferenceScript
    ,   txRedeemTxinWithReferenceScript
    ,   txRedeemUtxoWithDatumAndReferenceScript
    ,   txRedeemTxinWithDatumAndReferenceScript


    -- $ Transaction Inputs

    -- *** `TxSelection`s 
    -- | `TxSelection`s represent Utxos that can be used for balancing a transaction. They will be added to input if required for balancing the transaciton.
    -- Such utxos should be related to user's wallet and

    ,   txWalletAddress
    ,   txWalletAddresses
    ,   txWalletUtxos
    ,   txWalletUtxo

    ,   txWalletSignKey
    ,   txWalletSignKeys


   -- *** Payment functions
    ,    txPayTo
    
    ,   txPayToPkh
    
    ,   txPayToScript
    
    ,   txPayToScriptWithData 
    
    -- ,   txPayToScriptWithReference
    
    -- ,   txPayToScriptWithDataAndReference
   
    -- ,   txPayToWithReferenceScript
   -- $ Payment functions

    -- *** Minting functions
    ,   txMintSimpleScript
    ,   txMintPlutusScript



    -- *** Reference Inputs
    ,   txReferenceTxIn
    ,   txReferenctUtxo


    -- *** Extra Vkey Witnesses in Transaction
    ,   txSignBy
    ,   txSignByPkh
    ,   txSign

    -- | Explicitly set transaction fee
    ,   txSetFee 

    -- | Change address to send unbalanced value to. If change address is missing, it is automatically selected from one of the wallet addresses.
    , txChangeAddress

    -- *** Transactoin validity functions
    , txValidFromPosixTime
    , txValidUntilPosixTime
    , txValidPosixTimeRange
    , txValidFromSlot
    , txValidUntilSlot
    , txValidSlotRange

    -- *** Transactoin validity functions
    -- | these functions help dealing with plutus validator and cardano-api script types.
    , IsPlutusScript
    , IsSimpleScript
    , TxScript(..)
    , TxPlutusScript(..)
    , TxSimpleScript(..)
    , toTxSimpleScript
    , toTxPlutusScript
    , hashPlutusScript
    , plutusScriptAddr
    , plutusScriptToScriptAny
    , txScriptPolicyId
    , txScriptAddress
    , txScriptHash
    , hashTxScript
    , txScriptToScriptAny

    -- ** Error Class
    ,   ErrorType(..)
    ,   FrameworkError(..)
    ,  throwFrameworkError

    -- * Kontract  and ChainApi related API    
    , Kontract(KLift, KError,KResult)
    , kError
    , kWrapParser
    , kGetBackend
    , evaluateKontract
    , eitherToKontract
    , HasChainQueryAPI(..)
    , HasSubmitApi(..)
    , HasKuberAPI(..)
    , HasLocalNodeAPI(..)

    -- * Chain info Structures
    ,   ChainConnectInfo(..)
    ,   RemoteKuberConnection
    ,   createRemoteKuberConnection
    ,   createRemoteKuberConnection'
    ,   chainInfoFromEnv
    ,   chainInfoFromEnv'
    ,   chainInfoMainnet
    ,   chainInfoTestnet
    ,   localNodeConnInfo
    ,   getWorkPath
    ,   getWorkPathFunc
    ,   getNetworkFromEnv
)

where

import Cardano.Kuber.Utility.WalletUtil
import Cardano.Kuber.Core.ChainInfo
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Core.TxBuilder
import Cardano.Kuber.Core.TxFramework
import Cardano.Kuber.Data.TxBuilderAeson
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Utility.ChainInfoUtil
import Cardano.Kuber.Error
import Cardano.Kuber.Utility.QueryHelper
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Core.ChainAPI
import Cardano.Kuber.Core.KuberAPI
import Cardano.Kuber.Core.LocalNodeChainApi
import Cardano.Kuber.Http.Client
