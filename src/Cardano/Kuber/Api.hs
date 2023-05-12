module Cardano.Kuber.Api(
    
    -- * `TxBuilder` and companion functions

    -- |Transaction Builder Object
         TxBuilder

    -- *** Transaction Inputs
    -- | Add the txin to  transaction input
    ,   txConsumeTxIn
    ,   txConsumeUtxo
    ,   txConsumeUtxos
    -- | Add a script utxo containing inline-datum to transaction input.  Script code and Reedemer  should be passed.
    ,   txRedeemTxin
    -- | Add a  script utxo containing inline-datum  to  transaction input. Script code and redeemer should be  passed for building transaction.
    ,   txRedeemUtxo
    -- | Add a  script utxo containing datum-hash to  transaction input . Script code, datum matching datumHash and redeemer should be  passed for building transaction.
    , txRedeemUtxoWithDatum
    -- | Add a script utxo containing inline-datum to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.
    ,   txRedeemUtxoWithReferenceScript
    -- | Add a script utxo containing datum-hash to transaction input. Script code is inlined in provided TransactionInput. The script reference input will be automatically added to transaction reference inputs.

    ,   txRedeemUtxoWithDatumAndReferenceScript


    -- $ Transaction Inputs

    -- *** `TxSelection`s 
    -- | `TxSelection`s represent Utxos that can be used for balancing a transaction. They will be added to input if required for balancing the transaciton.
    -- Such utxos should be related to user's wallet and

    -- | add all current utxos of the address to selection
    ,   txWalletAddress
    ,   txWalletAddresses
    ,   txWalletUtxos
    ,   txWalletUtxo
    -- | add signkey to the selection. All the utxos from the enterprise address of the signkey will be automatically
    -- added to selection too. The `TxBuilder` containing signkeys when built to transaction, transaction will contain signatures if their utxo is used.
    -- 
    -- In order to automatically sign tx for address other than enterprise address, both address and signkey must be present in the builder.
    ,   txWalletSignKey
    ,   txWalletSignKeys


   -- *** Payment functions
    -- | Pay to this address in transaction
    ,    txPayTo
    -- | Pay to the enterprise address of this PublicKeyHash
    ,   txPayToPkh
    -- | Pay to script address with datumHash
    ,   txPayToScript
    -- | Pay to script address and inline the datum in utxo
    ,   txPayToScriptWithData 
    -- | Pay to the script and inline it in the utxo. Script enterprise address is derrived from script hash
    ,   txPayToScriptWithReference
    -- | Pay to script  with inline both datum and inline it in datum. Script enterprise address is derrived from script hash
    ,   txPayToScriptWithDataAndReference
    -- | Pay to address  and inline the script in resulting utxo.
    ,   txPayToWithReferenceScript
   -- $ Payment functions

    -- *** Minting functions
    -- | Mint token with simple script
    ,   txMintSimpleScript
    -- | Mint token with plutus v1 or v2 script
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




    -- Chain info Structures
    ,   ChainConnectInfo(..)
    ,   RemoteKuberConnection
    ,   createRemoeKuberConnection
    ,   createRemoeKuberConnection'
    ,   chainInfoFromEnv
    ,   chainInfoFromEnv'
    ,   chainInfoMainnet
    ,   chainInfoTestnet
    ,   localNodeConnInfo
    ,   getWorkPath
    ,   getWorkPathFunc
    ,   getNetworkFromEnv

    -- ** Error Class
    ,   ErrorType(..)
    ,   FrameworkError(..)
    ,  throwFrameworkError

    -- ** Kontract  and ChainApi related API    
    , Kontract(KLift, KError)
    , kError
    , kWrapParser
    , evaluateKontract
    , eitherToKontract
    , HasChainQueryAPI(..)
    , HasSubmitApi(..)
    , HasKuberAPI(..)
    , HasLocalNodeAPI(..)
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
