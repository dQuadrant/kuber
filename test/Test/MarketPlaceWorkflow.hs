{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Test.MarketPlaceWorkflow where
import Cardano.Kuber.Api
import Data.Functor ((<&>))
import Cardano.Kuber.Data.Parsers (parseSignKey)
import System.Environment (getEnv)
import qualified Data.Text as T
import Cardano.Api
import Cardano.Kuber.Util (skeyToAddrInEra)
import qualified Data.ByteString.Char8 as BS8
import Control.Exception (throw)
import qualified Data.Text.IO as T

main= do
    info <- chainInfoFromEnv >>= withDetails
    signKey <- getEnv "SIGN_KEY_PATH" >>= T.readFile >>= parseSignKey  
     

    let script = RequireSignature  @SimpleScriptV2 vkey 
        mintOp = txMintSimpleScript script  [(AssetName $ BS8.pack "one", 1)]
                    <> txWalletSignKey signKey
        network= getNetworkId info
        vkey = verificationKeyHash $ getVerificationKey  signKey
        policy= scriptPolicyId $  SimpleScript SimpleScriptV2 script
        
    tx <- txBuilderToTxIO info mintOp  >>= orThrow
    submitTx  (getConnectInfo  info) tx >>= orThrow
    putStrLn $ "[TxSubmit][Mint]  "++ binaryToString policy ++".one   tx : " ++ BS8.unpack (  serialiseToRawBytesHex $  getTxId $ getTxBody  tx)
    
    where 
        throOnError x = do 
            v<-x 
            case v of 
                Right v -> pure v
                Left e -> throw e
        orThrow v = do 
            case v of 
                Right v -> pure v
                Left e -> throw e
        binaryToString v = BS8.unpack (  serialiseToRawBytesHex $ v)


        --waitConfirmation txId  
    