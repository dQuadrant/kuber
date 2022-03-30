-- import Cardano.Api (ExecutionUnits (ExecutionUnits), ScriptData (ScriptDataNumber))
-- import Cardano.Contrib.Easy.Context
-- import Cardano.Contrib.Easy.Error
-- import Cardano.Contrib.Easy.Parsers (createMintingScriptWitness, parseAnyScript)
-- import Cardano.Contrib.Easy.Util (queryUtxos)
-- import Data.Text as T

-- -- getAddrAnyFromEra addrEra = unMaybe (SomeError "unexpected error converting address to another type") (deserialiseAddress AsAddressAny (serialiseAddress  addrEra))

main = do
--   -- ps <- parseAnyScript $ T.pack "{\
--   -- \\"type\": \"PlutusScriptV1\",\
--   -- \\"description\": \"\",\
--   -- \\"cborHex\": \"4e4d01000033222220051200120011\"\
--   -- \}"
--   -- pscriptWitness <- createTxInScriptWitness AlonzoEra ps
--   -- print $ show pscriptWitness

--   let requiredMemory = 700000000
--       requiredSteps = 700000000

--   ss <- parseAnyScript $ T.pack "{\"type\":\"all\",\"scripts\":[{\"keyHash\":\"ecfe3f453fe6d6048d53095c931f59e02bb504968b7beff3f8c010de\",\"type\":\"sig\"}]}"

--   sscriptWitness <- createMintingScriptWitness ss (ScriptDataNumber 1) (ExecutionUnits requiredMemory requiredSteps)

--   print $ show sscriptWitness

--   -- minCtx <- getDefaultTestnetContext
--   -- ctx <- toNetworkContext minCtx
--   -- let rawAddr = "addr_test1vrk0u0698lndvpyd2vy4eyclt8szhdgyj69hhmlnlrqpphssqgntw"
--   -- addrInEra <- case deserialiseAddress (AsAddressInEra AsAlonzoEra) (T.pack rawAddr) of
--   --     Nothing -> fail "Cannot deserialise Address"
--   --     Just aie -> return aie

--   -- addrAny <- getAddrAnyFromEra addrInEra

--   -- utxos <- queryUtxos (networkCtxConn ctx) addrAny
--   -- putStrLn $ toConsoleText  "  "  utxos

  print "ok"