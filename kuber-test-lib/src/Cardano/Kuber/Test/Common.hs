module Cardano.Kuber.Test.Common where
import Cardano.Kuber.Api (FrameworkError)
import Hedgehog (Property)
import Cardano.Kuber.Api
import Cardano.Kuber.Test.KuberTestnet
import Control.Monad.IO.Class (liftIO)



-- makeProperty :: KuberTestnet -> Kontract KuberTestnet w FrameworkError r -> Property
-- makeProperty testnet k =  liftIO $  do
--         evaluateKontract  testnet k
--         pure ()