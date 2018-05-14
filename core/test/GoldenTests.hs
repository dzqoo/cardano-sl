module Main where

import           Test.Tasty
import           Test.Pos.Core.TxInWitnessGT

--import qualified Katip.Tests
--import qualified Katip.Tests.Format.Time
--import qualified Katip.Tests.Scribes.Handle


main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Serialization & Deserialization of data types"
    [
     -- Katip.Tests.tests
    --, Katip.Tests.Format.Time.tests
    --, Katip.Tests.Scribes.Handle.tests
    ]