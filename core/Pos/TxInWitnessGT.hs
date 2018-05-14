module Pos.TxInWitnessGT
       ( testPublicKey
       , parsedPublicKey
       , test
       --, generatePublicKey
       )where


import           Universum hiding (fromStrict)

import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Test.Tasty.Golden (goldenVsFile)
import           Data.Text.Lazy.Builder
import           Pos.Crypto
import           Pos.Core.Txp (TxInWitness (..))
import           Test.Pos.Crypto.Arbitrary()
import           Cardano.Crypto.Wallet (XSignature, xsignature)
import           Pos.Binary.Class  -- NB you had to import this and Pos.Binary.Core.Txp to access `encode`
import           Pos.Binary.Core.Txp() -- Bi instances (gives you encode)
import           Data.ByteString as BS (writeFile)
{-
data TxInWitness
    = PkWitness { twKey :: !PublicKey     <-A
                , twSig :: !TxSig }       <-B
-------------------------------------------------------------------------
A
newtype PublicKey = PublicKey CC.XPub
    data XPub = XPub
        { xpubPublicKey :: !ByteString
        , xpubChaincode :: !ChainCode
        } deriving (Generic)

        newtype ChainCode = ChainCode ByteString

B
type TxSig = Signature TxSigData

    data TxSigData = TxSigData
        { txSigTxHash      :: !(Hash Tx) }

        data Tx = UnsafeTx
            { _txInputs     :: !(NonEmpty TxIn)
            , _txOutputs    :: !(NonEmpty TxOut)
            , _txAttributes :: !TxAttributes   }

sum type---> data TxIn = TxInUtxo
                { txInHash  :: !TxId, txInIndex :: !Word32 }
                | TxInUnknown !Word8 !ByteString

            data TxOut = TxOut
                { txOutAddress :: !Address
                , txOutValue   :: !Coin
                } deriving (Eq, Ord, Generic, Show, Typeable)

                    data Address = Address
                        { addrRoot       :: !(AddressHash Address')
                        , addrAttributes :: !(Attributes AddrAttributes)
                        , addrType       :: !AddrType
                        }

                        type AddressHash = AbstractHash Blake2b_224
                        newtype Address' = Address'{ unAddress' :: (AddrType, AddrSpendingData, Attributes AddrAttributes)}

                        data Attributes h = Attributes
                                { attrData   :: h
                                , attrRemain :: UnparsedFields }
                            data AddrAttributes = AddrAttributes
                                { aaPkDerivationPath  :: !(Maybe HDAddressPayload)
                                , aaStakeDistribution :: !AddrStakeDistribution
                                }
                    newtype Coin = Coin
                        { getCoin :: Word64}


-------------------------------------------------------------------------
    | ScriptWitness { twValidator :: !Script
                    , twRedeemer  :: !Script }
    | RedeemWitness { twRedeemKey :: !RedeemPublicKey
                    , twRedeemSig :: !(RedeemSignature TxSigData) }
    | UnknownWitnessType !Word8 !ByteString
    deriving (Eq, Show, Generic, Typeable)
-}

test :: PublicKey -> Builder
test x = formatFullPublicKey x

testPublicKey :: Text
testPublicKey = "s6xMQZD0xKcBuOw2+OyMUporuSLMLi99mU3A6/9cRBrO/ekTq8oBbS7yf5OgbYg58HzO8ASRpzuaca8hED08VQ=="

_signatureByteString :: ByteString
_signatureByteString = "\241\161\249.\EOT.\DEL\136\v3\153c\EOTO\176=\208\177\147}/\235\DC1:\224\161\NAK\251\149\243P\167\251KKb\224\217X\193\GSI[\154\194$&\219\179\242\DLEXK\228\&8\152\250\DC3\FS\\{\163P\r"

_testSignature :: XSignature
_testSignature = case xsignature _signatureByteString of
    Right xsig -> xsig
    Left _ -> error "This should not fail"

parsedPublicKey :: PublicKey
parsedPublicKey = case parseFullPublicKey testPublicKey of
    Right publicKey -> publicKey
    Left _ -> error "This should not fail"

-- | _pkWitness was generated with arbitrary instances, the public key does not correspond to the signature.
_pkWitness :: TxInWitness
_pkWitness = PkWitness parsedPublicKey (Signature _testSignature)

_generateGoldenFile :: IO ()
_generateGoldenFile = BS.writeFile "/Users/Jimbo/Documents/IOHK/cardano-sl/core/Pos/goldenFile.txt"  (toStrictByteString $ encode _pkWitness)

_generateTestFile :: IO ()
_generateTestFile = BS.writeFile "/Users/Jimbo/Documents/IOHK/cardano-sl/core/Pos/PkWitnessTestVal.txt"  (toStrictByteString $ encode _pkWitness)

_generateTestDsFile :: IO ()
_generateTestDsFile = BS.writeFile "/Users/Jimbo/Documents/IOHK/cardano-sl/core/Pos/PkWitnessDsTestVal.txt"  (toStrictByteString $ encode _pkWitness)

_binaryPkWitness :: ByteString
_binaryPkWitness = toStrictByteString $ encode _pkWitness



_main :: IO ()
_main = defaultMain =<< _testSuite

_testSuite :: IO TestTree
_testSuite = do
    return $ testGroup "Serialization of TxInWitness"
        [    goldenVsFile
            "Serialization of PkWitness"-- test name
            "goldenFile.txt" -- golden file path
            "/Users/Jimbo/Documents/IOHK/cardano-sl/core/Pos/PkWitnessTestVal.txt" -- path to output file
            _generateTestFile -- action that creates output file
        ]


