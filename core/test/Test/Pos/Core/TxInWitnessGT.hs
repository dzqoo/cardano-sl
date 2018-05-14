module Test.Pos.Core.TxInWitnessGT
       ( generatePublicKey
       , testPublicKey
       , parsedPublicKey
       )where

import           Universum
--import           Test.Tasty (defaultMain, TestTree, testGroup)
--import           Test.Tasty.Golden (findByExtension, goldenVsFile)
import           System.FilePath (takeBaseName)
import           Pos.Binary.Core.Txp
import           Pos.Core.Txp
import           Test.Pos.Crypto.Arbitrary (genSignatureEncoded)
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
generatePublicKey :: IO Text
generatePublicKey = toLazyText . formatFullPublicKey <$> generate (arbitrary :: Gen PublicKey)

generateTxSig =
testPublicKey :: Text
testPublicKey = "s6xMQZD0xKcBuOw2+OyMUporuSLMLi99mU3A6/9cRBrO/ekTq8oBbS7yf5OgbYg58HzO8ASRpzuaca8hED08VQ=="

parsedPublicKey :: PublicKey
parsedPublicKey = case parsedPublicKey testPublicKey of
    Right publicKey ->publicKey


generateGoldenFiles :: IO ()
generateGoldenFiles = do
    encode PkWitness {twKey =  testPublicKey
                         ,twSig = }