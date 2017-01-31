{-# LANGUAGE TypeSynonymInstances, DeriveAnyClass, DeriveGeneric #-}
module PromissoryNote.Types.Misc
(
    module PromissoryNote.Types.Misc
,   UTCTime
,   Pay.SendPubKey
,   Pay.BitcoinAmount
)
where

import Data.Word                (Word64)
import Data.Time                (UTCTime)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Text.Encoding       (encodeUtf8, decodeUtf8)
import Data.Aeson (FromJSON, ToJSON)

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Node   as HN
import qualified Data.Serialize         as Bin
import qualified Data.Serialize.Put     as BinPut
import qualified Data.Serialize.Get     as BinGet
import qualified Data.Bitcoin.PaymentChannel as Pay
import           GHC.Generics


type PubKey = HC.PubKey
type BitcoinAddress = HC.Address
type Amount = Pay.BitcoinAmount

type StringIdentifier = T.Text

data Currency = BTC
    deriving (Show, Generic, ToJSON, FromJSON, Bin.Serialize)

-- Length-prefixed (uint8) UTF-8 string (max string length: 255 bytes)
instance Bin.Serialize StringIdentifier where
    put txt = Bin.putWord8 (fromIntegral $ BS.length txtBS) >> Bin.putByteString txtBS
        where txtBS = encodeUtf8 txt :: BS.ByteString
    get = Bin.getWord8 >>= \l -> decodeUtf8 <$> Bin.getByteString (fromIntegral l)

instance Bin.Serialize UTCTime where
    put t = BinPut.putWord64be (round $ utcTimeToPOSIXSeconds t * 1e9)
    get = posixSecondsToUTCTime . (/ 1e9) . fromIntegral <$> BinGet.getWord64be
