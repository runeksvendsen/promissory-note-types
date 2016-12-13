{-# LANGUAGE TypeSynonymInstances #-}
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

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize         as Bin
import qualified Data.Serialize.Put     as BinPut
import qualified Data.Serialize.Get     as BinGet
import qualified Data.Bitcoin.PaymentChannel as Pay



type PubKey = HC.PubKey
type BitcoinAddress = HC.Address
type Amount = Pay.BitcoinAmount

type StringIdentifier = T.Text
type Currency = ()

instance Bin.Serialize StringIdentifier where
    put txt = Bin.put (encodeUtf8 txt :: BS.ByteString)
    get = decodeUtf8 <$> (Bin.get :: Bin.Get BS.ByteString)

instance Bin.Serialize UTCTime where
    put t = BinPut.putWord64be (round $ utcTimeToPOSIXSeconds t)
    get = posixSecondsToUTCTime . fromIntegral <$> BinGet.getWord64be
