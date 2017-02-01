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
import Data.String.Conversions  (cs)
import Data.Monoid              ((<>))
import Data.Aeson

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize         as Bin
import qualified Data.Serialize.Put     as BinPut
import qualified Data.Serialize.Get     as BinGet
import qualified Data.Bitcoin.PaymentChannel as Pay
import           GHC.Generics
import           Data.Time.Format


type PubKey = HC.PubKey
type BitcoinAddress = HC.Address
type Amount = Pay.BitcoinAmount

type StringId = T.Text

data Currency = BTC
    deriving (Eq, Show, Generic, Bin.Serialize)

instance ToJSON Currency where
    toJSON BTC = String "BTC"

instance FromJSON Currency where
    parseJSON = withText "Currency" $ \txt ->
        case txt of
            "BTC" -> return BTC
            x     -> fail $ "Unknown currency: " ++ show x


-- | Nanoseconds since epoch, as big-endian uint64
newtype Timestamp = Timestamp Word64
    deriving (Eq, Generic)

fromUTCTime :: UTCTime -> Timestamp
fromUTCTime t = Timestamp . fromIntegral . round $ utcTimeToPOSIXSeconds t * 1e9

toUTCTime :: Timestamp -> UTCTime
toUTCTime (Timestamp w) = posixSecondsToUTCTime . (/ 1e9) $ realToFrac $ fromIntegral w

instance Bin.Serialize Timestamp where
    put (Timestamp w) = BinPut.putWord64be w
    get = Timestamp <$> BinGet.getWord64be

tsFormat :: String
tsFormat = "%Y-%m-%d %H:%M:%S.%q"

instance ToJSON Timestamp where
    toJSON ts = String . cs . toNanoSecs $ formatTime
        defaultTimeLocale tsFormat (toUTCTime ts)
            where toNanoSecs timeStr = take (length timeStr - 3) timeStr :: String

instance FromJSON Timestamp where
    parseJSON = withText "Timestamp" $ \str ->
        fromUTCTime <$> parseTimeM False defaultTimeLocale tsFormat (cs $ finalStr str)
            where finalStr s = s <> "000" -- nanoseconds -> picoseconds (%q)

instance Show Timestamp where
    show = cs . encode

-- Length-prefixed (uint8) UTF-8 string (max string length: 255 bytes)
instance Bin.Serialize StringId where
    put txt = Bin.putWord8 (fromIntegral $ BS.length txtBS) >> Bin.putByteString txtBS
        where txtBS = encodeUtf8 txt :: BS.ByteString
    get = Bin.getWord8 >>= \l -> decodeUtf8 <$> Bin.getByteString (fromIntegral l)

-- instance Bin.Serialize UTCTime where
--     put t = BinPut.putWord64be (round $ utcTimeToPOSIXSeconds t * 1e9)
--     get = posixSecondsToUTCTime . (/ 1e9) . fromIntegral <$> BinGet.getWord64be
