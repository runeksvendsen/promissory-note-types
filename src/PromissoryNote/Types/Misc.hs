{-# LANGUAGE TypeSynonymInstances, DeriveAnyClass, DeriveGeneric #-}
module PromissoryNote.Types.Misc
( module PromissoryNote.Types.Misc
, StringId(..)
, UTCTime
, Pay.SendPubKey
, Pay.BtcAmount
)
where

import PromissoryNote.Types.StringId
import Data.Word                (Word64)
import Data.Time                (UTCTime)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.String.Conversions  (cs)
import Data.Monoid              ((<>))
import Data.Aeson

import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize         as Bin
import qualified Data.Serialize.Put     as BinPut
import qualified Data.Serialize.Get     as BinGet
import qualified PaymentChannel as Pay
import           GHC.Generics
import           Data.Time.Format


type PubKey = HC.PubKey
type BitcoinAddress = HC.Address
type Amount = Pay.BtcAmount

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

