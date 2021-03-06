{-# LANGUAGE OverloadedStrings #-}
module PromissoryNote.Types.UUID
(
    UUID
  , HasUUID(..)
  , zeroUUID
)
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson (Value(String), FromJSON(..), ToJSON(..), encode, decode, withText)
import           Data.String.Conversions (cs)
import           Data.Hashable
import           Data.Maybe (fromJust)
import qualified Data.Serialize as Bin
-- import           Test.QuickCheck
-- import           Data.ByteString.Arbitrary (slowRandBs)


-- | Universally unique ID (SHA-256)
data UUID = SHA256 BS.ByteString deriving Eq

zeroUUID :: UUID
zeroUUID = fromJust . decode . encode $
    String "0000000000000000000000000000000000000000000000000000000000000000"

class HasUUID a where
    serializeForID :: a -> BS.ByteString
    getUUID :: a -> UUID
    getUUID = SHA256 . SHA256.hash . serializeForID


instance Show UUID where
    show (SHA256 bs) = "<UUID " ++ cs (B16.encode bs) ++ ">"

instance ToJSON UUID where
    toJSON (SHA256 bs) = String . cs . B16.encode $ bs

instance FromJSON UUID where
    parseJSON = withText "UUID" (return . SHA256 . fst . B16.decode . cs)

instance Hashable UUID where
    hashWithSalt salt (SHA256 uuid) = salt `hashWithSalt` uuid

instance Bin.Serialize UUID where
    put (SHA256 bs) = Bin.putByteString bs
    get = SHA256 <$> Bin.getByteString 32

-- instance Arbitrary UUID where
--     arbitrary = SHA256 <$> slowRandBs 32
