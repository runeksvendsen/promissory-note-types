module PromissoryNote.Types.UUID
(
    UUID
  , HasUUID(..)
)
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson (Value(String), FromJSON(..), ToJSON(..), withText)
import           Data.String.Conversions (cs)
import           Data.Hashable
import qualified Data.Serialize as Bin


data UUID = SHA256 BS.ByteString deriving Eq

class HasUUID a where
    serializeForID :: a -> BS.ByteString
    getID :: a -> UUID
    getID = SHA256 . SHA256.hash . serializeForID


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
