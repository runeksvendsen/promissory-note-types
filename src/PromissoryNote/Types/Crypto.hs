{-# LANGUAGE ScopedTypeVariables #-}
module PromissoryNote.Types.Crypto
( Signature
, PublicKey
, SignatureG(..)
, dummySig
, edPubKeyDerive
)
where

import qualified Crypto.Sign.Ed25519 as Ed
import           PromissoryNote.Types.UUID
import           Data.Aeson (Value(String), FromJSON(..), ToJSON(..), encode, decode, withText)
import           Data.String.Conversions (cs)
import           Data.Hashable
import           Data.Maybe (fromJust)
import           Data.Word  (Word8)
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Tagged    (Tagged(..))


newtype Message = Message BS.ByteString

-- | Class for different digital signature schemes.
--   Identified by its 'markerByte', which is prepended
--    to all serialized signatures and public keys
class IsSigSystem pk sig where
    verifySig :: pk -> Message -> sig -> Bool

class HasMarkerByte a where
    markerByte :: Tagged a Word8

-- | Generic signature type, parametized over a 'IsSigSystem' and the actual signature
data SignatureG sig = MkSignatureG { getSig :: sig } deriving (Eq, Show)

-- | Generic public key type, parametized over a 'IsSigSystem' and the actual pubkey
data PublicKeyG pk = MkPublicKeyG { getPubKey :: pk } deriving (Eq, Show)


instance (HasMarkerByte sig, Bin.Serialize sig) => Bin.Serialize (SignatureG sig) where
    put sg = Bin.putWord8 (unTagged (markerByte :: Tagged sig Word8))
          >> Bin.put (getSig sg)
    get = Bin.getWord8 >> MkSignatureG <$> Bin.get

instance (HasMarkerByte pk, Bin.Serialize pk) => Bin.Serialize (PublicKeyG pk) where
    put pkg = Bin.putWord8 (unTagged (markerByte :: Tagged pk Word8))
           >> Bin.put (getPubKey pkg)
    get = Bin.getWord8 >> MkPublicKeyG <$> Bin.get

instance (HasMarkerByte sig, Bin.Serialize sig) => ToJSON (SignatureG sig) where
    toJSON = String . cs . hexEncode

instance (HasMarkerByte sig, Bin.Serialize sig) => FromJSON (SignatureG sig) where
    parseJSON = withText "Signature" $ either fail return . hexDecode . cs

instance (HasMarkerByte pk, Bin.Serialize pk) => HasUUID (PublicKeyG pk) where
    serializeForID = Bin.encode



-- Util
hexEncode :: Bin.Serialize a => a -> BS.ByteString
hexEncode = B16.encode . Bin.encode

hexDecode :: Bin.Serialize a => BS.ByteString -> Either String a
hexDecode = Bin.decode . fst . B16.decode



-- Implementation for the ed25519 public-key signature system
ed25519Marker :: Word8
ed25519Marker = 0x01

instance HasMarkerByte Ed.Signature where markerByte = Tagged ed25519Marker
instance HasMarkerByte Ed.PublicKey where markerByte = Tagged ed25519Marker

dummySig :: SignatureG Ed.Signature
dummySig = MkSignatureG (Ed.Signature BS.empty)

type Signature = SignatureG Ed.Signature
type PublicKey = PublicKeyG Ed.PublicKey

edPubKeyDerive :: Ed.SecretKey -> PublicKey
edPubKeyDerive = MkPublicKeyG . Ed.toPublicKey

instance Bin.Serialize Ed.Signature where
    put = Bin.putByteString . Ed.unSignature
    get = Ed.Signature <$> Bin.getByteString 64



