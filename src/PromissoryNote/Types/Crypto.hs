{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module PromissoryNote.Types.Crypto
( SigData(..)
, PubKeyG(..)
, Message(..)
, dummyEdSig
, dummyEdPubKey
, edPubKeyDerive
, PubKeyScheme(..)
, VerifyError(..)
, verifySigG
-- , SignatureG(..)
-- , SigG(..)
-- , PubKeyG(..)
)
where

import qualified Crypto.Sign.Ed25519    as Ed
import qualified Codec.Crypto.RSA.Pure  as RSA
import           PromissoryNote.Types.UUID

import           Data.String.Conversions (cs)
import           Data.Hashable
import           Data.Maybe (fromJust)
import           Data.Word  (Word8)
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Tagged    (Tagged(..))
import           Control.Monad
import           Data.Monoid
import           GHC.Generics (Generic)
import           Text.Printf
import qualified Data.Text as T

import           Data.Aeson.Types       (Parser)
import           Data.Aeson             (Value(String)
                                        , FromJSON(..)
                                        , ToJSON(..)
                                        , encode, decode, withText)

import Debug.Trace


-- | Class for different digital signature schemes.
--   Identified by its 'markerByte', which is prepended
--    to all serialized public keys
class PubKeyScheme pk where
    markerByte :: pk -> Word8

class Bin.Serialize sig => IsSignature sig

newtype Message = Message BS.ByteString

class (PubKeyScheme pk, IsSignature sig) => VerfiesSig sig pk where
    verifySig :: pk -> Message -> sig -> Bool

data VerifyError =
    PubKeySigMismatch PubKeyG SigData
        deriving Eq

instance Show VerifyError where
    show (PubKeySigMismatch pk sig) =
        "Signature type does not match public key type: " ++ show (pk, sig)

verifySigG :: PubKeyG -> Message -> SigData -> Either VerifyError Bool
verifySigG (PubKeyEd25519 pk) msg (SigEd25519 sig) =
    Right $ verifySig pk msg sig
verifySigG (PubKeyRSA pk) msg (SigRSA sig) =
    Right $ verifySig pk msg sig
verifySigG pk _ sig =
    Left $ PubKeySigMismatch pk sig


-- Sum type PubKey instance
data PubKeyG =
    PubKeyEd25519 Ed.PublicKey
  | PubKeyRSA     RSA.PublicKey
        deriving (Eq, Show, Generic)

instance Bin.Serialize PubKeyG where
    put (PubKeyEd25519 pk) = Bin.putWord8 (markerByte pk) >> Bin.put pk
    put (PubKeyRSA pk) = Bin.putWord8 (markerByte pk) >> Bin.put pk
    get = Bin.getWord8 >>= getPK
        where
            printWord = printf "0x%02x" -- eg. 0x02
            getPK w
                | w == ed25519Marker = PubKeyEd25519 <$> Bin.get
                | w == rsaMarker     = PubKeyRSA <$> Bin.get
                | otherwise = fail $ "Unknown marker byte: " ++
                    printWord w ++ ". Expected one of: " ++
                    unwords (map printWord [ed25519Marker, rsaMarker])

instance ToJSON PubKeyG where
    toJSON = String . cs . hexEncode

instance FromJSON PubKeyG where
    parseJSON = withText "PubKeyG" jsonHexBinary



-- Sum type Sig instance
data SigData =
    SigEd25519 Ed.Signature
  | SigRSA     RSASig
        deriving (Eq, Show, Generic)

instance Bin.Serialize SigData where
    put (SigEd25519 sig) = Bin.putWord8 ed25519Marker >> Bin.put sig
    put (SigRSA sig) = Bin.putWord8 rsaMarker >> Bin.put sig
    get = Bin.getWord8 >>= getSig
        where
            printWord = printf "0x%02x"
            getSig w
                | w == ed25519Marker = SigEd25519 <$> Bin.get
                | w == rsaMarker     = SigRSA <$> Bin.get
                | otherwise = fail $ "Unknown marker byte: " ++
                    printWord w ++ ". Expected one of: " ++
                    unwords (map printWord [ed25519Marker, rsaMarker])

instance ToJSON SigData where
    toJSON = String . cs . hexEncode

instance FromJSON SigData where
    parseJSON = withText "SigData" jsonHexBinary







-- Implementation for RSA
rsaMarker :: Word8
rsaMarker = 0x02

instance PubKeyScheme RSA.PublicKey where markerByte = const rsaMarker
instance IsSignature RSASig

instance VerfiesSig RSASig RSA.PublicKey where
    verifySig pk (Message msgBS) (RSASig sigBS) =
        either (const False) id $ RSA.verify pk (cs msgBS) (cs sigBS)

newtype RSASig = RSASig BS.ByteString
    deriving (Eq, Show, Generic)

instance Bin.Serialize RSASig where
    get = RSASig <$> Bin.get        -- Word64 length-prefixed ByteString
    put (RSASig bs) = Bin.put bs    -- Word64 length-prefixed ByteString

instance Bin.Serialize RSA.PublicKey where
    put = error "STUB"
    get = error "STUB"




-- Implementation for the Ed25519
ed25519Marker :: Word8
ed25519Marker = 0x01

instance PubKeyScheme Ed.PublicKey where markerByte = const ed25519Marker
instance IsSignature Ed.Signature

instance VerfiesSig Ed.Signature Ed.PublicKey where
    verifySig pk (Message bs) = Ed.dverify pk bs

instance Bin.Serialize Ed.Signature where
    put = Bin.putByteString . Ed.unSignature
    get = Ed.Signature <$> Bin.getByteString 64

instance Bin.Serialize Ed.PublicKey where
    put = Bin.putByteString . Ed.unPublicKey
    get = Ed.PublicKey <$> Bin.getByteString 32




edPubKeyDerive :: Ed.SecretKey -> PubKeyG
edPubKeyDerive = PubKeyEd25519 . Ed.toPublicKey

dummyEdSig :: SigData
dummyEdSig = SigEd25519 . Ed.Signature . fst .
    B16.decode $ "c3d8563047c214524ea75f888d89f325cea90fa18fe33b8311dda5f7a4ba9069" <>
                 "b02c9f2522f26e0cc392e1f60435940274c77d18162a84a2940bc4f6f0c49a1e"

dummyEdPubKey :: PubKeyG
dummyEdPubKey = PubKeyEd25519 . Ed.PublicKey . fst .
    B16.decode $ "d406160355df4dc9e118a47d204b697801adbf4bc20926a5c30fe4c8bc8d3c41"

-- Util
hexEncode :: Bin.Serialize a => a -> BS.ByteString
hexEncode = B16.encode . Bin.encode

hexDecode :: Bin.Serialize a => BS.ByteString -> Either String a
hexDecode = Bin.decode . fst . B16.decode

jsonHexBinary :: Bin.Serialize a => T.Text -> Parser a
jsonHexBinary = either fail return . hexDecode . cs





