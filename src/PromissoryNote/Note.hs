{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards, ScopedTypeVariables #-}
module PromissoryNote.Note
(
  module PromissoryNote.Note
, module PromissoryNote.Types
)
where


import           PromissoryNote.Types
import           GHC.Generics
import           Control.Monad
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Network.Haskoin.Node   as HN
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


-- | Promissory note
type PromissoryNote = PromissoryNoteG Signature
data PromissoryNoteG sigType = PromissoryNoteG
  { base_note           :: BaseNote                                 -- ^ Base note
  , negotiation_records :: NE.NonEmpty (NegotiationRecG sigType)    -- ^ Non-empty list of negotiation records
  } deriving (Show, Generic, ToJSON, FromJSON)


-- | Note without negotiation record(s)
data BaseNote = BaseNote
  { denomination        :: Currency         -- ^ Three-byte ASCII currency code (eg. BTC\/LTC\/ETH)
  , face_value          :: Amount           -- ^ Little-endian uint64 (Bitcoin standard)
  , issue_date          :: UTCTime          -- ^ Nanoseconds since epoch (big-endian uint64)
  , exp_date            :: UTCTime          -- ^ Nanoseconds since epoch (big-endian uint64)
  , issuer_name         :: StringIdentifier -- ^ Length-prefixed (uint8) UTF-8 string (max string byte size: 255 bytes)
  , issuer              :: UUID             -- ^ Ed25519 public key hash (SHA-256)
  , verifiers           :: UUID             -- ^ Root node hash in Merkle tree of verifier pubkey UUIDs
  } deriving (Show, Generic, ToJSON, FromJSON, Bin.Serialize)

-- | Negotiation record
type NegotiationRec = NegotiationRecG Signature
data NegotiationRecG sigType = NegRec
  { bearer              :: UUID         -- ^ Ed25519 public key hash (SHA-256)
  , payment_info_hash   :: UUID         -- ^ Hash (SHA-256) of payment info attribute
  , prev_bearer_sig     :: sigType      -- ^ 64-byte Ed25519 signature
  } deriving (Show, Generic, ToJSON, FromJSON, Bin.Serialize)


instance Bin.Serialize a => Bin.Serialize (PromissoryNoteG a)

instance HasUUID PromissoryNote where
    serializeForID PromissoryNoteG{..} = Bin.encode base_note

instance Bin.Serialize a => Bin.Serialize (NE.NonEmpty (NegotiationRecG a)) where
    put nrL = Bin.put (HN.VarInt . fromIntegral $ NE.length nrL)
           >> forM_ (NE.toList nrL) Bin.put
    get = Bin.get >>= \(HN.VarInt len) ->
          nonEmptyOrFail <$> replicateM (fromIntegral len) Bin.get
        where
          nonEmptyOrFail = fromMaybe (error errMsg) . NE.nonEmpty
          errMsg = "End of input: missing negotiation record(s)"

dummyNote = PromissoryNoteG dummyBaseNote (dummyNegRec NE.:| [])

dummyNegRec = NegRec zeroUUID zeroUUID dummySig :: NegotiationRec

dummyBaseNote = BaseNote BTC 0 epoch epoch "null" zeroUUID zeroUUID
    where epoch = posixSecondsToUTCTime 0

-- | Replace signature data
mapNoteSigs :: (a -> b) -> PromissoryNoteG a -> PromissoryNoteG b
mapNoteSigs f png = png { negotiation_records = NE.map (mapRecSig f) $ negotiation_records png }

mapRecSig :: (a -> b) -> NegotiationRecG a -> NegotiationRecG b
mapRecSig f nrg = nrg { prev_bearer_sig = f $ prev_bearer_sig nrg }

