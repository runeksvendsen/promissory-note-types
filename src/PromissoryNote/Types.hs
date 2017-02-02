{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards, ScopedTypeVariables #-}
module PromissoryNote.Types
( module PromissoryNote.Types
, module X
, NE.NonEmpty(..)

)
where

import PromissoryNote.Types.UUID    as X
import PromissoryNote.Types.Crypto  as X
import PromissoryNote.Types.Misc    as X


import           GHC.Generics
import           Control.Monad
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Network.Haskoin.Node   as HN
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


-- | Promissory note
type PromissoryNote = PromissoryNoteG SigData
data PromissoryNoteG sigType = PromissoryNoteG
  { base_note           :: BaseNote                             -- ^ Base note
  , negotiation_records :: NE.NonEmpty (NegotiationRecG sigType) -- ^ 'HN.VarInt'-prefixed non-empty list of negotiation records
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Bin.Serialize)

-- | Note without negotiation record(s)
data BaseNote = BaseNote
  { denomination        :: Currency     -- ^ Three-byte ASCII currency code (eg. BTC\/LTC\/ETH)
  , face_value          :: Amount       -- ^ Little-endian uint64 (Bitcoin standard)
  , issue_date          :: Timestamp    -- ^ Nanoseconds since epoch (big-endian uint64)
  , exp_date            :: Timestamp    -- ^ Nanoseconds since epoch (big-endian uint64)
  , issuer_name         :: StringId     -- ^ Length-prefixed (uint8) UTF-8 string (max string byte size: 255 bytes)
  , issuer              :: PubKeyG      -- ^ Issuer pubkey
  , verifiers           :: [PubKeyG]    -- ^ Verifier pubkey list
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Bin.Serialize)

-- | Negotiation record
type NegotiationRec = NegotiationRecG SigData
data NegotiationRecG sigType = NegRec
  { bearer              :: PubKeyG      -- ^ Bearer public key
  , payment_info_hash   :: UUID         -- ^ Hash (SHA-256) of payment info attribute
  , prev_bearer_sig     :: sigType      -- ^ Previous bearer's signature
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Bin.Serialize)


instance HasUUID PromissoryNote where
    serializeForID PromissoryNoteG{..} = Bin.encode base_note


instance Bin.Serialize sigType => Bin.Serialize (NE.NonEmpty (NegotiationRecG sigType)) where
    put nrL = Bin.put (HN.VarInt . fromIntegral $ NE.length nrL)
           >> forM_ (NE.toList nrL) Bin.put
    get = Bin.get >>= \(HN.VarInt len) ->
          nonEmptyOrFail <$> replicateM (fromIntegral len) Bin.get
        where
          nonEmptyOrFail = fromMaybe (error errMsg) . NE.nonEmpty
          errMsg = "End of input: missing negotiation record(s)"

