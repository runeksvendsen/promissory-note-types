{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module PromissoryNote.Note where

import           Types
import           Util.Crypto
import           ClearingServer.Lib.Types.Data
import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


data BaseNote = BaseNote
  { denomination        :: Currency     -- eg. BTC/USD/LTC/EUR etc.
  , face_value          :: Amount
  , issue_date          :: UTCTime
  , exp_date            :: UTCTime
  , issuer_name         :: Identity
  , issuer              :: UUID
  , verifiers           :: UUID
  } deriving (Generic, ToJSON, FromJSON, Bin.Serialize)

data PromissoryNote = PromissoryNote
  { base_note           :: BaseNote
  , negotiation_records :: [NegotiationRec]
  } deriving (Generic, ToJSON, FromJSON, Bin.Serialize)

data NegotiationRec = NegRec
  { bearer              :: UUID
  , payment_info        :: UUID
  -- | In case of the first negotiation record the previous bearer is the issuer
  , prev_bearer_sig     :: Signature
  } deriving (Generic, ToJSON, FromJSON, Bin.Serialize)


instance HasUUID PromissoryNote where
    serializeForId Note{..} = Bin.encode base_note
