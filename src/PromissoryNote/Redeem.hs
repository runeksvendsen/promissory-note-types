{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PromissoryNote.Redeem where

import           PromissoryNote.Note.Util
import           PromissoryNote.Types
import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)


-- | Used to negotiate multiple notes to a single bearer
data RedeemBlock = RedeemBlock
  { -- | Notes whose value is to be redeemed
    notes           :: [PromissoryNote]
    -- | Negotiation with server as new bearer and PaymentInfo field containing:
    --    PAY TO BITCOIN ADDRESS <bitcoin_address>
  , nrb_neg_rec     :: NegotiationRec
  } deriving (Generic, ToJSON, FromJSON, Bin.Serialize)

