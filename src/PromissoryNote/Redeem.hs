{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PromissoryNote.Redeem where

import           PromissoryNote.Note
import           PromissoryNote.Types
import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)


data RedeemBlock = RedeemBlock
  { notes           :: [PromissoryNote]
  , pay_to_addr     :: BitcoinAddress
  , signature       :: Signature
  } deriving (Generic, ToJSON, FromJSON, Bin.Serialize)

