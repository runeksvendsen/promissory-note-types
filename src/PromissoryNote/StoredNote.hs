{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module PromissoryNote.StoredNote where

import PromissoryNote.Types
import PromissoryNote.Note
import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)


data StoredNote = StoredNote
    { promissory_note   :: PromissoryNote
    , channel_source    :: PubKey
    , server_sig        :: Signature
    } deriving (Generic, ToJSON, FromJSON, Bin.Serialize)

instance HasUUID StoredNote where
    serializeForID StoredNote{..} = serializeForID promissory_note
