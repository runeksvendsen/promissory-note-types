{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module PromissoryNote.StoredNote where

import PromissoryNote.Types
import PromissoryNote.Note
import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)


data StoredNote = StoredNote
    { promissory_note       :: PromissoryNote   -- ^ The actual note
    , previous_note_id      :: UUID             -- ^ ID of the note issued before this one
    , payment_change_val    :: BitcoinAmount    -- ^ Client change value (in payment tx) for payment against which this note is issued
    , server_sig            :: Signature        -- ^ Signature over above data (not including 'most_recent_note')
    , most_recent_note      :: Bool             -- ^ Is this the newest note?
    } deriving (Show, Generic, ToJSON, FromJSON, Bin.Serialize)

instance HasUUID StoredNote where
    serializeForID StoredNote{..} = serializeForID promissory_note

mkStoredNote :: PromissoryNote -> UUID -> BitcoinAmount -> StoredNote
mkStoredNote pn prev_id chgVal = StoredNote pn prev_id chgVal () True

setMostRecentNote :: Bool -> StoredNote -> StoredNote
setMostRecentNote b n = n { most_recent_note = b }

