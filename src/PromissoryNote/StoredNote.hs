{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module PromissoryNote.StoredNote
(
  StoredNote
, mkGenesisNote, mkCheckStoredNote
, setMostRecentNote
)
where

import PromissoryNote.Types
import PromissoryNote.Note.Util
import qualified Data.Bitcoin.PaymentChannel as Pay

import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)


data StoredNote = StoredNote
    { promissory_note       :: PromissoryNote   -- ^ The actual note
    , previous_note_id      :: UUID             -- ^ ID of the note issued before this one
    , payment_source        :: Pay.Payment      -- ^ Client signature & change value (in payment tx), for payment against which this note is issued
    , most_recent_note      :: Bool             -- ^ Is this the newest note (chain tip)?
    } deriving (Show, Generic, ToJSON, FromJSON, Bin.Serialize)

instance HasUUID StoredNote where
    serializeForID StoredNote{..} = serializeForID promissory_note

mkGenesisNote :: PromissoryNote -> Pay.Payment -> StoredNote
mkGenesisNote pn p = StoredNote pn zeroUUID p True

-- | Create note for storage given previous note,
-- also double check note/previous+current payment value
mkCheckStoredNote :: PromissoryNote -> StoredNote -> Pay.Payment -> Either String StoredNote
mkCheckStoredNote newPN prevSN@(StoredNote storedPN storedID storedPmnt _) newPmnt =
    checkPaymentValue >>=
        \pn -> Right $ StoredNote pn (getUUID prevSN) newPmnt True
  where
    checkPaymentValue =
        if face_value (base_note newPN) == storedPmnt `diff` newPmnt then
            Right newPN
        else
            Left $ "BUG: mkCheckStoredNote: Value mismatch.\n" ++
                unlines [ show $ face_value (base_note newPN)
                        , show $ storedPmnt `diff` newPmnt
                        , show newPN
                        , show prevSN
                        , show newPmnt]

diff :: Pay.Payment -> Pay.Payment -> BitcoinAmount
diff p1 p2 = Pay.payClientChange p1 - Pay.payClientChange p2

setMostRecentNote :: Bool -> StoredNote -> StoredNote
setMostRecentNote b n = n { most_recent_note = b }

