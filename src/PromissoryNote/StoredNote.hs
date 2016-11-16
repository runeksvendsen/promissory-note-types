module PromissoryNote.StoredNote where

import PromissoryNote.Types
import PromissoryNote.Note


data StoredNote = StoredNote
    { promissory_note   :: PromissoryNote
    , channel_source    :: PubKey
    , server_sig        :: Signature
    }
