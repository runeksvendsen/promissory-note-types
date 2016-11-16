module PromissoryNote.NoteWrap where

import Types
import Util.Crypto
import PromissoryNote.Note


data StoredNote = StoredNote
    { promissory_note   :: PromissoryNote
    , channel_source    :: SendPubKey
    , server_sig        :: Signature
    }
