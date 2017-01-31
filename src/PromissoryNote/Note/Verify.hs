module PromissoryNote.Note.Verify where

import PromissoryNote.Types
import PromissoryNote.Note

import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Crypto.Sign.Ed25519    as Ed
import qualified Control.Monad.Reader   as R
import qualified Data.Serialize         as Bin



verifyLastRec :: PromissoryNote -> Bool
verifyLastRec pn@PromissoryNoteG{..} =
    verify pk (Bin.encode noSigNote) (getSig . prev_bearer_sig $ NE.last negotiation_records)
  where
    pk = error "STUB"
    verify = Ed.dverify
    noSigNote = mapNoteSigs (const ()) pn
