module PromissoryNote.Note.Verify where

import PromissoryNote.Types
import PromissoryNote.Note.Util


import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Crypto.Sign.Ed25519    as Ed
import qualified Control.Monad.Reader   as R
import qualified Data.Serialize         as Bin



verifyNote :: PromissoryNote -> Either VerifyError Bool
verifyNote pn
    | negRecLen pn == 1 = verifyLastRec pn
    | otherwise = verifyLastRec pn >> verifyNote (removeLastRec pn)

verifyLastRec :: PromissoryNote -> Either VerifyError Bool
verifyLastRec pn@PromissoryNoteG{..} =
    verifySigG
        (previousBearer pn)
        (Message $ Bin.encode noSigNote)
        (prev_bearer_sig $ NE.last negotiation_records)
  where
    noSigNote = mapNoteSigs (const ()) pn

