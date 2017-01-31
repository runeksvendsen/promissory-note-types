{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module PromissoryNote.Note.Negotiate
( createNote
, negotiateTo
, NoteSignM
, NoteSignConf
)
where

import PromissoryNote.Types
import PromissoryNote.Note

import           Data.Void              (Void)
import           Data.Monoid
import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Crypto.Sign.Ed25519    as Ed
import qualified Control.Monad.Reader   as R
import qualified Data.Serialize         as Bin
import qualified Data.ByteString        as BS


class Monad m => NoteSignM m where
    signGetPrv  :: m Ed.SecretKey
    noteIssueId :: m UUID

data NoteSignConf = MkNoteSignConf
    { nscIssuerId   :: UUID             -- ^ Pubkey hash for 'nscPrvKey'
    , nscPrvKey     :: Ed.SecretKey
    }

instance NoteSignM (R.Reader NoteSignConf) where
    signGetPrv  = R.asks nscPrvKey
    noteIssueId = R.asks nscIssuerId


data NegRecInfo = NegRecInfo
    { nriNewBearer  :: UUID
    , nriPayInfo    :: UUID
    }


createNote :: NoteSignM m =>
       NegRecInfo
    -> BaseNote
    -> m PromissoryNote
createNote nri bn = do
    prvKey <- signGetPrv
    return $ PromissoryNoteG bn $ mkNegRec nri (sign prvKey noSigNote) NE.:| []
  where
    noSigNote = PromissoryNoteG bn $ mkNegRec nri () NE.:| []

negotiateTo :: NoteSignM m =>
       NegRecInfo
    -> PromissoryNote
    -> m PromissoryNote
negotiateTo nri pn = do
    prvKey <- signGetPrv
    return $ addRecord pn $ sign prvKey (addRecord noSigNote ())
  where
    noSigNote = mapNoteSigs (const ()) pn
    replaceRecs pn' r = pn' { negotiation_records = r }
    addRecord pn' sig = replaceRecs pn' $ unsafeCastNE $
            NE.toList (negotiation_records pn') ++ [mkNegRec nri sig]

mkNegRec :: NegRecInfo -> a -> NegotiationRecG a
mkNegRec NegRecInfo{..} = NegRec nriNewBearer nriPayInfo


sign :: forall a. Bin.Serialize a =>
    Ed.SecretKey -> a -> SignatureG Ed.Signature
sign k = MkSignatureG . Ed.dsign k . Bin.encode



verifyLastRec :: PromissoryNote -> Bool
verifyLastRec pn@PromissoryNoteG{..} =
    verify pk (Bin.encode noSigNote) (getSig . prev_bearer_sig $ NE.last negotiation_records)
  where
    pk = error "STUB"
    verify = Ed.dverify
    noSigNote = mapNoteSigs (const ()) pn


-- Util
unsafeCastNE :: [a] -> NE.NonEmpty a
unsafeCastNE = fromMaybe (error "you promised this was a non-empty list") . NE.nonEmpty
