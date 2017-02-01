{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module PromissoryNote.Note.Negotiate
( negotiateTo
, NoteMonad
, NegotiationInfo(..)
)
where

import PromissoryNote.Note.Util
import PromissoryNote.Note.Create
import Data.Maybe
import qualified Data.List.NonEmpty     as NE


negotiateTo :: NoteMonad m =>
       NegotiationInfo
    -> PromissoryNote
    -> m PromissoryNote
negotiateTo nri pn = do
    prvKey <- signGetPrv
    return $ addRecord pn $ edSign prvKey (addRecord noSigNote ())
  where
    noSigNote = mapNoteSigs (const ()) pn
    replaceRecs pn' r = pn' { negotiation_records = r }
    addRecord pn' sig = replaceRecs pn' $ unsafeCastNE $
            NE.toList (negotiation_records pn') ++ [mkNegRec nri sig]

-- Util
unsafeCastNE :: [a] -> NE.NonEmpty a
unsafeCastNE = fromMaybe (error "you promised this was a non-empty list") . NE.nonEmpty
