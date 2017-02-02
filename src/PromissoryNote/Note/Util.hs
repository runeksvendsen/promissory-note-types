module PromissoryNote.Note.Util
(
  module PromissoryNote.Note.Util
, module PromissoryNote.Types
)
where

import PromissoryNote.Types
import PromissoryNote.Types.Misc

import           GHC.Generics
import           Control.Monad
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Network.Haskoin.Node   as HN


removeLastRec :: PromissoryNote -> PromissoryNote
removeLastRec pn@PromissoryNoteG{..}
    | Nothing   <- maybeInit negotiation_records = pn
    | Just init <- maybeInit negotiation_records =
        pn { negotiation_records = init }

previousBearer :: PromissoryNote -> PubKeyG
previousBearer PromissoryNoteG{..} =
    case nextLast negotiation_records of
        Nothing     -> issuer base_note
        Just negRec -> bearer negRec

negRecLen :: PromissoryNote -> Int
negRecLen PromissoryNoteG{..} = NE.length negotiation_records

nextLast :: NonEmpty a -> Maybe a
nextLast = fmap NE.last . maybeInit

maybeInit :: NonEmpty a -> Maybe (NonEmpty a)
maybeInit = NE.nonEmpty . NE.init

dummyNote = PromissoryNoteG dummyBaseNote (dummyNegRec :| [])

dummyNegRec = NegRec dummyEdPubKey zeroUUID dummyEdSig :: NegotiationRec

dummyBaseNote = BaseNote BTC 0 epoch epoch (StringId "dummy issuer") dummyEdPubKey [dummyEdPubKey]
    where epoch = fromUTCTime $ posixSecondsToUTCTime 0

-- | Replace signature data
mapNoteSigs :: (a -> b) -> PromissoryNoteG a -> PromissoryNoteG b
mapNoteSigs f png = png { negotiation_records = NE.map (mapRecSig f) $ negotiation_records png }

mapRecSig :: (a -> b) -> NegotiationRecG a -> NegotiationRecG b
mapRecSig f nrg = nrg { prev_bearer_sig = f $ prev_bearer_sig nrg }


