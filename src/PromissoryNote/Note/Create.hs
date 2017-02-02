{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module PromissoryNote.Note.Create
( createNote
, createNoteT
, NoteMonad(..)
, NoteSign, runNoteSignM
, NegotiationInfo(..)
, NoteConf(..)
, NoteSpec(..)
-- *Util
, mkNegRec
, edSign
)
where

import PromissoryNote.Types

import Control.Monad.Time
import Data.Time.Clock
import GHC.Generics
import Data.Aeson
import qualified Data.List.NonEmpty     as NE
import qualified Crypto.Sign.Ed25519    as Ed
import qualified Control.Monad.Reader   as R
import qualified Data.Serialize         as Bin
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}




class Monad m => NoteMonad m where
    liftNote   :: NoteSign a -> m a
    runNoteM   :: NoteConf -> m a -> a
    signGetPrv :: m Ed.SecretKey
    confGet    :: m NoteConf
    confPubKey :: m PubKeyG

instance NoteMonad NoteSign where
    runNoteM    = runNoteSignM
    signGetPrv  = get ncPrvKey
    confGet     = R.asks readerConf
    confPubKey  = R.asks readerPubKey
    liftNote ns = do
        conf <- confGet
        return $ runNoteSignM conf ns

data ReaderConf = ReaderConf { readerConf :: NoteConf, readerPubKey :: PubKeyG }

newtype NoteSign a = NoteSign (R.Reader ReaderConf a)
    deriving (Functor, Applicative, Monad, R.MonadReader ReaderConf)

data NoteConf = NoteConf
    { ncPrvKey     :: Ed.SecretKey      -- ^ Note signer key
    , ncCurrency   :: Currency          -- ^ Eg. BTC\/LTC\/ETH
    , ncName       :: StringId          -- ^ Display name, eg. "https://superpay.com"
    , ncDuration   :: NominalDiffTime   -- ^ Note duration
    }

runNoteSignM ::
       NoteConf
    -> NoteSign a
    -> a
runNoteSignM conf (NoteSign r) =
    R.runReader r $ ReaderConf conf (edPubKeyDerive $ ncPrvKey conf)


data NegotiationInfo = NegotiationInfo
    { niBearer  :: PubKeyG
    , niPayInfo :: UUID
    } deriving (Eq, Show, Generic)

-- | Run-time information needed in order to construct a new note
data NoteSpec = NoteSpec
    { note_amount       :: Amount           -- ^ Equal to value of payment
    , note_verifiers    :: [PubKeyG]        -- ^ Supplied by client
    , note_neg_info     :: NegotiationInfo  -- ^ Supplied by client
    } deriving (Eq, Show, Generic)


createNote :: (MonadTime m, NoteMonad m) =>
       NoteSpec
    -> m PromissoryNote
createNote ns = do
    now <- currentTime
    liftNote $ createNoteT now ns

createNoteT ::
       UTCTime
    -> NoteSpec
    -> NoteSign PromissoryNote
createNoteT now (NoteSpec val verifiers nri) = do
    bn <- mkBaseNote now verifiers val
    mkNote nri bn

mkBaseNote :: UTCTime -> [PubKeyG] -> Amount -> NoteSign BaseNote
mkBaseNote now verifiers value =
    BaseNote
        <$> get ncCurrency
        <*> pure value
        <*> pure (fromUTCTime now)
        <*> fmap fromUTCTime (addUTCTime <$> get ncDuration <*> pure now)
        <*> get ncName
        <*> confPubKey
        <*> pure verifiers

mkNote ::
       NegotiationInfo
    -> BaseNote
    -> NoteSign PromissoryNote
mkNote nri bn = do
    prvKey <- signGetPrv
    return $ PromissoryNoteG bn $ mkNegRec nri (edSign prvKey noSigNote) NE.:| []
  where
    noSigNote = PromissoryNoteG bn $ mkNegRec nri () NE.:| []


-- Util
mkNegRec :: NegotiationInfo -> a -> NegotiationRecG a
mkNegRec NegotiationInfo{..} = NegRec niBearer niPayInfo

edSign :: forall a. Bin.Serialize a =>
    Ed.SecretKey -> a -> SigData
edSign k = SigEd25519 . Ed.dsign k . Bin.encode

get :: NoteMonad m => (NoteConf -> a) -> m a
get f = f <$> confGet


-- Generic instances
instance ToJSON NegotiationInfo
instance FromJSON NegotiationInfo
instance Bin.Serialize NegotiationInfo

instance ToJSON NoteSpec
instance FromJSON NoteSpec
instance Bin.Serialize NoteSpec
