{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module PromissoryNote.Note.Create
( createNote
, NoteMonad(..)
, NoteSign, runNoteSignM
, NegotiationInfo(..)
-- , NoteConf(..)
-- *Util
, mkNegRec
, sign
)
where

import PromissoryNote.Types
import PromissoryNote.Note

import Control.Monad.Time
import Data.Time.Clock
import qualified Data.List.NonEmpty     as NE
import qualified Crypto.Sign.Ed25519    as Ed
import qualified Control.Monad.Reader   as R
import qualified Data.Serialize         as Bin



class Monad m => NoteMonad m where
    signGetPrv :: m Ed.SecretKey

instance NoteMonad NoteSign where signGetPrv = R.asks ncPrvKey

newtype NoteSign a = NoteSign (R.Reader NoteConf a)
    deriving (Functor, Applicative, Monad, R.MonadReader NoteConf)

runNoteSignM ::
       Ed.SecretKey
    -> Currency
    -> StringIdentifier
    -> DiffTime
    -> NoteSign a
    -> a
runNoteSignM prvKey curr ident dur (NoteSign r) =
    R.runReader r (NoteConf prvKey curr ident (calcPubId prvKey) dur)
        where calcPubId = error "STUB"

data NoteConf = NoteConf
    { ncPrvKey     :: Ed.SecretKey
    , ncCurrency   :: Currency
    , ncName       :: StringIdentifier
    , ncIssuerId   :: UUID              -- ^ Derived from private key
    , ncDuration   :: DiffTime
    }


data NegotiationInfo = NegotiationInfo
    { niBearer  :: UUID
    , niPayInfo :: UUID
    }

mkBaseNote :: MonadTime m => UUID -> Amount -> m BaseNote
mkBaseNote = undefined

-- BaseNote
{-  denomination        :: Currency         -- ^ Three-byte ASCII currency code (eg. BTC\/LTC\/ETH)
  , face_value          :: Amount           -- ^ Little-endian uint64 (Bitcoin standard)
  , issue_date          :: UTCTime          -- ^ Nanoseconds since epoch (big-endian uint64)
  , exp_date            :: UTCTime          -- ^ Nanoseconds since epoch (big-endian uint64)
  , issuer_name         :: StringIdentifier -- ^ Length-prefixed (uint8) UTF-8 string (max string byte size: 255 bytes)
  , issuer              :: UUID             -- ^ Ed25519 public key hash (SHA-256)
  , verifiers           :: UUID             -- ^ Root node hash in Merkle tree of verifier pubkey UUIDs
-}

createNote :: NoteMonad m =>
       NegotiationInfo
    -> BaseNote
    -> m PromissoryNote
createNote nri bn = do
    prvKey <- signGetPrv
    return $ PromissoryNoteG bn $ mkNegRec nri (sign prvKey noSigNote) NE.:| []
  where
    noSigNote = PromissoryNoteG bn $ mkNegRec nri () NE.:| []


-- Util
mkNegRec :: NegotiationInfo -> a -> NegotiationRecG a
mkNegRec NegotiationInfo{..} = NegRec niBearer niPayInfo

sign :: forall a. Bin.Serialize a =>
    Ed.SecretKey -> a -> SignatureG Ed.Signature
sign k = MkSignatureG . Ed.dsign k . Bin.encode

