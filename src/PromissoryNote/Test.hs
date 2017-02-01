module PromissoryNote.Test
( module PromissoryNote.Test
, module PromissoryNote.Note
)
where

import           PromissoryNote.Note

import           Test.QuickCheck
import           Data.Time.Clock                 (UTCTime(..))
import           Data.Time.Calendar              (Day(..), fromGregorian)
import           Data.Text.Arbitrary     ()
import           Data.ByteString.Arbitrary (slowRandBs)
import qualified Data.Serialize as Bin
import qualified Data.List.NonEmpty     as NE
import qualified Crypto.Sign.Ed25519    as Ed
import qualified Codec.Crypto.RSA.Pure  as RSA
import Control.Monad.Time
import Debug.Trace


{-
NoteConf = NoteConf
    { ncPrvKey     :: Ed.SecretKey      -- ^ Note signer key
    , ncCurrency   :: Currency          -- ^ Eg. BTC\/LTC\/ETH
    , ncName       :: StringId          -- ^ Display name, eg. "https://superpay.com"
    , ncIssuerPub  :: PubKeyG           -- ^ Derived from 'ncPrvKey'
    , ncDuration   :: NominalDiffTime   -- ^ Note duration
    }
-}

newtype ArbEdKeyPair = ArbEdKeyPair (Ed.SecretKey, Ed.PublicKey)

instance Arbitrary ArbEdKeyPair where
    arbitrary = do
        bs <- slowRandBs 32
        let Just (pk,sk) = Ed.createKeypairFromSeed_ bs
        return $ ArbEdKeyPair (sk, pk)

instance Arbitrary NoteConf where
    arbitrary = do
        ArbEdKeyPair (sk, _) <- arbitrary
        NoteConf
            <$> pure sk
            <*> pure BTC
            <*> arbitrary
            <*> ( fromInteger <$> randomDur )
      where
        randomDur :: Gen Integer
        randomDur = choose (3600*24, 3600*24*30)

data WithSecret a = WithSecret a Ed.SecretKey

instance Arbitrary (WithSecret NegotiationInfo) where
    arbitrary = do
        ArbEdKeyPair (sk, pk) <- arbitrary
        ni <- NegotiationInfo (PubKeyEd25519 pk) <$> arbitrary
        return $ WithSecret ni sk

instance Arbitrary PubKeyG where
    arbitrary = do
        ArbEdKeyPair (_, pk) <- arbitrary
        return $ PubKeyEd25519 pk

instance MonadTime Gen where
    currentTime = arbitrary

arbNoteOfValue :: Amount -> Gen PromissoryNote
arbNoteOfValue val = do
    vers    <- arbitrary
    WithSecret nri _ <- arbitrary
    conf <- arbitrary
    now  <- arbitrary
    return $ runNoteM conf $ createNoteT now (NoteSpec val vers nri)


instance Arbitrary Amount where
    arbitrary = fromIntegral <$> (choose (1, 1000000) :: Gen Integer)

instance Arbitrary PromissoryNote where
    arbitrary = arbitrary >>= arbNoteOfValue

instance Arbitrary UTCTime where
    arbitrary =
        do randomDay    <- choose (1    , 29    ) :: Gen Int
           randomMonth  <- choose (1    , 12    ) :: Gen Int
           randomYear   <- choose (2001 , 2002  ) :: Gen Integer
           randomTime   <- choose (0    , 86401 ) :: Gen Int
           return $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)


instance Arbitrary UUID where
    arbitrary =
        fromRight <$> ( Bin.decode <$> slowRandBs 32 )
            where fromRight = either (error . ("BUG: Arbitrary UUID fail: " ++) . show) id
