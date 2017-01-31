module PromissoryNote.Test where

import           PromissoryNote.Note
import           Test.QuickCheck
import           Data.Time.Clock                 (UTCTime(..))
import           Data.Time.Calendar              (Day(..), fromGregorian)
import           Data.Text.Arbitrary     ()
import           Data.ByteString.Arbitrary (slowRandBs)
import qualified Data.Serialize as Bin
import qualified Data.List.NonEmpty     as NE


arbNoteOfValue :: Amount -> UTCTime -> Gen PromissoryNote
arbNoteOfValue a t = arbitrary >>=
    \note@(PromissoryNoteG bn _) ->
        return $ note { base_note = bn { face_value = a, issue_date = t } }

instance Arbitrary BaseNote where
    arbitrary = BaseNote BTC
        <$> arbAmount
        <*> arbitrary   -- issue_date
        <*> arbitrary   -- exp_date
        <*> arbitrary   -- issuer_name
        <*> arbitrary   -- issuer
        <*> arbitrary   -- verifiers
      where
        arbAmount = fromIntegral <$> choose (1 :: Int, 100000 :: Int) :: Gen Amount

instance Arbitrary PromissoryNote where
    arbitrary = PromissoryNoteG <$> arbitrary <*> pure (dummyNegRec NE.:| [])
instance Arbitrary UTCTime where
    arbitrary =
        do randomDay <- choose (1, 29) :: Gen Int
           randomMonth <- choose (1, 12) :: Gen Int
           randomYear <- choose (2001, 2002) :: Gen Integer
           randomTime <- choose (0, 86401) :: Gen Int
           return $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)


instance Arbitrary UUID where
    arbitrary =
        fromRight <$> ( Bin.decode <$> slowRandBs 32 )
            where fromRight (Right a) = a
                  fromRight (Left e) = error $ "Arbitrary UUID error: " ++ show e
