{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PromissoryNote.Note.Order where

import PromissoryNote.Note.Create

import           GHC.Generics
import           Control.Monad
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe
import qualified Data.List.NonEmpty     as NE
import qualified Network.Haskoin.Node   as HN
import Data.Word
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


data NoteOrder = NoteOrder
  { note_spec   :: NoteSpec
  , quantity    :: Word64
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, Bin.Serialize)
