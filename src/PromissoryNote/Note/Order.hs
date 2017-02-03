{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module PromissoryNote.Note.Order where

import PromissoryNote.Types
import PromissoryNote.Note.Create
import GHC.Generics
import Data.Word
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Serialize as Bin
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


data NoteOrder = NoteOrder
  { note_spec   :: NoteSpec
  , quantity    :: Word64
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, Bin.Serialize)

orderNoteVal :: NoteOrder -> Amount
orderNoteVal = note_amount . note_spec