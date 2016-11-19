{-# LANGUAGE TypeSynonymInstances #-}
module PromissoryNote.Types.Misc
(
    module PromissoryNote.Types.Misc
,   UTCTime
,   Pay.SendPubKey
)
where

import Data.Word (Word64)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize as Bin
import qualified Data.Bitcoin.PaymentChannel as Pay


type PubKey = HC.PubKey
type BitcoinAddress = HC.Address
type Amount = Word64

type StringIdentifier = T.Text
type Currency = ()

instance Bin.Serialize StringIdentifier where
    put = error "STUB"
    get = error "STUB"

instance Bin.Serialize UTCTime where
    put = error "STUB"
    get = error "STUB"
