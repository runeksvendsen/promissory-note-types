{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module PromissoryNote.Api where

import PromissoryNote
import Servant.API
import qualified Network.Haskoin.Transaction as HT
import Servant.API.ContentTypes


type VER = "v1"

type RedeemNotes = VER  :> "redeem"  :> ReqBody '[JSON] RedeemBlock  :>  PostAccepted '[JSON] NoContent

