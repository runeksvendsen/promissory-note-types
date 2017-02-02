{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, DeriveGeneric #-}
module PromissoryNote.Types.StringId where

import Data.Text.Encoding       (encodeUtf8, decodeUtf8)
import Data.Aeson

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Serialize         as Bin
import           GHC.Generics


newtype StringId = StringId T.Text
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Length-prefixed (uint8) UTF-8 string (max string length: 255 bytes)
instance Bin.Serialize StringId where
    put (StringId txt) = Bin.putWord8 (fromIntegral $ BS.length txtBS) >> Bin.putByteString txtBS
        where txtBS = encodeUtf8 txt :: BS.ByteString
    get = Bin.getWord8 >>= \l -> StringId . decodeUtf8 <$> Bin.getByteString (fromIntegral l)
