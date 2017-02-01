{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import PromissoryNote.Test

import qualified Data.Aeson                 as JSON
import qualified Data.Serialize             as Bin
import qualified Data.ByteString            as BS
import qualified Data.Serialize.Get         as BinGet
import qualified Data.ByteString.Base16     as B16

import           Data.Typeable              (Typeable, typeOf)
import           Data.String.Conversions    (cs)
import Test.Framework                       (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Debug.Trace
{-# ANN module ("HLint: ignore Redundant if"::String) #-}



main :: IO ()
main = putStrLn "" >> defaultMain tests

tests :: [Test]
tests =
    [ testGroup "PromissoryNote"
        [ testProperty "Create/verify" noteCreateVerify
        , testProperty "Binary serialization"
            (binSerDeser :: PromissoryNote -> Bool)
        , testProperty "JSON serialization"
            (jsonSerDeser :: PromissoryNote -> Bool)
        ]
    ]

noteCreateVerify :: PromissoryNote -> Bool
noteCreateVerify = either fail' id . verifyNote
    where fail' e = show e `trace` False


jsonSerDeser :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => a -> Bool
jsonSerDeser fp =
    maybe False checkEquals decodedObj
        where decodedObj = JSON.decode $ JSON.encode fp
              checkEquals serDeserVal = cs (JSON.encode fp) `trace`
                if serDeserVal /= fp then
                        error $ "Ser/deser mismatch.\nOriginal: " ++
                                show fp ++ "\nCopy: " ++ show decodedObj
                    else
                        True

binSerDeser :: (Typeable a, Show a, Eq a, Bin.Serialize a) => a -> Bool
binSerDeser fp =
    checkEquals decodeRes
        where bs = Bin.encode fp
              decodeRes = deserEither bs
              checkEquals serDeserRes = case serDeserRes of
                    Left e      -> error $ "Serialize/deserialize error: " ++ show e
                    Right res   ->
                            if res /= fp then
                                error $ "Ser/deser mismatch.\nOriginal: " ++ show fp ++
                                        "\nCopy: " ++ show res
                            else
                                True

deserEither :: forall a. (Typeable a, Bin.Serialize a) => BS.ByteString -> Either String a
deserEither bs = do
    let eitherRes' = BinGet.runGetPartial (Bin.get :: BinGet.Get a) bs
    handleResult eitherRes'
    where
        toHexString = cs . B16.encode
        handleResult eitherRes = case eitherRes of
            BinGet.Done val _           -> Right val
            BinGet.Partial feedFunc     -> handleResult $ feedFunc BS.empty
            BinGet.Fail e leftoverBS    -> Left $
                "Type: " ++ show (typeOf (undefined :: a)) ++
                ". Error: " ++ e ++
                ". Data consumed (" ++ show offset ++ " bytes): " ++
                    toHexString (BS.take offset bs) ++
                ". Unconsumed data: (" ++ show ( BS.length bs - fromIntegral offset ) ++
                " bytes): " ++
                    toHexString leftoverBS
                        where offset = BS.length bs - BS.length leftoverBS
