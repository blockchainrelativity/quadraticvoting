{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import qualified Data.ByteString.Char8       as BS8
import System.Environment ( getArgs )
import Prelude
import Ledger (PaymentPubKeyHash(..), Address)
import Data.String as DS -- (fromString)
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe (fromJust)
import QuadraticVFOnChain (FundCreationDatum (..))
-- import Codec.Binary.Encoding (base16)
-- import Data.Text.Encoding (Base16(..))
import Data.Text (unpack)
import Data.ByteArray.Encoding
import Data.Text.Encoding (decodeUtf8)
import PlutusTx.Builtins.Class as BIN
import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx



data DummyData = DummyData {
    --fundOwner :: String,
    prizeAmount :: Integer
    --projectLabel :: [String]
}

instance Eq DummyData where 
  {-# INLINABLE (==) #-}
  a == b =
    (prizeAmount a == prizeAmount b)
    -- && (projectLabel a == projectLabel b)
    -- (fundOwner a == fundOwner b)
    -- && 

-- instance PlutusTx.UnsafeFromData DummyData where
--   unsafeFromBuiltinData = DummyData . decodeUtf8 . PlutusTx.unsafeFromBuiltinData
PlutusTx.unstableMakeIsData ''DummyData


main :: IO ()
main = do
  [amount'] <- getArgs
  let prize  = read amount'  -- vPrizeAmount
      -- owner = DS.fromString owner' -- vFundOwner
      
      -- label = labels' -- vProjectLabel BIN.stringToBuiltinByteString 
      createFund   = DummyData {
          --fundOwner = owner,
          prizeAmount = prize--,
          --projectLabel = label
      }
          --FundCreationDatum vFundOwner vPrizeAmount vProjectLabel
  writeData ("datum-lock.json") createFund
  putStrLn "Done"
-- Datum also needs to be passed when sending the token to the script (aka putting for sale)
-- When doing this, the datum needs to be hashed, see Alonzo-purple exercise-solutions on how to hash a datum
-- old writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()



writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)


--old toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString :: PlutusTx.ToData a => a -> LB.ByteString 
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema 
    . fromPlutusData
    . PlutusTx.toData

-- writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
-- writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript