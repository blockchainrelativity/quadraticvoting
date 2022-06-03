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
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NumericUnderscores  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module OnChain where


import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins    as Builtins
import           PlutusTx.Builtins.Internal (BuiltinInteger, BuiltinString)
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           PlutusTx.Prelude     (BuiltinByteString)
import           Prelude              (Show (..), String)
import           Ledger
import qualified Ledger.Scripts       as Scripts
import           Ledger.Ada           as Ada


-- THE VALIDATOR
-- {{{
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator datum _ _ =
  -- {{{
  let
    invalidDatum :: a -> ()
    invalidDatum = const $ traceError "Invalid datum."

    enoughAda :: BuiltinString -> BuiltinData -> ()
    enoughAda errorMsg bd =
      -- {{{
      Builtins.matchData bd
        (const invalidDatum) -- note that this handler required two parameters.
        invalidDatum
        invalidDatum
        ( \ada ->
            if ada >= minAda then
              ()
            else
              traceError errorMsg
        )
        invalidDatum
      -- }}}

    constrHandler :: Integer -> [BuiltinData] -> ()
    constrHandler index lst
      | index == 0 =
          -- {{{ CREATE FUND 
          case lst of
            [bFundOwner, iPrizeAmount, bProjectLabel] ->
              enoughAda "Not enough ADA." iPrizeAmount
            _  ->
              invalidDatum ()
          -- }}}
      | index == 1 =
          -- {{{ SUBMIT PROJECT 
          case lst of
            [bProjectOwner, iRegistrationFee, bProjectCategory, bFundPayIdentifier] ->
              enoughAda "Not enough funds to register project." iRegistrationFee
            _ ->
              invalidDatum ()
          -- }}}
      | index == 2 =
          -- {{{ VOTE 
          case lst of
            [bProjectToVote, bVotePayAddress, iAdaLovelaceValue, bVoteFundIdentifier] ->
              enoughAda "Not enough ADA to vote." iAdaLovelaceValue
            _ ->
              invalidDatum ()
          -- }}}
      | index == 3 =
          -- {{{ CONTRIBUTE 
          case lst of
            [bFundAddress, iPrizeFund] ->
              enoughAda "Incorrect inputs for contribution to pool." iPrizeFund
            _ ->
              invalidDatum ()
          -- }}}
      | index == 4 =
          -- {{{ COLLECT PRIZE 
          () -- TODO
          -- }}}
      | otherwise  =
          -- {{{ INVALID DATUM 
          invalidDatum ()
          -- }}}
  in
  Builtins.matchData datum
    constrHandler -- constr
    invalidDatum  -- map
    invalidDatum  -- list
    invalidDatum  -- integer
    invalidDatum  -- bytestring
  -- }}}


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])


valHash :: ValidatorHash
valHash = Scripts.validatorHash validator


scrAddress :: Address
scrAddress = scriptAddress validator
-- }}}


-- CONSTANTS
-- {{{
minAda :: BuiltinInteger
minAda = 1_000_000
-- }}}
