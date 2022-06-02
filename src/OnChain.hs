{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module OnChain where


import           Control.Monad              hiding (fmap)
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Text                  (Text)
import           Plutus.Contract
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins          as Builtins
import           PlutusTx.Builtins.Internal (BuiltinInteger)
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           PlutusTx.Prelude           (BuiltinByteString)
import           Ledger
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Ada                 as Ada
import           Prelude                    (Show (..), String)


-- FUND CREATION INFO
-- {{{
data FundCreationInfo = FundCreationInfo
  { fciFundOwner    :: !PaymentPubKeyHash
  , fciPrizeAmount  :: !BuiltinInteger
  , fciProjectLabel :: ![BuiltinByteString]
  } deriving (Show)

instance Eq FundCreationInfo where
  {-# INLINABLE (==) #-}
  a == b =
       (fciFundOwner    a == fciFundOwner    b)
    && (fciPrizeAmount  a == fciPrizeAmount  b)
    && (fciProjectLabel a == fciProjectLabel b)

PlutusTx.unstableMakeIsData ''FundCreationInfo
PlutusTx.makeLift ''FundCreationInfo
-- }}}


-- PROJECT SUBMISSION INFO
-- {{{
data ProjectSubmissionInfo = ProjectSubmissionInfo
  { psiProjectOwner           :: !PaymentPubKeyHash
  , psiProjectRegistrationFee :: !BuiltinInteger
  , psiProjectCategory        :: !BuiltinByteString
  , psiFundPayIdentifier      :: !PaymentPubKeyHash
  } deriving (Show)

instance Eq ProjectSubmissionInfo where
  {-# INLINABLE (==) #-}
  a == b =
       (psiProjectOwner           a == psiProjectOwner           b)
    && (psiProjectRegistrationFee a == psiProjectRegistrationFee b)
    && (psiProjectCategory        a == psiProjectCategory        b)
    && (psiFundPayIdentifier      a == psiFundPayIdentifier      b)

PlutusTx.unstableMakeIsData ''ProjectSubmissionInfo
PlutusTx.makeLift ''ProjectSubmissionInfo
-- }}}


-- VOTING INFO
-- {{{
data VotingInfo = VotingInfo
  { viProjectToVote      :: !PaymentPubKeyHash
  , viVoterPayAddress    :: !PaymentPubKeyHash
  , viAdaLovelaceValue   :: !BuiltinInteger
  , viVoteFundIdentifier :: !PaymentPubKeyHash
  } deriving (Show)

instance Eq VotingInfo where
  {-# INLINABLE (==) #-}
  a == b =
       (viProjectToVote      a == viProjectToVote      b)
    && (viVoterPayAddress    a == viVoterPayAddress    b)
    && (viAdaLovelaceValue   a == viAdaLovelaceValue   b)
    && (viVoteFundIdentifier a == viVoteFundIdentifier b)

PlutusTx.unstableMakeIsData ''VotingInfo
PlutusTx.makeLift ''VotingInfo
-- }}}


-- CONTRIBUTION INFO
-- {{{
data ContributionInfo = ContributionInfo
  { ciFundAddress :: !PaymentPubKeyHash
  , ciPrizeFund   :: !BuiltinInteger
  }
  deriving (Show)

instance Eq ContributionInfo where
  {-# INLINABLE (==) #-}
  a == b =
       (ciFundAddress a == ciFundAddress b)
    && (ciPrizeFund   a == ciPrizeFund   b)

PlutusTx.unstableMakeIsData ''ContributionInfo
PlutusTx.makeLift ''ContributionInfo
-- }}}


-- THE DATUM
-- {{{
data QVFDatum
  = CreateFund    FundCreationInfo
  | SubmitProject ProjectSubmissionInfo
  | Vote          VotingInfo
  | Contribute    ContributionInfo
  | CollectPrize
  deriving (Show)

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  CreateFund    info0 == CreateFund    info1 = info0 == info1
  SubmitProject info0 == SubmitProject info1 = info0 == info1
  Vote          info0 == Vote          info1 = info0 == info1
  Contribute    info0 == Contribute    info1 = info0 == info1
  CollectPrize        == CollectPrize        = True
  _                   == _                   = False

PlutusTx.unstableMakeIsData ''QVFDatum
PlutusTx.makeLift ''QVFDatum
-- }}}


-- THE VALIDATOR
-- {{{
{-# INLINABLE mkValidator #-}
mkValidator :: QVFDatum -> () -> ScriptContext -> Bool
mkValidator datum _ _ =
  -- {{{
  let
    enoughAda :: BuiltinInteger -> Bool
    enoughAda = Builtins.lessThanInteger minAda
  in
  case datum of
    CreateFund    FundCreationInfo      {..} ->
      -- {{{
      traceIfFalse "Not enough ADA." $
        enoughAda fciPrizeAmount
      -- }}}
    Vote          VotingInfo            {..} ->
      -- {{{
      traceIfFalse "not enough Ada to vote" $
        enoughAda viAdaLovelaceValue
      -- }}}
    Contribute    ContributionInfo      {..} ->
      -- {{{
      traceIfFalse "not correct inputs to contribute to pool" $
        enoughAda ciPrizeFund
      -- }}}
    SubmitProject ProjectSubmissionInfo {..} ->
      -- {{{
      traceIfFalse "Not enough funds to register project." $
        enoughAda psiProjectRegistrationFee
      -- }}}
    CollectPrize                             ->
      -- {{{
      True -- TODO
      -- }}}
  -- }}}


data QVF

instance Scripts.ValidatorTypes QVF where
  type DatumType    QVF = QVFDatum
  type RedeemerType QVF = ()


typedValidator :: Scripts.TypedValidator QVF
typedValidator =
  Scripts.mkTypedValidator @QVF
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @QVFDatum @()


validator :: Validator
validator = Scripts.validatorScript typedValidator


valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator


scrAddress :: Address
scrAddress = scriptAddress validator
-- }}}


-- CONSTANTS
-- {{{
minAda :: BuiltinInteger
minAda = 1_000_000
-- }}}
