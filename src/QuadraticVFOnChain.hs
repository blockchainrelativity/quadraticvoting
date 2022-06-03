{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
--{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
--{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module QuadraticVFOnChain (validationScriptShortBs, 
                           validationScript, 
                           FundCreationDatum (..),
                           ProjectSubmitDatum (..),
                           VotingActionDatum (..),
                           ConToMatchPool (..), 
                           realValidator, 
                           typedValidator, 
                           script,
                           valHash) where

import Control.Monad hiding (fmap)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Data.List (groupBy, sortOn)
import Data.Foldable (foldr1)
import Data.Aeson (FromJSON, ToJSON)
import PlutusTx.Sqrt
import Data.Function (on)
import Data.Map as Map
import Data.Text (Text)
import Control.Lens
import Data.Void (Void)
import GHC.Integer
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Address (PaymentPubKeyHash(..))
import Ledger (Address)
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import PlutusTx (Data (..))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude (BuiltinByteString)
import PlutusTx.Prelude hiding (Semigroup (..), unless, Integer)
import Data.Tuple
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show, String, show, toInteger, Float ,Int,(^),(/), sqrt, fromIntegral, div, read, sum, Num, Ord, snd, fst, unzip,Integer, Integral, error, sum, Fractional)


data FundCreationDatum = FundCreationDatum
  { vFundOwner :: PaymentPubKeyHash,
    vPrizeAmount :: Integer,
    vProjectLabel :: [BuiltinByteString]
  }
  deriving (Show)

instance Eq FundCreationDatum where
  {-# INLINABLE (==) #-}
  a == b =
    (vFundOwner a == vFundOwner b)
      && (vPrizeAmount a == vPrizeAmount b)
      && (vProjectLabel a == vProjectLabel b)

PlutusTx.unstableMakeIsData ''FundCreationDatum
PlutusTx.makeLift ''FundCreationDatum

data ProjectSubmitDatum = ProjectSubmitDatum
  { vProjectOwner :: PaymentPubKeyHash,
    vProjectRegistrationFee :: Integer,
    vProjectCategory :: BuiltinByteString,
    vFundPayIdentifier :: PaymentPubKeyHash
  }
  deriving (Show)

minFeeRegistration :: Integer
minFeeRegistration = 1000000

instance Eq ProjectSubmitDatum where
  {-# INLINABLE (==) #-}
  b == c =
    (vProjectOwner b == vProjectOwner c)
      && (vProjectRegistrationFee b == vProjectRegistrationFee c)
      && (vProjectCategory b == vProjectCategory c)
      && (vFundPayIdentifier b == vFundPayIdentifier c)

PlutusTx.unstableMakeIsData ''ProjectSubmitDatum
PlutusTx.makeLift ''ProjectSubmitDatum

data VotingActionDatum = VotingActionDatum
  { vProjectToVote :: PaymentPubKeyHash,
    vVoterPayAddress :: PaymentPubKeyHash,
    vAdaLovelaceValue :: Integer,
    vVoteFundIdentifier :: PaymentPubKeyHash
    --vActionName :: BuiltinByteString
  }
  deriving (Show)

instance Eq VotingActionDatum where
  {-# INLINABLE (==) #-}
  b == c =
    (vProjectToVote b == vProjectToVote c)
      && (vVoterPayAddress b == vVoterPayAddress c)
      && (vAdaLovelaceValue b == vAdaLovelaceValue c)
      && (vVoteFundIdentifier b == vVoteFundIdentifier c)

PlutusTx.unstableMakeIsData ''VotingActionDatum
PlutusTx.makeLift ''VotingActionDatum

minVotingAda :: Integer
minVotingAda = 10000000

data ConToMatchPool = ConToMatchPool
  { vFundAddress :: PaymentPubKeyHash,
    vPrizeFund :: Integer
  }
  deriving (Show)

instance Eq ConToMatchPool where
  {-# INLINABLE (==) #-}
  b == c =
    (vFundAddress b == vFundAddress c)
      && (vPrizeFund b == vPrizeFund c)

PlutusTx.unstableMakeIsData ''ConToMatchPool
PlutusTx.makeLift ''ConToMatchPool


-- Numbering the actions, to use Redeemer as a simple integer in transaction construction.
-- let createFundAction   = 1
-- let voteAction         = 2
-- let contributeAction   = 3
-- let registerAction     = 4
-- let collectPrizeAction = 5

type QuadraAction = Integer 
    --Start | Vote | ContributeToPool | ProjectRegistration | CollectPrize deriving (Show)

-- PlutusTx.unstableMakeIsData ''QuadraAction
-- PlutusTx.makeLift ''QuadraAction

data QuadraDatum = QuadraDatum
  { qCreateFund :: !(Maybe FundCreationDatum),
    qVoting :: !(Maybe VotingActionDatum),
    qSubProject :: !(Maybe ProjectSubmitDatum),
    qContrPool :: !(Maybe ConToMatchPool)
  }
  deriving (Show)

instance Eq QuadraDatum where
  {-# INLINABLE (==) #-}
  b == c =
    (qCreateFund b == qCreateFund c)
      && (qVoting b == qVoting c)
      && (qSubProject b == qSubProject c)
      && (qContrPool b == qContrPool c)

PlutusTx.unstableMakeIsData ''QuadraDatum
PlutusTx.makeLift ''QuadraDatum

{-# INLINABLE mkValidator #-}
mkValidator :: QuadraDatum -> QuadraAction -> ScriptContext -> Bool
mkValidator dat redeemer ctx
  | redeemer == 1 = case qCreateFund dat of 
                      Nothing -> True
                      Just FundCreationDatum{..} -> traceIfFalse "not enough ADA" (enoughAda vPrizeAmount)
  | redeemer == 4 = case qSubProject dat of
                      Nothing -> True
                      Just ProjectSubmitDatum{..} -> traceIfFalse "not enough funds to register project" (projectFundsSufficient vProjectRegistrationFee)
  | redeemer == 2 = case qVoting dat of
                      Nothing -> True
                      Just VotingActionDatum{..} -> traceIfFalse "not enough Ada to vote" (enoughAda vAdaLovelaceValue)
  | redeemer == 3 = case qContrPool dat of
                      Nothing -> True
                      Just ConToMatchPool{..} -> traceIfFalse "not correct inputs to contribute to pool" (enoughAda vPrizeFund)
 
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    projectFundsSufficient :: Integer -> Bool
    projectFundsSufficient regFee = regFee >= 1000000
    
    enoughAda :: Integer -> Bool
    enoughAda votingAda = votingAda >= 1000000
    
    -- correctProportion :: [Float] -> Bool
    -- correctProportion proportion = Prelude.sum proportion == 1.0 

data QuadraVoting

instance Scripts.ValidatorTypes QuadraVoting where
  type RedeemerType QuadraVoting = QuadraAction
  type DatumType QuadraVoting = QuadraDatum

typedValidator :: Scripts.TypedValidator QuadraVoting
typedValidator =
  Scripts.mkTypedValidator @QuadraVoting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @QuadraDatum @QuadraAction

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

realValidator :: Ledger.Validator
realValidator = Scripts.validatorScript typedValidator

script :: Plutus.Script
script = Plutus.unValidatorScript realValidator

validationScriptShortBs :: SBS.ShortByteString
validationScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

validationScript :: PlutusScript PlutusScriptV1
validationScript = PlutusScriptSerialised validationScriptShortBs