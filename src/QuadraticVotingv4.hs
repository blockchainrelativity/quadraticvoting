{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import PlutusTx (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude (BuiltinByteString)
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show, String, show, toInteger, (^))

data FundCreationDatum = FundCreationDatum
  { vFundOwner :: PaymentPubKeyHash,
    vPrizeAmount :: Integer,
    vProjectLabel :: [BuiltinByteString],
    vPrizeDistributionRatio :: [Integer]
  }
  deriving (Show)

instance Eq FundCreationDatum where
  {-# INLINEABLE (==) #-}
  a == b =
    (vFundOwner a == vFundOwner b)
      && (vPrizeAmount a == vPrizeAmount b)
      && (vProjectLabel a == vProjectLabel b)
      && (vPrizeDistributionRatio a == vPrizeDistributionRatio b)

PlutusTx.unstableMakeIsData ''FundCreationDatum
PlutusTx.makeLift ''FundCreationDatum

data ProjectSubmitDatum = ProjectSubmitDatum
  { vProjectOwner           :: PaymentPubKeyHash,
    vProjectRegistrationFee :: Integer,
    vProjectCategory        :: BuiltinByteString,
    vFundPayIdentifier      :: PaymentPubKeyHash
  }
  deriving (Show)

minFeeRegistration :: Integer
minFeeRegistration = 1000000

instance Eq ProjectSubmitDatum where
  {-# INLINEABLE (==) #-}
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
    vActionName :: BuiltinByteString
  }
  deriving (Show)

instance Eq VotingActionDatum where
  {-# INLINEABLE (==) #-}
  b == c =
    (vProjectToVote b == vProjectToVote c)
      && (vVoterPayAddress b == vVoterPayAddress c)
      && (vAdaLovelaceValue b == vAdaLovelaceValue c)
      && (vActionName b == vActionName c)

PlutusTx.unstableMakeIsData ''VotingActionDatum
PlutusTx.makeLift ''VotingActionDatum

minVotingAda :: Integer
minVotingAda = 10000000

data ConToMatchPool = ConToMatchPool
  { vFundAddress :: PaymentPubKey,
    vPrizeFund :: Integer
  }
  deriving (Show)

instance Eq ConToMatchPool where
  {-# INLINEABLE (==) #-}
  b == c =
    (vFundAddress b == vFundAddress c)
      && (vPrizeFund b == vPrizeFund c)

PlutusTx.unstableMakeIsData ''ConToMatchPool
PlutusTx.makeLift ''ConToMatchPool

--data QuadraAction = SubProject ProjectSubmitDatum | VoteProject VotingActionDatum | ContribPool ConToMatchPool deriving (Show)
-- Probably the above QuadraAuction was wrong here,
-- It is better to do it like this: 

data QuadraAction =  Vote | ContributeToPool | ProjectRegistration | CollectPrize deriving (Show)

PlutusTx.unstableMakeIsData ''QuadraAction
PlutusTx.makeLift ''QuadraAction

data QuadraDatum = QuadraDatum
  { qCreateFund :: !(Maybe FundCreationDatum),
    qVoting :: !(Maybe VotingActionDatum),
    qSubProject :: !(Maybe ProjectSubmitDatum),
    qContrPool :: !(Maybe ConToMatchPool)
  }
  deriving (Show)

instance Eq QuadraDatum where
  {-# INLINEABLE (==) #-}
  b == c =
    (qCreateFund b == qCreateFund c)
      && (qVoting b == qVoting c)
      && (qSubProject b == qSubProject c)
      && (qContrPool b == qContrPool c)

PlutusTx.unstableMakeIsData ''QuadraDatum
PlutusTx.makeLift ''QuadraDatum

{-# INLINEABLE mkValidator #-}
mkValidator :: QuadraDatum -> QuadraAction -> ScriptContext -> Bool
mkValidator dat redeemer ctx = 
   case redeemer of
      ProjectRegistration ->
        case qSubProject dat of
           Nothing -> True
           Just ProjectSubmitDatum{..}  -> traceIfFalse "not enough funds to register project" (projectFundsSufficient vProjectRegistrationFee)

      Vote ->
        case qVoting dat of
           Nothing -> True
           Just VotingActionDatum{..}    -> traceIfFalse "not enough Ada to vote" (enoughVotingAda vAdaLovelaceValue)
      -- here we will now start with other action types
      -- Vote             -> toimplement...
      -- COntributeToPool -> toimplement...
   where

     info :: TxInfo
     info = scriptContextTxInfo ctx

     projectFundsSufficient :: Integer -> Bool
     projectFundsSufficient regFee = regFee >= minFeeRegistration

     enoughVotingAda         :: Integer -> Bool
     enoughVotingAda votingAda = votingAda >= minVotingAda


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

type QuadraSchema =
  Endpoint "start" ()

mkSchemaDefinitions ''QuadraSchema
