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
  { vProjectOwner :: PaymentPubKeyHash,
    vProjectRegistrationFee :: Integer,
    vProjectCategory :: BuiltinByteString,
    vFundPayIdentifier :: PaymentPubKeyHash
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
    vVoteFundIdentifier :: PaymentPubKeyHash,
    vActionName :: BuiltinByteString
  }
  deriving (Show)

instance Eq VotingActionDatum where
  {-# INLINEABLE (==) #-}
  b == c =
    (vProjectToVote b == vProjectToVote c)
      && (vVoterPayAddress b == vVoterPayAddress c)
      && (vAdaLovelaceValue b == vAdaLovelaceValue c)
      && (vVoteFundIdentifier b == vVoteFundIdentifier c)
      && (vActionName b == vActionName c)

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
  {-# INLINEABLE (==) #-}
  b == c =
    (vFundAddress b == vFundAddress c)
      && (vPrizeFund b == vPrizeFund c)

PlutusTx.unstableMakeIsData ''ConToMatchPool
PlutusTx.makeLift ''ConToMatchPool

-- QUESTION ======= do we really need the quadra action ? We would be able to tell
-- What the action the user is doing just by looking at the datum right ?
-- If qCreateFund is filled out and the others is not , then it means it is
-- creating a fund , if qvoting is filled out and the others are not
-- that means it is voting etc.......

data QuadraAction = Vote | ContributeToPool | ProjectRegistration | CollectPrize deriving (Show)

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
        Just ProjectSubmitDatum {..} -> traceIfFalse "not enough funds to register project" (projectFundsSufficient vProjectRegistrationFee)
    Vote ->
      case qVoting dat of
        Nothing -> True
        Just VotingActionDatum {..} -> traceIfFalse "not enough Ada to vote" (enoughAda vAdaLovelaceValue)
    ContributeToPool ->
      case qContrPool dat of
        Nothing -> True
        Just ConToMatchPool {..} -> traceIfFalse "not correct inputs to contribute to pool" (enoughAda vPrizeFund)
  where
    -- here we will now start with other action types
    -- Vote             -> toimplement...
    -- COntributeToPool -> toimplement...

    info :: TxInfo
    info = scriptContextTxInfo ctx

    projectFundsSufficient :: Integer -> Bool
    projectFundsSufficient regFee = regFee >= minFeeRegistration

    enoughAda :: Integer -> Bool
    enoughAda votingAda = votingAda >= minVotingAda

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

data CollectPrizeParams = CollectPrizeParams
  { controllerAddress :: PaymentPubKeyHash, --(Snapbrillia's first)
    fundAddress :: PaymentPubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data StartFundParams = StartFundParams
  { prizeAmount :: Integer,
    projectLabel :: [BuiltinByteString],
    prizeDistributionRatio :: [Integer]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type QuadraSchema =
  Endpoint "collectPrize" CollectPrizeParams
    .\/ Endpoint "start" StartFundParams

---The function that i am struggling with. I am defining the values like this.
start :: forall w s e. AsContractError e => StartFundParams -> Contract w s e ()
start sp = do
  pkh <- ownPaymentPubKeyHash
  let dat =
        QuadraDatum {
        qCreateFund = Just (FundCreationDatum { vFundOwner = pkh,
              vPrizeAmount = prizeAmount sp,
              vPrizeDistributionRatio = prizeDistributionRatio sp,
              vProjectLabel = projectLabel sp
            }),
        qVoting     = Nothing,
        qSubProject = Nothing,
        qContrPool  = Nothing
        
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ (prizeAmount sp)
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "created fund %d"

collectPrize :: forall w s. CollectPrizeParams -> Contract w s Text ()
collectPrize CollectPrizeParams {..} = do
  initalMatchPool <- Map.filter (findInitalAmount fundAddress) <$> utxosAt scrAddress
  donatedMatchPool <- Map.filter (findDonateMatchPool fundAddress) <$> utxosAt scrAddress
  projects <- Map.filter (findProjects fundAddress) <$> utxosAt scrAddress
  votes <- findAttachedDatums fundAddress
  -- Map.filter (findVotes fundAddress) <$> utxosAt scrAddress
  --let countOfVotes = toInteger (Map.size votes)
  --let listOfDatumHashes = [(\datumHashes -> txOutDatum distinctUtxos) | distinctUtxos <- votes]
  --let listOfDatums = [(oref, o)| (oref,o) <- Map.toList votes]
  logInfo @String $ printf "prize collected"

--QUESTION ======= Why is it a paired tuple ?

findAttachedDatums ::
  PaymentPubKeyHash ->
  Contract w s Text [(TxOutRef, ChainIndexTxOut, VotingActionDatum)]
findAttachedDatums fundId = do
  utxos <- utxosAt $ scriptHashAddress valHash
  let theDatums =
        [ (oref, o)
          | (oref, o) <- Map.toList utxos
        ]
  case theDatums of
    [(oref, o)] -> case _ciTxOutDatum o of
      Left _ -> throwError "datum missing"
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwError "datum has wrong type"
        Just d@QuadraDatum {..} -> case qVoting of
          Nothing -> throwError "there is no voting datum passed"
          Just v@VotingActionDatum {..} -> return [(oref, o, v)]
    _ -> throwError "utxos not found"

endpoints :: Contract () QuadraSchema Text ()
endpoints = awaitPromise (collectPrize') >> endpoints
  where
    collectPrize' = endpoint @"collectPrize" collectPrize

findInitalAmount :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
findInitalAmount fundId o = case _ciTxOutDatum o of
  Left _ -> False
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> False
    Just d -> vFundOwner d == fundId

findDonateMatchPool :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
findDonateMatchPool fundId o = case _ciTxOutDatum o of
  Left _ -> False
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> False
    Just d -> vFundAddress d == fundId

findProjects :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
findProjects fundId o = case _ciTxOutDatum o of
  Left _ -> False
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> False
    Just d -> vFundPayIdentifier d == fundId

findVotes :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
findVotes fundId o = case _ciTxOutDatum o of
  Left _ -> False
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> False
    Just d -> vVoteFundIdentifier d == fundId

mkSchemaDefinitions ''QuadraSchema