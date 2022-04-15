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
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show, String, toInteger, (^))

-- Flow for this on playground should work as follows
-- 1)One wallet will start the fund with a prize for the winner of this fund
-- 2)User will vote in this fund. The cost to cast a vote to the same project is the amount of vote they cast in the past to the power of two
-- Example :: It will cost 1 vote to vote for project A in Fund 1.For the wallet to cast another vote again for the same project in the same fund , the
-- cost will be 4 , to cast another one it will be 9 , to cast another one it will be 16.
-- 3)After round has ended , user can get a refund on the money they used to vote
-- 4)The winner of the round can collect their winnings , if they recived the most amount of vote

data VotingDatum = VotingDatum

  { -- Do we want to allow here only 1 PubKeyHash? Maybe a fund can include multiple projects right?
    -- so maybe it should be [PaymentPubKeyHash]?
    projectPubKey :: PaymentPubKeyHash,
    amount :: Integer,
    fund :: Integer,

    -- Here I would recommend you to not use paymentPubKey as a name, as it is also a function in PLutus Haddock
    -- use another descriptive name
    paymentPubKey :: PaymentPubKeyHash

    -- deadlineRound :: POSIXTime
    -- we should also include here a deadline
  }
  deriving (Show)

PlutusTx.unstableMakeIsData ''VotingDatum

-- There needs to be more work done on the validator script. I am not sure if where the logic of some of the code should be located. Should
-- the logic of accepting the vote, determining if they can recieve the fund etc.... be in the validator script , or should it be off-chain in the wallet.
-- Having trouble deciding what part should be on-chain and what part should not , work-in-progress .........
{-# INLINEABLE mkValidator #-}
mkValidator :: VotingDatum -> () -> ScriptContext -> Bool
mkValidator dat _ ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = True

data Voting

instance Scripts.ValidatorTypes Voting where
  type DatumType Voting = VotingDatum
  type RedeemerType Voting = ()

typedValidator :: Scripts.TypedValidator Voting
typedValidator =
  Scripts.mkTypedValidator @Voting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @VotingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- Params for each endpoint(action) that the user can do

-- Here the Start params, should match with the VotingDatum.
-- Basically the start params should be what u already have, and your VoteParams combined in one (and keep the separate VoteParams ofc)
data StartParams = StartParams
  { spMatchAmount :: !Integer, -- i assume this is the amount from the Voting datum?
    -- there should also be projectKeyPubHash here as well
    spRoundEnd :: !POSIXTime,
    spFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data VoteParams = VoteParams
  { vpAmount :: !Integer,
    vpFund :: !Integer,
    vpProjectPubKey :: !PaymentPubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CollectParams = CollectParams
  { cpFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RefundParams = RefundParams
  { rpFund :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type VoteSchema =
  Endpoint "start" StartParams
    .\/ Endpoint "vote" VoteParams
    .\/ Endpoint "refund" RefundParams
    .\/ Endpoint "collect" CollectParams

-- Below is the function to start a fund, it is under development
-- start :: String
-- start = ""
-- The start function will be super easy once we properly define the Data Types

--  Function to cast their vote and the project they are voting for.
vote :: forall w s e. AsContractError e => VoteParams -> Contract w s e ()
vote vp = do
  pkh <- ownPaymentPubKeyHash
  previousUtxos <- Map.filter (isSuitable pkh (vpFund vp) (vpProjectPubKey vp)) <$> utxosAt scrAddress
  if (vpAmount vp) /= (((toInteger (Map.size previousUtxos) + 1) ^ 2) * 1000000000)
    then logInfo @String $ printf "not right amount to vote"
    else do
      let dat =
            VotingDatum
              { amount = vpAmount vp,
                projectPubKey = vpProjectPubKey vp,
                fund = vpFund vp,
                paymentPubKey = pkh
              }
          tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ vpAmount vp
      ledgerTx <- submitTxConstraints typedValidator tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ printf "vote success "
  where
    isSuitable :: PaymentPubKeyHash -> Integer -> PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isSuitable pkh fundRound projectKey o = case _ciTxOutDatum o of
      Left _ -> False
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d -> paymentPubKey d == pkh && fund d == fundRound && projectPubKey d == projectKey

-- Function for voters to get their refund
refund :: forall w s e. AsContractError e => RefundParams -> Contract w s e ()
refund rp = do
  pkh <- ownPaymentPubKeyHash
  utxos <- Map.filter (isSuitable pkh (rpFund rp)) <$> utxosAt scrAddress
  if Map.null utxos
    then logInfo @String $ "no refunds available"
    else do
      let orefs = fst <$> Map.toList utxos
          lookups =
            Constraints.unspentOutputs utxos
              <> Constraints.otherScript validator
          tx :: TxConstraints Void Void
          tx =
            mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "collected refunds"
  where
    isSuitable :: PaymentPubKeyHash -> Integer -> ChainIndexTxOut -> Bool
    isSuitable pkh fundRound o = case _ciTxOutDatum o of
      Left _ -> False
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d -> paymentPubKey d == pkh && fund d == fundRound

--Function for winner to collect winnings , it is under development
--collect :: ()
--collect () = ()

endpoints :: Contract () VoteSchema Text ()
endpoints = awaitPromise (vote' `select` refund') >> endpoints
  where
    vote' = endpoint @"vote" vote
    refund' = endpoint @"refund" refund

mkSchemaDefinitions ''VoteSchema
