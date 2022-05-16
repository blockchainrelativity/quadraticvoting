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

module QuadraticVFSC where

import Control.Monad hiding (fmap)
import Data.List (groupBy, sortOn)
import Data.Foldable (foldr1)
import Data.Aeson (FromJSON, ToJSON)
import PlutusTx.Sqrt
import Data.Function (on)
import Data.Map as Map
import Data.Text (Text)
import Control.Lens
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
import Data.Tuple
import Text.Printf (printf)
import Prelude (IO, Semigroup (..), Show, String, show, toInteger, Float ,Int,(^), sqrt, fromIntegral, div, read, sum, Num, Ord, snd, fst, unzip, Integral)


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
    vVoteFundIdentifier :: PaymentPubKeyHash
    --vActionName :: BuiltinByteString
  }
  deriving (Show)

instance Eq VotingActionDatum where
  {-# INLINEABLE (==) #-}
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
  {-# INLINEABLE (==) #-}
  b == c =
    (vFundAddress b == vFundAddress c)
      && (vPrizeFund b == vPrizeFund c)

PlutusTx.unstableMakeIsData ''ConToMatchPool
PlutusTx.makeLift ''ConToMatchPool

data QuadraAction = Start | Vote | ContributeToPool | ProjectRegistration | CollectPrize deriving (Show)

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
    Start ->
      case qCreateFund dat of
        Nothing -> True
        Just FundCreationDatum {..} -> traceIfFalse "not correct proportion" (correctProportion vPrizeDistributionRatio)
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

    info :: TxInfo
    info = scriptContextTxInfo ctx

    projectFundsSufficient :: Integer -> Bool
    projectFundsSufficient regFee = regFee >= minFeeRegistration
    
    enoughAda :: Integer -> Bool
    enoughAda votingAda = votingAda >= minVotingAda
    
    correctProportion :: [Integer] -> Bool
    correctProportion proportion = Prelude.sum proportion == 1 
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
  { controllerAddress :: PaymentPubKeyHash, 
    fundAddress :: PaymentPubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data StartFundParams = StartFundParams
  { prizeAmount :: Integer,
    projectLabel :: [BuiltinByteString],
    prizeDistributionRatio :: [Integer]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ProjectSubmitParams = ProjectSubmitParams
  { registrationFee :: Integer,
    projectCategory :: BuiltinByteString,
    fundIdentifier :: PaymentPubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ConToMatchPoolParams = ConToMatchPoolParams
  { fundPaymentAddress:: PaymentPubKeyHash,
    conAmount :: Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data VoteParams = VoteParams
  { projectToVote :: PaymentPubKeyHash
  , adaLoveLaceAmount :: Integer
  , voteFundIdentifier :: PaymentPubKeyHash
  , actionName :: BuiltinByteString
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type QuadraSchema =
  Endpoint "collectPrize" CollectPrizeParams
    .\/ Endpoint "start" StartFundParams
    .\/ Endpoint "submit project" ProjectSubmitParams
    .\/ Endpoint "contribute to pool" ConToMatchPoolParams
    .\/ Endpoint "vote" VoteParams

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
  logInfo @String $ printf "created fund "

submitProject :: forall w s e. AsContractError e => ProjectSubmitParams -> Contract w s e ()
submitProject sp = do
  pkh <- ownPaymentPubKeyHash
  let dat =
        QuadraDatum {
        qSubProject = Just (ProjectSubmitDatum { vProjectOwner = pkh,
              vProjectRegistrationFee = registrationFee sp,
              vProjectCategory = projectCategory sp,
              vFundPayIdentifier = fundIdentifier sp
            }),
        qVoting     = Nothing,
        qCreateFund = Nothing,
        qContrPool  = Nothing
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ (registrationFee sp)
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "created fund "

donateToPool :: forall w s e. AsContractError e => ConToMatchPoolParams -> Contract w s e ()
donateToPool mp = do
  let dat =
        QuadraDatum {
        qContrPool = Just (ConToMatchPool { vFundAddress = fundPaymentAddress mp,
              vPrizeFund = conAmount mp
            }),
        qVoting     = Nothing,
        qSubProject = Nothing,
        qCreateFund  = Nothing
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ (conAmount mp)
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "created fund "


vote :: forall w s e. AsContractError e => VoteParams -> Contract w s e ()
vote vp = do
  pkh <- ownPaymentPubKeyHash
  let dat =
        QuadraDatum {
        qVoting = Just (VotingActionDatum { vProjectToVote = projectToVote vp,
              vVoterPayAddress = pkh ,
              vAdaLovelaceValue = adaLoveLaceAmount vp,
              vVoteFundIdentifier = voteFundIdentifier vp
            }),
        qCreateFund    = Nothing,
        qSubProject = Nothing,
        qContrPool  = Nothing
            }
      tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ (adaLoveLaceAmount vp)
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "created fund "


-- Quadratic Formula & Collect Funds Functionality ---------------
------------------------------------------------------------------

collectPrize :: forall w s. CollectPrizeParams -> Contract w s Text ()
collectPrize CollectPrizeParams {..} = do
  -- Retrieving all datums apart from Voting datums
  initalMatchPool <- Map.map (findInitalAmount fundAddress) <$> utxosAt scrAddress
  donatedMatchPool <- Map.map (findDonateMatchPool fundAddress) <$> utxosAt scrAddress
  projects <- Map.map (findProjects fundAddress) <$> utxosAt scrAddress

  -- Retriving value from datum to get total amount of money in this fund
  let initalAmount =  vPrizeAmount $ snd $ head $ Map.toList (initalMatchPool)  
  let donatedAmount = [vPrizeFund (snd x) | x <- Map.toList donatedMatchPool ]
  let totalFundAmount = (sum donatedAmount) + initalAmount

  -- Distribute fund to each catgory of projects
  let ratio  = vPrizeDistributionRatio $ snd $ head $ Map.toList initalMatchPool
  let distributedRatio = [totalFundAmount * x | x <- ratio]


  -- Retrieving all the datums related to Votes
  votes <- findAttachedDatums fundAddress
  let numberOfVotes          = length votes 
  let listOfVoteDatums       = getThird votes
  let groupVotingAmounts     = [ (vAdaLovelaceValue x, vProjectToVote x) | x <- listOfVoteDatums] 
  let parseVotingPowers      = [ (show $ getSquareRoot ((x^. _1) `div` 1000000), x^. _2)| x <- groupVotingAmounts]
  let finalGroupVotingPowers = [ (read $ x^._1 :: Integer ,x^._2)| x <- parseVotingPowers ]
  let listOverallPowers      = [ x^._1 | x <- finalGroupVotingPowers]
  let totalValueVP       = Prelude.sum listOverallPowers
  let votingPowersGroupedByProject = sumByProject finalGroupVotingPowers

  -- Loging outputs
  logInfo @String $ printf $ (show listOfVoteDatums)
  logInfo @String $ printf "prize collected"

  where
     -- extract third element of tuple
    getThird :: [(a,b,c)] -> [c]
    getThird [(a,b,c)] = [c]

    -- Get square root of x
    getSquareRoot :: Integer -> Float -- Added Solution
    getSquareRoot x = (sqrt . fromIntegral) x -- sqrt( fromIntegral(x))


    --   Parse the list of tuples, group it by project so we can calculate VP per project
    --   function -> map (mapFst head) $ map unzip $ groupBy ((==) `on` fst) $ sort groupVotingPowers
    --   example list format :: [("Nick",10),("Nick",20),("George",10),("George",20)]
    --   example result output: [("Nick",30),("George",30)]

    sumByProject :: (Num a, AdditiveSemigroup a, AdditiveSemigroup Integer, Prelude.Ord b, Eq b) => [(a, b)] -> [(a, b)] -- Added Solution
    sumByProject dict 
        = fmap sumValues              
        $ groupBy equalName           
        $ sortOn Prelude.snd dict        
        where 
          equalName (_, n) (_, m) =  n == m   
          sumValues = foldr1 sumTuple       
          sumValues :: (AdditiveSemigroup a) => [(a, b1)] -> (a, b1) -- Added solution
          sumTuple (v2, n) (v1 , _) = (v1 + v2, n) 
    
    -- mapFst f (a, b) = (f a, b)
    --groupedVotingPowersfn vps = Map.map (mapFst head) $ Map.map unzip $ groupBy ((==) `on` Prelude.fst) $ sort vps





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
endpoints = awaitPromise (collectPrize' `select` start' `select` vote' `select` submit' `select` enroll') >> endpoints
  where
    collectPrize' = endpoint @"collectPrize" collectPrize
    start' = endpoint @"start" start
    vote' = endpoint @"vote" vote
    submit' = endpoint @"submit project" submitProject
    enroll' = endpoint @"contribute to pool" donateToPool
    
findInitalAmount :: PaymentPubKeyHash -> ChainIndexTxOut -> FundCreationDatum
findInitalAmount fundId o = case _ciTxOutDatum o of
  Left _ -> Prelude.error "not found"
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> Prelude.error "not found"
    Just d@QuadraDatum{..} -> case qCreateFund of 
      Nothing -> Prelude.error "not found"
      Just v@FundCreationDatum{..} -> case vFundOwner of 
        fundId -> v

findDonateMatchPool :: PaymentPubKeyHash -> ChainIndexTxOut -> ConToMatchPool
findDonateMatchPool fundId o = case _ciTxOutDatum o of
  Left _ -> Prelude.error "not found"
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> Prelude.error "not found"
    Just d@QuadraDatum{..} -> case qContrPool of 
      Nothing -> Prelude.error "not found"
      Just v@ConToMatchPool{..} -> case vFundAddress of 
        fundId -> v

findProjects :: PaymentPubKeyHash -> ChainIndexTxOut -> ProjectSubmitDatum
findProjects fundId o = case _ciTxOutDatum o of
  Left _ -> Prelude.error "not found"
  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
    Nothing -> Prelude.error "not found"
    Just d@QuadraDatum{..} -> case qSubProject of 
      Nothing -> Prelude.error "not found"
      Just v@ProjectSubmitDatum{..} -> case vFundPayIdentifier of
        fundId -> v

mkSchemaDefinitions ''QuadraSchema

