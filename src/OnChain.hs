-- EXTENSIONS
-- {{{
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
-- }}}


-- MODULE
-- {{{
module OnChain where
-- }}}


-- IMPORTS
-- {{{
import           Control.Monad              hiding (fmap)
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Text                  (Text)
import           Plutus.Contract
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import           PlutusTx.AssocMap          (Map)
import qualified PlutusTx.AssocMap          as Map
import qualified PlutusTx.Builtins          as Builtins
import           PlutusTx.Builtins.Internal (BuiltinInteger)
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           PlutusTx.Prelude           (BuiltinByteString)
import           PlutusTx.Sqrt              (Sqrt (..), isqrt)
import           Ledger
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Ada                 as Ada
import           Prelude                    (Show (..), String)
-- }}}


-- CONSTANTS
-- {{{
{-# INLINABLE keyHolder #-}
keyHolder :: PubKeyHash
keyHolder = "" -- TODO
-- }}}


-- PROJECT INFO
-- {{{
data ProjectInfo = ProjectInfo
  { piAddress   :: !Address
  , piLabel     :: !BuiltinByteString
  , piRequested :: !Integer
  }

instance Eq ProjectInfo where
  {-# INLINABLE (==) #-}
  a == b =
       (piAddress   a == piAddress   b)
    && (piLabel     a == piLabel     b)
    && (piRequested a == piRequested b)

PlutusTx.makeLift ''ProjectInfo
-- }}}


-- PROJECTS LIST
-- {{{
data ProjectsList = ProjectsList
  { getProjects :: ![ProjectInfo]
  }

PlutusTx.makeLift ''ProjectsList
-- }}}


-- UTILS 
-- {{{
{-# INLINABLE takeSqrt #-}
takeSqrt :: Integer -> Maybe Integer
takeSqrt val =
  -- {{{
  case isqrt val of
    Imaginary ->
      Nothing
    Exactly sqrt ->
      Just sqrt
    Approximately sqrt ->
      Just sqrt
  -- }}}


{-# INLINABLE pluck #-}
-- | If an element of the list satisfies the predicate,
--   that element is plucked from the list, and is returned
--   along with the new list. If no element can satisfy the
--   predicate, @Nothing@ is returned.
pluck :: (a -> Bool) -> [a] -> Maybe (a, [a])
pluck pred xs =
  -- {{{
  let
    go [] (Nothing, _)   = Nothing
    go ys (Just y, acc)  = Just (y, reverse acc ++ ys)
    go (y : ys) (_, acc) =
      if pred y then
        go ys (Just y, acc)
      else
        go ys (Nothing, y : acc)
  in
  go xs (Nothing, [])
  -- }}}


{-# INLINABLE lovelaceCount #-}
lovelaceCount :: Value -> Integer
lovelaceCount val =
  -- {{{
  Ledger.valueOf val Ada.adaSymbol Ada.adaToken
  -- }}}


{-# INLINABLE getSingularAdaCount #-}
getSingularAdaCount :: Value -> Maybe Integer
getSingularAdaCount val =
  -- {{{
  case flattenValue val of
    [(symbol, tokenName, lovelaceCount)] ->
      if symbol == Ada.adaSymbol && tokenName == Ada.adaToken then
        Just lovelaceCount
      else
        Nothing
    _ ->
      Nothing
  -- }}}
-- }}}


-- THE VALIDATORS
-- {{{
-- {{{ PROJECT VALIDATOR 
{-# INLINABLE mkProjectValidator #-}
mkProjectValidator :: ProjectInfo -> () -> () -> ScriptContext -> Bool
mkProjectValidator pi@ProjectInfo {..} _ _ ctx =
  -- {{{
  let
    info = scriptContextTxInfo ctx
  in
  if txSignedBy info keyHolder then
    let
      foldFn acc txIn =
        -- {{{
        let
          out                                       = txInInfoResolved txIn
          outVal                                    = txOutValue out
          flattened                                 = flattenValue txOutVal
          innerFold isValid (cs, TokenName tn, amt) =
            -- {{{
            if cs == Ada.adaSymbol then
              isValid
            else if cs == voteTokenCurrencySymbol (PubKeyHash tn) pi then
              isValid
            else
              traceError "Invalid token."
            -- }}}
          inputValueIsValid                         =
            -- {{{
            foldl' innerFold True flattened
            -- }}}
        in
        if inputValueIsValid then
          outVal <> acc
        else
          traceError "Invalid input value."
        -- }}}
      allInputs       = txInfoInputs info
      supposedOutputV = foldl' foldFn mempty allInputs
    in
    case txInfoOutputs info of
      [o] ->
        traceIfFalse "Invalid output." $ txOutValue o == supposedOutputV
      _   ->
       traceError "There should be exactly one output."
  else
    traceError "Unauthorized."
  -- }}}

-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data Project
instance Scripts.ValidatorTypes Project where
  type DatumType    Project = ()
  type RedeemerType Project = ()

{-# INLINABLE typedProjectValidator #-}
typedProjectValidator :: ProjectInfo -> Scripts.TypedValidator Project
typedProjectValidator pi =
  -- {{{
  Scripts.mkTypedValidator @Project
    ( $$(PlutusTx.compile [|| mkProjectValidator ||])
      `PlutusTx.applyCode`
      (PlutusTx.liftCode pi)
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()
  -- }}}

{-# INLINABLE projectValidator #-}
projectValidator :: ProjectInfo -> Validator
projectValidator = Scripts.validatorScript . typedProjectValidator

projectValidatorHash :: ProjectInfo -> ValidatorHash
projectValidatorHash = Scripts.validatorHash . typedProjectValidator

{-# INLINABLE projectAddress #-}
projectAddress :: ProjectInfo -> Address
projectAddress = scriptAddress . projectValidator
-- }}}
-- }}}


-- {{{ VOTE TOKEN MINTING POLICY 
{-# INLINABLE mkTokenPolicy #-}
mkVoteTokenPolicy :: PubKeyHash
                  -> ProjectInfo
                  -> ()
                  -> ScriptContext
                  -> Bool
mkVoteTokenPolicy donor ProjectInfo {..} () ctx =
  -- {{{
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx
  in
  undefined -- TODO
  -- }}}


{-# INLINABLE voteTokenPolicy #-}
voteTokenPolicy :: PubKeyHash -> ProjectInfo -> Scripts.MintingPolicy
voteTokenPolicy donor pi = mkMintingPolicyScript $
  -- {{{
  $$(PlutusTx.compile [|| \donor' pi' -> Scripts.wrapMintingPolicy $ mkVoteTokenPolicy donor' pi' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode donor
  `PlutusTx.applyCode`
  PlutusTx.liftCode pi
  -- }}}


{-# INLINABLE voteTokenCurrencySymbol #-}
voteTokenCurrencySymbol :: PubKeyHash -> ProjectInfo -> CurrencySymbol
voteTokenCurrencySymbol donor =
  -- {{{
  scriptCurrencySymbol . voteTokenPolicy donor
  -- }}}
-- }}}


-- {{{ QVF VALIDATOR 
{-# INLINABLE mkQVFValidator #-}
mkQVFValidator :: ProjectsList
               -> ()
               -> ()
               -> ScriptContext
               -> Bool
mkQVFValidator (ProjectsList pis) _ _ ctx =
  -- {{{
  let
    info = scriptContextTxInfo ctx
    pisWithScriptAddrs =
      -- {{{
      foldr
        (\pi acc -> (projectAddress pi, pi) : acc)
        []
        pis
      -- }}}
    foldFn txIn (remainingPIs, fundPool, pisToVotes) =
      -- {{{
      let
        resolved       = txInInfoResolved txIn
        sourceAddr     = txOutAddress resolved
        mPIAndNewPIs   = pluck ((sourceAddr ==) . fst) remainingPIs
        inputLovelaces =
          -- {{{
          case getSingularAdaCount (txOutValue resolved) of
            Just lovelaceCount ->
              lovelaceCount
            Nothing            ->
              traceError "Only ADA can be spent."
          -- }}}
        newFundPool    = fundPool ++ inputLovelaces
      in
      case mPIAndNewPIs of
        Nothing ->
          -- {{{
          ( remainingPIs
          , newFundPool
          , pisToVotes
          )
          -- }}}
        Just ((_, pi), newPIs) ->
          -- {{{
          case takeSqrt inputLovelaces of
            Just voteCount ->
              -- {{{
              ( newPIs
              , newFundPool
              , (pi, voteCount) : pisToVotes
              )
              -- }}}
            Nothing        ->
              -- {{{
              traceError "Bad input value." -- Shouldn't happen.
              -- }}}
          -- }}}
      -- }}}
    (didntReceiveDonation, allFunds, votesOfPIs) =
      foldr foldFn (pisWithScriptAddrs, 0, []) (txInfoInputs info)
    correctlyDistributed = True -- TODO
  in
     traceIfFalse "Unauthorized." (txSignedBy info keyHolder)
  && traceIfFalse "Improper distribution." correctlyDistributed
  -- }}}


-- TEMPLATE HASKELL, BOILERPLATE, ETC. 
-- {{{
data QVF
instance Scripts.ValidatorTypes QVF where
  type DatumType    QVF = ()
  type RedeemerType QVF = ()


typedQVFValidator :: ProjectsList -> Scripts.TypedValidator QVF
typedQVFValidator pis =
  -- {{{
  Scripts.mkTypedValidator @QVF
    ( PlutusTx.applyCode
        $$(PlutusTx.compile [|| mkQVFValidator ||])
        (PlutusTx.liftCode pis)
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()
  -- }}}


qvfValidator :: ProjectsList -> Validator
qvfValidator = Scripts.validatorScript . typedQVFValidator


qvfValidatorHash :: ProjectsList -> ValidatorHash
qvfValidatorHash = Scripts.validatorHash . typedQVFValidator


qvfAddress :: ProjectsList -> Address
qvfAddress = scriptAddress . qvfValidator
-- }}}
-- }}}
-- }}}


-- TxInfo = TxInfo
--   { txInfoInputs      :: [TxInInfo]
--   , txInfoOutputs     :: [TxOut]
--   , txInfoFee         :: Value
--   , txInfoMint        :: Value
--   , txInfoDCert       :: [DCert]
--   , txInfoWdrl        :: [(StakingCredential, Integer)]
--   , txInfoValidRange  :: POSIXTimeRange
--   , txInfoSignatories :: [PubKeyHash]
--   , txInfoData        :: [(DatumHash, Datum)]
--   , txInfoId          :: TxId
--   }

-- TxInInfo = TxInInfo
--   { txInInfoOutRef    :: TxOutRef
--   , txInInfoResolved  :: TxOut
--   }

-- TxOut = TxOut
--   { txOutAddress      :: Address
--   , txOutValue        :: Value
--   , txOutDatumHash    :: Maybe DatumHash
--   }


