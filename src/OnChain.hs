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


-- PROJECT INFO
-- {{{
data ProjectInfo = ProjectInfo
  { piAddress :: !Address
  , piLabel   :: !BuiltinByteString
  } deriving (Show)

instance Eq ProjectInfo where
  {-# INLINABLE (==) #-}
  a == b =
       (piAddress a == piAddress b)
    && (piLabel   a == piLabel   b)

PlutusTx.unstableMakeIsData ''ProjectInfo
-- }}}


-- THE DATUM
-- {{{
data QVFDatum
  = Voting ProjectInfo
  | Contribution
  deriving (Show)

instance Eq QVFDatum where
  {-# INLINABLE (==) #-}
  Voting info0 == Voting info1 = info0 == info1
  Contribution == Contribution = True
  _            == _            = False

PlutusTx.makeIsDataIndexed ''QVFDatum
  [ ('Voting      , 0)
  , ('Contribution, 1)
  ]
-- }}}


-- THE VALIDATOR
-- {{{
{-# INLINABLE mkValidator #-}
mkValidator :: PaymentPubKeyHash
            -> QVFDatum
            -> ()
            -> ScriptContext
            -> Bool
mkValidator fundOwner datum () ScriptContext {..} =
  -- {{{
  if txSignedBy scriptContextTxInfo (unPaymentPubKeyHash fundOwner) then
    True -- TODO: Implement the quadratic logic.
  else
    traceIfFalse "Unauthorized." False
  -- }}}



instance Scripts.ValidatorTypes QVF where
  type DatumType    QVF = QVFDatum
  type RedeemerType QVF = ()


typedValidator :: PaymentPubKeyHash -> Scripts.TypedValidator QVF
typedValidator fundOwner =
  Scripts.mkTypedValidator @QVF
    ( PlutusTx.applyCode
        $$(PlutusTx.compile [|| mkValidator ||])
        PlutusTx.liftCode fundOwner
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @QVFDatum @()


validator :: PaymentPubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator


valHash :: PaymentPubKeyHash -> ValidatorHash
valHash = Scripts.validatorHash . typedValidator


scrAddress :: PaymentPubKeyHash -> Address
scrAddress = scriptAddress . validator
-- }}}


