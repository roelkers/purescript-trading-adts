module Payment where

import Prelude

import ReactUtil (SetState)
import React.Basic.Hooks (Component, component,useState) 
import React.Basic.Hooks as R
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

newtype SEPA_ID = SEPA_ID String
newtype CardNumber = CardNumber String
newtype Email = Email String

data PaymentMethod
  = Paypal Email
  | CreditCard CardNumber
  | SEPA SEPA_ID

mkPaypalForm :: Component (SetState PaymentMethod)
mkPaypalForm = do
  component "PaypalForm" \props -> R.do
    Tuple email setEmail <- useState "" 
    let 
      handleEmail:: Maybe String -> Effect Unit
      handleEmail (Just str) = setEmail \x -> str 
      handleEmail Nothing = log $ "Unknown value"
    pure $
      D.div {
        children: [
          D.input {
            value: email,
            onChange: handler targetValue handleEmail
          }
        ]
      }
      

mkCreditCardForm :: Component (SetState PaymentMethod) 
mkCreditCardForm = do
  component "CreditCardForm" \props -> R.do
    Tuple card setCard <- useState "" 
    let 
      handleCard:: Maybe String -> Effect Unit
      handleCard (Just str) = setCard \x -> str 
      handleCard Nothing = log $ "Unknown value"
    pure $
      D.div {
        children: [
          D.input {
            value: card,
            onChange: handler targetValue handleCard
          }
        ]
      }
      

mkSEPAForm :: Component (SetState PaymentMethod)
mkSEPAForm = do
  component "SepaForm" \props -> R.do
    Tuple id setId <- useState "" 
    let 
      handleId:: Maybe String -> Effect Unit
      handleId (Just str) = setId \x -> str 
      handleId Nothing = log $ "Unknown value"
    pure $
      D.div {
        children: [
          D.input {
            value: id,
            onChange: handler targetValue handleId
          }
        ]
      }