module Payment where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, useState)
import React.Basic.Hooks as R
import ReactUtil (SetState)

newtype SEPA_ID = SEPA_ID String
newtype CardNumber = CardNumber String
newtype Email = Email String

data PaymentMethod
  = Paypal Email
  | CreditCard CardNumber
  | SEPA SEPA_ID
  
emailRegex :: Regex
emailRegex = unsafeRegex "^\\w+@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}$/" noFlags
  
validateEmail :: String -> Boolean
validateEmail = test emailRegex 

mkPaypalForm :: Component (SetState PaymentMethod)
mkPaypalForm = do
  component "PaypalForm" \setPaymentMethod -> R.do
    Tuple email setEmail <- useState "" 
    Tuple error setError <- useState Nothing
    let 
      handleEmail:: Maybe String -> Effect Unit
      handleEmail (Just str) = do 
        setEmail \x -> str 
        if validateEmail str then do 
          setPaymentMethod \p -> (Paypal (Email str))
          setError \e -> Nothing
        else
         setError \e -> (Just "Invalid Email") 
          
      handleEmail Nothing = log $ "Unknown value"
    pure $
      D.div {
        children: [
          D.input {
            value: email,
            onChange: handler targetValue handleEmail
          },
          case error of 
            Nothing -> R.fragment []
            (Just e) -> D.text e
        ]
      }
      
validateCard :: String -> Boolean
validateCard str = length str == 16 

mkCreditCardForm :: Component (SetState PaymentMethod) 
mkCreditCardForm = do
  component "CreditCardForm" \setPaymentMethod -> R.do
    Tuple card setCard <- useState "" 
    Tuple error setError <- useState Nothing
    let 
      handleCard:: Maybe String -> Effect Unit
      handleCard (Just str) = do
        setCard \x -> str 
        if validateCard str then do
          setError \e -> Nothing
          setPaymentMethod \p -> (CreditCard (CardNumber str))
        else
         setError \e -> (Just "Invalid Credit Card Number") 
      handleCard Nothing = log $ "Unknown value"
    pure $
      D.div {
        children: [
          D.input {
            value: card,
            onChange: handler targetValue handleCard
          },
          case error of 
            Nothing -> R.fragment []
            (Just e) -> D.text e
        ]
      }

validateSepaId :: String -> Boolean
validateSepaId str = length str >= 16 && length str <= 28

mkSEPAForm :: Component (SetState PaymentMethod)
mkSEPAForm = do
  component "SepaForm" \setPaymentMethod -> R.do
    Tuple id setId <- useState "" 
    Tuple error setError <- useState Nothing
    let 
      handleId:: Maybe String -> Effect Unit
      handleId (Just str) = do
        setId \x -> str 
        if validateSepaId str then do
          setError \e -> Nothing
          setPaymentMethod \p -> (SEPA (SEPA_ID str))
        else
         setError \e -> (Just "Invalid Credit Card Number") 
      handleId Nothing = log $ "Unknown value"
    pure $
      D.div {
        children: [
          D.input {
            value: id,
            onChange: handler targetValue handleId
          },
          case error of 
            Nothing -> R.fragment []
            (Just e) -> D.text e
        ]
      }