module Main where

import Prelude

import Account (Account, Linked(..), linkedAccountA, mkAccount, subAccountA)
import Crafting (mkCrafting)
import Data.Array (concat, fromFoldable)
import Data.Date (Date)
import Data.Int (fromString, toNumber)
import Data.Map (alter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Euro (Euro(..), getExchangeRate, getTransactionFee)
import Good (Good(..), goodFromString, mkGoodOptions)
import Payment (Email(..), PaymentMethod(..), mkCreditCardForm, mkPaypalForm, mkSEPAForm)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState)
import React.Basic.Hooks as R
import ReactUtil (SetState)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

data BillingCycle = Monthly | Yearly

newtype StartDate = StartDate Date
newtype Price = Price Number
data PaymentType 
  = Subscription StartDate BillingCycle Price 
  | OneTimePayment Price


data BankTransaction = Buy | Sell

mkMyApp :: Component Unit 
mkMyApp = do
  craftingComponent <- mkCrafting
  paypalForm <- mkPaypalForm
  creditCardForm <- mkCreditCardForm
  sepaForm <- mkSEPAForm
  -- incoming \props are unused
  component "MainApp" \apiKey -> R.do
    Tuple transaction setTransaction <- useState Buy 
    Tuple linkedAccount setLinkedAccount <- useState linkedAccountA 
    Tuple chosenGood setChosenGood <- useState Quakzarg
    Tuple amount setAmount <- useState 0 
    Tuple accountId setAccountId <- useState 0
    Tuple paymentMethod setPaymentMethod <- useState "Paypal"
    Tuple paymentData setPaymentData <- useState (Paypal (Email ""))

    let
      setAccount :: SetState Account 
      setAccount handler = do
        setLinkedAccount setter 
        where
          setter :: Linked Account -> Linked Account
          setter (Linked act linked) | act.id == accountId = Linked (handler act) linked
          setter (Linked act linked) = Linked act (setter linked)
          setter Leaf = Leaf

      account :: Account
      account = getAccount linkedAccount
        where 
          getAccount :: Linked Account -> Account
          getAccount Leaf = subAccountA
          getAccount (Linked a Leaf) = a
          getAccount (Linked a linked) | a.id == accountId = a
          getAccount (Linked a linked) = getAccount linked 
      
      handleChooseAccount :: Maybe String -> Effect Unit
      handleChooseAccount (Just str) = do 
        case fromString str of 
          Just id -> setAccountId \a -> id  
          Nothing -> setAccountId \a -> 0  
      handleChooseAccount _ = pure unit

      handleChooseTransaction :: Maybe String -> Effect Unit
      handleChooseTransaction (Just "Buy") = setTransaction \x -> Buy  
      handleChooseTransaction (Just "Sell") = setTransaction \x -> Sell 
      handleChooseTransaction str = log $ "invalid transaction:" <> fromMaybe "" str  

      chooseGood :: Maybe String -> Effect Unit
      chooseGood (Just str) = setChosenGood \x -> goodFromString str
      chooseGood good = log $ "Unknown chosen good:" <> fromMaybe "Unknown" good 
      
      chooseAmount :: Maybe String -> Effect Unit 
      chooseAmount (Just amt) = do
        let 
          strAmount = fromString amt
        case strAmount of
          Just a -> setAmount \x -> a
          Nothing -> setAmount \x -> x 
      chooseAmount Nothing = log "No Amount given" 
      
      choosePaymentMethod :: Maybe String -> Effect Unit 
      choosePaymentMethod (Just p) = setPaymentMethod \x -> p
      choosePaymentMethod Nothing = log "Not a payment method" 
      
      handleTransaction :: Effect Unit
      handleTransaction = do
        let
          value = Euro (toNumber amount) * ( getExchangeRate chosenGood )
          fee = getTransactionFee amount chosenGood 
        case transaction of
          Buy -> do
            setAccount \a -> a { 
              balance = account.balance - fee - value,
              goods = alter (\v -> map (\i -> i + amount) v) chosenGood account.goods
            }
          Sell -> do
            setAccount \a -> a { 
              balance = account.balance - fee + value, 
              goods = alter (\v -> map (\i -> i - amount) v) chosenGood account.goods
            }
      accountIds :: Array Int 
      accountIds = concat $ map (\a -> [a.id]) (fromFoldable linkedAccount)

    pure $
      D.div {
        className: "container",
        children: [
          D.h1 {
            children : [
              D.text "Purescript Trading" 
            ]
          },
          D.div {
            className: "row",
            children: [
              D.div { className: "col-sm-10", children: [ D.text "Select Account: " ] },
              D.select {
                className: "col-sm-2", 
                onChange: handler targetValue handleChooseAccount,
                children: map (\x -> 
                D.option { children: [ D.text $ show x ] }) accountIds
              }
            ] 
          },
          mkAccount account,
          D.h4 { children: [ D.text "Buy or Sell" ] },
          D.div {
            className: "mt-2",
            children : [
              D.select {
              onChange: handler targetValue handleChooseTransaction,
              children: [
                D.option {
                  className: "form-select form-select-lg",
                  children: [ D.text "Buy" ]
                },
                D.option {
                  className: "form-select form-select-lg",
                  children: [ D.text "Sell" ]
                }
              ]
              }
            ]
          },
          D.div {
            className: "mt-2",
            children : [
              D.select {
                onChange: handler targetValue chooseGood,
                children: mkGoodOptions 
              }
            ] 
          },
          D.div {
            className: "mt-2",
            children : [
              D.input {
                type: "number",
                value: show amount,
                onChange: handler targetValue chooseAmount 
              }
            ]
          },
          D.div {
            className: "mt-2",
            children : [
              D.select {
                onChange: handler targetValue choosePaymentMethod,
                value: paymentMethod,
                children: [
                  D.option {
                    className: "form-select form-select-lg",
                    children: [ D.text "Paypal" ]
                  },
                  D.option {
                    className: "form-select form-select-lg",
                    children: [ D.text "Credit Card" ]
                  },
                  D.option {
                    className: "form-select form-select-lg",
                    children: [ D.text "SEPA" ]
                  }
                ]
              }
            ] 
          },
          case paymentMethod of
            "Paypal" -> paypalForm setPaymentData
            "Credit Card" -> creditCardForm setPaymentData
            "SEPA" -> sepaForm setPaymentData
            _ -> R.fragment [] 
          ,D.button {
            className: "mt-2 btn btn-secondary",
            onClick: handler_ handleTransaction,
            children : [
              D.text "Run Transaction"
            ]
          },
          craftingComponent { account: account, setAccount: setAccount }
        ]
      }

main :: Effect Unit
main = do
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- body doc 
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkMyApp 
      -- let
        -- Create JSX node from react component. Pass-in empty props
        -- app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      -- D.render app c
      D.render (addressBookApp unit) (toElement c)
