module Main where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat (string)
import Data.Array (concat, find, foldl, index, intersect, mapWithIndex, slice, (:))
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Int (fromString, toNumber)
import Data.Map (Map, alter, foldSubmap, insert, lookup, singleton, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Exception (throw)
import Prim.Ordering (GT)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (type (/\), Component, JSX, ReactComponent, component, element, reactComponent, unsafeRenderEffect, useEffectOnce, useState)
import React.Basic.Hooks as R
import React.Basic.Hooks.Aff (useAff)
import Simple.JSON (readJSON)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLDocument (body, toNonElementParentNode)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.HTMLOptionElement (setSelected)
import Web.HTML.HTMLTableElement (createTFoot)
import Web.HTML.Window (document)

type SetState state
  = (state -> state) -> Effect Unit

newtype SEPA_ID = SEPA_ID String
newtype CardNumber = CardNumber String
newtype Email = Email String

data PaymentMethod
  = Paypal Email
  | CreditCard CardNumber
  | SEPA SEPA_ID

data BillingCycle = Monthly | Yearly

newtype StartDate = StartDate Date
newtype Price = Price Number
data PaymentType 
  = Subscription StartDate BillingCycle Price 
  | OneTimePayment Price

data Good =
  Quakzarg | Remislu | Inden | Zzrimus  

data CraftGood =
  CraftGood Good Int

instance craftGoodSemiGroup :: Semigroup CraftGood where
  -- symmetric
  append (CraftGood Quakzarg a1) (CraftGood Quakzarg a2) = CraftGood Quakzarg (a1 + a2)  
  append (CraftGood Zzrimus a1) (CraftGood Zzrimus a2) = CraftGood Zzrimus (a1 + a2)  
  append (CraftGood Inden a1) (CraftGood Inden a2) = CraftGood Quakzarg ((a1 + a2) /2 )  
  append (CraftGood Remislu a1) (CraftGood Remislu a2) = CraftGood Inden((a1 + a2) / 2)  
  -- left 
  append (CraftGood Quakzarg a1) (CraftGood Zzrimus a2) = CraftGood Remislu (4* a1 + 3*a2)
  append (CraftGood Quakzarg a1) (CraftGood Inden a2) = CraftGood Zzrimus ((a1 + a2) / 2)  
  append (CraftGood Quakzarg a1) (CraftGood Remislu a2) = CraftGood Remislu (4*a1 + a2)  
  append (CraftGood Inden a1) (CraftGood Zzrimus a2) = CraftGood Remislu (2*a1 + 3*a2)  
  append (CraftGood Inden a1) (CraftGood Remislu a2) = CraftGood Zzrimus((a1 + a2) /3)  
  append (CraftGood Remislu a1) (CraftGood Zzrimus a2) = CraftGood Quakzarg((a1 + a2) / 4)  
  --right
  append (CraftGood Zzrimus a1) (CraftGood Quakzarg a2) = CraftGood Zzrimus (3*a1 + 4*a2)  
  append (CraftGood Inden a1) (CraftGood Quakzarg a2) = CraftGood Zzrimus ((a1 + a2) / 2)  
  append (CraftGood Remislu a1) (CraftGood Quakzarg a2) = CraftGood Zzrimus (a1 + a2 * 4)  
  append (CraftGood Zzrimus a1) (CraftGood Inden a2) = CraftGood Zzrimus (3*a1 + 2*a2)  
  append (CraftGood Remislu a1) (CraftGood Inden a2) = CraftGood Zzrimus ((a1 + a2) / 3)  
  append (CraftGood Zzrimus a1) (CraftGood Remislu a2) = CraftGood Zzrimus ((a1 + a2) /4)  

instance showCraftGood :: Show CraftGood where
  show (CraftGood good amount) = show amount <> " " <> stringFromGood good
  
getGood :: CraftGood -> Good
getGood (CraftGood g a) = g

getAmount :: CraftGood -> Int
getAmount (CraftGood g a) = a 

  
stringFromGood :: Good -> String
stringFromGood Quakzarg = "Quakzarg"
stringFromGood Remislu = "Remislu"
stringFromGood Inden = "Inden"
stringFromGood Zzrimus = "Zzrimus"

goodFromString :: String -> Good
goodFromString "Quakzarg" = Quakzarg
goodFromString "Remislu" = Remislu 
goodFromString "Inden" = Inden 
goodFromString "Zzrimus" = Zzrimus
goodFromString _ = Quakzarg

derive instance eqGood :: Eq Good  
derive instance ordGood :: Ord Good  
 
type Account = {
  goods :: Map Good Int,
  balance :: Euro
}

accountA:: Account
accountA = {
  goods : 
  insert Remislu 25 
  $ insert Inden 100 
  $ insert Zzrimus 7 
  $ singleton Quakzarg 5,
  balance : Euro 10.0
}

newtype Euro = Euro Number 
derive newtype instance euroShow :: Show Euro
derive newtype instance euroSemiring :: Semiring Euro
derive newtype instance euroRing :: Ring Euro

instance euroSemigroup :: Semigroup Euro where
  append (Euro e1) (Euro e2) = Euro (e1 + e2)

instance euroMonoid :: Monoid Euro where
  mempty = Euro (0.0)


fromEuro :: Euro -> Number
fromEuro (Euro e) = e

getExchangeRate:: Good -> Euro 
getExchangeRate Quakzarg = Euro 2.25
getExchangeRate Remislu = Euro 0.48 
getExchangeRate Inden = Euro 0.97
getExchangeRate Zzrimus = Euro 1.22 

data BankTransaction = Buy | Sell

getTransactionFee :: Int -> Good -> Euro
getTransactionFee amount (Inden) | amount <= 5 = Euro 0.15
getTransactionFee amount (Inden) = Euro 0.1
getTransactionFee amount (Remislu) | amount == 1 = Euro 0.01
getTransactionFee amount (Remislu) | amount <= 20 = Euro 0.005
getTransactionFee amount (Remislu) = Euro 0.0001
getTransactionFee amount (Quakzarg) = Euro 0.2
getTransactionFee amount (Zzrimus) = Euro 0.5

mkAccount :: Account -> JSX
mkAccount account = 
    D.div {
      children: [ 
        D.h3 {
          children: [ D.text "Your Account" ]
        },
        D.h5 {
          children : [
            D.text $ "Your Balance: " <> show account.balance
          ]
        },
        D.h5 {
          children : [
            D.text $ "Total Account Value: " <> show (account.balance + totalValue)
          ]
        },
        D.table {
          className: "table",
          children: [
            D.thead {
              children: [
                D.tr {
                  children: [
                    D.th { scope: "col", children: [ D.text "Good" ]},
                    D.th { scope: "col", children: [ D.text "Amount" ]},
                    D.th { scope: "col", children: [ D.text "Value per Unit" ]}
                  ] 
                } 
              ] 
            },
            D.tbody {
              children: (map tableRow goodArray)
            }
          ]
        }
      ] 
    }
    where
      goodArray :: Array (Tuple Good Int)
      goodArray = foldSubmap Nothing Nothing (\k v -> [Tuple k v]) account.goods
      totalValue :: Euro
      totalValue = foldSubmap Nothing Nothing (\k v -> (Euro (toNumber v)) * getExchangeRate k) account.goods
      tableRow :: Tuple Good Int -> JSX
      tableRow entry = 
        D.tr {
          children: [
            D.td {
              children: [ D.text $ stringFromGood $ fst entry ]
            },
            D.td {
              children: [ D.text $ show $ snd entry ]
            },
            D.td {
              children: [ D.text $ show $ getExchangeRate (fst entry) ]
            }
          ]
        } 

mkCrafting :: Component { 
  account:: Account,
  setAccount :: SetState Account
}  
mkCrafting = do
  component "Crafting" \props -> R.do
    Tuple goodA setChosenGoodA <- useState Quakzarg
    Tuple goodB setChosenGoodB <- useState Quakzarg
    Tuple amountA setAmountA <- useState 0
    Tuple amountB setAmountB <- useState 0
    let
      chooseGoodA :: Maybe String -> Effect Unit
      chooseGoodA (Just str) = setChosenGoodA \x -> goodFromString str
      chooseGoodA good = log $ "Unknown chosen good:" <> fromMaybe "Unknown" good 
      chooseGoodB :: Maybe String -> Effect Unit
      chooseGoodB (Just str) = setChosenGoodB \x -> goodFromString str
      chooseGoodB good = log $ "Unknown chosen good:" <> fromMaybe "Unknown" good 
      chooseAmountA :: Maybe String -> Effect Unit
      chooseAmountA (Just amount) = 
        case fromString amount of
          Just a -> setAmountA \x -> a
          Nothing -> setAmountA \x -> 0 
      chooseAmountA Nothing = log "No Amount given" 
      chooseAmountB :: Maybe String -> Effect Unit
      chooseAmountB (Just amount) = 
        case fromString amount of
          Just a -> setAmountB \x -> a
          Nothing -> setAmountB \x -> 0 
      chooseAmountB Nothing = log "No Amount given" 
      
      craft :: Effect Unit
      craft = props.setAccount
        (\a -> a {
          goods = 
            alter (\v -> map (\i -> i - amountA) v) goodA 
            $ alter (\v -> map (\i -> i - amountB) v) goodB 
            $ alter (\v -> map (\i -> i + resultAmount) v) resultGood
            a.goods
        }) 
        where 
          res :: CraftGood
          res = append (CraftGood goodA amountA) (CraftGood goodB amountB)  
          resultGood :: Good
          resultGood = getGood res
          resultAmount :: Int
          resultAmount = getAmount res

    pure $
      D.div {
        children: [
          D.h4 {
            className: "mt-5",
            children: [ D.text "Crafting" ]
          },
          D.div {
            className: "row",
            children: [
              D.input {
                className: "col-sm-3",
                type: "number",
                value: show amountA,
                onChange: handler targetValue chooseAmountA
              },
              D.select {
                className: "form-select form-select-lg col-sm-3",
                children: mkGoodOptions,
                onChange: handler targetValue chooseGoodA
              },
              D.input {
                className: "col-sm-3",
                type: "number",
                value: show amountB,
                onChange: handler targetValue chooseAmountB
              },
              D.select {
                className: "form-select form-select-lg col-sm-3",
                children: mkGoodOptions, 
                onChange: handler targetValue chooseGoodB
              }
            ]
          },
          D.div {
            className: "row mt-4",
            children: [
              D.text $ "Result: " <> (show $ append (CraftGood goodA amountA) (CraftGood goodB amountB)  )
            ]
          },
          D.button {
            onClick: handler_ craft,
            className : "btn btn-secondary",
            children: [
              D.text "Craft"
            ]
          }
        ]     
      }


mkGoodOptions :: Array JSX
mkGoodOptions = [ 
  D.option {
    className: "form-select form-select-lg",
    children: [ D.text "Quakzarg" ]
  },
  D.option {
    className: "form-select form-select-lg",
    children: [ D.text "Remislu" ]
  },
  D.option {
    className: "form-select form-select-lg",
    children: [ D.text "Inden" ]
  },
  D.option {
    className: "form-select form-select-lg",
    children: [ D.text "Zzrimus" ]
  }
]


mkMyApp :: Component Unit 
mkMyApp = do
  craftingComponent <- mkCrafting
  -- incoming \props are unused
  component "MainApp" \apiKey -> R.do
    Tuple transaction setTransaction <- useState Buy 
    Tuple account setAccount <- useState accountA 
    Tuple chosenGood setChosenGood <- useState Quakzarg
    Tuple amount setAmount <- useState 0
    let 
      handleChooseTransaction :: Maybe String -> Effect Unit
      handleChooseTransaction (Just "Buy") = setTransaction \x -> Buy  
      handleChooseTransaction (Just "Sell") = setTransaction \x -> Sell 
      handleChooseTransaction str = log $ "invalid transaction:" <> fromMaybe "" str  

      chooseGood :: Maybe String -> Effect Unit
      chooseGood (Just str) = setChosenGood \x -> goodFromString str
      chooseGood good = log $ "Unknown chosen good:" <> fromMaybe "Unknown" good 
      
      chooseAmount :: Maybe String -> Effect Unit 
      chooseAmount (Just amount) = 
        case fromString amount of
          Just a -> setAmount \x -> a
          Nothing -> setAmount \x -> 0 
      chooseAmount Nothing = log "No Amount given" 
      
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

    pure $
      D.div {
        className: "container",
        children: [
          D.h1 {
            children : [
              D.text "Purescript Trading" 
            ]
          },
          mkAccount account,
          D.h4 {
            children: [ D.text "Buy or Sell" ]
          },
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
          D.button {
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
