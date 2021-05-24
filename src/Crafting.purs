module Crafting where

import Prelude
import Good (Good(..), stringFromGood, goodFromString, mkGoodOptions)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, component,useState) 
import React.Basic.Hooks as R
import Account (Account)
import ReactUtil (SetState)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Int(fromString)
import Data.Map (alter)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)

data CraftGood =
  CraftGood Good Int
  
getGood :: CraftGood -> Good
getGood (CraftGood g a) = g

getAmount :: CraftGood -> Int
getAmount (CraftGood g a) = a 

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