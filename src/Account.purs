module Account where
  
import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Int (toNumber)
import Data.Map (Map, foldSubmap, insert, singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Euro (Euro(..), getExchangeRate)
import Good (Good(..), stringFromGood)
import React.Basic.DOM as D
import React.Basic.Hooks (JSX)

type Account = {
  goods :: Map Good Int,
  balance :: Euro,
  id :: Int
}

linkedAccountA :: Linked Account 
linkedAccountA = Linked accountA linkedSubAccount
 
data Linked a = Leaf | Linked a (Linked a)  

instance functorLinked :: Functor Linked where
  map f (Linked a linked) = Linked (f a) (map f linked) 
  map f Leaf = Leaf
  
instance foldableLinked :: Foldable Linked where
  foldr f acc Leaf = acc
  foldr f acc (Linked a linked) = foldr f (foldr f (f a acc) linked) linked 
  foldl f acc Leaf = acc
  foldl f acc (Linked a linked) = foldl f (foldl f (f acc a) linked) linked 
  foldMap f Leaf = mempty
  foldMap f (Linked a linked) = f a <> foldMap f linked  

accountA:: Account
accountA = {
  goods : 
  insert Remislu 25 
  $ insert Inden 100 
  $ insert Zzrimus 7 
  $ singleton Quakzarg 5,
  balance : Euro 10.0,
  id : 0
}

linkedSubAccount :: Linked Account
linkedSubAccount = Linked subAccountA Leaf

subAccountA :: Account 
subAccountA = {
  goods : 
  insert Remislu 2
  $ insert Inden 50
  $ insert Zzrimus 2 
  $ singleton Quakzarg 1,
  balance : Euro 3.0,
  id : 1
}

mkAccount :: Account -> JSX
mkAccount account = 
    D.div {
      children: [ 
        D.h3 {
          children: [ D.text $ "Showing Account with ID: " <> show account.id ]
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