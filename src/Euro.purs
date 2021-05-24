module Euro where
  
import Prelude

import Good (Good(..))

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

getTransactionFee :: Int -> Good -> Euro
getTransactionFee amount (Inden) | amount <= 5 = Euro 0.15
getTransactionFee amount (Inden) = Euro 0.1
getTransactionFee amount (Remislu) | amount == 1 = Euro 0.01
getTransactionFee amount (Remislu) | amount <= 20 = Euro 0.005
getTransactionFee amount (Remislu) = Euro 0.0001
getTransactionFee amount (Quakzarg) = Euro 0.2
getTransactionFee amount (Zzrimus) = Euro 0.5

getExchangeRate:: Good -> Euro 
getExchangeRate Quakzarg = Euro 2.25
getExchangeRate Remislu = Euro 0.48 
getExchangeRate Inden = Euro 0.97
getExchangeRate Zzrimus = Euro 1.22 