module Good where 

import Prelude

import React.Basic.Hooks (JSX)
import React.Basic.DOM as D

data Good =
  Quakzarg | Remislu | Inden | Zzrimus  

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