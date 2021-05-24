module ReactUtil where

import Prelude
import Effect (Effect)

type SetState state
  = (state -> state) -> Effect Unit