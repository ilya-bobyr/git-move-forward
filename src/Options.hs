module Options
  ( Options (..),
  )
where

import RIO (Bool, Maybe, Text)

data Options = Options
  { optionsVerbose :: !Bool,
    optionsMainName :: !Text,
    optionsOriginName :: !Text,
    optionsUpstreamName :: !Text,
    optionsForceMoveMain :: !Bool,
    optionsCheckoutBranch :: !(Maybe Text)
  }
