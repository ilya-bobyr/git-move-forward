{-# LANGUAGE NoImplicitPrelude #-}

module Options
  ( Options (..),
  )
where

import RIO (Bool, Maybe, Text)

data Options = Options
  { optionsVerbose :: !Bool,
    optionsCheckoutBranch :: !(Maybe Text)
  }
