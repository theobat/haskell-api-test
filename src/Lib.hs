module Lib
    ( someFunc
    ) where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text (Text)
import Data.Maybe

someFunc :: a -> Maybe a
someFunc = Just
