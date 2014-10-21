-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL00
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <<//ga-beacon.appspot.com/UA-53767359-1/sdlhs/Graphics-UI-SDL00>>
-------------------------------------------------------------------------------

module Graphics.UI.SDL00 (
  -----------------------------------------------------------------------------
  -- * Run-Time Linking
  -----------------------------------------------------------------------------
    module Graphics.UI.SDL00.LibLD

  -----------------------------------------------------------------------------
  -- * Exceptions
  -----------------------------------------------------------------------------
  , SDLE (..)
  

  ) where

import Graphics.UI.SDL00.LibHT
import Graphics.UI.SDL00.LibLD

