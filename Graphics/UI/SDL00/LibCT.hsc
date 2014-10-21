-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL00.LibCT
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Graphics.UI.SDL00.LibCT where

import Control.Applicative ((<$>), (<*>))

import Foreign.Storable
--import Foreign.C.String
--import Foreign.C.Types
--import Foreign.Ptr

import Data.Word
import Data.Int

#include "LibC0.c"



-------------------------------------------------------------------------------
-- SDL_stdinc.h
-------------------------------------------------------------------------------
type Sint8  = Int8
type Sint16 = Int16
type Sint32 = Int32
type Sint64 = Int64

type Uint8  = Word8
type Uint16 = Word16
type Uint32 = Word32
type Uint64 = Word64


-------------------------------------------------------------------------------
-- SDL_version.h
-------------------------------------------------------------------------------
data SDL_version = SDL_version Uint8 Uint8 Uint8

instance Storable SDL_version where
  sizeOf    _ = #{size    SDL_version}
  alignment _ = #{alignof SDL_version}
  poke _ _    = undefined
  peek ptr    = SDL_version
    <$> #{peek SDL_version, major} ptr
    <*> #{peek SDL_version, minor} ptr
    <*> #{peek SDL_version, patch} ptr

