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
import Foreign.C.Types
import Foreign.Ptr

import Data.Word
import Data.Int

#include "LibC0.c"



-------------------------------------------------------------------------------
type NonConstPtr a = Ptr a



-------------------------------------------------------------------------------
-- ### SDL_stdinc.h ###########################################################
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
-- ### SDL.h ##################################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_hints.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_error.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_log.h ##############################################################
-------------------------------------------------------------------------------
type SDL_LogPriority = CInt



-------------------------------------------------------------------------------
-- ### SDL_assert.h ###########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_version.h ##########################################################
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



-------------------------------------------------------------------------------
-- ### SDL_video.h ############################################################
-------------------------------------------------------------------------------
data SDL_DisplayMode = SDL_DisplayMode Uint32 CInt CInt CInt (Ptr ())

instance Storable SDL_DisplayMode where
  sizeOf    _ = #{size    SDL_DisplayMode}
  alignment _ = #{alignof SDL_DisplayMode}
  peek ptr    = SDL_DisplayMode
    <$> #{peek SDL_DisplayMode, format      } ptr
    <*> #{peek SDL_DisplayMode, w           } ptr
    <*> #{peek SDL_DisplayMode, h           } ptr
    <*> #{peek SDL_DisplayMode, refresh_rate} ptr
    <*> #{peek SDL_DisplayMode, driverdata  } ptr
  poke ptr (SDL_DisplayMode f w h r d) = do
    #{poke SDL_DisplayMode, format      } ptr f
    #{poke SDL_DisplayMode, w           } ptr w
    #{poke SDL_DisplayMode, h           } ptr h
    #{poke SDL_DisplayMode, refresh_rate} ptr r
    #{poke SDL_DisplayMode, driverdata  } ptr d



-------------------------------------------------------------------------------
-- ### SDL_rect.h #############################################################
-------------------------------------------------------------------------------
data SDL_Point = SDL_Point CInt CInt

instance Storable SDL_Point where
  sizeOf    _ = #{size    SDL_Point}
  alignment _ = #{alignof SDL_Point}
  poke _ _    = undefined
  peek ptr    = SDL_Point
    <$> #{peek SDL_Point, x} ptr
    <*> #{peek SDL_Point, y} ptr

-------------------------------------------------------------------------------
data SDL_Rect = SDL_Rect CInt CInt CInt CInt

instance Storable SDL_Rect where
  sizeOf    _ = #{size    SDL_Rect}
  alignment _ = #{alignof SDL_Rect}
  poke _ _    = undefined
  peek ptr    = SDL_Rect
    <$> #{peek SDL_Rect, x} ptr
    <*> #{peek SDL_Rect, y} ptr
    <*> #{peek SDL_Rect, w} ptr
    <*> #{peek SDL_Rect, h} ptr

