-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL00.LibHT
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE GADTs              #-}

module Graphics.UI.SDL00.LibHT where

import qualified Graphics.UI.SDL00.LibCT as C

import Control.Exception  (Exception)
-- import Control.Concurrent (MVar)

--import Data.Time.Clock (UTCTime)
--import Data.ByteString (ByteString)
import Data.Typeable   (Typeable, typeOf)
import Data.Maybe      (mapMaybe)
import Data.List       (foldl')
import Data.Bits       ((.|.), (.&.))
-- import Data.Int        (Int64)
-- import Data.Unique     (Unique, hashUnique)
-- import Data.IORef      (IORef)

import Foreign.C.Types
import Foreign.Ptr

#include "LibC0.c"


-------------------------------------------------------------------------------
class ENUM a where
  toENUM :: a -> Int
  enumlist :: [a]

instance ENUM a => ENUM [a] where
  toENUM = foldl' (.|.) 0 . map toENUM
  enumlist = []

toCInt :: ENUM a => a -> CInt
toCInt = fromIntegral . toENUM

toUint32 :: ENUM a => a -> C.Uint32
toUint32 = fromIntegral . toENUM

fromCIntMask :: ENUM a => CInt -> [a]
fromCIntMask mask =
  let checkBit x = if ((mask .&. (toCInt x)) == 0) then Nothing else Just x
  in  mapMaybe checkBit enumlist

fromUint32Mask :: ENUM a => C.Uint32 -> [a]
fromUint32Mask mask =
  let checkBit x = if ((mask .&. (toUint32 x)) == 0) then Nothing else Just x
  in  mapMaybe checkBit enumlist

fromCInt :: (ENUM a, Typeable a) => CInt -> a
fromCInt cval =
  let enums = map (\enum -> (toCInt enum, enum)) enumlist
      enumE = error $ concat ["<sdlhs> unknown constant (", v, ") -> ", t]
      (v,t) = (show cval, show $ typeOf $ snd $ head enums)
  in  maybe enumE id $ lookup cval enums

fromUint32 :: (ENUM a, Typeable a) => C.Uint32 -> a
fromUint32 cval =
  let enums = map (\enum -> (toUint32 enum, enum)) enumlist
      enumE = error $ concat ["<sdlhs> unknown constant (", v, ") -> ", t]
      (v,t) = (show cval, show $ typeOf $ snd $ head enums)
  in  maybe enumE id $ lookup cval enums


-------------------------------------------------------------------------------
data SDLE = SDLE String String
  deriving (Eq, Typeable)

instance Exception SDLE

instance Show SDLE where
  showsPrec _ (SDLE func desc) =
    showString "<sdlhs> " . showString func
    . showString ": " . showString desc



-------------------------------------------------------------------------------
-- ### SDL.h ##################################################################
-------------------------------------------------------------------------------

#{ENUM SDL_InitFlag       \
, SDL_INIT_TIMER          \
, SDL_INIT_AUDIO          \
, SDL_INIT_VIDEO          \
, SDL_INIT_JOYSTICK       \
, SDL_INIT_HAPTIC         \
, SDL_INIT_GAMECONTROLLER \
, SDL_INIT_EVENTS         }

deriving instance (Show SDL_InitFlag)



-------------------------------------------------------------------------------
-- ### SDL_hints.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_error.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_log.h ##############################################################
-------------------------------------------------------------------------------

#{ENUM SDL_LogCategory         \
, SDL_LOG_CATEGORY_APPLICATION \
, SDL_LOG_CATEGORY_ERROR       \
, SDL_LOG_CATEGORY_ASSERT      \
, SDL_LOG_CATEGORY_SYSTEM      \
, SDL_LOG_CATEGORY_AUDIO       \
, SDL_LOG_CATEGORY_VIDEO       \
, SDL_LOG_CATEGORY_RENDER      \
, SDL_LOG_CATEGORY_INPUT       \
, SDL_LOG_CATEGORY_TEST        }

#{ENUM SDL_LogPriority      \
, SDL_LOG_PRIORITY_VERBOSE  \
, SDL_LOG_PRIORITY_DEBUG    \
, SDL_LOG_PRIORITY_INFO     \
, SDL_LOG_PRIORITY_WARN     \
, SDL_LOG_PRIORITY_ERROR    \
, SDL_LOG_PRIORITY_CRITICAL }

deriving instance (Typeable SDL_LogPriority)
deriving instance (Show     SDL_LogPriority)



-------------------------------------------------------------------------------
-- ### SDL_assert.h ###########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_version.h ##########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_video.h ############################################################
-------------------------------------------------------------------------------
newtype SDL_Display = SDL_Display CInt
  deriving (Show, Eq)

-------------------------------------------------------------------------------
data SDL_DisplayMode = SDL_DisplayMode (Int,Int,Int) SDL_PixelFormat
  deriving (Show)

toHS'SDL_DisplayMode :: C.SDL_DisplayMode -> SDL_DisplayMode
toHS'SDL_DisplayMode (C.SDL_DisplayMode f w h r _) =
  let whr = (fromIntegral w, fromIntegral h, fromIntegral r)
  in  SDL_DisplayMode whr (fromUint32 f)

toCC'SDL_DisplayMode :: SDL_DisplayMode -> C.SDL_DisplayMode
toCC'SDL_DisplayMode (SDL_DisplayMode (w,h,r) f) =
  C.SDL_DisplayMode (toUint32 f)
    (fromIntegral w) (fromIntegral h) (fromIntegral r) nullPtr



-------------------------------------------------------------------------------
-- ### SDL_pixels.h ###########################################################
-------------------------------------------------------------------------------

#{ENUM SDL_PixelFormat        \
, SDL_PIXELFORMAT_UNKNOWN     \
, SDL_PIXELFORMAT_INDEX1LSB   \
, SDL_PIXELFORMAT_INDEX1MSB   \
, SDL_PIXELFORMAT_INDEX4LSB   \
, SDL_PIXELFORMAT_INDEX4MSB   \
, SDL_PIXELFORMAT_INDEX8      \
, SDL_PIXELFORMAT_RGB332      \
, SDL_PIXELFORMAT_RGB444      \
, SDL_PIXELFORMAT_RGB555      \
, SDL_PIXELFORMAT_BGR555      \
, SDL_PIXELFORMAT_ARGB4444    \
, SDL_PIXELFORMAT_RGBA4444    \
, SDL_PIXELFORMAT_ABGR4444    \
, SDL_PIXELFORMAT_BGRA4444    \
, SDL_PIXELFORMAT_ARGB1555    \
, SDL_PIXELFORMAT_RGBA5551    \
, SDL_PIXELFORMAT_ABGR1555    \
, SDL_PIXELFORMAT_BGRA5551    \
, SDL_PIXELFORMAT_RGB565      \
, SDL_PIXELFORMAT_BGR565      \
, SDL_PIXELFORMAT_RGB24       \
, SDL_PIXELFORMAT_BGR24       \
, SDL_PIXELFORMAT_RGB888      \
, SDL_PIXELFORMAT_RGBX8888    \
, SDL_PIXELFORMAT_BGR888      \
, SDL_PIXELFORMAT_BGRX8888    \
, SDL_PIXELFORMAT_ARGB8888    \
, SDL_PIXELFORMAT_RGBA8888    \
, SDL_PIXELFORMAT_ABGR8888    \
, SDL_PIXELFORMAT_BGRA8888    \
, SDL_PIXELFORMAT_ARGB2101010 \
, SDL_PIXELFORMAT_YV12        \
, SDL_PIXELFORMAT_IYUV        \
, SDL_PIXELFORMAT_YUY2        \
, SDL_PIXELFORMAT_UYVY        \
, SDL_PIXELFORMAT_YVYU        }

deriving instance (Typeable SDL_PixelFormat)
deriving instance (Show     SDL_PixelFormat)

