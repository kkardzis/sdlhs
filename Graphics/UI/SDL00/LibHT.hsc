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
-- import Foreign.Ptr

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


-------------------------------------------------------------------------------
data SDLE = SDLE String String
  deriving (Eq, Typeable)

instance Exception SDLE

instance Show SDLE where
  showsPrec _ (SDLE func desc) =
    showString "<sdlhs> " . showString func
    . showString ": " . showString desc


-------------------------------------------------------------------------------
deriving instance (Show SDL_InitFlag)

#{ENUM SDL_InitFlag       \
, SDL_INIT_TIMER          \
, SDL_INIT_AUDIO          \
, SDL_INIT_VIDEO          \
, SDL_INIT_JOYSTICK       \
, SDL_INIT_HAPTIC         \
, SDL_INIT_GAMECONTROLLER \
, SDL_INIT_EVENTS         }

