-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL00.LibLD
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Graphics.UI.SDL00.LibLD
  ( RTLD(..), LIBSDL(..)
  ) where

import Graphics.UI.SDL00.LibCC (sdlADRTAB, sdlSYMTAB, sdlTABLEN)
import Graphics.UI.SDL00.LibHC (sdlInit, sdlQuit, sdlGetVersion)

import Control.Concurrent (MVar, newMVar)
import System.IO.Unsafe (unsafePerformIO)

import Data.Version (Version (..))

import System.Info (os)
import System.RTLD


-------------------------------------------------------------------------------
data LIBSDL = SDL20
  deriving (Eq, Ord, Enum, Bounded)

instance RTLD LIBSDL where
  loadlib x = rtload sdlRTSO x
  freelib _ = rtfree sdlRTSO


-------------------------------------------------------------------------------
sdlGlobalState :: MVar (Maybe (LIBSDL, LIBH, Int))
sdlGlobalState = unsafePerformIO (newMVar Nothing)
{-# NOINLINE sdlGlobalState #-}

sdlRTSO :: RTSO LIBSDL
sdlRTSO = RTSO
  { rtPKGMVAR = sdlGlobalState
  , rtPKGNAME = "<sdlhs>"
  , rtLIBNAME = libname
  , rtSONAMES = sonames
  , rtONLOAD  = const sdlInit
  , rtONFREE  = const sdlQuit
  , rtGETAPI  = const (fmap readapi sdlGetVersion)
  , rtSYMTAB  = sdlSYMTAB
  , rtADRTAB  = sdlADRTAB
  , rtTABLEN  = sdlTABLEN
  }


-------------------------------------------------------------------------------
libname :: LIBSDL -> String
libname SDL20 = "SDL 2.0"

readapi :: Version -> Maybe LIBSDL
readapi v
  | v >= Version [2,0,0] [] = Just SDL20
  | otherwise               = Nothing


-------------------------------------------------------------------------------
sonames :: LIBSDL -> [String]
sonames
  | os == "mingw32" = winsonames
  | os == "darwin"  = osxsonames
  | os == "linux"   = gnusonames
  | os == "freebsd" = bsdsonames
  | otherwise = const []

winsonames :: LIBSDL -> [String]
winsonames _ = ["SDL2.dll"]

osxsonames :: LIBSDL -> [String]
osxsonames _ = ["libSDL2.dylib"]

gnusonames :: LIBSDL -> [String]
gnusonames _ = ["libSDL2.so.4", "libSDL2.so"]

bsdsonames :: LIBSDL -> [String]
bsdsonames _ = ["libSDL2.so"]

