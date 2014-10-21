-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL00.LibCC
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.SDL00.LibCC
  ( sdlADRTAB, sdlSYMTAB, sdlTABLEN

  -- SDL.h
  , sdlInit
  , sdlInitSubSystem
  , sdlQuitSubSystem
  , sdlWasInit
  , sdlQuit

  -- SDL_error.h
 -- sdlSetError
  , sdlGetError
  , sdlClearError

  -- SDL_version.h
  , sdlGetVersion
  , sdlGetRevision
  , sdlGetRevisionNumber

  ) where

import Graphics.UI.SDL00.LibCT

import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Control.Monad (when)

import System.IO.Unsafe (unsafePerformIO)
import System.RTLD

#include "LibC0.c"


-------------------------------------------------------------------------------
foreign import ccall "&sdlADRTAB" sdlADRTAB :: Ptr (FunPtr ())
foreign import ccall "&sdlSYMTAB" sdlSYMTAB :: Ptr SYMTABENTRY

sdlTABLEN :: Int
sdlTABLEN = #{const TABLEN}

peekFP :: Int -> IO (FunPtr a)
peekFP fid = do
  fp <- peekElemOff sdlADRTAB fid
  when (fp==nullFunPtr) (nullFP fid)
  return (castFunPtr fp)

nullFP :: Int -> IO ()
nullFP fid =
  let name = unsafePerformIO $
        peekElemOff sdlSYMTAB fid >>= \(RTSYM (_,_,cs)) -> peekCString cs
  in  error $ concat ["<sdlhs> failed to call '", name, "' (NULL)"]




-------------------------------------------------------------------------------
-- SDL.h
-------------------------------------------------------------------------------
#{SAFECALL sdlInit, Uint32, IO CInt}
#{SAFECALL sdlInitSubSystem, Uint32, IO CInt}
#{SAFECALL sdlQuitSubSystem, Uint32, IO ()}
#{SAFECALL sdlWasInit, Uint32, IO Uint32}
#{SAFECALL sdlQuit, IO ()}


-------------------------------------------------------------------------------
-- SDL_error.h
-------------------------------------------------------------------------------
#{SAFECALL sdlGetError, IO (Ptr CChar)}
#{SAFECALL sdlClearError, IO ()}


-------------------------------------------------------------------------------
-- SDL_version.h
-------------------------------------------------------------------------------
#{SAFECALL sdlGetVersion, Ptr SDL_version, IO ()}
#{SAFECALL sdlGetRevision, IO (Ptr CChar)}
#{SAFECALL sdlGetRevisionNumber, IO CInt}

