-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL00.LibHC
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-------------------------------------------------------------------------------

module Graphics.UI.SDL00.LibHC where

import qualified Graphics.UI.SDL00.LibCC as C
import qualified Graphics.UI.SDL00.LibCT as C

import Graphics.UI.SDL00.LibHT

import Control.Exception
import Control.Monad

import Data.Version

import Foreign.Marshal.Alloc
-- import Foreign.Marshal.Array
-- import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include "LibC0.c"


-------------------------------------------------------------------------------
throwSDLE :: String -> String -> IO a
throwSDLE func desc = throwIO (SDLE func desc)

checkCODE :: String -> (IO CInt) -> IO ()
checkCODE func action =
  action >>= \code -> when (code/=0) $ do
    desc <- C.sdlGetError >>= peekCString0
    C.sdlClearError >> throwSDLE func desc

-------------------------------------------------------------------------------
peekCString0 :: Ptr CChar -> IO String
peekCString0 cs = if (cs==nullPtr) then return [] else peekCAString cs



-------------------------------------------------------------------------------
-- ### SDL.h ##################################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- int SDL_Init(Uint32 flags);
-- '''''''''''''''''''''''''''
-- | Initialize the SDL library
-- (<http://wiki.libsdl.org/SDL_Init SDL_Init>)
-------------------------------------------------------------------------------
sdlInit :: IO ()
sdlInit = checkCODE "sdlInit" $ C.sdlInit 0


-------------------------------------------------------------------------------
-- void SDL_Quit(void);
-- ''''''''''''''''''''
-- | Clean up all initialized subsystems
-- (<http://wiki.libsdl.org/SDL_Quit SDL_Quit>)
-------------------------------------------------------------------------------
sdlQuit :: IO ()
sdlQuit = C.sdlQuit


-------------------------------------------------------------------------------
-- int SDL_InitSubSystem(Uint32 flags);
-- ''''''''''''''''''''''''''''''''''''
-- | Initialize specific SDL subsystems
-- (<http://wiki.libsdl.org/SDL_InitSubSystem SDL_InitSubSystem>)
-------------------------------------------------------------------------------
sdlInitSubSystem :: [SDL_InitFlag] -> IO ()
sdlInitSubSystem = checkCODE "sdlInitSubSystem" . C.sdlInitSubSystem . toUint32


-------------------------------------------------------------------------------
-- void SDL_QuitSubSystem(Uint32 flags);
-- '''''''''''''''''''''''''''''''''''''
-- | Shut down specific SDL subsystems
-- (<http://wiki.libsdl.org/SDL_QuitSubSystem SDL_QuitSubSystem>)
-------------------------------------------------------------------------------
sdlQuitSubSystem :: [SDL_InitFlag] -> IO ()
sdlQuitSubSystem = C.sdlQuitSubSystem . toUint32


-------------------------------------------------------------------------------
-- Uint32 SDL_WasInit(Uint32 flags);
-- '''''''''''''''''''''''''''''''''
-- | Return a list of initialized subsystems
-- (<http://wiki.libsdl.org/SDL_WasInit SDL_WasInit>)
-------------------------------------------------------------------------------
sdlWasInit :: IO [SDL_InitFlag]
sdlWasInit = fmap fromUint32Mask $ C.sdlWasInit 0



-------------------------------------------------------------------------------
-- ### SDL_version.h ##########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- void SDL_GetVersion(SDL_version * ver);
-- '''''''''''''''''''''''''''''''''''''''
-- | Get the version of SDL that is linked against your program
-- (<http://wiki.libsdl.org/SDL_GetVersion SDL_GetVersion>)
-------------------------------------------------------------------------------
sdlGetVersion :: IO Version
sdlGetVersion = do
  (C.SDL_version x y z) <- alloca $ \ptr -> C.sdlGetVersion ptr >> peek ptr
  return $ Version [fromIntegral x, fromIntegral y, fromIntegral z] []


-------------------------------------------------------------------------------
-- const char * SDL_GetRevision(void);
-- '''''''''''''''''''''''''''''''''''
-- | Get the code revision of SDL that is linked against your program
-- (<http://wiki.libsdl.org/SDL_GetRevision SDL_GetRevision>)
-------------------------------------------------------------------------------
sdlGetRevision :: IO String
sdlGetRevision = C.sdlGetRevision >>= peekCString0


-------------------------------------------------------------------------------
-- int SDL_GetRevisionNumber(void);
-- ''''''''''''''''''''''''''''''''
-- | Get the revision number of SDL that is linked against your program
-- (<http://wiki.libsdl.org/SDL_GetRevisionNumber SDL_GetRevisionNumber>)
-------------------------------------------------------------------------------
sdlGetRevisionNumber :: IO Int
sdlGetRevisionNumber = fmap fromIntegral C.sdlGetRevisionNumber

