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

-------------------------------------------------------------------------------
-- ### SDL.h ##################################################################
-------------------------------------------------------------------------------
  , sdlInit
  , sdlInitSubSystem
  , sdlQuitSubSystem
  , sdlWasInit
  , sdlQuit
 
-------------------------------------------------------------------------------
-- ### SDL_hints.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_error.h ############################################################
-------------------------------------------------------------------------------
 -- sdlSetError
  , sdlGetError
  , sdlClearError

-------------------------------------------------------------------------------
-- ### SDL_log.h ##############################################################
-------------------------------------------------------------------------------
  , sdlLogSetAllPriority
  , sdlLogSetPriority
  , sdlLogGetPriority
  , sdlLogResetPriorities
 -- sdlLog
 -- sdlLogVerbose
 -- sdlLogDebug
 -- sdlLogInfo
 -- sdlLogWarn
 -- sdlLogError
 -- sdlLogCritical
 -- sdlLogMessage
 -- sdlLogMessageV
 -- sdlLogGetOutputFunction
 -- sdlLogSetOutputFunction

-------------------------------------------------------------------------------
-- ### SDL_assert.h ###########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_version.h ##########################################################
-------------------------------------------------------------------------------
  , sdlGetVersion
  , sdlGetRevision
  , sdlGetRevisionNumber

-------------------------------------------------------------------------------
-- ### SDL_video.h ############################################################
-------------------------------------------------------------------------------
  , sdlGetNumVideoDrivers
  , sdlGetVideoDriver
  , sdlVideoInit
  , sdlVideoQuit
  , sdlGetCurrentVideoDriver
  , sdlGetNumVideoDisplays
  , sdlGetDisplayName
  , sdlGetDisplayBounds
  , sdlGetNumDisplayModes
  , sdlGetDisplayMode
  , sdlGetDesktopDisplayMode
  , sdlGetCurrentDisplayMode
  , sdlGetClosestDisplayMode
 -- sdlGetWindowDisplayIndex
 -- sdlSetWindowDisplayMode
 -- sdlGetWindowDisplayMode
 -- sdlGetWindowPixelFormat

  ) where

import Graphics.UI.SDL00.LibCT

import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Control.Monad

import System.IO.Unsafe
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
-- ### SDL.h ##################################################################
-------------------------------------------------------------------------------

-- int SDL_Init(Uint32 flags);
#{SAFECALL sdlInit, Uint32, IO CInt}

-- int SDL_InitSubSystem(Uint32 flags);
#{SAFECALL sdlInitSubSystem, Uint32, IO CInt}

-- void SDL_QuitSubSystem(Uint32 flags);
#{SAFECALL sdlQuitSubSystem, Uint32, IO ()}

-- Uint32 SDL_WasInit(Uint32 flags);
#{SAFECALL sdlWasInit, Uint32, IO Uint32}

-- void SDL_Quit(void);
#{SAFECALL sdlQuit, IO ()}



-------------------------------------------------------------------------------
-- ### SDL_hints.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_error.h ############################################################
-------------------------------------------------------------------------------

-- int SDL_SetError(const char *fmt, ...);
-- #{SAFECALL sdlSetError, Ptr CChar, IO CInt}

-- const char * SDL_GetError(void);
#{SAFECALL sdlGetError, IO (Ptr CChar)}

-- void SDL_ClearError(void);
#{SAFECALL sdlClearError, IO ()}



-------------------------------------------------------------------------------
-- ### SDL_log.h ##############################################################
-------------------------------------------------------------------------------

-- void SDL_LogSetAllPriority(SDL_LogPriority priority);
#{SAFECALL sdlLogSetAllPriority, SDL_LogPriority, IO ()}

-- void SDL_LogSetPriority(int category, SDL_LogPriority priority);
#{SAFECALL sdlLogSetPriority, CInt, SDL_LogPriority, IO ()}

-- SDL_LogPriority SDL_LogGetPriority(int category);
#{SAFECALL sdlLogGetPriority, CInt, IO SDL_LogPriority}

-- void SDL_LogResetPriorities(void);
#{SAFECALL sdlLogResetPriorities, IO ()}

-- void SDL_Log(const char *fmt, ...);

-- void SDL_LogVerbose(int category, const char *fmt, ...);

-- void SDL_LogDebug(int category, const char *fmt, ...);

-- void SDL_LogInfo(int category, const char *fmt, ...);

-- void SDL_LogWarn(int category, const char *fmt, ...);

-- void SDL_LogError(int category, const char *fmt, ...);

-- void SDL_LogCritical(int category, const char *fmt, ...);

-- void SDL_LogMessage(int category, SDL_LogPriority priority, const char *fmt, ...);

-- void SDL_LogMessageV(int category, SDL_LogPriority priority, const char *fmt, va_list ap);

-- void SDL_LogGetOutputFunction(SDL_LogOutputFunction *callback, void **userdata);

-- void SDL_LogSetOutputFunction(SDL_LogOutputFunction callback, void *userdata);



-------------------------------------------------------------------------------
-- ### SDL_assert.h ###########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_version.h ##########################################################
-------------------------------------------------------------------------------

-- void SDL_GetVersion(SDL_version * ver);
#{SAFECALL sdlGetVersion, NonConstPtr SDL_version, IO ()}

-- const char * SDL_GetRevision(void);
#{SAFECALL sdlGetRevision, IO (Ptr CChar)}

-- int SDL_GetRevisionNumber(void);
#{SAFECALL sdlGetRevisionNumber, IO CInt}



-------------------------------------------------------------------------------
-- ### SDL_video.h ############################################################
-------------------------------------------------------------------------------

-- int SDL_GetNumVideoDrivers(void);
#{SAFECALL sdlGetNumVideoDrivers, IO CInt}

-- const char * SDL_GetVideoDriver(int index);
#{SAFECALL sdlGetVideoDriver, CInt, IO (Ptr CChar)}

-- int SDL_VideoInit(const char *driver_name);
#{SAFECALL sdlVideoInit, Ptr CChar, IO CInt}

-- void SDL_VideoQuit(void);
#{SAFECALL sdlVideoQuit, IO ()}

-- const char * SDL_GetCurrentVideoDriver(void);
#{SAFECALL sdlGetCurrentVideoDriver, IO (Ptr CChar)}

-- int SDL_GetNumVideoDisplays(void);
#{SAFECALL sdlGetNumVideoDisplays, IO CInt}

-- const char * SDL_GetDisplayName(int displayIndex);
#{SAFECALL sdlGetDisplayName, CInt, IO (Ptr CChar)}

-- int SDL_GetDisplayBounds(int displayIndex, SDL_Rect * rect);
#{SAFECALL sdlGetDisplayBounds, CInt, NonConstPtr SDL_Rect, IO CInt}

-- int SDL_GetNumDisplayModes(int displayIndex);
#{SAFECALL sdlGetNumDisplayModes, CInt, IO CInt}

-- int SDL_GetDisplayMode(int displayIndex, int modeIndex,
--   SDL_DisplayMode * mode);
#{SAFECALL sdlGetDisplayMode, CInt, CInt, NonConstPtr SDL_DisplayMode, IO CInt}

-- int SDL_GetDesktopDisplayMode(int displayIndex, SDL_DisplayMode * mode);
#{SAFECALL sdlGetDesktopDisplayMode, CInt, NonConstPtr SDL_DisplayMode, IO CInt}

-- int SDL_GetCurrentDisplayMode(int displayIndex, SDL_DisplayMode * mode);
#{SAFECALL sdlGetCurrentDisplayMode, CInt, NonConstPtr SDL_DisplayMode, IO CInt}

-- SDL_DisplayMode * SDL_GetClosestDisplayMode(int displayIndex,
--   const SDL_DisplayMode * mode, SDL_DisplayMode * closest);
#{SAFECALL sdlGetClosestDisplayMode, CInt, Ptr SDL_DisplayMode \
, NonConstPtr SDL_DisplayMode, IO (NonConstPtr SDL_DisplayMode)}

-- int SDL_GetWindowDisplayIndex(SDL_Window * window);

-- int SDL_SetWindowDisplayMode(SDL_Window * window,
-- const SDL_DisplayMode * mode);

-- int SDL_GetWindowDisplayMode(SDL_Window * window, SDL_DisplayMode * mode);

-- Uint32 SDL_GetWindowPixelFormat(SDL_Window * window);

