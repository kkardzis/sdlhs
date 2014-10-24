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

import Codec.Binary.UTF8.String

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
-- import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#include "LibC0.c"


-------------------------------------------------------------------------------
throwSDLE :: String -> String -> IO a
throwSDLE func desc = throwIO (SDLE func desc)

checkCODE :: String -> (IO CInt) -> IO ()
checkCODE func action =
  action >>= \code -> when (code<0) $ do
    desc <- C.sdlGetError >>= peekCString0
    C.sdlClearError >> throwSDLE func desc

checkNUMB :: String -> (IO CInt) -> IO CInt
checkNUMB func action =
  action >>= \code -> if (code>0) then return code else do
    desc <- C.sdlGetError >>= peekCString0
    C.sdlClearError >> throwSDLE func desc

checkNULL :: String -> (IO (Ptr a)) -> IO (Ptr a)
checkNULL func action =
  action >>= \cs -> if (cs/=nullPtr) then return cs else do
    desc <- C.sdlGetError >>= peekCString0
    C.sdlClearError >> throwSDLE func desc

-------------------------------------------------------------------------------
peekUTF8String :: Ptr CChar -> IO String
peekUTF8String = fmap decode . peekArray0 0 . castPtr

withUTF8String :: String -> (Ptr CChar -> IO a) -> IO a
withUTF8String xs use = withArray0 0 (encode xs) $ \cs -> use (castPtr cs)

peekCString0 :: Ptr CChar -> IO String
peekCString0 cs = if (cs==nullPtr) then return [] else peekUTF8String cs

withCString0 :: String -> (Ptr CChar -> IO a) -> IO a
withCString0 [] action = action nullPtr
withCString0 xs action = withUTF8String xs action



-------------------------------------------------------------------------------
-- ### SDL.h ##################################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- int SDL_Init(Uint32 flags);
-- '''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_Init SDL_Init>
-- | Use this function to initialize the SDL library.
--   This must be called before using any other SDL function.
-------------------------------------------------------------------------------
sdlInit :: IO ()
sdlInit = checkCODE "sdlInit" $ C.sdlInit 0


-------------------------------------------------------------------------------
-- void SDL_Quit(void);
-- ''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_Quit SDL_Quit>
-- | Use this function to clean up all initialized subsystems.
--   You should call it upon all exit conditions. 
-------------------------------------------------------------------------------
sdlQuit :: IO ()
sdlQuit = C.sdlQuit


-------------------------------------------------------------------------------
-- int SDL_InitSubSystem(Uint32 flags);
-- ''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_InitSubSystem SDL_InitSubSystem>
-- | Use this function to initialize specific SDL subsystems.
-------------------------------------------------------------------------------
sdlInitSubSystem :: [SDL_InitFlag] -> IO ()
sdlInitSubSystem = checkCODE "sdlInitSubSystem" . C.sdlInitSubSystem . toUint32


-------------------------------------------------------------------------------
-- void SDL_QuitSubSystem(Uint32 flags);
-- '''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_QuitSubSystem SDL_QuitSubSystem>
-- | Use this function to shut down specific SDL subsystems.
-------------------------------------------------------------------------------
sdlQuitSubSystem :: [SDL_InitFlag] -> IO ()
sdlQuitSubSystem = C.sdlQuitSubSystem . toUint32


-------------------------------------------------------------------------------
-- Uint32 SDL_WasInit(Uint32 flags);
-- '''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_WasInit SDL_WasInit>
-- | Use this function to return a list of the subsystems
--   which have previously been initialized. 
-------------------------------------------------------------------------------
sdlWasInit :: IO [SDL_InitFlag]
sdlWasInit = fmap fromUint32Mask $ C.sdlWasInit 0



-------------------------------------------------------------------------------
-- ### SDL_hints.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_error.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- int SDL_SetError(const char *fmt, ...);
-- '''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_SetError SDL_SetError>
-- | Use this function to set the SDL error string.
-------------------------------------------------------------------------------
sdlSetError :: String -> IO ()
sdlSetError = undefined


-------------------------------------------------------------------------------
-- const char * SDL_GetError(void);
-- ''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetError SDL_GetError>
-- | Use this function to retrieve a message about
--   the last error that occurred.
-------------------------------------------------------------------------------
sdlGetError :: IO String
sdlGetError = C.sdlGetError >>= peekCString0


-------------------------------------------------------------------------------
-- void SDL_ClearError(void);
-- ''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_ClearError SDL_ClearError>
-- | Use this function to clear any previous error message.
-------------------------------------------------------------------------------
sdlClearError :: IO ()
sdlClearError = C.sdlClearError



-------------------------------------------------------------------------------
-- ### SDL_log.h ##############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- void SDL_LogSetAllPriority(SDL_LogPriority priority);
-- '''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_LogSetAllPriority SDL_LogSetAllPriority>
-- | Use this function to set the priority of all log categories.
-------------------------------------------------------------------------------
sdlLogSetAllPriority :: SDL_LogPriority -> IO ()
sdlLogSetAllPriority = C.sdlLogSetAllPriority . toCInt


-------------------------------------------------------------------------------
-- void SDL_LogSetPriority(int category, SDL_LogPriority priority);
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_LogSetPriority SDL_LogSetPriority>
-- | Use this function to set the priority of a particular log category.
-------------------------------------------------------------------------------
sdlLogSetPriority :: SDL_LogCategory -> SDL_LogPriority -> IO ()
sdlLogSetPriority c p = C.sdlLogSetPriority (toCInt c) (toCInt p)


-------------------------------------------------------------------------------
-- SDL_LogPriority SDL_LogGetPriority(int category);
-- '''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_LogGetPriority SDL_LogGetPriority>
-- | Use this function to get the priority of a particular log category.
-------------------------------------------------------------------------------
sdlLogGetPriority :: SDL_LogCategory -> IO SDL_LogPriority
sdlLogGetPriority = fmap fromCInt . C.sdlLogGetPriority . toCInt


-------------------------------------------------------------------------------
-- void SDL_LogResetPriorities(void);
-- ''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_LogResetPriorities SDL_LogResetPriorities>
-- | Use this function to reset all priorities to default.
-------------------------------------------------------------------------------
sdlLogResetPriorities :: IO ()
sdlLogResetPriorities = C.sdlLogResetPriorities



-------------------------------------------------------------------------------
-- ### SDL_assert.h ###########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- ### SDL_version.h ##########################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- void SDL_GetVersion(SDL_version * ver);
-- '''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetVersion SDL_GetVersion>
-- | Use this function to get the version of SDL
--   that is linked against your program.
-------------------------------------------------------------------------------
sdlGetVersion :: IO Version
sdlGetVersion = do
  (C.SDL_version x y z) <- alloca $ \ptr -> C.sdlGetVersion ptr >> peek ptr
  return $ Version [fromIntegral x, fromIntegral y, fromIntegral z] []


-------------------------------------------------------------------------------
-- const char * SDL_GetRevision(void);
-- '''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetRevision SDL_GetRevision>
-- | Use this function to get the code revision of SDL
--   that is linked against your program.
-------------------------------------------------------------------------------
sdlGetRevision :: IO String
sdlGetRevision = C.sdlGetRevision >>= peekCString0


-------------------------------------------------------------------------------
-- int SDL_GetRevisionNumber(void);
-- ''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetRevisionNumber SDL_GetRevisionNumber>
-- | Use this function to get the revision number of SDL
--   that is linked against your program.
-------------------------------------------------------------------------------
sdlGetRevisionNumber :: IO Int
sdlGetRevisionNumber = fmap fromIntegral C.sdlGetRevisionNumber



-------------------------------------------------------------------------------
-- ### SDL_video.h ############################################################
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- int SDL_GetNumVideoDrivers(void);
-- const char * SDL_GetVideoDriver(int index);
-- '''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetNumVideoDrivers SDL_GetNumVideoDrivers>
-- | <http://wiki.libsdl.org/SDL_GetVideoDriver SDL_GetVideoDriver>
-- | Use this function to get the names of video drivers compiled into SDL.
-------------------------------------------------------------------------------
sdlGetVideoDrivers :: IO [String]
sdlGetVideoDrivers = do
  len <- checkNUMB "sdlGetVideoDrivers" C.sdlGetNumVideoDrivers
  mapM (\i -> C.sdlGetVideoDriver i >>= peekCString0) [0 .. (len-1)]


-------------------------------------------------------------------------------
-- int SDL_VideoInit(const char *driver_name);
-- '''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_VideoInit SDL_VideoInit>
-- | Use this function to initialize the video subsystem, optionally
--   specifying a video driver. Pass empty string for the default driver.
-------------------------------------------------------------------------------
sdlVideoInit :: String -> IO ()
sdlVideoInit xs = withCString0 xs (checkCODE "sdlVideoInit" . C.sdlVideoInit)


-------------------------------------------------------------------------------
-- void SDL_VideoQuit(void);
-- '''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_VideoQuit SDL_VideoQuit>
-- | Use this function to shut down the video subsystem,
--   if initialized with SDL_VideoInit().
-------------------------------------------------------------------------------
sdlVideoQuit :: IO ()
sdlVideoQuit = C.sdlVideoQuit


-------------------------------------------------------------------------------
-- const char * SDL_GetCurrentVideoDriver(void);
-- '''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetCurrentVideoDriver SDL_GetCurrentVideoDriver>
-- | Use this function to return the name of the currently
--   initialized video driver. 
-------------------------------------------------------------------------------
sdlGetCurrentVideoDriver :: IO String
sdlGetCurrentVideoDriver = C.sdlGetCurrentVideoDriver >>= peekCString0


-------------------------------------------------------------------------------
-- int SDL_GetNumVideoDisplays(void);
-- ''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetNumVideoDisplays SDL_GetNumVideoDisplays>
-- | Use this function to return the list of available video displays.
-------------------------------------------------------------------------------
sdlGetVideoDisplays :: IO [SDL_Display]
sdlGetVideoDisplays = do
  len <- checkNUMB "sdlGetVideoDisplays" C.sdlGetNumVideoDisplays
  return $ map (SDL_Display) [0 .. (len-1)]


-------------------------------------------------------------------------------
-- const char * SDL_GetDisplayName(int displayIndex);
-- ''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetDisplayName SDL_GetDisplayName>
-- | Use this function to get the name of a display in UTF-8 encoding.
-------------------------------------------------------------------------------
sdlGetDisplayName :: SDL_Display -> IO String
sdlGetDisplayName (SDL_Display i) =
  checkNULL "sdlGetDisplayName" (C.sdlGetDisplayName i) >>= peekCString0


-------------------------------------------------------------------------------
-- int SDL_GetDisplayBounds(int displayIndex, SDL_Rect * rect);
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetDisplayBounds SDL_GetDisplayBounds>
-- | Use this function to get the desktop area represented by a display,
--   with the primary display located at 0,0.
-------------------------------------------------------------------------------
sdlGetDisplayBounds :: SDL_Display -> IO (Int,Int,Int,Int)
sdlGetDisplayBounds (SDL_Display i) = do
  let use = checkCODE "sdlGetDisplayBounds" . C.sdlGetDisplayBounds i
  (C.SDL_Rect x y w h) <- alloca $ \ptr -> use ptr >> peek ptr
  return (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)


-------------------------------------------------------------------------------
-- int SDL_GetNumDisplayModes(int displayIndex);
-- int SDL_GetDisplayMode(int displayIndex, int modeIndex,
--   SDL_DisplayMode * mode);
-- '''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetNumDisplayModes SDL_GetNumDisplayModes>
-- | <http://wiki.libsdl.org/SDL_GetDisplayMode SDL_GetDisplayMode>
-- | Use this function to return the list of available display modes. 
-------------------------------------------------------------------------------
sdlGetDisplayModes :: SDL_Display -> IO [SDL_DisplayMode]
sdlGetDisplayModes (SDL_Display di) = do
  len <- checkNUMB "sdlGetDisplayModes" (C.sdlGetNumDisplayModes di)
  let use mi p = checkCODE "sdlGetDisplayModes" (C.sdlGetDisplayMode di mi p)
  let getmode mi = alloca $ \ptr -> use mi ptr >> peek ptr
  mapM (fmap toHS'SDL_DisplayMode . getmode) [0 .. (len-1)]


-------------------------------------------------------------------------------
-- int SDL_GetDesktopDisplayMode(int displayIndex, SDL_DisplayMode * mode);
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetDesktopDisplayMode SDL_GetDesktopDisplayMode>
-- | Use this function to get information about the desktop display mode.
-------------------------------------------------------------------------------
sdlGetDesktopDisplayMode :: SDL_Display -> IO SDL_DisplayMode
sdlGetDesktopDisplayMode (SDL_Display i) = do
  let use = checkCODE "sdlGetDesktopDisplayMode" . C.sdlGetDesktopDisplayMode i
  fmap toHS'SDL_DisplayMode $ alloca $ \ptr -> use ptr >> peek ptr


-------------------------------------------------------------------------------
-- int SDL_GetCurrentDisplayMode(int displayIndex, SDL_DisplayMode * mode);
-- ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetCurrentDisplayMode SDL_GetCurrentDisplayMode>
-- | Use this function to get information about the current display mode.
-------------------------------------------------------------------------------
sdlGetCurrentDisplayMode :: SDL_Display -> IO SDL_DisplayMode
sdlGetCurrentDisplayMode (SDL_Display i) = do
  let use = checkCODE "sdlGetCurrentDisplayMode" . C.sdlGetCurrentDisplayMode i
  fmap toHS'SDL_DisplayMode $ alloca $ \ptr -> use ptr >> peek ptr


-------------------------------------------------------------------------------
-- SDL_DisplayMode * SDL_GetClosestDisplayMode(int displayIndex,
--   const SDL_DisplayMode * mode, SDL_DisplayMode * closest);
-- '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
-- | <http://wiki.libsdl.org/SDL_GetClosestDisplayMode SDL_GetClosestDisplayMode>
-- | Use this function to get the closest match to the requested display mode.
-------------------------------------------------------------------------------
sdlGetClosestDisplayMode
 :: SDL_Display -> SDL_DisplayMode -> IO (Maybe SDL_DisplayMode)
sdlGetClosestDisplayMode (SDL_Display i) mode =
  with (toCC'SDL_DisplayMode mode) $ \modeptr -> alloca $ \ptr ->
    C.sdlGetClosestDisplayMode i modeptr ptr >>= \res ->
      if (res==nullPtr) then return Nothing else
        fmap (Just . toHS'SDL_DisplayMode) (peek ptr)

