-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL20
-- Copyright   :  Copyright (c) 2014 Krzysztof Kardzis
-- License     :  ISC License (MIT/BSD-style, see LICENSE file for details)
-- 
-- Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <<https://ga-beacon.appspot.com/UA-53767359-1/sdlhs/Graphics-UI-SDL20>>
-------------------------------------------------------------------------------

module Graphics.UI.SDL20
  ( module Graphics.UI.SDL00

  -- |
  -- Using functions from this module requires an explicit linking
  -- with @libsdl\/2.0@ or newer at program runtime:
  --
  -- > main = withlib SDL20 $ do
  -- >   ...
  --
  -- Without that, any foreign call to @libsdl@ will fail.
  --
  -- More info may be found in the <docs/#/README.md docs>.


-------------------------------------------------------------------------------
-- * Basics
-------------------------------------------------------------------------------

  -- ** <http://wiki.libsdl.org/CategoryInit Initialization and Shutdown>
  , sdlInitSubSystem
  , sdlQuitSubSystem
  , sdlWasInit
  , SDL_InitFlag (..)

  -- ** <http://wiki.libsdl.org/CategoryHints Configuration Variables>

  -- ** <http://wiki.libsdl.org/CategoryError Error Handling>
 -- sdlSetError
  , sdlGetError
  , sdlClearError

  -- ** <http://wiki.libsdl.org/CategoryLog Log Handling>
  , sdlLogSetAllPriority
  , sdlLogSetPriority
  , sdlLogGetPriority
  , sdlLogResetPriorities
  , SDL_LogCategory (..)
  , SDL_LogPriority (..)

  -- ** <http://wiki.libsdl.org/CategoryAssertions Assertions>

  -- ** <http://wiki.libsdl.org/CategoryVersion Querying SDL Version>
  , sdlGetVersion
  , sdlGetRevision
  , sdlGetRevisionNumber


-------------------------------------------------------------------------------
-- * Video
-------------------------------------------------------------------------------

  -- ** <http://wiki.libsdl.org/CategoryVideo Display and Window Management>

  -- *** Video Drivers
  , sdlGetVideoDrivers
  , sdlVideoInit
  , sdlVideoQuit
  , sdlGetCurrentVideoDriver

  -- *** Video Displays
  , sdlGetVideoDisplays
  , sdlGetDisplayName
  , sdlGetDisplayBounds
  , sdlGetDisplayModes
  , sdlGetDesktopDisplayMode
  , sdlGetCurrentDisplayMode
  , sdlGetClosestDisplayMode
  , SDL_Display
  , SDL_DisplayMode (..)
  , SDL_PixelFormat (..)

  -- ** 2D Accelerated Rendering

  -- ** Pixel Formats and Conversion Routines

  -- ** Rectangle Functions

  -- ** Surface Creation and Simple Drawing

  -- ** Platform-specific Window Management

  -- ** Clipboard Handling


-------------------------------------------------------------------------------
-- * Input Events
-------------------------------------------------------------------------------

  -- ** Event Handling

  -- ** Keyboard Support

  -- ** Mouse Support

  -- ** Joystick Support

  -- ** Game Controller Support
  

-------------------------------------------------------------------------------
-- * Force Feedback
-------------------------------------------------------------------------------

  -- ** Force Feedback Support
  

-------------------------------------------------------------------------------
-- * Audio
-------------------------------------------------------------------------------

  -- ** Audio Device Management, Playing and Recording
  

-------------------------------------------------------------------------------
-- * Threads
-------------------------------------------------------------------------------

  -- ** Thread Management

  -- ** Thread Synchronization Primitives

  -- ** Atomic Operations
  

-------------------------------------------------------------------------------
-- * Timers
-------------------------------------------------------------------------------

  -- ** Timer Support
  

-------------------------------------------------------------------------------
-- * File Abstraction
-------------------------------------------------------------------------------

  -- ** Filesystem Paths

  -- ** File I/O Abstraction
  

-------------------------------------------------------------------------------
-- * Shared Object Support
-------------------------------------------------------------------------------

  -- ** Shared Object Loading and Function Lookup

  
-------------------------------------------------------------------------------
-- * Platform and CPU Information
-------------------------------------------------------------------------------

  -- ** Platform Detection

  -- ** CPU Feature Detection

  -- ** Byte Order and Byte Swapping

  -- ** Bit Manipulation
  

-------------------------------------------------------------------------------
-- * Power Management
-------------------------------------------------------------------------------

  -- ** Power Management Status


-------------------------------------------------------------------------------
-- * Additional
-------------------------------------------------------------------------------

  -- ** Platform-specific functionality

  -- ** Other


  ) where

import Graphics.UI.SDL00.LibHC
import Graphics.UI.SDL00.LibHT
import Graphics.UI.SDL00

