/* ------------------------------------------------------------------------- */
/* |                                                                         */
/* Module      :  Graphics.UI.SDL00.LibC0                                    */
/* Copyright   :  Copyright (c) 2014 Krzysztof Kardzis                       */
/* License     :  ISC License (MIT/BSD-style, see LICENSE file for details)  */
/*                                                                           */
/* Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>                     */
/* Stability   :  experimental                                               */
/* Portability :  non-portable                                               */
/*                                                                           */
/* ------------------------------------------------------------------------- */

#include "SDL2-2.0.3/SDL.h"

#include "RTLD.h"


/* ------------------------------------------------------------------------- */
/* global symbol table (for run-time linking)                                */
/* ------------------------------------------------------------------------- */
enum { SDL20 };

#define SDLXX SDL20
#define SYMTAB sdlSYMTAB
#define ADRTAB sdlADRTAB

SYMTABENTRY SYMTAB[] =
/* ------------------------------------------------------------------------- */
/* ### SDL.h ############################################################### */
/* ------------------------------------------------------------------------- */
  { {SDL20, SDLXX, "SDL_Init"}
  , {SDL20, SDLXX, "SDL_InitSubSystem"}
  , {SDL20, SDLXX, "SDL_QuitSubSystem"}
  , {SDL20, SDLXX, "SDL_WasInit"}
  , {SDL20, SDLXX, "SDL_Quit"}
 
/* ------------------------------------------------------------------------- */
/* ### SDL_hints.h ######################################################### */
/* ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- */
/* ### SDL_error.h ######################################################### */
/* ------------------------------------------------------------------------- */
  , {SDL20, SDLXX, "SDL_SetError"}
  , {SDL20, SDLXX, "SDL_GetError"}
  , {SDL20, SDLXX, "SDL_ClearError"}

/* ------------------------------------------------------------------------- */
/* ### SDL_log.h ########################################################### */
/* ------------------------------------------------------------------------- */
  , {SDL20, SDLXX, "SDL_LogSetAllPriority"}
  , {SDL20, SDLXX, "SDL_LogSetPriority"}
  , {SDL20, SDLXX, "SDL_LogGetPriority"}
  , {SDL20, SDLXX, "SDL_LogResetPriorities"}
  , {SDL20, SDLXX, "SDL_Log"}
  , {SDL20, SDLXX, "SDL_LogVerbose"}
  , {SDL20, SDLXX, "SDL_LogDebug"}
  , {SDL20, SDLXX, "SDL_LogInfo"}
  , {SDL20, SDLXX, "SDL_LogWarn"}
  , {SDL20, SDLXX, "SDL_LogError"}
  , {SDL20, SDLXX, "SDL_LogCritical"}
  , {SDL20, SDLXX, "SDL_LogMessage"}
  , {SDL20, SDLXX, "SDL_LogMessageV"}
  , {SDL20, SDLXX, "SDL_LogGetOutputFunction"}
  , {SDL20, SDLXX, "SDL_LogSetOutputFunction"}

/* ------------------------------------------------------------------------- */
/* ### SDL_assert.h ######################################################## */
/* ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- */
/* ### SDL_version.h ####################################################### */
/* ------------------------------------------------------------------------- */
  , {SDL20, SDLXX, "SDL_GetVersion"}
  , {SDL20, SDLXX, "SDL_GetRevision"}
  , {SDL20, SDLXX, "SDL_GetRevisionNumber"}

/* ------------------------------------------------------------------------- */
/* ### SDL_video.h ######################################################### */
/* ------------------------------------------------------------------------- */
  , {SDL20, SDLXX, "SDL_GetNumVideoDrivers"}
  , {SDL20, SDLXX, "SDL_GetVideoDriver"}
  , {SDL20, SDLXX, "SDL_VideoInit"}
  , {SDL20, SDLXX, "SDL_VideoQuit"}
  , {SDL20, SDLXX, "SDL_GetCurrentVideoDriver"}
  , {SDL20, SDLXX, "SDL_GetNumVideoDisplays"}
  , {SDL20, SDLXX, "SDL_GetDisplayName"}
  , {SDL20, SDLXX, "SDL_GetDisplayBounds"}
  , {SDL20, SDLXX, "SDL_GetNumDisplayModes"}
  , {SDL20, SDLXX, "SDL_GetDisplayMode"}
  , {SDL20, SDLXX, "SDL_GetDesktopDisplayMode"}
  , {SDL20, SDLXX, "SDL_GetCurrentDisplayMode"}
  , {SDL20, SDLXX, "SDL_GetClosestDisplayMode"}
  , {SDL20, SDLXX, "SDL_GetWindowDisplayIndex"}
  , {SDL20, SDLXX, "SDL_SetWindowDisplayMode"}
  , {SDL20, SDLXX, "SDL_GetWindowDisplayMode"}
  , {SDL20, SDLXX, "SDL_GetWindowPixelFormat"}


/* ------------------------------------------------------------------------- */
//, {SDL20, SDLXX, "SDL_AddEventWatch"}
//, {SDL20, SDLXX, "SDL_AddHintCallback"}
//, {SDL20, SDLXX, "SDL_AddTimer"}
//, {SDL20, SDLXX, "SDL_AllocFormat"}
//, {SDL20, SDLXX, "SDL_AllocPalette"}
//, {SDL20, SDLXX, "SDL_AllocRW"}
//, {SDL20, SDLXX, "SDL_AtomicAdd"}
//, {SDL20, SDLXX, "SDL_AtomicCAS"}
//, {SDL20, SDLXX, "SDL_AtomicCASPtr"}
//, {SDL20, SDLXX, "SDL_AtomicGet"}
//, {SDL20, SDLXX, "SDL_AtomicGetPtr"}
//, {SDL20, SDLXX, "SDL_AtomicLock"}
//, {SDL20, SDLXX, "SDL_AtomicSet"}
//, {SDL20, SDLXX, "SDL_AtomicSetPtr"}
//, {SDL20, SDLXX, "SDL_AtomicTryLock"}
//, {SDL20, SDLXX, "SDL_AtomicUnlock"}
//, {SDL20, SDLXX, "SDL_AudioInit"}
//, {SDL20, SDLXX, "SDL_AudioQuit"}
//, {SDL20, SDLXX, "SDL_BuildAudioCVT"}
//, {SDL20, SDLXX, "SDL_CalculateGammaRamp"}
//, {SDL20, SDLXX, "SDL_ClearHints"}
//, {SDL20, SDLXX, "SDL_CloseAudio"}
//, {SDL20, SDLXX, "SDL_CloseAudioDevice"}
//, {SDL20, SDLXX, "SDL_CondBroadcast"}
//, {SDL20, SDLXX, "SDL_CondSignal"}
//, {SDL20, SDLXX, "SDL_CondWait"}
//, {SDL20, SDLXX, "SDL_CondWaitTimeout"}
//, {SDL20, SDLXX, "SDL_ConvertAudio"}
//, {SDL20, SDLXX, "SDL_ConvertPixels"}
//, {SDL20, SDLXX, "SDL_ConvertSurface"}
//, {SDL20, SDLXX, "SDL_ConvertSurfaceFormat"}
//, {SDL20, SDLXX, "SDL_CreateColorCursor"}
//, {SDL20, SDLXX, "SDL_CreateCond"}
//, {SDL20, SDLXX, "SDL_CreateCursor"}
//, {SDL20, SDLXX, "SDL_CreateMutex"}
//, {SDL20, SDLXX, "SDL_CreateRGBSurface"}
//, {SDL20, SDLXX, "SDL_CreateRGBSurfaceFrom"}
//, {SDL20, SDLXX, "SDL_CreateRenderer"}
//, {SDL20, SDLXX, "SDL_CreateSemaphore"}
//, {SDL20, SDLXX, "SDL_CreateShapedWindow"}
//, {SDL20, SDLXX, "SDL_CreateSoftwareRenderer"}
//, {SDL20, SDLXX, "SDL_CreateSystemCursor"}
//, {SDL20, SDLXX, "SDL_CreateTexture"}
//, {SDL20, SDLXX, "SDL_CreateTextureFromSurface"}
//, {SDL20, SDLXX, "SDL_CreateThread"}
//, {SDL20, SDLXX, "SDL_CreateWindow"}
//, {SDL20, SDLXX, "SDL_CreateWindowAndRenderer"}
//, {SDL20, SDLXX, "SDL_CreateWindowFrom"}
//, {SDL20, SDLXX, "SDL_DXGIGetOutputInfo"}
//, {SDL20, SDLXX, "SDL_DYNAPI_entry"}
//, {SDL20, SDLXX, "SDL_DelEventWatch"}
//, {SDL20, SDLXX, "SDL_DelHintCallback"}
//, {SDL20, SDLXX, "SDL_Delay"}
//, {SDL20, SDLXX, "SDL_DestroyCond"}
//, {SDL20, SDLXX, "SDL_DestroyMutex"}
//, {SDL20, SDLXX, "SDL_DestroyRenderer"}
//, {SDL20, SDLXX, "SDL_DestroySemaphore"}
//, {SDL20, SDLXX, "SDL_DestroyTexture"}
//, {SDL20, SDLXX, "SDL_DestroyWindow"}
//, {SDL20, SDLXX, "SDL_DetachThread"}
//, {SDL20, SDLXX, "SDL_Direct3D9GetAdapterIndex"}
//, {SDL20, SDLXX, "SDL_DisableScreenSaver"}
//, {SDL20, SDLXX, "SDL_EnableScreenSaver"}
//, {SDL20, SDLXX, "SDL_EnclosePoints"}
//, {SDL20, SDLXX, "SDL_Error"}
//, {SDL20, SDLXX, "SDL_EventState"}
//, {SDL20, SDLXX, "SDL_FillRect"}
//, {SDL20, SDLXX, "SDL_FillRects"}
//, {SDL20, SDLXX, "SDL_FilterEvents"}
//, {SDL20, SDLXX, "SDL_FlushEvent"}
//, {SDL20, SDLXX, "SDL_FlushEvents"}
//, {SDL20, SDLXX, "SDL_FreeCursor"}
//, {SDL20, SDLXX, "SDL_FreeFormat"}
//, {SDL20, SDLXX, "SDL_FreePalette"}
//, {SDL20, SDLXX, "SDL_FreeRW"}
//, {SDL20, SDLXX, "SDL_FreeSurface"}
//, {SDL20, SDLXX, "SDL_FreeWAV"}
//, {SDL20, SDLXX, "SDL_GL_BindTexture"}
//, {SDL20, SDLXX, "SDL_GL_CreateContext"}
//, {SDL20, SDLXX, "SDL_GL_DeleteContext"}
//, {SDL20, SDLXX, "SDL_GL_ExtensionSupported"}
//, {SDL20, SDLXX, "SDL_GL_GetAttribute"}
//, {SDL20, SDLXX, "SDL_GL_GetCurrentContext"}
//, {SDL20, SDLXX, "SDL_GL_GetCurrentWindow"}
//, {SDL20, SDLXX, "SDL_GL_GetDrawableSize"}
//, {SDL20, SDLXX, "SDL_GL_GetProcAddress"}
//, {SDL20, SDLXX, "SDL_GL_GetSwapInterval"}
//, {SDL20, SDLXX, "SDL_GL_LoadLibrary"}
//, {SDL20, SDLXX, "SDL_GL_MakeCurrent"}
//, {SDL20, SDLXX, "SDL_GL_ResetAttributes"}
//, {SDL20, SDLXX, "SDL_GL_SetAttribute"}
//, {SDL20, SDLXX, "SDL_GL_SetSwapInterval"}
//, {SDL20, SDLXX, "SDL_GL_SwapWindow"}
//, {SDL20, SDLXX, "SDL_GL_UnbindTexture"}
//, {SDL20, SDLXX, "SDL_GL_UnloadLibrary"}
//, {SDL20, SDLXX, "SDL_GameControllerAddMapping"}
//, {SDL20, SDLXX, "SDL_GameControllerAddMappingsFromRW"}
//, {SDL20, SDLXX, "SDL_GameControllerClose"}
//, {SDL20, SDLXX, "SDL_GameControllerEventState"}
//, {SDL20, SDLXX, "SDL_GameControllerGetAttached"}
//, {SDL20, SDLXX, "SDL_GameControllerGetAxis"}
//, {SDL20, SDLXX, "SDL_GameControllerGetAxisFromString"}
//, {SDL20, SDLXX, "SDL_GameControllerGetBindForAxis"}
//, {SDL20, SDLXX, "SDL_GameControllerGetBindForButton"}
//, {SDL20, SDLXX, "SDL_GameControllerGetButton"}
//, {SDL20, SDLXX, "SDL_GameControllerGetButtonFromString"}
//, {SDL20, SDLXX, "SDL_GameControllerGetJoystick"}
//, {SDL20, SDLXX, "SDL_GameControllerGetStringForAxis"}
//, {SDL20, SDLXX, "SDL_GameControllerGetStringForButton"}
//, {SDL20, SDLXX, "SDL_GameControllerMapping"}
//, {SDL20, SDLXX, "SDL_GameControllerMappingForGUID"}
//, {SDL20, SDLXX, "SDL_GameControllerName"}
//, {SDL20, SDLXX, "SDL_GameControllerNameForIndex"}
//, {SDL20, SDLXX, "SDL_GameControllerOpen"}
//, {SDL20, SDLXX, "SDL_GameControllerUpdate"}
//, {SDL20, SDLXX, "SDL_GetAssertionHandler"}
//, {SDL20, SDLXX, "SDL_GetAssertionReport"}
//, {SDL20, SDLXX, "SDL_GetAudioDeviceName"}
//, {SDL20, SDLXX, "SDL_GetAudioDeviceStatus"}
//, {SDL20, SDLXX, "SDL_GetAudioDriver"}
//, {SDL20, SDLXX, "SDL_GetAudioStatus"}
//, {SDL20, SDLXX, "SDL_GetBasePath"}
//, {SDL20, SDLXX, "SDL_GetCPUCacheLineSize"}
//, {SDL20, SDLXX, "SDL_GetCPUCount"}
//, {SDL20, SDLXX, "SDL_GetClipRect"}
//, {SDL20, SDLXX, "SDL_GetClipboardText"}
//, {SDL20, SDLXX, "SDL_GetColorKey"}
//, {SDL20, SDLXX, "SDL_GetCurrentAudioDriver"}
//, {SDL20, SDLXX, "SDL_GetCursor"}
//, {SDL20, SDLXX, "SDL_GetDefaultAssertionHandler"}
//, {SDL20, SDLXX, "SDL_GetDefaultCursor"}
//, {SDL20, SDLXX, "SDL_GetEventFilter"}
//, {SDL20, SDLXX, "SDL_GetHint"}
//, {SDL20, SDLXX, "SDL_GetKeyFromName"}
//, {SDL20, SDLXX, "SDL_GetKeyFromScancode"}
//, {SDL20, SDLXX, "SDL_GetKeyName"}
//, {SDL20, SDLXX, "SDL_GetKeyboardFocus"}
//, {SDL20, SDLXX, "SDL_GetKeyboardState"}
//, {SDL20, SDLXX, "SDL_GetModState"}
//, {SDL20, SDLXX, "SDL_GetMouseFocus"}
//, {SDL20, SDLXX, "SDL_GetMouseState"}
//, {SDL20, SDLXX, "SDL_GetNumAudioDevices"}
//, {SDL20, SDLXX, "SDL_GetNumAudioDrivers"}
//, {SDL20, SDLXX, "SDL_GetNumRenderDrivers"}
//, {SDL20, SDLXX, "SDL_GetNumTouchDevices"}
//, {SDL20, SDLXX, "SDL_GetNumTouchFingers"}
//, {SDL20, SDLXX, "SDL_GetPerformanceCounter"}
//, {SDL20, SDLXX, "SDL_GetPerformanceFrequency"}
//, {SDL20, SDLXX, "SDL_GetPixelFormatName"}
//, {SDL20, SDLXX, "SDL_GetPlatform"}
//, {SDL20, SDLXX, "SDL_GetPowerInfo"}
//, {SDL20, SDLXX, "SDL_GetPrefPath"}
//, {SDL20, SDLXX, "SDL_GetRGB"}
//, {SDL20, SDLXX, "SDL_GetRGBA"}
//, {SDL20, SDLXX, "SDL_GetRelativeMouseMode"}
//, {SDL20, SDLXX, "SDL_GetRelativeMouseState"}
//, {SDL20, SDLXX, "SDL_GetRenderDrawBlendMode"}
//, {SDL20, SDLXX, "SDL_GetRenderDrawColor"}
//, {SDL20, SDLXX, "SDL_GetRenderDriverInfo"}
//, {SDL20, SDLXX, "SDL_GetRenderTarget"}
//, {SDL20, SDLXX, "SDL_GetRenderer"}
//, {SDL20, SDLXX, "SDL_GetRendererInfo"}
//, {SDL20, SDLXX, "SDL_GetRendererOutputSize"}
//, {SDL20, SDLXX, "SDL_GetScancodeFromKey"}
//, {SDL20, SDLXX, "SDL_GetScancodeFromName"}
//, {SDL20, SDLXX, "SDL_GetScancodeName"}
//, {SDL20, SDLXX, "SDL_GetShapedWindowMode"}
//, {SDL20, SDLXX, "SDL_GetSurfaceAlphaMod"}
//, {SDL20, SDLXX, "SDL_GetSurfaceBlendMode"}
//, {SDL20, SDLXX, "SDL_GetSurfaceColorMod"}
//, {SDL20, SDLXX, "SDL_GetSystemRAM"}
//, {SDL20, SDLXX, "SDL_GetTextureAlphaMod"}
//, {SDL20, SDLXX, "SDL_GetTextureBlendMode"}
//, {SDL20, SDLXX, "SDL_GetTextureColorMod"}
//, {SDL20, SDLXX, "SDL_GetThreadID"}
//, {SDL20, SDLXX, "SDL_GetThreadName"}
//, {SDL20, SDLXX, "SDL_GetTicks"}
//, {SDL20, SDLXX, "SDL_GetTouchDevice"}
//, {SDL20, SDLXX, "SDL_GetTouchFinger"}
//, {SDL20, SDLXX, "SDL_GetWindowBrightness"}
//, {SDL20, SDLXX, "SDL_GetWindowData"}
//, {SDL20, SDLXX, "SDL_GetWindowFlags"}
//, {SDL20, SDLXX, "SDL_GetWindowFromID"}
//, {SDL20, SDLXX, "SDL_GetWindowGammaRamp"}
//, {SDL20, SDLXX, "SDL_GetWindowGrab"}
//, {SDL20, SDLXX, "SDL_GetWindowID"}
//, {SDL20, SDLXX, "SDL_GetWindowMaximumSize"}
//, {SDL20, SDLXX, "SDL_GetWindowMinimumSize"}
//, {SDL20, SDLXX, "SDL_GetWindowPosition"}
//, {SDL20, SDLXX, "SDL_GetWindowSize"}
//, {SDL20, SDLXX, "SDL_GetWindowSurface"}
//, {SDL20, SDLXX, "SDL_GetWindowTitle"}
//, {SDL20, SDLXX, "SDL_GetWindowWMInfo"}
//, {SDL20, SDLXX, "SDL_HapticClose"}
//, {SDL20, SDLXX, "SDL_HapticDestroyEffect"}
//, {SDL20, SDLXX, "SDL_HapticEffectSupported"}
//, {SDL20, SDLXX, "SDL_HapticGetEffectStatus"}
//, {SDL20, SDLXX, "SDL_HapticIndex"}
//, {SDL20, SDLXX, "SDL_HapticName"}
//, {SDL20, SDLXX, "SDL_HapticNewEffect"}
//, {SDL20, SDLXX, "SDL_HapticNumAxes"}
//, {SDL20, SDLXX, "SDL_HapticNumEffects"}
//, {SDL20, SDLXX, "SDL_HapticNumEffectsPlaying"}
//, {SDL20, SDLXX, "SDL_HapticOpen"}
//, {SDL20, SDLXX, "SDL_HapticOpenFromJoystick"}
//, {SDL20, SDLXX, "SDL_HapticOpenFromMouse"}
//, {SDL20, SDLXX, "SDL_HapticOpened"}
//, {SDL20, SDLXX, "SDL_HapticPause"}
//, {SDL20, SDLXX, "SDL_HapticQuery"}
//, {SDL20, SDLXX, "SDL_HapticRumbleInit"}
//, {SDL20, SDLXX, "SDL_HapticRumblePlay"}
//, {SDL20, SDLXX, "SDL_HapticRumbleStop"}
//, {SDL20, SDLXX, "SDL_HapticRumbleSupported"}
//, {SDL20, SDLXX, "SDL_HapticRunEffect"}
//, {SDL20, SDLXX, "SDL_HapticSetAutocenter"}
//, {SDL20, SDLXX, "SDL_HapticSetGain"}
//, {SDL20, SDLXX, "SDL_HapticStopAll"}
//, {SDL20, SDLXX, "SDL_HapticStopEffect"}
//, {SDL20, SDLXX, "SDL_HapticUnpause"}
//, {SDL20, SDLXX, "SDL_HapticUpdateEffect"}
//, {SDL20, SDLXX, "SDL_Has3DNow"}
//, {SDL20, SDLXX, "SDL_HasAVX"}
//, {SDL20, SDLXX, "SDL_HasAltiVec"}
//, {SDL20, SDLXX, "SDL_HasClipboardText"}
//, {SDL20, SDLXX, "SDL_HasEvent"}
//, {SDL20, SDLXX, "SDL_HasEvents"}
//, {SDL20, SDLXX, "SDL_HasIntersection"}
//, {SDL20, SDLXX, "SDL_HasMMX"}
//, {SDL20, SDLXX, "SDL_HasRDTSC"}
//, {SDL20, SDLXX, "SDL_HasSSE"}
//, {SDL20, SDLXX, "SDL_HasSSE2"}
//, {SDL20, SDLXX, "SDL_HasSSE3"}
//, {SDL20, SDLXX, "SDL_HasSSE41"}
//, {SDL20, SDLXX, "SDL_HasSSE42"}
//, {SDL20, SDLXX, "SDL_HasScreenKeyboardSupport"}
//, {SDL20, SDLXX, "SDL_HideWindow"}
//, {SDL20, SDLXX, "SDL_IntersectRect"}
//, {SDL20, SDLXX, "SDL_IntersectRectAndLine"}
//, {SDL20, SDLXX, "SDL_IsGameController"}
//, {SDL20, SDLXX, "SDL_IsScreenKeyboardShown"}
//, {SDL20, SDLXX, "SDL_IsScreenSaverEnabled"}
//, {SDL20, SDLXX, "SDL_IsShapedWindow"}
//, {SDL20, SDLXX, "SDL_IsTextInputActive"}
//, {SDL20, SDLXX, "SDL_JoystickClose"}
//, {SDL20, SDLXX, "SDL_JoystickEventState"}
//, {SDL20, SDLXX, "SDL_JoystickGetAttached"}
//, {SDL20, SDLXX, "SDL_JoystickGetAxis"}
//, {SDL20, SDLXX, "SDL_JoystickGetBall"}
//, {SDL20, SDLXX, "SDL_JoystickGetButton"}
//, {SDL20, SDLXX, "SDL_JoystickGetDeviceGUID"}
//, {SDL20, SDLXX, "SDL_JoystickGetGUID"}
//, {SDL20, SDLXX, "SDL_JoystickGetGUIDFromString"}
//, {SDL20, SDLXX, "SDL_JoystickGetGUIDString"}
//, {SDL20, SDLXX, "SDL_JoystickGetHat"}
//, {SDL20, SDLXX, "SDL_JoystickInstanceID"}
//, {SDL20, SDLXX, "SDL_JoystickIsHaptic"}
//, {SDL20, SDLXX, "SDL_JoystickName"}
//, {SDL20, SDLXX, "SDL_JoystickNameForIndex"}
//, {SDL20, SDLXX, "SDL_JoystickNumAxes"}
//, {SDL20, SDLXX, "SDL_JoystickNumBalls"}
//, {SDL20, SDLXX, "SDL_JoystickNumButtons"}
//, {SDL20, SDLXX, "SDL_JoystickNumHats"}
//, {SDL20, SDLXX, "SDL_JoystickOpen"}
//, {SDL20, SDLXX, "SDL_JoystickUpdate"}
//, {SDL20, SDLXX, "SDL_LoadBMP_RW"}
//, {SDL20, SDLXX, "SDL_LoadDollarTemplates"}
//, {SDL20, SDLXX, "SDL_LoadFunction"}
//, {SDL20, SDLXX, "SDL_LoadObject"}
//, {SDL20, SDLXX, "SDL_LoadWAV_RW"}
//, {SDL20, SDLXX, "SDL_LockAudio"}
//, {SDL20, SDLXX, "SDL_LockAudioDevice"}
//, {SDL20, SDLXX, "SDL_LockMutex"}
//, {SDL20, SDLXX, "SDL_LockSurface"}
//, {SDL20, SDLXX, "SDL_LockTexture"}
//, {SDL20, SDLXX, "SDL_LowerBlit"}
//, {SDL20, SDLXX, "SDL_LowerBlitScaled"}
//, {SDL20, SDLXX, "SDL_MapRGB"}
//, {SDL20, SDLXX, "SDL_MapRGBA"}
//, {SDL20, SDLXX, "SDL_MasksToPixelFormatEnum"}
//, {SDL20, SDLXX, "SDL_MaximizeWindow"}
//, {SDL20, SDLXX, "SDL_MinimizeWindow"}
//, {SDL20, SDLXX, "SDL_MixAudio"}
//, {SDL20, SDLXX, "SDL_MixAudioFormat"}
//, {SDL20, SDLXX, "SDL_MouseIsHaptic"}
//, {SDL20, SDLXX, "SDL_NumHaptics"}
//, {SDL20, SDLXX, "SDL_NumJoysticks"}
//, {SDL20, SDLXX, "SDL_OpenAudio"}
//, {SDL20, SDLXX, "SDL_OpenAudioDevice"}
//, {SDL20, SDLXX, "SDL_PauseAudio"}
//, {SDL20, SDLXX, "SDL_PauseAudioDevice"}
//, {SDL20, SDLXX, "SDL_PeepEvents"}
//, {SDL20, SDLXX, "SDL_PixelFormatEnumToMasks"}
//, {SDL20, SDLXX, "SDL_PollEvent"}
//, {SDL20, SDLXX, "SDL_PumpEvents"}
//, {SDL20, SDLXX, "SDL_PushEvent"}
//, {SDL20, SDLXX, "SDL_QueryTexture"}
//, {SDL20, SDLXX, "SDL_RWFromConstMem"}
//, {SDL20, SDLXX, "SDL_RWFromFP"}
//, {SDL20, SDLXX, "SDL_RWFromFile"}
//, {SDL20, SDLXX, "SDL_RWFromMem"}
//, {SDL20, SDLXX, "SDL_RaiseWindow"}
//, {SDL20, SDLXX, "SDL_ReadBE16"}
//, {SDL20, SDLXX, "SDL_ReadBE32"}
//, {SDL20, SDLXX, "SDL_ReadBE64"}
//, {SDL20, SDLXX, "SDL_ReadLE16"}
//, {SDL20, SDLXX, "SDL_ReadLE32"}
//, {SDL20, SDLXX, "SDL_ReadLE64"}
//, {SDL20, SDLXX, "SDL_ReadU8"}
//, {SDL20, SDLXX, "SDL_RecordGesture"}
//, {SDL20, SDLXX, "SDL_RegisterApp"}
//, {SDL20, SDLXX, "SDL_RegisterEvents"}
//, {SDL20, SDLXX, "SDL_RemoveTimer"}
//, {SDL20, SDLXX, "SDL_RenderClear"}
//, {SDL20, SDLXX, "SDL_RenderCopy"}
//, {SDL20, SDLXX, "SDL_RenderCopyEx"}
//, {SDL20, SDLXX, "SDL_RenderDrawLine"}
//, {SDL20, SDLXX, "SDL_RenderDrawLines"}
//, {SDL20, SDLXX, "SDL_RenderDrawPoint"}
//, {SDL20, SDLXX, "SDL_RenderDrawPoints"}
//, {SDL20, SDLXX, "SDL_RenderDrawRect"}
//, {SDL20, SDLXX, "SDL_RenderDrawRects"}
//, {SDL20, SDLXX, "SDL_RenderFillRect"}
//, {SDL20, SDLXX, "SDL_RenderFillRects"}
//, {SDL20, SDLXX, "SDL_RenderGetClipRect"}
//, {SDL20, SDLXX, "SDL_RenderGetD3D9Device"}
//, {SDL20, SDLXX, "SDL_RenderGetLogicalSize"}
//, {SDL20, SDLXX, "SDL_RenderGetScale"}
//, {SDL20, SDLXX, "SDL_RenderGetViewport"}
//, {SDL20, SDLXX, "SDL_RenderPresent"}
//, {SDL20, SDLXX, "SDL_RenderReadPixels"}
//, {SDL20, SDLXX, "SDL_RenderSetClipRect"}
//, {SDL20, SDLXX, "SDL_RenderSetLogicalSize"}
//, {SDL20, SDLXX, "SDL_RenderSetScale"}
//, {SDL20, SDLXX, "SDL_RenderSetViewport"}
//, {SDL20, SDLXX, "SDL_RenderTargetSupported"}
//, {SDL20, SDLXX, "SDL_ReportAssertion"}
//, {SDL20, SDLXX, "SDL_ResetAssertionReport"}
//, {SDL20, SDLXX, "SDL_RestoreWindow"}
//, {SDL20, SDLXX, "SDL_SaveAllDollarTemplates"}
//, {SDL20, SDLXX, "SDL_SaveBMP_RW"}
//, {SDL20, SDLXX, "SDL_SaveDollarTemplate"}
//, {SDL20, SDLXX, "SDL_SemPost"}
//, {SDL20, SDLXX, "SDL_SemTryWait"}
//, {SDL20, SDLXX, "SDL_SemValue"}
//, {SDL20, SDLXX, "SDL_SemWait"}
//, {SDL20, SDLXX, "SDL_SemWaitTimeout"}
//, {SDL20, SDLXX, "SDL_SetAssertionHandler"}
//, {SDL20, SDLXX, "SDL_SetClipRect"}
//, {SDL20, SDLXX, "SDL_SetClipboardText"}
//, {SDL20, SDLXX, "SDL_SetColorKey"}
//, {SDL20, SDLXX, "SDL_SetCursor"}
//, {SDL20, SDLXX, "SDL_SetEventFilter"}
//, {SDL20, SDLXX, "SDL_SetHint"}
//, {SDL20, SDLXX, "SDL_SetHintWithPriority"}
//, {SDL20, SDLXX, "SDL_SetMainReady"}
//, {SDL20, SDLXX, "SDL_SetModState"}
//, {SDL20, SDLXX, "SDL_SetPaletteColors"}
//, {SDL20, SDLXX, "SDL_SetPixelFormatPalette"}
//, {SDL20, SDLXX, "SDL_SetRelativeMouseMode"}
//, {SDL20, SDLXX, "SDL_SetRenderDrawBlendMode"}
//, {SDL20, SDLXX, "SDL_SetRenderDrawColor"}
//, {SDL20, SDLXX, "SDL_SetRenderTarget"}
//, {SDL20, SDLXX, "SDL_SetSurfaceAlphaMod"}
//, {SDL20, SDLXX, "SDL_SetSurfaceBlendMode"}
//, {SDL20, SDLXX, "SDL_SetSurfaceColorMod"}
//, {SDL20, SDLXX, "SDL_SetSurfacePalette"}
//, {SDL20, SDLXX, "SDL_SetSurfaceRLE"}
//, {SDL20, SDLXX, "SDL_SetTextInputRect"}
//, {SDL20, SDLXX, "SDL_SetTextureAlphaMod"}
//, {SDL20, SDLXX, "SDL_SetTextureBlendMode"}
//, {SDL20, SDLXX, "SDL_SetTextureColorMod"}
//, {SDL20, SDLXX, "SDL_SetThreadPriority"}
//, {SDL20, SDLXX, "SDL_SetWindowBordered"}
//, {SDL20, SDLXX, "SDL_SetWindowBrightness"}
//, {SDL20, SDLXX, "SDL_SetWindowData"}
//, {SDL20, SDLXX, "SDL_SetWindowFullscreen"}
//, {SDL20, SDLXX, "SDL_SetWindowGammaRamp"}
//, {SDL20, SDLXX, "SDL_SetWindowGrab"}
//, {SDL20, SDLXX, "SDL_SetWindowIcon"}
//, {SDL20, SDLXX, "SDL_SetWindowMaximumSize"}
//, {SDL20, SDLXX, "SDL_SetWindowMinimumSize"}
//, {SDL20, SDLXX, "SDL_SetWindowPosition"}
//, {SDL20, SDLXX, "SDL_SetWindowShape"}
//, {SDL20, SDLXX, "SDL_SetWindowSize"}
//, {SDL20, SDLXX, "SDL_SetWindowTitle"}
//, {SDL20, SDLXX, "SDL_ShowCursor"}
//, {SDL20, SDLXX, "SDL_ShowMessageBox"}
//, {SDL20, SDLXX, "SDL_ShowSimpleMessageBox"}
//, {SDL20, SDLXX, "SDL_ShowWindow"}
//, {SDL20, SDLXX, "SDL_SoftStretch"}
//, {SDL20, SDLXX, "SDL_StartTextInput"}
//, {SDL20, SDLXX, "SDL_StopTextInput"}
//, {SDL20, SDLXX, "SDL_TLSCreate"}
//, {SDL20, SDLXX, "SDL_TLSGet"}
//, {SDL20, SDLXX, "SDL_TLSSet"}
//, {SDL20, SDLXX, "SDL_ThreadID"}
//, {SDL20, SDLXX, "SDL_TryLockMutex"}
//, {SDL20, SDLXX, "SDL_UnionRect"}
//, {SDL20, SDLXX, "SDL_UnloadObject"}
//, {SDL20, SDLXX, "SDL_UnlockAudio"}
//, {SDL20, SDLXX, "SDL_UnlockAudioDevice"}
//, {SDL20, SDLXX, "SDL_UnlockMutex"}
//, {SDL20, SDLXX, "SDL_UnlockSurface"}
//, {SDL20, SDLXX, "SDL_UnlockTexture"}
//, {SDL20, SDLXX, "SDL_UnregisterApp"}
//, {SDL20, SDLXX, "SDL_UpdateTexture"}
//, {SDL20, SDLXX, "SDL_UpdateWindowSurface"}
//, {SDL20, SDLXX, "SDL_UpdateWindowSurfaceRects"}
//, {SDL20, SDLXX, "SDL_UpdateYUVTexture"}
//, {SDL20, SDLXX, "SDL_UpperBlit"}
//, {SDL20, SDLXX, "SDL_UpperBlitScaled"}
//, {SDL20, SDLXX, "SDL_WaitEvent"}
//, {SDL20, SDLXX, "SDL_WaitEventTimeout"}
//, {SDL20, SDLXX, "SDL_WaitThread"}
//, {SDL20, SDLXX, "SDL_WarpMouseInWindow"}
//, {SDL20, SDLXX, "SDL_WriteBE16"}
//, {SDL20, SDLXX, "SDL_WriteBE32"}
//, {SDL20, SDLXX, "SDL_WriteBE64"}
//, {SDL20, SDLXX, "SDL_WriteLE16"}
//, {SDL20, SDLXX, "SDL_WriteLE32"}
//, {SDL20, SDLXX, "SDL_WriteLE64"}
//, {SDL20, SDLXX, "SDL_WriteU8"}
//, {SDL20, SDLXX, "SDL_abs"}
//, {SDL20, SDLXX, "SDL_acos"}
//, {SDL20, SDLXX, "SDL_asin"}
//, {SDL20, SDLXX, "SDL_atan"}
//, {SDL20, SDLXX, "SDL_atan2"}
//, {SDL20, SDLXX, "SDL_atof"}
//, {SDL20, SDLXX, "SDL_atoi"}
//, {SDL20, SDLXX, "SDL_calloc"}
//, {SDL20, SDLXX, "SDL_ceil"}
//, {SDL20, SDLXX, "SDL_copysign"}
//, {SDL20, SDLXX, "SDL_cos"}
//, {SDL20, SDLXX, "SDL_cosf"}
//, {SDL20, SDLXX, "SDL_fabs"}
//, {SDL20, SDLXX, "SDL_floor"}
//, {SDL20, SDLXX, "SDL_free"}
//, {SDL20, SDLXX, "SDL_getenv"}
//, {SDL20, SDLXX, "SDL_iconv"}
//, {SDL20, SDLXX, "SDL_iconv_close"}
//, {SDL20, SDLXX, "SDL_iconv_open"}
//, {SDL20, SDLXX, "SDL_iconv_string"}
//, {SDL20, SDLXX, "SDL_isdigit"}
//, {SDL20, SDLXX, "SDL_isspace"}
//, {SDL20, SDLXX, "SDL_itoa"}
//, {SDL20, SDLXX, "SDL_lltoa"}
//, {SDL20, SDLXX, "SDL_log"}
//, {SDL20, SDLXX, "SDL_ltoa"}
//, {SDL20, SDLXX, "SDL_malloc"}
//, {SDL20, SDLXX, "SDL_memcmp"}
//, {SDL20, SDLXX, "SDL_memcpy"}
//, {SDL20, SDLXX, "SDL_memmove"}
//, {SDL20, SDLXX, "SDL_memset"}
//, {SDL20, SDLXX, "SDL_pow"}
//, {SDL20, SDLXX, "SDL_qsort"}
//, {SDL20, SDLXX, "SDL_realloc"}
//, {SDL20, SDLXX, "SDL_scalbn"}
//, {SDL20, SDLXX, "SDL_setenv"}
//, {SDL20, SDLXX, "SDL_sin"}
//, {SDL20, SDLXX, "SDL_sinf"}
//, {SDL20, SDLXX, "SDL_snprintf"}
//, {SDL20, SDLXX, "SDL_sqrt"}
//, {SDL20, SDLXX, "SDL_sscanf"}
//, {SDL20, SDLXX, "SDL_strcasecmp"}
//, {SDL20, SDLXX, "SDL_strchr"}
//, {SDL20, SDLXX, "SDL_strcmp"}
//, {SDL20, SDLXX, "SDL_strdup"}
//, {SDL20, SDLXX, "SDL_strlcat"}
//, {SDL20, SDLXX, "SDL_strlcpy"}
//, {SDL20, SDLXX, "SDL_strlen"}
//, {SDL20, SDLXX, "SDL_strlwr"}
//, {SDL20, SDLXX, "SDL_strncasecmp"}
//, {SDL20, SDLXX, "SDL_strncmp"}
//, {SDL20, SDLXX, "SDL_strrchr"}
//, {SDL20, SDLXX, "SDL_strrev"}
//, {SDL20, SDLXX, "SDL_strstr"}
//, {SDL20, SDLXX, "SDL_strtod"}
//, {SDL20, SDLXX, "SDL_strtol"}
//, {SDL20, SDLXX, "SDL_strtoll"}
//, {SDL20, SDLXX, "SDL_strtoul"}
//, {SDL20, SDLXX, "SDL_strtoull"}
//, {SDL20, SDLXX, "SDL_strupr"}
//, {SDL20, SDLXX, "SDL_tolower"}
//, {SDL20, SDLXX, "SDL_toupper"}
//, {SDL20, SDLXX, "SDL_uitoa"}
//, {SDL20, SDLXX, "SDL_ulltoa"}
//, {SDL20, SDLXX, "SDL_ultoa"}
//, {SDL20, SDLXX, "SDL_utf8strlcpy"}
//, {SDL20, SDLXX, "SDL_vsnprintf"}
//, {SDL20, SDLXX, "SDL_vsscanf"}
//, {SDL20, SDLXX, "SDL_wcslcat"}
//, {SDL20, SDLXX, "SDL_wcslcpy"}
//, {SDL20, SDLXX, "SDL_wcslen"}
  };

/* Force section .data instead of default .bss, because with .bss something  */
/* wrong happens during linking in ghci/runghc (at least on FreeBSD).        */
void* __attribute__((section(".data"))) ADRTAB[TABLEN] = {};



/* ------------------------------------------------------------------------- */
#undef hsc_FPID
#define hsc_FPID(fn)                            \
  { int i; for (i = 0; i < TABLEN; i++) {       \
      if (strcmp((#fn)+3, (SYMTAB[i].name)+4) == 0) {   \
        printf("("); hsc_const(i); printf(")"); \
        break;                                  \
      };                                        \
    };                                          \
  }

