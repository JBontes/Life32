unit DirectX;

interface

{$Z4}
{$A+}
{$WEAKPACKAGEUNIT}

uses
  Windows, MMSystem;

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       ddraw.h
 *  Content:    DirectDraw include file
 *
 ***************************************************************************)

{ GUIDS used by DirectDraw objects }

const
  CLSID_DirectDraw: TGUID = (D1:$D7B70EE0;D2:$4340;D3:$11CF;D4:($B0,$63,$00,$20,$AF,$C2,$CD,$35));
  CLSID_DirectDrawClipper: TGUID = (D1:$593817A0;D2:$7DB3;D3:$11CF;D4:($A2,$DE,$00,$AA,$00,$b9,$33,$56));

  IID_IDirectDraw: TGUID = (D1:$6C14DB80;D2:$A733;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectDraw2: TGUID = (D1:$B3A6F3E0;D2:$2B43;D3:$11CF;D4:($A2,$DE,$00,$AA,$00,$B9,$33,$56));

  IID_IDirectDrawSurface: TGUID = (D1:$6C14DB81;D2:$A733;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectDrawSurface2: TGUID = (D1:$57805885;D2:$6eec;D3:$11cf;D4:($94,$41,$a8,$23,$03,$c1,$0e,$27));
  IID_IDirectDrawSurface3: TGUID = (D1:$DA044E00;D2:$69B2;D3:$11D0;D4:($A1,$D5,$00,$AA,$00,$B8,$DF,$BB));

  IID_IDirectDrawPalette: TGUID = (D1:$6C14DB84;D2:$A733;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectDrawClipper: TGUID = (D1:$6C14DB85;D2:$A733;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectDrawColorControl: TGUID = (D1:$4B9F0EE0;D2:$0D7E;D3:$11D0;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));

const
  DD_ROP_SPACE = (256 div 32 );       // space required to store ROP array

{ DirectDraw Structures }

type
  LPDDENUMCALLBACKA = function(lpGUID: PGUID; lpDriverDescription: LPSTR;
      lpDriverName: LPSTR; lpContext: Pointer): BOOL; stdcall;
  LPDDENUMCALLBACKW = function(lpGUID: PGUID; lpDriverDescription: LPWSTR;
      lpDriverName: LPWSTR; lpContext: Pointer): BOOL; stdcall;

  LPDDENUMCALLBACK = LPDDENUMCALLBACKA;

type
  IDirectDraw = interface;
  IDirectDraw2 = interface;
  IDirectDrawSurface = interface;
  IDirectDrawSurface2 = interface;
  IDirectDrawSurface3 = interface;

  IDirectDrawPalette = interface;
  IDirectDrawClipper = interface;
  IDirectDrawColorControl = interface;

{ DDCOLORKEY structure }

  TDDCOLORKEY = record
    dwColorSpaceLowValue: DWORD;   // low boundary of color space that is to
                                   //  be treated as Color Key, inclusive
    dwColorSpaceHighValue: DWORD;  // high boundary of color space that is
                                   //  to be treated as Color Key, inclusive
  end;
  LPDDCOLORKEY = ^TDDCOLORKEY;

{ DDBLTFX structure }

  TDDBLTFX = record
    dwSize: DWORD;                           // size of structure
    dwDDFX: DWORD;                           // FX operations
    dwROP: DWORD;                            // Win32 raster operations
    dwDDROP: DWORD;                          // Raster operations new for DirectDraw
    dwRotationAngle: DWORD;                  // Rotation angle for blt
    dwZBufferOpCode: DWORD;                  // ZBuffer compares
    dwZBufferLow: DWORD;                     // Low limit of Z buffer
    dwZBufferHigh: DWORD;                    // High limit of Z buffer
    dwZBufferBaseDest: DWORD;                // Destination base value
    dwZDestConstBitDepth: DWORD;             // Bit depth used to specify Z constant for destination
    case Integer of
    0: (
      dwZDestConst: DWORD;                   // Constant to use as Z buffer for dest
      dwZSrcConstBitDepth: DWORD;            // Bit depth used to specify Z constant for source
      dwZSrcConst: DWORD;                    // Constant to use as Z buffer for src
      dwAlphaEdgeBlendBitDepth: DWORD;       // Bit depth used to specify constant for alpha edge blend
      dwAlphaEdgeBlend: DWORD;               // Alpha for edge blending
      dwReserved: DWORD;
      dwAlphaDestConstBitDepth: DWORD;       // Bit depth used to specify alpha constant for destination
      dwAlphaDestConst: DWORD;               // Constant to use as Alpha Channel
      dwAlphaSrcConstBitDepth: DWORD;        // Bit depth used to specify alpha constant for source
      dwAlphaSrcConst: DWORD;                // Constant to use as Alpha Channel
      dwFillColor: DWORD;                    // color in RGB or Palettized
      ddckDestColorkey: TDDColorKey;          // DestColorkey override
      ddckSrcColorkey: TDDColorKey;           // SrcColorkey override
      );
    1: (
      lpDDSZBufferDest: IDirectDrawSurface;  // Surface to use as Z buffer for dest
      UNIONFILLER1b: DWORD;
      lpDDSZBufferSrc: IDirectDrawSurface;   // Surface to use as Z buffer for src
      UNIONFILLER1d: DWORD;
      UNIONFILLER1e: DWORD;
      UNIONFILLER1f: DWORD;
      UNIONFILLER1g: DWORD;
      lpDDSAlphaDest: IDirectDrawSurface;    // Surface to use as Alpha Channel
      UNIONFILLER1i: DWORD;
      lpDDSAlphaSrc: IDirectDrawSurface;     // Surface to use as Alpha Channel
      dwFillDepth: DWORD;                    // depth value for z-buffer
      );
    2: (
      UNIONFILLER2a: DWORD;
      UNIONFILLER2b: DWORD;
      UNIONFILLER2c: DWORD;
      UNIONFILLER2d: DWORD;
      UNIONFILLER2e: DWORD;
      UNIONFILLER2f: DWORD;
      UNIONFILLER2g: DWORD;
      UNIONFILLER2h: DWORD;
      UNIONFILLER2i: DWORD;
      UNIONFILLER2j: DWORD;
      lpDDSPattern: IDirectDrawSurface;       // Surface to use as pattern
      );
  end;
  PDDBLTFX = ^TDDBLTFX;

{ DDSCAPS structure }

  TDDSCaps = record
    dwCaps: DWORD;         // capabilities of surface wanted
  end;
  LPDDSCAPS = ^TDDSCaps;

{ DDCAPS structure }

  TDDCAPS = record
    dwSize: DWORD;                 // size of the DDDRIVERCAPS structure
    dwCaps: DWORD;                 // driver specific capabilities
    dwCaps2: DWORD;                // more driver specific capabilites
    dwCKeyCaps: DWORD;             // color key capabilities of the surface
    dwFXCaps: DWORD;               // driver specific stretching and effects capabilites
    dwFXAlphaCaps: DWORD;          // alpha driver specific capabilities
    dwPalCaps: DWORD;              // palette capabilities
    dwSVCaps: DWORD;               // stereo vision capabilities
    dwAlphaBltConstBitDepths: DWORD;       // DDBD_2,4,8
    dwAlphaBltPixelBitDepths: DWORD;       // DDBD_1,2,4,8
    dwAlphaBltSurfaceBitDepths: DWORD;     // DDBD_1,2,4,8
    dwAlphaOverlayConstBitDepths: DWORD;   // DDBD_2,4,8
    dwAlphaOverlayPixelBitDepths: DWORD;   // DDBD_1,2,4,8
    dwAlphaOverlaySurfaceBitDepths: DWORD; // DDBD_1,2,4,8
    dwZBufferBitDepths: DWORD;             // DDBD_8,16,24,32
    dwVidMemTotal: DWORD;          // total amount of video memory
    dwVidMemFree: DWORD;           // amount of free video memory
    dwMaxVisibleOverlays: DWORD;   // maximum number of visible overlays
    dwCurrVisibleOverlays: DWORD;  // current number of visible overlays
    dwNumFourCCCodes: DWORD;       // number of four cc codes
    dwAlignBoundarySrc: DWORD;     // source rectangle alignment
    dwAlignSizeSrc: DWORD;         // source rectangle byte size
    dwAlignBoundaryDest: DWORD;    // dest rectangle alignment
    dwAlignSizeDest: DWORD;        // dest rectangle byte size
    dwAlignStrideAlign: DWORD;     // stride alignment
    dwRops: Array [ 0..DD_ROP_SPACE-1 ] of DWORD;   // ROPS supported
    TDDSCaps: TDDSCaps;              // TDDSCaps structure has all the general capabilities
    dwMinOverlayStretch: DWORD;    // minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxOverlayStretch: DWORD;    // maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinLiveVideoStretch: DWORD;  // minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxLiveVideoStretch: DWORD;  // maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinHwCodecStretch: DWORD;    // minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxHwCodecStretch: DWORD;    // maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwReserved1: DWORD;            // reserved
    dwReserved2: DWORD;            // reserved
    dwReserved3: DWORD;            // reserved
    dwSVBCaps: DWORD;              // driver specific capabilities for System->Vmem blts
    dwSVBCKeyCaps: DWORD;          // driver color key capabilities for System->Vmem blts
    dwSVBFXCaps: DWORD;            // driver FX capabilities for System->Vmem blts
    dwSVBRops: Array [ 0..DD_ROP_SPACE-1 ] of DWORD;// ROPS supported for System->Vmem blts
    dwVSBCaps: DWORD;              // driver specific capabilities for Vmem->System blts
    dwVSBCKeyCaps: DWORD;          // driver color key capabilities for Vmem->System blts
    dwVSBFXCaps: DWORD;            // driver FX capabilities for Vmem->System blts
    dwVSBRops: Array [ 0..DD_ROP_SPACE-1 ] of DWORD;// ROPS supported for Vmem->System blts
    dwSSBCaps: DWORD;              // driver specific capabilities for System->System blts
    dwSSBCKeyCaps: DWORD;          // driver color key capabilities for System->System blts
    dwSSBFXCaps: DWORD;            // driver FX capabilities for System->System blts
    dwSSBRops: Array [ 0..DD_ROP_SPACE-1 ] of DWORD;// ROPS supported for System->System blts
    dwMaxVideoPorts: DWORD;        // maximum number of usable video ports
    dwCurrVideoPorts: DWORD;       // current number of video ports used
    dwSVBCaps2: DWORD;             // more driver specific capabilities for System->Vmem blts
    dwNLVBCaps: DWORD;             // driver specific capabilities for non-local->local vidmem blts
    dwNLVBCaps2: DWORD;            // more driver specific capabilities non-local->local vidmem blts
    dwNLVBCKeyCaps: DWORD;         // driver color key capabilities for non-local->local vidmem blts
    dwNLVBFXCaps: DWORD;           // driver FX capabilities for non-local->local blts
    dwNLVBRops: Array [ 0..DD_ROP_SPACE-1 ] of DWORD; // ROPS supported for non-local->local blts
  end;
  LPDDCAPS = ^TDDCAPS;

{ DDPIXELFORMAT structure }

  DDPIXELFORMAT = record
    dwSize: DWORD;                // size of structure
    dwFlags: DWORD;               // pixel format flags
    dwFourCC: DWORD;              // (FOURCC code)
    case Integer of
    0: (
      dwRGBBitCount: DWORD;       // how many bits per pixel
      dwRBitMask: DWORD;          // mask for red bit
      dwGBitMask: DWORD;          // mask for green bits
      dwBBitMask: DWORD;          // mask for blue bits
      dwRGBAlphaBitMask: DWORD;   // mask for alpha channel
      );
    1: (
      dwYUVBitCount: DWORD;       // how many bits per pixel
      dwYBitMask: DWORD;          // mask for Y bits
      dwUBitMask: DWORD;          // mask for U bits
      dwVBitMask: DWORD;          // mask for V bits
      dwYUVAlphaBitMask: DWORD;   // mask for alpha channel
      );
    2: (
      dwZBufferBitDepth: DWORD;   // how many bits for z buffers
      );
    3: (
      dwAlphaBitDepth: DWORD;     // how many bits for alpha channels
      );
  end;
  LPDDPIXELFORMAT = ^DDPIXELFORMAT;

{ DDOVERLAYFX structure }

  DDOVERLAYFX = record
    dwSize: DWORD;                         // size of structure
    dwAlphaEdgeBlendBitDepth: DWORD;       // Bit depth used to specify constant for alpha edge blend
    dwAlphaEdgeBlend: DWORD;               // Constant to use as alpha for edge blend
    dwReserved: DWORD;
    dwAlphaDestConstBitDepth: DWORD;       // Bit depth used to specify alpha constant for destination
    case Integer of
    0: (
      dwAlphaDestConst: DWORD;             // Constant to use as alpha channel for dest
      dwAlphaSrcConstBitDepth: DWORD;      // Bit depth used to specify alpha constant for source
      dwAlphaSrcConst: DWORD;              // Constant to use as alpha channel for src
      dckDestColorkey: TDDColorKey;         // DestColorkey override
      dckSrcColorkey: TDDColorKey;          // DestColorkey override
      dwDDFX: DWORD;                       // Overlay FX
      dwFlags: DWORD;                      // flags
      );
    1: (
      lpDDSAlphaDest: IDirectDrawSurface;  // Surface to use as alpha channel for dest
      UNIONFILLER1b: DWORD;
      lpDDSAlphaSrc: IDirectDrawSurface;   // Surface to use as alpha channel for src
      );
  end;
  LPDDOVERLAYFX = ^DDOVERLAYFX;

{ DDBLTBATCH structure }

  TDDBltBatch = record
    lprDest: ^TRect;
    lpDDSSrc: IDirectDrawSurface;
    lprSrc: ^TRect;
    dwFlags: DWORD;
    lpDDBltFx: PDDBLTFX;
  end;
  LPDDBLTBATCH = ^TDDBltBatch;

{ DDSURFACEDESC structure }

  TDDSurfaceDesc = record
    dwSize: DWORD;                 // size of the DDSURFACEDESC structure
    dwFlags: DWORD;                // determines what fields are valid
    dwHeight: DWORD;               // height of surface to be created
    dwWidth: DWORD;                // width of input surface
    lPitch: Longint;                 // distance to start of next line (return value only)
    dwBackBufferCount: DWORD;      // number of back buffers requested
    case Integer of
    0: (
      dwMipMapCount: DWORD;          // number of mip-map levels requested
      dwAlphaBitDepth: DWORD;        // depth of alpha buffer requested
      dwReserved: DWORD;             // reserved
      lpSurface: Pointer;              // pointer to the associated surface memory
      ddckCKDestOverlay: TDDColorKey;      // color key for destination overlay use
      ddckCKDestBlt: TDDColorKey;          // color key for destination blt use
      ddckCKSrcOverlay: TDDColorKey;       // color key for source overlay use
      ddckCKSrcBlt: TDDColorKey;           // color key for source blt use
      ddpfPixelFormat: DDPIXELFORMAT;        // pixel format description of the surface
      DDSCaps: TDDSCaps;                // direct draw surface capabilities
      );
    1: (
      dwZBufferBitDepth: DWORD;      // depth of Z buffer requested
      );
    2: (
      dwRefreshRate: DWORD;          // refresh rate (used when display mode is described)
      );
  end;
  LPDDSURFACEDESC = ^TDDSurfaceDesc;

  DDCOLORCONTROL = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lBrightness: Longint;
    lContrast: Longint;
    lHue: Longint;
    lSaturation: Longint;
    lSharpness: Longint;
    lGamma: Longint;
    lColorEnable: Longint;
    dwReserved1: DWORD;
  end;
  LPDDCOLORCONTROL = ^DDCOLORCONTROL;

  LPCLIPPERCALLBACK = function(lpDDClipper: IDirectDrawClipper; hWnd: HWND;
    code: DWORD; lpContext: Pointer): HRESULT; stdcall;

  LPSURFACESTREAMINGCALLBACK = function(Arg: DWORD): HRESULT; stdcall;

  LPDDENUMMODESCALLBACK = function(const lpDDSurfaceDesc: TDDSURFACEDESC;
      lpContext: Pointer ): HRESULT; stdcall;
  LPDDENUMSURFACESCALLBACK = function(lpDDSurface: IDirectDrawSurface;
      const lpDDSurfaceDesc: TDDSURFACEDESC; lpContext: Pointer ): HRESULT;
      stdcall;

{ IDirectDraw Interface }

  IDirectDraw = interface(IUnknown)
    ['{6C14DB80-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDraw methods ***)
    function Compact: HRESULT; stdcall;
    function CreateClipper(dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper; pUnkOuter: IUnknown ): HRESULT;
        stdcall;
    function CreatePalette(dwFlags: DWORD; lpColorTable: PPaletteEntry;
        out lplpDDPalette: IDirectDrawPalette; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function CreateSurface(const lpDDSurfaceDesc: TDDSurfaceDesc;
        out lplpDDSurface: IDirectDrawSurface; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface;
        out lplpDupDDSurface: IDirectDrawSurface): HRESULT; stdcall;
    function EnumDisplayModes(dwFlags: DWORD;
        const lpDDSurfaceDesc: TDDSurfaceDesc; lpContext: Pointer;
        lpEnumModesCallback: LPDDENUMMODESCALLBACK): HRESULT; stdcall;
    function EnumSurfaces(dwFlags: DWORD; const lpDDSD: TDDSurfaceDesc;
        lpContext: Pointer; lpEnumCallback: LPDDENUMSURFACESCALLBACK): HRESULT;
        stdcall;
    function FlipToGDISurface: HRESULT; stdcall;
    function GetCaps(var lpDDDriverCaps: TDDCAPS; var lpDDHELCaps: TDDCAPS):
        HRESULT; stdcall;
    function GetDisplayMode(var lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT;
        stdcall;
    function GetFourCCCodes(var lpNumCodes, lpCodes: DWORD): HRESULT; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface): HRESULT;
        stdcall;
    function GetMonitorFrequency(var lpdwFrequency: DWORD): HRESULT; stdcall;
    function GetScanLine(var lpdwScanLine: DWORD): HRESULT; stdcall;
    function GetVerticalBlankStatus(var lpbIsInVB: BOOL): HRESULT; stdcall;
    function Initialize(lpGUID: PGUID): HRESULT; stdcall;
    function RestoreDisplayMode: HRESULT; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBpp: DWORD): HRESULT; stdcall;
    function WaitForVerticalBlank(dwFlags: DWORD; hEvent: THandle): HRESULT;
        stdcall;
  end;

{ IDirectDraw2 Interface }

  IDirectDraw2 = interface(IUnknown)
    ['{B3A6F3E0-2B43-11CF-A2DE-00AA00B93356}']
    (*** IDirectDraw methods ***)
    function Compact: HRESULT; stdcall;
    function CreateClipper(dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function CreatePalette(dwFlags: DWORD; lpColorTable: PPaletteEntry;
        out lplpDDPalette: IDirectDrawPalette; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function CreateSurface(const lpDDSurfaceDesc: TDDSurfaceDesc;
        out lplpDDSurface: IDirectDrawSurface; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface;
        out lplpDupDDSurface: IDirectDrawSurface): HRESULT; stdcall;
    function EnumDisplayModes(dwFlags: DWORD;
        const lpDDSurfaceDesc: TDDSurfaceDesc; lpContext: Pointer;
        lpEnumModesCallback: LPDDENUMMODESCALLBACK): HRESULT; stdcall;
    function EnumSurfaces(dwFlags: DWORD; const lpDDSD: TDDSurfaceDesc;
        lpContext: Pointer; lpEnumCallback: LPDDENUMSURFACESCALLBACK): HRESULT;
        stdcall;
    function FlipToGDISurface: HRESULT; stdcall;
    function GetCaps(var lpDDDriverCaps: TDDCAPS; var lpDDHELCaps: TDDCAPS):
        HRESULT; stdcall;
    function GetDisplayMode(var lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT;
        stdcall;
    function GetFourCCCodes(var lpNumCodes, lpCodes: DWORD): HRESULT; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface): HRESULT;
        stdcall;
    function GetMonitorFrequency(var lpdwFrequency: DWORD): HRESULT; stdcall;
    function GetScanLine(var lpdwScanLine: DWORD): HRESULT; stdcall;
    function GetVerticalBlankStatus(var lpbIsInVB: BOOL): HRESULT; stdcall;
    function Initialize(lpGUID: PGUID): HRESULT; stdcall;
    function RestoreDisplayMode: HRESULT; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBPP, dwRefreshRate: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function WaitForVerticalBlank(dwFlags: DWORD; hEvent: THandle): HRESULT;
        stdcall;
    (*** IDirectDraw2 methods ***)
    function GetAvailableVidMem(var lpDDSCaps: TDDSCaps;
        var lpdwTotal, lpdwFree: DWORD): HRESULT; stdcall;
  end;

{ IDirectDrawPalette Interface }

  IDirectDrawPalette = interface(IUnknown)
    ['{6C14DB84-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawPalette methods ***)
    function GetCaps(varlpdwCaps: DWORD): HRESULT; stdcall;
    function GetEntries(dwFlags: DWORD; dwBase: DWORD; dwNumEntries: DWORD;
        var lpEntries): HRESULT; stdcall; // csc
    function Initialize(lpDD: IDirectDraw; dwFlags: DWORD;
        var lpDDColorTable): HRESULT; stdcall; // csc
    function SetEntries(dwFlags: DWORD; dwStartingEntry: DWORD;
        dwCount: DWORD; var lpEntries): HRESULT; stdcall; // csc
  end;

{ IDirectDrawClipper Interface }

  IDirectDrawClipper = interface(IUnknown)
    ['{6C14DB85-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawClipper methods ***)
    function GetClipList(const lpRect: TRect; lpClipList: PRgnData;
        var lpdwSize: DWORD): HRESULT; stdcall;
    function GetHWnd(var lphWnd: HWND): HRESULT; stdcall;
    function Initialize(lpDD: IDirectDraw; dwFlags: DWORD): HRESULT; stdcall;
    function IsClipListChanged(var lpbChanged: BOOL): HRESULT; stdcall;
    function SetClipList(lpClipList: PRgnData; dwFlags: DWORD): HRESULT;
        stdcall;
    function SetHWnd(dwFlags: DWORD; hWnd: HWND): HRESULT; stdcall;
  end;

{ IDirectDrawSurface Interface }

  IDirectDrawSurface = interface(IUnknown)
    ['{6C14DB81-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface):
        HRESULT; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HRESULT; stdcall;
    function Blt(const lpDestRect: TRect; lpDDSrcSurface: IDirectDrawSurface;
        lpSrcRect: PRect; dwFlags: DWORD; lpDDBltFx: PDDBLTFX): // csc changed from const to pointer on last param
        HRESULT; stdcall;
    function BltBatch(const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function BltFast(dwX, dwY: DWORD; lpDDSrcSurface: IDirectDrawSurface;
        const lpSrcRect: TRect; dwTrans: DWORD): HRESULT; stdcall;
    function DeleteAttachedSurface(dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface): HRESULT; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
        lpEnumSurfacesCallback: LPDDENUMSURFACESCALLBACK): HRESULT; stdcall;
    function EnumOverlayZOrders(dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: LPDDENUMSURFACESCALLBACK): HRESULT; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetAttachedSurface(var lpDDSCaps: TDDSCaps;
        out lplpDDAttachedSurface: IDirectDrawSurface): HRESULT; stdcall;
    function GetBltStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetCaps(var lpDDSCaps: TDDSCaps): HRESULT; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HRESULT;
        stdcall;
    function GetColorKey(dwFlags: DWORD; var lpDDColorKey: TDDColorKey): HRESULT;
        stdcall;
    function GetDC(var lphDC: HDC): HRESULT; stdcall;
    function GetFlipStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetOverlayPosition(var lplX, lplY: Longint): HRESULT; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HRESULT;
        stdcall;
    function GetPixelFormat(var lpDDPixelFormat: DDPIXELFORMAT): HRESULT;
        stdcall;
    function GetSurfaceDesc(var lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT;
        stdcall;
    function Initialize(lpDD: IDirectDraw;
        const lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT; stdcall;
    function IsLost: HRESULT; stdcall;
    function Lock(lpDestRect: PRect; var lpDDSurfaceDesc: TDDSurfaceDesc;
        dwFlags: DWORD; hEvent: THandle): HRESULT; stdcall;
    function ReleaseDC(hDC: HDC): HRESULT; stdcall;
    function Restore: HRESULT; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HRESULT; stdcall;
    function SetColorKey(dwFlags: DWORD; const lpDDColorKey: TDDColorKey):
        HRESULT; stdcall;
    function SetOverlayPosition(lX, lY: Longint): HRESULT; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HRESULT; stdcall;
    function Unlock(lpSurfaceData: Pointer): HRESULT; stdcall;
    function UpdateOverlay(const lpSrcRect: TRect;
        lpDDDestSurface: IDirectDrawSurface; const lpDestRect: TRect;
        dwFlags: DWORD; const lpDDOverlayFx: DDOVERLAYFX ): HRESULT; stdcall;
    function UpdateOverlayDisplay(dwFlags: DWORD): HRESULT; stdcall;
    function UpdateOverlayZOrder(dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface): HRESULT; stdcall;
  end;

{ IDirectDrawSurface2 Interface }

  IDirectDrawSurface2 = interface(IUnknown)
    ['{57805885-6EEC-11CF-9441-A82303C10E27}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface2):
        HRESULT; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HRESULT; stdcall;
    function Blt(const lpDestRect: TRect; lpDDSrcSurface: IDirectDrawSurface2;
        const lpSrcRect: TRect; dwFlags: DWORD; lpDDBltFx: PDDBLTFX):
        HRESULT; stdcall;
    function BltBatch(const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function BltFast(dwX, dwY: DWORD; lpDDSrcSurface: IDirectDrawSurface2;
        const lpSrcRect: TRect; dwTrans: DWORD): HRESULT; stdcall;
    function DeleteAttachedSurface(dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface2): HRESULT; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
        lpEnumSurfacesCallback: LPDDENUMSURFACESCALLBACK): HRESULT; stdcall;
    function EnumOverlayZOrders(dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: LPDDENUMSURFACESCALLBACK): HRESULT; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface2;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetAttachedSurface(var lpDDSCaps: TDDSCaps;
        out lplpDDAttachedSurface: IDirectDrawSurface2): HRESULT;
        stdcall;
    function GetBltStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetCaps(var lpDDSCaps: TDDSCaps): HRESULT; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HRESULT;
        stdcall;
    function GetColorKey(dwFlags: DWORD; var lpDDColorKey: TDDColorKey):
        HRESULT; stdcall;
    function GetDC(var lphDC: HDC): HRESULT; stdcall;
    function GetFlipStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetOverlayPosition(var lplX, lplY: Longint): HRESULT; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HRESULT;
        stdcall;
    function GetPixelFormat(var lpDDPixelFormat: DDPIXELFORMAT): HRESULT;
        stdcall;
    function GetSurfaceDesc(var lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT;
        stdcall;
    function Initialize(lpDD: IDirectDraw;
        const lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT; stdcall;
    function IsLost: HRESULT; stdcall;
    function Lock(lpDestRect: PRect; const lpDDSurfaceDesc: TDDSurfaceDesc;
        dwFlags: DWORD; hEvent: THandle): HRESULT; stdcall;
    function ReleaseDC(hDC: HDC): HRESULT; stdcall;
    function Restore: HRESULT; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HRESULT; stdcall;
    function SetColorKey(dwFlags: DWORD; const lpDDColorKey: TDDColorKey):
        HRESULT; stdcall;
    function SetOverlayPosition(lX, lY: Longint): HRESULT; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HRESULT; stdcall;
    function Unlock(lpSurfaceData: Pointer): HRESULT; stdcall;
    function UpdateOverlay(const lpSrcRect: TRect;
        lpDDDestSurface: IDirectDrawSurface2; const lpDestRect: TRect;
        dwFlags: DWORD; const lpDDOverlayFx: DDOVERLAYFX): HRESULT; stdcall;
    function UpdateOverlayDisplay(dwFlags: DWORD): HRESULT; stdcall;
    function UpdateOverlayZOrder(dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface2): HRESULT; stdcall;
    (*** IDirectDrawSurface2 methods ***)
    function GetDDInterface(out lplpDD: IDirectDraw): HRESULT; stdcall;
    function PageLock(dwFlags: DWORD): HRESULT; stdcall;
    function PageUnlock(dwFlags: DWORD): HRESULT; stdcall;
  end;

{ IDirectDrawSurface3 Interface }

  IDirectDrawSurface3 = interface(IUnknown)
    ['{DA044E00-69B2-11D0-A1D5-00AA00B8DFBB}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface(lpDDSAttachedSurface: IDirectDrawSurface3):
        HRESULT; stdcall;
    function AddOverlayDirtyRect(const lpRect: TRect): HRESULT; stdcall;
    function Blt(const lpDestRect: TRect; lpDDSrcSurface: IDirectDrawSurface3;
        const lpSrcRect: TRect; dwFlags: DWORD; lpDDBltFx: PDDBltFx):
        HRESULT; stdcall;
    function BltBatch(const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function BltFast(dwX, dwY: DWORD; lpDDSrcSurface: IDirectDrawSurface3;
        const lpSrcRect: TRect; dwTrans: DWORD): HRESULT; stdcall;
    function DeleteAttachedSurface(dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface3): HRESULT; stdcall;
    function EnumAttachedSurfaces(lpContext: Pointer;
        lpEnumSurfacesCallback: LPDDENUMSURFACESCALLBACK): HRESULT; stdcall;
    function EnumOverlayZOrders(dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: LPDDENUMSURFACESCALLBACK): HRESULT; stdcall;
    function Flip(lpDDSurfaceTargetOverride: IDirectDrawSurface3;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetAttachedSurface(var lpDDSCaps: TDDSCaps;
        out lplpDDAttachedSurface: IDirectDrawSurface3): HRESULT;
        stdcall;
    function GetBltStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetCaps(var lpDDSCaps: TDDSCaps): HRESULT; stdcall;
    function GetClipper(out lplpDDClipper: IDirectDrawClipper): HRESULT;
        stdcall;
    function GetColorKey(dwFlags: DWORD; var lpDDColorKey: TDDColorKey): HRESULT;
        stdcall;
    function GetDC(var lphDC: HDC): HRESULT; stdcall;
    function GetFlipStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetOverlayPosition(var lplX, lplY: Longint): HRESULT; stdcall;
    function GetPalette(out lplpDDPalette: IDirectDrawPalette): HRESULT;
        stdcall;
    function GetPixelFormat(var lpDDPixelFormat: DDPIXELFORMAT): HRESULT;
        stdcall;
    function GetSurfaceDesc(var lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT;
        stdcall;
    function Initialize(lpDD: IDirectDraw;
        const lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT; stdcall;
    function IsLost: HRESULT; stdcall;
    function Lock(lpDestRect: PRect; const lpDDSurfaceDesc: TDDSurfaceDesc;
        dwFlags: DWORD; hEvent: THandle): HRESULT; stdcall;
    function ReleaseDC(hDC: HDC): HRESULT; stdcall;
    function Restore: HRESULT; stdcall;
    function SetClipper(lpDDClipper: IDirectDrawClipper): HRESULT; stdcall;
    function SetColorKey(dwFlags: DWORD; const lpDDColorKey: TDDColorKey):
        HRESULT; stdcall;
    function SetOverlayPosition(lX, lY: Longint): HRESULT; stdcall;
    function SetPalette(lpDDPalette: IDirectDrawPalette): HRESULT; stdcall;
    function Unlock(lpSurfaceData: Pointer): HRESULT; stdcall;
    function UpdateOverlay(const lpSrcRect: TRect;
        lpDDDestSurface: IDirectDrawSurface3; const lpDestRect: TRect;
        dwFlags: DWORD; const lpDDOverlayFx: DDOVERLAYFX): HRESULT; stdcall;
    function UpdateOverlayDisplay(dwFlags: DWORD): HRESULT; stdcall;
    function UpdateOverlayZOrder(dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface3): HRESULT; stdcall;
    (*** IDirectDrawSurface2 methods ***)
    function GetDDInterface(out lplpDD: IDirectDraw): HRESULT; stdcall;
    function PageLock(dwFlags: DWORD): HRESULT; stdcall;
    function PageUnlock(dwFlags: DWORD): HRESULT; stdcall;
    (*** IDirectDrawSurface3 methods ***)
    function SetSurfaceDesc(const lpddsd: TDDSurfaceDesc; dwFlags: DWORD): HRESULT;
        stdcall;
  end;

{ IDirectDrawColorControl Interface }

  IDirectDrawColorControl = interface(IUnknown)
    ['{4B9F0EE0-0D7E-11D0-9B06-00A0C903A3B8}']
    (*** IDirectDrawColorControl methods ***)
    function GetColorControls(var lpColorControl: DDCOLORCONTROL): HRESULT;
        stdcall;
    function SetColorControls(const lpColorControl: DDCOLORCONTROL): HRESULT;
        stdcall;
  end;

const
{ TDDSCaps field is valid. }
  DDSD_CAPS               = $00000001;     // default
  DDSD_HEIGHT             = $00000002;
  DDSD_WIDTH              = $00000004;
  DDSD_PITCH              = $00000008;
  DDSD_BACKBUFFERCOUNT    = $00000020;
  DDSD_ZBUFFERBITDEPTH    = $00000040;
  DDSD_ALPHABITDEPTH      = $00000080;
  DDSD_PIXELFORMAT        = $00001000;
  DDSD_CKDESTOVERLAY      = $00002000;
  DDSD_CKDESTBLT          = $00004000;
  DDSD_CKSRCOVERLAY       = $00008000;
  DDSD_CKSRCBLT           = $00010000;
  DDSD_MIPMAPCOUNT        = $00020000;
  DDSD_REFRESHRATE        = $00040000;
  DDSD_ALL                = $0007f9ee;

  DDCOLOR_BRIGHTNESS    = $00000001;
  DDCOLOR_CONTRAST      = $00000002;
  DDCOLOR_HUE           = $00000004;
  DDCOLOR_SATURATION    = $00000008;
  DDCOLOR_SHARPNESS     = $00000010;
  DDCOLOR_GAMMA         = $00000020;
  DDCOLOR_COLORENABLE   = $00000040;

{ DirectDrawSurface Capability Flags }

  DDSCAPS_3D                  = $00000001;
  DDSCAPS_ALPHA               = $00000002;
  DDSCAPS_BACKBUFFER          = $00000004;
  DDSCAPS_COMPLEX             = $00000008;
  DDSCAPS_FLIP                = $00000010;
  DDSCAPS_FRONTBUFFER         = $00000020;
  DDSCAPS_OFFSCREENPLAIN      = $00000040;
  DDSCAPS_OVERLAY             = $00000080;
  DDSCAPS_PALETTE             = $00000100;
  DDSCAPS_PRIMARYSURFACE      = $00000200;
  DDSCAPS_PRIMARYSURFACELEFT  = $00000400;
  DDSCAPS_SYSTEMMEMORY        = $00000800;
  DDSCAPS_TEXTURE             = $00001000;
  DDSCAPS_3DDEVICE            = $00002000;
  DDSCAPS_VIDEOMEMORY         = $00004000;
  DDSCAPS_VISIBLE             = $00008000;
  DDSCAPS_WRITEONLY           = $00010000;
  DDSCAPS_ZBUFFER             = $00020000;
  DDSCAPS_OWNDC               = $00040000;
  DDSCAPS_LIVEVIDEO           = $00080000;
  DDSCAPS_HWCODEC             = $00100000;
  DDSCAPS_MODEX               = $00200000;
  DDSCAPS_MIPMAP              = $00400000;
  DDSCAPS_ALLOCONLOAD         = $04000000;

{ DirectDraw Driver Capability Flags }

  DDCAPS_3D                   = $00000001;
  DDCAPS_ALIGNBOUNDARYDEST    = $00000002;
  DDCAPS_ALIGNSIZEDEST        = $00000004;
  DDCAPS_ALIGNBOUNDARYSRC     = $00000008;
  DDCAPS_ALIGNSIZESRC         = $00000010;
  DDCAPS_ALIGNSTRIDE          = $00000020;
  DDCAPS_BLT                  = $00000040;
  DDCAPS_BLTQUEUE             = $00000080;
  DDCAPS_BLTFOURCC            = $00000100;
  DDCAPS_BLTSTRETCH           = $00000200;
  DDCAPS_GDI                  = $00000400;
  DDCAPS_OVERLAY              = $00000800;
  DDCAPS_OVERLAYCANTCLIP      = $00001000;
  DDCAPS_OVERLAYFOURCC        = $00002000;
  DDCAPS_OVERLAYSTRETCH       = $00004000;
  DDCAPS_PALETTE              = $00008000;
  DDCAPS_PALETTEVSYNC         = $00010000;
  DDCAPS_READSCANLINE         = $00020000;
  DDCAPS_STEREOVIEW           = $00040000;
  DDCAPS_VBI                  = $00080000;
  DDCAPS_ZBLTS                = $00100000;
  DDCAPS_ZOVERLAYS            = $00200000;
  DDCAPS_COLORKEY             = $00400000;
  DDCAPS_ALPHA                = $00800000;
  DDCAPS_COLORKEYHWASSIST     = $01000000;
  DDCAPS_NOHARDWARE           = $02000000;
  DDCAPS_BLTCOLORFILL         = $04000000;
  DDCAPS_BANKSWITCHED         = $08000000;
  DDCAPS_BLTDEPTHFILL         = $10000000;
  DDCAPS_CANCLIP              = $20000000;
  DDCAPS_CANCLIPSTRETCHED     = $40000000;
  DDCAPS_CANBLTSYSMEM         = $80000000;

{ More DirectDraw Driver Capability Flags (dwCaps2) }

  DDCAPS2_CERTIFIED           = $00000001;
  DDCAPS2_NO2DDURING3DSCENE   = $00000002;

{ DirectDraw FX Alpha Capability Flags }

  DDFXALPHACAPS_BLTALPHAEDGEBLEND         = $00000001;
  DDFXALPHACAPS_BLTALPHAPIXELS            = $00000002;
  DDFXALPHACAPS_BLTALPHAPIXELSNEG         = $00000004;
  DDFXALPHACAPS_BLTALPHASURFACES          = $00000008;
  DDFXALPHACAPS_BLTALPHASURFACESNEG       = $00000010;
  DDFXALPHACAPS_OVERLAYALPHAEDGEBLEND     = $00000020;
  DDFXALPHACAPS_OVERLAYALPHAPIXELS        = $00000040;
  DDFXALPHACAPS_OVERLAYALPHAPIXELSNEG     = $00000080;
  DDFXALPHACAPS_OVERLAYALPHASURFACES      = $00000100;
  DDFXALPHACAPS_OVERLAYALPHASURFACESNEG   = $00000200;

{ DirectDraw FX Capability Flags }

  DDFXCAPS_BLTARITHSTRETCHY       = $00000020;
  DDFXCAPS_BLTARITHSTRETCHYN      = $00000010;
  DDFXCAPS_BLTMIRRORLEFTRIGHT     = $00000040;
  DDFXCAPS_BLTMIRRORUPDOWN        = $00000080;
  DDFXCAPS_BLTROTATION            = $00000100;
  DDFXCAPS_BLTROTATION90          = $00000200;
  DDFXCAPS_BLTSHRINKX             = $00000400;
  DDFXCAPS_BLTSHRINKXN            = $00000800;
  DDFXCAPS_BLTSHRINKY             = $00001000;
  DDFXCAPS_BLTSHRINKYN            = $00002000;
  DDFXCAPS_BLTSTRETCHX            = $00004000;
  DDFXCAPS_BLTSTRETCHXN           = $00008000;
  DDFXCAPS_BLTSTRETCHY            = $00010000;
  DDFXCAPS_BLTSTRETCHYN           = $00020000;
  DDFXCAPS_OVERLAYARITHSTRETCHY   = $00040000;
  DDFXCAPS_OVERLAYARITHSTRETCHYN  = $00000008;
  DDFXCAPS_OVERLAYSHRINKX         = $00080000;
  DDFXCAPS_OVERLAYSHRINKXN        = $00100000;
  DDFXCAPS_OVERLAYSHRINKY         = $00200000;
  DDFXCAPS_OVERLAYSHRINKYN        = $00400000;
  DDFXCAPS_OVERLAYSTRETCHX        = $00800000;
  DDFXCAPS_OVERLAYSTRETCHXN       = $01000000;
  DDFXCAPS_OVERLAYSTRETCHY        = $02000000;
  DDFXCAPS_OVERLAYSTRETCHYN       = $04000000;
  DDFXCAPS_OVERLAYMIRRORLEFTRIGHT = $08000000;
  DDFXCAPS_OVERLAYMIRRORUPDOWN    = $10000000;

{ DirectDraw Stereo View Capabilities }

  DDSVCAPS_ENIGMA  = $00000001;
  DDSVCAPS_FLICKER = $00000002;
  DDSVCAPS_REDBLUE = $00000004;
  DDSVCAPS_SPLIT   = $00000008;

{ DirectDrawPalette Capabilities }

  DDPCAPS_4BIT               = $00000001;
  DDPCAPS_8BITENTRIES        = $00000002;
  DDPCAPS_8BIT               = $00000004;
  DDPCAPS_INITIALIZE         = $00000008;
  DDPCAPS_PRIMARYSURFACE     = $00000010;
  DDPCAPS_PRIMARYSURFACELEFT = $00000020;
  DDPCAPS_ALLOW256           = $00000040;
  DDPCAPS_VSYNC              = $00000080;
  DDPCAPS_1BIT               = $00000100;
  DDPCAPS_2BIT               = $00000200;

{ DirectDraw BitDepth Constants }

  DDBD_1  = $00004000;
  DDBD_2  = $00002000;
  DDBD_4  = $00001000;
  DDBD_8  = $00000800;
  DDBD_16 = $00000400;
  DDBD_24 = $00000200;
  DDBD_32 = $00000100;

{ DirectDraw Set/Get Color Key Flags }

  DDCKEY_COLORSPACE  = $00000001;
  DDCKEY_DESTBLT     = $00000002;
  DDCKEY_DESTOVERLAY = $00000004;
  DDCKEY_SRCBLT      = $00000008;
  DDCKEY_SRCOVERLAY  = $00000010;

{ DirectDraw Color Key Capability Flags }

  DDCKEYCAPS_DESTBLT                = $00000001;
  DDCKEYCAPS_DESTBLTCLRSPACE        = $00000002;
  DDCKEYCAPS_DESTBLTCLRSPACEYUV     = $00000004;
  DDCKEYCAPS_DESTBLTYUV             = $00000008;
  DDCKEYCAPS_DESTOVERLAY            = $00000010;
  DDCKEYCAPS_DESTOVERLAYCLRSPACE    = $00000020;
  DDCKEYCAPS_DESTOVERLAYCLRSPACEYUV = $00000040;
  DDCKEYCAPS_DESTOVERLAYONEACTIVE   = $00000080;
  DDCKEYCAPS_DESTOVERLAYYUV         = $00000100;
  DDCKEYCAPS_SRCBLT                 = $00000200;
  DDCKEYCAPS_SRCBLTCLRSPACE         = $00000400;
  DDCKEYCAPS_SRCBLTCLRSPACEYUV      = $00000800;
  DDCKEYCAPS_SRCBLTYUV              = $00001000;
  DDCKEYCAPS_SRCOVERLAY             = $00002000;
  DDCKEYCAPS_SRCOVERLAYCLRSPACE     = $00004000;
  DDCKEYCAPS_SRCOVERLAYCLRSPACEYUV  = $00008000;
  DDCKEYCAPS_SRCOVERLAYONEACTIVE    = $00010000;
  DDCKEYCAPS_SRCOVERLAYYUV          = $00020000;
  DDCKEYCAPS_NOCOSTOVERLAY          = $00040000;

{ DirectDraw PixelFormat Flags }

  DDPF_ALPHAPIXELS       = $00000001;
  DDPF_ALPHA             = $00000002;
  DDPF_FOURCC            = $00000004;
  DDPF_PALETTEINDEXED4   = $00000008;
  DDPF_PALETTEINDEXEDTO8 = $00000010;
  DDPF_PALETTEINDEXED8   = $00000020;
  DDPF_RGB               = $00000040;
  DDPF_COMPRESSED        = $00000080;
  DDPF_RGBTOYUV          = $00000100;
  DDPF_YUV               = $00000200;
  DDPF_ZBUFFER           = $00000400;
  DDPF_PALETTEINDEXED1   = $00000800;
  DDPF_PALETTEINDEXED2   = $00001000;

{ DirectDraw EnumSurfaces Flags }

  DDENUMSURFACES_ALL           = $00000001;
  DDENUMSURFACES_MATCH         = $00000002;
  DDENUMSURFACES_NOMATCH       = $00000004;
  DDENUMSURFACES_CANBECREATED  = $00000008;
  DDENUMSURFACES_DOESEXIST     = $00000010;

{ DirectDraw EnumDisplayModes Flags }

  DDEDM_REFRESHRATES = $00000001;

{ DirectDraw SetCooperativeLevel Flags }

  DDSCL_FULLSCREEN      = $00000001;
  DDSCL_ALLOWREBOOT     = $00000002;
  DDSCL_NOWINDOWCHANGES = $00000004;
  DDSCL_NORMAL          = $00000008;
  DDSCL_EXCLUSIVE       = $00000010;
  DDSCL_ALLOWMODEX      = $00000040;

{ DirectDraw Blt Flags }

  DDBLT_ALPHADEST                = $00000001;
  DDBLT_ALPHADESTCONSTOVERRIDE   = $00000002;
  DDBLT_ALPHADESTNEG             = $00000004;
  DDBLT_ALPHADESTSURFACEOVERRIDE = $00000008;
  DDBLT_ALPHAEDGEBLEND           = $00000010;
  DDBLT_ALPHASRC                 = $00000020;
  DDBLT_ALPHASRCCONSTOVERRIDE    = $00000040;
  DDBLT_ALPHASRCNEG              = $00000080;
  DDBLT_ALPHASRCSURFACEOVERRIDE  = $00000100;
  DDBLT_ASYNC                    = $00000200;
  DDBLT_COLORFILL                = $00000400;
  DDBLT_DDFX                     = $00000800;
  DDBLT_DDROPS                   = $00001000;
  DDBLT_KEYDEST                  = $00002000;
  DDBLT_KEYDESTOVERRIDE          = $00004000;
  DDBLT_KEYSRC                   = $00008000;
  DDBLT_KEYSRCOVERRIDE           = $00010000;
  DDBLT_ROP                      = $00020000;
  DDBLT_ROTATIONANGLE            = $00040000;
  DDBLT_ZBUFFER                  = $00080000;
  DDBLT_ZBUFFERDESTCONSTOVERRIDE = $00100000;
  DDBLT_ZBUFFERDESTOVERRIDE      = $00200000;
  DDBLT_ZBUFFERSRCCONSTOVERRIDE  = $00400000;
  DDBLT_ZBUFFERSRCOVERRIDE       = $00800000;
  DDBLT_WAIT                     = $01000000;
  DDBLT_DEPTHFILL                = $02000000;

{ BltFast Flags }

  DDBLTFAST_NOCOLORKEY   = $00000000;
  DDBLTFAST_SRCCOLORKEY  = $00000001;
  DDBLTFAST_DESTCOLORKEY = $00000002;
  DDBLTFAST_WAIT         = $00000010;

{ Flip Flags }

  DDFLIP_WAIT  = $00000001;

{ DirectDraw Surface Overlay Flags }

  DDOVER_ALPHADEST                = $00000001;
  DDOVER_ALPHADESTCONSTOVERRIDE   = $00000002;
  DDOVER_ALPHADESTNEG             = $00000004;
  DDOVER_ALPHADESTSURFACEOVERRIDE = $00000008;
  DDOVER_ALPHAEDGEBLEND           = $00000010;
  DDOVER_ALPHASRC                 = $00000020;
  DDOVER_ALPHASRCCONSTOVERRIDE    = $00000040;
  DDOVER_ALPHASRCNEG              = $00000080;
  DDOVER_ALPHASRCSURFACEOVERRIDE  = $00000100;
  DDOVER_HIDE                     = $00000200;
  DDOVER_KEYDEST                  = $00000400;
  DDOVER_KEYDESTOVERRIDE          = $00000800;
  DDOVER_KEYSRC                   = $00001000;
  DDOVER_KEYSRCOVERRIDE           = $00002000;
  DDOVER_SHOW                     = $00004000;
  DDOVER_ADDDIRTYRECT             = $00008000;
  DDOVER_REFRESHDIRTYRECTS        = $00010000;
  DDOVER_REFRESHALL               = $00020000;
  DDOVER_DDFX                     = $00080000;

{ DirectDrawSurface Lock Flags }

  DDLOCK_SURFACEMEMORYPTR  = $00000000;    // default
  DDLOCK_WAIT              = $00000001;
  DDLOCK_EVENT             = $00000002;
  DDLOCK_READONLY          = $00000010;
  DDLOCK_WRITEONLY         = $00000020;

{ DirectDrawSurface Blt FX Flags }

  DDBLTFX_ARITHSTRETCHY    = $00000001;
  DDBLTFX_MIRRORLEFTRIGHT  = $00000002;
  DDBLTFX_MIRRORUPDOWN     = $00000004;
  DDBLTFX_NOTEARING        = $00000008;
  DDBLTFX_ROTATE180        = $00000010;
  DDBLTFX_ROTATE270        = $00000020;
  DDBLTFX_ROTATE90         = $00000040;
  DDBLTFX_ZBUFFERRANGE     = $00000080;
  DDBLTFX_ZBUFFERBASEDEST  = $00000100;

{ DirectDrawSurface Overlay FX Flags }

  DDOVERFX_ARITHSTRETCHY   = $00000001;
  DDOVERFX_MIRRORLEFTRIGHT = $00000002;
  DDOVERFX_MIRRORUPDOWN    = $00000004;

{ DirectDraw WaitForVerticalBlank Flags }

  DDWAITVB_BLOCKBEGIN      = $00000001;
  DDWAITVB_BLOCKBEGINEVENT = $00000002;
  DDWAITVB_BLOCKEND        = $00000004;

{ DirectDraw GetFlipStatus Flags }

  DDGFS_CANFLIP    = $00000001;
  DDGFS_ISFLIPDONE = $00000002;

{ DirectDraw GetBltStatus Flags }

  DDGBS_CANBLT     = $00000001;
  DDGBS_ISBLTDONE  = $00000002;

{ DirectDraw EnumOverlayZOrder Flags }

  DDENUMOVERLAYZ_BACKTOFRONT = $00000000;
  DDENUMOVERLAYZ_FRONTTOBACK = $00000001;

{ DirectDraw UpdateOverlayZOrder Flags }

  DDOVERZ_SENDTOFRONT     = $00000000;
  DDOVERZ_SENDTOBACK      = $00000001;
  DDOVERZ_MOVEFORWARD     = $00000002;
  DDOVERZ_MOVEBACKWARD    = $00000003;
  DDOVERZ_INSERTINFRONTOF = $00000004;
  DDOVERZ_INSERTINBACKOF  = $00000005;

{ DirectDraw Return Codes }

  DD_OK                                   = 0;
  DDENUMRET_CANCEL                        = 0;
  DDENUMRET_OK                            = 1;
  DDERR_ALREADYINITIALIZED                = $88760000 + 5;
  DDERR_CANNOTATTACHSURFACE               = $88760000 + 10;
  DDERR_CANNOTDETACHSURFACE               = $88760000 + 20;
  DDERR_CURRENTLYNOTAVAIL                 = $88760000 + 40;
  DDERR_EXCEPTION                         = $88760000 + 55;
  DDERR_GENERIC                           = E_FAIL;
  DDERR_HEIGHTALIGN                       = $88760000 + 90;
  DDERR_INCOMPATIBLEPRIMARY               = $88760000 + 95;
  DDERR_INVALIDCAPS                       = $88760000 + 100;
  DDERR_INVALIDCLIPLIST                   = $88760000 + 110;
  DDERR_INVALIDMODE                       = $88760000 + 120;
  DDERR_INVALIDOBJECT                     = $88760000 + 130;
  DDERR_INVALIDPARAMS                     = E_INVALIDARG;
  DDERR_INVALIDPIXELFORMAT                = $88760000 + 145;
  DDERR_INVALIDRECT                       = $88760000 + 150;
  DDERR_LOCKEDSURFACES                    = $88760000 + 160;
  DDERR_NO3D                              = $88760000 + 170;
  DDERR_NOALPHAHW                         = $88760000 + 180;
  DDERR_NOCLIPLIST                        = $88760000 + 205;
  DDERR_NOCOLORCONVHW                     = $88760000 + 210;
  DDERR_NOCOOPERATIVELEVELSET             = $88760000 + 212;
  DDERR_NOCOLORKEY                        = $88760000 + 215;
  DDERR_NOCOLORKEYHW                      = $88760000 + 220;
  DDERR_NODIRECTDRAWSUPPORT               = $88760000 + 222;
  DDERR_NOEXCLUSIVEMODE                   = $88760000 + 225;
  DDERR_NOFLIPHW                          = $88760000 + 230;
  DDERR_NOGDI                             = $88760000 + 240;
  DDERR_NOMIRRORHW                        = $88760000 + 250;
  DDERR_NOTFOUND                          = $88760000 + 255;
  DDERR_NOOVERLAYHW                       = $88760000 + 260;
  DDERR_NORASTEROPHW                      = $88760000 + 280;
  DDERR_NOROTATIONHW                      = $88760000 + 290;
  DDERR_NOSTRETCHHW                       = $88760000 + 310;
  DDERR_NOT4BITCOLOR                      = $88760000 + 316;
  DDERR_NOT4BITCOLORINDEX                 = $88760000 + 317;
  DDERR_NOT8BITCOLOR                      = $88760000 + 320;
  DDERR_NOTEXTUREHW                       = $88760000 + 330;
  DDERR_NOVSYNCHW                         = $88760000 + 335;
  DDERR_NOZBUFFERHW                       = $88760000 + 340;
  DDERR_NOZOVERLAYHW                      = $88760000 + 350;
  DDERR_OUTOFCAPS                         = $88760000 + 360;
  DDERR_OUTOFMEMORY                       = E_OUTOFMEMORY;
  DDERR_OUTOFVIDEOMEMORY                  = $88760000 + 380;
  DDERR_OVERLAYCANTCLIP                   = $88760000 + 382;
  DDERR_OVERLAYCOLORKEYONLYONEACTIVE      = $88760000 + 384;
  DDERR_PALETTEBUSY                       = $88760000 + 387;
  DDERR_COLORKEYNOTSET                    = $88760000 + 400;
  DDERR_SURFACEALREADYATTACHED            = $88760000 + 410;
  DDERR_SURFACEALREADYDEPENDENT           = $88760000 + 420;
  DDERR_SURFACEBUSY                       = $88760000 + 430;
  DDERR_CANTLOCKSURFACE                   = $88760000 + 435;
  DDERR_SURFACEISOBSCURED                 = $88760000 + 440;
  DDERR_SURFACELOST                       = $88760000 + 450;
  DDERR_SURFACENOTATTACHED                = $88760000 + 460;
  DDERR_TOOBIGHEIGHT                      = $88760000 + 470;
  DDERR_TOOBIGSIZE                        = $88760000 + 480;
  DDERR_TOOBIGWIDTH                       = $88760000 + 490;
  DDERR_UNSUPPORTED                       = E_NOTIMPL;
  DDERR_UNSUPPORTEDFORMAT                 = $88760000 + 510;
  DDERR_UNSUPPORTEDMASK                   = $88760000 + 520;
  DDERR_VERTICALBLANKINPROGRESS           = $88760000 + 537;
  DDERR_WASSTILLDRAWING                   = $88760000 + 540;
  DDERR_XALIGN                            = $88760000 + 560;
  DDERR_INVALIDDIRECTDRAWGUID             = $88760000 + 561;
  DDERR_DIRECTDRAWALREADYCREATED          = $88760000 + 562;
  DDERR_NODIRECTDRAWHW                    = $88760000 + 563;
  DDERR_PRIMARYSURFACEALREADYEXISTS       = $88760000 + 564;
  DDERR_NOEMULATION                       = $88760000 + 565;
  DDERR_REGIONTOOSMALL                    = $88760000 + 566;
  DDERR_CLIPPERISUSINGHWND                = $88760000 + 567;
  DDERR_NOCLIPPERATTACHED                 = $88760000 + 568;
  DDERR_NOHWND                            = $88760000 + 569;
  DDERR_HWNDSUBCLASSED                    = $88760000 + 570;
  DDERR_HWNDALREADYSET                    = $88760000 + 571;
  DDERR_NOPALETTEATTACHED                 = $88760000 + 572;
  DDERR_NOPALETTEHW                       = $88760000 + 573;
  DDERR_BLTFASTCANTCLIP                   = $88760000 + 574;
  DDERR_NOBLTHW                           = $88760000 + 575;
  DDERR_NODDROPSHW                        = $88760000 + 576;
  DDERR_OVERLAYNOTVISIBLE                 = $88760000 + 577;
  DDERR_NOOVERLAYDEST                     = $88760000 + 578;
  DDERR_INVALIDPOSITION                   = $88760000 + 579;
  DDERR_NOTAOVERLAYSURFACE                = $88760000 + 580;
  DDERR_EXCLUSIVEMODEALREADYSET           = $88760000 + 581;
  DDERR_NOTFLIPPABLE                      = $88760000 + 582;
  DDERR_CANTDUPLICATE                     = $88760000 + 583;
  DDERR_NOTLOCKED                         = $88760000 + 584;
  DDERR_CANTCREATEDC                      = $88760000 + 585;
  DDERR_NODC                              = $88760000 + 586;
  DDERR_WRONGMODE                         = $88760000 + 587;
  DDERR_IMPLICITLYCREATED                 = $88760000 + 588;
  DDERR_NOTPALETTIZED                     = $88760000 + 589;
  DDERR_UNSUPPORTEDMODE                   = $88760000 + 590;
  DDERR_NOMIPMAPHW                        = $88760000 + 591;
  DDERR_INVALIDSURFACETYPE                = $88760000 + 592;
  DDERR_DCALREADYCREATED                  = $88760000 + 620;
  DDERR_CANTPAGELOCK                      = $88760000 + 640;
  DDERR_CANTPAGEUNLOCK                    = $88760000 + 660;
  DDERR_NOTPAGELOCKED                     = $88760000 + 680;
  DDERR_NOTINITIALIZED                    = CO_E_NOTINITIALIZED;

{ API's }

const
  REGSTR_KEY_DDHW_DESCRIPTION = 'Description';
  REGSTR_KEY_DDHW_DRIVERNAME  = 'DriverName';
  REGSTR_PATH_DDHW            = 'Hardware\DirectDrawDrivers';

  DDCREATE_HARDWAREONLY       = $00000001;
  DDCREATE_EMULATIONONLY      = $00000002;

function DirectDrawEnumerateA(lpCallback: LPDDENUMCALLBACKA;
    lpContext: Pointer): HRESULT; stdcall;
function DirectDrawEnumerateW(lpCallback: LPDDENUMCALLBACKA;
    lpContext: Pointer): HRESULT; stdcall;
function DirectDrawEnumerate(lpCallback: LPDDENUMCALLBACKW;
    lpContext: Pointer): HRESULT; stdcall;

function DirectDrawCreate(lpGUID: PGUID; out lplpDD: IDirectDraw;
    pUnkOuter: IUnknown): HRESULT; stdcall;

function DirectDrawCreateClipper(dwFlags: DWORD;
    out lplpDDClipper: IDirectDrawClipper;
    pUnkOuter: IUnknown): HRESULT; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dvp.h
 *  Content:    DirectDrawVideoPort include file
 *
 ***************************************************************************)

{ GUIDS used by DirectDrawVideoPort objects }

const
  IID_IDDVideoPortContainer: TGUID = (D1:$6C142760;D2:$A733;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectDrawVideoPort: TGUID = (D1:$B36D93E0;D2:$2B43;D3:$11CF;D4:($A2,$DE,$00,$AA,$00,$B9,$33,$56));

  DDVPTYPE_E_HREFH_VREFH: TGUID = (D1:$54F39980;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFH_VREFL: TGUID = (D1:$92783220;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFL_VREFH: TGUID = (D1:$A07A02E0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFL_VREFL: TGUID = (D1:$E09C77E0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_CCIR656: TGUID = (D1:$FCA326A0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_BROOKTREE: TGUID = (D1:$1352A560;D2:$DA61;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_PHILIPS: TGUID = (D1:$332CF160;D2:$DA61;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));

{ DirectDraw Structures }

type
  IDDVideoPortContainer = interface;
  IDirectDrawVideoPort = interface;

{ DDVIDEOPORTCONNECT structure }

  DDVIDEOPORTCONNECT = record
    dwSize: DWORD;        // size of the DDVIDEOPORTCONNECT structure
    dwPortWidth: DWORD;   // Width of the video port
    guidTypeID: TGUID;    // Description of video port connection
    dwFlags: DWORD;       // Connection flags
    dwReserved1: DWORD;   // Reserved, set to zero.
  end;
  LPDDVIDEOPORTCONNECT = ^DDVIDEOPORTCONNECT;

{ DDVIDEOPORTCAPS structure }

  DDVIDEOPORTCAPS = record
    dwSize: DWORD;                          // size of the DDVIDEOPORTCAPS structure
    dwFlags: DWORD;                         // indicates which fields contain data
    dwMaxWidth: DWORD;                      // max width of the video port field
    dwMaxVBIWidth: DWORD;                   // max width of the VBI data
    dwMaxHeight: DWORD;                     // max height of the video port field
    dwVideoPortID: DWORD;                   // Video port ID (0 - (dwMaxVideoPorts -1))
    dwCaps: DWORD;                          // Video port capabilities
    dwFX: DWORD;                            // More video port capabilities
    dwNumAutoFlipSurfaces: DWORD;           // Number of autoflippable surfaces
    dwAlignVideoPortBoundary: DWORD;        // Byte restriction of placement within the surface
    dwAlignVideoPortPrescaleWidth: DWORD;   // Byte restriction of width after prescaling
    dwAlignVideoPortCropBoundary: DWORD;    // Byte restriction of left cropping
    dwAlignVideoPortCropWidth: DWORD;       // Byte restriction of cropping width
    dwPreshrinkXStep: DWORD;                // Width can be shrunk in steps of 1/x
    dwPreshrinkYStep: DWORD;                // Height can be shrunk in steps of 1/x
    dwNumVBIAutoFlipSurfaces: DWORD;        // Number of VBI autoflippable surfaces
    dwReserved1: DWORD;                     // Reserved for future use
    dwReserved2: DWORD;                     // Reserved for future use
  end;
  LPDDVIDEOPORTCAPS = ^DDVIDEOPORTCAPS;

{ DDVIDEOPORTDESC structure }

  DDVIDEOPORTDESC = record
    dwSize: DWORD;                       // size of the DDVIDEOPORTDESC structure
    dwFieldWidth: DWORD;                 // width of the video port field
    dwVBIWidth: DWORD;                   // width of the VBI data
    dwFieldHeight: DWORD;                // height of the video port field
    dwMicrosecondsPerField: DWORD;       // Microseconds per video field
    dwMaxPixelsPerSecond: DWORD;         // Maximum pixel rate per second
    dwVideoPortID: DWORD;                // Video port ID (0 - (dwMaxVideoPorts -1))
    dwReserved1: DWORD;                  // Reserved for future use - set to zero
    VideoPortType: DDVIDEOPORTCONNECT;   // Description of video port connection
    dwReserved2: DWORD;                  // Reserved for future use - set to zero
    dwReserved3: DWORD;                  // Reserved for future use - set to zero
  end;
  LPDDVIDEOPORTDESC = ^DDVIDEOPORTDESC;

{ DDVIDEOPORTINFO structure }

  DDVIDEOPORTINFO = record
    dwSize: DWORD;                            // Size of the structure
    dwOriginX: DWORD;                         // Placement of the video data within the surface.
    dwOriginY: DWORD;                         // Placement of the video data within the surface.
    dwVPFlags: DWORD;                         // Video port options
    rCrop: TRect;                             // Cropping rectangle (optional).
    dwPrescaleWidth: DWORD;                   // Determines pre-scaling/zooming in the X direction (optional).
    dwPrescaleHeight: DWORD;                  // Determines pre-scaling/zooming in the Y direction (optional).
    lpddpfInputFormat: LPDDPIXELFORMAT;       // Video format written to the video port
    lpddpfVBIInputFormat: LPDDPIXELFORMAT;    // Input format of the VBI data
    lpddpfVBIOutputFormat: LPDDPIXELFORMAT;   // Output format of the data
    dwVBIHeight: DWORD;                       // Specifies the number of lines of data within the vertical blanking interval.
    dwReserved1: DWORD;                       // Reserved for future use - set to zero
    dwReserved2: DWORD;                       // Reserved for future use - set to zero
  end;
  LPDDVIDEOPORTINFO = ^DDVIDEOPORTINFO;

{ DDVIDEOPORTBANDWIDTH structure }

  DDVIDEOPORTBANDWIDTH = record
    dwSize: DWORD;                 // Size of the structure
    dwCaps: DWORD;
    dwOverlay: DWORD;              // Zoom factor at which overlay is supported
    dwColorkey: DWORD;             // Zoom factor at which overlay w/ colorkey is supported
    dwYInterpolate: DWORD;         // Zoom factor at which overlay w/ Y interpolation is supported
    dwYInterpAndColorkey: DWORD;   // Zoom factor at which ovelray w/ Y interpolation and colorkeying is supported
    dwReserved1: DWORD;            // Reserved for future use - set to zero
    dwReserved2: DWORD;            // Reserved for future use - set to zero
  end;
  LPDDVIDEOPORTBANDWIDTH = ^DDVIDEOPORTBANDWIDTH;

{ DDVIDEOPORTSTATUS structure }

  DDVIDEOPORTSTATUS = record
    dwSize: DWORD;                       // Size of the structure
    bInUse: BOOL;                        // TRUE if video port is currently being used
    dwFlags: DWORD;                      // Currently not used
    dwReserved1: DWORD;                  // Reserved for future use
    VideoPortType: DDVIDEOPORTCONNECT;   // Information about the connection
    dwReserved2: DWORD;                  // Reserved for future use
    dwReserved3: DWORD;                  // Reserved for future use
  end;
  LPDDVIDEOPORTSTATUS = ^DDVIDEOPORTSTATUS;

{ API's }

  LPDDENUMVIDEOCALLBACK = function(lpDDVideoPortCaps: DDVIDEOPORTCAPS;
      lpContext: Pointer): HRESULT; stdcall;

{ IDirectDrawVideoPortContainer Interface }

  IDDVideoPortContainer = interface(IUnknown)
    ['{6C142760-A733-11CE-A521-0020AF0BE560}']
    (*** IDDVideoPortContainer methods ***)
    function CreateVideoPort(dwFlags: DWORD; const lpDDVideoPortDesc:
        DDVIDEOPORTDESC; out lplpDDVideoPort: IDirectDrawVideoPort;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function EnumVideoPorts(dwFlags: DWORD;
        const lpDDVideoPortCaps: DDVIDEOPORTCAPS; lpContext: Pointer;
        lpEnumVideoCallback: LPDDENUMVIDEOCALLBACK): HRESULT; stdcall;
    function GetVideoPortConnectInfo(dwPortId: DWORD; var lpNumEntries: DWORD;
        var lpConnectInfo: DDVIDEOPORTCONNECT): HRESULT; stdcall;
    function QueryVideoPortStatus(dwPortId: DWORD;
        var lpVPStatus: DDVIDEOPORTSTATUS): HRESULT; stdcall;
  end;

{ IDirectDrawVideoPort Interface }

  IDirectDrawVideoPort = interface(IUnknown)
    ['{B36D93E0-2B43-11CF-A2DE-00AA00B93356}']
    (*** IDirectDrawVideoPort methods ***)
    function Flip(lpDDSurface: IDirectDrawSurface; dwFlags: DWORD): HRESULT;
        stdcall;
    function GetBandwidthInfo(const lpddpfFormat: DDPIXELFORMAT; dwWidth: DWORD;
        dwHeight: DWORD; dwFlags: DWORD; var lpBandwidth: DDVIDEOPORTBANDWIDTH):
        HRESULT; stdcall;
    function GetColorControls(var lpColorControl: DDCOLORCONTROL): HRESULT;
        stdcall;
    function GetInputFormats(var lpNumFormats: DWORD; var lpFormats:
        DDPIXELFORMAT; dwFlags: DWORD): HRESULT; stdcall;
    function GetOutputFormats(const lpInputFormat: DDPIXELFORMAT;
        var lpNumFormats: DWORD; var lpFormats: DDPIXELFORMAT; dwFlags: DWORD):
        HRESULT; stdcall;
    function GetFieldPolarity(var lpbVideoField: BOOL): HRESULT; stdcall;
    function GetVideoLine(var lpdwLine: DWORD): HRESULT; stdcall;
    function GetVideoSignalStatus(varlpdwStatus: DWORD): HRESULT; stdcall;
    function SetColorControls(const lpColorControl: DDCOLORCONTROL): HRESULT;
        stdcall;
    function SetTargetSurface(lpDDSurface: IDirectDrawSurface; dwFlags: DWORD):
        HRESULT; stdcall;
    function StartVideo(const lpVideoInfo: DDVIDEOPORTINFO): HRESULT; stdcall;
    function StopVideo: HRESULT; stdcall;
    function UpdateVideo(const lpVideoInfo: DDVIDEOPORTINFO): HRESULT; stdcall;
    function WaitForSync(dwFlags: DWORD; dwLine: DWORD; dwTimeout: DWORD):
        HRESULT; stdcall;
  end;


const
{ Video Port Flags }

  DDVPD_WIDTH = $00000001;
  DDVPD_HEIGHT = $00000002;
  DDVPD_ID = $00000004;
  DDVPD_CAPS = $00000008;
  DDVPD_FX = $00000010;
  DDVPD_AUTOFLIP = $00000020;
  DDVPD_ALIGN = $00000040;

{ VIDEOPORT DDVIDEOPORTCONNECT FLAGS }

  DDVPCONNECT_DOUBLECLOCK = $00000001;
  DDVPCONNECT_VACT = $00000002;
  DDVPCONNECT_INVERTPOLARITY = $00000004;
  DDVPCONNECT_DISCARDSVREFDATA = $00000008;
  DDVPCONNECT_HALFLINE = $00000010;
  DDVPCONNECT_INTERLACED = $00000020;
  DDVPCONNECT_SHAREEVEN = $00000040;
  DDVPCONNECT_SHAREODD = $00000080;

{ VIDEOPORT DDVIDEOPORTDESC CAPS }

  DDVPCAPS_AUTOFLIP = $00000001;
  DDVPCAPS_INTERLACED = $00000002;
  DDVPCAPS_NONINTERLACED = $00000004;
  DDVPCAPS_READBACKFIELD = $00000008;
  DDVPCAPS_READBACKLINE = $00000010;
  DDVPCAPS_SHAREABLE = $00000020;
  DDVPCAPS_SKIPEVENFIELDS = $00000040;
  DDVPCAPS_SKIPODDFIELDS = $00000080;
  DDVPCAPS_SYNCMASTER = $00000100;
  DDVPCAPS_VBISURFACE = $00000200;
  DDVPCAPS_COLORCONTROL = $00000400;
  DDVPCAPS_OVERSAMPLEDVBI = $00000800;
  DDVPCAPS_SYSTEMMEMORY = $00001000;

{ VIDEOPORT DDVIDEOPORTDESC FX }

  DDVPFX_CROPTOPDATA = $00000001;
  DDVPFX_CROPX = $00000002;
  DDVPFX_CROPY = $00000004;
  DDVPFX_INTERLEAVE = $00000008;
  DDVPFX_MIRRORLEFTRIGHT = $00000010;
  DDVPFX_MIRRORUPDOWN = $00000020;
  DDVPFX_PRESHRINKX = $00000040;
  DDVPFX_PRESHRINKY = $00000080;
  DDVPFX_PRESHRINKXB = $00000100;
  DDVPFX_PRESHRINKYB = $00000200;
  DDVPFX_PRESHRINKXS = $00000400;
  DDVPFX_PRESHRINKYS = $00000800;
  DDVPFX_PRESTRETCHX = $00001000;
  DDVPFX_PRESTRETCHY = $00002000;
  DDVPFX_PRESTRETCHXN = $00004000;
  DDVPFX_PRESTRETCHYN = $00008000;
  DDVPFX_VBICONVERT = $00010000;
  DDVPFX_VBINOSCALE = $00020000;
  DDVPFX_IGNOREVBIXCROP = $00040000;

{ VIDEOPORT DDVIDEOPORTINFO FLAGS }

  DDVP_AUTOFLIP = $00000001;
  DDVP_CONVERT = $00000002;
  DDVP_CROP = $00000004;
  DDVP_INTERLEAVE = $00000008;
  DDVP_MIRRORLEFTRIGHT = $00000010;
  DDVP_MIRRORUPDOWN = $00000020;
  DDVP_PRESCALE = $00000040;
  DDVP_SKIPEVENFIELDS = $00000080;
  DDVP_SKIPODDFIELDS = $00000100;
  DDVP_SYNCMASTER = $00000200;
  DDVP_VBICONVERT = $00000400;
  DDVP_VBINOSCALE = $00000800;
  DDVP_OVERRIDEBOBWEAVE = $00001000;
  DDVP_IGNOREVBIXCROP = $00002000;

{ DIRIRECTDRAWVIDEOPORT GETINPUTFORMAT/GETOUTPUTFORMAT FLAGS }

  DDVPFORMAT_VIDEO = $00000001;
  DDVPFORMAT_VBI = $00000002;

{ DIRIRECTDRAWVIDEOPORT SETTARGETSURFACE FLAGS }

  DDVPTARGET_VIDEO = $00000001;
  DDVPTARGET_VBI = $00000002;

{ DIRIRECTDRAWVIDEOPORT WAITFORSYNC FLAGS }

  DDVPWAIT_BEGIN = $00000001;
  DDVPWAIT_END = $00000002;
  DDVPWAIT_LINE = $00000003;

{ DIRECTDRAWVIDEOPORT FLIP FLAGS }

  DDVPFLIP_VIDEO = $00000001;
  DDVPFLIP_VBI = $00000002;

{ DIRIRECTDRAWVIDEOPORT GETVIDEOSIGNALSTATUS VALUES }

  DDVPSQ_NOSIGNAL = $00000001;
  DDVPSQ_SIGNALOK = $00000002;

{ VIDEOPORTBANDWIDTH Flags }

  DDVPB_VIDEOPORT = $00000001;
  DDVPB_OVERLAY = $00000002;
  DDVPB_TYPE = $00000004;

{ VIDEOPORTBANDWIDTH Caps }

  DDVPBCAPS_SOURCE = $00000001;
  DDVPBCAPS_DESTINATION = $00000002;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3dtypes.h
 *  Content:    Direct3D types include file
 *
 ***************************************************************************)

type
  D3DVALUE = Single;
  D3DFIXED = Longint;
  D3DCOLOR = DWORD;

function D3DVALP(val: D3DVALUE; prec: Integer): D3DVALUE;
function D3DVAL(val: D3DVALUE): D3DVALUE;
function D3DDivide(a, b: D3DVALUE): D3DVALUE;
function D3DMultiply(a, b: D3DVALUE): D3DVALUE;

(*
 * Format of CI colors is
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |    alpha      |         color index           |   fraction    |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *)

function CI_GETALPHA(ci: Integer): Byte;
function CI_GETINDEX(ci: Integer): Word;
function CI_GETFRACTION(ci: Integer): Byte;
function CI_ROUNDINDEX(ci: Integer): Integer;
function CI_MASKALPHA(ci: Integer): Integer;
function CI_MAKE(a: Byte; i: Word; f: Byte): Integer;

(*
 * Format of RGBA colors is
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |    alpha      |      red      |     green     |     blue      |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *)

function RGBA_GETALPHA(rgb: D3DCOLOR): Byte;
function RGBA_GETRED(rgb: D3DCOLOR): Byte;
function RGBA_GETGREEN(rgb: D3DCOLOR): Byte;
function RGBA_GETBLUE(rgb: D3DCOLOR): Byte;
function RGBA_MAKE(r, g, b, a: Byte): D3DCOLOR;

(* D3DRGB and D3DRGBA may be used as initialisers for D3DCOLORs
 * The float values must be in the range 0..1
 *)

function D3DRGB(r, g, b: D3DVALUE): D3DCOLOR;
function D3DRGBA(r, g, b, a: D3DVALUE): D3DCOLOR;

(*
 * Format of RGB colors is
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |    ignored    |      red      |     green     |     blue      |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *)

function RGB_GETRED(rgb: D3DCOLOR): Byte;
function RGB_GETGREEN(rgb: D3DCOLOR): Byte;
function RGB_GETBLUE(rgb: D3DCOLOR): Byte;
function RGBA_SETALPHA(rgba: D3DCOLOR; x: Byte): D3DCOLOR;
function RGB_MAKE(r, g, b: Byte): D3DCOLOR;
function RGBA_TORGB(rgba: D3DCOLOR): D3DCOLOR;
function RGB_TORGBA(rgb: D3DCOLOR): D3DCOLOR;

{ Flags for Enumerate functions }
const
  D3DENUMRET_CANCEL                        = DDENUMRET_CANCEL;
  D3DENUMRET_OK                            = DDENUMRET_OK;

type
  LPD3DVALIDATECALLBACK = function(lpUserArg: Pointer; dwOffset: DWORD):
      HRESULT; stdcall;

  LPD3DENUMTEXTUREFORMATSCALLBACK = function(const lpDdsd: TDDSurfaceDesc;
      lpContext: Pointer): HRESULT; stdcall;

  D3DMATERIALHANDLE = DWORD;
  D3DTEXTUREHANDLE = DWORD;
  D3DMATRIXHANDLE = DWORD;

  D3DCOLORVALUE = record
    case Integer of
    0: (
      r: D3DVALUE;
      g: D3DVALUE;
      b: D3DVALUE;
      a: D3DVALUE;
      );
    1: (
      dvR: D3DVALUE;
      dvG: D3DVALUE;
      dvB: D3DVALUE;
      dvA: D3DVALUE;
      );
  end;

  D3DRECT = record
    case Integer of
    0: (
      x1: Longint;
      y1: Longint;
      x2: Longint;
      y2: Longint;
      );
    1: (
      lX1: Longint;
      lY1: Longint;
      lX2: Longint;
      lY2: Longint;
      );
  end;
  LPD3DRECT = ^D3DRECT;

  D3DVECTOR = record
    case Integer of
    0: (
      x: D3DVALUE;
      y: D3DVALUE;
      z: D3DVALUE;
      );
    1: (
      dvX: D3DVALUE;
      dvY: D3DVALUE;
      dvZ: D3DVALUE;
      );
  end;
  LPD3DVECTOR = ^D3DVECTOR;

// Addition and subtraction
function VectorAdd(v1, v2: D3DVECTOR) : D3DVECTOR;
function VectorSub(v1, v2: D3DVECTOR) : D3DVECTOR;
// Scalar multiplication and division
function VectorMulS(v: D3DVECTOR; s: D3DVALUE) : D3DVECTOR;
function VectorDivS(v: D3DVECTOR; s: D3DVALUE) : D3DVECTOR;
// Memberwise multiplication and division
function VectorMul(v1, v2: D3DVECTOR) : D3DVECTOR;
function VectorDiv(v1, v2: D3DVECTOR) : D3DVECTOR;
// Vector dominance
function VectorSmaller(v1, v2: D3DVECTOR) : boolean;
function VectorSmallerEquel(v1, v2: D3DVECTOR) : boolean;
// Bitwise equality
function VectorEquel(v1, v2: D3DVECTOR) : boolean;
// Length-related functions
function VectorSquareMagnitude(v: D3DVECTOR) : D3DVALUE;
function VectorMagnitude(v: D3DVECTOR) : D3DVALUE;
// Returns vector with same direction and unit length
function VectorNormalize(v: D3DVECTOR) : D3DVECTOR;
// Return min/max component of the input vector
function VectorMin(v: D3DVECTOR) : D3DVALUE;
function VectorMax(v: D3DVECTOR) : D3DVALUE;
// Return memberwise min/max of input vectors
function VectorMinimize(v1, v2: D3DVECTOR) : D3DVECTOR;
function VectorMaximize(v1, v2: D3DVECTOR) : D3DVECTOR;
// Dot and cross product
function VectorDotProduct(v1, v2: D3DVECTOR) : D3DVALUE;
function VectorCrossProduct(v1, v2: D3DVECTOR) : D3DVECTOR;

type
{ Vertex data types supported in an ExecuteBuffer. }

{ D3DHVERTEX structure }

  D3DHVERTEX = record
    dwFlags: DWORD;        // Homogeneous clipping flags
    case Integer of
    0: (
      hx: D3DVALUE;
      hy: D3DVALUE;
      hz: D3DVALUE;
      );
    1: (
      dvHX: D3DVALUE;
      dvHY: D3DVALUE;
      dvHZ: D3DVALUE;
      );
  end;

{ D3DTLVERTEX structure }

  D3DTLVERTEX = record
    case Integer of
    0: (
      sx: D3DVALUE;             // Screen coordinates
      sy: D3DVALUE;
      sz: D3DVALUE;
      rhw: D3DVALUE;            // Reciprocal of homogeneous w
      color: D3DCOLOR;          // Vertex color
      specular: D3DCOLOR;       // Specular component of vertex
      tu: D3DVALUE;             // Texture coordinates
      tv: D3DVALUE;
      );
    1: (
      dvSX: D3DVALUE;
      dvSY: D3DVALUE;
      dvSZ: D3DVALUE;
      dvRHW: D3DVALUE;
      dcColor: D3DCOLOR;
      dcSpecular: D3DCOLOR;
      dvTU: D3DVALUE;
      dvTV: D3DVALUE;
      );
  end;

{ D3DLVERTEX structure }

  D3DLVERTEX = record
    case Integer of
    0: (
      x: D3DVALUE;             // Homogeneous coordinates
      y: D3DVALUE;
      z: D3DVALUE;
      dwReserved: DWORD;
      color: D3DCOLOR;         // Vertex color
      specular: D3DCOLOR;      // Specular component of vertex
      tu: D3DVALUE;            // Texture coordinates
      tv: D3DVALUE;
      );
    1: (
      dvX: D3DVALUE;
      dvY: D3DVALUE;
      dvZ: D3DVALUE;
      UNIONFILLER1d: DWORD;
      dcColor: D3DCOLOR;
      dcSpecular: D3DCOLOR;
      dvTU: D3DVALUE;
      dvTV: D3DVALUE;
      );
  end;

{ D3DVERTEX structure }

  D3DVERTEX = record
    case Integer of
    0: (
      x: D3DVALUE;             // Homogeneous coordinates
      y: D3DVALUE;
      z: D3DVALUE;
      nx: D3DVALUE;            // Normal
      ny: D3DVALUE;
      nz: D3DVALUE;
      tu: D3DVALUE;            // Texture coordinates
      tv: D3DVALUE;
      );
    1: (
      dvX: D3DVALUE;
      dvY: D3DVALUE;
      dvZ: D3DVALUE;
      dvNX: D3DVALUE;
      dvNY: D3DVALUE;
      dvNZ: D3DVALUE;
      dvTU: D3DVALUE;
      dvTV: D3DVALUE;
      );
  end;

{ D3DMATRIX structure }

  D3DMATRIX = record
    _11, _12, _13, _14: D3DVALUE;
    _21, _22, _23, _24: D3DVALUE;
    _31, _32, _33, _34: D3DVALUE;
    _41, _42, _43, _44: D3DVALUE;
  end;

{ D3DVIEWPORT structure }

  D3DVIEWPORT = record
    dwSize: DWORD;
    dwX: DWORD;
    dwY: DWORD;               // Top left
    dwWidth: DWORD;
    dwHeight: DWORD;          // Dimensions
    dvScaleX: D3DVALUE;       // Scale homogeneous to screen
    dvScaleY: D3DVALUE;       // Scale homogeneous to screen
    dvMaxX: D3DVALUE;         // Min/max homogeneous x coord
    dvMaxY: D3DVALUE;         // Min/max homogeneous y coord
    dvMinZ: D3DVALUE;
    dvMaxZ: D3DVALUE;         // Min/max homogeneous z coord
  end;

{ D3DVIEWPORT2 structure }

  D3DVIEWPORT2 = record
    dwSize: DWORD;
    dwX: DWORD;
    dwY: DWORD;               // Top left
    dwWidth: DWORD;
    dwHeight: DWORD;          // Dimensions
    dvClipX: D3DVALUE;        // Top left of clip volume
    dvClipY: D3DVALUE;
    dvClipWidth: D3DVALUE;    // Clip Volume Dimensions
    dvClipHeight: D3DVALUE;
    dvMinZ: D3DVALUE;
    dvMaxZ: D3DVALUE;         // Min/max homogeneous z coord
  end;

{ Values for clip fields. }

const
  D3DCLIP_LEFT                            = $00000001;
  D3DCLIP_RIGHT                           = $00000002;
  D3DCLIP_TOP                             = $00000004;
  D3DCLIP_BOTTOM                          = $00000008;
  D3DCLIP_FRONT                           = $00000010;
  D3DCLIP_BACK                            = $00000020;
  D3DCLIP_GEN0                            = $00000040;
  D3DCLIP_GEN1                            = $00000080;
  D3DCLIP_GEN2                            = $00000100;
  D3DCLIP_GEN3                            = $00000200;
  D3DCLIP_GEN4                            = $00000400;
  D3DCLIP_GEN5                            = $00000800;

{ Values for d3d status. }

  D3DSTATUS_CLIPUNIONLEFT                 = D3DCLIP_LEFT;
  D3DSTATUS_CLIPUNIONRIGHT                = D3DCLIP_RIGHT;
  D3DSTATUS_CLIPUNIONTOP                  = D3DCLIP_TOP;
  D3DSTATUS_CLIPUNIONBOTTOM               = D3DCLIP_BOTTOM;
  D3DSTATUS_CLIPUNIONFRONT                = D3DCLIP_FRONT;
  D3DSTATUS_CLIPUNIONBACK                 = D3DCLIP_BACK;
  D3DSTATUS_CLIPUNIONGEN0                 = D3DCLIP_GEN0;
  D3DSTATUS_CLIPUNIONGEN1                 = D3DCLIP_GEN1;
  D3DSTATUS_CLIPUNIONGEN2                 = D3DCLIP_GEN2;
  D3DSTATUS_CLIPUNIONGEN3                 = D3DCLIP_GEN3;
  D3DSTATUS_CLIPUNIONGEN4                 = D3DCLIP_GEN4;
  D3DSTATUS_CLIPUNIONGEN5                 = D3DCLIP_GEN5;

  D3DSTATUS_CLIPINTERSECTIONLEFT          = $00001000;
  D3DSTATUS_CLIPINTERSECTIONRIGHT         = $00002000;
  D3DSTATUS_CLIPINTERSECTIONTOP           = $00004000;
  D3DSTATUS_CLIPINTERSECTIONBOTTOM        = $00008000;
  D3DSTATUS_CLIPINTERSECTIONFRONT         = $00010000;
  D3DSTATUS_CLIPINTERSECTIONBACK          = $00020000;
  D3DSTATUS_CLIPINTERSECTIONGEN0          = $00040000;
  D3DSTATUS_CLIPINTERSECTIONGEN1          = $00080000;
  D3DSTATUS_CLIPINTERSECTIONGEN2          = $00100000;
  D3DSTATUS_CLIPINTERSECTIONGEN3          = $00200000;
  D3DSTATUS_CLIPINTERSECTIONGEN4          = $00400000;
  D3DSTATUS_CLIPINTERSECTIONGEN5          = $00800000;
  D3DSTATUS_ZNOTVISIBLE                   = $01000000;

  D3DSTATUS_CLIPUNIONALL = (
    D3DSTATUS_CLIPUNIONLEFT or
    D3DSTATUS_CLIPUNIONRIGHT or
    D3DSTATUS_CLIPUNIONTOP or
    D3DSTATUS_CLIPUNIONBOTTOM or
    D3DSTATUS_CLIPUNIONFRONT or
    D3DSTATUS_CLIPUNIONBACK or
    D3DSTATUS_CLIPUNIONGEN0 or
    D3DSTATUS_CLIPUNIONGEN1 or
    D3DSTATUS_CLIPUNIONGEN2 or
    D3DSTATUS_CLIPUNIONGEN3 or
    D3DSTATUS_CLIPUNIONGEN4 or
    D3DSTATUS_CLIPUNIONGEN5);

  D3DSTATUS_CLIPINTERSECTIONALL = (
    D3DSTATUS_CLIPINTERSECTIONLEFT or
    D3DSTATUS_CLIPINTERSECTIONRIGHT or
    D3DSTATUS_CLIPINTERSECTIONTOP or
    D3DSTATUS_CLIPINTERSECTIONBOTTOM or
    D3DSTATUS_CLIPINTERSECTIONFRONT or
    D3DSTATUS_CLIPINTERSECTIONBACK or
    D3DSTATUS_CLIPINTERSECTIONGEN0 or
    D3DSTATUS_CLIPINTERSECTIONGEN1 or
    D3DSTATUS_CLIPINTERSECTIONGEN2 or
    D3DSTATUS_CLIPINTERSECTIONGEN3 or
    D3DSTATUS_CLIPINTERSECTIONGEN4 or
    D3DSTATUS_CLIPINTERSECTIONGEN5 );

  D3DSTATUS_DEFAULT = (
    D3DSTATUS_CLIPINTERSECTIONALL or
    D3DSTATUS_ZNOTVISIBLE );

{ Options for direct transform calls }

  D3DTRANSFORM_CLIPPED       = $00000001;
  D3DTRANSFORM_UNCLIPPED     = $00000002;

type
{ D3DTRANSFORMDATA structure }

  D3DTRANSFORMDATA = record
    dwSize: DWORD;
    lpIn: Pointer;             // Input vertices
    dwInSize: DWORD;           // Stride of input vertices
    lpOut: Pointer;            // Output vertices
    dwOutSize: DWORD;          // Stride of output vertices
    lpHOut: ^D3DHVERTEX;       // Output homogeneous vertices
    dwClip: DWORD;             // Clipping hint
    dwClipIntersection: DWORD;
    dwClipUnion: DWORD;        // Union of all clip flags
    drExtent: D3DRECT;         // Extent of transformed vertices
  end;

{ D3DLIGHTINGELEMENT structure }

  D3DLIGHTINGELEMENT = record
    dvPosition: D3DVECTOR;           // Lightable point in model space
    dvNormal: D3DVECTOR;             // Normalised unit vector
  end;

{ D3DMATERIAL structure }

  D3DMATERIAL = record
    dwSize: DWORD;
    case Integer of
    0: (
      diffuse: D3DCOLORVALUE;        // Diffuse color RGBA
      ambient: D3DCOLORVALUE;        // Ambient color RGB
      specular: D3DCOLORVALUE;       // Specular 'shininess'
      emissive: D3DCOLORVALUE;       // Emissive color RGB
      power: D3DVALUE;               // Sharpness if specular highlight
      hTexture: D3DTEXTUREHANDLE;    // Handle to texture map
      dwRampSize: DWORD;
      );
    1: (
      dcvDiffuse: D3DCOLORVALUE;
      dcvAmbient: D3DCOLORVALUE;
      dcvSpecular: D3DCOLORVALUE;
      dcvEmissive: D3DCOLORVALUE;
      dvPower: D3DVALUE;
      );
  end;

  D3DLIGHTTYPE = (
    D3DLIGHT_INVALID_0,
    D3DLIGHT_POINT,
    D3DLIGHT_SPOT,
    D3DLIGHT_DIRECTIONAL,
    D3DLIGHT_PARALLELPOINT,
    D3DLIGHT_GLSPOT );

{ D3DLIGHT structure }

  D3DLIGHT = record
    dwSize: DWORD;
    dltType: D3DLIGHTTYPE;     // Type of light source
    dcvColor: D3DCOLORVALUE;   // Color of light
    dvPosition: D3DVECTOR;     // Position in world space
    dvDirection: D3DVECTOR;    // Direction in world space
    dvRange: D3DVALUE;         // Cutoff range
    dvFalloff: D3DVALUE;       // Falloff
    dvAttenuation0: D3DVALUE;  // Constant attenuation
    dvAttenuation1: D3DVALUE;  // Linear attenuation
    dvAttenuation2: D3DVALUE;  // Quadratic attenuation
    dvTheta: D3DVALUE;         // Inner angle of spotlight cone
    dvPhi: D3DVALUE;           // Outer angle of spotlight cone
  end;

{ Structure defining a light source and its properties. }

{ flags bits }
const
  D3DLIGHT_ACTIVE                       = $00000001;
  D3DLIGHT_NO_SPECULAR                  = $00000002;

type
{ D3DLIGHT2 structure }

  D3DLIGHT2 = record
    dwSize: DWORD;
    dltType: D3DLIGHTTYPE;     // Type of light source
    dcvColor: D3DCOLORVALUE;   // Color of light
    dvPosition: D3DVECTOR;     // Position in world space
    dvDirection: D3DVECTOR;    // Direction in world space
    dvRange: D3DVALUE;         // Cutoff range
    dvFalloff: D3DVALUE;       // Falloff
    dvAttenuation0: D3DVALUE;  // Constant attenuation
    dvAttenuation1: D3DVALUE;  // Linear attenuation
    dvAttenuation2: D3DVALUE;  // Quadratic attenuation
    dvTheta: D3DVALUE;         // Inner angle of spotlight cone
    dvPhi: D3DVALUE;           // Outer angle of spotlight cone
    dwFlags: DWORD;
  end;

{ D3DLIGHTDATA structure }

  D3DLIGHTDATA = record
    dwSize: DWORD;
    lpIn: ^D3DLIGHTINGELEMENT;   // Input positions and normals
    dwInSize: DWORD;             // Stride of input elements
    lpOut: ^D3DTLVERTEX;         // Output colors
    dwOutSize: DWORD;            // Stride of output colors
  end;

(*
 * Before DX5, these values were in an enum called
 * D3DCOLORMODEL. This was not correct, since they are
 * bit flags. A driver can surface either or both flags
 * in the dcmColorModel member of D3DDEVICEDESC.
 *)

type
  D3DCOLORMODEL = DWORD;

const
  D3DCOLOR_INVALID_0 = 0;
  D3DCOLOR_MONO = 1;
  D3DCOLOR_RGB = 2;

{ Options for clearing }

const
  D3DCLEAR_TARGET            = $00000001; // Clear target surface
  D3DCLEAR_ZBUFFER           = $00000002; // Clear target z buffer

{ Supported op codes for execute instructions. }

type
  D3DOPCODE = (
    D3DOP_INVALID_0,
    D3DOP_POINT,
    D3DOP_LINE,
    D3DOP_TRIANGLE,
    D3DOP_MATRIXLOAD,
    D3DOP_MATRIXMULTIPLY,
    D3DOP_STATETRANSFORM,
    D3DOP_STATELIGHT,
    D3DOP_STATERENDER,
    D3DOP_PROCESSVERTICES,
    D3DOP_TEXTURELOAD,
    D3DOP_EXIT,
    D3DOP_BRANCHFORWARD,
    D3DOP_SPAN,
    D3DOP_SETSTATUS );

  D3DINSTRUCTION = record
    bOpcode: BYTE;   // Instruction opcode
    bSize: BYTE;     // Size of each instruction data unit
    wCount: WORD;    // Count of instruction data units to follow
  end;

{ D3DTEXTURELOAD structure }

  D3DTEXTURELOAD = record
    hDestTexture: D3DTEXTUREHANDLE;
    hSrcTexture: D3DTEXTUREHANDLE;
  end;

{ D3DPICKRECORD structure }

  D3DPICKRECORD = record
    bOpcode: BYTE;
    bPad: BYTE;
    dwOffset: DWORD;
    dvZ: D3DVALUE;
  end;

  D3DSHADEMODE = (
    D3DSHADE_INVALID_0,
    D3DSHADE_FLAT,
    D3DSHADE_GOURAUD,
    D3DSHADE_PHONG );

  D3DFILLMODE = (
    D3DFILL_INVALID_0,
    D3DFILL_POINT,
    D3DFILL_WIREFRAME,
    D3DFILL_SOLID );

  D3DLINEPATTERN = record
    wRepeatFactor: WORD;
    wLinePattern: WORD;
  end;

  D3DTEXTUREFILTER = (
    D3DFILTER_INVALID_0,
    D3DFILTER_NEAREST,
    D3DFILTER_LINEAR,
    D3DFILTER_MIPNEAREST,
    D3DFILTER_MIPLINEAR,
    D3DFILTER_LINEARMIPNEAREST,
    D3DFILTER_LINEARMIPLINEAR );

  D3DBLEND = (
    D3DBLEND_INVALID_0,
    D3DBLEND_ZERO,
    D3DBLEND_ONE,
    D3DBLEND_SRCCOLOR,
    D3DBLEND_INVSRCCOLOR,
    D3DBLEND_SRCALPHA,
    D3DBLEND_INVSRCALPHA,
    D3DBLEND_DESTALPHA,
    D3DBLEND_INVDESTALPHA,
    D3DBLEND_DESTCOLOR,
    D3DBLEND_INVDESTCOLOR,
    D3DBLEND_SRCALPHASAT,
    D3DBLEND_BOTHSRCALPHA,
    D3DBLEND_BOTHINVSRCALPHA );

  D3DTEXTUREBLEND = (
    D3DTBLEND_INVALID_0,
    D3DTBLEND_DECAL,
    D3DTBLEND_MODULATE,
    D3DTBLEND_DECALALPHA,
    D3DTBLEND_MODULATEALPHA,
    D3DTBLEND_DECALMASK,
    D3DTBLEND_MODULATEMASK,
    D3DTBLEND_COPY,
    D3DTBLEND_ADD );

  D3DTEXTUREADDRESS = (
    D3DTADDRESS_INVALID_0,
    D3DTADDRESS_WRAP,
    D3DTADDRESS_MIRROR,
    D3DTADDRESS_CLAMP,
    D3DTADDRESS_BORDER );

  D3DCULL = (
    D3DCULL_INVALID_0,
    D3DCULL_NONE,
    D3DCULL_CW,
    D3DCULL_CCW );

  D3DCMPFUNC = (
    D3DCMP_INVALID_0,
    D3DCMP_NEVER,
    D3DCMP_LESS,
    D3DCMP_EQUAL,
    D3DCMP_LESSEQUAL,
    D3DCMP_GREATER,
    D3DCMP_NOTEQUAL,
    D3DCMP_GREATEREQUAL,
    D3DCMP_ALWAYS );

  D3DFOGMODE = (
    D3DFOG_NONE,
    D3DFOG_EXP,
    D3DFOG_EXP2,
    D3DFOG_LINEAR );

type
  D3DANTIALIASMODE = (
    D3DANTIALIAS_NONE,
    D3DANTIALIAS_SORTDEPENDENT,
    D3DANTIALIAS_SORTINDEPENDENT );

  D3DVERTEXTYPE = (
    D3DVT_INVALID_0,
    D3DVT_VERTEX,
    D3DVT_LVERTEX,
    D3DVT_TLVERTEX );

  D3DPRIMITIVETYPE = (
    D3DPT_INVALID_0,
    D3DPT_POINTLIST,
    D3DPT_LINELIST,
    D3DPT_LINESTRIP,
    D3DPT_TRIANGLELIST,
    D3DPT_TRIANGLESTRIP,
    D3DPT_TRIANGLEFAN );

{ Amount to add to a state to generate the override for that state. }

const
  D3DSTATE_OVERRIDE_BIAS          = 256;

{ A state which sets the override flag for the specified state type. }

type
  D3DTRANSFORMSTATETYPE = (
    D3DTRANSFORMSTATE_INVALID_0,
    D3DTRANSFORMSTATE_WORLD,
    D3DTRANSFORMSTATE_VIEW,
    D3DTRANSFORMSTATE_PROJECTION );

  D3DLIGHTSTATETYPE = (
    D3DLIGHTSTATE_INVALID_0,
    D3DLIGHTSTATE_MATERIAL,
    D3DLIGHTSTATE_AMBIENT,
    D3DLIGHTSTATE_COLORMODEL,
    D3DLIGHTSTATE_FOGMODE,
    D3DLIGHTSTATE_FOGSTART,
    D3DLIGHTSTATE_FOGEND,
    D3DLIGHTSTATE_FOGDENSITY );

  D3DRENDERSTATETYPE = (
    D3DRENDERSTATE_INVALID_0,
    D3DRENDERSTATE_TEXTUREHANDLE,       // Texture handle
    D3DRENDERSTATE_ANTIALIAS,           // Antialiasing prim edges
    D3DRENDERSTATE_TEXTUREADDRESS,      // D3DTEXTUREADDRESS
    D3DRENDERSTATE_TEXTUREPERSPECTIVE,  // TRUE for perspective correction
    D3DRENDERSTATE_WRAPU,               // TRUE for wrapping in u
    D3DRENDERSTATE_WRAPV,               // TRUE for wrapping in v
    D3DRENDERSTATE_ZENABLE,             // TRUE to enable z test
    D3DRENDERSTATE_FILLMODE,            // D3DFILL_MODE
    D3DRENDERSTATE_SHADEMODE,           // D3DSHADEMODE
    D3DRENDERSTATE_LINEPATTERN,         // D3DLINEPATTERN
    D3DRENDERSTATE_MONOENABLE,          // TRUE to enable mono rasterization
    D3DRENDERSTATE_ROP2,                // ROP2
    D3DRENDERSTATE_PLANEMASK,           // DWORD physical plane mask
    D3DRENDERSTATE_ZWRITEENABLE,        // TRUE to enable z writes
    D3DRENDERSTATE_ALPHATESTENABLE,     // TRUE to enable alpha tests
    D3DRENDERSTATE_LASTPIXEL,           // TRUE for last-pixel on lines
    D3DRENDERSTATE_TEXTUREMAG,          // D3DTEXTUREFILTER
    D3DRENDERSTATE_TEXTUREMIN,          // D3DTEXTUREFILTER
    D3DRENDERSTATE_SRCBLEND,            // D3DBLEND
    D3DRENDERSTATE_DESTBLEND,           // D3DBLEND
    D3DRENDERSTATE_TEXTUREMAPBLEND,     // D3DTEXTUREBLEND
    D3DRENDERSTATE_CULLMODE,            // D3DCULL
    D3DRENDERSTATE_ZFUNC,               // D3DCMPFUNC
    D3DRENDERSTATE_ALPHAREF,            // D3DFIXED
    D3DRENDERSTATE_ALPHAFUNC,           // D3DCMPFUNC
    D3DRENDERSTATE_DITHERENABLE,        // TRUE to enable dithering
    D3DRENDERSTATE_ALPHABLENDENABLE,    // TRUE to enable alpha blending
    D3DRENDERSTATE_FOGENABLE,           // TRUE to enable fog
    D3DRENDERSTATE_SPECULARENABLE,      // TRUE to enable specular
    D3DRENDERSTATE_ZVISIBLE,            // TRUE to enable z checking
    D3DRENDERSTATE_SUBPIXEL,            // TRUE to enable subpixel correction
    D3DRENDERSTATE_SUBPIXELX,           // TRUE to enable correction in X only
    D3DRENDERSTATE_STIPPLEDALPHA,       // TRUE to enable stippled alpha
    D3DRENDERSTATE_FOGCOLOR,            // D3DCOLOR
    D3DRENDERSTATE_FOGTABLEMODE,        // D3DFOGMODE
    D3DRENDERSTATE_FOGTABLESTART,       // Fog table start
    D3DRENDERSTATE_FOGTABLEEND,         // Fog table end
    D3DRENDERSTATE_FOGTABLEDENSITY,     // Fog table density
    D3DRENDERSTATE_STIPPLEENABLE,       // TRUE to enable stippling
    D3DRENDERSTATE_EDGEANTIALIAS,       // TRUE to enable edge antialiasing
    D3DRENDERSTATE_COLORKEYENABLE,      // TRUE to enable source colorkeyed textures
    D3DRENDERSTATE_BORDERCOLOR,         // Border color for texturing w/border
    D3DRENDERSTATE_TEXTUREADDRESSU,     // Texture addressing mode for U coordinate
    D3DRENDERSTATE_TEXTUREADDRESSV,     // Texture addressing mode for V coordinate
    D3DRENDERSTATE_MIPMAPLODBIAS,       // D3DVALUE Mipmap LOD bias
    D3DRENDERSTATE_ZBIAS,               // LONG Z bias
    D3DRENDERSTATE_RANGEFOGENABLE,      // Enables range-based fog
    D3DRENDERSTATE_ANISOTROPY,          // Max. anisotropy. 1 = no anisotropy
    D3DRENDERSTATE_FLUSHBATCH,          // Explicit flush for DP batching (DX5 Only)
    D3DRENDERSTATE_INVALID_51,
    D3DRENDERSTATE_INVALID_52,
    D3DRENDERSTATE_INVALID_53,
    D3DRENDERSTATE_INVALID_54,
    D3DRENDERSTATE_INVALID_55,
    D3DRENDERSTATE_INVALID_56,
    D3DRENDERSTATE_INVALID_57,
    D3DRENDERSTATE_INVALID_58,
    D3DRENDERSTATE_INVALID_59,
    D3DRENDERSTATE_INVALID_60,
    D3DRENDERSTATE_INVALID_61,
    D3DRENDERSTATE_INVALID_62,
    D3DRENDERSTATE_INVALID_63,
    D3DRENDERSTATE_STIPPLEPATTERN00,   // Stipple pattern 01...
    D3DRENDERSTATE_STIPPLEPATTERN01,
    D3DRENDERSTATE_STIPPLEPATTERN02,
    D3DRENDERSTATE_STIPPLEPATTERN03,
    D3DRENDERSTATE_STIPPLEPATTERN04,
    D3DRENDERSTATE_STIPPLEPATTERN05,
    D3DRENDERSTATE_STIPPLEPATTERN06,
    D3DRENDERSTATE_STIPPLEPATTERN07,
    D3DRENDERSTATE_STIPPLEPATTERN08,
    D3DRENDERSTATE_STIPPLEPATTERN09,
    D3DRENDERSTATE_STIPPLEPATTERN10,
    D3DRENDERSTATE_STIPPLEPATTERN11,
    D3DRENDERSTATE_STIPPLEPATTERN12,
    D3DRENDERSTATE_STIPPLEPATTERN13,
    D3DRENDERSTATE_STIPPLEPATTERN14,
    D3DRENDERSTATE_STIPPLEPATTERN15,
    D3DRENDERSTATE_STIPPLEPATTERN16,
    D3DRENDERSTATE_STIPPLEPATTERN17,
    D3DRENDERSTATE_STIPPLEPATTERN18,
    D3DRENDERSTATE_STIPPLEPATTERN19,
    D3DRENDERSTATE_STIPPLEPATTERN20,
    D3DRENDERSTATE_STIPPLEPATTERN21,
    D3DRENDERSTATE_STIPPLEPATTERN22,
    D3DRENDERSTATE_STIPPLEPATTERN23,
    D3DRENDERSTATE_STIPPLEPATTERN24,
    D3DRENDERSTATE_STIPPLEPATTERN25,
    D3DRENDERSTATE_STIPPLEPATTERN26,
    D3DRENDERSTATE_STIPPLEPATTERN27,
    D3DRENDERSTATE_STIPPLEPATTERN28,
    D3DRENDERSTATE_STIPPLEPATTERN29,
    D3DRENDERSTATE_STIPPLEPATTERN30,
    D3DRENDERSTATE_STIPPLEPATTERN31 );

const
  D3DRENDERSTATE_BLENDENABLE = D3DRENDERSTATE_ALPHABLENDENABLE;

type

  D3DSTATE = record
    case Integer of
    0: (
      dtstTransformStateType: D3DTRANSFORMSTATETYPE;
      dwArg: Array [ 0..0 ] of DWORD;
      );
    1: (
      dlstLightStateType: D3DLIGHTSTATETYPE;
      dvArg: Array [ 0..0 ] of D3DVALUE;
      );
    2: (
      drstRenderStateType: D3DRENDERSTATETYPE;
      );
  end;

{ D3DMATRIXLOAD structure }

  D3DMATRIXLOAD = record
    hDestMatrix: D3DMATRIXHANDLE;   // Destination matrix
    hSrcMatrix: D3DMATRIXHANDLE;    // Source matrix
  end;

{ D3DMATRIXMULTIPLY structure }

  D3DMATRIXMULTIPLY = record
    hDestMatrix: D3DMATRIXHANDLE;   // Destination matrix
    hSrcMatrix1: D3DMATRIXHANDLE;   // First source matrix
    hSrcMatrix2: D3DMATRIXHANDLE;   // Second source matrix
  end;

{ D3DPROCESSVERTICES structure }

  D3DPROCESSVERTICES = record
  dwFlags: DWORD;           // Do we transform or light or just copy?
  wStart: WORD;             // Index to first vertex in source
  wDest: WORD;              // Index to first vertex in local buffer
  dwCount: DWORD;           // Number of vertices to be processed
  dwReserved: DWORD;        // Must be zero
  end;

const
  D3DPROCESSVERTICES_TRANSFORMLIGHT       = $00000000;
  D3DPROCESSVERTICES_TRANSFORM            = $00000001;
  D3DPROCESSVERTICES_COPY                 = $00000002;
  D3DPROCESSVERTICES_OPMASK               = $00000007;

  D3DPROCESSVERTICES_UPDATEEXTENTS        = $00000008;
  D3DPROCESSVERTICES_NOCOLOR              = $00000010;

{ Triangle flags }
  D3DTRIFLAG_START                        = $00000000;

function D3DTRIFLAG_STARTFLAT(len: DWORD) : DWORD;

const
  D3DTRIFLAG_ODD                          = $0000001e;
  D3DTRIFLAG_EVEN                         = $0000001f;

{ Triangle edge flags }

  D3DTRIFLAG_EDGEENABLE1                  = $00000100; // v0-v1 edge
  D3DTRIFLAG_EDGEENABLE2                  = $00000200; // v1-v2 edge
  D3DTRIFLAG_EDGEENABLE3                  = $00000400; // v2-v0 edge
  D3DTRIFLAG_EDGEENABLETRIANGLE =
      D3DTRIFLAG_EDGEENABLE1 or D3DTRIFLAG_EDGEENABLE2 or D3DTRIFLAG_EDGEENABLE3;

type
{ D3DTRIANGLE structure }

  D3DTRIANGLE = record
    case Integer of
    0: (
      v1: WORD;            // Vertex indices
      v2: WORD;
      v3: WORD;
      wFlags: WORD;        // Edge (and other) flags
      );
    1: (
      wV1: WORD;
      wV2: WORD;
      wV3: WORD;
      );
  end;

{ D3DLINE structure }

  D3DLINE = record
    case Integer of
    0: (
      v1: WORD;            // Vertex indices
      v2: WORD;
      );
    1: (
      wV1: WORD;
      wV2: WORD;
      );
  end;

{ D3DSPAN structure }

  D3DSPAN = record
    wCount: WORD;        // Number of spans
    wFirst: WORD;        // Index to first vertex
  end;

{ D3DPOINT structure }

  D3DPOINT = record
    wCount: WORD;        // number of points
    wFirst: WORD;        // index to first vertex
  end;

{ D3DBRANCH structure }

  D3DBRANCH = record
    dwMask: DWORD;         // Bitmask against D3D status
    dwValue: DWORD;
    bNegate: BOOL;         // TRUE to negate comparison
    dwOffset: DWORD;       // How far to branch forward (0 for exit)
  end;

{ D3DSTATUS structure }

  D3DSTATUS = record
    dwFlags: DWORD;        // Do we set extents or status
    dwStatus: DWORD;       // D3D status
    drExtent: D3DRECT;
  end;

const
  D3DSETSTATUS_STATUS   = $00000001;
  D3DSETSTATUS_EXTENTS  = $00000002;
  D3DSETSTATUS_ALL      = D3DSETSTATUS_STATUS or D3DSETSTATUS_EXTENTS;

type
  D3DCLIPSTATUS = record
    dwFlags: DWORD;     // Do we set 2d extents, 3D extents or status
    dwStatus: DWORD;    // Clip status
    minx, maxx: Single; // X extents
    miny, maxy: Single; // Y extents
    minz, maxz: Single; // Z extents
  end;

const
  D3DCLIPSTATUS_STATUS        = $00000001;
  D3DCLIPSTATUS_EXTENTS2      = $00000002;
  D3DCLIPSTATUS_EXTENTS3      = $00000004;

type
{ D3DSTATS structure }

  D3DSTATS = record
    dwSize: DWORD;
    dwTrianglesDrawn: DWORD;
    dwLinesDrawn: DWORD;
    dwPointsDrawn: DWORD;
    dwSpansDrawn: DWORD;
    dwVerticesProcessed: DWORD;
  end;

const
  D3DEXECUTE_CLIPPED       = $00000001;
  D3DEXECUTE_UNCLIPPED     = $00000002;

type
  D3DEXECUTEDATA = record
    dwSize: DWORD;
    dwVertexOffset: DWORD;
    dwVertexCount: DWORD;
    dwInstructionOffset: DWORD;
    dwInstructionLength: DWORD;
    dwHVertexOffset: DWORD;
    dsStatus: D3DSTATUS;       // Status after execute
  end;

{ Palette flags. }

const
  D3DPAL_FREE     = $00;    // Renderer may use this entry freely
  D3DPAL_READONLY = $40;    // Renderer may not set this entry
  D3DPAL_RESERVED = $80;    // Renderer may not use this entry

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3dcaps.h
 *  Content:    Direct3D capabilities include file
 *
 ***************************************************************************)

{ Description of capabilities of transform }

type
  D3DTRANSFORMCAPS = record
    dwSize: DWORD;
    dwCaps: DWORD;
  end;

const
  D3DTRANSFORMCAPS_CLIP           = $00000001; // Will clip whilst transforming

{ Description of capabilities of lighting }

type
  D3DLIGHTINGCAPS = record
    dwSize: DWORD;
    dwCaps: DWORD;                    // Lighting caps
    dwLightingModel: DWORD;           // Lighting model - RGB or mono
    dwNumLights: DWORD;               // Number of lights that can be handled
  end;

const
  D3DLIGHTINGMODEL_RGB            = $00000001;
  D3DLIGHTINGMODEL_MONO           = $00000002;

  D3DLIGHTCAPS_POINT              = $00000001; // Point lights supported
  D3DLIGHTCAPS_SPOT               = $00000002; // Spot lights supported
  D3DLIGHTCAPS_DIRECTIONAL        = $00000004; // Directional lights supported
  D3DLIGHTCAPS_PARALLELPOINT      = $00000008; // Parallel point lights supported
  D3DLIGHTCAPS_GLSPOT             = $00000010; // GL syle spot lights supported

{ Description of capabilities for each primitive type }

type
  D3DPrimCaps = record
    dwSize: DWORD;
    dwMiscCaps: DWORD;                 // Capability flags
    dwRasterCaps: DWORD;
    dwZCmpCaps: DWORD;
    dwSrcBlendCaps: DWORD;
    dwDestBlendCaps: DWORD;
    dwAlphaCmpCaps: DWORD;
    dwShadeCaps: DWORD;
    dwTextureCaps: DWORD;
    dwTextureFilterCaps: DWORD;
    dwTextureBlendCaps: DWORD;
    dwTextureAddressCaps: DWORD;
    dwStippleWidth: DWORD;             // maximum width and height of
    dwStippleHeight: DWORD;            // of supported stipple (up to 32x32)
  end;

const
{ D3DPRIMCAPS dwMiscCaps }

  D3DPMISCCAPS_MASKPLANES         = $00000001;
  D3DPMISCCAPS_MASKZ              = $00000002;
  D3DPMISCCAPS_LINEPATTERNREP     = $00000004;
  D3DPMISCCAPS_CONFORMANT         = $00000008;
  D3DPMISCCAPS_CULLNONE           = $00000010;
  D3DPMISCCAPS_CULLCW             = $00000020;
  D3DPMISCCAPS_CULLCCW            = $00000040;

{ D3DPRIMCAPS dwRasterCaps }

  D3DPRASTERCAPS_DITHER                   = $00000001;
  D3DPRASTERCAPS_ROP2                     = $00000002;
  D3DPRASTERCAPS_XOR                      = $00000004;
  D3DPRASTERCAPS_PAT                      = $00000008;
  D3DPRASTERCAPS_ZTEST                    = $00000010;
  D3DPRASTERCAPS_SUBPIXEL                 = $00000020;
  D3DPRASTERCAPS_SUBPIXELX                = $00000040;
  D3DPRASTERCAPS_FOGVERTEX                = $00000080;
  D3DPRASTERCAPS_FOGTABLE                 = $00000100;
  D3DPRASTERCAPS_STIPPLE                  = $00000200;
  D3DPRASTERCAPS_ANTIALIASSORTDEPENDENT   = $00000400;
  D3DPRASTERCAPS_ANTIALIASSORTINDEPENDENT = $00000800;
  D3DPRASTERCAPS_ANTIALIASEDGES           = $00001000;
  D3DPRASTERCAPS_MIPMAPLODBIAS            = $00002000;
  D3DPRASTERCAPS_ZBIAS                    = $00004000;
  D3DPRASTERCAPS_ZBUFFERLESSHSR           = $00008000;
  D3DPRASTERCAPS_FOGRANGE                 = $00010000;
  D3DPRASTERCAPS_ANISOTROPY               = $00020000;

{ D3DPRIMCAPS dwZCmpCaps, dwAlphaCmpCaps }

  D3DPCMPCAPS_NEVER               = $00000001;
  D3DPCMPCAPS_LESS                = $00000002;
  D3DPCMPCAPS_EQUAL               = $00000004;
  D3DPCMPCAPS_LESSEQUAL           = $00000008;
  D3DPCMPCAPS_GREATER             = $00000010;
  D3DPCMPCAPS_NOTEQUAL            = $00000020;
  D3DPCMPCAPS_GREATEREQUAL        = $00000040;
  D3DPCMPCAPS_ALWAYS              = $00000080;

{ D3DPRIMCAPS dwSourceBlendCaps, dwDestBlendCaps }

  D3DPBLENDCAPS_ZERO              = $00000001;
  D3DPBLENDCAPS_ONE               = $00000002;
  D3DPBLENDCAPS_SRCCOLOR          = $00000004;
  D3DPBLENDCAPS_INVSRCCOLOR       = $00000008;
  D3DPBLENDCAPS_SRCALPHA          = $00000010;
  D3DPBLENDCAPS_INVSRCALPHA       = $00000020;
  D3DPBLENDCAPS_DESTALPHA         = $00000040;
  D3DPBLENDCAPS_INVDESTALPHA      = $00000080;
  D3DPBLENDCAPS_DESTCOLOR         = $00000100;
  D3DPBLENDCAPS_INVDESTCOLOR      = $00000200;
  D3DPBLENDCAPS_SRCALPHASAT       = $00000400;
  D3DPBLENDCAPS_BOTHSRCALPHA      = $00000800;
  D3DPBLENDCAPS_BOTHINVSRCALPHA   = $00001000;

{ D3DPRIMCAPS dwShadeCaps }

  D3DPSHADECAPS_COLORFLATMONO             = $00000001;
  D3DPSHADECAPS_COLORFLATRGB              = $00000002;
  D3DPSHADECAPS_COLORGOURAUDMONO          = $00000004;
  D3DPSHADECAPS_COLORGOURAUDRGB           = $00000008;
  D3DPSHADECAPS_COLORPHONGMONO            = $00000010;
  D3DPSHADECAPS_COLORPHONGRGB             = $00000020;

  D3DPSHADECAPS_SPECULARFLATMONO          = $00000040;
  D3DPSHADECAPS_SPECULARFLATRGB           = $00000080;
  D3DPSHADECAPS_SPECULARGOURAUDMONO       = $00000100;
  D3DPSHADECAPS_SPECULARGOURAUDRGB        = $00000200;
  D3DPSHADECAPS_SPECULARPHONGMONO         = $00000400;
  D3DPSHADECAPS_SPECULARPHONGRGB          = $00000800;

  D3DPSHADECAPS_ALPHAFLATBLEND            = $00001000;
  D3DPSHADECAPS_ALPHAFLATSTIPPLED         = $00002000;
  D3DPSHADECAPS_ALPHAGOURAUDBLEND         = $00004000;
  D3DPSHADECAPS_ALPHAGOURAUDSTIPPLED      = $00008000;
  D3DPSHADECAPS_ALPHAPHONGBLEND           = $00010000;
  D3DPSHADECAPS_ALPHAPHONGSTIPPLED        = $00020000;

  D3DPSHADECAPS_FOGFLAT                   = $00040000;
  D3DPSHADECAPS_FOGGOURAUD                = $00080000;
  D3DPSHADECAPS_FOGPHONG                  = $00100000;

{ D3DPRIMCAPS dwTextureCaps }

  D3DPTEXTURECAPS_PERSPECTIVE     = $00000001;
  D3DPTEXTURECAPS_POW2            = $00000002;
  D3DPTEXTURECAPS_ALPHA           = $00000004;
  D3DPTEXTURECAPS_TRANSPARENCY    = $00000008;
  D3DPTEXTURECAPS_BORDER          = $00000010;
  D3DPTEXTURECAPS_SQUAREONLY      = $00000020;

{ D3DPRIMCAPS dwTextureFilterCaps }

  D3DPTFILTERCAPS_NEAREST         = $00000001;
  D3DPTFILTERCAPS_LINEAR          = $00000002;
  D3DPTFILTERCAPS_MIPNEAREST      = $00000004;
  D3DPTFILTERCAPS_MIPLINEAR       = $00000008;
  D3DPTFILTERCAPS_LINEARMIPNEAREST = $00000010;
  D3DPTFILTERCAPS_LINEARMIPLINEAR = $00000020;

{ D3DPRIMCAPS dwTextureBlendCaps }

  D3DPTBLENDCAPS_DECAL            = $00000001;
  D3DPTBLENDCAPS_MODULATE         = $00000002;
  D3DPTBLENDCAPS_DECALALPHA       = $00000004;
  D3DPTBLENDCAPS_MODULATEALPHA    = $00000008;
  D3DPTBLENDCAPS_DECALMASK        = $00000010;
  D3DPTBLENDCAPS_MODULATEMASK     = $00000020;
  D3DPTBLENDCAPS_COPY             = $00000040;
  D3DPTBLENDCAPS_ADD              = $00000080;

{ D3DPRIMCAPS dwTextureAddressCaps }

  D3DPTADDRESSCAPS_WRAP           = $00000001;
  D3DPTADDRESSCAPS_MIRROR         = $00000002;
  D3DPTADDRESSCAPS_CLAMP          = $00000004;
  D3DPTADDRESSCAPS_BORDER         = $00000008;
  D3DPTADDRESSCAPS_INDEPENDENTUV  = $00000010;

{ Description for a device. }

type
  D3DDeviceDesc = record
    dwSize: DWORD;                       // Size of D3DDEVICEDESC structure
    dwFlags: DWORD;                      // Indicates which fields have valid data
    dcmColorModel: D3DCOLORMODEL;        // Color model of device
    dwDevCaps: DWORD;                    // Capabilities of device
    dtcTransformCaps: D3DTRANSFORMCAPS;  // Capabilities of transform
    bClipping: BOOL;                     // Device can do 3D clipping
    dlcLightingCaps: D3DLIGHTINGCAPS;    // Capabilities of lighting
    dpcLineCaps: D3DPRIMCAPS;
    dpcTriCaps: D3DPRIMCAPS;
    dwDeviceRenderBitDepth: DWORD;       // One of DDBB_8, 16, etc..
    dwDeviceZBufferBitDepth: DWORD;      // One of DDBD_16, 32, etc..
    dwMaxBufferSize: DWORD;              // Maximum execute buffer size
    dwMaxVertexCount: DWORD;             // Maximum vertex count
    // *** New fields for DX5 *** //

    // Width and height caps are 0 for legacy HALs.
    dwMinTextureWidth, dwMinTextureHeight  : DWORD;
    dwMaxTextureWidth, dwMaxTextureHeight  : DWORD;
    dwMinStippleWidth, dwMaxStippleWidth   : DWORD;
    dwMinStippleHeight, dwMaxStippleHeight : DWORD;
  end;

const
  D3DDEVICEDESCSIZE = sizeof(D3DDEVICEDESC);

type
  LPD3DENUMDEVICESCALLBACK = function(const lpGuid: TGUID;
      lpDeviceDescription: LPSTR; lpDeviceName: LPSTR;
      const lpD3DHWDeviceDesc: D3DDEVICEDESC;
      const lpD3DHELDeviceDesc: D3DDEVICEDESC;
      lpUserArg: Pointer): HRESULT; stdcall;

{ D3DDEVICEDESC dwFlags indicating valid fields }

const
  D3DDD_COLORMODEL            = $00000001; // dcmColorModel is valid
  D3DDD_DEVCAPS               = $00000002; // dwDevCaps is valid
  D3DDD_TRANSFORMCAPS         = $00000004; // dtcTransformCaps is valid
  D3DDD_LIGHTINGCAPS          = $00000008; // dlcLightingCaps is valid
  D3DDD_BCLIPPING             = $00000010; // bClipping is valid
  D3DDD_LINECAPS              = $00000020; // dpcLineCaps is valid
  D3DDD_TRICAPS               = $00000040; // dpcTriCaps is valid
  D3DDD_DEVICERENDERBITDEPTH  = $00000080; // dwDeviceRenderBitDepth is valid
  D3DDD_DEVICEZBUFFERBITDEPTH = $00000100; // dwDeviceZBufferBitDepth is valid
  D3DDD_MAXBUFFERSIZE         = $00000200; // dwMaxBufferSize is valid
  D3DDD_MAXVERTEXCOUNT        = $00000400; // dwMaxVertexCount is valid

{ D3DDEVICEDESC dwDevCaps flags }

  D3DDEVCAPS_FLOATTLVERTEX        = $00000001; // Device accepts floating point
                                                    // for post-transform vertex data
  D3DDEVCAPS_SORTINCREASINGZ      = $00000002; // Device needs data sorted for increasing Z
  D3DDEVCAPS_SORTDECREASINGZ      = $00000004; // Device needs data sorted for decreasing Z
  D3DDEVCAPS_SORTEXACT            = $00000008; // Device needs data sorted exactly

  D3DDEVCAPS_EXECUTESYSTEMMEMORY  = $00000010; // Device can use execute buffers from system memory
  D3DDEVCAPS_EXECUTEVIDEOMEMORY   = $00000020; // Device can use execute buffers from video memory
  D3DDEVCAPS_TLVERTEXSYSTEMMEMORY = $00000040; // Device can use TL buffers from system memory
  D3DDEVCAPS_TLVERTEXVIDEOMEMORY  = $00000080; // Device can use TL buffers from video memory
  D3DDEVCAPS_TEXTURESYSTEMMEMORY  = $00000100; // Device can texture from system memory
  D3DDEVCAPS_TEXTUREVIDEOMEMORY   = $00000200; // Device can texture from device memory
  D3DDEVCAPS_DRAWPRIMTLVERTEX     = $00000400; // Device can draw TLVERTEX primitives
  D3DDEVCAPS_CANRENDERAFTERFLIP   = $00000800; // Device can render without waiting for flip to complete
  D3DDEVCAPS_TEXTURENONLOCALVIDMEM = $00001000; // Device can texture from nonlocal video memory

  D3DFDS_COLORMODEL        = $00000001; // Match color model
  D3DFDS_GUID              = $00000002; // Match guid
  D3DFDS_HARDWARE          = $00000004; // Match hardware/software
  D3DFDS_TRIANGLES         = $00000008; // Match in triCaps
  D3DFDS_LINES             = $00000010; // Match in lineCaps
  D3DFDS_MISCCAPS          = $00000020; // Match primCaps.dwMiscCaps
  D3DFDS_RASTERCAPS        = $00000040; // Match primCaps.dwRasterCaps
  D3DFDS_ZCMPCAPS          = $00000080; // Match primCaps.dwZCmpCaps
  D3DFDS_ALPHACMPCAPS      = $00000100; // Match primCaps.dwAlphaCmpCaps
  D3DFDS_SRCBLENDCAPS      = $00000200; // Match primCaps.dwSourceBlendCaps
  D3DFDS_DSTBLENDCAPS      = $00000400; // Match primCaps.dwDestBlendCaps
  D3DFDS_SHADECAPS         = $00000800; // Match primCaps.dwShadeCaps
  D3DFDS_TEXTURECAPS       = $00001000; // Match primCaps.dwTextureCaps
  D3DFDS_TEXTUREFILTERCAPS = $00002000; // Match primCaps.dwTextureFilterCaps
  D3DFDS_TEXTUREBLENDCAPS  = $00004000; // Match primCaps.dwTextureBlendCaps
  D3DFDS_TEXTUREADDRESSCAPS  = $00008000; // Match primCaps.dwTextureBlendCaps

{ FindDevice arguments }

type
  D3DFINDDEVICESEARCH = record
    dwSize: DWORD;
    dwFlags: DWORD;
    bHardware: BOOL;
    dcmColorModel: D3DCOLORMODEL;
    guid: TGUID;
    dwCaps: DWORD;
    dpcPrimCaps: D3DPRIMCAPS;
  end;

  D3DFINDDEVICERESULT = record
    dwSize: DWORD;
    guid: TGUID;               // guid which matched
    ddHwDesc: D3DDEVICEDESC;   // hardware D3DDEVICEDESC
    ddSwDesc: D3DDEVICEDESC;   // software D3DDEVICEDESC
  end;

{ Description of execute buffer. }

  D3DExecuteBufferDesc = record
    dwSize: DWORD;         // size of this structure
    dwFlags: DWORD;        // flags indicating which fields are valid
    dwCaps: DWORD;         // capabilities of execute buffer
    dwBufferSize: DWORD;   // size of execute buffer data
    lpData: Pointer;       // pointer to actual data
  end;

{ D3DEXECUTEBUFFER dwFlags indicating valid fields }

const
  D3DDEB_BUFSIZE          = $00000001;     // buffer size valid
  D3DDEB_CAPS             = $00000002;     // caps valid
  D3DDEB_LPDATA           = $00000004;     // lpData valid

{ D3DEXECUTEBUFFER dwCaps }

  D3DDEBCAPS_SYSTEMMEMORY = $00000001;     // buffer in system memory
  D3DDEBCAPS_VIDEOMEMORY  = $00000002;     // buffer in device memory
  D3DDEBCAPS_MEM          = D3DDEBCAPS_SYSTEMMEMORY or D3DDEBCAPS_VIDEOMEMORY;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3d.h
 *  Content:    Direct3D include file
 *
 ***************************************************************************)

{ Interface IID's }

const
  IID_IDirect3D: TGUID = (D1:$3BBA0080;D2:$2421;D3:$11CF;D4:($A3,$1A,$00,$AA,$00,$B9,$33,$56));
  IID_IDirect3D2: TGUID = (D1:$6aae1ec1;D2:$662a;D3:$11d0;D4:($88,$9d,$00,$aa,$00,$bb,$b7,$6a));

  IID_IDirect3DRampDevice: TGUID = (D1:$F2086B20;D2:$259F;D3:$11CF;D4:($A3,$1A,$00,$AA,$00,$B9,$33,$56));
  IID_IDirect3DRGBDevice: TGUID = (D1:$A4665C60;D2:$2673;D3:$11CF;D4:($A3,$1A,$00,$AA,$00,$B9,$33,$56));
  IID_IDirect3DHALDevice: TGUID = (D1:$84E63dE0;D2:$46AA;D3:$11CF;D4:($81,$6F,$00,$00,$C0,$20,$15,$6E));
  IID_IDirect3DMMXDevice: TGUID = (D1:$881949a1;D2:$d6f3;D3:$11d0;D4:($89,$ab,$00,$a0,$c9,$05,$41,$29));

  IID_IDirect3DDevice: TGUID = (D1:$64108800;D2:$957d;D3:$11d0;D4:($89,$ab,$00,$a0,$c9,$05,$41,$29));
  IID_IDirect3DDevice2: TGUID = (D1:$93281501;D2:$8cf8;D3:$11d0;D4:($89,$ab,$0,$a0,$c9,$5,$41,$29));
  IID_IDirect3DTexture: TGUID = (D1:$2CDCD9E0;D2:$25A0;D3:$11CF;D4:($A3,$1A,$00,$AA,$00,$B9,$33,$56));
  IID_IDirect3DTexture2: TGUID = (D1:$93281502;D2:$8cf8;D3:$11d0;D4:($89, $ab, $0, $a0, $c9, $5, $41, $29));
  IID_IDirect3DLight: TGUID = (D1:$4417C142;D2:$33AD;D3:$11CF;D4:($81,$6F,$00,$00,$C0,$20,$15,$6E));
  IID_IDirect3DMaterial: TGUID = (D1:$4417C144;D2:$33AD;D3:$11CF;D4:($81,$6F,$00,$00,$C0,$20,$15,$6E));
  IID_IDirect3DMaterial2: TGUID = (D1:$93281503;D2:$8cf8;D3:$11d0;D4:($89, $ab, $0, $a0, $c9, $5, $41, $29));
  IID_IDirect3DExecuteBuffer: TGUID = (D1:$4417C145;D2:$33AD;D3:$11CF;D4:($81,$6F,$00,$00,$C0,$20,$15,$6E));
  IID_IDirect3DViewport: TGUID = (D1:$4417C146;D2:$33AD;D3:$11CF;D4:($81,$6F,$00,$00,$C0,$20,$15,$6E));
  IID_IDirect3DViewport2: TGUID = (D1:$93281500;D2:$8cf8;D3:$11d0;D4:($89, $ab, $0, $a0, $c9, $5, $41, $29));

{ Data structures }

type
  IDirect3D = interface;
  IDirect3D2 = interface;
  IDirect3DDevice = interface;
  IDirect3DDevice2 = interface;
  IDirect3DExecuteBuffer = interface;
  IDirect3DLight = interface;
  IDirect3DMaterial = interface;
  IDirect3DMaterial2 = interface;
  IDirect3DTexture = interface;
  IDirect3DTexture2 = interface;
  IDirect3DViewport = interface;
  IDirect3DViewport2 = interface;

  IDirect3D = interface(IUnknown)
    ['{3BBA0080-2421-11CF-A31A-00AA00B93356}']
    (*** IDirect3D methods ***)
    function Initialize(const lpREFIID: TGUID): HRESULT; stdcall;
    function EnumDevices(lpEnumDevicesCallback: LPD3DENUMDEVICESCALLBACK;
        lpUserArg: Pointer): HRESULT; stdcall;
    function CreateLight(out lplpDirect3Dlight: IDirect3DLight;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function CreateMaterial(out lplpDirect3DMaterial: IDirect3DMaterial;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function CreateViewport(out lplpD3DViewport: IDirect3DViewport;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function FindDevice(const lpD3DFDS: D3DFINDDEVICESEARCH;
        var lpD3DFDR: D3DFINDDEVICERESULT): HRESULT; stdcall;
  end;

  IDirect3D2 = interface(IUnknown)
    ['{6AAE1EC1-662A-11D0-889D-00AA00BBB76A}']
    (*** IDirect3D methods ***)
    function EnumDevices(lpEnumDevicesCallback: LPD3DENUMDEVICESCALLBACK;
        lpUserArg: Pointer): HRESULT; stdcall;
    function CreateLight(out lplpDirect3Dlight: IDirect3DLight;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function CreateMaterial(out lplpDirect3DMaterial2: IDirect3DMaterial2;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function CreateViewport(out lplpD3DViewport2: IDirect3DViewport2;
        pUnkOuter: IUnknown ): HRESULT; stdcall;
    function FindDevice(const lpD3DFDS: D3DFINDDEVICESEARCH;
        var lpD3DFDR: D3DFINDDEVICERESULT): HRESULT; stdcall;
    function CreateDevice(const rclsid: TGUID; lpDDS: IDirectDrawSurface;
        out lplpD3DDevice2: IDirect3DDevice2): HRESULT; stdcall;
  end;

  IDirect3DDevice = interface(IUnknown)
    ['{64108800-957D-11D0-89AB-00A0C9054129}']
    (*** IDirect3DDevice methods ***)
    function Initialize(lpd3d: IDirect3D; const lpGUID: TGUID;
        const lpd3ddvdesc: D3DDEVICEDESC): HRESULT; stdcall;
    function GetCaps(var lpD3DHWDevDesc: D3DDEVICEDESC;
        var lpD3DHELDevDesc: D3DDEVICEDESC): HRESULT; stdcall;
    function SwapTextureHandles(lpD3DTex1: IDirect3DTexture;
        lpD3DTex2: IDirect3DTexture): HRESULT; stdcall;
    function CreateExecuteBuffer(const lpDesc: D3DEXECUTEBUFFERDESC;
        out lplpDirect3DExecuteBuffer: IDirect3DExecuteBuffer;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function GetStats(var lpD3DStats: D3DSTATS): HRESULT; stdcall;
    function Execute(lpDirect3DExecuteBuffer: IDirect3DExecuteBuffer;
        lpDirect3DViewport: IDirect3DViewport; dwFlags: DWORD): HRESULT;
        stdcall;
    function AddViewport(lpDirect3DViewport: IDirect3DViewport): HRESULT;
        stdcall;
    function DeleteViewport(lpDirect3DViewport: IDirect3DViewport): HRESULT;
        stdcall;
    function NextViewport(lpDirect3DViewport: IDirect3DViewport;
        out lplpDirect3DViewport: IDirect3DViewport; dwFlags: DWORD):
        HRESULT; stdcall;
    function Pick(lpDirect3DExecuteBuffer: IDirect3DExecuteBuffer;
        lpDirect3DViewport: IDirect3DViewport; dwFlags: DWORD;
        const lpRect: D3DRECT): HRESULT; stdcall;
    function GetPickRecords(var lpCount: DWORD;
        var lpD3DPickRec: D3DPICKRECORD): HRESULT; stdcall;
    function EnumTextureFormats(lpd3dEnumTextureProc:
        LPD3DENUMTEXTUREFORMATSCALLBACK; lpArg: Pointer): HRESULT; stdcall;
    function CreateMatrix(var lpD3DMatHandle: D3DMATRIXHANDLE): HRESULT;
        stdcall;
    function SetMatrix(d3dMatHandle: D3DMATRIXHANDLE;
        const lpD3DMatrix: D3DMATRIX): HRESULT; stdcall;
    function GetMatrix(lpD3DMatHandle: D3DMATRIXHANDLE;
        var lpD3DMatrix: D3DMATRIX): HRESULT; stdcall;
    function DeleteMatrix(d3dMatHandle: D3DMATRIXHANDLE): HRESULT; stdcall;
    function BeginScene: HRESULT; stdcall;
    function EndScene: HRESULT; stdcall;
    function GetDirect3D(out lpD3D: IDirect3D): HRESULT; stdcall;
  end;

  IDirect3DDevice2 = interface(IUnknown)
    ['{93281501-8CF8-11D0-89AB-00A0C9054129}']
    (*** IDirect3DDevice2 methods ***)
    function GetCaps(var lpD3DHWDevDesc: D3DDEVICEDESC;
        var lpD3DHELDevDesc: D3DDEVICEDESC): HRESULT; stdcall;
    function SwapTextureHandles(lpD3DTex1: IDirect3DTexture2;
        lpD3DTex2: IDirect3DTexture2): HRESULT; stdcall;
    function GetStats(var lpD3DStats: D3DSTATS): HRESULT; stdcall;
    function AddViewport(lpDirect3DViewport2: IDirect3DViewport2): HRESULT;
        stdcall;
    function DeleteViewport(lpDirect3DViewport: IDirect3DViewport2): HRESULT;
        stdcall;
    function NextViewport(lpDirect3DViewport: IDirect3DViewport2;
        out lplpDirect3DViewport: IDirect3DViewport2; dwFlags: DWORD): HRESULT;
        stdcall;
    function EnumTextureFormats(lpd3dEnumTextureProc:
        LPD3DENUMTEXTUREFORMATSCALLBACK; lpArg: Pointer): HRESULT; stdcall;
    function BeginScene: HRESULT; stdcall;
    function EndScene: HRESULT; stdcall;
    function GetDirect3D(out lpD3D: IDirect3D2): HRESULT; stdcall;
    (*** DrawPrimitive API ***)
    function SetCurrentViewport(lpd3dViewport2: IDirect3DViewport2): HRESULT;
        stdcall;
    function GetCurrentViewport(out lplpd3dViewport2: IDirect3DViewport2):
        HRESULT; stdcall;
    function SetRenderTarget(lpNewRenderTarget: IDirectDrawSurface): HRESULT;
        stdcall;
    function GetRenderTarget(out lplpNewRenderTarget: IDirectDrawSurface):
        HRESULT; stdcall;
    function Begin_(d3dpt: D3DPRIMITIVETYPE; d3dvt: D3DVERTEXTYPE;
        dwFlags: DWORD): HRESULT; stdcall;
    function BeginIndexed(dptPrimitiveType: D3DPRIMITIVETYPE; dvtVertexType:
        D3DVERTEXTYPE; const lpvVertices; dwNumVertices: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function Vertex(const lpVertexType): HRESULT; stdcall;
    function Index(wVertexIndex: WORD): HRESULT; stdcall;
    function End_(dwFlags: DWORD): HRESULT; stdcall;
    function GetRenderState(dwRenderStateType: D3DRENDERSTATETYPE;
        var lpdwRenderState: DWORD): HRESULT; stdcall;
    function SetRenderState(dwRenderStateType: D3DRENDERSTATETYPE;
        dwRenderState: DWORD): HRESULT; stdcall;
    function GetLightState(dwLightStateType: D3DLIGHTSTATETYPE;
        var lpdwLightState: DWORD): HRESULT; stdcall;
    function SetLightState(dwLightStateType: D3DLIGHTSTATETYPE;
        dwLightState: DWORD): HRESULT; stdcall;
    function SetTransform(dtstTransformStateType: D3DTRANSFORMSTATETYPE;
        const lpD3DMatrix: D3DMATRIX): HRESULT; stdcall;
    function GetTransform(dtstTransformStateType: D3DTRANSFORMSTATETYPE;
        var lpD3DMatrix: D3DMATRIX): HRESULT; stdcall;
    function MultiplyTransform(dtstTransformStateType: D3DTRANSFORMSTATETYPE;
        var lpD3DMatrix: D3DMATRIX): HRESULT; stdcall;
    function DrawPrimitive(dptPrimitiveType: D3DPRIMITIVETYPE;
        dvtVertexType: D3DVERTEXTYPE; const lpvVertices; dwVertexCount,
        dwFlags: DWORD): HRESULT; stdcall;
    function DrawIndexedPrimitive(dptPrimitiveType: D3DPRIMITIVETYPE;
        dvtVertexType: D3DVERTEXTYPE; const lpvVertices;
        dwVertexCount: DWORD; const dwIndices; dwIndexCount: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function SetClipStatus (const lpD3DClipStatus: D3DCLIPSTATUS): HRESULT;
        stdcall;
    function GetClipStatus (var lpD3DClipStatus: D3DCLIPSTATUS): HRESULT;
        stdcall;
  end;

  IDirect3DExecuteBuffer = interface(IUnknown)
    ['{4417C145-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DExecuteBuffer methods ***)
    function Initialize(lpDirect3DDevice: IDirect3DDevice;
        const lpDesc: D3DEXECUTEBUFFERDESC): HRESULT; stdcall;
    function Lock(var lpDesc: D3DEXECUTEBUFFERDESC): HRESULT; stdcall;
    function Unlock: HRESULT; stdcall;
    function SetExecuteData(const lpData: D3DEXECUTEDATA): HRESULT; stdcall;
    function GetExecuteData(var lpData: D3DEXECUTEDATA): HRESULT; stdcall;
    function Validate(var lpdwOffset: DWORD; lpFunc: LPD3DVALIDATECALLBACK;
        lpUserArg: Pointer; dwReserved: DWORD): HRESULT; stdcall;
    function Optimize(dwFlags: DWORD): HRESULT; stdcall;
  end;

  IDirect3DLight = interface(IUnknown)
    ['{4417C142-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DLight methods ***)
    function Initialize(lpDirect3D: IDirect3D): HRESULT; stdcall;
    function SetLight(const lpLight: D3DLIGHT): HRESULT; stdcall;
    function GetLight(var lpLight: D3DLIGHT): HRESULT; stdcall;
  end;

  IDirect3DMaterial = interface(IUnknown)
    ['{4417C144-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DMaterial methods ***)
    function Initialize(lpDirect3D: IDirect3D): HRESULT; stdcall;
    function SetMaterial(const lpMat: D3DMATERIAL): HRESULT; stdcall;
    function GetMaterial(var lpMat: D3DMATERIAL): HRESULT; stdcall;
    function GetHandle(lpDirect3DDevice: IDirect3DDevice;
        var lpHandle: D3DMATERIALHANDLE): HRESULT; stdcall;
    function Reserve: HRESULT; stdcall;
    function Unreserve: HRESULT; stdcall;
  end;

  IDirect3DMaterial2 = interface(IUnknown)
    ['{93281503-8CF8-11D0-89AB-00A0C9054129}']
    (*** IDirect3DMaterial2 methods ***)
    function SetMaterial(const lpMat: D3DMATERIAL): HRESULT; stdcall;
    function GetMaterial(var lpMat: D3DMATERIAL): HRESULT; stdcall;
    function GetHandle(lpDirect3DDevice: IDirect3DDevice2;
        var lpHandle: D3DMATERIALHANDLE): HRESULT; stdcall;
  end;

  IDirect3DTexture = interface(IUnknown)
    ['{2CDCD9E0-25A0-11CF-A31A-00AA00B93356}']
    (*** IDirect3DTexture methods ***)
    function Initialize(lpD3DDevice: IDirect3DDevice;
        lpDDSurface: IDirectDrawSurface): HRESULT; stdcall;
    function GetHandle(lpDirect3DDevice: IDirect3DDevice;
        var lpHandle: D3DTEXTUREHANDLE): HRESULT; stdcall;
    function PaletteChanged(dwStart: DWORD; dwCount: DWORD): HRESULT; stdcall;
    function Load(lpD3DTexture: IDirect3DTexture): HRESULT; stdcall;
    function Unload: HRESULT; stdcall;
  end;

  IDirect3DTexture2 = interface(IUnknown)
    ['{93281502-8CF8-11D0-89AB-00A0C9054129}']
    (*** IDirect3DTexture2 methods ***)
    function GetHandle(lpDirect3DDevice2: IDirect3DDevice2;
        var lpHandle: D3DTEXTUREHANDLE): HRESULT; stdcall;
    function PaletteChanged(dwStart: DWORD; dwCount: DWORD): HRESULT; stdcall;
    function Load(lpD3DTexture2: IDirect3DTexture2): HRESULT; stdcall;
  end;

  IDirect3DViewport = interface(IUnknown)
    ['{4417C146-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DViewport methods ***)
    function Initialize(lpDirect3D: IDirect3D): HRESULT; stdcall;
    function GetViewport(var lpData: D3DVIEWPORT): HRESULT; stdcall;
    function SetViewport(const lpData: D3DVIEWPORT): HRESULT; stdcall;
    function TransformVertices(dwVertexCount: DWORD;
        var lpData: D3DTRANSFORMDATA; dwFlags: DWORD;
        var lpOffscreen: DWORD): HRESULT; stdcall;
    function LightElements(dwElementCount: DWORD;
        var lpData: D3DLIGHTDATA): HRESULT; stdcall;
    function SetBackground(hMat: D3DMATERIALHANDLE): HRESULT; stdcall;
    function GetBackground(hMat: D3DMATERIALHANDLE): HRESULT; stdcall;
    function SetBackgroundDepth(lpDDSurface: IDirectDrawSurface): HRESULT;
        stdcall;
    function GetBackgroundDepth(out lplpDDSurface: IDirectDrawSurface;
        var lpValid: BOOL): HRESULT; stdcall;
    function Clear(dwCount: DWORD; const lpRects: D3DRECT; dwFlags: DWORD):
        HRESULT; stdcall;
    function AddLight(lpDirect3DLight: IDirect3DLight): HRESULT; stdcall;
    function DeleteLight(lpDirect3DLight: IDirect3DLight): HRESULT; stdcall;
    function NextLight(lpDirect3DLight: IDirect3DLight;
        out lplpDirect3DLight: IDirect3DLight; dwFlags: DWORD): HRESULT;
        stdcall;
  end;

  IDirect3DViewport2 = interface(IDirect3DViewport)
    ['{93281500-8CF8-11D0-89AB-00A0C9054129}']
    (*** IDirect3DViewport2 methods ***)
    function GetViewport2(var lpData: D3DVIEWPORT2): HRESULT; stdcall;
    function SetViewport2(const lpData: D3DVIEWPORT2): HRESULT; stdcall;
  end;

const

{ Flags for IDirect3DDevice::NextViewport }

  D3DNEXT_NEXT =        $00000001;
  D3DNEXT_HEAD =        $00000002;
  D3DNEXT_TAIL =        $00000004;

{ Flags for DrawPrimitive/DrawIndexedPrimitive
   Also valid for Begin/BeginIndexed }

  D3DDP_WAIT = $00000001;
  D3DDP_DONOTCLIP = $00000004;
  D3DDP_DONOTUPDATEEXTENTS = $00000008;

{ Direct3D Errors }

const
  D3D_OK                          = DD_OK;
  D3DERR_BADMAJORVERSION          = $88760000 + 700;
  D3DERR_BADMINORVERSION          = $88760000 + 701;

{ An invalid device was requested by the application. }

  D3DERR_INVALID_DEVICE   = $88760000 + 705;
  D3DERR_INITFAILED       = $88760000 + 706;

{ SetRenderTarget attempted on a device that was
  QI'd off the render target. }

  D3DERR_DEVICEAGGREGATED = $88760000 + 707;

  D3DERR_EXECUTE_CREATE_FAILED    = $88760000 + 710;
  D3DERR_EXECUTE_DESTROY_FAILED   = $88760000 + 711;
  D3DERR_EXECUTE_LOCK_FAILED      = $88760000 + 712;
  D3DERR_EXECUTE_UNLOCK_FAILED    = $88760000 + 713;
  D3DERR_EXECUTE_LOCKED           = $88760000 + 714;
  D3DERR_EXECUTE_NOT_LOCKED       = $88760000 + 715;

  D3DERR_EXECUTE_FAILED           = $88760000 + 716;
  D3DERR_EXECUTE_CLIPPED_FAILED   = $88760000 + 717;

  D3DERR_TEXTURE_NO_SUPPORT       = $88760000 + 720;
  D3DERR_TEXTURE_CREATE_FAILED    = $88760000 + 721;
  D3DERR_TEXTURE_DESTROY_FAILED   = $88760000 + 722;
  D3DERR_TEXTURE_LOCK_FAILED      = $88760000 + 723;
  D3DERR_TEXTURE_UNLOCK_FAILED    = $88760000 + 724;
  D3DERR_TEXTURE_LOAD_FAILED      = $88760000 + 725;
  D3DERR_TEXTURE_SWAP_FAILED      = $88760000 + 726;
  D3DERR_TEXTURE_LOCKED           = $88760000 + 727;
  D3DERR_TEXTURE_NOT_LOCKED       = $88760000 + 728;
  D3DERR_TEXTURE_GETSURF_FAILED   = $88760000 + 729;

  D3DERR_MATRIX_CREATE_FAILED     = $88760000 + 730;
  D3DERR_MATRIX_DESTROY_FAILED    = $88760000 + 731;
  D3DERR_MATRIX_SETDATA_FAILED    = $88760000 + 732;
  D3DERR_MATRIX_GETDATA_FAILED    = $88760000 + 733;
  D3DERR_SETVIEWPORTDATA_FAILED   = $88760000 + 734;

  D3DERR_INVALIDCURRENTVIEWPORT   = $88760000 + 735;
  D3DERR_INVALIDPRIMITIVETYPE     = $88760000 + 736;
  D3DERR_INVALIDVERTEXTYPE        = $88760000 + 737;
  D3DERR_TEXTURE_BADSIZE          = $88760000 + 738;
  D3DERR_INVALIDRAMPTEXTURE       = $88760000 + 739;

  D3DERR_MATERIAL_CREATE_FAILED   = $88760000 + 740;
  D3DERR_MATERIAL_DESTROY_FAILED  = $88760000 + 741;
  D3DERR_MATERIAL_SETDATA_FAILED  = $88760000 + 742;
  D3DERR_MATERIAL_GETDATA_FAILED  = $88760000 + 743;
  D3DERR_INVALIDPALETTE           = $88760000 + 744;

  D3DERR_ZBUFF_NEEDS_SYSTEMMEMORY = $88760000 + 745;
  D3DERR_ZBUFF_NEEDS_VIDEOMEMORY  = $88760000 + 746;
  D3DERR_SURFACENOTINVIDMEM       = $88760000 + 747;

  D3DERR_LIGHT_SET_FAILED         = $88760000 + 750;
  D3DERR_LIGHTHASVIEWPORT         = $88760000 + 751;
  D3DERR_LIGHTNOTINTHISVIEWPORT   = $88760000 + 752;

  D3DERR_SCENE_IN_SCENE           = $88760000 + 760;
  D3DERR_SCENE_NOT_IN_SCENE       = $88760000 + 761;
  D3DERR_SCENE_BEGIN_FAILED       = $88760000 + 762;
  D3DERR_SCENE_END_FAILED         = $88760000 + 763;

  D3DERR_INBEGIN                  = $88760000 + 770;
  D3DERR_NOTINBEGIN               = $88760000 + 771;
  D3DERR_NOVIEWPORTS              = $88760000 + 772;
  D3DERR_VIEWPORTDATANOTSET       = $88760000 + 773;
  D3DERR_VIEWPORTHASNODEVICE      = $88760000 + 774;
  D3DERR_NOCURRENTVIEWPORT        = $88760000 + 775;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drm.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

type
  D3DRMVECTOR4D = record
    x, y, z, w: D3DVALUE;
  end;
  LPD3DRMVECTOR4D = ^D3DRMVECTOR4D;

  D3DRMMATRIX4D = array[0..3, 0..3] of D3DVALUE;

  D3DRMQUATERNION = record
    s: D3DVALUE;
    v: D3DVECTOR;
  end;
  LPD3DRMQUATERNION = ^D3DRMQUATERNION;

  D3DRMRAY = record
    dvDir: D3DVECTOR;
    dvPos: D3DVECTOR;
  end;

  D3DRMBOX = record
    min, max: D3DVECTOR;
  end;
  LPD3DRMBOX = ^D3DRMBOX;

  D3DRMWRAPCALLBACK = procedure(var lpD3DVector: D3DVECTOR; var lpU: Integer;
      var lpV: Integer; var lpD3DRMVA: D3DVECTOR; lpD3DRMVB: D3DVECTOR;
      lpArg: Pointer); stdcall;

  D3DRMLIGHTTYPE = (
    D3DRMLIGHT_AMBIENT,
    D3DRMLIGHT_POINT,
    D3DRMLIGHT_SPOT,
    D3DRMLIGHT_DIRECTIONAL,
    D3DRMLIGHT_PARALLELPOINT
  );

  D3DRMSHADEMODE = WORD;

const
  D3DRMSHADE_FLAT = 0;
  D3DRMSHADE_GOURAUD = 1;
  D3DRMSHADE_PHONG = 2;
  D3DRMSHADE_MASK = 7;
  D3DRMSHADE_MAX = 8;

type
  D3DRMLIGHTMODE = WORD;

const
  D3DRMLIGHT_OFF  = 0 * D3DRMSHADE_MAX;
  D3DRMLIGHT_ON   = 1 * D3DRMSHADE_MAX;
  D3DRMLIGHT_MASK = 7 * D3DRMSHADE_MAX;
  D3DRMLIGHT_MAX  = 8 * D3DRMSHADE_MAX;

type
  D3DRMFILLMODE = WORD;

const
  D3DRMFILL_POINTS    = 0 * D3DRMLIGHT_MAX;
  D3DRMFILL_WIREFRAME = 1 * D3DRMLIGHT_MAX;
  D3DRMFILL_SOLID     = 2 * D3DRMLIGHT_MAX;
  D3DRMFILL_MASK      = 7 * D3DRMLIGHT_MAX;
  D3DRMFILL_MAX       = 8 * D3DRMLIGHT_MAX;

type
  D3DRMRENDERQUALITY = DWORD;

const
  D3DRMRENDER_WIREFRAME   = D3DRMSHADE_FLAT + D3DRMLIGHT_OFF + D3DRMFILL_WIREFRAME;
  D3DRMRENDER_UNLITFLAT   = D3DRMSHADE_FLAT + D3DRMLIGHT_OFF + D3DRMFILL_SOLID;
  D3DRMRENDER_FLAT        = D3DRMSHADE_FLAT + D3DRMLIGHT_ON + D3DRMFILL_SOLID;
  D3DRMRENDER_GOURAUD     = D3DRMSHADE_GOURAUD + D3DRMLIGHT_ON + D3DRMFILL_SOLID;
  D3DRMRENDER_PHONG       = D3DRMSHADE_PHONG + D3DRMLIGHT_ON + D3DRMFILL_SOLID;

  D3DRMRENDERMODE_BLENDEDTRANSPARENCY   = 1;
  D3DRMRENDERMODE_SORTEDTRANSPARENCY    = 2;

type
  D3DRMTEXTUREQUALITY = (
    D3DRMTEXTURE_NEAREST,               // choose nearest texel
    D3DRMTEXTURE_LINEAR,                // interpolate 4 texels
    D3DRMTEXTURE_MIPNEAREST,            // nearest texel in nearest mipmap
    D3DRMTEXTURE_MIPLINEAR,             // interpolate 2 texels from 2 mipmaps
    D3DRMTEXTURE_LINEARMIPNEAREST,      // interpolate 4 texels in nearest mipmap
    D3DRMTEXTURE_LINEARMIPLINEAR        // interpolate 8 texels from 2 mipmaps
  );

  D3DRMCOMBINETYPE = (
     D3DRMCOMBINE_REPLACE,
     D3DRMCOMBINE_BEFORE,
     D3DRMCOMBINE_AFTER
  );

  D3DRMCOLORMODEL = D3DCOLORMODEL;
  LPD3DRMCOLORMODEL = ^D3DRMCOLORMODEL;

  D3DRMPALETTEFLAGS = (
    D3DRMPALETTE_FREE,                  // renderer may use this entry freely
    D3DRMPALETTE_READONLY,              // fixed but may be used by renderer
    D3DRMPALETTE_RESERVED               // may not be used by renderer
  );

  D3DRMPALETTEENTRY = record
    red: Byte;          // 0 .. 255
    green: Byte;        // 0 .. 255
    blue: Byte;         // 0 .. 255
    flags: Byte;        // one of D3DRMPALETTEFLAGS
  end;
  LPD3DRMPALETTEENTRY = ^D3DRMPALETTEENTRY;

  D3DRMIMAGE = record
    width, height: Integer;    (* width and height in pixels *)
    aspectx, aspecty: Integer; (* aspect ratio for non-square pixels *)
    depth: Integer;            (* bits per pixel *)
    rgb: Integer;              (* if false, pixels are indices into a
                                   palette otherwise, pixels encode
                                   RGB values. *)
    bytes_per_line: Integer;   (* number of bytes of memory for a
                                   scanline. This must be a multiple
                                   of 4. *)
    buffer1: Pointer;          (* memory to render into (first buffer). *)
    buffer2: Pointer;          (* second rendering buffer for double
                                   buffering, set to NULL for single
                                   buffering. *)
    red_mask: Longint;
    green_mask: Longint;
    blue_mask: Longint;
    alpha_mask: Longint;       (* if rgb is true, these are masks for
                                   the red, green and blue parts of a
                                   pixel.  Otherwise, these are masks
                                   for the significant bits of the
                                   red, green and blue elements in the
                                   palette.  For instance, most SVGA
                                   displays use 64 intensities of red,
                                   green and blue, so the masks should
                                   all be set to = $fc. *)
    palette_size: Integer;     (* number of entries in palette *)
    palette: ^D3DRMPALETTEENTRY; (* description of the palette (only if
                                   rgb is false).  Must be (1<<depth)
                                   elements. *)
  end;
  LPD3DRMIMAGE = ^D3DRMIMAGE;

  D3DRMWRAPTYPE = (
    D3DRMWRAP_FLAT,
    D3DRMWRAP_CYLINDER,
    D3DRMWRAP_SPHERE,
    D3DRMWRAP_CHROME
  );

const
  D3DRMWIREFRAME_CULL             = 1; // cull backfaces
  D3DRMWIREFRAME_HIDDENLINE       = 2; // lines are obscured by closer objects

type
  D3DRMPROJECTIONTYPE = (
    D3DRMPROJECT_PERSPECTIVE,
    D3DRMPROJECT_ORTHOGRAPHIC,
    D3DRMPROJECT_RIGHTHANDPERSPECTIVE,
    D3DRMPROJECT_RIGHTHANDORTHOGRAPHIC
  );

  D3DRMXOFFORMAT = (
    D3DRMXOF_BINARY,
    D3DRMXOF_COMPRESSED,
    D3DRMXOF_TEXT
  );

  D3DRMSAVEOPTIONS = DWORD;

const
  D3DRMXOFSAVE_NORMALS = 1;
  D3DRMXOFSAVE_TEXTURECOORDINATES = 2;
  D3DRMXOFSAVE_MATERIALS = 4;
  D3DRMXOFSAVE_TEXTURENAMES = 8;
  D3DRMXOFSAVE_ALL = 15;
  D3DRMXOFSAVE_TEMPLATES = 16;
  D3DRMXOFSAVE_TEXTURETOPOLOGY = 32;

type
  D3DRMCOLORSOURCE = (
    D3DRMCOLOR_FROMFACE,
    D3DRMCOLOR_FROMVERTEX
  );

  D3DRMFRAMECONSTRAINT = (
    D3DRMCONSTRAIN_Z,           // use only X and Y rotations
    D3DRMCONSTRAIN_Y,           // use only X and Z rotations
    D3DRMCONSTRAIN_X            // use only Y and Z rotations
  );

  D3DRMMATERIALMODE = (
    D3DRMMATERIAL_FROMMESH,
    D3DRMMATERIAL_FROMPARENT,
    D3DRMMATERIAL_FROMFRAME
  );

  D3DRMFOGMODE = (
    D3DRMFOG_LINEAR,            // linear between start and end
    D3DRMFOG_EXPONENTIAL,       // density * exp(-distance)
    D3DRMFOG_EXPONENTIALSQUARED // density * exp(-distance*distance)
  );

  D3DRMZBUFFERMODE = (
    D3DRMZBUFFER_FROMPARENT,    // default
    D3DRMZBUFFER_ENABLE,        // enable zbuffering
    D3DRMZBUFFER_DISABLE        // disable zbuffering
  );

  D3DRMSORTMODE = (
    D3DRMSORT_FROMPARENT,       // default
    D3DRMSORT_NONE,             // don't sort child frames
    D3DRMSORT_FRONTTOBACK,      // sort child frames front-to-back
    D3DRMSORT_BACKTOFRONT       // sort child frames back-to-front
  );

const
{ Values for flags in Frame2::AddMoveCallback. }

  D3DRMCALLBACK_PREORDER                = 0;
  D3DRMCALLBACK_POSTORDER               = 1;

{ Values for flags in MeshBuilder2::RayPick. }

  D3DRMRAYPICK_ONLYBOUNDINGBOXES        = 1;
  D3DRMRAYPICK_IGNOREFURTHERPRIMITIVES  = 2;
  D3DRMRAYPICK_INTERPOLATEUV            = 4;
  D3DRMRAYPICK_INTERPOLATECOLOR         = 8;
  D3DRMRAYPICK_INTERPOLATENORMAL        = $10;

{ Values for flags in MeshBuilder2::GenerateNormals. }

  D3DRMGENERATENORMALS_PRECOMPACT       = 1;
  D3DRMGENERATENORMALS_USECREASEANGLE   = 2;

type
  D3DRMANIMATIONOPTIONS = DWORD;

const
  D3DRMANIMATION_OPEN = $01;
  D3DRMANIMATION_CLOSED = $02;
  D3DRMANIMATION_LINEARPOSITION = $04;
  D3DRMANIMATION_SPLINEPOSITION = $08;
  D3DRMANIMATION_SCALEANDROTATION = $00000010;
  D3DRMANIMATION_POSITION = $00000020;

type
  D3DRMINTERPOLATIONOPTIONS = DWORD;

const
  D3DRMINTERPOLATION_OPEN = $01;
  D3DRMINTERPOLATION_CLOSED = $02;
  D3DRMINTERPOLATION_NEAREST = $0100;
  D3DRMINTERPOLATION_LINEAR = $04;
  D3DRMINTERPOLATION_SPLINE = $08;
  D3DRMINTERPOLATION_VERTEXCOLOR = $40;
  D3DRMINTERPOLATION_SLERPNORMALS = $80;

type
  D3DRMLOADOPTIONS = DWORD;

const
  D3DRMLOAD_FROMFILE  = $00;
  D3DRMLOAD_FROMRESOURCE = $01;
  D3DRMLOAD_FROMMEMORY = $02;
  D3DRMLOAD_FROMSTREAM = $04;
  D3DRMLOAD_FROMURL = $08;

  D3DRMLOAD_BYNAME = $10;
  D3DRMLOAD_BYPOSITION = $20;
  D3DRMLOAD_BYGUID = $40;
  D3DRMLOAD_FIRST = $80;

  D3DRMLOAD_INSTANCEBYREFERENCE = $100;
  D3DRMLOAD_INSTANCEBYCOPYING = $200;

  D3DRMLOAD_ASYNCHRONOUS = $400;

type
  D3DRMLOADRESOURCE = record
    hModule: HMODULE;
    lpName: PChar;
    lpType: PChar;
  end;
  LPD3DRMLOADRESOURCE = ^D3DRMLOADRESOURCE;

  D3DRMLOADMEMORY = record
    lpMemory: Pointer;
    dSize: DWORD;
  end;
  LPD3DRMLOADMEMORY = ^D3DRMLOADMEMORY;

const
  D3DRMPMESHSTATUS_VALID = $01;
  D3DRMPMESHSTATUS_INTERRUPTED = $02;
  D3DRMPMESHSTATUS_BASEMESHCOMPLETE = $04;
  D3DRMPMESHSTATUS_COMPLETE = $08;
  D3DRMPMESHSTATUS_RENDERABLE = $10;

  D3DRMPMESHEVENT_BASEMESH = $01;
  D3DRMPMESHEVENT_COMPLETE = $02;

type
  D3DRMPMESHLOADSTATUS = record
    dwSize: DWORD;           // Size of this structure
    dwPMeshSize: DWORD;      // Total Size (bytes)
    dwBaseMeshSize: DWORD;   // Total Size of the Base Mesh
    dwBytesLoaded: DWORD;    // Total bytes loaded
    dwVerticesLoaded: DWORD; // Number of vertices loaded
    dwFacesLoaded: DWORD;    // Number of faces loaded
    dwLoadResult: HRESULT;   // Result of the load operation
    dwFlags: DWORD;
  end;
  LPD3DRMPMESHLOADSTATUS = ^D3DRMPMESHLOADSTATUS;

  D3DRMUSERVISUALREASON = (
    D3DRMUSERVISUAL_CANSEE,
    D3DRMUSERVISUAL_RENDER
  );

  D3DRMMAPPING = DWORD;
  D3DRMMAPPINGFLAG = DWORD;
  LPD3DRMMAPPING = ^DWORD;

const
  D3DRMMAP_WRAPU = 1;
  D3DRMMAP_WRAPV = 2;
  D3DRMMAP_PERSPCORRECT = 4;

type
  D3DRMVERTEX = record
    position: D3DVECTOR;
    normal: D3DVECTOR;
    tu, tv: D3DVALUE;
    color: D3DCOLOR;
  end;
  LPD3DRMVERTEX = ^D3DRMVERTEX;

  D3DRMGROUPINDEX = Longint; // group indexes begin a 0

const
  D3DRMGROUP_ALLGROUPS = -1;

function D3DRMCreateColorRGB(red, green, blue: D3DVALUE): D3DCOLOR; stdcall;
function D3DRMCreateColorRGBA(red, green, blue, alpha: D3DVALUE): D3DCOLOR; stdcall;
function D3DRMColorGetRed(d3drmc: D3DCOLOR): D3DVALUE; stdcall;
function D3DRMColorGetGreen(d3drmc: D3DCOLOR): D3DVALUE; stdcall;
function D3DRMColorGetBlue(d3drmc: D3DCOLOR): D3DVALUE; stdcall;
function D3DRMColorGetAlpha(d3drmc: D3DCOLOR): D3DVALUE; stdcall;
function D3DRMVectorAdd(var d, s1, s2: D3DVECTOR): LPD3DVECTOR; stdcall;
function D3DRMVectorSubtract(var d, s1, s2: D3DVECTOR): LPD3DVECTOR; stdcall;
function D3DRMVectorReflect(var d, ray, norm: D3DVECTOR): LPD3DVECTOR; stdcall;
function D3DRMVectorCrossProduct(var d, s1, s2: D3DVECTOR): LPD3DVECTOR; stdcall;
function D3DRMVectorDotProduct(var s1, s2: D3DVECTOR): D3DVALUE; stdcall;
function D3DRMVectorNormalize(var lpv: D3DVECTOR): LPD3DVECTOR; stdcall;
function D3DRMVectorModulus(var v: D3DVECTOR): D3DVALUE; stdcall;
function D3DRMVectorRotate(var r, v, axis: D3DVECTOR; theta: D3DVALUE):
    LPD3DVECTOR; stdcall;
function D3DRMVectorScale( var d, s: D3DVECTOR; factor: D3DVALUE):
    LPD3DVECTOR; stdcall;
function D3DRMVectorRandom(var d: D3DVECTOR): LPD3DVECTOR; stdcall;
function D3DRMQuaternionFromRotation(var quat: LPD3DRMQUATERNION;
    var v: LPD3DVECTOR; theta: D3DVALUE): LPD3DRMQUATERNION; stdcall;
function D3DRMQuaternionMultiply(var q, a, b: D3DRMQUATERNION):
    LPD3DRMQUATERNION; stdcall;
function D3DRMQuaternionSlerp( var q, a, b: D3DRMQUATERNION; alpha: D3DVALUE):
    LPD3DRMQUATERNION; stdcall;
procedure D3DRMMatrixFromQuaternion(dmMat: D3DRMMATRIX4D; var lpDqQuat:
    D3DRMQUATERNION); stdcall;

{function D3DRMQuaternionFromMatrix(D3DRMMATRIX4D: LPD3DRMQUATERNION):
    LPD3DRMQUATERNION; stdcall;
}


(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drm.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

(*
 * Direct3DRM Object classes
 *)

const
  CLSID_CDirect3DRMDevice: TGUID =(D1:$4fa3568e;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMViewport: TGUID =(D1:$4fa3568f;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMFrame: TGUID =(D1:$4fa35690;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMesh: TGUID =(D1:$4fa35691;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMeshBuilder: TGUID =(D1:$4fa35692;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMFace: TGUID =(D1:$4fa35693;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMLight: TGUID =(D1:$4fa35694;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMTexture: TGUID =(D1:$4fa35695;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMWrap: TGUID =(D1:$4fa35696;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMaterial: TGUID =(D1:$4fa35697;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMAnimation: TGUID =(D1:$4fa35698;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMAnimationSet: TGUID =(D1:$4fa35699;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMUserVisual: TGUID =(D1:$4fa3569a;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMShadow: TGUID =(D1:$4fa3569b;D2:$623f;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMViewportInterpolator: TGUID =(D1:$de9eaa1;D2:$3b84;D3:$11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  CLSID_CDirect3DRMFrameInterpolator: TGUID =(D1:$de9eaa2;D2:$3b84;D3:$11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  CLSID_CDirect3DRMMeshInterpolator: TGUID =(D1:$de9eaa3;D2:$3b84;D3:$11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  CLSID_CDirect3DRMLightInterpolator: TGUID =(D1:$de9eaa6;D2:$3b84;D3:$11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  CLSID_CDirect3DRMMaterialInterpolator: TGUID =(D1:$de9eaa7;D2:$3b84;D3:$11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  CLSID_CDirect3DRMTextureInterpolator: TGUID =(D1:$de9eaa8;D2:$3b84;D3:$11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  CLSID_CDirect3DRMProgressiveMesh: TGUID =(D1:$4516ec40;D2:$8f20;D3:$11d0;D4:($9b, $6d, $00, $00, $c0, $78, $1b, $c3));

(*
 * Direct3DRM Object interfaces
 *)

  IID_IDirect3DRMObject: TGUID =(D1:$eb16cb00;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMDevice: TGUID =(D1:$e9e19280;D2:$6e05;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMDevice2: TGUID = (D1:$4516ec78;D2: $8f20;D3: $11d0;D4:($9b, $6d, $00, $00, $c0, $78, $1b, $c3));
  IID_IDirect3DRMViewport: TGUID =(D1:$eb16cb02;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMFrame: TGUID =(D1:$eb16cb03;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMFrame2: TGUID = (D1:$c3dfbd60;D2: $3988;D3: $11d0;D4:($9e, $c2, $0, $0, $c0, $29, $1a, $c3));
  IID_IDirect3DRMVisual: TGUID =(D1:$eb16cb04;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMMesh: TGUID =(D1:$a3a80d01;D2:$6e12;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMMeshBuilder: TGUID =(D1:$a3a80d02;D2:$6e12;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMMeshBuilder2: TGUID = (D1:$4516ec77;D2: $8f20;D3: $11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  IID_IDirect3DRMFace: TGUID =(D1:$eb16cb07;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMLight: TGUID =(D1:$eb16cb08;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMTexture: TGUID =(D1:$eb16cb09;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMTexture2: TGUID = (D1:$120f30c0;D2: $1629;D3: $11d0;D4:($94, $1c, $0, $80, $c8, $c, $fa, $7b));
  IID_IDirect3DRMWrap: TGUID =(D1:$eb16cb0a;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMMaterial: TGUID =(D1:$eb16cb0b;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMAnimation: TGUID =(D1:$eb16cb0d;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMAnimationSet: TGUID =(D1:$eb16cb0e;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMDeviceArray: TGUID =(D1:$eb16cb10;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMViewportArray: TGUID =(D1:$eb16cb11;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMFrameArray: TGUID =(D1:$eb16cb12;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMVisualArray: TGUID =(D1:$eb16cb13;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMLightArray: TGUID =(D1:$eb16cb14;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMPickedArray: TGUID =(D1:$eb16cb16;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMFaceArray: TGUID =(D1:$eb16cb17;D2:$d271;D3:$11ce;D4:($ac,$48,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMUserVisual: TGUID =(D1:$59163de0;D2:$6d43;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMShadow: TGUID =(D1:$af359780;D2:$6ba3;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRMInterpolator: TGUID = (D1:$242f6bc1;D2: $3849;D3: $11d0;D4:($9b, $6d, $0, $0, $c0, $78, $1b, $c3));
  IID_IDirect3DRMProgressiveMesh: TGUID = (D1:$4516ec79;D2: $8f20;D3: $11d0;D4:($9b, $6d, $00, $00, $c0, $78, $1b, $c3));
  IID_IDirect3DRMPicked2Array: TGUID = (D1:$4516ec7b;D2: $8f20;D3: $11d0;D4:($9b, $6d, $00, $00, $c0, $78, $1b, $c3));

type
  IDirect3DRMObject = interface;
  IDirect3DRMDevice = interface;
  IDirect3DRMDevice2 = interface;
  IDirect3DRMViewport = interface;
  IDirect3DRMFrame = interface;
  IDirect3DRMFrame2 = interface;
  IDirect3DRMVisual = interface;
  IDirect3DRMMesh = interface;
  IDirect3DRMMeshBuilder = interface;
  IDirect3DRMMeshBuilder2 = interface;
  IDirect3DRMFace = interface;
  IDirect3DRMLight = interface;
  IDirect3DRMTexture = interface;
  IDirect3DRMTexture2 = interface;
  IDirect3DRMWrap = interface;
  IDirect3DRMMaterial = interface;
  IDirect3DRMInterpolator = interface;
  IDirect3DRMAnimation = interface;
  IDirect3DRMAnimationSet = interface;
  IDirect3DRMUserVisual = interface;
  IDirect3DRMShadow = interface;
  IDirect3DRMArray = interface;
  IDirect3DRMObjectArray = interface;
  IDirect3DRMDeviceArray = interface;
  IDirect3DRMFaceArray = interface;
  IDirect3DRMViewportArray = interface;
  IDirect3DRMFrameArray = interface;
  IDirect3DRMVisualArray = interface;
  IDirect3DRMPickedArray = interface;
  IDirect3DRMPicked2Array = interface;
  IDirect3DRMLightArray = interface;
  IDirect3DRMProgressiveMesh = interface;


  D3DRMOBJECTCALLBACK = procedure(lpD3DRMobj: IDirect3DRMObject;
      lpArg: Pointer); CDECL;

  D3DRMFRAMEMOVECALLBACK = procedure(lpD3DRMFrame: IDirect3DRMFrame;
      lpArg: Pointer; delta: D3DVALUE); CDECL;

  D3DRMUPDATECALLBACK = procedure(lpobj: IDirect3DRMDevice; lpArg: Pointer;
      iRectCount: Integer; d3dRectUpdate: LPD3DRECT); CDECL;

  D3DRMUSERVISUALCALLBACK = function(lpD3DRMUV: IDirect3DRMUserVisual;
      lpArg: Pointer; lpD3DRMUVreason: D3DRMUSERVISUALREASON;
      lpD3DRMDev: IDirect3DRMDevice;
      lpD3DRMview: IDirect3DRMViewport): Integer; CDECL;

  D3DRMLOADTEXTURECALLBACK = function(tex_name: PChar; lpArg: Pointer;
      lpD3DRMTex: IDirect3DRMTexture): HRESULT; CDECL;

  D3DRMLOADCALLBACK = procedure(lpObject: IDirect3DRMObject; const ObjectGuid: TGUID;
     lpArg: Pointer); CDECL;

  D3DRMPICKDESC = record
    ulFaceIdx: Longint;
    lGroupIdx: Longint;
    vPosition: D3DVECTOR;
  end;
  LPD3DRMPICKDESC = ^D3DRMPICKDESC;

  D3DRMPICKDESC2 = record
    ulFaceIdx: Longint;
    lGroupIdx: Longint;
    dvPosition: D3DVECTOR;
    tu, tv: D3DVALUE;
    dvNormal: D3DVECTOR;
    dcColor: D3DCOLOR;
  end;
  LPD3DRMPICKDESC2 = ^D3DRMPICKDESC2;

(*
 * Base interface
 *)
  IDirect3DRMObject = interface(IUnknown)
    ['{EB16CB00-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMObject methods ***)
    function Clone(pUnkOuter: IUnknown; const riid: TGUID;
        out ppvObj): HRESULT; stdcall;
    function AddDestroyCallback(lpCallback: D3DRMOBJECTCALLBACK;
        lpArg: Pointer): HRESULT; stdcall;
    function DeleteDestroyCallback(d3drmObjProc: D3DRMOBJECTCALLBACK;
        lpArg: Pointer): HRESULT; stdcall;
    function SetAppData(ulData: DWORD): HRESULT; stdcall;
    function GetAppData: DWORD; stdcall;
    function SetName(lpName: PChar): HRESULT; stdcall;
    function GetName(var lpdwSize: DWORD; lpName: PChar): HRESULT; stdcall;
    function GetClassName(var lpdwSize: DWORD; lpName: PChar): HRESULT; stdcall;
  end;

  IDirect3DRMVisual = interface(IDirect3DRMObject)
    ['{EB16CB04-D271-11CE-AC48-0000C03825A1}']
  end;

  IDirect3DRMDevice = interface(IDirect3DRMObject)
    ['{E9E19280-6E05-11CF-AC4A-0000C03825A1}']
    (*** IDirect3DRMDevice methods ***)
    function Init(width: Longint; height: Longint): HRESULT; stdcall;
    function InitFromD3D(lpD3D: IDirect3D; lpD3DIMDev: IDirect3DDevice):
        HRESULT; stdcall;
    function InitFromClipper(lpDDClipper: IDirectDrawClipper;
        const lpGUID: TGUID; width: Integer; height: Integer): HRESULT; stdcall;
    function Update: HRESULT; stdcall;
    function AddUpdateCallback(d3drmUpdateProc: D3DRMUPDATECALLBACK;
        arg: Pointer): HRESULT; stdcall;
    function DeleteUpdateCallback(d3drmUpdateProc: D3DRMUPDATECALLBACK;
        arg: Pointer): HRESULT; stdcall;
    function SetBufferCount(dwCount: DWORD): HRESULT; stdcall;
    function GetBufferCount: DWORD; stdcall;
    function SetDither(bDither: BOOL): HRESULT; stdcall;
    function SetShades(ulShades: DWORD): HRESULT; stdcall;
    function SetQuality(rqQuality: D3DRMRENDERQUALITY): HRESULT; stdcall;
    function SetTextureQuality(tqTextureQuality: D3DRMTEXTUREQUALITY): HRESULT;
        stdcall;
    function GetViewports(out lplpViewports: IDirect3DRMViewportArray): HRESULT;
        stdcall;
    function GetDither: BOOL; stdcall;
    function GetShades: DWORD; stdcall;
    function GetHeight: DWORD; stdcall;
    function GetWidth: DWORD; stdcall;
    function GetTrianglesDrawn: DWORD; stdcall;
    function GetWireframeOptions: DWORD; stdcall;
    function GetQuality: D3DRMRENDERQUALITY; stdcall;
    function GetColorModel: D3DCOLORMODEL; stdcall;
    function GetTextureQuality: D3DRMTEXTUREQUALITY; stdcall;
    function GetDirect3DDevice(out lplpD3DDevice: IDirect3DDevice): HRESULT;
        stdcall;
  end;

  IDirect3DRMDevice2 = interface(IDirect3DRMDevice)
    ['{4516EC78-8F20-11D0-9B6D-0000C0781BC3}']
    (*** IDirect3DRMDevice2 methods ***)
    function InitFromD3D2(lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2):
        HRESULT; stdcall;
    function InitFromSurface(const lpGUID: TGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface): HRESULT; stdcall;
    function SetRenderMode(dwFlags: DWORD): HRESULT; stdcall;
    function GetRenderMode: DWORD; stdcall;
    function GetDirect3DDevice2(out lplpD3DDevice: IDirect3DDevice2): HRESULT;
        stdcall;
  end;

  IDirect3DRMViewport = interface(IDirect3DRMObject)
    ['{EB16CB02-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMViewport methods ***)
    function Init(lpD3DRMDevice: IDirect3DRMDevice;
        lpD3DRMFrameCamera: IDirect3DRMFrame; xpos, ypos, width, height: DWORD):
        HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
    function Render(lpD3DRMFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function SetFront(rvFront: D3DVALUE): HRESULT; stdcall;
    function SetBack(rvBack: D3DVALUE): HRESULT; stdcall;
    function SetField(rvField: D3DVALUE): HRESULT; stdcall;
    function SetUniformScaling(bScale: BOOL): HRESULT; stdcall;
    function SetCamera(lpCamera: IDirect3DRMFrame): HRESULT; stdcall;
    function SetProjection(rptType: D3DRMPROJECTIONTYPE): HRESULT; stdcall;
    function Transform(var lprvDst: D3DRMVECTOR4D; var lprvSrc: D3DVECTOR):
        HRESULT; stdcall;
    function InverseTransform(var lprvDst: D3DVECTOR;
        var lprvSrc: D3DRMVECTOR4D): HRESULT; stdcall;
    function Configure(lX, lY: Longint; dwWidth, dwHeight: DWORD): HRESULT;
         stdcall;
    function ForceUpdate(dwX1, dwY1, dwX2, dwY2: DWORD): HRESULT; stdcall;
    function SetPlane(rvLeft, rvRight, rvBottom, rvTop: D3DVALUE): HRESULT;
         stdcall;
    function GetCamera(out lpCamera: IDirect3DRMFrame): HRESULT; stdcall;
    function GetDevice(out lpD3DRMDevice: IDirect3DRMDevice): HRESULT;  stdcall;
    function GetPlane(var lpd3dvLeft, lpd3dvRight, lpd3dvBottom,
        lpd3dvTop: D3DVALUE): HRESULT; stdcall;
    function Pick(lX, lY: Longint; out lplpVisuals: IDirect3DRMPickedArray):
        HRESULT; stdcall;
    function GetUniformScaling: BOOL; stdcall;
    function GetX: Longint; stdcall;
    function GetY: Longint; stdcall;
    function GetWidth: DWORD; stdcall;
    function GetHeight: DWORD; stdcall;
    function GetField: D3DVALUE; stdcall;
    function GetBack: D3DVALUE; stdcall;
    function GetFront: D3DVALUE; stdcall;
    function GetProjection: D3DRMPROJECTIONTYPE; stdcall;
    function GetDirect3DViewport(out lplpD3DViewport: IDirect3DViewport):
        HRESULT; stdcall;
  end;

  IDirect3DRMFrame = interface(IDirect3DRMVisual)
    ['{EB16CB03-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMFrame methods ***)
    function AddChild(lpD3DRMFrameChild: IDirect3DRMFrame): HRESULT; stdcall;
    function AddLight(lpD3DRMLight: IDirect3DRMLight): HRESULT; stdcall;
    function AddMoveCallback(d3drmFMC: D3DRMFRAMEMOVECALLBACK;
        lpArg: Pointer): HRESULT; stdcall;
    function AddTransform(rctCombine: D3DRMCOMBINETYPE;
        rmMatrix: D3DRMMATRIX4D): HRESULT; stdcall;
    function AddTranslation(rctCombine: D3DRMCOMBINETYPE; rvX, rvY,
        rvZ: D3DVALUE): HRESULT; stdcall;
    function AddScale(rctCombine: D3DRMCOMBINETYPE; rvX, rvY,
        rvZ: D3DVALUE): HRESULT; stdcall;
    function AddRotation(rctCombine: D3DRMCOMBINETYPE; rvX, rvY, rvZ,
        rvTheta: D3DVALUE): HRESULT; stdcall;
    function AddVisual(lpD3DRMVisual: IDirect3DRMVisual): HRESULT; stdcall;
    function GetChildren(out lplpChildren: IDirect3DRMFrameArray): HRESULT;
         stdcall;
    function GetColor: D3DCOLOR; stdcall;
    function GetLights(out lplpLights: IDirect3DRMLightArray): HRESULT; stdcall;
    function GetMaterialMode: D3DRMMATERIALMODE; stdcall;
    function GetParent(out lplpParent: IDirect3DRMFrame): HRESULT; stdcall;
    function GetPosition(lpRef: IDirect3DRMFrame; var lprvPos: D3DVECTOR):
        HRESULT; stdcall;
    function GetRotation(lpRef: IDirect3DRMFrame; var lprvAxis: D3DVECTOR;
        var lprvTheta: D3DVALUE): HRESULT; stdcall;
    function GetScene(out lplpRoot: IDirect3DRMFrame): HRESULT; stdcall;
    function GetSortMode: D3DRMSORTMODE; stdcall;
    function GetTexture(out lplpTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function GetTransform(var rmMatrix: D3DRMMATRIX4D): HRESULT; stdcall;
    function GetVelocity(lpRef: IDirect3DRMFrame; var lprvVel: D3DVECTOR;
        fRotVel: BOOL): HRESULT; stdcall;
    function GetOrientation(lpRef: IDirect3DRMFrame; var lprvDir: D3DVECTOR;
        var lprvUp: D3DVECTOR): HRESULT; stdcall;
    function GetVisuals(out lplpVisuals: IDirect3DRMVisualArray): HRESULT;
        stdcall;
    function GetTextureTopology(var lpU, lpV: BOOL): HRESULT; stdcall;
    function InverseTransform(var lprvDst, lprvSrc: D3DVECTOR): HRESULT;
        stdcall;
    function Load(lpvObjSource: Pointer; lpvObjID: Pointer;
        d3drmLOFlags: D3DRMLOADOPTIONS; d3drmLoadTextureProc:
        D3DRMLOADTEXTURECALLBACK; lpArgLTP: Pointer): HRESULT; stdcall;
    function LookAt(lpTarget, lpRef: IDirect3DRMFrame;
        rfcConstraint: D3DRMFRAMECONSTRAINT ): HRESULT; stdcall;
    function Move(delta: D3DVALUE): HRESULT; stdcall;
    function DeleteChild(lpChild: IDirect3DRMFrame): HRESULT; stdcall;
    function DeleteLight(lpD3DRMLight: IDirect3DRMLight): HRESULT; stdcall;
    function DeleteMoveCallback(d3drmFMC: D3DRMFRAMEMOVECALLBACK;
        lpArg: Pointer): HRESULT; stdcall;
    function DeleteVisual(lpD3DRMVisual: IDirect3DRMVisual): HRESULT; stdcall;
    function GetSceneBackground: D3DCOLOR; stdcall;
    function GetSceneBackgroundDepth(out lplpDDSurface: IDirectDrawSurface):
        HRESULT; stdcall;
    function GetSceneFogColor: D3DCOLOR; stdcall;
    function GetSceneFogEnable: BOOL; stdcall;
    function GetSceneFogMode: D3DRMFOGMODE; stdcall;
    function GetSceneFogParams(var lprvStart, lprvEnd, lprvDensity: D3DVALUE):
        HRESULT; stdcall;
    function SetSceneBackground(rcColor: D3DCOLOR): HRESULT; stdcall;
    function SetSceneBackgroundRGB(rvRed, rvGreen, rvBlue: D3DVALUE): HRESULT;
        stdcall;
    function SetSceneBackgroundDepth(lpImage: IDirectDrawSurface): HRESULT;
        stdcall;
    function SetSceneBackgroundImage(lpTexture: IDirect3DRMTexture): HRESULT;
        stdcall;
    function SetSceneFogEnable(bEnable: BOOL): HRESULT; stdcall;
    function SetSceneFogColor(rcColor: D3DCOLOR): HRESULT; stdcall;
    function SetSceneFogMode(rfMode: D3DRMFOGMODE): HRESULT; stdcall;
    function SetSceneFogParams(rvStart, rvEnd, rvDensity: D3DVALUE): HRESULT;
         stdcall;
    function SetColor(rcColor: D3DCOLOR): HRESULT; stdcall;
    function SetColorRGB(rvRed, rvGreen, rvBlue: D3DVALUE): HRESULT; stdcall;
    function GetZbufferMode: D3DRMZBUFFERMODE; stdcall;
    function SetMaterialMode(rmmMode: D3DRMMATERIALMODE): HRESULT; stdcall;
    function SetOrientation(lpRef: IDirect3DRMFrame; rvDx, rvDy, rvDz, rvUx,
        rvUy, rvUz: D3DVALUE): HRESULT; stdcall;
    function SetPosition(lpRef: IDirect3DRMFrame; rvX, rvY, rvZ: D3DVALUE):
        HRESULT; stdcall;
    function SetRotation(lpRef: IDirect3DRMFrame; rvX, rvY, rvZ,
        rvTheta: D3DVALUE): HRESULT; stdcall;
    function SetSortMode(d3drmSM: D3DRMSORTMODE): HRESULT; stdcall;
    function SetTexture(lpD3DRMTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function SetTextureTopology(cylU, cylV: BOOL): HRESULT; stdcall;
    function SetVelocity(lpRef: IDirect3DRMFrame; rvX, rvY, rvZ: D3DVALUE;
        fRotVel: BOOL): HRESULT; stdcall;
    function SetZbufferMode(d3drmZBM: D3DRMZBUFFERMODE): HRESULT; stdcall;
    function Transform(var lpd3dVDst, lpd3dVSrc: D3DVECTOR): HRESULT; stdcall;
  end;

  IDirect3DRMFrame2 = interface(IDirect3DRMFrame)
    ['{C3DFBD60-3988-11D0-9EC2-0000C0291AC3}']
    (*** IDirect3DRMFrame2 methods ***)
    function AddMoveCallback2(d3drmFMC: D3DRMFRAMEMOVECALLBACK; lpArg:
        Pointer; dwFlags: DWORD): HRESULT; stdcall;
    function GetBox(var lpD3DRMBox: D3DRMBOX): HRESULT; stdcall;
    function GetBoxEnable: BOOL; stdcall;
    function GetAxes(var dir, up: D3DVECTOR): HRESULT; stdcall;
    function GetMaterial(out lplpMaterial: IDirect3DRMMaterial): HRESULT;
         stdcall;
    function GetInheritAxes: BOOL; stdcall;
    function GetHierarchyBox(var lpD3DRMBox: D3DRMBOX): HRESULT; stdcall;
    function SetBox(const lpD3DRMBox: D3DRMBOX): HRESULT; stdcall;
    function SetBoxEnable(bEnableFlag: BOOL): HRESULT; stdcall;
    function SetAxes(dx, dy, dz, ux, uy, uz: D3DVALUE): HRESULT; stdcall;
    function SetInheritAxes(inherit_from_parent: BOOL): HRESULT; stdcall;
    function SetMaterial(const lplpMaterial: IDirect3DRMMaterial): HRESULT;
         stdcall;
    function SetQuaternion(lpRef: IDirect3DRMFrame2; var quat: D3DRMQUATERNION):
        HRESULT; stdcall;
    function RayPick(lpRefFrame: IDirect3DRMFrame; const ray: D3DRMRAY;
        dwFlags: DWORD; out lplpPicked2Array: IDirect3DRMPicked2Array) :
        HRESULT; stdcall;
    function Save(lpFilename: PChar; d3dFormat: D3DRMXOFFORMAT;
        d3dSaveFlags: D3DRMSAVEOPTIONS): HRESULT; stdcall;
  end;

  IDirect3DRMMesh = interface(IDirect3DRMVisual)
    ['{A3A80D01-6E12-11CF-AC4A-0000C03825A1}']
    (*** IDirect3DRMMesh methods ***)
    function Scale(sx, sy, sz: D3DVALUE): HRESULT; stdcall;
    function Translate(tx, ty, tz: D3DVALUE): HRESULT; stdcall;
    function GetBox(var lpD3DRMBox: D3DRMBOX): HRESULT; stdcall;
    function AddGroup(vCount, fCount, vPerFace: DWORD; var fData: DWORD;
        var returnId: D3DRMGROUPINDEX): HRESULT; stdcall;
    function SetVertices(id: D3DRMGROUPINDEX; index, count: DWORD;
        var values: D3DRMVERTEX): HRESULT; stdcall;
    function SetGroupColor(id: D3DRMGROUPINDEX; value: D3DCOLOR): HRESULT;
         stdcall;
    function SetGroupColorRGB(id: D3DRMGROUPINDEX; red, green,
        blue: D3DVALUE): HRESULT; stdcall;
    function SetGroupMapping(id: D3DRMGROUPINDEX;
        value: D3DRMMAPPING): HRESULT; stdcall;
    function SetGroupQuality(id: D3DRMGROUPINDEX;
        value: D3DRMRENDERQUALITY): HRESULT; stdcall;
    function SetGroupMaterial(id: D3DRMGROUPINDEX; value:
        IDirect3DRMMaterial): HRESULT; stdcall;
    function SetGroupTexture(id: D3DRMGROUPINDEX; value: IDirect3DRMTexture):
        HRESULT; stdcall;
    function GetGroupCount: DWORD; stdcall;
    function GetGroup(id: D3DRMGROUPINDEX; var vCount, fCount, vPerFace,
        fDataSize, fData: DWORD): HRESULT; stdcall;
    function GetVertices(id: D3DRMGROUPINDEX; index: DWORD; count: DWORD;
        var returnPtr: D3DRMVertex): HResult; stdcall;
    function GetGroupColor(id: D3DRMGROUPINDEX): D3DCOLOR; stdcall;
    function GetGroupMapping(id: D3DRMGROUPINDEX): D3DRMMAPPING; stdcall;
    function GetGroupQuality(id: D3DRMGROUPINDEX): D3DRMRENDERQUALITY; stdcall;
    function GetGroupMaterial(id: D3DRMGROUPINDEX;
        out returnPtr: IDirect3DRMMaterial): HRESULT; stdcall;
    function GetGroupTexture(id: D3DRMGROUPINDEX;
        out returnPtr: IDirect3DRMTexture): HRESULT; stdcall;
  end;

  IDirect3DRMProgressiveMesh = interface(IDirect3DRMVisual)
    ['{4516EC79-8F20-11D0-9B6D-0000C0781BC3}']
    (*** IDirect3DRMProgressiveMesh methods ***)
    function Load(lpSource, lpObjID: pointer; dloLoadflags: D3DRMLOADOPTIONS;
        lpCallback: D3DRMLOADTEXTURECALLBACK; lpArg: pointer): HRESULT; stdcall;
    function GetLoadStatus(var lpStatus: D3DRMPMESHLOADSTATUS): HRESULT;
        stdcall;
    function SetMinRenderDetail(d3dVal: D3DVALUE): HRESULT; stdcall;
    function Abort(dwFlags: DWORD): HRESULT; stdcall;
    function GetFaceDetail(var lpdwCount: DWORD): HRESULT; stdcall;
    function GetVertexDetail(var lpdwCount: DWORD): HRESULT; stdcall;
    function SetFaceDetail(dwCount: DWORD): HRESULT; stdcall;
    function SetVertexDetail(dwCount: DWORD): HRESULT; stdcall;
    function GetFaceDetailRange(var lpdwMin, lpdwMax: DWORD): HRESULT; stdcall;
    function GetVertexDetailRange(var lpdwMin, lpdwMax: DWORD): HRESULT; stdcall;
    function GetDetail(var lpdvVal: D3DVALUE): HRESULT; stdcall;
    function SetDetail(lpdvVal: D3DVALUE): HRESULT; stdcall;
    function RegisterEvents(hEvent: THANDLE; dwFlags, dwReserved: DWORD):
        HRESULT; stdcall;
    function CreateMesh(out lplpD3DRMMesh: IDirect3DRMMesh): HRESULT; stdcall;
    function Duplicate(out lplpD3DRMPMesh: IDirect3DRMProgressiveMesh): HRESULT;
        stdcall;
    function GetBox(var lpBBox: D3DRMBOX): HRESULT; stdcall;
    function SetQuality(quality: D3DRMRENDERQUALITY): HRESULT; stdcall;
    function GetQuality(var lpdwquality: D3DRMRENDERQUALITY): HRESULT; stdcall;
  end;

  IDirect3DRMShadow = interface(IDirect3DRMVisual)
    ['{AF359780-6BA3-11CF-AC4A-0000C03825A1}']
    (*** IDirect3DRMShadow methods ***)
    function Init(lpD3DRMVisual: IDirect3DRMVisual;
        lpD3DRMLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: D3DVALUE):
        HRESULT; stdcall;
  end;

  IDirect3DRMFace = interface(IDirect3DRMObject)
    ['{EB16CB07-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMFace methods ***)
    function AddVertex(x, y, z: D3DVALUE): HRESULT; stdcall;
    function AddVertexAndNormalIndexed(vertex: DWORD; normal: DWORD): HRESULT;
        stdcall;
    function SetColorRGB(red, green, blue: D3DVALUE): HRESULT; stdcall;
    function SetColor(color: D3DCOLOR): HRESULT; stdcall;
    function SetTexture(lpD3DRMTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function SetTextureCoordinates(vertex: DWORD; u, v: D3DVALUE): HRESULT;
        stdcall;
    function SetMaterial(lpMat: IDirect3DRMMaterial): HRESULT; stdcall;
    function SetTextureTopology(cylU, cylV: BOOL): HRESULT; stdcall;
    function GetVertex(index: DWORD; var lpPosition: D3DVECTOR;
        var lpNormal: D3DVECTOR): HRESULT; stdcall;
    function GetVertices(var lpdwVertexCount: DWORD;
        var lpPosition, lpNormal: D3DVECTOR): HRESULT; stdcall;
    function GetTextureCoordinates(index: DWORD; var lpU, lpV: D3DVALUE):
        HRESULT; stdcall;
    function GetTextureTopology(var lpU, lpV: BOOL): HRESULT; stdcall;
    function GetNormal(var lpNormal: D3DVECTOR): HRESULT; stdcall;
    function GetTexture(out lplpTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function GetMaterial(out lpMat: IDirect3DRMMaterial): HRESULT; stdcall;
    function GetVertexCount: Integer; stdcall;
    function GetVertexIndex(dwIndex: DWORD): Integer; stdcall;
    function GetTextureCoordinateIndex(dwIndex: DWORD): Integer; stdcall;
    function GetColor: D3DCOLOR; stdcall;
  end;

  IDirect3DRMMeshBuilder = interface(IDirect3DRMVisual)
    ['{A3A80D02-6E12-11CF-AC4A-0000C03825A1}']
    (*** IDirect3DRMMeshBuilder methods ***)
    function Load(lpvObjSource, lpvObjID: Pointer; d3drmLOFlags:
        D3DRMLOADOPTIONS; d3drmLoadTextureProc: D3DRMLOADTEXTURECALLBACK;
        lpvArg: Pointer): HRESULT; stdcall;
    function Save(lpFilename: PChar; d3drmXOFFormat: D3DRMXOFFORMAT;
        d3drmSOContents: D3DRMSAVEOPTIONS): HRESULT; stdcall;
    function Scale(sx, sy, sz: D3DVALUE): HRESULT; stdcall;
    function Translate(tx, ty, tz: D3DVALUE): HRESULT; stdcall;
    function SetColorSource(source: D3DRMCOLORSOURCE): HRESULT; stdcall;
    function GetBox(var lpD3DRMBox: D3DRMBOX): HRESULT; stdcall;
    function GenerateNormals: HRESULT; stdcall;
    function GetColorSource: D3DRMCOLORSOURCE; stdcall;
    function AddMesh(lpD3DRMMesh: IDirect3DRMMesh): HRESULT; stdcall;
    function AddMeshBuilder(lpD3DRMMeshBuild: IDirect3DRMMeshBuilder): HRESULT;
        stdcall;
    function AddFrame(lpD3DRMFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function AddFace(lpD3DRMFace: IDirect3DRMFace): HRESULT; stdcall;
    function AddFaces(dwVertexCount: DWORD; var lpD3DVertices: D3DVECTOR;
        normalCount: DWORD; var lpNormals: D3DVECTOR; var lpFaceData: DWORD;
        out lplpD3DRMFaceArray: IDirect3DRMFaceArray): HRESULT; stdcall;
    function ReserveSpace(vertexCount, normalCount, faceCount: DWORD): HRESULT;
        stdcall;
    function SetColorRGB(red, green, blue: D3DVALUE): HRESULT; stdcall;
    function SetColor(color: D3DCOLOR): HRESULT; stdcall;
    function SetTexture(lpD3DRMTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function SetMaterial(lpIDirect3DRMmaterial: IDirect3DRMMaterial): HRESULT;
        stdcall;
    function SetTextureTopology(cylU, cylV: BOOL): HRESULT; stdcall;
    function SetQuality(quality: D3DRMRENDERQUALITY): HRESULT; stdcall;
    function SetPerspective(perspective: BOOL): HRESULT; stdcall;
    function SetVertex(index: DWORD; x, y, z: D3DVALUE): HRESULT; stdcall;
    function SetNormal(index: DWORD; x, y, z: D3DVALUE): HRESULT; stdcall;
    function SetTextureCoordinates(index: DWORD; u, v: D3DVALUE): HRESULT;
        stdcall;
    function SetVertexColor(index: DWORD; color: D3DCOLOR): HRESULT; stdcall;
    function SetVertexColorRGB(index: DWORD; red, green, blue: D3DVALUE):
        HRESULT; stdcall;
    function GetFaces(out lplpD3DRMFaceArray: IDirect3DRMFaceArray): HRESULT;
        stdcall;
    function GetVertices(var vcount: DWORD; var vertices: D3DVECTOR;
        var ncount: DWORD; var normals: D3DVECTOR; var face_data_size: DWORD;
        var face_data: DWORD): HRESULT; stdcall;
    function GetTextureCoordinates(index: DWORD; var u, v: D3DVALUE): HRESULT;
        stdcall;
    function AddVertex(x, y, z: D3DVALUE): Integer; stdcall;
    function AddNormal(x, y, z: D3DVALUE): Integer; stdcall;
    function CreateFace(out lplpd3drmFace: IDirect3DRMFace): HRESULT; stdcall;
    function GetQuality: D3DRMRENDERQUALITY; stdcall;
    function GetPerspective: BOOL; stdcall;
    function GetFaceCount: Integer; stdcall;
    function GetVertexCount: Integer; stdcall;
    function GetVertexColor(index: DWORD): D3DCOLOR; stdcall;
    function CreateMesh(out lplpD3DRMMesh: IDirect3DRMMesh): HRESULT; stdcall;
  end;

  IDirect3DRMMeshBuilder2 = interface(IDirect3DRMMeshBuilder)
    ['{4516EC77-8F20-11D0-9B6D-0000C0781BC3}']
    (*** IDirect3DRMMeshBuilder2 methods ***)
    function GenerateNormals2(dvCreaseAngle: D3DVALUE; dwFlags: DWORD) :
        HRESULT; stdcall;
    function GetFace(dwIndex: DWORD; out lplpD3DRMFace: IDirect3DRMFace) :
        HRESULT; stdcall;
  end;

  IDirect3DRMLight = interface(IDirect3DRMObject)
    ['{EB16CB08-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMLight methods ***)
    function SetType(d3drmtType: D3DRMLIGHTTYPE): HRESULT; stdcall;
    function SetColor(rcColor: D3DCOLOR): HRESULT; stdcall;
    function SetColorRGB(rvRed, rvGreen, rvBlue: D3DVALUE): HRESULT; stdcall;
    function SetRange(rvRange: D3DVALUE): HRESULT; stdcall;
    function SetUmbra(rvAngle: D3DVALUE): HRESULT; stdcall;
    function SetPenumbra(rvAngle: D3DVALUE): HRESULT; stdcall;
    function SetConstantAttenuation(rvAtt: D3DVALUE): HRESULT; stdcall;
    function SetLinearAttenuation(rvAtt: D3DVALUE): HRESULT; stdcall;
    function SetQuadraticAttenuation(rvAtt: D3DVALUE): HRESULT; stdcall;
    function GetRange: D3DVALUE; stdcall;
    function GetUmbra: D3DVALUE; stdcall;
    function GetPenumbra: D3DVALUE; stdcall;
    function GetConstantAttenuation: D3DVALUE; stdcall;
    function GetLinearAttenuation: D3DVALUE; stdcall;
    function GetQuadraticAttenuation: D3DVALUE; stdcall;
    function GetColor: D3DCOLOR; stdcall;
    function GetType: D3DRMLIGHTTYPE; stdcall;
    function SetEnableFrame(lpEnableFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function GetEnableFrame(out lplpEnableFrame: IDirect3DRMFrame): HRESULT;
        stdcall;
  end;

  IDirect3DRMTexture = interface(IDirect3DRMVisual)
    ['{EB16CB09-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMTexture methods ***)
    function InitFromFile(filename: PChar): HRESULT; stdcall;
    function InitFromSurface(lpDDS: IDirectDrawSurface): HRESULT; stdcall;
    function InitFromResource(rs: HRSRC): HRESULT; stdcall;
    function Changed(bPixels, bPalette: BOOL): HRESULT; stdcall;
    function SetColors(ulColors: DWORD): HRESULT; stdcall;
    function SetShades(ulShades: DWORD): HRESULT; stdcall;
    function SetDecalSize(rvWidth, rvHeight: D3DVALUE): HRESULT; stdcall;
    function SetDecalOrigin(lX, lY: Longint): HRESULT; stdcall;
    function SetDecalScale(dwScale: DWORD): HRESULT; stdcall;
    function SetDecalTransparency(bTransp: BOOL): HRESULT; stdcall;
    function SetDecalTransparentColor(rcTransp: D3DCOLOR): HRESULT; stdcall;
    function GetDecalSize(var lprvWidth, lprvHeight: D3DVALUE): HRESULT; stdcall;
    function GetDecalOrigin(var lplX, lplY: Longint): HRESULT; stdcall;
    function GetImage: LPD3DRMIMAGE; stdcall;
    function GetShades: DWORD; stdcall;
    function GetColors: DWORD; stdcall;
    function GetDecalScale: DWORD; stdcall;
    function GetDecalTransparency: BOOL; stdcall;
    function GetDecalTransparentColor: D3DCOLOR; stdcall;
  end;

  IDirect3DRMTexture2 = interface(IDirect3DRMTexture)
    ['{120F30C0-1629-11D0-941C-0080C80CFA7B}']
    (*** IDirect3DRMTexture2 methods ***)
    function InitFromImage(const lpImage: D3DRMIMAGE): HRESULT; stdcall;
    function InitFromResource2(hModule: HModule; strName, strType: PChar):
        HRESULT; stdcall;
    function GenerateMIPMap(dwFlags: DWORD): HRESULT; stdcall;
  end;

  IDirect3DRMWrap = interface(IDirect3DRMObject)
    ['{EB16CB0A-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMWrap methods ***)
    function Init(d3drmwt: D3DRMWRAPTYPE; lpd3drmfRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: D3DVALUE):
        HRESULT; stdcall;
    function Apply(lpObject: IDirect3DRMObject): HRESULT; stdcall;
    function ApplyRelative(frame: IDirect3DRMFrame; mesh: IDirect3DRMObject):
        HRESULT; stdcall;
  end;

  IDirect3DRMMaterial = interface(IDirect3DRMObject)
    ['{EB16CB0B-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMMaterial methods ***)
    function SetPower(rvPower: D3DVALUE): HRESULT; stdcall;
    function SetSpecular(r, g, b: D3DVALUE): HRESULT; stdcall;
    function SetEmissive(r, g, b: D3DVALUE): HRESULT; stdcall;
    function GetPower: D3DVALUE; stdcall;
    function GetSpecular(var lpr, lpg, lpb: D3DVALUE): HRESULT; stdcall;
    function GetEmissive(var lpr, lpg, lpb: D3DVALUE): HRESULT; stdcall;
  end;

  IDirect3DRMAnimation = interface(IDirect3DRMObject)
    ['{EB16CB0D-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMAnimation methods ***)
    function SetOptions(d3drmanimFlags: D3DRMANIMATIONOPTIONS): HRESULT;
        stdcall;
    function AddRotateKey(rvTime: D3DVALUE; var rqQuat: D3DRMQUATERNION):
        HRESULT; stdcall;
    function AddPositionKey(rvTime, rvX, rvY, rvZ: D3DVALUE): HRESULT; stdcall;
    function AddScaleKey(time, x, y, z: D3DVALUE): HRESULT; stdcall;
    function DeleteKey(time: D3DVALUE): HRESULT; stdcall;
    function SetFrame(lpD3DRMFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function SetTime(rvTime: D3DVALUE): HRESULT; stdcall;
    function GetOptions: D3DRMANIMATIONOPTIONS; stdcall;
  end;

  IDirect3DRMAnimationSet = interface(IDirect3DRMObject)
    ['{EB16CB0E-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMAnimationSet methods ***)
    function AddAnimation(lpD3DRMAnimation: IDirect3DRMAnimation): HRESULT;
         stdcall;
    function Load(lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: D3DRMLOADOPTIONS;
        d3drmLoadTextureProc: D3DRMLOADTEXTURECALLBACK; lpArgLTP: Pointer;
        lpParentFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function DeleteAnimation(lpD3DRMAnimation: IDirect3DRMAnimation): HRESULT;
        stdcall;
    function SetTime(rvTime: D3DVALUE): HRESULT; stdcall;
  end;

  IDirect3DRMUserVisual = interface(IDirect3DRMVisual)
    ['{59163DE0-6D43-11CF-AC4A-0000C03825A1}']
    (*** IDirect3DRMUserVisual methods ***)
    function Init(d3drmUVProc: D3DRMUSERVISUALCALLBACK; lpArg: Pointer):
        HRESULT; stdcall;
  end;

  IDirect3DRMArray = interface(IUnknown)
    function GetSize: DWORD; stdcall;
    (* No GetElement method as it would get overloaded
     * in derived classes, and overloading is
     * a no-no in COM
     *)
  end;

  IDirect3DRMObjectArray = interface(IDirect3DRMArray)
    function GetElement(index: DWORD; out lplpD3DRMObject:
        IDirect3DRMObject): HRESULT; stdcall;
  end;

  IDirect3DRMDeviceArray = interface(IDirect3DRMArray)
    ['{EB16CB10-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMDeviceArray methods ***)
    function GetElement(index: DWORD; out lplpD3DRMDevice:
        IDirect3DRMDevice): HRESULT; stdcall;
  end;

  IDirect3DRMFrameArray = interface(IDirect3DRMArray)
    ['{EB16CB12-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMFrameArray methods ***)
    function GetElement(index: DWORD; out lplpD3DRMFrame: IDirect3DRMFrame):
        HRESULT; stdcall;
  end;

  IDirect3DRMViewportArray = interface(IDirect3DRMArray)
    ['{EB16CB11-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMViewportArray methods ***)
    function GetElement(index: DWORD; out lplpD3DRMViewport:
        IDirect3DRMViewport): HRESULT; stdcall;
  end;

  IDirect3DRMVisualArray = interface(IDirect3DRMArray)
    ['{EB16CB13-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMVisualArray methods ***)
    function GetElement(index: DWORD; out lplpD3DRMVisual:
        IDirect3DRMVisual): HRESULT; stdcall;
  end;

  IDirect3DRMPickedArray = interface(IDirect3DRMArray)
    ['{EB16CB16-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMPickedArray methods ***)
    function GetPick(index: DWORD; out lplpVisual: IDirect3DRMVisual;
        out lplpFrameArray: IDirect3DRMFrameArray;
        var lpD3DRMPickDesc: D3DRMPICKDESC): HRESULT; stdcall;
  end;

  IDirect3DRMLightArray = interface(IDirect3DRMArray)
    ['{EB16CB14-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMLightArray methods ***)
    function GetElement(index: DWORD; out lplpD3DRMLight: IDirect3DRMLight):
        HRESULT; stdcall;
  end;

  IDirect3DRMFaceArray = interface(IDirect3DRMArray)
    ['{EB16CB17-D271-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMFaceArray methods ***)
    function GetElement(index: DWORD; out lplpD3DRMFace: IDirect3DRMFace):
        HRESULT; stdcall;
  end;

  IDirect3DRMPicked2Array = interface(IDirect3DRMArray)
    ['{4516EC7B-8F20-11D0-9B6D-0000C0781BC3}']
    (*** IDirect3DRMPicked2Array methods ***)
    function GetPick(index: DWORD; out lplpVisual: IDirect3DRMVisual;
        lplpFrameArray: IDirect3DRMFrameArray; out lpD3DRMPickDesc2:
        D3DRMPICKDESC2): HRESULT; stdcall;
  end;

  IDirect3DRMInterpolator = interface(IDirect3DRMObject)
    ['{242F6BC1-3849-11D0-9B6D-0000C0781BC3}']
    (*** IDirect3DRMInterpolator methods ***)
    function AttachObject(lpD3DRMObject: IDirect3DRMObject): HRESULT; stdcall;
    function GetAttachedObjects(lpD3DRMObjectArray: IDirect3DRMObjectArray):
        HRESULT; stdcall;
    function DetachObject(lpD3DRMObject: IDirect3DRMObject): HRESULT; stdcall;
    function SetIndex(d3dVal: D3DVALUE): HRESULT; stdcall;
    function GetIndex: D3DVALUE; stdcall;
    function Interpolate(d3dVal: D3DVALUE; lpD3DRMObject: IDirect3DRMObject;
        d3drmInterpFlags: D3DRMINTERPOLATIONOPTIONS): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drmwin.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

{ GUIDS used by Direct3DRM Windows interface }

const
  IID_IDirect3DRMWinDevice: TGUID = (D1:$c5016cc0;D2:$d273;D3:$11ce;D4:($ac,$48,$00,$00,$c0,$38,$25,$a1)) ;

type
  IDirect3DRMWinDevice = interface(IDirect3DRMObject)
    ['{C5016CC0-D273-11CE-AC48-0000C03825A1}']
    (*** IDirect3DRMWinDevice methods ***)
    function HandlePaint(hDC: HDC): HRESULT; stdcall;
    function HandleActivate(wparam: WORD): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drm.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

const
  IID_IDirect3DRM: TGUID = ( D1:$2bc49361;D2:$8327;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
  IID_IDirect3DRM2: TGUID = ( D1:$4516ecc8;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));

type
  IDirect3DRM = interface(IUnknown)
    ['{2BC49361-8327-11CF-AC4A-0000C03825A1}']
    (*** IDirect3DRM methods ***)
    function CreateObject(const rclsid: TGUID; pUnkOuter: IUnknown;
        const riid: TGUID; out ppv): HRESULT; stdcall;
    function CreateFrame(lpD3DRMFrame: IDirect3DRMFrame;
        out lplpD3DRMFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function CreateMesh(out lplpD3DRMMesh: IDirect3DRMMesh): HRESULT; stdcall;
    function CreateMeshBuilder(out lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder): HRESULT; stdcall;
    function CreateFace(out lplpd3drmFace: IDirect3DRMFace): HRESULT; stdcall;
    function CreateAnimation(out lplpD3DRMAnimation: IDirect3DRMAnimation):
        HRESULT; stdcall;
    function CreateAnimationSet(out lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet): HRESULT; stdcall;
    function CreateTexture(const lpImage: D3DRMIMAGE;
        out lplpD3DRMTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function CreateLight(d3drmltLightType: D3DRMLIGHTTYPE;
        cColor: D3DCOLOR; out lplpD3DRMLight: IDirect3DRMLight): HRESULT;
        stdcall;
    function CreateLightRGB(ltLightType: D3DRMLIGHTTYPE; vRed, vGreen, vBlue:
        D3DVALUE; out lplpD3DRMLight: IDirect3DRMLight): HRESULT; stdcall;
    function CreateMaterial(vPower: D3DVALUE; out lplpD3DRMMaterial:
        IDirect3DRMMaterial): HRESULT; stdcall;
    function CreateDevice(dwWidth, dwHeight: DWORD; out lplpD3DRMDevice:
        IDirect3DRMDevice): HRESULT; stdcall;
    function CreateDeviceFromSurface(const lpGUID: TGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; out lplpD3DRMDevice: IDirect3DRMDevice):
        HRESULT; stdcall;
    function CreateDeviceFromD3D(lpD3D: IDirect3D; lpD3DDev: IDirect3DDevice;
        out lplpD3DRMDevice: IDirect3DRMDevice): HRESULT; stdcall;
    function CreateDeviceFromClipper(lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer; out lplpD3DRMDevice:
        IDirect3DRMDevice): HRESULT; stdcall;
    function CreateTextureFromSurface(lpDDS: IDirectDrawSurface;
        out lplpD3DRMTexture: IDirect3DRMTexture): HRESULT; stdcall;
    function CreateShadow(lpVisual: IDirect3DRMVisual;
        lpLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: D3DVALUE;
        out lplpShadow: IDirect3DRMVisual): HRESULT; stdcall;
    function CreateViewport(lpDev: IDirect3DRMDevice;
        lpCamera: IDirect3DRMFrame; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        out lplpD3DRMViewport: IDirect3DRMViewport): HRESULT; stdcall;
    function CreateWrap(wraptype: D3DRMWRAPTYPE; lpRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: D3DVALUE;
        out lplpD3DRMWrap: IDirect3DRMWrap): HRESULT; stdcall;
    function CreateUserVisual(fn: D3DRMUSERVISUALCALLBACK; lpArg: Pointer;
        out lplpD3DRMUV: IDirect3DRMUserVisual): HRESULT; stdcall;
    function LoadTexture(lpFileName: LPSTR; out lplpD3DRMTexture:
        IDirect3DRMTexture): HRESULT; stdcall;
    function LoadTextureFromResource(rs: HRSRC; out lplpD3DRMTexture:
        IDirect3DRMTexture): HRESULT; stdcall;
    function SetSearchPath(lpPath: LPSTR): HRESULT; stdcall;
    function AddSearchPath(lpPath: LPSTR): HRESULT; stdcall;
    function GetSearchPath(var lpdwSize: DWORD; lpszPath: LPSTR): HRESULT;
         stdcall;
    function SetDefaultTextureColors(dwColors: DWORD): HRESULT; stdcall;
    function SetDefaultTextureShades(dwShades: DWORD): HRESULT; stdcall;
    function GetDevices(out lplpDevArray: IDirect3DRMDeviceArray): HRESULT;
         stdcall;
    function GetNamedObject(lpName: LPSTR; out lplpD3DRMObject:
        IDirect3DRMObject): HRESULT; stdcall;
    function EnumerateObjects(func: D3DRMOBJECTCALLBACK; lpArg: Pointer):
        HRESULT; stdcall;
    function Load(lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: D3DRMLOADOPTIONS; d3drmLoadProc:
        D3DRMLOADCALLBACK; lpArgLP: Pointer; d3drmLoadTextureProc:
        D3DRMLOADTEXTURECALLBACK; lpArgLTP: Pointer;
        lpParentFrame: IDirect3DRMFrame): HRESULT; stdcall;
    function Tick(d3dvalTick: D3DVALUE): HRESULT; stdcall;
  end;

  IDirect3DRM2 = interface(IUnknown)
    ['{4516ECC8-8F20-11D0-9B6D-0000C0781BC3}']
    (*** IDirect3DRM2 methods ***)
    function CreateObject(const rclsid: TGUID; pUnkOuter: IUnknown;
        const riid: TGUID; out ppv): HRESULT; stdcall;
    function CreateFrame(lpD3DRMFrame: IDirect3DRMFrame;
        out lplpD3DRMFrame: IDirect3DRMFrame2): HRESULT; stdcall;
    function CreateMesh(out lplpD3DRMMesh: IDirect3DRMMesh): HRESULT; stdcall;
    function CreateMeshBuilder(out lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder2): HRESULT; stdcall;
    function CreateFace(out lplpd3drmFace: IDirect3DRMFace): HRESULT; stdcall;
    function CreateAnimation(out lplpD3DRMAnimation: IDirect3DRMAnimation):
        HRESULT; stdcall;
    function CreateAnimationSet(out lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet): HRESULT; stdcall;
    function CreateTexture(const lpImage: D3DRMIMAGE;
        out lplpD3DRMTexture: IDirect3DRMTexture2): HRESULT; stdcall;
    function CreateLight(d3drmltLightType: D3DRMLIGHTTYPE;
        cColor: D3DCOLOR; out lplpD3DRMLight: IDirect3DRMLight): HRESULT;
        stdcall;
    function CreateLightRGB(ltLightType: D3DRMLIGHTTYPE; vRed,
        vGreen, vBlue: D3DVALUE; out lplpD3DRMLight: IDirect3DRMLight):
        HRESULT; stdcall;
    function CreateMaterial(vPower: D3DVALUE; out lplpD3DRMMaterial:
        IDirect3DRMMaterial): HRESULT; stdcall;
    function CreateDevice(dwWidth, dwHeight: DWORD; out lplpD3DRMDevice:
        IDirect3DRMDevice2): HRESULT; stdcall;
    function CreateDeviceFromSurface(const lpGUID: TGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; out lplpD3DRMDevice: IDirect3DRMDevice2):
        HRESULT; stdcall;
    function CreateDeviceFromD3D(lpD3D: IDirect3D2; lpD3DDev: IDirect3DDevice2;
        out lplpD3DRMDevice: IDirect3DRMDevice2): HRESULT; stdcall;
    function CreateDeviceFromClipper(lpDDClipper: IDirectDrawClipper;
        const lpGUID: TGUID; width, height: Integer; out lplpD3DRMDevice:
        IDirect3DRMDevice2): HRESULT; stdcall;
    function CreateTextureFromSurface( lpDDS: IDirectDrawSurface;
        out lplpD3DRMTexture: IDirect3DRMTexture2): HRESULT; stdcall;
    function CreateShadow(lpVisual: IDirect3DRMVisual;
        lpLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: D3DVALUE;
        out lplpShadow: IDirect3DRMVisual): HRESULT; stdcall;
    function CreateViewport(lpDev: IDirect3DRMDevice;
        lpCamera: IDirect3DRMFrame; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        out lplpD3DRMViewport: IDirect3DRMViewport): HRESULT; stdcall;
    function CreateWrap(wraptype: D3DRMWRAPTYPE; lpRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: D3DVALUE;
        out lplpD3DRMWrap: IDirect3DRMWrap): HRESULT; stdcall;
    function CreateUserVisual(fn: D3DRMUSERVISUALCALLBACK; lpArg: Pointer;
        out lplpD3DRMUV: IDirect3DRMUserVisual): HRESULT; stdcall;
    function LoadTexture(lpFileName: LPSTR; out lplpD3DRMTexture:
        IDirect3DRMTexture2): HRESULT; stdcall;
    function LoadTextureFromResource(rs: HRSRC; out lplpD3DRMTexture:
        IDirect3DRMTexture2): HRESULT; stdcall;
    function SetSearchPath(lpPath: LPSTR): HRESULT; stdcall;
    function AddSearchPath(lpPath: LPSTR): HRESULT; stdcall;
    function GetSearchPath(var lpdwSize: DWORD; lpszPath: LPSTR): HRESULT;
         stdcall;
    function SetDefaultTextureColors(dwColors: DWORD): HRESULT; stdcall;
    function SetDefaultTextureShades(dwShades: DWORD): HRESULT; stdcall;
    function GetDevices(out lplpDevArray: IDirect3DRMDeviceArray): HRESULT;
         stdcall;
    function GetNamedObject(lpName: LPSTR; out lplpD3DRMObject:
        IDirect3DRMObject): HRESULT; stdcall;
    function EnumerateObjects(func: D3DRMOBJECTCALLBACK; lpArg: Pointer):
        HRESULT; stdcall;
    function Load(lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: D3DRMLOADOPTIONS; d3drmLoadProc:
        D3DRMLOADCALLBACK; lpArgLP: Pointer; d3drmLoadTextureProc:
        D3DRMLOADTEXTURECALLBACK; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame): HRESULT; stdcall;
    function Tick(d3dvalTick: D3DVALUE): HRESULT; stdcall;
    function CreateProgressiveMesh(out lplpD3DRMProgressiveMesh:
        IDirect3DRMProgressiveMesh): HRESULT; stdcall;
  end;

const
  D3DRM_OK                        = DD_OK;
  D3DRMERR_BADOBJECT              = $88760000 + 781;
  D3DRMERR_BADTYPE                = $88760000 + 782;
  D3DRMERR_BADALLOC               = $88760000 + 783;
  D3DRMERR_FACEUSED               = $88760000 + 784;
  D3DRMERR_NOTFOUND               = $88760000 + 785;
  D3DRMERR_NOTDONEYET             = $88760000 + 786;
  D3DRMERR_FILENOTFOUND           = $88760000 + 787;
  D3DRMERR_BADFILE                = $88760000 + 788;
  D3DRMERR_BADDEVICE              = $88760000 + 789;
  D3DRMERR_BADVALUE               = $88760000 + 790;
  D3DRMERR_BADMAJORVERSION        = $88760000 + 791;
  D3DRMERR_BADMINORVERSION        = $88760000 + 792;
  D3DRMERR_UNABLETOEXECUTE        = $88760000 + 793;
  D3DRMERR_LIBRARYNOTFOUND        = $88760000 + 794;
  D3DRMERR_INVALIDLIBRARY         = $88760000 + 795;
  D3DRMERR_PENDING                = $88760000 + 796;
  D3DRMERR_NOTENOUGHDATA          = $88760000 + 797;
  D3DRMERR_REQUESTTOOLARGE        = $88760000 + 798;
  D3DRMERR_REQUESTTOOSMALL        = $88760000 + 799;
  D3DRMERR_CONNECTIONLOST         = $88760000 + 800;
  D3DRMERR_LOADABORTED            = $88760000 + 801;
  D3DRMERR_NOINTERNET             = $88760000 + 802;
  D3DRMERR_BADCACHEFILE           = $88760000 + 803;
  D3DRMERR_BOXNOTSET              = $88760000 + 804;
  D3DRMERR_BADPMDATA              = $88760000 + 805;

{ Create a Direct3DRM API }
function Direct3DRMCreate(out lplpDirect3DRM: IDirect3DRM): HRESULT; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dinput.h
 *  Content:    DirectInput include file
 *
 ****************************************************************************)

const
{$IFDEF DIRECTX3}
  DIRECTINPUT_VERSION = $0300;
{$ELSE}
  DIRECTINPUT_VERSION = $0500;
{$ENDIF}

{ Class IDs }

const
  CLSID_DirectInput: TGUID = (D1:$25E609E0;D2:$B259;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  CLSID_DirectInputDevice: TGUID = (D1:$25E609E1;D2:$B259;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

{ Interfaces }

const
  IID_IDirectInput: TGUID = (D1:$89521360;D2:$AA8A;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputA: TGUID = (D1:$89521360;D2:$AA8A;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputW: TGUID = (D1:$89521361;D2:$AA8A;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInput2: TGUID = (D1:$5944E662;D2:$AA8A;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInput2A: TGUID = (D1:$5944E662;D2:$AA8A;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInput2W: TGUID = (D1:$5944E663;D2:$AA8A;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputDevice: TGUID = (D1:$5944E680;D2:$C92E;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputDeviceA: TGUID = (D1:$5944E680;D2:$C92E;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputDeviceW: TGUID = (D1:$5944E681;D2:$C92E;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputDevice2: TGUID = (D1:$5944E682;D2:$C92E;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputDevice2A: TGUID = (D1:$5944E682;D2:$C92E;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputDevice2W: TGUID = (D1:$5944E683;D2:$C92E;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  IID_IDirectInputEffect: TGUID = (D1:$E7E1F7C0;D2:$88D2;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));

{ Predefined object types }

const
  GUID_XAxis: TGUID = (D1:$A36D02E0;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_YAxis: TGUID = (D1:$A36D02E1;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_ZAxis: TGUID = (D1:$A36D02E2;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_RxAxis: TGUID = (D1:$A36D02F4;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_RyAxis: TGUID = (D1:$A36D02F5;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_RzAxis: TGUID = (D1:$A36D02E3;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Slider: TGUID = (D1:$A36D02E4;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Button: TGUID = (D1:$A36D02F0;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Key: TGUID = (D1:$55728220;D2:$D33C;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_POV: TGUID = (D1:$A36D02F2;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Unknown: TGUID = (D1:$A36D02F3;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

{ Predefined product GUIDs }

const
  GUID_SysMouse: TGUID = (D1:$6F1D2B60;D2:$D5A0;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_SysKeyboard: TGUID = (D1:$6F1D2B61;D2:$D5A0;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Joystick: TGUID = (D1:$6F1D2B70;D2:$D5A0;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

{ Predefined force feedback effects }

const
  GUID_ConstantForce: TGUID = (D1:$13541C20;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_RampForce: TGUID = (D1:$13541C21;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Square: TGUID = (D1:$13541C22;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Sine: TGUID = (D1:$13541C23;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Triangle: TGUID = (D1:$13541C24;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_SawtoothUp: TGUID = (D1:$13541C25;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_SawtoothDown: TGUID = (D1:$13541C26;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Spring: TGUID = (D1:$13541C27;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Damper: TGUID = (D1:$13541C28;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Inertia: TGUID = (D1:$13541C29;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Friction: TGUID = (D1:$13541C2A;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_CustomForce: TGUID = (D1:$13541C2B;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));

{ IDirectInputEffect }

const
  DIEFT_ALL                = $00000000;

  DIEFT_CONSTANTFORCE      = $00000001;
  DIEFT_RAMPFORCE          = $00000002;
  DIEFT_PERIODIC           = $00000003;
  DIEFT_CONDITION          = $00000004;
  DIEFT_CUSTOMFORCE        = $00000005;
  DIEFT_HARDWARE           = $000000FF;

  DIEFT_FFATTACK           = $00000200;
  DIEFT_FFFADE             = $00000400;
  DIEFT_SATURATION         = $00000800;
  DIEFT_POSNEGCOEFFICIENTS = $00001000;
  DIEFT_POSNEGSATURATION   = $00002000;
  DIEFT_DEADBAND           = $00004000;

function DIEFT_GETTYPE(n: DWORD): DWORD;

const
  DI_DEGREES               = 100;
  DI_FFNOMINALMAX          = 10000;
  DI_SECONDS               = 1000000;

type
  DICONSTANTFORCE = record
    lMagnitude: Longint;
  end;
  LPDICONSTANTFORCE = ^DICONSTANTFORCE;

  DIRAMPFORCE = record
    lStart: Longint;
    lEnd: Longint;
  end;
  LPDIRAMPFORCE = ^DIRAMPFORCE;

  DIPERIODIC = record
    dwMagnitude: DWORD;
    lOffset: Longint;
    dwPhase: DWORD;
    dwPeriod: DWORD;
  end;
  LPDIPERIODIC = ^DIPERIODIC;

  DICONDITION = record
    lOffset: Longint;
    lPositiveCoefficient: Longint;
    lNegativeCoefficient: Longint;
    dwPositiveSaturation: DWORD;
    dwNegativeSaturation: DWORD;
    lDeadBand: Longint;
  end;
  LPDICONDITION = ^DICONDITION;

  DICUSTOMFORCE = record
    cChannels: DWORD;
    dwSamplePeriod: DWORD;
    cSamples: DWORD;
    rglForceData: PLongint;
  end;
  LPDICUSTOMFORCE = ^DICUSTOMFORCE;

  DIENVELOPE = record
    dwSize: DWORD;                   // sizeof(DIENVELOPE)
    dwAttackLevel: DWORD;
    dwAttackTime: DWORD;             // Microseconds
    dwFadeLevel: DWORD;
    dwFadeTime: DWORD;               // Microseconds
  end;
  LPDIENVELOPE = ^DIENVELOPE;

  DIEFFECT = record
    dwSize: DWORD;                   // sizeof(DIEFFECT)
    dwFlags: DWORD;                  // DIEFF_*
    dwDuration: DWORD;               // Microseconds
    dwSamplePeriod: DWORD;           // Microseconds
    dwGain: DWORD;
    dwTriggerButton: DWORD;          // or DIEB_NOTRIGGER
    dwTriggerRepeatInterval: DWORD;  // Microseconds
    cAxes: DWORD;                    // Number of axes
    rgdwAxes: LPDWORD;               // Array of axes
    rglDirection: PLongint;          // Array of directions
    lpEnvelope: LPDIENVELOPE;        // Optional
    cbTypeSpecificParams: DWORD;     // Size of params
    lpvTypeSpecificParams: Pointer;  // Pointer to params
  end;
  LPDIEFFECT = ^DIEFFECT;

const
  DIEFF_OBJECTIDS             = $00000001;
  DIEFF_OBJECTOFFSETS         = $00000002;
  DIEFF_CARTESIAN             = $00000010;
  DIEFF_POLAR                 = $00000020;
  DIEFF_SPHERICAL             = $00000040;

  DIEP_DURATION               = $00000001;
  DIEP_SAMPLEPERIOD           = $00000002;
  DIEP_GAIN = $00000004;
  DIEP_TRIGGERBUTTON          = $00000008;
  DIEP_TRIGGERREPEATINTERVAL  = $00000010;
  DIEP_AXES                   = $00000020;
  DIEP_DIRECTION              = $00000040;
  DIEP_ENVELOPE               = $00000080;
  DIEP_TYPESPECIFICPARAMS     = $00000100;
  DIEP_ALLPARAMS              = $000001FF;
  DIEP_START                  = $20000000;
  DIEP_NORESTART              = $40000000;
  DIEP_NODOWNLOAD             = $80000000;
  DIEB_NOTRIGGER              = $FFFFFFFF;

  DIES_SOLO                   = $00000001;
  DIES_NODOWNLOAD             = $80000000;

  DIEGES_PLAYING              = $00000001;
  DIEGES_EMULATED             = $00000002;

type
  DIEFFESCAPE = record
    dwSize: DWORD;
    dwCommand: DWORD;
    lpvInBuffer: Pointer;
    cbInBuffer: DWORD;
    lpvOutBuffer: Pointer;
    cbOutBuffer: DWORD;
  end;
  LPDIEFFESCAPE = ^DIEFFESCAPE;

  IDirectInputEffect = interface(IUnknown)
    ['{E7E1F7C0-88D2-11D0-9AD0-00A0C9A06E35}']
    (*** IDirectInputEffect methods ***)
    function Initialize(hinst: THandle; dwVersion: DWORD; const rguid: TGUID):
        HRESULT; stdcall;
    function GetEffectGuid(var pguid: TGUID): HRESULT; stdcall;
    function GetParameters(var peff: DIEFFECT; dwFlags: DWORD): HRESULT;
        stdcall;
    function SetParameters(const peff: DIEFFECT; dwFlags: DWORD): HRESULT;
        stdcall;
    function Start(dwIterations: DWORD; dwFlags: DWORD): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function GetEffectStatus(var pdwFlags: DWORD): HRESULT; stdcall;
    function DownLoad: HRESULT; stdcall;
    function Unload: HRESULT; stdcall;
    function Escape(const pesc: DIEFFESCAPE): HRESULT; stdcall;
  end;

{ IDirectInputDevice }

const
  DIDEVTYPE_DEVICE   = 1;
  DIDEVTYPE_MOUSE    = 2;
  DIDEVTYPE_KEYBOARD = 3;
  DIDEVTYPE_JOYSTICK = 4;
  DIDEVTYPE_HID      = $00010000;

  DIDEVTYPEMOUSE_UNKNOWN     = 1;
  DIDEVTYPEMOUSE_TRADITIONAL = 2;
  DIDEVTYPEMOUSE_FINGERSTICK = 3;
  DIDEVTYPEMOUSE_TOUCHPAD    = 4;
  DIDEVTYPEMOUSE_TRACKBALL   = 5;

  DIDEVTYPEKEYBOARD_UNKNOWN     = 0;
  DIDEVTYPEKEYBOARD_PCXT        = 1;
  DIDEVTYPEKEYBOARD_OLIVETTI    = 2;
  DIDEVTYPEKEYBOARD_PCAT        = 3;
  DIDEVTYPEKEYBOARD_PCENH       = 4;
  DIDEVTYPEKEYBOARD_NOKIA1050   = 5;
  DIDEVTYPEKEYBOARD_NOKIA9140   = 6;
  DIDEVTYPEKEYBOARD_NEC98       = 7;
  DIDEVTYPEKEYBOARD_NEC98LAPTOP = 8;
  DIDEVTYPEKEYBOARD_NEC98106    = 9;
  DIDEVTYPEKEYBOARD_JAPAN106    = 10;
  DIDEVTYPEKEYBOARD_JAPANAX     = 11;
  DIDEVTYPEKEYBOARD_J3100       = 12;

  DIDEVTYPEJOYSTICK_UNKNOWN     = 1;
  DIDEVTYPEJOYSTICK_TRADITIONAL = 2;
  DIDEVTYPEJOYSTICK_FLIGHTSTICK = 3;
  DIDEVTYPEJOYSTICK_GAMEPAD     = 4;
  DIDEVTYPEJOYSTICK_RUDDER      = 5;
  DIDEVTYPEJOYSTICK_WHEEL       = 6;
  DIDEVTYPEJOYSTICK_HEADTRACKER = 7;

function GET_DIDEVICE_TYPE(dwDevType: DWORD): DWORD;
function GET_DIDEVICE_SUBTYPE(dwDevType: DWORD): DWORD;

type
  DIDEVCAPS = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDevType: DWORD;
    dwAxes: DWORD;
    dwButtons: DWORD;
    dwPOVs: DWORD;
{$IFNDEF DIRECTX3}
    dwFFSamplePeriod: DWORD;
    dwFFMinTimeResolution: DWORD;
    dwFirmwareRevision: DWORD;
    dwHardwareRevision: DWORD;
    dwFFDriverVersion: DWORD;
{$ENDIF}
  end;
  LPDIDEVCAPS = ^DIDEVCAPS;

const
  DIDC_ATTACHED           = $00000001;
  DIDC_POLLEDDEVICE       = $00000002;
  DIDC_EMULATED           = $00000004;
  DIDC_POLLEDDATAFORMAT   = $00000008;

  DIDC_FORCEFEEDBACK      = $00000100;
  DIDC_FFATTACK           = $00000200;
  DIDC_FFFADE             = $00000400;
  DIDC_SATURATION         = $00000800;
  DIDC_POSNEGCOEFFICIENTS = $00001000;
  DIDC_POSNEGSATURATION   = $00002000;
  DIDC_DEADBAND           = $00004000;

  DIDFT_ALL        = $00000000;

  DIDFT_RELAXIS    = $00000001;
  DIDFT_ABSAXIS    = $00000002;
  DIDFT_AXIS       = $00000003;

  DIDFT_PSHBUTTON  = $00000004;
  DIDFT_TGLBUTTON  = $00000008;
  DIDFT_BUTTON     = $0000000C;

  DIDFT_POV        = $00000010;

  DIDFT_COLLECTION = $00000040;
  DIDFT_NODATA     = $00000080;

  DIDFT_ANYINSTANCE = $00FFFF00;
  DIDFT_INSTANCEMASK = DIDFT_ANYINSTANCE;

function DIDFT_MAKEINSTANCE(n: WORD): DWORD;
function DIDFT_GETINSTANCE(n: DWORD): WORD;

const
  DIDFT_FFACTUATOR = $01000000;
  DIDFT_FFEFFECTTRIGGER = $02000000;

function DIDFT_ENUMCOLLECTION(n: WORD): DWORD;

const
  DIDFT_NOCOLLECTION = $00FFFF00;

type
  DIOBJECTDATAFORMAT = record
    pguid: PGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
  end;
  LPDIOBJECTDATAFORMAT = ^DIOBJECTDATAFORMAT;

  DIDATAFORMAT = record
    dwSize: DWORD;
    dwObjSize: DWORD;
    dwFlags: DWORD;
    dwDataSize: DWORD;
    dwNumObjs: DWORD;
    rgodf: LPDIOBJECTDATAFORMAT;
  end;
  LPDIDATAFORMAT = ^DIDATAFORMAT;

const
  DIDF_ABSAXIS = $00000001;
  DIDF_RELAXIS = $00000002;

type
  DIDEVICEOBJECTINSTANCEA = record
    dwSize: DWORD;
    guidType: TGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: array[0..MAX_PATH-1] of CHAR;
{$IFNDEF DIRECTX3}
    dwFFMaxForce: DWORD;
    dwFFForceResolution: DWORD;
    wCollectionNumber: WORD;
    wDesignatorIndex: WORD;
    wUsagePage: WORD;
    wUsage: WORD;
    dwDimension: DWORD;
    wExponent: WORD;
    wReserved: WORD;
{$ENDIF}
  end;
  LPDIDEVICEOBJECTINSTANCEA = ^DIDEVICEOBJECTINSTANCEA;

  DIDEVICEOBJECTINSTANCEW = record
    dwSize: DWORD;
    guidType: TGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: array[0..MAX_PATH-1] of WCHAR;
{$IFNDEF DIRECTX3}
    dwFFMaxForce: DWORD;
    dwFFForceResolution: DWORD;
    wCollectionNumber: WORD;
    wDesignatorIndex: WORD;
    wUsagePage: WORD;
    wUsage: WORD;
    dwDimension: DWORD;
    wExponent: WORD;
    wReserved: WORD;
{$ENDIF}
  end;
  LPDIDEVICEOBJECTINSTANCEW = ^DIDEVICEOBJECTINSTANCEW;

  DIDEVICEOBJECTINSTANCE = DIDEVICEOBJECTINSTANCEA;
  LPDIDEVICEOBJECTINSTANCE = LPDIDEVICEOBJECTINSTANCEA;

  LPDIENUMDEVICEOBJECTSCALLBACKA = function(const peff: DIDEVICEOBJECTINSTANCEA;
      pvRef: Pointer): HRESULT; stdcall;
  LPDIENUMDEVICEOBJECTSCALLBACKW = function(const peff: DIDEVICEOBJECTINSTANCEW;
      pvRef: Pointer): HRESULT; stdcall;

  LPDIENUMDEVICEOBJECTSCALLBACK = LPDIENUMDEVICEOBJECTSCALLBACKA;

const
  DIDOI_FFACTUATOR      = $00000001;
  DIDOI_FFEFFECTTRIGGER = $00000002;
  DIDOI_POLLED          = $00008000;
  DIDOI_ASPECTPOSITION  = $00000100;
  DIDOI_ASPECTVELOCITY  = $00000200;
  DIDOI_ASPECTACCEL     = $00000300;
  DIDOI_ASPECTFORCE     = $00000400;
  DIDOI_ASPECTMASK      = $00000F00;

type
  DIPROPHEADER = record
    dwSize: DWORD;
    dwHeaderSize: DWORD;
    dwObj: DWORD;
    dwHow: DWORD;
  end;
  LPDIPROPHEADER = ^DIPROPHEADER;

const
  DIPH_DEVICE   = 0;
  DIPH_BYOFFSET = 1;
  DIPH_BYID     = 2;

type
  DIPROPDWORD = record
    diph: DIPROPHEADER;
    dwData: DWORD;
  end;
  LPDIPROPDWORD = ^DIPROPDWORD;

  DIPROPRANGE = record
    diph: DIPROPHEADER;
    lMin: Longint;
    lMax: Longint;
  end;
  LPDIPROPRANGE = ^DIPROPRANGE;

const
  DIPROPRANGE_NOMIN   = $80000000;
  DIPROPRANGE_NOMAX   = $7FFFFFFF;

  DIPROP_BUFFERSIZE   = PGUID(1);
  DIPROP_AXISMODE     = PGUID(2);

  DIPROPAXISMODE_ABS  = 0;
  DIPROPAXISMODE_REL  = 1;

  DIPROP_GRANULARITY  = PGUID(3);
  DIPROP_RANGE        = PGUID(4);
  DIPROP_DEADZONE     = PGUID(5);
  DIPROP_SATURATION   = PGUID(6);
  DIPROP_FFGAIN       = PGUID(7);
  DIPROP_FFLOAD       = PGUID(8);
  DIPROP_AUTOCENTER   = PGUID(9);

  DIPROPAUTOCENTER_OFF = 0;
  DIPROPAUTOCENTER_ON  = 1;

  DIPROP_CALIBRATIONMODE = PGUID(10);

  DIPROPCALIBRATIONMODE_COOKED = 0;
  DIPROPCALIBRATIONMODE_RAW    = 1;

type
  DIDEVICEOBJECTDATA = record
    dwOfs: DWORD;
    dwData: DWORD;
    dwTimeStamp: DWORD;
    dwSequence: DWORD;
  end;
  LPDIDEVICEOBJECTDATA = ^DIDEVICEOBJECTDATA;

const
  DIGDD_PEEK = $00000001;

  DISCL_EXCLUSIVE    = $00000001;
  DISCL_NONEXCLUSIVE = $00000002;
  DISCL_FOREGROUND   = $00000004;
  DISCL_BACKGROUND   = $00000008;

type
  DIDEVICEINSTANCEA = record
    dwSize: DWORD;
    guidInstance: TGUID;
    guidProduct: TGUID;
    dwDevType: DWORD;
    tszInstanceName: array[0..MAX_PATH-1] of CHAR;
    tszProductName: array[0..MAX_PATH-1] of CHAR;
{$IFNDEF DIRECTX3}
    guidFFDriver: TGUID;
    wUsagePage: WORD;
    wUsage: WORD;
{$ENDIF}
  end;
  LPDIDEVICEINSTANCEA = ^DIDEVICEINSTANCEA;

  DIDEVICEINSTANCEW = record
    dwSize: DWORD;
    guidInstance: TGUID;
    guidProduct: TGUID;
    dwDevType: DWORD;
    tszInstanceName: array[0..MAX_PATH-1] of WCHAR;
    tszProductName: array[0..MAX_PATH-1] of WCHAR;
{$IFNDEF DIRECTX3}
    guidFFDriver: TGUID;
    wUsagePage: WORD;
    wUsage: WORD;
{$ENDIF}
  end;
  LPDIDEVICEINSTANCEW = ^DIDEVICEINSTANCEW;

  DIDEVICEINSTANCE = DIDEVICEINSTANCEA;
  LPDIDEVICEINSTANCE = LPDIDEVICEINSTANCEA;

  IDirectInputDeviceW = interface(IUnknown)
    ['{5944E681-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDeviceW methods ***)
    function GetCapabilities(var lpDIDevCaps: DIDEVCAPS): HRESULT; stdcall;
    function EnumObjects(lpCallback: LPDIENUMDEVICEOBJECTSCALLBACKW;
        pvRef: Pointer; dwFlags: DWORD): HRESULT; stdcall;
    function GetProperty(rguidProp: PGUID; var pdiph: DIPROPHEADER): HRESULT;
        stdcall;
    function SetProperty(rguidProp: PGUID; const pdiph: DIPROPHEADER): HRESULT;
        stdcall;
    function Acquire: HRESULT; stdcall;
    function Unacquire: HRESULT; stdcall;
    function GetDeviceState(cbData: DWORD; var lpvData): HRESULT; stdcall;
    function GetDeviceData(cbObjectData: DWORD; var rgdod: DIDEVICEOBJECTDATA;
        var pdwInOut: DWORD; dwFlags: DWORD): HRESULT; stdcall;
    function SetDataFormat(const lpdf: DIDATAFORMAT): HRESULT; stdcall;
    function SetEventNotification(hEvent: THandle): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function GetObjectInfo(var pdidoi: DIDEVICEOBJECTINSTANCEW; dwObj: DWORD;
        dwHow: DWORD): HRESULT; stdcall;
    function GetDeviceInfo(var pdidi: DIDEVICEINSTANCEW): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD; const rguid: TGUID):
        HRESULT; stdcall;
  end;

  IDirectInputDeviceA = interface(IUnknown)
    ['{5944E680-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDeviceA methods ***)
    function GetCapabilities(var lpDIDevCaps: DIDEVCAPS): HRESULT; stdcall;
    function EnumObjects(lpCallback: LPDIENUMDEVICEOBJECTSCALLBACKA;
        pvRef: Pointer; dwFlags: DWORD): HRESULT; stdcall;
    function GetProperty(rguidProp: PGUID; var pdiph: DIPROPHEADER): HRESULT;
        stdcall;
    function SetProperty(rguidProp: PGUID; const pdiph: DIPROPHEADER): HRESULT;
        stdcall;
    function Acquire: HRESULT; stdcall;
    function Unacquire: HRESULT; stdcall;
    function GetDeviceState(cbData: DWORD; var lpvData): HRESULT; stdcall;
    function GetDeviceData(cbObjectData: DWORD; var rgdod: DIDEVICEOBJECTDATA;
        var pdwInOut: DWORD; dwFlags: DWORD): HRESULT; stdcall;
    function SetDataFormat(const lpdf: DIDATAFORMAT): HRESULT; stdcall;
    function SetEventNotification(hEvent: THandle): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function GetObjectInfo(var pdidoi: DIDEVICEOBJECTINSTANCEA; dwObj: DWORD;
        dwHow: DWORD): HRESULT; stdcall;
    function GetDeviceInfo(var pdidi: DIDEVICEINSTANCEA): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD; const rguid: TGUID):
        HRESULT; stdcall;
  end;

  IDirectInputDevice = IDirectInputDeviceA;

const
  DISFFC_RESET           = $00000001;
  DISFFC_STOPALL         = $00000002;
  DISFFC_PAUSE           = $00000004;
  DISFFC_CONTINUE        = $00000008;
  DISFFC_SETACTUATORSON  = $00000010;
  DISFFC_SETACTUATORSOFF = $00000020;

  DIGFFS_EMPTY           = $00000001;
  DIGFFS_STOPPED         = $00000002;
  DIGFFS_PAUSED          = $00000004;
  DIGFFS_ACTUATORSON     = $00000010;
  DIGFFS_ACTUATORSOFF    = $00000020;
  DIGFFS_POWERON         = $00000040;
  DIGFFS_POWEROFF        = $00000080;
  DIGFFS_SAFETYSWITCHON  = $00000100;
  DIGFFS_SAFETYSWITCHOFF = $00000200;
  DIGFFS_USERFFSWITCHON  = $00000400;
  DIGFFS_USERFFSWITCHOFF = $00000800;
  DIGFFS_DEVICELOST      = $80000000;

type
  DIEFFECTINFOA = record
    dwSize: DWORD;
    guid: TGUID;
    dwEffType: DWORD;
    dwStaticParams: DWORD;
    dwDynamicParams: DWORD;
    tszName: array[0..MAX_PATH-1] of CHAR;
  end;
  LPDIEFFECTINFOA = ^DIEFFECTINFOA;

  DIEFFECTINFOW = record
    dwSize: DWORD;
    guid: TGUID;
    dwEffType: DWORD;
    dwStaticParams: DWORD;
    dwDynamicParams: DWORD;
    tszName: array[0..MAX_PATH-1] of WCHAR;
  end;
  LPDIEFFECTINFOW = ^DIEFFECTINFOW;

  DIEFFECTINFO = DIEFFECTINFOA;
  LPDIEFFECTINFO = LPDIEFFECTINFOA;

  LPDIENUMEFFECTSCALLBACKA = function(const pdei: DIEFFECTINFOA;
      pvRef: Pointer): HRESULT; stdcall;
  LPDIENUMEFFECTSCALLBACKW = function(const pdei: DIEFFECTINFOW;
      pvRef: Pointer): HRESULT; stdcall;

  LPDIENUMEFFECTSCALLBACK = LPDIENUMEFFECTSCALLBACKA;

  LPDIENUMCREATEDEFFECTOBJECTSCALLBACK = function(const peff:
      IDirectInputEffect; pvRef: Pointer): HRESULT; stdcall;

  IDirectInputDevice2W = interface(IDirectInputDeviceW)
    ['{5944E683-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDevice2W methods ***)
    function CreateEffect(const rguid: TGUID; const lpeff: DIEFFECT;
        out ppdeff: IDirectInputEffect; punkOuter: IUnknown): HRESULT; stdcall;
    function EnumEffects(lpCallback: LPDIENUMEFFECTSCALLBACKW; pvRef: Pointer;
        dwEffType: DWORD): HRESULT; stdcall;
    function GetEffectInfo(var pdei: DIEFFECTINFOW; const rguid: TGUID):
       HRESULT; stdcall;
    function GetForceFeedbackState(var pdwOut: DWORD): HRESULT; stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD): HRESULT; stdcall;
    function EnumCreatedEffectObjects(lpCallback:
        LPDIENUMCREATEDEFFECTOBJECTSCALLBACK; pvRef: Pointer; fl: DWORD):
        HRESULT; stdcall;
    function Escape(const pesc: DIEFFESCAPE): HRESULT; stdcall;
    function Poll: HRESULT; stdcall;
    function SendDeviceData(Arg1: DWORD; Arg2: LPDIDEVICEOBJECTDATA;
        Arg3: LPDWORD; Arg4: DWORD): HRESULT; stdcall;
  end;

  IDirectInputDevice2A = interface(IDirectInputDeviceA)
    ['{5944E682-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDevice2A methods ***)
    function CreateEffect(const rguid: TGUID; const lpeff: DIEFFECT;
        out ppdeff: IDirectInputEffect; punkOuter: IUnknown): HRESULT; stdcall;
    function EnumEffects(lpCallback: LPDIENUMEFFECTSCALLBACKA; pvRef: Pointer;
        dwEffType: DWORD): HRESULT; stdcall;
    function GetEffectInfo(var pdei: DIEFFECTINFOA; const rguid: TGUID):
        HRESULT; stdcall;
    function GetForceFeedbackState(var pdwOut: DWORD): HRESULT; stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD): HRESULT; stdcall;
    function EnumCreatedEffectObjects(lpCallback:
        LPDIENUMCREATEDEFFECTOBJECTSCALLBACK; pvRef: Pointer; fl: DWORD):
        HRESULT; stdcall;
    function Escape(const pesc: DIEFFESCAPE): HRESULT; stdcall;
    function Poll: HRESULT; stdcall;
    function SendDeviceData(Arg1: DWORD; Arg2: LPDIDEVICEOBJECTDATA;
        Arg3: LPDWORD; Arg4: DWORD): HRESULT; stdcall;
  end;

  IDirectInputDevice2 = IDirectInputDevice2A;

{ Mouse }

type
  DIMOUSESTATE = record
    lX: Longint;
    lY: Longint;
    lZ: Longint;
    rgbButtons: array[0..3] of BYTE;
  end;

const
  _c_dfDIMouse_Objects: array[0..1] of DIOBJECTDATAFORMAT = (
    (  pguid: nil;
       dwOfs: 0;
       dwType: DIDFT_RELAXIS or DIDFT_ANYINSTANCE;
       dwFlags: 0),
    (  pguid: @GUID_Button;
       dwOfs: 12;
       dwType: DIDFT_BUTTON or DIDFT_ANYINSTANCE;
       dwFlags: 0)
  );

  c_dfDIMouse: DIDATAFORMAT = (
      dwSize: Sizeof(c_dfDIMouse);
      dwObjSize: Sizeof(DIOBJECTDATAFORMAT);
      dwFlags: DIDF_RELAXIS;
      dwDataSize: Sizeof(DIMOUSESTATE);
      dwNumObjs: High(_c_dfDIMouse_Objects)+1;
      rgodf: @_c_dfDIMouse_Objects);

{ Keyboard }

type
  DIKEYBOARDSTATE = array[0..255] of Byte;

const
  _c_dfDIKeyboard_Objects: array[0..0] of DIOBJECTDATAFORMAT = (
    (  pguid: @GUID_Key;
       dwOfs: 1;
       dwType: DIDFT_BUTTON or DIDFT_ANYINSTANCE;
       dwFlags: 0)
  );

  c_dfDIKeyboard: DIDATAFORMAT = (
      dwSize: Sizeof(c_dfDIKeyboard);
      dwObjSize: Sizeof(DIOBJECTDATAFORMAT);
      dwFlags: 0;
      dwDataSize: SizeOf(DIKEYBOARDSTATE);
      dwNumObjs: High(_c_dfDIKeyboard_Objects)+1;
      rgodf: @_c_dfDIKeyboard_Objects);

{ DirectInput keyboard scan codes }

const
  DIK_ESCAPE       = $01;
  DIK_1            = $02;
  DIK_2            = $03;
  DIK_3            = $04;
  DIK_4            = $05;
  DIK_5            = $06;
  DIK_6            = $07;
  DIK_7            = $08;
  DIK_8            = $09;
  DIK_9            = $0A;
  DIK_0            = $0B;
  DIK_EQUALS       = $0D;
  DIK_BACK         = $0E;
  DIK_TAB          = $0F;
  DIK_Q            = $10;
  DIK_W            = $11;
  DIK_E            = $12;
  DIK_R            = $13;
  DIK_T            = $14;
  DIK_Y            = $15;
  DIK_U            = $16;
  DIK_I            = $17;
  DIK_O            = $18;
  DIK_P            = $19;
  DIK_LBRACKET     = $1A;
  DIK_RBRACKET     = $1B;
  DIK_LCONTROL     = $1D;
  DIK_A            = $1E;
  DIK_S            = $1F;
  DIK_D            = $20;
  DIK_F            = $21;
  DIK_G            = $22;
  DIK_H            = $23;
  DIK_J            = $24;
  DIK_K            = $25;
  DIK_L            = $26;
  DIK_SEMICOLON    = $27;
  DIK_APOSTROPHE   = $28;
  DIK_LSHIFT       = $2A;
  DIK_BACKSLASH    = $2B;
  DIK_Z            = $2C;
  DIK_X            = $2D;
  DIK_C            = $2E;
  DIK_V            = $2F;
  DIK_B            = $30;
  DIK_N            = $31;
  DIK_M            = $32;
  DIK_COMMA        = $33;
  DIK_PERIOD       = $34;     // . on main keyboard
  DIK_SLASH        = $35;     // / on main keyboard
  DIK_RSHIFT       = $36;
  DIK_MULTIPLY     = $37;     // * on main keyboard
  DIK_LMENU        = $38;     // left Alt
  DIK_SPACE        = $39;
  DIK_CAPITAL      = $3A;
  DIK_F1           = $3B;
  DIK_F2           = $3C;
  DIK_F3           = $3D;
  DIK_F4           = $3E;
  DIK_F5           = $3F;
  DIK_F6           = $40;
  DIK_F7           = $41;
  DIK_F8           = $42;
  DIK_F9           = $43;
  DIK_F10          = $44;
  DIK_NUMLOCK      = $45;
  DIK_SCROLL       = $46;     // Scroll Lock
  DIK_NUMPAD7      = $47;
  DIK_NUMPAD8      = $48;
  DIK_NUMPAD9      = $49;
  DIK_SUBTRACT     = $4A;     // - on numeric keypad
  DIK_NUMPAD4      = $4B;
  DIK_NUMPAD5      = $4C;
  DIK_NUMPAD6      = $4D;
  DIK_ADD          = $4E;     // + on numeric keypad
  DIK_NUMPAD1      = $4F;
  DIK_NUMPAD2      = $50;
  DIK_NUMPAD3      = $51;
  DIK_NUMPAD0      = $52;
  DIK_DECIMAL      = $53;     // . on numeric keypad
  DIK_F11          = $57;
  DIK_F12          = $58;

  DIK_F13          = $64;     //                     (NEC PC98)
  DIK_F14          = $65;     //                     (NEC PC98)
  DIK_F15          = $66;     //                     (NEC PC98)

  DIK_KANA         = $70;     // (Japanese keyboard)
  DIK_CONVERT      = $79;     // (Japanese keyboard)
  DIK_NOCONVERT    = $7B;     // (Japanese keyboard)
  DIK_YEN          = $7D;     // (Japanese keyboard)
  DIK_NUMPADEQUALS = $8D;     // = on numeric keypad (NEC PC98)
  DIK_CIRCUMFLEX   = $90;     // (Japanese keyboard)
  DIK_AT           = $91;     //                     (NEC PC98)
  DIK_COLON        = $92;     //                     (NEC PC98)
  DIK_UNDERLINE    = $93;     //                     (NEC PC98)
  DIK_KANJI        = $94;     // (Japanese keyboard)
  DIK_STOP         = $95;     //                     (NEC PC98)
  DIK_AX           = $96;     //                     (Japan AX)
  DIK_UNLABELED    = $97;     //                        (J3100)
  DIK_NUMPADENTER  = $9C;     // Enter on numeric keypad
  DIK_RCONTROL     = $9D;
  DIK_NUMPADCOMMA  = $B3;     // , on numeric keypad (NEC PC98)
  DIK_DIVIDE       = $B5;     // / on numeric keypad
  DIK_SYSRQ        = $B7;
  DIK_RMENU        = $B8;     // right Alt
  DIK_HOME         = $C7;     // Home on arrow keypad
  DIK_UP           = $C8;     // UpArrow on arrow keypad
  DIK_PRIOR        = $C9;     // PgUp on arrow keypad
  DIK_LEFT         = $CB;     // LeftArrow on arrow keypad
  DIK_RIGHT        = $CD;     // RightArrow on arrow keypad
  DIK_END          = $CF;     // End on arrow keypad
  DIK_DOWN         = $D0;     // DownArrow on arrow keypad
  DIK_NEXT         = $D1;     // PgDn on arrow keypad
  DIK_INSERT       = $D2;     // Insert on arrow keypad
  DIK_DELETE       = $D3;     // Delete on arrow keypad
  DIK_LWIN         = $DB;     // Left Windows key
  DIK_RWIN         = $DC;     // Right Windows key
  DIK_APPS         = $DD;     // AppMenu key

{ Alternate names for keys, to facilitate transition from DOS. }

  DIK_BACKSPACE    = DIK_BACK;            // backspace
  DIK_NUMPADSTAR   = DIK_MULTIPLY;        // * on numeric keypad
  DIK_LALT         = DIK_LMENU;           // left Alt
  DIK_CAPSLOCK     = DIK_CAPITAL;         // CapsLock
  DIK_NUMPADMINUS  = DIK_SUBTRACT;        // - on numeric keypad
  DIK_NUMPADPLUS   = DIK_ADD;             // + on numeric keypad
  DIK_NUMPADPERIOD = DIK_DECIMAL;         // . on numeric keypad
  DIK_NUMPADSLASH  = DIK_DIVIDE;          // / on numeric keypad
  DIK_RALT         = DIK_RMENU;           // right Alt
  DIK_UPARROW      = DIK_UP;              // UpArrow on arrow keypad
  DIK_PGUP         = DIK_PRIOR;           // PgUp on arrow keypad
  DIK_LEFTARROW    = DIK_LEFT;            // LeftArrow on arrow keypad
  DIK_RIGHTARROW   = DIK_RIGHT;           // RightArrow on arrow keypad
  DIK_DOWNARROW    = DIK_DOWN;            // DownArrow on arrow keypad
  DIK_PGDN         = DIK_NEXT;            // PgDn on arrow keypad

{ Joystick }

type
  DIJOYSTATE = record
    lX: Longint;                        // x-axis position
    lY: Longint;                        // y-axis position
    lZ: Longint;                        // z-axis position
    lRx: Longint;                       // x-axis rotation
    lRy: Longint;                       // y-axis rotation
    lRz: Longint;                       // z-axis rotation
    rglSlider: array[0..1] of Longint;  // extra axes positions
    rgdwPOV: array[0..3] of DWORD;      // POV directions
    rgbButtons: array[0..31] of BYTE;   // 32 buttons
  end;

  DIJOYSTATE2 = record
    lX: Longint;                        // x-axis position
    lY: Longint;                        // y-axis position
    lZ: Longint;                        // z-axis position
    lRx: Longint;                       // x-axis rotation
    lRy: Longint;                       // y-axis rotation
    lRz: Longint;                       // z-axis rotation
    rglSlider: array[0..1] of Longint;  // extra axes positions
    rgdwPOV: array[0..3] of DWORD;      // POV directions
    rgbButtons: array[0..127] of BYTE;  // 128 buttons
    lVX: Longint;                       // x-axis velocity
    lVY: Longint;                       // y-axis velocity
    lVZ: Longint;                       // z-axis velocity
    lVRx: Longint;                      // x-axis angular velocity
    lVRy: Longint;                      // y-axis angular velocity
    lVRz: Longint;                      // z-axis angular velocity
    rglVSlider: array[0..1] of Longint; // extra axes velocities
    lAX: Longint;                       // x-axis acceleration
    lAY: Longint;                       // y-axis acceleration
    lAZ: Longint;                       // z-axis acceleration
    lARx: Longint;                      // x-axis angular acceleration
    lARy: Longint;                      // y-axis angular acceleration
    lARz: Longint;                      // z-axis angular acceleration
    rglASlider: array[0..1] of Longint; // extra axes accelerations
    lFX: Longint;                       // x-axis force
    lFY: Longint;                       // y-axis force
    lFZ: Longint;                       // z-axis force
    lFRx: Longint;                      // x-axis torque
    lFRy: Longint;                      // y-axis torque
    lFRz: Longint;                      // z-axis torque
    rglFSlider: array[0..1] of Longint; // extra axes forces
  end;
{
const
  _c_dfDIJoystick_Objects: array[0..1] of DIOBJECTDATAFORMAT = (
    (  pguid: nil;
       dwOfs: 0;
       dwType: DIDFT_AXIS or DIDFT_ANYINSTANCE;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: 48;
       dwType: DIDFT_BUTTON or DIDFT_ANYINSTANCE;
       dwFlags: 0)
  );

  c_dfDIJoystick: DIDATAFORMAT = (
      dwSize: Sizeof(c_dfDIJoystick);
      dwObjSize: Sizeof(DIOBJECTDATAFORMAT);
      dwFlags: DIDF_ABSAXIS;
      dwDataSize: SizeOf(DIJOYSTATE);
      dwNumObjs: High(_c_dfDIJoystick_Objects)+1;
      rgodf: @_c_dfDIJoystick_Objects);
}
const
  DIJOFS_X            = 0;
  DIJOFS_Y            = 4;
  DIJOFS_Z            = 8;
  DIJOFS_RX           = 12;
  DIJOFS_RY           = 16;
  DIJOFS_RZ           = 20;
  DIJOFS_SLIDER       = 24;
  DIJOFS_POV          = 32;
  DIJOFS_BUTTON       = 48;

{ IDirectInput }

const
  DIENUM_STOP = 0;
  DIENUM_CONTINUE = 1;

type
  LPDIENUMDEVICESCALLBACKA = function(const lpddi: DIDEVICEINSTANCEA;
      pvRef: Pointer): HRESULT; stdcall;
  LPDIENUMDEVICESCALLBACKW = function(const lpddi: DIDEVICEINSTANCEW;
      pvRef: Pointer): HRESULT; stdcall;

  LPDIENUMDEVICESCALLBACK = LPDIENUMDEVICESCALLBACKA;

const
  DIEDFL_ALLDEVICES    = $00000000;
  DIEDFL_ATTACHEDONLY  = $00000001;
  DIEDFL_FORCEFEEDBACK = $00000100;

type
  IDirectInputW = interface(IUnknown)
    ['{89521361-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInputW methods ***)
    function CreateDevice(const rguid: TGUID;
        out lplpDirectInputDevice: IDirectInputDeviceW; pUnkOuter: IUnknown):
        HRESULT; stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: LPDIENUMDEVICESCALLBACKW;
        pvRef: Pointer; dwFlags: DWORD): HRESULT; stdcall;
    function GetDeviceStatus(var rguidInstance: TGUID): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD): HRESULT; stdcall;
  end;

  IDirectInputA = interface(IUnknown)
    ['{89521360-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInputA methods ***)
    function CreateDevice(const rguid: TGUID;
        out lplpDirectInputDevice: IDirectInputDeviceA; pUnkOuter: IUnknown):
        HRESULT; stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: LPDIENUMDEVICESCALLBACKA;
        pvRef: Pointer; dwFlags: DWORD): HRESULT; stdcall;
    function GetDeviceStatus(const rguidInstance: TGUID): HRESULT; stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD): HRESULT; stdcall;
  end;

  IDirectInput = IDirectInputA;

  IDirectInput2W = interface(IDirectInputW)
    ['{5944E663-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInput2W methods ***)
    function FindDevice(Arg1: PGUID; Arg2: PWideChar; Arg3: PGUID):
        HRESULT; stdcall;
  end;

  IDirectInput2A = interface(IDirectInputA)
    ['{5944E662-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInput2A methods ***)
    function FindDevice(Arg1: PGUID; Arg2: PAnsiChar; Arg3: PGUID):
        HRESULT; stdcall;
  end;

  IDirectInput2 = IDirectInput2A;

{ Return Codes }

const
  DI_OK = S_OK;
  DI_NOTATTACHED = S_FALSE;
  DI_BUFFEROVERFLOW = S_FALSE;
  DI_PROPNOEFFECT = S_FALSE;
  DI_NOEFFECT = S_FALSE;
  DI_POLLEDDEVICE = $00000002;
  DI_DOWNLOADSKIPPED = $00000003;
  DI_EFFECTRESTARTED = $00000004;
  DI_TRUNCATED = $00000008;
  DI_TRUNCATEDANDRESTARTED = $0000000C;

  DIERR_OLDDIRECTINPUTVERSION = -2147023746;
  DIERR_BETADIRECTINPUTVERSION = -2147023743;
  DIERR_BADDRIVERVER = -2147024777;
  DIERR_DEVICENOTREG = REGDB_E_CLASSNOTREG;
  DIERR_NOTFOUND = -2147024894;
  DIERR_OBJECTNOTFOUND = -2147024894;
  DIERR_INVALIDPARAM = E_INVALIDARG;
  DIERR_NOINTERFACE = E_NOINTERFACE;
  DIERR_GENERIC = E_FAIL;
  DIERR_OUTOFMEMORY = E_OUTOFMEMORY;
  DIERR_UNSUPPORTED = E_NOTIMPL;
  DIERR_NOTINITIALIZED = -2147024875;
  DIERR_ALREADYINITIALIZED = -2147023649;
  DIERR_NOAGGREGATION = CLASS_E_NOAGGREGATION;
  DIERR_OTHERAPPHASPRIO = E_ACCESSDENIED;
  DIERR_INPUTLOST = -2147024866;
  DIERR_ACQUIRED = -2147024726;
  DIERR_NOTACQUIRED = -2147024884;
  DIERR_READONLY = E_ACCESSDENIED;
  DIERR_HANDLEEXISTS = E_ACCESSDENIED;
  DIERR_PENDING = $80070007;
  DIERR_INSUFFICIENTPRIVS = $80040200;
  DIERR_DEVICEFULL = $80040201;
  DIERR_MOREDATA = $80040202;
  DIERR_NOTDOWNLOADED = $80040203;
  DIERR_HASEFFECTS = $80040204;
  DIERR_NOTEXCLUSIVEACQUIRED = $80040205;
  DIERR_INCOMPLETEEFFECT = $80040206;
  DIERR_NOTBUFFERED = $80040207;
  DIERR_EFFECTPLAYING = $80040208;

{ Definitions for non-IDirectInput (VJoyD) features defined more recently
  than the current sdk files }

  JOY_PASSDRIVERDATA = $10000000;
  JOY_HWS_ISHEADTRACKER = $02000000;
  JOY_HWS_ISGAMEPORTDRIVER = $04000000;
  JOY_HWS_ISANALOGPORTDRIVER = $08000000;
  JOY_HWS_AUTOLOAD = $10000000;
  JOY_HWS_NODEVNODE = $20000000;
  JOY_HWS_ISGAMEPORTEMULATOR = $40000000;
  JOY_US_VOLATILE = $00000008;

{ Definitions for non-IDirectInput (VJoyD) features defined more recently
  than the current ddk files }

  JOY_OEMPOLL_PASSDRIVERDATA = 7;

function DirectInputCreate(hinst: THandle; dwVersion: DWORD;
    out ppDI: IDirectInputA; punkOuter: IUnknown): HRESULT; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dplay.h
 *  Content:    DirectPlay include file
 *
 ***************************************************************************)

const
{ GUIDS used by DirectPlay objects }

  CLSID_DirectPlay: TGUID = (D1:$d1eb6d20;D2:$8923;D3:$11d0;D4:($9d,$97,$0,$a0,$c9,$a,$43,$cb));

  IID_IDirectPlay: TGUID = (D1:$5454e9a0;D2:$db65;D3:$11ce;D4:($92,$1c,$00,$aa,$00,$6c,$49,$72));
  IID_IDirectPlay2: TGUID = (D1:$2b74f7c0;D2:$9154;D3:$11cf;D4:($a9,$cd,$0,$aa,$0,$68,$86,$e3));
  IID_IDirectPlay2A: TGUID = (D1:$9d460580;D2:$a822;D3:$11cf;D4:($96,$c,$0,$80,$c7,$53,$4e,$82));
  IID_IDirectPlay3: TGUID = (D1:$133efe40;D2:$32dc;D3:$11d0;D4:($9c,$fb,$0,$a0,$c9,$a,$43,$cb));
  IID_IDirectPlay3A: TGUID = (D1:$133efe41;D2:$32dc;D3:$11d0;D4:($9c,$fb,$0,$a0,$c9,$a,$43,$cb));

{ GUIDS used by Service Providers shipped with DirectPlay
  Use these to identify Service Provider returned by EnumConnections }

  DPSPGUID_IPX: TGUID = (D1:$685bc400;D2:$9d2c;D3:$11cf;D4:($a9,$cd,$0,$aa,$0,$68,$86,$e3));
  DPSPGUID_TCPIP: TGUID = (D1:$36E95EE0;D2:$8577;D3:$11cf;D4:($96,$c,$0,$80,$c7,$53,$4e,$82));
  DPSPGUID_SERIAL: TGUID = (D1:$f1d6860;D2:$88d9;D3:$11cf;D4:($9c,$4e,$0,$a0,$c9,$5,$42,$5e));
  DPSPGUID_MODEM: TGUID = (D1:$44eaa760;D2:$cb68;D3:$11cf;D4:($9c,$4e,$0,$a0,$c9,$5,$42,$5e));

{ DirectPlay Structures }

type
  DPID = DWORD;
  LPDPID = ^DPID;

const
  DPID_SYSMSG = 0;
  DPID_ALLPLAYERS = 0;
  DPID_SERVERPLAYER = 1;
  DPID_UNKNOWN = $FFFFFFFF;

type
  DPCAPS = record
    dwSize: DWORD;              // Size of structure, in bytes
    dwFlags: DWORD;             // DPCAPS_xxx flags
    dwMaxBufferSize: DWORD;     // Maximum message size, in bytes,  for this service provider
    dwMaxQueueSize: DWORD;      // Obsolete.
    dwMaxPlayers: DWORD;        // Maximum players/groups (local + remote)
    dwHundredBaud: DWORD;       // Bandwidth in 100 bits per second units;
                                // i.e. 24 is 2400, 96 is 9600, etc.
    dwLatency: DWORD;           // Estimated latency; 0 = unknown
    dwMaxLocalPlayers: DWORD;   // Maximum # of locally created players allowed
    dwHeaderLength: DWORD;      // Maximum header length, in bytes, on messages
                                // added by the service provider
    dwTimeout: DWORD;           // Service provider's suggested timeout value
                                // This is how long DirectPlay will wait for
                                // responses to system messages
  end;
  LPDPCAPS = ^DPCAPS;

const
  DPCAPS_ISHOST = $00000002;
  DPCAPS_GROUPOPTIMIZED = $00000008;
  DPCAPS_KEEPALIVEOPTIMIZED = $00000010;
  DPCAPS_GUARANTEEDOPTIMIZED = $00000020;
  DPCAPS_GUARANTEEDSUPPORTED = $00000040;
  DPCAPS_SIGNINGSUPPORTED = $00000080;
  DPCAPS_ENCRYPTIONSUPPORTED = $00000100;

type
  DPSESSIONDESC2 = record
    dwSize: DWORD;             // Size of structure
    dwFlags: DWORD;            // DPSESSION_xxx flags
    guidInstance: TGUID;       // ID for the session instance
    guidApplication: TGUID;    // GUID of the DirectPlay application.
                               // GUID_NULL for all applications.
    dwMaxPlayers: DWORD;       // Maximum # players allowed in session
    dwCurrentPlayers: DWORD;   // Current # players in session (read only)
    lpszSessionNameA: PChar;   // HACK - Selected ANSI section from union below
(*
    union
    {                           // Name of the session
        LPWSTR  lpszSessionName;    // Unicode
        LPSTR   lpszSessionNameA;   // ANSI
    };
*)
    lpszPasswordA: PChar;      // HACK - Selected ANSI section from union below
(*
    union
    {                           // Password of the session (optional)
        LPWSTR  lpszPassword;       // Unicode
        LPSTR   lpszPasswordA;      // ANSI
    };
*)
    dwReserved1: DWORD;        // Reserved for future MS use.
    dwReserved2: DWORD;
    dwUser1: DWORD;            // For use by the application
    dwUser2: DWORD;
    dwUser3: DWORD;
    dwUser4: DWORD;
  end;
  LPDPSESSIONDESC2 = ^DPSESSIONDESC2;

const
  DPSESSION_NEWPLAYERSDISABLED = $00000001;
  DPSESSION_MIGRATEHOST = $00000004;
  DPSESSION_NOMESSAGEID = $00000008;
  DPSESSION_JOINDISABLED = $00000020;
  DPSESSION_KEEPALIVE = $00000040;
  DPSESSION_NODATAMESSAGES = $00000080;
  DPSESSION_SECURESERVER = $00000100;
  DPSESSION_PRIVATE = $00000200;
  DPSESSION_PASSWORDREQUIRED = $00000400;
  DPSESSION_MULTICASTSERVER = $00000800;
  DPSESSION_CLIENTSERVER = $00001000;

type
  DPNAME = record
    dwSize: DWORD;    // Size of structure
    dwFlags: DWORD;   // Not used. Must be zero.
    case Integer of
      // Unicode section
            0: (lpszShortName: LPWSTR; // The short or friendly name
                lpszLongName: LPWSTR);
      // ANSI section
      1: (lpszShortNameA: LPSTR; // The long or formal name
          lpszLongNameA: LPSTR);
  end;
  LPDPNAME = ^DPNAME;

(*
 * DPCREDENTIALS
 * Used to hold the user name and password of a DirectPlay user
 *)
  DPCREDENTIALS = record
    dwSize: DWORD;    // Size of structure
    dwFlags: DWORD;   // Not used. Must be zero.
    case Integer of
                // Unicode section
        0: (lpszUsername: LPWSTR;   // User name of the account
          lpszPassword: LPWSTR;   // Password of the account
          lpszDomain: LPWSTR);    // Domain name of the account
      // ANSI section
      1: (lpszUsernameA: LPSTR;
          lpszPasswordA: LPSTR;
          lpszDomainA: LPSTR);
  end;
  LPDPCREDENTIALS = ^DPCREDENTIALS;

(*
 * DPSECURITYDESC
 * Used to describe the security properties of a DirectPlay
 * session instance
 *)
  DPSECURITYDESC = record
    dwSize: DWORD;                  // Size of structure
    dwFlags: DWORD;                 // Not used. Must be zero.
    lpszSSIProviderA: LPSTR;        // HACK - Selected ANSI from union below
(*
    union
    {                               // SSPI provider name
        LPWSTR  lpszSSPIProvider;   // Unicode
        LPSTR   lpszSSPIProviderA;  // ANSI
    };
*)
    lpszCAPIProviderA: LPSTR;       // HACK - Selected ANSI from union below
(*
    union
    {                               // CAPI provider name
        LPWSTR lpszCAPIProvider;    // Unicode
        LPSTR  lpszCAPIProviderA;   // ANSI
    };
*)
    dwCAPIProviderType: DWORD;      // Crypto Service Provider type
    dwEncryptionAlgorithm: DWORD;   // Encryption Algorithm type
  end;
  LPDPSECURITYDESC = ^DPSECURITYDESC;

(*
 * DPACCOUNTDESC
 * Used to describe a user membership account
 *)
  DPACCOUNTDESC = record
    dwSize: DWORD;    // Size of structure
    dwFlags: DWORD;   // Not used. Must be zero.
    case Integer of   // Account identifier
        0: (lpszAccountID: LPWSTR);  // UNICODE
      1: (lpszAccountIDA: LPSTR);  // ANSI
  end;
  LPDPACCOUNTDESC = ^DPACCOUNTDESC;

(*
 * DPLCONNECTION
 * Used to hold all in the informaion needed to connect
 * an application to a session or create a session
 *)
  DPLCONNECTION = record
    dwSize: DWORD;                     // Size of this structure
    dwFlags: DWORD;                    // Flags specific to this structure
    lpSessionDesc: LPDPSESSIONDESC2;   // Pointer to session desc to use on connect
    lpPlayerName: LPDPNAME;            // Pointer to Player name structure
    guidSP: TGUID;                     // GUID of the DPlay SP to use
    lpAddress: Pointer;                // Address for service provider
    dwAddressSize: DWORD;              // Size of address data
  end;
  LPDPLCONNECTION = ^DPLCONNECTION;

(*
 * DPCHAT
 * Used to hold the a DirectPlay chat message
 *)
  DPCHAT = record
    dwSize: DWORD;
    dwFlags: DWORD;
    case Integer of            // Message string
        0: (lpszMessage: LPWSTR);  // Unicode
        1: (lpszMessageA: LPSTR);  // ANSI
  end;
  LPDPCHAT = ^DPCHAT;

{ Prototypes for DirectPlay callback functions }

{ Callback for IDirectPlay2::EnumSessions }
  LPDPENUMSESSIONSCALLBACK2 = function(const lpThisSD: DPSESSIONDESC2;
      var lpdwTimeOut: DWORD; dwFlags: DWORD; lpContext: Pointer): BOOL;
      stdcall;

const
  DPESC_TIMEDOUT = $00000001;

type
  LPDPENUMPLAYERSCALLBACK2 = function(dpId: DPID; dwPlayerType: DWORD;
      const lpName: DPNAME; dwFlags: DWORD; lpContext: Pointer): BOOL;
      stdcall;

  LPDPENUMDPCALLBACK = function(const lpguidSP: TGUID; lpSPName: LPWSTR;
      dwMajorVersion: DWORD; dwMinorVersion: DWORD; lpContext: Pointer):
      BOOL; stdcall;

  LPDPENUMDPCALLBACKA = function(const lpguidSP: TGUID; lpSPName: LPSTR;
      dwMajorVersion: DWORD; dwMinorVersion: DWORD; lpContext: Pointer):
      BOOL; stdcall;

  LPDPENUMCONNECTIONSCALLBACK = function(const lpguidSP: TGUID;
      lpConnection: Pointer; dwConnectionSize: DWORD; const lpName: DPNAME;
      dwFlags: DWORD; lpContext: Pointer): BOOL; stdcall;

{ API's }

function DirectPlayEnumerateA(lpEnumDPCallback: LPDPENUMDPCALLBACKA;
    lpContext: Pointer): HRESULT; stdcall;
function DirectPlayEnumerateW(lpEnumDPCallback: LPDPENUMDPCALLBACK;
    lpContext: Pointer): HRESULT; stdcall;
function DirectPlayEnumerate(lpEnumDPCallback: LPDPENUMDPCALLBACKA;
    lpContext: Pointer): HRESULT; stdcall;


{ IDirectPlay2 (and IDirectPlay2A) Interface }

type
  IDirectPlay2 = interface;
  IDirectPlay3 = interface;

  IDirectPlay2A = IDirectPlay2;
  IDirectPlay3A = IDirectPlay3;

  IDirectPlay2 = interface(IUnknown)
    ['{2B74F7C0-9154-11CF-A9CD-00AA006886E3}']
    (*** IDirectPlay2 methods ***)
    function AddPlayerToGroup(idGroup: DPID; idPlayer: DPID): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function CreateGroup(var lpidGroup: DPID; var lpGroupName: DPNAME;
        const lpData; dwDataSize: DWORD; dwFlags: DWORD): HRESULT; stdcall;
    function CreatePlayer(var lpidPlayer: DPID; var pPlayerName: DPNAME;
        hEvent: THandle; const lpData; dwDataSize: DWORD; dwFliags: DWORD):
        HRESULT; stdcall;
    function DeletePlayerFromGroup(idGroup: DPID; idPlayer: DPID): HRESULT;
        stdcall;
    function DestroyGroup(idGroup: DPID): HRESULT; stdcall;
    function DestroyPlayer(idPlayer: DPID): HRESULT; stdcall;
    function EnumGroupPlayers(idGroup: DPID; const lpguidInstance: TGUID;
        lpEnumPlayersCallback2: LPDPENUMPLAYERSCALLBACK2; lpContext: Pointer;
        dwFlags: DWORD): HRESULT; stdcall;
    function EnumGroups(const lpguidInstance: TGUID; lpEnumPlayersCallback2:
        LPDPENUMPLAYERSCALLBACK2; lpContext: Pointer; dwFlags: DWORD): HRESULT;
        stdcall;
    function EnumPlayers(const lpguidInstance: TGUID; lpEnumPlayersCallback2:
        LPDPENUMPLAYERSCALLBACK2; lpContext: Pointer; dwFlags: DWORD): HRESULT;
        stdcall;
    function EnumSessions(var lpsd: DPSESSIONDESC2; dwTimeout: DWORD;
        lpEnumSessionsCallback2: LPDPENUMSESSIONSCALLBACK2; lpContext: Pointer;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetCaps(var lpDPCaps: DPCAPS; dwFlags: DWORD): HRESULT; stdcall;
    function GetGroupData(idGroup: DPID; var lpData; var lpdwDataSize: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetGroupName(idGroup: DPID; var lpData; var lpdwDataSize: DWORD):
        HRESULT; stdcall;
    function GetMessageCount(idPlayer: DPID; var lpdwCount: DWORD): HRESULT;
         stdcall;
    function GetPlayerAddress(idPlayer: DPID; var lpAddress;
        var lpdwAddressSize: DWORD): HRESULT; stdcall;
    function GetPlayerCaps(idPlayer: DPID; var lpPlayerCaps: DPCAPS;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetPlayerData(idPlayer: DPID; var lpData; var lpdwDataSize: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetPlayerName(idPlayer: DPID; var lpData; var lpdwDataSize: DWORD):
        HRESULT; stdcall;
    function GetSessionDesc(var lpData; var lpdwDataSize: DWORD): HRESULT;
         stdcall;
    function Initialize(const lpGUID: TGUID): HRESULT; stdcall;
    function Open(var lpsd: DPSESSIONDESC2; dwFlags: DWORD): HRESULT; stdcall;
    function Receive(var lpidFrom: DPID; var lpidTo: DPID; dwFlags: DWORD;
        var lpData; var lpdwDataSize: DWORD): HRESULT; stdcall;
    function Send(idFrom: DPID; lpidTo: DPID; dwFlags: DWORD; const lpData;
        lpdwDataSize: DWORD): HRESULT; stdcall;
    function SetGroupData(idGroup: DPID; const lpData; dwDataSize: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function SetGroupName(idGroup: DPID; const lpGroupName: DPNAME;
        dwFlags: DWORD): HRESULT; stdcall;
    function SetPlayerData(idPlayer: DPID; const lpData; dwDataSize: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function SetPlayerName(idPlayer: DPID; const lpPlayerName: DPNAME;
        dwFlags: DWORD): HRESULT; stdcall;
    function SetSessionDesc(const lpSessDesc: DPSESSIONDESC2; dwFlags: DWORD):
        HRESULT; stdcall;
  end;

{ IDirectPlay3 (and IDirectPlay3A) Interface }

  IDirectPlay3 = interface(IDirectPlay2)
    ['{133EFE40-32DC-11D0-9CFB-00A0C90A43CB}']
    (*** IDirectPlay3 methods ***)
    function AddGroupToGroup(idParentGroup: DPID; idGroup: DPID): HRESULT;
        stdcall;
    function CreateGroupInGroup(idParentGroup: DPID; var lpidGroup: DPID;
        var lpGroupName: DPNAME; const lpData; dwDataSize: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function DeleteGroupFromGroup(idParentGroup: DPID; idGroup: DPID): HRESULT;
         stdcall;
    function EnumConnections(const lpguidApplication: TGUID;
        lpEnumCallback: LPDPENUMCONNECTIONSCALLBACK; lpContext: Pointer;
        dwFlags: DWORD): HRESULT; stdcall;
    function EnumGroupsInGroup(idGroup: DPID; const lpguidInstance: TGUID;
        lpEnumPlayersCallback2: LPDPENUMPLAYERSCALLBACK2; lpContext: Pointer;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetGroupConnectionSettings(dwFlags: DWORD; idGroup: DPID;
        var lpData; var lpdwDataSize: DWORD): HRESULT; stdcall;
    function InitializeConnection(lpConnection: Pointer; dwFlags: DWORD):
         HRESULT; stdcall;
    function SecureOpen(const lpsd: DPSESSIONDESC2; dwFlags: DWORD;
        const lpSecurity: DPSECURITYDESC; const lpCredentials: DPCREDENTIALS):
        HRESULT; stdcall;
    function SendChatMessage(idFrom: DPID; idTo: DPID; dwFlags: DWORD;
        const lpChatMessage: DPCHAT): HRESULT; stdcall;
    function SetGroupConnectionSettings(dwFlags: DWORD; idGroup: DPID;
        const lpConnection: DPLCONNECTION): HRESULT; stdcall;
    function StartSession(dwFlags: DWORD; idGroup: DPID): HRESULT; stdcall;
    function GetGroupFlags(idGroup: DPID; var lpdwFlags: DWORD): HRESULT;
        stdcall;
    function GetGroupParent(idGroup: DPID; var lpidParent: DPID): HRESULT;
        stdcall;
    function GetPlayerAccount(idPlayer: DPID; dwFlags: DWORD; var lpData;
        var lpdwDataSize: DWORD): HRESULT; stdcall;
    function GetPlayerFlags(idPlayer: DPID; var lpdwFlags: DWORD): HRESULT;
         stdcall;
  end;

const
{ EnumConnections API flags }

  DPCONNECTION_DIRECTPLAY = $00000001;
  DPCONNECTION_DIRECTPLAYLOBBY = $00000002;

{ EnumPlayers API flags }

  DPENUMPLAYERS_ALL = $00000000;
  DPENUMGROUPS_ALL = DPENUMPLAYERS_ALL;
  DPENUMPLAYERS_LOCAL = $00000008;
  DPENUMGROUPS_LOCAL = DPENUMPLAYERS_LOCAL;
  DPENUMPLAYERS_REMOTE = $00000010;
  DPENUMGROUPS_REMOTE = DPENUMPLAYERS_REMOTE;
  DPENUMPLAYERS_GROUP = $00000020;
  DPENUMPLAYERS_SESSION = $00000080;
  DPENUMGROUPS_SESSION = DPENUMPLAYERS_SESSION;
  DPENUMPLAYERS_SERVERPLAYER = $00000100;
  DPENUMPLAYERS_SPECTATOR = $00000200;
  DPENUMGROUPS_SHORTCUT = $00000400;
  DPENUMGROUPS_STAGINGAREA = $00000800;

{ CreatePlayer API flags }

  DPPLAYER_SERVERPLAYER = DPENUMPLAYERS_SERVERPLAYER;
  DPPLAYER_SPECTATOR = DPENUMPLAYERS_SPECTATOR;
  DPPLAYER_LOCAL = DPENUMPLAYERS_LOCAL;

{ CreateGroup API flags }

  DPGROUP_STAGINGAREA = DPENUMGROUPS_STAGINGAREA;
  DPGROUP_LOCAL = DPENUMGROUPS_LOCAL;

{ EnumSessions API flags }

  DPENUMSESSIONS_AVAILABLE = $00000001;
  DPENUMSESSIONS_ALL = $00000002;
  DPENUMSESSIONS_ASYNC = $00000010;
  DPENUMSESSIONS_STOPASYNC = $00000020;
  DPENUMSESSIONS_PASSWORDREQUIRED = $00000040;
  DPENUMSESSIONS_RETURNSTATUS = $00000080;

{ GetCaps and GetPlayerCaps API flags }

  DPGETCAPS_GUARANTEED = $00000001;

{ GetGroupData, GetPlayerData API flags }

  DPGET_REMOTE = $00000000;
  DPGET_LOCAL = $00000001;

{ Open API flags }

  DPOPEN_JOIN = $00000001;
  DPOPEN_CREATE = $00000002;
  DPOPEN_RETURNSTATUS = DPENUMSESSIONS_RETURNSTATUS;

{ DPLCONNECTION flags }

  DPLCONNECTION_CREATESESSION = DPOPEN_CREATE;
  DPLCONNECTION_JOINSESSION = DPOPEN_JOIN;

{ Receive API flags }

  DPRECEIVE_ALL = $00000001;
  DPRECEIVE_TOPLAYER = $00000002;
  DPRECEIVE_FROMPLAYER = $00000004;
  DPRECEIVE_PEEK = $00000008;

{ Send API flags }

  DPSEND_GUARANTEED = $00000001;
  DPSEND_HIGHPRIORITY = $00000002;
  DPSEND_OPENSTREAM = $00000008;
  DPSEND_CLOSESTREAM = $00000010;
  DPSEND_SIGNED = $00000020;
  DPSEND_ENCRYPTED = $00000040;

{ SetGroupData, SetGroupName, SetPlayerData, SetPlayerName,
  SetSessionDesc API flags. }

  DPSET_REMOTE = $00000000;
  DPSET_LOCAL = $00000001;
  DPSET_GUARANTEED = $00000002;

{ DirectPlay system messages and message data structures }

  DPSYS_CREATEPLAYERORGROUP = $0003;
  DPSYS_DESTROYPLAYERORGROUP = $0005;
  DPSYS_ADDPLAYERTOGROUP = $0007;
  DPSYS_DELETEPLAYERFROMGROUP = $0021;
  DPSYS_SESSIONLOST = $0031;
  DPSYS_HOST = $0101;
  DPSYS_SETPLAYERORGROUPDATA = $0102;
  DPSYS_SETPLAYERORGROUPNAME = $0103;
  DPSYS_SETSESSIONDESC = $0104;
  DPSYS_ADDGROUPTOGROUP = $0105;
  DPSYS_DELETEGROUPFROMGROUP = $0106;
  DPSYS_SECUREMESSAGE = $0107;
  DPSYS_STARTSESSION = $0108;
  DPSYS_CHAT = $0109;
  DPPLAYERTYPE_GROUP = $00000000;
  DPPLAYERTYPE_PLAYER = $00000001;

type
  DPMSG_GENERIC = record
    dwType: DWORD;   // Message type
  end;
  LPDPMSG_GENERIC = ^DPMSG_GENERIC;

  DPMSG_CREATEPLAYERORGROUP = record
    dwType: DWORD;             // Message type
    dwPlayerType: DWORD;       // Is it a player or group
    dpId: DPID;                // ID of the player or group
    dwCurrentPlayers: DWORD;   // current # players & groups in session
    lpData: Pointer;           // pointer to remote data
    dwDataSize: DWORD;         // size of remote data
    dpnName: DPNAME;           // structure with name info
                               // the following fields are only available when using
                               // the IDirectPlay3 interface or greater
    dpIdParent: DPID;          // id of parent group
    dwFlags: DWORD;            // player or group flags
  end;
  LPDPMSG_CREATEPLAYERORGROUP = ^DPMSG_CREATEPLAYERORGROUP;

  DPMSG_DESTROYPLAYERORGROUP = record
    dwType: DWORD;             // Message type
    dwPlayerType: DWORD;       // Is it a player or group
    dpId: DPID;                // player ID being deleted
    lpLocalData: Pointer;      // copy of players local data
    dwLocalDataSize: DWORD;    // sizeof local data
    lpRemoteData: Pointer;     // copy of players remote data
    dwRemoteDataSize: DWORD;   // sizeof remote data
                               // the following fields are only available when using
                               // the IDirectPlay3 interface or greater
    dpnName: DPNAME;           // structure with name info
    dpIdParent: DPID;          // id of parent group
    dwFlags: DWORD;            // player or group flags
  end;
  LPDPMSG_DESTROYPLAYERORGROUP = ^DPMSG_DESTROYPLAYERORGROUP;

  DPMSG_ADDPLAYERTOGROUP = record
    dwType: DWORD;      // Message type
    dpIdGroup: DPID;    // group ID being added to
    dpIdPlayer: DPID;   // player ID being added
  end;
  LPDPMSG_ADDPLAYERTOGROUP = ^DPMSG_ADDPLAYERTOGROUP;

  DPMSG_DELETEPLAYERFROMGROUP = DPMSG_ADDPLAYERTOGROUP;
  LPDPMSG_DELETEPLAYERFROMGROUP = ^DPMSG_DELETEPLAYERFROMGROUP;

  DPMSG_ADDGROUPTOGROUP = record
    dwType: DWORD;           // Message type
    dpIdParentGroup: DPID;   // group ID being added to
    dpIdGroup: DPID;         // group ID being added
  end;
  LPDPMSG_ADDGROUPTOGROUP = ^DPMSG_ADDGROUPTOGROUP;

  DPMSG_DELETEGROUPFROMGROUP = DPMSG_ADDGROUPTOGROUP;
  LPDPMSG_DELETEGROUPFROMGROUP = ^DPMSG_DELETEGROUPFROMGROUP;

  DPMSG_SETPLAYERORGROUPDATA = record
    dwType: DWORD;         // Message type
    dwPlayerType: DWORD;   // Is it a player or group
    dpId: DPID;            // ID of player or group
    lpData: Pointer;       // pointer to remote data
    dwDataSize: DWORD;     // size of remote data
  end;
  LPDPMSG_SETPLAYERORGROUPDATA = ^DPMSG_SETPLAYERORGROUPDATA;

  DPMSG_SETPLAYERORGROUPNAME = record
    dwType: DWORD;         // Message type
    dwPlayerType: DWORD;   // Is it a player or group
    dpId: DPID;            // ID of player or group
    dpnName: DPNAME;       // structure with new name info
  end;
  LPDPMSG_SETPLAYERORGROUPNAME = ^DPMSG_SETPLAYERORGROUPNAME;

  DPMSG_SETSESSIONDESC = record
    dwType: DWORD;            // Message type
    dpDesc: DPSESSIONDESC2;   // Session desc
  end;
  LPDPMSG_SETSESSIONDESC = ^DPMSG_SETSESSIONDESC;

  DPMSG_HOST = DPMSG_GENERIC;
  LPDPMSG_HOST = ^DPMSG_HOST;

  DPMSG_SESSIONLOST = DPMSG_GENERIC;
  LPDPMSG_SESSIONLOST = ^DPMSG_SESSIONLOST;

  DPMSG_SECUREMESSAGE = record
    dwType: DWORD;       // Message Type
    dwFlags: DWORD;      // Signed/Encrypted
    dpIdFrom: DPID;      // ID of Sending Player
    lpData: Pointer;     // Player message
    dwDataSize: DWORD;   // Size of player message
  end;
  LPDPMSG_SECUREMESSAGE = ^DPMSG_SECUREMESSAGE;

  DPMSG_STARTSESSION = record
    dwType: DWORD;             // Message type
    lpConn: LPDPLCONNECTION;   // DPLCONNECTION structure
  end;
  LPDPMSG_STARTSESSION = ^DPMSG_STARTSESSION;

  DPMSG_CHAT = record
    dwType: DWORD;        // Message type
    dwFlags: DWORD;       // Message flags
    idFromPlayer: DPID;   // ID of the Sending Player
    idToPlayer: DPID;     // ID of the To Player
    idToGroup: DPID;      // ID of the To Group
    lpChat: LPDPCHAT;     // Pointer to a structure containing the chat message
  end;
  LPDPMSG_CHAT = ^DPMSG_CHAT;

{ DIRECTPLAY ERRORS }
const
  DP_OK = S_OK;
  DPERR_ALREADYINITIALIZED = $88770000 + 5;
  DPERR_ACCESSDENIED = $88770000 + 10;
  DPERR_ACTIVEPLAYERS = $88770000 + 20;
  DPERR_BUFFERTOOSMALL = $88770000 + 30;
  DPERR_CANTADDPLAYER = $88770000 + 40;
  DPERR_CANTCREATEGROUP = $88770000 + 50;
  DPERR_CANTCREATEPLAYER = $88770000 + 60;
  DPERR_CANTCREATESESSION = $88770000 + 70;
  DPERR_CAPSNOTAVAILABLEYET = $88770000 + 80;
  DPERR_EXCEPTION = $88770000 + 90;
  DPERR_GENERIC = E_FAIL;
  DPERR_INVALIDFLAGS = $88770000 + 120;
  DPERR_INVALIDOBJECT = $88770000 + 130;
  DPERR_INVALIDPARAM = E_INVALIDARG;
  DPERR_INVALIDPARAMS = DPERR_INVALIDPARAM;
  DPERR_INVALIDPLAYER = $88770000 + 150;
  DPERR_INVALIDGROUP = $88770000 + 155;
  DPERR_NOCAPS = $88770000 + 160;
  DPERR_NOCONNECTION = $88770000 + 170;
  DPERR_NOMEMORY = E_OUTOFMEMORY;
  DPERR_OUTOFMEMORY = DPERR_NOMEMORY;
  DPERR_NOMESSAGES = $88770000 + 190;
  DPERR_NONAMESERVERFOUND = $88770000 + 200;
  DPERR_NOPLAYERS = $88770000 + 210;
  DPERR_NOSESSIONS = $88770000 + 220;
  DPERR_PENDING = $80070007;
  DPERR_SENDTOOBIG = $88770000 + 230;
  DPERR_TIMEOUT = $88770000 + 240;
  DPERR_UNAVAILABLE = $88770000 + 250;
  DPERR_UNSUPPORTED = E_NOTIMPL;
  DPERR_BUSY = $88770000 + 270;
  DPERR_USERCANCEL = $88770000 + 280;
  DPERR_NOINTERFACE = E_NOINTERFACE;
  DPERR_CANNOTCREATESERVER = $88770000 + 290;
  DPERR_PLAYERLOST = $88770000 + 300;
  DPERR_SESSIONLOST = $88770000 + 310;
  DPERR_UNINITIALIZED = $88770000 + 320;
  DPERR_NONEWPLAYERS = $88770000 + 330;
  DPERR_INVALIDPASSWORD = $88770000 + 340;
  DPERR_CONNECTING = $88770000 + 350;


  DPERR_BUFFERTOOLARGE = $88770000 + 1000;
  DPERR_CANTCREATEPROCESS = $88770000 + 1010;
  DPERR_APPNOTSTARTED = $88770000 + 1020;
  DPERR_INVALIDINTERFACE = $88770000 + 1030;
  DPERR_NOSERVICEPROVIDER = $88770000 + 1040;
  DPERR_UNKNOWNAPPLICATION = $88770000 + 1050;
  DPERR_NOTLOBBIED = $88770000 + 1070;
  DPERR_SERVICEPROVIDERLOADED = $88770000 + 1080;
  DPERR_ALREADYREGISTERED = $88770000 + 1090;
  DPERR_NOTREGISTERED = $88770000 + 1100;

//
// Security related errors
//
  DPERR_AUTHENTICATIONFAILED = $88770000 + 2000;
  DPERR_CANTLOADSSPI = $88770000 + 2010;
  DPERR_ENCRYPTIONFAILED = $88770000 + 2020;
  DPERR_SIGNFAILED = $88770000 + 2030;
  DPERR_CANTLOADSECURITYPACKAGE = $88770000 + 2040;
  DPERR_ENCRYPTIONNOTSUPPORTED = $88770000 + 2050;
  DPERR_CANTLOADCAPI = $88770000 + 2060;
  DPERR_NOTLOGGEDIN = $88770000 + 2070;
  DPERR_LOGONDENIED = $88770000 + 2080;

// define this to ignore obsolete interfaces and constants

  DPOPEN_OPENSESSION = DPOPEN_JOIN;
  DPOPEN_CREATESESSION = DPOPEN_CREATE;

  DPENUMSESSIONS_PREVIOUS = $00000004;

  DPENUMPLAYERS_PREVIOUS = $00000004;

  DPSEND_GUARANTEE = DPSEND_GUARANTEED;
  DPSEND_TRYONCE = $00000004;

  DPCAPS_NAMESERVICE = $00000001;
  DPCAPS_NAMESERVER = DPCAPS_ISHOST;
  DPCAPS_GUARANTEED = $00000004;

  DPLONGNAMELEN = 52;
  DPSHORTNAMELEN = 20;
  DPSESSIONNAMELEN = 32;
  DPPASSWORDLEN = 16;
  DPUSERRESERVED = 16;

  DPSYS_ADDPLAYER = $0003;
  DPSYS_DELETEPLAYER = $0005;

  DPSYS_DELETEGROUP = $0020;
  DPSYS_DELETEPLAYERFROMGRP = $0021;
  DPSYS_CONNECT = $484b;

type
  DPMSG_ADDPLAYER = record
    dwType: DWORD;
    dwPlayerType: DWORD;
    dpId: DPID;
    szLongName: array[0..DPLONGNAMELEN-1] of Char;
    szShortName: array[0..DPSHORTNAMELEN-1] of Char;
    dwCurrentPlayers: DWORD;
  end;
  LPDPMSG_ADDPLAYER = ^DPMSG_ADDPLAYER;

  DPMSG_ADDGROUP = DPMSG_ADDPLAYER;

  DPMSG_GROUPADD = record
    dwType: DWORD;
    dpIdGroup: DPID;
    dpIdPlayer: DPID;
  end;
  LPDPMSG_GROUPADD = ^DPMSG_GROUPADD;

  DPMSG_GROUPDELETE = DPMSG_GROUPADD;
  DPMSG_DELETEPLAYER = record
    dwType: DWORD;
    dpId: DPID;
  end;
  LPDPMSG_DELETEPLAYER = ^DPMSG_DELETEPLAYER;

  LPDPENUMPLAYERSCALLBACK = function(dpId: DPID; lpFriendlyName: LPSTR;
      lpFormalName: LPSTR; dwFlags: DWORD; lpContext: Pointer): BOOL;
      stdcall;

  DPSESSIONDESC = record
    dwSize: DWORD;
    guidSession: TGUID;
    dwSession: DWORD;
    dwMaxPlayers: DWORD;
    dwCurrentPlayers: DWORD;
    dwFlags: DWORD;
    szSessionName: Array [0..DPSESSIONNAMELEN-1] of char;
    szUserField: Array [0..DPUSERRESERVED-1] of char;
    dwReserved1: DWORD;
    szPassword: Array [0..DPPASSWORDLEN-1] of char;
    dwReserved2: DWORD;
    dwUser1: DWORD;
    dwUser2: DWORD;
    dwUser3: DWORD;
    dwUser4: DWORD;
  end;
  LPDPSESSIONDESC = ^DPSESSIONDESC;

  LPDPENUMSESSIONSCALLBACK = function(var lpDPSessionDesc: DPSESSIONDESC;
      lpContext: Pointer; var lpdwTimeOut: DWORD; dwFlags: DWORD): BOOL;
      stdcall;

type
(*
 * IDirectPlay
 *)
  IDirectPlay = interface(IUnknown)
    (*** IDirectPlay methods ***)
    function AddPlayerToGroup(pidGroup: DPID; pidPlayer: DPID): HRESULT;
        stdcall;
    function Close: HRESULT; stdcall;
    function CreatePlayer(var lppidID: DPID; lpPlayerFriendlyName: LPSTR;
        lpPlayerFormalName: LPSTR; lpEvent: PHandle): HRESULT; stdcall;
    function CreateGroup(var lppidID: DPID; lpGroupFriendlyName: LPSTR;
        lpGroupFormalName: LPSTR): HRESULT; stdcall;
    function DeletePlayerFromGroup(pidGroup: DPID; pidPlayer: DPID): HRESULT;
        stdcall;
    function DestroyPlayer(pidID: DPID): HRESULT; stdcall;
    function DestroyGroup(pidID: DPID): HRESULT; stdcall;
    function EnableNewPlayers(bEnable: BOOL): HRESULT; stdcall;
    function EnumGroupPlayers(pidGroupPID: DPID; lpEnumPlayersCallback:
        LPDPENUMPLAYERSCALLBACK; lpContext: Pointer; dwFlags: DWORD): HRESULT;
        stdcall;
    function EnumGroups(dwSessionID: DWORD; lpEnumPlayersCallback:
        LPDPENUMPLAYERSCALLBACK; lpContext: Pointer; dwFlags: DWORD): HRESULT;
        stdcall;
    function EnumPlayers(dwSessionId: DWORD; lpEnumPlayersCallback:
        LPDPENUMPLAYERSCALLBACK; lpContext: Pointer; dwFlags: DWORD): HRESULT;
        stdcall;
    function EnumSessions(const lpSDesc: DPSESSIONDESC; dwTimeout: DWORD;
        lpEnumSessionsCallback: LPDPENUMSESSIONSCALLBACK; lpContext: Pointer;
        dwFlags: DWORD): HRESULT; stdcall;
    function GetCaps(const lpDPCaps: DPCAPS): HRESULT; stdcall;
    function GetMessageCount(pidID: DPID; var lpdwCount: DWORD): HRESULT;
        stdcall;
    function GetPlayerCaps(pidID: DPID; const lpDPPlayerCaps: DPCAPS): HRESULT;
        stdcall;
    function GetPlayerName(pidID: DPID; lpPlayerFriendlyName: LPSTR;
        var lpdwFriendlyNameLength: DWORD; lpPlayerFormalName: LPSTR;
        var lpdwFormalNameLength: DWORD): HRESULT; stdcall;
    function Initialize(const lpGUID: TGUID): HRESULT; stdcall;
    function Open(const lpSDesc: DPSESSIONDESC): HRESULT; stdcall;
    function Receive(var lppidFrom, lppidTo: DPID; dwFlags: DWORD;
        var lpvBuffer; var lpdwSize: DWORD): HRESULT; stdcall;
    function SaveSession(lpSessionName: LPSTR): HRESULT; stdcall;
    function Send(pidFrom: DPID; pidTo: DPID; dwFlags: DWORD;
        const lpvBuffer; dwBuffSize: DWORD): HRESULT; stdcall;
    function SetPlayerName(pidID: DPID; lpPlayerFriendlyName: LPSTR;
        lpPlayerFormalName: LPSTR): HRESULT; stdcall;
  end;

{ API's (cont.) }

function DirectPlayCreate(const lpGUID: TGUID; out lplpDP: IDirectPlay;
    pUnk: IUnknown): HRESULT; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dplobby.h
 *  Content:    DirectPlayLobby include file
 *
 ***************************************************************************)

{ GUIDS used by DirectPlay objects }

const
  CLSID_DirectPlayLobby: TGUID = (D1:$2fe8f810;D2:$b2a5;D3:$11d0;D4:($a7,$87,$0,$0,$f8,$3,$ab,$fc));

  IID_IDirectPlayLobby: TGUID = (D1:$af465c71;D2:$9588;D3:$11cf;D4:($a0,$20,$0,$aa,$0,$61,$57,$ac));
  IID_IDirectPlayLobbyA: TGUID = (D1:$26c66a70;D2:$b367;D3:$11cf;D4:($a0,$24,$0,$aa,$0,$61,$57,$ac));
  IID_IDirectPlayLobby2: TGUID = (D1:$194c220;D2:$a303;D3:$11d0;D4:($9c,$4f,$0,$a0,$c9,$5,$42,$5e));
  IID_IDirectPlayLobby2A: TGUID = (D1:$1bb4af80;D2:$a303;D3:$11d0;D4:($9c,$4f,$0,$a0,$c9,$5,$42,$5e));

{ IDirectPlayLobby Structures }

type
  DPLAPPINFO = record
    dwSize: DWORD;            // Size of this structure
    guidApplication: TGUID;   // GUID of the Application
    case Integer of           // Pointer to the Application Name
      0: (lpszAppNameA: LPSTR);
      1: (lpszAppName: LPWSTR);
  end;
  LPDPLAPPINFO = ^DPLAPPINFO;

  DPCOMPOUNDADDRESSELEMENT = record
    guidDataType: TGUID;
    dwDataSize: DWORD;
    lpData: Pointer;
  end;
  LPDPCOMPOUNDADDRESSELEMENT = ^DPCOMPOUNDADDRESSELEMENT;

{ Enumeration Method Callback Prototypes }

  LPDPENUMADDRESSCALLBACK = function(const guidDataType: TGUID;
      dwDataSize: DWORD; lpData: Pointer; lpContext: Pointer): BOOL;
      stdcall;

  LPDPLENUMADDRESSTYPESCALLBACK = function(const guidDataType: TGUID;
      lpContext: Pointer; dwFlags: DWORD): BOOL; stdcall;

  LPDPLENUMLOCALAPPLICATIONSCALLBACK = function(const lpAppInfo: DPLAPPINFO;
      lpContext: Pointer; dwFlags: DWORD): BOOL; stdcall;

{ IDirectPlayLobby (and IDirectPlayLobbyA) Interface }

type
  IDirectPlayLobby = interface;
  IDirectPlayLobby2 = interface;

  IDirectPlayLobbyA = IDirectPlayLobby;

  IDirectPlayLobby2A = IDirectPlayLobby2;

  IDirectPlayLobby = interface(IUnknown)
    ['{AF465C71-9588-11CF-A020-00AA006157AC}']
    (*** IDirectPlayLobby methods ***)
    function Connect(dwFlags: DWORD; out lplpDP: IDirectPlay2;
        pUnk: IUnknown): HRESULT; stdcall;
    function CreateAddress(const guidSP, guidDataType: TGUID; const lpData;
        dwDataSize: DWORD; var lpAddress; var lpdwAddressSize: DWORD): HRESULT;
        stdcall;
    function EnumAddress(lpEnumAddressCallback: LPDPENUMADDRESSCALLBACK;
        const lpAddress; dwAddressSize: DWORD; lpContext : Pointer): HRESULT;
        stdcall;
    function EnumAddressTypes(lpEnumAddressTypeCallback:
        LPDPLENUMADDRESSTYPESCALLBACK; const guidSP: TGUID; lpContext: Pointer;
        dwFlags: DWORD): HRESULT; stdcall;
    function EnumLocalApplications(lpEnumLocalAppCallback:
        LPDPLENUMLOCALAPPLICATIONSCALLBACK; lpContext: Pointer; dwFlags: DWORD):
        HRESULT; stdcall;
    function GetConnectionSettings(dwAppID: DWORD; var lpData: DPLCONNECTION;
        var lpdwDataSize: DWORD): HRESULT; stdcall;
    function ReceiveLobbyMessage(dwFlags: DWORD; dwAppID: DWORD;
        var lpdwMessageFlags: DWORD; var lpData; var lpdwDataSize: DWORD):
        HRESULT; stdcall;
    function RunApplication(dwFlags: DWORD; var lpdwAppId: DWORD;
        const lpConn: DPLCONNECTION; hReceiveEvent: THandle): HRESULT; stdcall;
    function SendLobbyMessage(dwFlags: DWORD; dwAppID: DWORD; const lpData;
        dwDataSize: DWORD): HRESULT; stdcall;
    function SetConnectionSettings(dwFlags: DWORD; dwAppID: DWORD;
        const lpConn: DPLCONNECTION): HRESULT; stdcall;
    function SetLobbyMessageEvent(dwFlags: DWORD; dwAppID: DWORD;
        hReceiveEvent: THandle): HRESULT; stdcall;
  end;

{ IDirectPlayLobby2 (and IDirectPlayLobby2A) Interface }

  IDirectPlayLobby2 = interface(IDirectPlayLobby)
    ['{0194C220-A303-11D0-9C4F-00A0C905425E}']
    (*** IDirectPlayLobby2 methods ***)
    function CreateCompoundAddress(const lpElements: DPCOMPOUNDADDRESSELEMENT;
        dwElementCount: DWORD; var lpAddress; var lpdwAddressSize: DWORD):
        HRESULT; stdcall;
  end;

{ DirectPlayLobby API Prototypes }

function DirectPlayLobbyCreateW(const lpguidSP: TGUID; out lplpDPL: IDirectPlayLobby;
    lpUnk: IUnknown; lpData: Pointer; dwDataSize: DWORD): HRESULT; stdcall;
function DirectPlayLobbyCreateA(const lpguidSP: TGUID; out lplpDPL: IDirectPlayLobbyA;
    lpUnk: IUnknown; lpData: Pointer; dwDataSize: DWORD): HRESULT; stdcall;
function DirectPlayLobbyCreate(const lpguidSP: TGUID; out lplpDPL: IDirectPlayLobbyA;
    lpUnk: IUnknown; lpData: Pointer; dwDataSize: DWORD): HRESULT; stdcall;

{ DirectPlayLobby Flags }

const
  DPLMSG_SYSTEM = $00000001;
  DPLMSG_STANDARD = $00000002;


{ DirectPlayLobby messages and message data structures }

type
  DPLMSG_GENERIC = record
    dwType: DWORD;   // Message type
  end;
  LPDPLMSG_GENERIC = ^DPLMSG_GENERIC;

  DPLMSG_SETPROPERTY = record
    dwType: DWORD;                           // Message type
    dwRequestID: DWORD;                      // Request ID (DPL_NOCONFIRMATION if no confirmation desired)
    guidPlayer: TGUID;                       // Player GUID
    guidPropertyTag: TGUID;                  // Property GUID
    dwDataSize: DWORD;                       // Size of data
    dwPropertyData: array[0..0] of DWORD;   // Buffer containing data
  end;
  LPDPLMSG_SETPROPERTY = ^DPLMSG_SETPROPERTY;

const
  DPL_NOCONFIRMATION = 0;

type
  DPLMSG_SETPROPERTYRESPONSE = record
    dwType: DWORD;            // Message type
    dwRequestID: DWORD;       // Request ID
    guidPlayer: TGUID;        // Player GUID
    guidPropertyTag: TGUID;   // Property GUID
    hr: HRESULT;              // Return Code
  end;
  LPDPLMSG_SETPROPERTYRESPONSE = ^DPLMSG_SETPROPERTYRESPONSE;

  DPLMSG_GETPROPERTY = record
    dwType: DWORD;            // Message type
    dwRequestID: DWORD;       // Request ID
    guidPlayer: TGUID;        // Player GUID
    guidPropertyTag: TGUID;   // Property GUID
  end;
  LPDPLMSG_GETPROPERTY = ^DPLMSG_GETPROPERTY;

  DPLMSG_GETPROPERTYRESPONSE = record
    dwType: DWORD;                           // Message type
    dwRequestID: DWORD;                      // Request ID
    guidPlayer: TGUID;                       // Player GUID
    guidPropertyTag: TGUID;                  // Property GUID
    hr: HRESULT;                             // Return Code
    dwDataSize: DWORD;                       // Size of data
    dwPropertyData: array[0..0] of DWORD;   // Buffer containing data
  end;
  LPDPLMSG_GETPROPERTYRESPONSE = ^DPLMSG_GETPROPERTYRESPONSE;


{ DirectPlay Lobby message dwType values }

const
  DPLSYS_CONNECTIONSETTINGSREAD = $00000001;
  DPLSYS_DPLAYCONNECTFAILED = $00000002;
  DPLSYS_DPLAYCONNECTSUCCEEDED = $00000003;
  DPLSYS_APPTERMINATED = $00000004;
  DPLSYS_SETPROPERTY = $00000005;
  DPLSYS_SETPROPERTYRESPONSE = $00000006;
  DPLSYS_GETPROPERTY = $00000007;
  DPLSYS_GETPROPERTYRESPONSE = $00000008;

{ DirectPlay defined property GUIDs and associated data structures }

  DPLPROPERTY_MessagesSupported: TGUID = (D1:$762ccda1;D2:$d916;D3:$11d0;D4:($ba,$39,$0,$c0,$4f,$d7,$ed,$67));
  DPLPROPERTY_PlayerGuid: TGUID = (D1:$b4319322;D2:$d20d;D3:$11d0;D4:($ba,$39,$0,$c0,$4f,$d7,$ed,$67));

{ DPLDATA_PLAYERGUID }
type
  DPLDATA_PLAYERGUID = record
    guidPlayer: TGUID;
    dwPlayerFlags: DWORD;
  end;
  LPDPLDATA_PLAYERGUID = ^DPLDATA_PLAYERGUID;

{ DPLPROPERTY_PlayerScore }

const
  DPLPROPERTY_PlayerScore: TGUID = (D1:$48784000;D2:$d219;D3:$11d0;D4:($ba,$39,$0,$c0,$4f,$d7,$ed,$67));

{ DPLDATA_PLAYERSCORE }

type
  DPLDATA_PLAYERSCORE = record
    dwScoreCount: DWORD;
    Score: array[0..0] of Longint;
  end;
  LPDPLDATA_PLAYERSCORE = ^DPLDATA_PLAYERSCORE;

{ DirectPlay Address ID's }

  DPADDRESS = record
    guidDataType: TGUID;
    dwDataSize: DWORD;
  end;
  LPDPADDRESS = ^DPADDRESS;

const
  DPAID_TotalSize: TGUID = (D1:$1318f560;D2:$912c;D3:$11d0;D4:($9d,$aa,$0,$a0,$c9,$a,$43,$cb));
  DPAID_ServiceProvider: TGUID = (D1:$7d916c0;D2:$e0af;D3:$11cf;D4:($9c,$4e,$0,$a0,$c9,$5,$42,$5e));
  DPAID_LobbyProvider: TGUID = (D1:$59b95640;D2:$9667;D3:$11d0;D4:($a7,$7d,$0,$0,$f8,$3,$ab,$fc));
  DPAID_Phone: TGUID = (D1:$78ec89a0;D2:$e0af;D3:$11cf;D4:($9c,$4e,$0,$a0,$c9,$5,$42,$5e));
  DPAID_PhoneW: TGUID = (D1:$ba5a7a70;D2:$9dbf;D3:$11d0;D4:($9c,$c1,$0,$a0,$c9,$5,$42,$5e));
  DPAID_Modem: TGUID = (D1:$f6dcc200;D2:$a2fe;D3:$11d0;D4:($9c,$4f,$0,$a0,$c9,$5,$42,$5e));
  DPAID_ModemW: TGUID = (D1:$1fd92e0;D2:$a2ff;D3:$11d0;D4:($9c,$4f,$0,$a0,$c9,$5,$42,$5e));
  DPAID_INet: TGUID = (D1:$c4a54da0;D2:$e0af;D3:$11cf;D4:($9c,$4e,$0,$a0,$c9,$5,$42,$5e));
  DPAID_INetW: TGUID = (D1:$e63232a0;D2:$9dbf;D3:$11d0;D4:($9c,$c1,$0,$a0,$c9,$5,$42,$5e));

type
  DPCOMPORTADDRESS = record
    dwComPort: DWORD;       // COM port to use (1-4)
    dwBaudRate: DWORD;      // baud rate (100-256k)
    dwStopBits: DWORD;      // no. stop bits (1-2)
    dwParity: DWORD;        // parity (none, odd, even, mark)
    dwFlowControl: DWORD;   // flow control (none, xon/xoff, rts, dtr)
  end;
  LPDPCOMPORTADDRESS = ^DPCOMPORTADDRESS;

const
  DPAID_ComPort: TGUID = (D1:$f2f0ce00;D2:$e0af;D3:$11cf;D4:($9c,$4e,$0,$a0,$c9,$5,$42,$5e));

{ dplobby 1.0 obsolete definitions }

  DPLAD_SYSTEM = DPLMSG_SYSTEM;

(*==========================================================================
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dsetup.h
 *  Content:    DirectXSetup, error codes and flags
 *
 ***************************************************************************)

// DSETUP Error Codes, must remain compatible with previous setup.
const
  DSETUPERR_SUCCESS_RESTART     = 1;
  DSETUPERR_SUCCESS             = 0;
  DSETUPERR_BADWINDOWSVERSION   = -1;
  DSETUPERR_SOURCEFILENOTFOUND  = -2;
  DSETUPERR_BADSOURCESIZE       = -3;
  DSETUPERR_BADSOURCETIME       = -4;
  DSETUPERR_NOCOPY              = -5;
  DSETUPERR_OUTOFDISKSPACE      = -6;
  DSETUPERR_CANTFINDINF         = -7;
  DSETUPERR_CANTFINDDIR         = -8;
  DSETUPERR_INTERNAL            = -9;
  DSETUPERR_UNKNOWNOS           = -11;
  DSETUPERR_USERHITCANCEL       = -12;
  DSETUPERR_NOTPREINSTALLEDONNT = -13;

// DSETUP flags. DirectX 5.0 apps should use these flags only.
  DSETUP_DDRAWDRV     = $00000008;   (* install DirectDraw Drivers           *)
  DSETUP_DSOUNDDRV    = $00000010;   (* install DirectSound Drivers          *)
  DSETUP_DXCORE       = $00010000;   (* install DirectX runtime              *)
  DSETUP_DIRECTX = DSETUP_DXCORE or DSETUP_DDRAWDRV or DSETUP_DSOUNDDRV;
  DSETUP_TESTINSTALL  = $00020000;   (* just test install, don't do anything *)

// These OBSOLETE flags are here for compatibility with pre-DX5 apps only.
// They are present to allow DX3 apps to be recompiled with DX5 and still work.
// DO NOT USE THEM for DX5. They will go away in future DX releases.

  DSETUP_DDRAW         = $00000001; (* OBSOLETE. install DirectDraw           *)
  DSETUP_DSOUND        = $00000002; (* OBSOLETE. install DirectSound          *)
  DSETUP_DPLAY         = $00000004; (* OBSOLETE. install DirectPlay           *)
  DSETUP_DPLAYSP       = $00000020; (* OBSOLETE. install DirectPlay Providers *)
  DSETUP_DVIDEO        = $00000040; (* OBSOLETE. install DirectVideo          *)
  DSETUP_D3D           = $00000200; (* OBSOLETE. install Direct3D             *)
  DSETUP_DINPUT        = $00000800; (* OBSOLETE. install DirectInput          *)
  DSETUP_DIRECTXSETUP  = $00001000; (* OBSOLETE. install DirectXSetup DLL's   *)
  DSETUP_NOUI          = $00002000; (* OBSOLETE. install DirectX with NO UI   *)
  DSETUP_PROMPTFORDRIVERS = $10000000; (* OBSOLETE. prompt when replacing display/audio drivers *)
  DSETUP_RESTOREDRIVERS = $20000000;(* OBSOLETE. restore display/audio drivers *)

//******************************************************************
// DirectX Setup Callback mechanism
//******************************************************************

// DSETUP Message Info Codes, passed to callback as Reason parameter.
  DSETUP_CB_MSG_NOMESSAGE                 = 0;
  DSETUP_CB_MSG_CANTINSTALL_UNKNOWNOS     = 1;
  DSETUP_CB_MSG_CANTINSTALL_NT            = 2;
  DSETUP_CB_MSG_CANTINSTALL_BETA          = 3;
  DSETUP_CB_MSG_CANTINSTALL_NOTWIN32      = 4;
  DSETUP_CB_MSG_CANTINSTALL_WRONGLANGUAGE = 5;
  DSETUP_CB_MSG_CANTINSTALL_WRONGPLATFORM = 6;
  DSETUP_CB_MSG_PREINSTALL_NT             = 7;
  DSETUP_CB_MSG_NOTPREINSTALLEDONNT       = 8;
  DSETUP_CB_MSG_SETUP_INIT_FAILED         = 9;
  DSETUP_CB_MSG_INTERNAL_ERROR            = 10;
  DSETUP_CB_MSG_CHECK_DRIVER_UPGRADE      = 11;
  DSETUP_CB_MSG_OUTOFDISKSPACE            = 12;
  DSETUP_CB_MSG_BEGIN_INSTALL             = 13;
  DSETUP_CB_MSG_BEGIN_INSTALL_RUNTIME     = 14;
  DSETUP_CB_MSG_BEGIN_INSTALL_DRIVERS     = 15;
  DSETUP_CB_MSG_BEGIN_RESTORE_DRIVERS     = 16;
  DSETUP_CB_MSG_FILECOPYERROR             = 17;


  DSETUP_CB_UPGRADE_TYPE_MASK      = $000F;
  DSETUP_CB_UPGRADE_KEEP           = $0001;
  DSETUP_CB_UPGRADE_SAFE           = $0002;
  DSETUP_CB_UPGRADE_FORCE          = $0004;
  DSETUP_CB_UPGRADE_UNKNOWN        = $0008;

  DSETUP_CB_UPGRADE_HASWARNINGS    = $0100;
  DSETUP_CB_UPGRADE_CANTBACKUP     = $0200;

  DSETUP_CB_UPGRADE_DEVICE_ACTIVE  = $0800;

  DSETUP_CB_UPGRADE_DEVICE_DISPLAY = $1000;
  DSETUP_CB_UPGRADE_DEVICE_MEDIA   = $2000;


type
  DSETUP_CB_UPGRADEINFO = record
    UpgradeFlags: DWORD;
  end;
  LPDSETUP_CB_UPGRADEINFO = ^DSETUP_CB_UPGRADEINFO;

  DSETUP_CB_FILECOPYERROR = record
    dwError: DWORD;
  end;
  LPDSETUP_CB_FILECOPYERROR = ^DSETUP_CB_FILECOPYERROR;

//
// Data Structures
//
  DIRECTXREGISTERAPPA = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpszApplicationName: PAnsiChar;
    lpGUID: PGUID;
    lpszFilename: PAnsiChar;
    lpszCommandLine: PAnsiChar;
    lpszPath: PAnsiChar;
    lpszCurrentDirectory: PAnsiChar;
  end;
  LPDIRECTXREGISTERAPPA = ^DIRECTXREGISTERAPPA;

  DIRECTXREGISTERAPPW = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpszApplicationName: PWideChar;
    lpGUID: PGUID;
    lpszFilename: PWideChar;
    lpszCommandLine: PWideChar;
    lpszPath: PWideChar;
    lpszCurrentDirectory: PWideChar;
  end;
  LPDIRECTXREGISTERAPPW = ^DIRECTXREGISTERAPPW;

  DIRECTXREGISTERAPP = DIRECTXREGISTERAPPA;
  LPDIRECTXREGISTERAPP = LPDIRECTXREGISTERAPPA;


//
// API
//
function DirectXSetupA(hWnd: HWND; lpszRootPath: PAnsiChar; dwFlags: DWORD):
    Integer; stdcall;
function DirectXSetupW(hWnd: HWND; lpszRootPath: PWideChar; dwFlags: DWORD):
    Integer; stdcall;
function DirectXSetup(hWnd: HWND; lpszRootPath: PAnsiChar; dwFlags: DWORD):
    Integer; stdcall;

function DirectXDeviceDriverSetupA(hWnd: HWND; lpszDriverClass: PAnsiChar;
    lpszDriverPath: PAnsiChar; dwFlags: DWORD): Integer; stdcall;
function DirectXDeviceDriverSetupW(hWnd: HWND; lpszDriverClass: PWideChar;
    lpszDriverPath: PWideChar; dwFlags: DWORD): Integer; stdcall;
function DirectXDeviceDriverSetup(hWnd: HWND; lpszDriverClass: PAnsiChar;
    lpszDriverPath: PAnsiChar; dwFlags: DWORD): Integer; stdcall;

function DirectXRegisterApplicationA(hWnd: HWND;
    var lpDXRegApp: DIRECTXREGISTERAPPA): Integer; stdcall;
function DirectXRegisterApplicationW(hWnd: HWND;
    var lpDXRegApp: DIRECTXREGISTERAPPW): Integer; stdcall;
function DirectXRegisterApplication(hWnd: HWND;
    var lpDXRegApp: DIRECTXREGISTERAPPA): Integer; stdcall;

function DirectXUnRegisterApplication(hWnd: HWND; const lpGUID: TGUID): Integer;
    stdcall;

type
  DSETUP_CALLBACK = function (Reason: DWORD; MsgType: DWORD;
      szMessage: PAnsiChar; szName: PAnsiChar; pInfo: Pointer): DWORD; stdcall;

function DirectXSetupSetCallback(Callback: DSETUP_CALLBACK): Integer; stdcall;

function DirectXSetupGetVersion(var lpdwVersion: DWORD;
    var lpdwMinorVersion: DWORD): Integer; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1995,1996 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dsound.h
 *  Content:    DirectSound include file
 *
 **************************************************************************)

{ GUIDS used by DirectDraw objects }

const
  CLSID_DirectSound: TGUID = (D1:$47d4d946;D2:$62e8;D3:$11cf;D4:($93,$bc,$44,$45,$53,$54,$0,$0));
  CLSID_DirectSoundCapture: TGUID = (D1:$b0210780;D2:$89cd;D3:$11d0;D4:($af,$8,$0,$a0,$c9,$25,$cd,$16));
  IID_IDirectSound: TGUID = (D1:$279AFA83;D2:$4981;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectSoundBuffer: TGUID = (D1:$279AFA85;D2:$4981;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectSound3DListener: TGUID = (D1:$279AFA84;D2:$4981;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectSound3DBuffer: TGUID = (D1:$279AFA86;D2:$4981;D3:$11CE;D4:($A5,$21,$00,$20,$AF,$0B,$E5,$60));
  IID_IDirectSoundCapture: TGUID = (D1:$b0210781;D2:$89cd;D3:$11d0;D4:($af,$8,$0,$a0,$c9,$25,$cd,$16));
  IID_IDirectSoundCaptureBuffer: TGUID = (D1:$b0210782;D2:$89cd;D3:$11d0;D4:($af,$8,$0,$a0,$c9,$25,$cd,$16));
  IID_IDirectSoundNotify: TGUID = (D1:$b0210783;D2:$89cd;D3:$11d0;D4:($af,$8,$0,$a0,$c9,$25,$cd,$16));

{ DirectSound Structures }

type
  IDirectSound = interface;
  IDirectSoundBuffer = interface;
  IDirectSound3DListener = interface;
  IDirectSound3DBuffer = interface;
  IDirectSoundCapture = interface;
  IDirectSoundCaptureBuffer = interface;
  IDirectSoundNotify = interface;

{ DSCAPS }

  DSCAPS = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwMinSecondarySampleRate: DWORD;
    dwMaxSecondarySampleRate: DWORD;
    dwPrimaryBuffers: DWORD;
    dwMaxHwMixingAllBuffers: DWORD;
    dwMaxHwMixingStaticBuffers: DWORD;
    dwMaxHwMixingStreamingBuffers: DWORD;
    dwFreeHwMixingAllBuffers: DWORD;
    dwFreeHwMixingStaticBuffers: DWORD;
    dwFreeHwMixingStreamingBuffers: DWORD;
    dwMaxHw3DAllBuffers: DWORD;
    dwMaxHw3DStaticBuffers: DWORD;
    dwMaxHw3DStreamingBuffers: DWORD;
    dwFreeHw3DAllBuffers: DWORD;
    dwFreeHw3DStaticBuffers: DWORD;
    dwFreeHw3DStreamingBuffers: DWORD;
    dwTotalHwMemBytes: DWORD;
    dwFreeHwMemBytes: DWORD;
    dwMaxContigFreeHwMemBytes: DWORD;
    dwUnlockTransferRateHwBuffers: DWORD;
    dwPlayCpuOverheadSwBuffers: DWORD;
    dwReserved1: DWORD;
    dwReserved2: DWORD;
  end;
  LPDSCAPS = ^DSCAPS;

{ DSBCAPS }

  DSBCAPS = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    dwUnlockTransferRate: DWORD;
    dwPlayCpuOverhead: DWORD;
  end;
  LPDSBCAPS = ^DSBCAPS;

{ DSBUFFERDESC }

  TDSBUFFERDESC = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    dwReserved: DWORD;
    lpwfxFormat: PWaveFormatEx;
  end;
  LPDSBUFFERDESC = ^TDSBUFFERDESC;

{ DS3DBUFFER }

  DS3DBUFFER = record
    dwSize: DWORD;
    vPosition: D3DVECTOR;
    vVelocity: D3DVECTOR;
    dwInsideConeAngle: DWORD;
    dwOutsideConeAngle: DWORD;
    vConeOrientation: D3DVECTOR;
    lConeOutsideVolume: Longint;
    flMinDistance: D3DVALUE;
    flMaxDistance: D3DVALUE;
    dwMode: DWORD;
  end;
  LPDS3DBUFFER = ^DS3DBUFFER;

{ DS3DLISTENER }

  DS3DLISTENER = record
    dwSize: DWORD;
    vPosition: D3DVECTOR;
    vVelocity: D3DVECTOR;
    vOrientFront: D3DVECTOR;
    vOrientTop: D3DVECTOR;
    flDistanceFactor: D3DVALUE;
    flRolloffFactor: D3DVALUE;
    flDopplerFactor: D3DVALUE;
  end;
  LPDS3DLISTENER = ^DS3DLISTENER;

{ DSCCAPS }

  DSCCAPS = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwFormats: DWORD;
    dwChannels: DWORD;
  end;
  LPDSCCAPS = ^DSCCAPS;

{ DSCBUFFERDESC }

  DSCBUFFERDESC = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    dwReserved: DWORD;
    lpwfxFormat: PWaveFormatEx;
  end;
  LPDSCBUFFERDESC = ^DSCBUFFERDESC;

{ DSCBCAPS }

  DSCBCAPS = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    dwReserved: DWORD;
  end;
  LPDSCBCAPS = ^DSCBCAPS;

{ DSBPOSITIONNOTIFY }

  DSBPOSITIONNOTIFY = record
    dwOffset: DWORD;
    hEventNotify: THandle;
  end;
  LPDSBPOSITIONNOTIFY = ^DSBPOSITIONNOTIFY;

{ DirectSound API }

  LPDSENUMCALLBACKW = function(lpGuid: PGUID; lpstrDescription: LPCWSTR;
    lpstrModule: LPCWSTR; lpContext: Pointer): BOOL;
  LPDSENUMCALLBACKA = function(lpGuid: PGUID; lpstrDescription: LPCSTR;
    lpstrModule: LPCSTR; lpContext: Pointer): BOOL; stdcall;

  LPDSENUMCALLBACK = LPDSENUMCALLBACKA;

{ IDirectSound }

  IDirectSound = interface(IUnknown)
    ['{279AFA83-4981-11CE-A521-0020AF0BE560}']
    (*** IDirectSound methods ***)
    function CreateSoundBuffer(const lpDSBufferDesc: TDSBUFFERDESC;
        out lplpDirectSoundBuffer: IDirectSoundBuffer;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function GetCaps(var lpDSCaps: DSCAPS): HRESULT; stdcall;
    function DuplicateSoundBuffer(lpDsbOriginal: IDirectSoundBuffer;
        out lpDsbDuplicate: IDirectSoundBuffer): HRESULT; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwLevel: DWORD): HRESULT; stdcall;
    function Compact: HRESULT; stdcall;
    function GetSpeakerConfig(var lpdwSpeakerConfig: DWORD): HRESULT; stdcall;
    function SetSpeakerConfig(dwSpeakerConfig: DWORD): HRESULT; stdcall;
    function Initialize(lpGuid: PGUID): HRESULT; stdcall;
  end;

{ IDirectSoundBuffer }

  IDirectSoundBuffer = interface(IUnknown)
    ['{279AFA85-4981-11CE-A521-0020AF0BE560}']
    (*** IDirectSoundBuffer methods ***)
    function GetCaps(var lpDSBufferCaps: DSBCAPS): HRESULT; stdcall;
    function GetCurrentPosition(var lpdwCurrentPlayCursor,
        lpdwCurrentWriteCursor: DWORD): HRESULT; stdcall;
    function GetFormat(var lpwfxFormat: TWaveFormatEx; dwSizeAllocated: DWORD;
        var lpdwSizeWritten: DWORD): HRESULT; stdcall;
    function GetVolume(var lplVolume: Longint): HRESULT; stdcall;
    function GetPan(var lplPan: Longint): HRESULT; stdcall;
    function GetFrequency(var lpdwFrequency: DWORD): HRESULT; stdcall;
    function GetStatus(var lpdwStatus: DWORD): HRESULT; stdcall;
    function Initialize(lpDirectSound: IDirectSound; const
        lpDSBufferDesc: TDSBUFFERDESC): HRESULT; stdcall;
    function Lock(dwWriteCursor: DWORD; dwWriteBytes: DWORD;
        var lplpvAudioPtr1: Pointer; var lpdwAudioBytes1: DWORD;
        var lplpvAudioPtr2: Pointer; var lpdwAudioBytes2: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function Play(dwReserved1, dwReserved2: DWORD; dwFlags: DWORD): HRESULT;
        stdcall;
    function SetCurrentPosition(dwNewPosition: DWORD): HRESULT; stdcall;
    function SetFormat(const lpfxFormat: TWaveFormatEx): HRESULT; stdcall;
    function SetVolume(lVolume: Longint): HRESULT; stdcall;
    function SetPan(lPan: Longint): HRESULT; stdcall;
    function SetFrequency(dwFrequency: DWORD): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Unlock(lpvAudioPtr1: Pointer; dwAudioBytes1: DWORD;
        lpvAudioPtr2: Pointer; dwAudioBytes2: DWORD): HRESULT; stdcall;
    function Restore: HRESULT; stdcall;
  end;

{ IDirectSound3DListener }

  IDirectSound3DListener = interface(IUnknown)
    ['{279AFA84-4981-11CE-A521-0020AF0BE560}']
    (*** IDirectSound3DListener methods ***)
    function GetAllParameters(var lpListener: DS3DLISTENER): HRESULT; stdcall;
    function GetDistanceFactor(var lpflDistanceFactor: D3DVALUE): HRESULT;
        stdcall;
    function GetDopplerFactor(var lpflDopplerFactor: D3DVALUE): HRESULT;
        stdcall;
    function GetOrientation(var lpvOrientFront, lpvOrientTop: D3DVECTOR):
        HRESULT; stdcall;
    function GetPosition(var lpvPosition: D3DVECTOR): HRESULT; stdcall;
    function GetRolloffFactor(var lpflRolloffFactor: D3DVALUE): HRESULT;
        stdcall;
    function GetVelocity(var lpvVelocity: D3DVECTOR): HRESULT; stdcall;
    function SetAllParameters(const lpListener: DS3DLISTENER; dwApply: DWORD):
        HRESULT; stdcall;
    function SetDistanceFactor(flDistanceFactor: D3DVALUE; dwApply: DWORD):
        HRESULT; stdcall;
    function SetDopplerFactor(flDopplerFactor: D3DVALUE; dwApply: DWORD):
        HRESULT; stdcall;
    function SetOrientation(xFront, yFront, zFront, xTop, yTop, zTop: D3DVALUE;
        dwApply: DWORD): HRESULT; stdcall;
    function SetPosition(x, y, z: D3DVALUE; dwApply: DWORD): HRESULT; stdcall;
    function SetRolloffFactor(flRolloffFactor: D3DVALUE; dwApply: DWORD):
        HRESULT; stdcall;
    function SetVelocity(x, y, z: D3DVALUE; dwApply: DWORD): HRESULT; stdcall;
    function CommitDeferredSettings: HRESULT; stdcall;
  end;

{ IDirectSound3DBuffer }

  IDirectSound3DBuffer = interface(IUnknown)
    ['{279AFA86-4981-11CE-A521-0020AF0BE560}']
    (*** IDirectSound3DBuffer methods ***)
    function GetAllParameters(var lpDs3dBuffer: DS3DBUFFER): HRESULT; stdcall;
    function GetConeAngles(var lpdwInsideConeAngle: DWORD;
        var lpdwOutsideConeAngle: DWORD): HRESULT; stdcall;
    function GetConeOrientation(var lpvOrientation: D3DVECTOR): HRESULT;
        stdcall;
    function GetConeOutsideVolume(var lplConeOutsideVolume: Longint): HRESULT;
        stdcall;
    function GetMaxDistance(var lpflMaxDistance: D3DVALUE): HRESULT; stdcall;
    function GetMinDistance(var lpflMinDistance: D3DVALUE): HRESULT; stdcall;
    function GetMode(var lpdwMod: DWORD): HRESULT; stdcall;
    function GetPosition(var lpvPosition: D3DVECTOR): HRESULT; stdcall;
    function GetVelocity(var lpvVelocity: D3DVECTOR): HRESULT; stdcall;
    function SetAllParameters(const lpDs3dBuffer: DS3DBUFFER; dwApply: DWORD):
        HRESULT; stdcall;
    function SetConeAngles(dwInsideConeAngle: DWORD; dwOutsideConeAngle: DWORD;
        dwApply: DWORD): HRESULT; stdcall;
    function SetConeOrientation(x, y, z: D3DVALUE; dwApply: DWORD): HRESULT; stdcall;
    function SetConeOutsideVolume(lConeOutsideVolume: Longint; dwApply: DWORD):
        HRESULT; stdcall;
    function SetMaxDistance(flMaxDistance: D3DVALUE; dwApply: DWORD): HRESULT;
        stdcall;
    function SetMinDistance(flMinDistance: D3DVALUE; dwApply: DWORD): HRESULT;
        stdcall;
    function SetMode(dwMode: DWORD; dwApply: DWORD): HRESULT; stdcall;
    function SetPosition(x, y, z: D3DVALUE; dwApply: DWORD): HRESULT; stdcall;
    function SetVelocity(x, y, z: D3DVALUE; dwApply: DWORD): HRESULT; stdcall;
  end;

{ IDirectSoundCapture }

  IDirectSoundCapture = interface(IUnknown)
    (*** IDirectSoundCapture methods ***)
    function CreateCaptureBuffer(const lpDSCBufferDesc: DSCBUFFERDESC;
        out lplpDirectSoundCaptureBuffer: IDirectSoundCaptureBuffer;
        pUnkOuter: IUnknown): HRESULT; stdcall;
    function GetCaps(var lpDSCCaps: DSCCAPS): HRESULT; stdcall;
    function Initialize(lpGuid: PGUID): HRESULT; stdcall;
  end;

{ IDirectSoundCaptureBuffer }

  IDirectSoundCaptureBuffer = interface(IUnknown)
    ['{B0210782-89CD-11D0-AF08-00A0C925CD16}']
    (*** IDirectSoundCaptureBuffer methods ***)
    function GetCaps(var lpDSCBCaps: DSCBCAPS): HRESULT; stdcall;
    function GetCurrentPosition(var lpdwCapturePosition,
        lpdwReadPosition: DWORD): HRESULT; stdcall;
    function GetFormat(var lpwfxFormat: TWaveFormatEx; dwSizeAllocated: DWORD;
        var lpdwSizeWritten: DWORD): HRESULT; stdcall;
    function GetStatus(var lpdwStatus: DWORD): HRESULT; stdcall;
    function Initialize(lpDirectSoundCapture: IDirectSoundCapture;
        const lpcDSBufferDesc: DSCBUFFERDESC): HRESULT; stdcall;
    function Lock(dwReadCursor: DWORD; dwReadBytes: DWORD;
        var lplpvAudioPtr1: Pointer; var lpdwAudioBytes1: DWORD;
        var lplpvAudioPtr2: Pointer; var lpdwAudioBytes2: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function Start(dwFlags: DWORD): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Unlock(lpvAudioPtr1: Pointer; dwAudioBytes1: DWORD;
        lpvAudioPtr2: Pointer; dwAudioBytes2: DWORD): HRESULT; stdcall;
  end;

{ IDirectSoundNotify }

  IDirectSoundNotify = interface(IUnknown)
    ['{B0210783-89CD-11D0-AF08-00A0C925CD16}']
    (*** IDirectSoundNotify methods ***)
    function SetNotificationPositions(cPositionNotifies: DWORD;
        const lpcPositionNotifies: DSBPOSITIONNOTIFY): HRESULT; stdcall;
  end;

{ IKsPropertySet }

const
  KSPROPERTY_SUPPORT_GET = $00000001;
  KSPROPERTY_SUPPORT_SET = $00000002;

  IID_IKsPropertySet: TGUID = (D1:$31efac30;D2:$515c;D3:$11d0;D4:($a9,$aa,$00,$aa,$00,$61,$be,$93));

type
  IKsPropertySet = interface;

  IKsPropertySet = interface(IUnknown)
    ['{31EFAC30-515C-11D0-A9AA-00AA0061BE93}']
    (*** IKsPropertySet methods ***)
    function GetProperty(const PropertySetId: TGUID; PropertyId: DWORD;
        var pPropertyParams; cbPropertyParams: DWORD;
        var pPropertyData; cbPropertyData: DWORD;
        var pcbReturnedData: ULONG): HRESULT; stdcall;
    function SetProperty(const PropertySetId: TGUID; PropertyId: DWORD;
        const pPropertyParams; cbPropertyParams: DWORD;
        const pPropertyData; cbPropertyData: DWORD): HRESULT; stdcall;
    function QuerySupport(const PropertySetId: TGUID; PropertyId: DWORD;
        var pSupport: ULONG): HRESULT; stdcall;
  end;

{ Return Codes }

const
  DS_OK = 0;
  DSERR_ALLOCATED = $88780000 + 10;
  DSERR_CONTROLUNAVAIL = $88780000 + 30;
  DSERR_INVALIDPARAM = E_INVALIDARG;
  DSERR_INVALIDCALL = $88780000 + 50;
  DSERR_GENERIC = E_FAIL;
  DSERR_PRIOLEVELNEEDED = $88780000 + 70;
  DSERR_OUTOFMEMORY = E_OUTOFMEMORY;
  DSERR_BADFORMAT = $88780000 + 100;
  DSERR_UNSUPPORTED = E_NOTIMPL;
  DSERR_NODRIVER = $88780000 + 120;
  DSERR_ALREADYINITIALIZED = $88780000 + 130;
  DSERR_NOAGGREGATION = CLASS_E_NOAGGREGATION;
  DSERR_BUFFERLOST = $88780000 + 150;
  DSERR_OTHERAPPHASPRIO = $88780000 + 160;
  DSERR_UNINITIALIZED = $88780000 + 170;
  DSERR_NOINTERFACE = E_NOINTERFACE;

{ Flags }

  DSCAPS_PRIMARYMONO      = $00000001;
  DSCAPS_PRIMARYSTEREO    = $00000002;
  DSCAPS_PRIMARY8BIT      = $00000004;
  DSCAPS_PRIMARY16BIT     = $00000008;
  DSCAPS_CONTINUOUSRATE   = $00000010;
  DSCAPS_EMULDRIVER       = $00000020;
  DSCAPS_CERTIFIED        = $00000040;
  DSCAPS_SECONDARYMONO    = $00000100;
  DSCAPS_SECONDARYSTEREO  = $00000200;
  DSCAPS_SECONDARY8BIT    = $00000400;
  DSCAPS_SECONDARY16BIT   = $00000800;

  DSBPLAY_LOOPING         = $00000001;

  DSBSTATUS_PLAYING       = $00000001;
  DSBSTATUS_BUFFERLOST    = $00000002;
  DSBSTATUS_LOOPING       = $00000004;

  DSBLOCK_FROMWRITECURSOR = $00000001;
  DSBLOCK_ENTIREBUFFER    = $00000002;

  DSSCL_NORMAL            = $00000001;
  DSSCL_PRIORITY          = $00000002;
  DSSCL_EXCLUSIVE         = $00000003;
  DSSCL_WRITEPRIMARY      = $00000004;

  DS3DMODE_NORMAL         = $00000000;
  DS3DMODE_HEADRELATIVE   = $00000001;
  DS3DMODE_DISABLE        = $00000002;

  DS3D_IMMEDIATE          = $00000000;
  DS3D_DEFERRED           = $00000001;

  DS3D_MINDISTANCEFACTOR     = 0.0;
  DS3D_MAXDISTANCEFACTOR     = 10.0;
  DS3D_DEFAULTDISTANCEFACTOR = 1.0;

  DS3D_MINROLLOFFFACTOR      = 0.0;
  DS3D_MAXROLLOFFFACTOR      = 10.0;
  DS3D_DEFAULTROLLOFFFACTOR  = 1.0;

  DS3D_MINDOPPLERFACTOR      = 0.0;
  DS3D_MAXDOPPLERFACTOR      = 10.0;
  DS3D_DEFAULTDOPPLERFACTOR  = 1.0;

  DS3D_DEFAULTMINDISTANCE    = 1.0;
  DS3D_DEFAULTMAXDISTANCE    = 1000000000.0;

  DS3D_MINCONEANGLE          = 0;
  DS3D_MAXCONEANGLE          = 360;
  DS3D_DEFAULTCONEANGLE      = 360;

  DS3D_DEFAULTCONEOUTSIDEVOLUME = 0;

  DSBCAPS_PRIMARYBUFFER       = $00000001;
  DSBCAPS_STATIC              = $00000002;
  DSBCAPS_LOCHARDWARE         = $00000004;
  DSBCAPS_LOCSOFTWARE         = $00000008;
  DSBCAPS_CTRL3D              = $00000010;
  DSBCAPS_CTRLFREQUENCY       = $00000020;
  DSBCAPS_CTRLPAN             = $00000040;
  DSBCAPS_CTRLVOLUME          = $00000080;
  DSBCAPS_CTRLPOSITIONNOTIFY  = $00000100;
  DSBCAPS_CTRLDEFAULT         = $000000E0;
  DSBCAPS_CTRLALL             = $000001F0;
  DSBCAPS_STICKYFOCUS         = $00004000;
  DSBCAPS_GLOBALFOCUS         = $00008000;
  DSBCAPS_GETCURRENTPOSITION2 = $00010000;
  DSBCAPS_MUTE3DATMAXDISTANCE = $00020000;

  DSCBCAPS_WAVEMAPPED = $80000000;

  DSSPEAKER_HEADPHONE = $00000001;
  DSSPEAKER_MONO      = $00000002;
  DSSPEAKER_QUAD      = $00000003;
  DSSPEAKER_STEREO    = $00000004;
  DSSPEAKER_SURROUND  = $00000005;

function DSSPEAKER_COMBINED(c, g: Byte): DWORD;
function DSSPEAKER_CONFIG(a: DWORD): Byte;
function DSSPEAKER_GEOMETRY(a: DWORD): Byte;

const
  DSCCAPS_EMULDRIVER    = $00000020;

  DSCBLOCK_ENTIREBUFFER = $00000001;

  DSCBSTATUS_CAPTURING  = $00000001;
  DSCBSTATUS_LOOPING    = $00000002;

  DSCBSTART_LOOPING     = $00000001;

  DSBFREQUENCY_MIN      = 100;
  DSBFREQUENCY_MAX      = 100000;
  DSBFREQUENCY_ORIGINAL = 0;

  DSBPAN_LEFT   = -10000;
  DSBPAN_CENTER = 0;
  DSBPAN_RIGHT  = 10000;

  DSBVOLUME_MIN = -10000;
  DSBVOLUME_MAX = 0;

  DSBSIZE_MIN = 4;
  DSBSIZE_MAX = $0FFFFFFF;

  DSBPN_OFFSETSTOP = $FFFFFFFF;

function DirectSoundCreate(lpGUID: PGUID; out lpDS: IDirectSound;
  pUnkOuter: IUnknown): HRESULT; stdcall;
function DirectSoundEnumerateA(lpDSEnumCallback: LPDSENUMCALLBACKA;
  lpContext: Pointer): HRESULT; stdcall;
function DirectSoundEnumerateW(lpDSEnumCallback: LPDSENUMCALLBACKW;
  lpContext: Pointer): HRESULT; stdcall;
function DirectSoundEnumerate(lpDSEnumCallback: LPDSENUMCALLBACKA;
  lpContext: Pointer): HRESULT; stdcall;

function DirectSoundCaptureCreate(lpGUID: PGUID;
  out lplpDSC: IDirectSoundCapture; pUnkOuter: IUnknown): HRESULT; stdcall;
function DirectSoundCaptureEnumerateA(lpDSEnumCallback: LPDSENUMCALLBACKA;
  lpContext: Pointer): HRESULT; stdcall;
function DirectSoundCaptureEnumerateW(lpDSEnumCallback: LPDSENUMCALLBACKW;
  lpContext: Pointer): HRESULT; stdcall;
function DirectSoundCaptureEnumerate(lpDSEnumCallback: LPDSENUMCALLBACKA;
  lpContext: Pointer): HRESULT; stdcall;

implementation

const
  ddraw = 'ddraw.dll';
  d3drm = 'd3drm.dll';
  dinput = 'dinput.dll';
  DPlayX = 'dplayx.dll';
  DPLobby = 'dplobby.dll';
  dsetup = 'dsetup.dll';
  dsound = 'dsound.dll';
  
{ DirectDraw }

function DirectDrawEnumerateA; external ddraw;
function DirectDrawEnumerateW; external ddraw;
function DirectDrawEnumerate; external ddraw name 'DirectDrawEnumerateA';

function DirectDrawCreate; external ddraw;
function DirectDrawCreateClipper; external ddraw;

{ Direct3D }

function D3DVALP(val: D3DVALUE; prec: Integer): D3DVALUE;
begin
  Result := val;
end;

function D3DVAL(val: D3DVALUE): D3DVALUE;
begin
  Result := val;
end;

function D3DDivide(a, b: D3DVALUE): D3DVALUE;
begin
  Result := a / b;
end;

function D3DMultiply(a, b: D3DVALUE): D3DVALUE;
begin
  Result := a * b;
end;

function D3DTRIFLAG_STARTFLAT(len: DWORD) : DWORD;
begin
  if not (len in [1..29]) then len := 0;
  result := len;
end;

function CI_GETALPHA(ci: Integer): Byte;
begin
  Result := ci shr 24;
end;

function CI_GETINDEX(ci: Integer): Word;
begin
  Result := ci shr 8;
end;

function CI_GETFRACTION(ci: Integer): Byte;
begin
  Result := ci;
end;

function CI_ROUNDINDEX(ci: Integer): Integer;
begin
  Result := CI_GETINDEX(ci)+$80;
end;

function CI_MASKALPHA(ci: Integer): Integer;
begin
  Result := ci and $FFFFFF;
end;

function CI_MAKE(a: Byte; i: Word; f: Byte): Integer;
begin
  Result := (a shl 24) or (i shl 8) or f;
end;

function RGBA_GETALPHA(rgb: D3DCOLOR): Byte;
begin
  Result := rgb shr 24;
end;

function RGBA_GETRED(rgb: D3DCOLOR): Byte;
begin
  Result := rgb shr 16;
end;

function RGBA_GETGREEN(rgb: D3DCOLOR): Byte;
begin
  Result := rgb shr 8;
end;

function RGBA_GETBLUE(rgb: D3DCOLOR): Byte;
begin
  Result := rgb;
end;

function RGBA_MAKE(r, g, b, a: Byte): D3DCOLOR;
begin
  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

function D3DRGB(r, g, b: D3DVALUE): D3DCOLOR;
begin
  Result := $FF000000 or (Trunc(r*255) shl 16) or (Trunc(g*255) shl 8) or
       (Trunc(b*255));
end;

function D3DRGBA(r, g, b, a: D3DVALUE): D3DCOLOR;
begin
  Result := (Trunc(a*255) shl 24) or (Trunc(r*255) shl 16) or (Trunc(g*255) shl 8) or
    (Trunc(b*255));
end;

function RGB_GETRED(rgb: D3DCOLOR): Byte;
begin
  Result := rgb shr 16;
end;

function RGB_GETGREEN(rgb: D3DCOLOR): Byte;
begin
  Result := rgb shr 8;
end;

function RGB_GETBLUE(rgb: D3DCOLOR): Byte;
begin
  Result := rgb;
end;

function RGBA_SETALPHA(rgba: D3DCOLOR; x: Byte): D3DCOLOR;
begin
  Result := (x shl 24) or (rgba and $00FFFFFF);
end;

function RGB_MAKE(r, g, b: Byte): D3DCOLOR;
begin
  Result := (r shl 16) or (g shl 8) or b;
end;

function RGBA_TORGB(rgba: D3DCOLOR): D3DCOLOR;
begin
  Result := rgba and $00FFFFFF;
end;

function RGB_TORGBA(rgb: D3DCOLOR): D3DCOLOR;
begin
  Result := rgb or $FF000000;
end;

function VectorAdd(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;

function VectorSub(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

function VectorMulS(v: D3DVECTOR; s: D3DVALUE) : D3DVECTOR;
begin
  result.x := v.x*s;
  result.y := v.y*s;
  result.z := v.z*s;
end;

function VectorDivS(v: D3DVECTOR; s: D3DVALUE) : D3DVECTOR;
begin
  result.x := v.x/s;
  result.y := v.y/s;
  result.z := v.z/s;
end;

function VectorMul(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  result.x := v1.x*v2.x;
  result.y := v1.y*v2.y;
  result.z := v1.z*v2.z;
end;

function VectorDiv(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  result.x := v1.x/v2.x;
  result.y := v1.y/v2.y;
  result.z := v1.z/v2.z;
end;

function VectorSmaller(v1, v2: D3DVECTOR) : boolean;
begin
  result := (v1.x < v2.x) and (v1.y < v2.y) and (v1.z < v2.z);
end;

function VectorSmallerEquel(v1, v2: D3DVECTOR) : boolean;
begin
  result := (v1.x <= v2.x) and (v1.y <= v2.y) and (v1.z <= v2.z);
end;

function VectorEquel(v1, v2: D3DVECTOR) : boolean;
begin
  result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

function VectorSquareMagnitude(v: D3DVECTOR) : D3DVALUE;
begin
  result := (v.x*v.x) + (v.y*v.y) + (v.z*v.z);
end;

function VectorMagnitude(v: D3DVECTOR) : D3DVALUE;
begin
  result := sqrt( (v.x*v.x) + (v.y*v.y) + (v.z*v.z) );
end;

function VectorNormalize(v: D3DVECTOR) : D3DVECTOR;
begin
  result := VectorDivS(v,VectorMagnitude(v));
end;

function VectorMin(v: D3DVECTOR) : D3DVALUE;
var
  ret : D3DVALUE;
begin
  ret := v.x;
  if (v.y < ret) then ret := v.y;
  if (v.z < ret) then ret := v.z;
  result := ret;
end;

function VectorMax(v: D3DVECTOR) : D3DVALUE;
var
  ret : D3DVALUE;
begin
  ret := v.x;
  if (ret < v.y) then ret := v.y;
  if (ret < v.z) then ret := v.z;
  result := ret;
end;

function VectorMinimize(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  if v1.x < v2.x then result.x := v1.x else result.x := v2.x;
  if v1.y < v2.y then result.y := v1.y else result.y := v2.y;
  if v1.z < v2.z then result.z := v1.z else result.z := v2.z;
end;

function VectorMaximize(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  if v1.x > v2.x then result.x := v1.x else result.x := v2.x;
  if v1.y > v2.y then result.y := v1.y else result.y := v2.y;
  if v1.z > v2.z then result.z := v1.z else result.z := v2.z;
end;

function VectorDotProduct(v1, v2: D3DVECTOR) : D3DVALUE;
begin
  result := (v1.x*v2.x) + (v1.y * v2.y) + (v1.z*v2.z);
end;

function VectorCrossProduct(v1, v2: D3DVECTOR) : D3DVECTOR;
begin
  result.x := (v1.y*v2.z) - (v1.z*v1.y);
  result.y := (v1.z*v2.x) - (v1.x*v1.z);
  result.z := (v1.x*v2.y) - (v1.y*v1.x);
end;

{ Direct3DRM }

function Direct3DRMCreate; external d3drm;

function D3DRMCreateColorRGB; external d3drm;
function D3DRMCreateColorRGBA; external d3drm;
function D3DRMColorGetRed; external d3drm;
function D3DRMColorGetGreen; external d3drm;
function D3DRMColorGetBlue; external d3drm;
function D3DRMColorGetAlpha; external d3drm;
function D3DRMVectorAdd; external d3drm;
function D3DRMVectorSubtract; external d3drm;
function D3DRMVectorReflect; external d3drm;
function D3DRMVectorCrossProduct; external d3drm;
function D3DRMVectorDotProduct; external d3drm;
function D3DRMVectorNormalize; external d3drm;
function D3DRMVectorModulus; external d3drm;
function D3DRMVectorRotate; external d3drm;
function D3DRMVectorScale; external d3drm;
function D3DRMVectorRandom; external d3drm;
function D3DRMQuaternionFromRotation; external d3drm;
function D3DRMQuaternionMultiply; external d3drm;
function D3DRMQuaternionSlerp; external d3drm;
procedure D3DRMMatrixFromQuaternion; external d3drm;

{ DirectInput }

function GET_DIDEVICE_TYPE(dwDevType: DWORD): DWORD;
begin
  Result := LOBYTE(dwDevType);
end;

function GET_DIDEVICE_SUBTYPE(dwDevType: DWORD): DWORD;
begin
  Result := HIBYTE(dwDevType);
end;

function DIEFT_GETTYPE(n: DWORD): DWORD;
begin
  Result := LOBYTE(n);
end;

function DIDFT_MAKEINSTANCE(n: WORD): DWORD;
begin
  Result := n shl 8;
end;

function DIDFT_GETINSTANCE(n: DWORD): WORD;
begin
  Result := n shr 8;
end;

function DIDFT_ENUMCOLLECTION(n: WORD): DWORD;
begin
  Result := n shl 8;
end;

function DirectInputCreate; external dinput name 'DirectInputCreateA';

{ DirectPlay }

function DirectPlayEnumerateA; external DPlayX;
function DirectPlayEnumerateW; external DPlayX;
function DirectPlayEnumerate; external DPlayX name 'DirectPlayEnumerateA';

function DirectPlayCreate; external DPlayX;

function DirectPlayLobbyCreateW; external DPLobby;
function DirectPlayLobbyCreateA; external DPLobby;
function DirectPlayLobbyCreate; external DPLobby name 'DirectPlayLobbyCreateA';

{ DirectSetup }

function DirectXSetupA; external dsetup;
function DirectXSetupW; external dsetup;
function DirectXSetup; external dsetup name 'DirectXSetupA';

function DirectXDeviceDriverSetupA; external dsetup;
function DirectXDeviceDriverSetupW; external dsetup;
function DirectXDeviceDriverSetup; external dsetup name 'DirectXDeviceDriverSetupA';

function DirectXRegisterApplicationA; external dsetup;
function DirectXRegisterApplicationW; external dsetup;
function DirectXRegisterApplication; external dsetup name 'DirectXRegisterApplicationA';

function DirectXUnRegisterApplication; external dsetup;

function DirectXSetupSetCallback; external dsetup;

function DirectXSetupGetVersion; external dsetup;

{ DirectSound }

function DSSPEAKER_COMBINED(c, g: Byte): DWORD;
begin
  Result := c or (g shl 16);
end;

function DSSPEAKER_CONFIG(a: DWORD): Byte;
begin
  Result := a;
end;

function DSSPEAKER_GEOMETRY(a: DWORD): Byte;
begin
  Result := a shr 16;
end;

function DirectSoundCreate; external dsound;
function DirectSoundEnumerateA; external dsound;
function DirectSoundEnumerateW; external dsound;
function DirectSoundEnumerate; external dsound name 'DirectSoundEnumerateA';

function DirectSoundCaptureCreate; external dsound;
function DirectSoundCaptureEnumerateA; external dsound;
function DirectSoundCaptureEnumerateW; external dsound;
function DirectSoundCaptureEnumerate; external dsound name 'DirectSoundCaptureEnumerateA';

end.


