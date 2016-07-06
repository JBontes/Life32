unit DDSurface;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Windows, DDraw, SysUtils, Controls, Forms, Graphics, Classes, Ole2,
  LifeConst, DDUtils;

type
  TVidMem = record
    case integer of
      0: (B: Byte);
      1: (C: array [0..3] of byte);
      2: (I: integer);
      3: (W: Word);
      4: (S: Smallint);
  end;
  PVidMem = ^TVidMem;

type
  TDisplayCelPart = procedure(x1,y1: integer; CelPart,AColor: integer) of object;
  TDisplayBlackPart = procedure(x1, y1: integer; AColor: integer) of object;

type
  EDDrawError = class(Exception)
  end;

type
  TMyDirectDrawCreate = function (Guid: PGuid; var DirectDraw: IDirectDraw;
                          UnkOuter: IUnknown):HResult; stdcall;


type
  TMyCustomControl = class(TCustomControl)
  public
    property Canvas;
  end;

  TMyDDSurface = class(TObject)
  private
    FOwner: TWinControl;
    FDirectDraw: IDirectDraw;
    FDDSurface: IDirectDrawSurface;
    FVidMem: PVidMem;
    FVidWidth, FVidHeight: integer;
    FPitch: integer;
    FBytesPerPixel: integer;
    FBounds: TRect;
    FDisplayCelPart: TDisplayCelPart;
    FDisplayBlackPart: TDisplayBlackPart;
    FDisplayCelPartXor: TDisplayCelPart;
    FCanvas: TCanvas;
    CelSize: Integer;
    CelSpace: Integer;
    CelSpace2: Integer;
    CelSpace3: integer;
    CelSpace4: Integer;
    CelSpace8: Integer;
    CelSpace12: Integer;
    CelSpace16: Integer;
    GridEnabled: boolean;
    FDirectDrawEnabled: TDDStates;
    FInstalled: boolean;
    FLocked: Boolean;

    procedure DisplayCelPartXorDDraw(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPartXorDDraw_24(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPartXorDC(x1,y1,CelPart,AColor: integer);

    {$ifdef win32}
    procedure DisplayCelPart1x1(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart1x1_16(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart1x1_24(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart1x1_32(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart2x2(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart2x2_16(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart2x2_24(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart2x2_32(x1,y1,CelPart,AColor: integer);
    {$endif}
    procedure DisplayCelPart2plus(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPart2plus_24(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPartGrid(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPartGrid_24(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPartWithDC(x1,y1,CelPart,AColor: integer);
    procedure DisplayCelPartGridWithDC(x1,y1,CelPart,AColor: integer);

    {$ifdef win32}
    procedure DisplayBlackPart1x1(x1,y1,AColor: integer);
    procedure DisplayBlackPart1x1_16(x1,y1,AColor: integer);
    procedure DisplayBlackPart1x1_24(x1,y1,AColor: integer);
    procedure DisplayBlackPart1x1_32(x1,y1,AColor: integer);
    procedure DisplayBlackPart2x2(x1,y1,AColor: integer);
    procedure DisplayBlackPart2x2_16(x1,y1,AColor: integer);
    procedure DisplayBlackPart2x2_24(x1,y1,AColor: integer);
    procedure DisplayBlackPart2x2_32(x1,y1,AColor: integer);
    {$endif}
    procedure DisplayBlackPart2plus(x1,y1,AColor: integer);
    procedure DisplayBlackPart2plus_24(x1,y1,AColor: integer);
    procedure DisplayBlackPartGrid(x1,y1,Acolor: integer);
    procedure DisplayBlackPartGrid_24(x1,y1,Acolor: integer);
    procedure DisplayBlackPartWithDC(x1,y1,AColor: integer);
    procedure DisplayBlackPartGridWithDC(x1,y1,AColor: integer);
  protected
    procedure SetBounds(Value: TRect);
    procedure SetDirectDrawEnabled(Value: TDDStates);
    procedure InitDirectDraw;
    procedure InitWithOutDDraw;
    function GetCanvas: TCanvas;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    function GetWinNearestColor(AColor: TColor): TColor;
    function GetDDNearestColor(AColor: TColor): integer;
    procedure SetDDPixel(X,Y: Integer; AColor: integer);
    {$ifdef win32}
    procedure XorBlt(x1,y1,AWidth, AHeight, AColor: integer);
    {$endif}
    procedure XorBlt24(x1,y1,AWidth, AHeight, AColor: integer);
    {$ifdef win32}
    procedure PatBlt(x1,y1,AWidth, AHeight, AColor: integer);
    {$endif}
    procedure PatBlt24(x1,y1,AWidth, AHeight, AColor: integer);

    procedure FillRect(ARect: TRect; AColor: integer; Wait: boolean);
    //procedure FillRectXor(ARect: TRect; AColor: integer);
    procedure FillWindow(AWindow: TControl; ARect: TRect; AColor: integer; Wait: boolean);
    procedure InitDrawRoutines(ACelSpace: integer; GridOn: boolean);
    procedure SuspendDirectDraw(Suspend: boolean);
    procedure DisplayChange(BitsPerPixel,Width,Height: integer);
    function Lock(ACanvas: TCanvas): boolean;
    procedure UnLock;

    property DirectDrawEnabled: TDDStates read FDirectDrawEnabled write SetDirectDrawEnabled;
    property Installed: boolean read FInstalled;
    property Bounds: TRect read FBounds write SetBounds;
    property Pixel[X: integer; Y: integer]: integer write SetDDPixel;
    property Canvas: TCanvas read GetCanvas;
    property Locked: Boolean read FLocked;
    property Pitch: integer read FPitch;
    property DisplayCelPart: TDisplayCelPart read FDisplayCelPart;
    property DisplayBlackPart: TDisplayBlackPart read FDisplayBlackPart;
    property DisplayCelPartXor: TDisplayCelPart read FDisplayCelPartXor;
    property BytesPerPixel: integer read FBytesPerPixel;
  end;

implementation

var
  PitchLookup: array [0..2048] of integer;

var
  DDrawLoaded: boolean;
  HDDrawLib: THandle;

constructor TMyDDSurface.Create(AOwner: TWinControl);
var
  i: integer;
  Filename: string;
  FileHandle: THandle;
  FileDate: TDateTime;
begin
  inherited Create;
  FOwner:= AOwner;
  with FBounds do begin
    Left:= 0; Top:= 0;
    Right:= Screen.Width;
    Bottom:= Screen.Height;
  end; {with}
  FVidWidth:= FBounds.Right;
  FVidHeight:= FBounds.Bottom;

  HDDrawLib:= 0;
  //are we in designing or running state.
  if (Application.Tag = asRunning) then begin
    HDDrawLib:= LoadLibrary('DDraw.dll');
  end;
  DDrawLoaded:= (HDDrawLib >= 32) or (HDDrawLib > $8000000);
  if DDrawLoaded then begin
    SetLength(Filename,1024);
    GetModuleFilename(HDDrawLib,PChar(Filename),1023);
    FileHandle:= CreateFile(PChar(FileName),GENERIC_READ,FILE_SHARE_READ,nil,
                            OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
    try
      FileDate:= FileDateToDateTime(FileGetDate(FileHandle));
      finally CloseHandle(FileHandle);
    end; {try}
    DDrawLoaded:= (FileDate > EncodeDate(1997,3,30));
    if not DDrawLoaded then FreeLibrary(HDDrawLib);
  end;

  DirectDrawEnabled:= DDFast;
  FInstalled:= (DirectDrawEnabled and DDFast) = DDFast;


  for i:= Low(PitchLookup) to High(PitchLookup) do begin
    PitchLookup[i]:= FPitch * i;
  end; {for i}

end;

destructor TMyDDSurface.Destroy;
begin
  if DDrawLoaded and ((DirectDrawEnabled and DDFast) = DDFast) then begin
    DirectDrawEnabled:= 0;
    //FDDSurface.Free;
    //FDirectDraw.Free;
  end;
  if DDrawLoaded and not(csDesigning in Application.ComponentState) then begin
    DDrawLoaded:= false;
    FreeLibrary(HDDrawLib);
  end;
  inherited Destroy;
end;


procedure TMyDDSurface.SetDirectDrawEnabled(Value: TDDStates);
var
  OldDDE: TDDStates;
begin
  if (Value <> FDirectDrawEnabled) then begin
    if not(DDrawLoaded) then Value:= 0;
    OldDDE:= FDirectDrawEnabled;
    FDirectDrawEnabled:= Value;
    if (((Value = DDFast) and not ((OldDDE and DDTempDisabled) = DDTempDisabled))
       and DDrawLoaded) then try
      //if we were just suspended, do not reload DirectDraw, else go ahead.
      InitDirectDraw;
    except begin
      FDirectDrawEnabled:= (FDirectDrawEnabled and not DDFast);
      InitWithoutDDraw;
    end; {except}
    end; {if try}
    if ((Value and DDTempDisabled)=DDTempDisabled)
       or ((OldDDE and DDTempDisabled)=DDTempDisabled) then begin
      if Locked then Unlock;
      //Switch between DD-enabled draw routines and GDI-based.
      SuspendDirectDraw((DDTempDisabled and Value)=DDTempDisabled);
    end
    else if (Value <> DDFast) then begin //unload DirectDraw.
      if not(DDrawLoaded) then FDirectDrawEnabled:= 0;
      if Locked then Unlock;
      InitWithOutDDraw;
    end; {else}
  end; {if}
end;

procedure TMyDDSurface.InitDirectDraw;
var
  ddsd: TDDSurfaceDesc;
  i: integer;
  MyDirectDrawCreate: TMyDirectDrawCreate;
begin
  @MyDirectDrawCreate:= GetProcAddress(HDDrawLib,'DirectDrawCreate');
  if @MyDirectDrawCreate = nil then
    raise EDDrawError.Create('Cannot load DirectDrawCreate function');
  if MyDirectDrawCreate(nil,FDirectDraw,nil) <> DD_OK then
    raise EDDrawError.Create('Cannot create DirectDraw object');
  //if FDirectDraw.QueryInterface(IID_IDirectDraw2,DD2) <> S_OK then
  //  raise EDDrawError.Create('Not the correct DirectDraw version');
  FDirectDraw.SetCooperativeLevel(0, DDSCL_NORMAL);
  with ddsd, FDirectDraw do begin
    FillChar(ddsd,SizeOf(ddsd),0);
    dwSize:= SizeOf(ddsd);
    dwFlags:= DDSD_CAPS;
    ddsCaps.dwCaps:= DDSCAPS_PRIMARYSURFACE;
    if CreateSurface(ddsd,FDDSurface,nil) <> DD_OK then
      raise EDDrawError.Create('Cannot make primary DDSurface');
  end; {with}
  Lock(nil);
  Unlock; //Lock does some init work to.
  FBytesPerPixel:= FPitch div FVidWidth;
  if PitchLookup[1] <> FPitch then
    for i:= 0 to High(PitchLookup) do begin
      PitchLookup[i]:= FPitch * i;
    end; {for i}
  InitDrawRoutines(CelSpace,GridEnabled);
end;

procedure TMyDDSurface.InitWithOutDDraw;
begin
  if Assigned(FDDSurface) then FDDSurface.Release;
  FDDSurface:= nil;
  if Assigned(FDirectDraw) then FDirectDraw.Release;
  FDirectDraw:= nil;
  InitDrawRoutines(CelSpace,GridEnabled);
end;


function TMyDDSurface.Lock(ACanvas: TCanvas): boolean;
const
  NoLock = false;
  LockOK = true;
var
  ddsd: TDDSurfaceDesc;
  LockResult: HResult;
begin
  Result:= LockOk;
  if not(Locked) then begin
    if (DirectDrawEnabled = DDFast) then begin
      FillChar(ddsd,SizeOf(ddsd),0);
      ddsd.dwSize:= SizeOf(ddsd);
      try
        LockResult:= HResult(DDERR_SURFACEBUSY);
        while LockResult = HResult(DDERR_SURFACEBUSY) do begin
          LockResult:= FDDSurface.Lock(nil, @ddsd, DDLOCK_WAIT, 0);
        end;
        if LockResult = DD_OK then begin
          FVidMem:= ddsd.lpSurface;
          FPitch:= ddsd.lPitch;
        end
        else begin
          FDDSurface.Unlock(ddsd.lpSurface);
          if Assigned(ACanvas) then FCanvas:= ACanvas;
          Result:= NoLock;
        end;
        except begin
          FDDSurface.Unlock(ddsd.lpSurface);
        end; {except}
      end; {try}
    end {true:}
    //DDraw is disabled.
    else begin
      if Assigned(ACanvas) then FCanvas:= ACanvas;
      Result:= lockOK;
    end; {else}
  end; {if}
  FLocked:= Result;
end;

procedure TMyDDSurface.UnLock;
begin
  FLocked:= false;
  if (DirectDrawEnabled = DDFast) then begin
    FDDSurface.Unlock(FVidMem);
    FVidMem:= nil;
  end; {if}
end;

function TMyDDSurface.GetCanvas: TCanvas;
begin
  Result:= FCanvas;
end;

function TMyDDSurface.GetWinNearestColor(AColor: TColor): TColor;
var
  OldColor: TColor;
  HDesktop: THandle;
  HDesktopDC: HDC;
begin
  HDeskTop:= GetDesktopWindow;
  HDeskTopDC:= GetWindowDC(HDesktop);
  OldColor:= GetPixel(HDeskTopDC,0,0);
  try
    SetPixelV(HDesktopDC,0,0,AColor);
    Result:= GetPixel(HDesktopDC,0,0);
    SetPixelV(HDesktopDC,0,0,OldColor);
    finally ReleaseDC(HDesktop,HDesktopDC);
  end; {try}
end;

function TMyDDSurface.GetDDNearestColor(AColor: TColor): integer;
//begin
  //Result:= DDColorMatch(FDDSurface,AColor);
var
  Buffer: PVidMem;
  BytesInBitmap: integer;
  //WasLocked: boolean;
  HDesktop: THandle;
  HDesktopDC: HDC;
  HCompDC: HDC;
  HBitmap: THandle;
  HOldBitmap: THandle;
begin
  Result:= AColor;
  //WasLocked:= Locked;
  if (DirectDrawEnabled = DDFast) then begin
    //if Waslocked then Unlock;
    if AColor = 0 then Result:= 0
    else if AColor = $00ffffff then Result:= -1
    else begin
      HDesktop:= GetDesktopWindow;
      HDesktopDC:= GetWindowDC(HDesktop);
      try
        HCompDC:= CreateCompatibleDC(HDeskTopDC);
        HBitmap:= CreateCompatibleBitmap(HDeskTopDC,1,1);
        finally ReleaseDC(HDeskTop,HDesktopDC);
      end; {try}
      HOldBitmap:= SelectObject(HCompDC,HBitmap);
      SetPixelV(HCompDC,0,0,AColor);
      GetMem(Buffer,10);
      BytesInBitmap:= GetBitmapBits(HBitmap,FBytesPerPixel,Buffer);
      SelectObject(HCompDC,HOldBitmap);
      DeleteObject(HBitmap);
      DeleteDC(HCompDC);
      case BytesInBitmap of
        1:Result:= Buffer^.B;
        2:Result:= Buffer^.S;
        //3:Result:= WritePos^.I and $00ffffff;
        3:Result:= Buffer^.I and $00ffffff;
        //4:Result:= WritePos^.I;
        4:Result:= Buffer^.I;
      end; {case}
      FreeMem(Buffer,10);
      case FBytesPerPixel of
        1: begin
          Result:= Result and $ff;
          Result:= Result or (Result * 256) or (Result * (256*256)) or
                   (Result * (256*256*256));
        end; {1:}
        2: begin
          Result:= Result and $FFFF;
          Result:= Result or (Result * (256*256));
        end; {2:}
      end; {case}
    end;
    //if WasLocked then Lock(nil);
  end; {if} (**)
end;

procedure TMyDDSurface.SetDDPixel(X,Y: integer; AColor: integer);
type
  PByte = ^Byte;
var
  WritePos: PVidmem;
  WasLocked: Boolean;
begin
  WasLocked:= Locked;
  if Lock(nil) then try //does nothing if already 'locked'
    x:= x * FBytesPerPixel;
    NativeInt(WritePos):= NativeInt(FVidMem) + x + PitchLookup[y];
    case FBytesPerPixel of
      1:WritePos^.B:= AColor;
      2:WritePos^.S:= AColor;
      3:begin
        WritePos^.S:= AColor;
        WritePos^.C[2]:= ((AColor shr 16))
      end;
      4:WritePos^.I:= AColor;
    end; {case}
    finally if not WasLocked then UnLock; //restore to previous condition
  end; {try}
end;

{$ifdef win32}
procedure TMyDDSurface.DisplayBlackPart1x1(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  add ecx,edx                             //v
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  mov [ecx],edx                           //u
  mov [ecx+ebx],edx                       //v
  mov [ecx+ebx*2],edx                     //u
  add ecx,ebx                             //v
  mov [ecx+ebx*2],edx                     //u
  pop ebx                                 //v
end; {asm}


procedure TMyDDSurface.DisplayBlackPart1x1_16(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  lea ecx,[ecx+edx*2]                     //v
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  mov [ecx],edx                           //u
  mov [ecx+4],edx                         //v
  mov [ecx+ebx],edx                       //u
  mov [ecx+ebx+4],edx                     //v
  mov [ecx+ebx*2],edx                     //u
  mov [ecx+ebx*2+4],edx                   //v
  add ecx,ebx                             //v
  mov [ecx+ebx*2],edx                     //u
  mov [ecx+ebx*2+4],edx                   //v
  pop ebx                                 //u
end; {asm}

procedure TMyDDSurface.DisplayBlackPart1x1_24(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  lea ecx,[ecx+edx*2]                     //v
  lea ecx,[ecx+edx]
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  mov al,dl
  shr edx,8
  mov [ecx],al                           //u
  mov [ecx+3],al                         //v
  mov [ecx+6],al                         //u
  mov [ecx+9],al                        //v
  mov [ecx+ebx],al                       //u
  mov [ecx+ebx+3],al                     //v
  mov [ecx+ebx+6],al                     //u
  mov [ecx+ebx+9],al                    //v
  mov [ecx+ebx*2],al                     //u
  mov [ecx+ebx*2+3],al                   //v
  mov [ecx+ebx*2+6],al                   //u
  mov [ecx+ebx*2+9],al                  //v

  mov [ecx+1],dx                           //u
  mov [ecx+3+1],dx                         //v
  mov [ecx+6+1],dx                         //u
  mov [ecx+9+1],dx                        //v
  mov [ecx+ebx+1],dx                       //u
  mov [ecx+ebx+3+1],dx                     //v
  mov [ecx+ebx+6+1],dx                     //u
  mov [ecx+ebx+9+1],dx                    //v
  mov [ecx+ebx*2+1],dx                     //u
  mov [ecx+ebx*2+3+1],dx                   //v
  mov [ecx+ebx*2+6+1],dx                   //u
  mov [ecx+ebx*2+9+1],dx                  //v
  add ecx,ebx                             //v
  mov [ecx+ebx*2],al                     //u
  mov [ecx+ebx*2+3],al                   //v
  mov [ecx+ebx*2+6],al                   //u
  mov [ecx+ebx*2+9],al                  //v

  mov [ecx+ebx*2+1],dx                     //u
  mov [ecx+ebx*2+3+1],dx                   //v
  mov [ecx+ebx*2+6+1],dx                   //u
  mov [ecx+ebx*2+9+1],dx                  //v
  pop ebx                                 //u
end; {asm}


procedure TMyDDSurface.DisplayBlackPart1x1_32(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  lea ecx,[ecx+edx*4]                     //v
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  mov [ecx],edx                           //u
  mov [ecx+4],edx                         //v
  mov [ecx+8],edx                         //u
  mov [ecx+12],edx                        //v
  mov [ecx+ebx],edx                       //u
  mov [ecx+ebx+4],edx                     //v
  mov [ecx+ebx+8],edx                     //u
  mov [ecx+ebx+12],edx                    //v
  mov [ecx+ebx*2],edx                     //u
  mov [ecx+ebx*2+4],edx                   //v
  mov [ecx+ebx*2+8],edx                   //u
  mov [ecx+ebx*2+12],edx                  //v
  add ecx,ebx                             //v
  mov [ecx+ebx*2],edx                     //u
  mov [ecx+ebx*2+4],edx                   //v
  mov [ecx+ebx*2+8],edx                   //u
  mov [ecx+ebx*2+12],edx                  //v
  pop ebx                                 //u
end; {asm}


procedure TMyDDSurface.DisplayBlackPart2x2(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  add ecx,edx                             //v
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  push esi

  mov [ecx],edx
  mov [ecx+4],edx
  mov [ecx+ebx],edx
  mov [ecx+ebx+4],edx

  lea eax,[ebx+ebx*2] // eax:= FPitch *3
  mov [ecx+ebx*2],edx
  mov [ecx+ebx*2+4],edx
  mov [ecx+eax],edx
  mov [ecx+eax+4],edx

  lea eax,[eax+ebx*2] //eax:= FPitch * 5
  lea esi,[ecx+ebx]
  mov [ecx+ebx*4],edx
  mov [ecx+ebx*4+4],edx
  mov [ecx+eax],edx
  mov [ecx+eax+4],edx
  lea esi,[esi+eax]

  mov [esi],edx
  mov [esi+4],edx
  mov [esi+ebx],edx
  mov [esi+ebx+4],edx

  pop esi
  pop ebx
end; {asm}

procedure TMyDDSurface.DisplayBlackPart2x2_16(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  lea ecx,[ecx+edx*2]                     //v
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  push esi

  mov [ecx],edx
  mov [ecx+4],edx
  mov [ecx+8],edx
  mov [ecx+12],edx
  mov [ecx+ebx],edx
  mov [ecx+ebx+4],edx
  mov [ecx+ebx+8],edx
  mov [ecx+ebx+12],edx

  lea eax,[ebx+ebx*2] // eax:= FPitch *3
  mov [ecx+ebx*2],edx
  mov [ecx+ebx*2+4],edx
  mov [ecx+ebx*2+8],edx
  mov [ecx+ebx*2+12],edx
  mov [ecx+eax],edx
  mov [ecx+eax+4],edx
  mov [ecx+eax+8],edx
  mov [ecx+eax+12],edx

  lea eax,[eax+ebx*2] //eax:= FPitch * 5
  lea esi,[ecx+ebx]
  mov [ecx+ebx*4],edx
  mov [ecx+ebx*4+4],edx
  mov [ecx+ebx*4+8],edx
  mov [ecx+ebx*4+12],edx
  mov [ecx+eax],edx
  mov [ecx+eax+4],edx
  mov [ecx+eax+8],edx
  mov [ecx+eax+12],edx
  lea esi,[esi+eax]

  mov [esi],edx
  mov [esi+4],edx
  mov [esi+8],edx
  mov [esi+12],edx
  mov [esi+ebx],edx
  mov [esi+ebx+4],edx
  mov [esi+ebx+8],edx
  mov [esi+ebx+12],edx

  pop esi
  pop ebx
end; {asm}


procedure TMyDDSurface.DisplayBlackPart2x2_24(x1,y1,AColor: integer);
begin
  DisplayCelPart2x2_24(x1,y1,$ffff,AColor);
  (*asm                                            //eax = self.
    push ebx   //u
    //ecx:= y1 * FPitch
    mov ecx,dword ptr [4*ecx+PitchLookup]   //v  //ecx:= pitchlookup[y1]
    //ebx:= Self.FPitch
    mov ebx,[eax+TMyDDSurface.FPitch]       //u  //ebx:= FPitch.
    //esi:= (y1*FPitch) + x1
    lea ecx,[ecx+edx*2]                     //v
    lea ecx,[ecx+edx]
    mov edx,[AColor]                        //u
    //esi:= esi + Self.FVidMem
    add ecx,[eax+TMyDDSurface.FVidMem]      //v
    push esi
    push edi
    mov  al,dl
    shr  edx,8

    mov [ecx],al               //Draw line 1, byte 1
    mov [ecx+3],al
    mov [ecx+6],al
    mov [ecx+9],al
    mov [ecx+12],al
    mov [ecx+15],al
    mov [ecx+18],al
    mov [ecx+21],al
    mov [ecx+ebx],al           //Draw Line 2, byte 1
    mov [ecx+ebx+3],al
    mov [ecx+ebx+6],al
    mov [ecx+ebx+9],al
    mov [ecx+ebx+12],al
    mov [ecx+ebx+15],al
    mov [ecx+ebx+18],al
    mov [ecx+ebx+21],al

    mov [ecx+1],dx             //Draw line 1, byte 2 and 3
    mov [ecx+3+1],dx
    mov [ecx+6+1],dx
    mov [ecx+9+1],dx
    mov [ecx+12+1],dx
    mov [ecx+15+1],dx
    mov [ecx+18+1],dx
    mov [ecx+21+1],dx
    mov [ecx+ebx+1],dx         //Draw Line 2, byte 2 and 3
    mov [ecx+ebx+3+1],dx
    mov [ecx+ebx+6+1],dx
    mov [ecx+ebx+9+1],dx
    mov [ecx+ebx+12+1],dx
    mov [ecx+ebx+15+1],dx
    mov [ecx+ebx+18+1],dx
    mov [ecx+ebx+21+1],dx

    lea edi,[ebx+ebx*2] // eax:= FPitch *3
    mov [ecx+ebx*2],al         //Draw Line 3, byte 1
    mov [ecx+ebx*2+3],al
    mov [ecx+ebx*2+6],al
    mov [ecx+ebx*2+9],al
    mov [ecx+ebx*2+12],al
    mov [ecx+ebx*2+15],al
    mov [ecx+ebx*2+18],al
    mov [ecx+ebx*2+21],al
    mov [ecx+edi],al           //Draw Line 4, byte 1
    mov [ecx+edi+3],al
    mov [ecx+edi+6],al
    mov [ecx+edi+9],al
    mov [ecx+edi+12],al
    mov [ecx+edi+15],al
    mov [ecx+edi+18],al
    mov [ecx+edi+21],al

    mov [ecx+ebx*2+1],dx       //Draw Line 3, byte 2 and 3
    mov [ecx+ebx*2+3+1],dx
    mov [ecx+ebx*2+6+1],dx
    mov [ecx+ebx*2+9+1],dx
    mov [ecx+ebx*2+12+1],dx
    mov [ecx+ebx*2+15+1],dx
    mov [ecx+ebx*2+18+1],dx
    mov [ecx+ebx*2+21+1],dx
    mov [ecx+edi+1],dx         //Draw Line 4, byte 2 and 3
    mov [ecx+edi+3+1],dx
    mov [ecx+edi+6+1],dx
    mov [ecx+edi+9+1],dx
    mov [ecx+edi+12+1],dx
    mov [ecx+edi+15+1],dx
    mov [ecx+edi+18+1],dx
    mov [ecx+edi+21+1],dx

    lea edi,[edi+ebx*2] //edi:= FPitch * 5
    lea esi,[edi+ebx]   //esi:= FPitch * 6
    mov [ecx+ebx*4],al         //Draw Line 5, byte 1
    mov [ecx+ebx*4+3],al
    mov [ecx+ebx*4+6],al
    mov [ecx+ebx*4+9],al
    mov [ecx+ebx*4+12],al
    mov [ecx+ebx*4+15],al
    mov [ecx+ebx*4+18],al
    mov [ecx+ebx*4+21],al
    mov [ecx+edi],al           //Draw Line 6, byte 1
    mov [ecx+edi+3],al
    mov [ecx+edi+6],al
    mov [ecx+edi+9],al
    mov [ecx+edi+12],al
    mov [ecx+edi+15],al
    mov [ecx+edi+18],al
    mov [ecx+edi+21],al

    mov [ecx+ebx*4+1],dx       //Draw Line 5, byte 2 and 3
    mov [ecx+ebx*4+3+1],dx
    mov [ecx+ebx*4+6+1],dx
    mov [ecx+ebx*4+9+1],dx
    mov [ecx+ebx*4+12+1],dx
    mov [ecx+ebx*4+15+1],dx
    mov [ecx+ebx*4+18+1],dx
    mov [ecx+ebx*4+21+1],dx
    mov [ecx+edi+1],dx         //Draw Line 6, byte 2 and 3
    mov [ecx+edi+3+1],dx
    mov [ecx+edi+6+1],dx
    mov [ecx+edi+9+1],dx
    mov [ecx+edi+12+1],dx
    mov [ecx+edi+15+1],dx
    mov [ecx+edi+18+1],dx
    mov [ecx+edi+21+1],dx
    lea esi,[esi+edi]

    mov [esi],al               //Draw Line 7, byte 1
    mov [esi+3],al
    mov [esi+6],al
    mov [esi+9],al
    mov [esi+12],al
    mov [esi+15],al
    mov [esi+18],al
    mov [esi+21],al
    mov [esi+ebx],al           //Draw Line 8, byte 1
    mov [esi+ebx+3],al
    mov [esi+ebx+6],al
    mov [esi+ebx+9],al
    mov [esi+ebx+12],al
    mov [esi+ebx+15],al
    mov [esi+ebx+18],al
    mov [esi+ebx+21],al

    mov [esi+1],dx             //Draw Line 7, byte 2 and 3
    mov [esi+3+1],dx
    mov [esi+6+1],dx
    mov [esi+9+1],dx
    mov [esi+12+1],dx
    mov [esi+15+1],dx
    mov [esi+18+1],dx
    mov [esi+21+1],dx
    mov [esi+ebx+1],dx         //Draw Line 8, byte 2 and 3
    mov [esi+ebx+3+1],dx
    mov [esi+ebx+6+1],dx
    mov [esi+ebx+9+1],dx
    mov [esi+ebx+12+1],dx
    mov [esi+ebx+15+1],dx
    mov [esi+ebx+18+1],dx
    mov [esi+ebx+21+1],dx

    pop edi
    pop esi
    pop ebx
  end; {asm} (**)
end;


procedure TMyDDSurface.DisplayBlackPart2x2_32(x1,y1,AColor: integer);
asm
  push ebx   //u
  //ecx:= y1 * FPitch
  mov ecx,dword ptr [4*ecx+PitchLookup]   //v
  //ebx:= Self.FPitch
  mov ebx,[eax+TMyDDSurface.FPitch]       //u
  //esi:= (y1*FPitch) + x1
  lea ecx,[ecx+edx*4]                     //v
  mov edx,[AColor]                        //u
  //esi:= esi + Self.FVidMem
  add ecx,[eax+TMyDDSurface.FVidMem]      //v
  push esi

  mov [ecx],edx                //Line 1
  mov [ecx+4],edx
  mov [ecx+8],edx
  mov [ecx+12],edx
  mov [ecx+16],edx
  mov [ecx+20],edx
  mov [ecx+24],edx
  mov [ecx+28],edx
  mov [ecx+ebx],edx            //Line 2
  mov [ecx+ebx+4],edx
  mov [ecx+ebx+8],edx
  mov [ecx+ebx+12],edx
  mov [ecx+ebx+16],edx
  mov [ecx+ebx+20],edx
  mov [ecx+ebx+24],edx
  mov [ecx+ebx+28],edx

  lea eax,[ebx+ebx*2] // eax:= FPitch *3
  mov [ecx+ebx*2],edx          //Line 3
  mov [ecx+ebx*2+4],edx
  mov [ecx+ebx*2+8],edx
  mov [ecx+ebx*2+12],edx
  mov [ecx+ebx*2+16],edx
  mov [ecx+ebx*2+20],edx
  mov [ecx+ebx*2+24],edx
  mov [ecx+ebx*2+28],edx
  mov [ecx+eax],edx            //Line 4
  mov [ecx+eax+4],edx
  mov [ecx+eax+8],edx
  mov [ecx+eax+12],edx
  mov [ecx+eax+16],edx
  mov [ecx+eax+20],edx
  mov [ecx+eax+24],edx
  mov [ecx+eax+28],edx

  lea eax,[eax+ebx*2] //eax:= FPitch * 5
  lea esi,[ecx+ebx]
  mov [ecx+ebx*4],edx          //Line 5
  mov [ecx+ebx*4+4],edx
  mov [ecx+ebx*4+8],edx
  mov [ecx+ebx*4+12],edx
  mov [ecx+ebx*4+16],edx
  mov [ecx+ebx*4+20],edx
  mov [ecx+ebx*4+24],edx
  mov [ecx+ebx*4+28],edx
  mov [ecx+eax],edx            //Line 6
  mov [ecx+eax+4],edx
  mov [ecx+eax+8],edx
  mov [ecx+eax+12],edx
  mov [ecx+eax+16],edx
  mov [ecx+eax+20],edx
  mov [ecx+eax+24],edx
  mov [ecx+eax+28],edx
  lea esi,[esi+eax]

  mov [esi],edx                //Line 2 + 6 + 1 = 7
  mov [esi+4],edx
  mov [esi+8],edx
  mov [esi+12],edx
  mov [esi+16],edx
  mov [esi+20],edx
  mov [esi+24],edx
  mov [esi+28],edx
  mov [esi+ebx],edx            //Line 8
  mov [esi+ebx+4],edx
  mov [esi+ebx+8],edx
  mov [esi+ebx+12],edx
  mov [esi+ebx+16],edx
  mov [esi+ebx+20],edx
  mov [esi+ebx+24],edx
  mov [esi+ebx+28],edx

  pop esi
  pop ebx
end; {asm}
{$endif}

procedure TMyDDSurface.DisplayBlackPart2plus(x1,y1,Acolor: integer);
begin
  DisplayCelPart2Plus(x1,y1,$ffff,AColor);
end;

procedure TMyDDSurface.DisplayBlackPart2plus_24(x1,y1,Acolor: integer);
begin
  DisplayCelPart2Plus_24(x1,y1,$ffff,AColor);
end;

procedure TMyDDSurface.DisplayBlackPartGrid(x1,y1,Acolor: integer);
begin
  DisplayCelPartGrid(x1,y1,$ffff,AColor);
end;

procedure TMyDDSurface.DisplayBlackPartGrid_24(x1,y1,Acolor: integer);
begin
  DisplayCelPartGrid_24(x1,y1,$ffff,AColor);
end;

{$ifdef win32}
procedure TMyDDSurface.DisplayCelPart1x1(x1,y1: integer; CelPart,AColor: integer);
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  AfterCase, Loopi;
  //for comments see DisplayCelPart2x2 which is conceptually equal.
asm
    push ebx
    push edi
    push esi

    mov esi,dword ptr [4*ecx+PitchLookup]
    mov ebx,[eax+TMyDDSurface.FPitch]
    add esi,edx
    mov ecx,[AColor]
    add esi,[eax+TMyDDSurface.FVidMem]
  //top line.


    mov eax,[CelPart]
    mov edx,4
    mov edi,eax
Loopi:
    //case (CelPart and $c0c0) of   //fe------76------

      and edi,$c0c0
      jz Aftercase
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      sub edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      sub edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      sub edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      sub edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0


S0040:
      mov [esi+1*3],cl
      jmp aftercase

S0080:
      mov [esi+1*2],cl
      jmp aftercase

S00c0:
      mov [esi+1*2],cx
      jmp aftercase

S4000:
      mov [esi+1*1],cl
      jmp aftercase

S4040:
      mov [esi+1*1],cl
      mov [esi+1*3],cl
      jmp aftercase

S4080:
      mov [esi+1*1],cx
      jmp aftercase

S40c0:
      mov [esi+1*1],cx
      mov [esi+1*3],cl
      jmp aftercase

S8000:
      mov [esi],cl
      jmp aftercase

S8040:
      mov [esi],cl
      mov [esi+1*3],cl
      jmp aftercase

S8080:
      mov [esi],cl
      mov [esi+1*2],cl
      jmp aftercase

S80c0:
      mov [esi],cl
      mov [esi+1*2],cx
      jmp aftercase

Sc000:
      mov [esi],cx
      jmp aftercase

Sc040:
      mov [esi],cx
      mov [esi+1*3],cl
      jmp aftercase

Sc080:
      mov [esi],cx
      mov [esi+1*2],cl
      jmp aftercase

Sc0c0:
      mov [esi],ecx

AfterCase:
    lea esi,[esi+ebx]
    lea eax,[eax*4] //shl can't run in v-pipe.

    dec edx
    //or eax,eax
    mov edi,eax
    jnz Loopi
    pop esi
    pop edi
    pop ebx
end;


//16 bit pixels.
procedure TMyDDSurface.DisplayCelPart1x1_16(x1,y1: integer; CelPart,AColor: integer);
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  AfterCase, Loopi;
  //for comments see DisplayCelPart2x2 which is conceptually equal.
asm
    push ebx
    push edi
    push esi

    mov esi,dword ptr PitchLookup[4*ecx]   //esi:= y1 * FPitch;
    mov ebx,[eax+TMyDDSurface.FPitch]      //ebx:= Self.FPitch
    lea esi,[esi+edx*2]                    //esi:= (y1*FPitch) + x1
    mov ecx,[AColor]
    add esi,[eax+TMyDDSurface.FVidMem]     //add startaddres.
  //top line.


    mov eax,[CelPart]
    mov edx,4
    mov edi,eax
Loopi:
    //case (CelPart and $c0c0) of   //fe------76------

      and edi,$c0c0
      jz Aftercase
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      sub edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      sub edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      sub edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      sub edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0


S0040:
      mov [esi+1*3*2],cx
      jmp aftercase

S0080:
      mov [esi+1*2*2],cx
      jmp aftercase

S00c0:
      mov [esi+1*2*2],ecx
      jmp aftercase

S4000:
      mov [esi+1*1*2],cx
      jmp aftercase

S4040:
      mov [esi+1*1*2],cx
      mov [esi+1*3*2],cx
      jmp aftercase

S4080:
      mov [esi+1*1*2],ecx
      jmp aftercase

S40c0:
      mov [esi+1*1*2],ecx
      mov [esi+1*3*2],cx
      jmp aftercase

S8000:
      mov [esi],cx
      jmp aftercase

S8040:
      mov [esi],cx
      mov [esi+1*3*2],cx
      jmp aftercase

S8080:
      mov [esi],cx
      mov [esi+1*2*2],cx
      jmp aftercase

S80c0:
      mov [esi],cx
      mov [esi+1*2*2],ecx
      jmp aftercase

Sc000:
      mov [esi],ecx
      jmp aftercase

Sc040:
      mov [esi],ecx
      mov [esi+1*3*2],cx
      jmp aftercase

Sc080:
      mov [esi],ecx
      mov [esi+1*2*2],cx
      jmp aftercase

Sc0c0:
      mov [esi],ecx
      mov [esi+1*2*2],ecx

AfterCase:
    lea esi,[esi+ebx]
    lea eax,[eax*4] //shl can't run in v-pipe.

    dec edx
    //or eax,eax
    mov edi,eax
    jnz Loopi
    pop esi
    pop edi
    pop ebx
end;

//24 nit pixels
procedure TMyDDSurface.DisplayCelPart1x1_24(x1,y1: integer; CelPart,AColor: integer);
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  AfterCase, Loopi;
  //for comments see DisplayCelPart2x2 which is conceptually equal.
asm
    push ebx
    push edi
    push esi
    push ebp

    mov esi,dword ptr PitchLookup[4*ecx]   //esi:= y1 * FPitch;
    mov ebx,[eax+TMyDDSurface.FPitch]      //ebx:= Self.FPitch
    lea esi,[esi+edx*2]                    //esi:= (y1*FPitch) + x1
    lea esi,[esi+edx]
    mov ecx,[AColor]
    add esi,[eax+TMyDDSurface.FVidMem]     //add startaddres.
  //top line.


    mov eax,[CelPart]
    mov edx,4
    mov edi,eax
    mov ebp,ebx
    mov ebx,ecx //low byte in bl.
    shr ecx,8   //high word in cx
Loopi:
    //case (CelPart and $c0c0) of   //fe------76------

      and edi,$c0c0
      jz Aftercase
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      sub edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      sub edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      sub edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      sub edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0


S0040:
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

S0080:
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      jmp aftercase

S00c0:
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

S4000:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      jmp aftercase

S4040:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

S4080:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      jmp aftercase

S40c0:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

S8000:
      mov [esi],bl
      mov [esi+1],cx
      jmp aftercase

S8040:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

S8080:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      jmp aftercase

S80c0:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

Sc000:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi],bl
      mov [esi+1],cx
      jmp aftercase

Sc040:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx
      jmp aftercase

Sc080:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      jmp aftercase

Sc0c0:
      mov [esi+1*1*3],bl
      mov [esi+1*1*3+1],cx
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+1*2*3],bl
      mov [esi+1*2*3+1],cx
      mov [esi+1*3*3],bl
      mov [esi+1*3*3+1],cx

AfterCase:
    lea esi,[esi+ebp]
    lea eax,[eax*4] //shl can't run in v-pipe.

    dec edx
    //or eax,eax
    mov edi,eax
    jnz Loopi
    pop ebp
    pop esi
    pop edi
    pop ebx
end;



//32 bit pixels.
procedure TMyDDSurface.DisplayCelPart1x1_32(x1,y1: integer; CelPart,AColor: integer);
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  AfterCase, Loopi;
  //for comments see DisplayCelPart2x2 which is conceptually equal.
asm
    push ebx
    push edi
    push esi

    mov esi,dword ptr PitchLookup[4*ecx]   //esi:= y1 * FPitch;
    mov ebx,[eax+TMyDDSurface.FPitch]      //ebx:= Self.FPitch
    lea esi,[esi+edx*4]                    //esi:= (y1*FPitch) + x1
    mov ecx,[AColor]
    add esi,[eax+TMyDDSurface.FVidMem]     //add startaddres.
  //top line.


    mov eax,[CelPart]
    mov edx,4
    mov edi,eax
Loopi:
    //case (CelPart and $c0c0) of   //fe------76------

      and edi,$c0c0
      jz Aftercase
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      sub edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      sub edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      sub edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      sub edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0


S0040:
      mov [esi+1*3*4],ecx
      jmp aftercase

S0080:
      mov [esi+1*2*4],ecx
      jmp aftercase

S00c0:
      mov [esi+1*2*4],ecx
      mov [esi+1*2*4+4],ecx
      jmp aftercase

S4000:
      mov [esi+1*1*4],ecx
      jmp aftercase

S4040:
      mov [esi+1*1*4],ecx
      mov [esi+1*3*4],ecx
      jmp aftercase

S4080:
      mov [esi+1*1*4],ecx
      mov [esi+1*1*4+4],ecx
      jmp aftercase

S40c0:
      mov [esi+1*1*4],ecx
      mov [esi+1*1*4+4],ecx
      mov [esi+1*3*4],ecx
      jmp aftercase

S8000:
      mov [esi],ecx
      jmp aftercase

S8040:
      mov [esi],ecx
      mov [esi+1*3*4],ecx
      jmp aftercase

S8080:
      mov [esi],ecx
      mov [esi+1*2*4],ecx
      jmp aftercase

S80c0:
      mov [esi],ecx
      mov [esi+1*2*4],ecx
      mov [esi+1*2*4+4],ecx
      jmp aftercase

Sc000:
      mov [esi],ecx
      mov [esi+4],ecx
      jmp aftercase

Sc040:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+1*3*4],ecx
      jmp aftercase

Sc080:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+1*2*4],ecx
      jmp aftercase

Sc0c0:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+1*2*4],ecx
      mov [esi+1*2*4+4],ecx

AfterCase:
    lea esi,[esi+ebx]
    lea eax,[eax*4] //shl can't run in v-pipe.

    dec edx
    //or eax,eax
    mov edi,eax
    jnz Loopi
    pop esi
    pop edi
    pop ebx
end;



procedure TMyDDSurface.DisplayCelPart2x2(x1,y1: integer; CelPart,AColor: integer);
//var
  //WritePos: PVidMem;
  //i: integer;
  //BColor: byte absolute AColor;
  //SColor: smallint absolute AColor;
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  Loopi, AfterCase, Aftercase2;
asm
    push ebx
    push edi
    push esi
  //WritePos:= FVidMem;

  //x1,y1 is the top-left part of the CelPart.

  //Inc(Integer(WritePos),x1 + (FPitch * y1));
    mov esi,dword ptr [4*ecx+PitchLookup]
    mov ebx,[eax+TMyDDSurface.FPitch]
    //imul ecx,esi  //due to bug in Delphi 2, this compiles to imul esi,ecx
    add esi,edx   //so in the object-code the result of imul is stored in esi.
    mov edx,[CelPart]
    mov ecx,[AColor]
    mov edi,edx
    add esi,[eax+TMyDDSurface.FVidMem]
Loopi:
      and edi,$c0c0
      jz Aftercase2
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      cmp edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      cmp edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      cmp edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      cmp edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0

      {S0000: begin
        Inc(Integer(WritePos),Pitch*2);
        goto Aftercase
      end;{}

      {S0040: begin
        Inc(Integer(WritePos),2*3); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch-(2*3));
        goto Aftercase
      end; {04:}
S0040:
      mov [esi+2*3],cx
      mov [esi+ebx+2*3],cx
      xor edx,edi
      jmp aftercase

      {S0080: begin
        Inc(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch-(2*2));
        goto Aftercase
      end; {08:}
S0080:
      mov [esi+2*2],cx
      mov [esi+ebx+2*2],cx
      xor edx,edi
      jmp aftercase

      {S00c0: begin
        Inc(Integer(WritePos),2*2); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch-(2*2));
        goto Aftercase
      end; {0c:}
S00c0:
      mov [esi+2*2],ecx
      mov [esi+ebx+2*2],ecx
      xor edx,edi
      jmp aftercase

      {S4000: begin
        Inc(Integer(WritePos),2*1); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch-(2*1));
        goto Aftercase
      end; {40:}
S4000:
      mov [esi+2*1],cx
      mov [esi+ebx+2*1],cx
      xor edx,edi
      jmp aftercase

      {S4040: begin
        Inc(Integer(WritePos),2*1); WritePos^.S:= SColor;
        Inc(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Dec(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch-(2*1));
        goto Aftercase
      end; {44:}
S4040:
      mov [esi+2*1],cx
      mov [esi+2*3],cx
      mov [esi+ebx+2*1],cx
      mov [esi+ebx+2*3],cx
      xor edx,edi
      jmp aftercase

      {S4080: begin
        Inc(Integer(WritePos),2*1); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch-(2*1));
        goto Aftercase
      end; {48:}
S4080:
      mov [esi+2*1],ecx
      mov [esi+ebx+2*1],ecx
      xor edx,edi
      jmp aftercase

      {S40c0: begin
        Inc(Integer(WritePos),2*1); WritePos^.I:= AColor;
        Inc(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Dec(Integer(WritePos),2*2); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch-(2*1));
        goto Aftercase
      end; {4c:}
S40c0:
      mov [esi+2*1],ecx
      mov [esi+2*3],cx
      mov [esi+ebx+2*1],ecx
      mov [esi+ebx+2*3],cx
      xor edx,edi
      jmp aftercase

      {S8000: begin
        WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {80:}
S8000:
      mov [esi],cx
      mov [esi+ebx],cx
      xor edx,edi
      jmp aftercase

      {S8040: begin
        WritePos^.S:= SColor;
        Inc(Integer(WritePos),2*3); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Dec(Integer(WritePos),2*3); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {84:}
S8040:
      mov [esi],cx
      mov [esi+2*3],cx
      mov [esi+ebx],cx
      mov [esi+ebx+2*3],cx
      xor edx,edi
      jmp aftercase

      {S8080: begin
        WritePos^.S:= SColor;
        Inc(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Dec(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {88:}
S8080:
      mov [esi],cx
      mov [esi+2*2],cx
      mov [esi+ebx],cx
      mov [esi+ebx+2*2],cx
      xor edx,edi
      jmp aftercase

      {S80c0: begin
        WritePos^.S:= SColor;
        Inc(Integer(WritePos),2*2); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch); WritePos^.I:= AColor;
        Dec(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {84:}
S80c0:
      mov [esi],cx
      mov [esi+2*2],ecx
      mov [esi+ebx],cx
      mov [esi+ebx+2*2],ecx
      xor edx,edi
      jmp aftercase

      {Sc000: begin
        WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {c0:}
Sc000:
      mov [esi],ecx
      mov [esi+ebx],ecx
      xor edx,edi
      jmp aftercase

      {Sc040: begin
        WritePos^.I:= AColor;
        Inc(Integer(WritePos),2*3); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Dec(Integer(WritePos),2*3); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {c4:}
Sc040:
      mov [esi],ecx
      mov [esi+2*3],cx
      mov [esi+ebx],ecx
      mov [esi+ebx+2*3],cx
      xor edx,edi
      jmp aftercase

      {Sc080: begin
        WritePos^.I:= AColor;
        Inc(Integer(WritePos),2*2); WritePos^.S:= SColor;
        Inc(Integer(WritePos),Pitch); WritePos^.S:= SColor;
        Dec(Integer(WritePos),2*2); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch);
        goto Aftercase
      end; {c8:}
Sc080:
      mov [esi],ecx
      mov [esi+2*2],cx
      mov [esi+ebx],ecx
      mov [esi+ebx+2*2],cx
      xor edx,edi
      jmp aftercase

      {Sc0c0: begin
        WritePos^.I:= AColor;
        Inc(Integer(WritePos),2*2); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch); WritePos^.I:= AColor;
        Dec(Integer(WritePos),2*2); WritePos^.I:= AColor;
        Inc(Integer(WritePos),Pitch);
      end; {cc:}
Sc0c0:
      mov [esi],ecx
      mov [esi+2*2],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+2*2],ecx

Aftercase2:
    //end; {case}
    xor edx,edi //displayed pixels are masked out of edx
AfterCase:
    lea edx,[edx*4] //shl can't run in v-pipe.
    lea esi,[esi+ebx*2]
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    //CelPart:= CelPart shl 2; //next row;

    mov edi,edx

  //end; {for i} {easy to unroll later on}

    jnz Loopi  //next loop if edx still has any pixels left in it.
    pop esi
    pop edi
    pop ebx
end;

procedure TMyDDSurface.DisplayCelPart2x2_16(x1,y1: integer; CelPart,AColor: integer);
//var
  //WritePos: PVidMem;
  //i: integer;
  //BColor: byte absolute AColor;
  //SColor: smallint absolute AColor;
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  Loopi, AfterCase, Aftercase2;
asm
    push ebx
    push edi
    push esi

  //Inc(Integer(WritePos),x1 + (FPitch * y1));
    mov esi,dword ptr PitchLookup[4*ecx]
    mov ebx,[eax+TMyDDSurface.FPitch]
    lea esi,[esi+edx*2]
    mov edx,[CelPart]
    mov ecx,[AColor]
    mov edi,edx
    add esi,[eax+TMyDDSurface.FVidMem]
Loopi:
      and edi,$c0c0
      jz Aftercase2
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      cmp edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      cmp edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      cmp edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      cmp edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0

S0040:
      mov [esi+2*3*2],ecx
      mov [esi+ebx+2*3*2],ecx
      xor edx,edi
      jmp aftercase

S0080:
      mov [esi+2*2*2],ecx
      mov [esi+ebx+2*2*2],ecx
      xor edx,edi
      jmp aftercase

S00c0:
      mov [esi+2*2*2],ecx
      mov [esi+2*2*2+4],ecx
      mov [esi+ebx+2*2*2],ecx
      mov [esi+ebx+2*2*2+4],ecx
      xor edx,edi
      jmp aftercase

S4000:
      mov [esi+2*1*2],ecx
      mov [esi+ebx+2*1*2],ecx
      xor edx,edi
      jmp aftercase

S4040:
      mov [esi+2*1*2],ecx
      mov [esi+2*3*2],ecx
      mov [esi+ebx+2*1*2],ecx
      mov [esi+ebx+2*3*2],ecx
      xor edx,edi
      jmp aftercase

S4080:
      mov [esi+2*1*2],ecx
      mov [esi+2*1*2+4],ecx
      mov [esi+ebx+2*1*2],ecx
      mov [esi+ebx+2*1*2+4],ecx
      xor edx,edi
      jmp aftercase

S40c0:
      mov [esi+2*1*2],ecx
      mov [esi+2*1*2+4],ecx
      mov [esi+2*3*2],ecx
      mov [esi+ebx+2*1*2],ecx
      mov [esi+ebx+2*1*2+4],ecx
      mov [esi+ebx+2*3*2],ecx
      xor edx,edi
      jmp aftercase

S8000:
      mov [esi],ecx
      mov [esi+ebx],ecx
      xor edx,edi
      jmp aftercase

S8040:
      mov [esi],ecx
      mov [esi+2*3*2],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+2*3*2],ecx
      xor edx,edi
      jmp aftercase

S8080:
      mov [esi],ecx
      mov [esi+2*2*2],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+2*2*2],ecx
      xor edx,edi
      jmp aftercase

S80c0:
      mov [esi],ecx
      mov [esi+2*2*2],ecx
      mov [esi+2*2*2+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+2*2*2],ecx
      mov [esi+ebx+2*2*2+4],ecx
      xor edx,edi
      jmp aftercase

Sc000:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      xor edx,edi
      jmp aftercase

Sc040:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+2*3*2],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+2*3*2],ecx
      xor edx,edi
      jmp aftercase

Sc080:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+2*2*2],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+2*2*2],ecx
      xor edx,edi
      jmp aftercase

Sc0c0:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+2*2*2],ecx
      mov [esi+2*2*2+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+2*2*2],ecx
      mov [esi+ebx+2*2*2+4],ecx

Aftercase2:
    //end; {case}
    xor edx,edi //displayed pixels are masked out of edx
AfterCase:
    lea edx,[edx*4] //shl can't run in v-pipe.
    lea esi,[esi+ebx*2]
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    //CelPart:= CelPart shl 2; //next row;

    mov edi,edx

    jnz Loopi  //next loop if edx still has any pixels left in it.
    pop esi
    pop edi
    pop ebx
end;

//24 bit pixels
procedure TMyDDSurface.DisplayCelPart2x2_24(x1,y1: integer; CelPart,AColor: integer);
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  Loopi, AfterCase, Aftercase2;
begin
  DisplayCelPart2plus_24(x1,y1,CelPart,AColor);
  (*
  asm
    push ebx
    push edi
    push esi
    push ebp

  //Inc(Integer(WritePos),x1 + (FPitch * y1));
    mov esi,dword ptr PitchLookup[4*ecx]
    mov ebx,[eax+TMyDDSurface.FPitch]
    lea esi,[esi+edx*2]
    add esi,edx              //esi + edx * 3
    mov edx,[CelPart]
    mov ecx,[AColor]
    mov edi,edx
    add esi,[eax+TMyDDSurface.FVidMem]

    mov ebp,ebx
    mov bl,cl
    shr ecx,8
Loopi:
      and edi,$c0c0
      jz Aftercase2
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      cmp edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      cmp edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      cmp edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      cmp edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0

S0040:
      mov [esi+2*3*3],bl
      mov [esi+2*3*3+1],cx
      mov [esi+2*3*3+3],bl
      mov [esi+2*3*3+4],cx
      mov [esi+ebp+2*3*3],bl
      mov [esi+ebp+2*3*3+1],cx
      mov [esi+ebp+2*3*3+3],bl
      mov [esi+ebp+2*3*3+4],cx
      xor edx,edi
      jmp aftercase

S0080:
      mov [esi+2*2*3],bl
      mov [esi+2*2*3+1],cx
      mov [esi+2*2*3+3],bl
      mov [esi+2*2*3+3+1],cx
      mov [esi+ebp+2*2*3],bl
      mov [esi+ebp+2*2*3+1],cx
      mov [esi+ebp+2*2*3+3],bl
      mov [esi+ebp+2*2*3+3+1],cx
      xor edx,edi
      jmp aftercase

S00c0:
      mov [esi+2*2*3],bl
      mov [esi+2*2*3+1],cx
      mov [esi+2*2*3+3],bl
      mov [esi+2*2*3+3+1],cx
      mov [esi+2*2*3+6],bl
      mov [esi+2*2*3+6+1],cx
      mov [esi+2*2*3+9],bl
      mov [esi+2*2*3+9+1],cx
      mov [esi+ebp+2*2*3],bl
      mov [esi+ebp+2*2*3+1],cx
      mov [esi+ebp+2*2*3+3],bl
      mov [esi+ebp+2*2*3+3+1],cx
      mov [esi+ebp+2*2*3+6],bl
      mov [esi+ebp+2*2*3+6+1],cx
      mov [esi+ebp+2*2*3+9],bl
      mov [esi+ebp+2*2*3+9+1],cx
      xor edx,edi
      jmp aftercase

S4000:
      mov [esi+2*1*3],bl
      mov [esi+2*1*3+1],cx
      mov [esi+2*1*3+3],bl
      mov [esi+2*1*3+3+1],cx
      mov [esi+ebp+2*1*3],bl
      mov [esi+ebp+2*1*3+1],cx
      mov [esi+ebp+2*1*3+3],bl
      mov [esi+ebp+2*1*3+3+1],cx
      xor edx,edi
      jmp aftercase

S4040:
      mov [esi+2*1*3],bl
      mov [esi+2*1*3+1],cx
      mov [esi+2*1*3+3],bl
      mov [esi+2*1*3+3+1],cx
      mov [esi+2*3*3],bl
      mov [esi+2*3*3+1],cx
      mov [esi+2*3*3+3],bl
      mov [esi+2*3*3+3+1],cx
      mov [esi+ebp+2*1*3],bl
      mov [esi+ebp+2*1*3+1],cx
      mov [esi+ebp+2*1*3+3],bl
      mov [esi+ebp+2*1*3+3+1],cx
      mov [esi+ebp+2*3*3],bl
      mov [esi+ebp+2*3*3+1],cx
      mov [esi+ebp+2*3*3+3],bl
      mov [esi+ebp+2*3*3+3+1],cx
      xor edx,edi
      jmp aftercase

S4080:
      mov [esi+2*1*3],bl
      mov [esi+2*1*3+1],cx
      mov [esi+2*1*3+3],bl
      mov [esi+2*1*3+3+1],cx
      mov [esi+2*1*3+6],bl
      mov [esi+2*1*3+6+1],cx
      mov [esi+2*1*3+9],bl
      mov [esi+2*1*3+9+1],cx
      mov [esi+ebp+2*1*3],bl
      mov [esi+ebp+2*1*3+1],cx
      mov [esi+ebp+2*1*3+3],bl
      mov [esi+ebp+2*1*3+3+1],cx
      mov [esi+ebp+2*1*3+6],bl
      mov [esi+ebp+2*1*3+6+1],cx
      mov [esi+ebp+2*1*3+9],bl
      mov [esi+ebp+2*1*3+9+1],cx
      xor edx,edi
      jmp aftercase

S40c0:
      mov [esi+2*1*3],bl
      mov [esi+2*1*3+1],cx
      mov [esi+2*1*3+3],bl
      mov [esi+2*1*3+3+1],cx
      mov [esi+2*1*3+6],bl
      mov [esi+2*1*3+6+1],cx
      mov [esi+2*1*3+9],bl
      mov [esi+2*1*3+9+1],cx
      mov [esi+2*3*3],bl
      mov [esi+2*3*3+1],cx
      mov [esi+2*3*3+3],bl
      mov [esi+2*3*3+3+1],cx
      mov [esi+ebp+2*1*3],bl
      mov [esi+ebp+2*1*3+1],cx
      mov [esi+ebp+2*1*3+3],bl
      mov [esi+ebp+2*1*3+3+1],cx
      mov [esi+ebp+2*1*3+6],bl
      mov [esi+ebp+2*1*3+6+1],cx
      mov [esi+ebp+2*1*3+9],bl
      mov [esi+ebp+2*1*3+9+1],cx
      mov [esi+ebp+2*3*3],bl
      mov [esi+ebp+2*3*3+1],cx
      mov [esi+ebp+2*3*3+3],bl
      mov [esi+ebp+2*3*3+3+1],cx
      xor edx,edi
      jmp aftercase

S8000:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      xor edx,edi
      jmp aftercase

S8040:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+2*3*3],bl
      mov [esi+2*3*3+1],cx
      mov [esi+2*3*3+3],bl
      mov [esi+2*3*3+3+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+2*3*3],bl
      mov [esi+ebp+2*3*3+1],cx
      mov [esi+ebp+2*3*3+3],bl
      mov [esi+ebp+2*3*3+3+1],cx
      xor edx,edi
      jmp aftercase

S8080:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+2*2*3],bl
      mov [esi+2*2*3+1],cx
      mov [esi+2*2*3+3],bl
      mov [esi+2*2*3+3+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+2*2*3],bl
      mov [esi+ebp+2*2*3+1],cx
      mov [esi+ebp+2*2*3+3],bl
      mov [esi+ebp+2*2*3+3+1],cx
      xor edx,edi
      jmp aftercase

S80c0:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+2*2*3],bl
      mov [esi+2*2*3+1],cx
      mov [esi+2*2*3+3],bl
      mov [esi+2*2*3+3+1],cx
      mov [esi+2*2*3+6],bl
      mov [esi+2*2*3+6+1],cx
      mov [esi+2*2*3+9],bl
      mov [esi+2*2*3+9+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+2*2*3],bl
      mov [esi+ebp+2*2*3+1],cx
      mov [esi+ebp+2*2*3+3],bl
      mov [esi+ebp+2*2*3+3+1],cx
      mov [esi+ebp+2*2*3+6],bl
      mov [esi+ebp+2*2*3+6+1],cx
      mov [esi+ebp+2*2*3+9],bl
      mov [esi+ebp+2*2*3+9+1],cx
      xor edx,edi
      jmp aftercase

Sc000:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+6],bl
      mov [esi+6+1],cx
      mov [esi+9],bl
      mov [esi+9+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+6],bl
      mov [esi+ebp+6+1],cx
      mov [esi+ebp+9],bl
      mov [esi+ebp+9+1],cx
      xor edx,edi
      jmp aftercase

Sc040:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+6],bl
      mov [esi+6+1],cx
      mov [esi+9],bl
      mov [esi+9+1],cx
      mov [esi+2*3*3],bl
      mov [esi+2*3*3+1],cx
      mov [esi+2*3*3+3],bl
      mov [esi+2*3*3+3+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+6],bl
      mov [esi+ebp+6+1],cx
      mov [esi+ebp+9],bl
      mov [esi+ebp+9+1],cx
      mov [esi+ebp+2*3*3],bl
      mov [esi+ebp+2*3*3+1],cx
      mov [esi+ebp+2*3*3+3],bl
      mov [esi+ebp+2*3*3+3+1],cx
      xor edx,edi
      jmp aftercase

Sc080:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+6],bl
      mov [esi+6+1],cx
      mov [esi+9],bl
      mov [esi+9+1],cx
      mov [esi+2*2*3],bl
      mov [esi+2*2*3+1],cx
      mov [esi+2*2*3+3],bl
      mov [esi+2*2*3+3+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+6],bl
      mov [esi+ebp+6+1],cx
      mov [esi+ebp+9],bl
      mov [esi+ebp+9+1],cx
      mov [esi+ebp+2*2*3],bl
      mov [esi+ebp+2*2*3+1],cx
      mov [esi+ebp+2*2*3+3],bl
      mov [esi+ebp+2*2*3+3+1],cx
      xor edx,edi
      jmp aftercase

Sc0c0:
      mov [esi],bl
      mov [esi+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+3],bl
      mov [esi+3+1],cx
      mov [esi+9],bl
      mov [esi+9+1],cx
      mov [esi+12],bl
      mov [esi+12+1],cx
      mov [esi+15],bl
      mov [esi+15+1],cx
      mov [esi+18],bl
      mov [esi+18+1],cx
      mov [esi+21],bl
      mov [esi+21+1],cx
      mov [esi+ebp],bl
      mov [esi+ebp+1],cx
      mov [esi+ebp+3],bl
      mov [esi+ebp+3+1],cx
      mov [esi+ebp+6],bl
      mov [esi+ebp+6+1],cx
      mov [esi+ebp+9],bl
      mov [esi+ebp+9+1],cx
      mov [esi+ebp+12],bl
      mov [esi+ebp+12+1],cx
      mov [esi+ebp+15],bl
      mov [esi+ebp+15+1],cx
      mov [esi+ebp+18],bl
      mov [esi+ebp+18+1],cx
      mov [esi+ebp+21],bl
      mov [esi+ebp+21+1],cx
Aftercase2:
    //end; {case}
    xor edx,edi //displayed pixels are masked out of edx
AfterCase:
    lea edx,[edx*4] //shl can't run in v-pipe.
    lea esi,[esi+ebp*2]
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    //CelPart:= CelPart shl 2; //next row;

    mov edi,edx

    jnz Loopi  //next loop if edx still has any pixels left in it.
    pop ebp
    pop esi
    pop edi
    pop ebx
  end;  (**)
end;


procedure TMyDDSurface.DisplayCelPart2x2_32(x1,y1: integer; CelPart,AColor: integer);
label
  S0000, S0040, S0080, S00C0, S4000, S4040, S4080, S40C0,
  S8000, S8040, S8080, S80C0, SC000, SC040, SC080, SC0C0,
  Loopi, AfterCase, Aftercase2;
asm
    push ebx
    push edi
    push esi

  //Inc(Integer(WritePos),x1 + (FPitch * y1));
    mov esi,dword ptr PitchLookup[4*ecx]
    mov ebx,[eax+TMyDDSurface.FPitch]
    lea esi,[esi+edx*4]
    mov edx,[CelPart]
    mov ecx,[AColor]
    mov edi,edx
    add esi,[eax+TMyDDSurface.FVidMem]
Loopi:
      and edi,$c0c0
      jz Aftercase2
      cmp edi,$8000
      jg @@Larger8000
      je S8000
      cmp edi,$4000
      jg @@Larger4000
      je S4000
      cmp edi,$80
      jb S0040
      je S0080
      jmp S00c0
@@Larger4000:
      cmp edi,$4080
      jb S4040
      je S4080
      jmp S40c0
@@Larger8000:
      cmp edi,$c000
      jg @@Largerc000
      je Sc000
      cmp edi,$8080
      jb S8040
      je S8080
      jmp S80c0
@@Largerc000:
      cmp edi,$c080
      jb Sc040
      je Sc080
      jmp Sc0c0

S0040:
      mov [esi+2*3*4],ecx
      mov [esi+2*3*4+4],ecx
      mov [esi+ebx+2*3*4],ecx
      mov [esi+ebx+2*3*4+4],ecx
      xor edx,edi
      jmp aftercase

S0080:
      mov [esi+2*2*4],ecx
      mov [esi+2*2*4+4],ecx
      mov [esi+ebx+2*2*4],ecx
      mov [esi+ebx+2*2*4+4],ecx
      xor edx,edi
      jmp aftercase

S00c0:
      mov [esi+2*2*4],ecx
      mov [esi+2*2*4+4],ecx
      mov [esi+2*2*4+8],ecx
      mov [esi+2*2*4+12],ecx
      mov [esi+ebx+2*2*4],ecx
      mov [esi+ebx+2*2*4+4],ecx
      mov [esi+ebx+2*2*4+8],ecx
      mov [esi+ebx+2*2*4+12],ecx
      xor edx,edi
      jmp aftercase

S4000:
      mov [esi+2*1*4],ecx
      mov [esi+2*1*4+4],ecx
      mov [esi+ebx+2*1*4],ecx
      mov [esi+ebx+2*1*4+4],ecx
      xor edx,edi
      jmp aftercase

S4040:
      mov [esi+2*1*4],ecx
      mov [esi+2*1*4+4],ecx
      mov [esi+2*3*4],ecx
      mov [esi+2*3*4+4],ecx
      mov [esi+ebx+2*1*4],ecx
      mov [esi+ebx+2*1*4+4],ecx
      mov [esi+ebx+2*3*4],ecx
      mov [esi+ebx+2*3*4+4],ecx
      xor edx,edi
      jmp aftercase

S4080:
      mov [esi+2*1*4],ecx
      mov [esi+2*1*4+4],ecx
      mov [esi+2*1*4+8],ecx
      mov [esi+2*1*4+12],ecx
      mov [esi+ebx+2*1*4],ecx
      mov [esi+ebx+2*1*4+4],ecx
      mov [esi+ebx+2*1*4+8],ecx
      mov [esi+ebx+2*1*4+12],ecx
      xor edx,edi
      jmp aftercase

S40c0:
      mov [esi+2*1*4],ecx
      mov [esi+2*1*4+4],ecx
      mov [esi+2*1*4+8],ecx
      mov [esi+2*1*4+12],ecx
      mov [esi+2*3*4],ecx
      mov [esi+2*3*4+4],ecx
      mov [esi+ebx+2*1*4],ecx
      mov [esi+ebx+2*1*4+4],ecx
      mov [esi+ebx+2*1*4+8],ecx
      mov [esi+ebx+2*1*4+12],ecx
      mov [esi+ebx+2*3*4],ecx
      mov [esi+ebx+2*3*4+4],ecx
      xor edx,edi
      jmp aftercase

S8000:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      xor edx,edi
      jmp aftercase

S8040:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+2*3*4],ecx
      mov [esi+2*3*4+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+2*3*4],ecx
      mov [esi+ebx+2*3*4+4],ecx
      xor edx,edi
      jmp aftercase

S8080:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+2*2*4],ecx
      mov [esi+2*2*4+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+2*2*4],ecx
      mov [esi+ebx+2*2*4+4],ecx
      xor edx,edi
      jmp aftercase

S80c0:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+2*2*4],ecx
      mov [esi+2*2*4+4],ecx
      mov [esi+2*2*4+8],ecx
      mov [esi+2*2*4+12],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+2*2*4],ecx
      mov [esi+ebx+2*2*4+4],ecx
      mov [esi+ebx+2*2*4+8],ecx
      mov [esi+ebx+2*2*4+12],ecx
      xor edx,edi
      jmp aftercase

Sc000:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+8],ecx
      mov [esi+12],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+8],ecx
      mov [esi+ebx+12],ecx
      xor edx,edi
      jmp aftercase

Sc040:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+8],ecx
      mov [esi+12],ecx
      mov [esi+2*3*4],ecx
      mov [esi+2*3*4+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+8],ecx
      mov [esi+ebx+12],ecx
      mov [esi+ebx+2*3*4],ecx
      mov [esi+ebx+2*3*4+4],ecx
      xor edx,edi
      jmp aftercase

Sc080:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+8],ecx
      mov [esi+12],ecx
      mov [esi+2*2*4],ecx
      mov [esi+2*2*4+4],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+8],ecx
      mov [esi+ebx+12],ecx
      mov [esi+ebx+2*2*4],ecx
      mov [esi+ebx+2*2*4+4],ecx
      xor edx,edi
      jmp aftercase

Sc0c0:
      mov [esi],ecx
      mov [esi+4],ecx
      mov [esi+8],ecx
      mov [esi+12],ecx
      mov [esi+16],ecx
      mov [esi+20],ecx
      mov [esi+24],ecx
      mov [esi+28],ecx
      mov [esi+ebx],ecx
      mov [esi+ebx+4],ecx
      mov [esi+ebx+8],ecx
      mov [esi+ebx+12],ecx
      mov [esi+ebx+16],ecx
      mov [esi+ebx+20],ecx
      mov [esi+ebx+24],ecx
      mov [esi+ebx+28],ecx
Aftercase2:
    //end; {case}
    xor edx,edi //displayed pixels are masked out of edx
AfterCase:
    lea edx,[edx*4] //shl can't run in v-pipe.
    lea esi,[esi+ebx*2]
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    //CelPart:= CelPart shl 2; //next row;

    mov edi,edx

    jnz Loopi  //next loop if edx still has any pixels left in it.
    pop esi
    pop edi
    pop ebx
end;
{$endif}

procedure TMyDDSurface.DisplayCelPart2plus(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
begin
  {$ifdef win32}
  for i:= 0 to 3 do begin
    case (CelPart and $c0c0) of   //fe------76------
      $0040: PatBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      $0080: PatBlt(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      $00c0: PatBlt(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      $4000: PatBlt(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
      $4040: begin
        PatBlt(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
        PatBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {44:}
      $4080: PatBlt(x1+CelSpace,y1,CelSpace2,CelSpace,AColor);
      $40c0: PatBlt(x1+CelSpace,y1,CelSpace3,CelSpace,AColor);
      $8000: PatBlt(x1,y1,CelSpace,CelSpace,AColor);
      $8040: begin
        PatBlt(x1,y1,CelSpace,CelSpace,AColor);
        PatBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {84:}
      $8080: begin
        PatBlt(x1,y1,CelSpace,CelSpace,AColor);
        PatBlt(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      end; {88:}
      $80c0: begin
        PatBlt(x1,y1,CelSpace,CelSpace,AColor);
        PatBlt(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      end; {84:}
      $c000: PatBlt(x1,y1,CelSpace2,CelSpace,AColor);
      $c040: begin
        PatBlt(x1,y1,CelSpace2,CelSpace,AColor);
        PatBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {c4:}
      $c080: PatBlt(x1,y1,CelSpace3,CelSpace,AColor);
      $c0c0: PatBlt(x1,y1,CelSpace4,CelSpace,AColor);
    end; {case}
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    CelPart:= CelPart shl 2; //next row;
    Inc(y1,CelSpace);
  end; {for i} {easy to unroll later on}
  {$endif}
end;

procedure TMyDDSurface.DisplayCelPart2plus_24(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
begin
  for i:= 0 to 3 do begin
    case (CelPart and $c0c0) of   //fe------76------
      $0040: PatBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      $0080: PatBlt24(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      $00c0: PatBlt24(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      $4000: PatBlt24(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
      $4040: begin
        PatBlt24(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {44:}
      $4080: PatBlt24(x1+CelSpace,y1,CelSpace2,CelSpace,AColor);
      $40c0: PatBlt24(x1+CelSpace,y1,CelSpace3,CelSpace,AColor);
      $8000: PatBlt24(x1,y1,CelSpace,CelSpace,AColor);
      $8040: begin
        PatBlt24(x1,y1,CelSpace,CelSpace,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {84:}
      $8080: begin
        PatBlt24(x1,y1,CelSpace,CelSpace,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      end; {88:}
      $80c0: begin
        PatBlt24(x1,y1,CelSpace,CelSpace,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      end; {84:}
      $c000: PatBlt24(x1,y1,CelSpace2,CelSpace,AColor);
      $c040: begin
        PatBlt24(x1,y1,CelSpace2,CelSpace,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {c4:}
      $c080: PatBlt24(x1,y1,CelSpace3,CelSpace,AColor);
      $c0c0: PatBlt24(x1,y1,CelSpace4,CelSpace,AColor);
    end; {case}
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    CelPart:= CelPart shl 2; //next row;
    Inc(y1,CelSpace);
  end; {for i} {easy to unroll later on}(**)
end;


procedure TMyDDSurface.DisplayCelPartXorDDraw(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
begin
  {$ifdef win32}
  for i:= 0 to 3 do begin
    case (CelPart and $c0c0) of   //fe------76------
      $0040: XorBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      $0080: XorBlt(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      $00c0: XorBlt(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      $4000: XorBlt(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
      $4040: begin
        XorBlt(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
        XorBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {44:}
      $4080: XorBlt(x1+CelSpace,y1,CelSpace2,CelSpace,AColor);
      $40c0: XorBlt(x1+CelSpace,y1,CelSpace3,CelSpace,AColor);
      $8000: XorBlt(x1,y1,CelSpace,CelSpace,AColor);
      $8040: begin
        XorBlt(x1,y1,CelSpace,CelSpace,AColor);
        XorBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {84:}
      $8080: begin
        XorBlt(x1,y1,CelSpace,CelSpace,AColor);
        XorBlt(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      end; {88:}
      $80c0: begin
        XorBlt(x1,y1,CelSpace,CelSpace,AColor);
        XorBlt(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      end; {84:}
      $c000: XorBlt(x1,y1,CelSpace2,CelSpace,AColor);
      $c040: begin
        XorBlt(x1,y1,CelSpace2,CelSpace,AColor);
        XorBlt(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {c4:}
      $c080: XorBlt(x1,y1,CelSpace3,CelSpace,AColor);
      $c0c0: XorBlt(x1,y1,CelSpace4,CelSpace,AColor);
    end; {case}
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    CelPart:= CelPart shl 2; //next row;
    Inc(y1,CelSpace);
  end; {for i} {easy to unroll later on}
  {$endif}
end;

procedure TMyDDSurface.DisplayCelPartXorDDraw_24(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
begin
  for i:= 0 to 3 do begin
    case (CelPart and $c0c0) of   //fe------76------
      $0040: XorBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      $0080: XorBlt24(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      $00c0: XorBlt24(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      $4000: XorBlt24(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
      $4040: begin
        XorBlt24(x1+CelSpace,y1,CelSpace,CelSpace,AColor);
        XorBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {44:}
      $4080: XorBlt24(x1+CelSpace,y1,CelSpace2,CelSpace,AColor);
      $40c0: XorBlt24(x1+CelSpace,y1,CelSpace3,CelSpace,AColor);
      $8000: XorBlt24(x1,y1,CelSpace,CelSpace,AColor);
      $8040: begin
        XorBlt24(x1,y1,CelSpace,CelSpace,AColor);
        XorBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {84:}
      $8080: begin
        XorBlt24(x1,y1,CelSpace,CelSpace,AColor);
        XorBlt24(x1+CelSpace2,y1,CelSpace,CelSpace,AColor);
      end; {88:}
      $80c0: begin
        XorBlt24(x1,y1,CelSpace,CelSpace,AColor);
        XorBlt24(x1+CelSpace2,y1,CelSpace2,CelSpace,AColor);
      end; {84:}
      $c000: XorBlt24(x1,y1,CelSpace2,CelSpace,AColor);
      $c040: begin
        XorBlt24(x1,y1,CelSpace2,CelSpace,AColor);
        XorBlt24(x1+CelSpace3,y1,CelSpace,CelSpace,AColor);
      end; {c4:}
      $c080: XorBlt24(x1,y1,CelSpace3,CelSpace,AColor);
      $c0c0: XorBlt24(x1,y1,CelSpace4,CelSpace,AColor);
    end; {case}
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    CelPart:= CelPart shl 2; //next row;
    Inc(y1,CelSpace);
  end; {for i} {easy to unroll later on}
end;



procedure TMyDDSurface.DisplayCelPartGrid(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
begin
  {$ifdef win32}
  for i:= 0 to 3 do begin
    case (CelPart and $c0c0) of   //fe------76------
      $0040: PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      $0080: PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      $00c0: begin
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $4000: PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
      $4040: begin
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end; {44:}
      $4080: begin
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $40c0: begin
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $8000: PatBlt(x1,y1,CelSize,CelSize,AColor);
      $8040: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end; {84:}
      $8080: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end; {88:}
      $80c0: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end; {84:}
      $c000: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
      end;
      $c040: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end; {c4:}
      $c080: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $c0c0: begin
        PatBlt(x1,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace2,y1,CelSize,CelSize,AColor);
        PatBlt(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end;
    end; {case}
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    CelPart:= CelPart shl 2; //next row;
    Inc(y1,CelSpace);
  end; {for i} {easy to unroll later on}
  {$endif}
end;

procedure TMyDDSurface.DisplayCelPartGrid_24(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
begin
  for i:= 0 to 3 do begin
    case (CelPart and $c0c0) of   //fe------76------
      $0040: PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      $0080: PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      $00c0: begin
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $4000: PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
      $4040: begin
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end; {44:}
      $4080: begin
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $40c0: begin
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $8000: PatBlt24(x1,y1,CelSize,CelSize,AColor);
      $8040: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end; {84:}
      $8080: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end; {88:}
      $80c0: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end; {84:}
      $c000: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
      end;
      $c040: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end; {c4:}
      $c080: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
      end;
      $c0c0: begin
        PatBlt24(x1,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace2,y1,CelSize,CelSize,AColor);
        PatBlt24(x1+CelSpace3,y1,CelSize,CelSize,AColor);
      end;
    end; {case}
    //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
    CelPart:= CelPart shl 2; //next row;
    Inc(y1,CelSpace);
  end; {for i} {easy to unroll later on}
end;


procedure TMyDDSurface.DisplayCelPartWithDC(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
  MyDC: HDC;
  Logop: integer;
begin
  if Assigned(FCanvas) then with FCanvas do begin
    if (AColor = clBlack) then LogOp:= Blackness
    else if (AColor = clWhite) then LogOp:= Whiteness
    else begin
      FCanvas.Brush.Color:= AColor;
      LogOp:= PATCOPY;
    end; {else}
    MyDC:= FCanvas.Handle;
    for i:= 0 to 3 do begin
      case (CelPart and $c0c0) of   //fe------76------
        $0040: Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        $0080: Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace,CelSpace,LogOp);
        $00c0: Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace2,CelSpace,LogOp);
        $4000: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace,CelSpace,LogOp);
        $4040: begin
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        end; {44:}
        $4080: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace2,CelSpace,LogOp);
        $40c0: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace3,CelSpace,LogOp);
        $8000: Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
        $8040: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        end; {84:}
        $8080: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace,CelSpace,LogOp);
        end; {88:}
        $80c0: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace2,CelSpace,LogOp);
        end; {84:}
        $c000: Windows.PatBlt(MyDC,x1,y1,CelSpace2,CelSpace,LogOp);
        $c040: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace2,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        end; {c4:}
        $c080: Windows.PatBlt(MyDC,x1,y1,CelSpace3,CelSpace,LogOp);
        $c0c0: Windows.PatBlt(MyDC,x1,y1,CelSpace4,CelSpace,LogOp);
      end; {case}
      //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
      CelPart:= CelPart shl 2; //next row;
      Inc(y1,CelSpace);
    end; {for i} {easy to unroll later on}
  end; {if}
end;

procedure TMyDDSurface.DisplayCelPartXorDC(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
  MyDC: HDC;
  Logop: integer;
begin
  if Assigned(FCanvas) then with FCanvas do begin
    FCanvas.Brush.Color:= AColor;
    LogOp:= PATINVERT;
    MyDC:= FCanvas.Handle;
    for i:= 0 to 3 do begin
      case (CelPart and $c0c0) of   //fe------76------
        $0040: Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        $0080: Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace,CelSpace,LogOp);
        $00c0: Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace2,CelSpace,LogOp);
        $4000: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace,CelSpace,LogOp);
        $4040: begin
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        end; {44:}
        $4080: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace2,CelSpace,LogOp);
        $40c0: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSpace3,CelSpace,LogOp);
        $8000: Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
        $8040: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        end; {84:}
        $8080: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace,CelSpace,LogOp);
        end; {88:}
        $80c0: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSpace2,CelSpace,LogOp);
        end; {84:}
        $c000: Windows.PatBlt(MyDC,x1,y1,CelSpace2,CelSpace,LogOp);
        $c040: begin
          Windows.PatBlt(MyDC,x1,y1,CelSpace2,CelSpace,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSpace,CelSpace,LogOp);
        end; {c4:}
        $c080: Windows.PatBlt(MyDC,x1,y1,CelSpace3,CelSpace,LogOp);
        $c0c0: Windows.PatBlt(MyDC,x1,y1,CelSpace4,CelSpace,LogOp);
      end; {case}
      //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
      CelPart:= CelPart shl 2; //next row;
      Inc(y1,CelSpace);
    end; {for i} {easy to unroll later on}
  end; {if}
end;


procedure TMyDDSurface.DisplayCelPartGridWithDC(x1,y1: integer; CelPart,AColor: integer);
var
  i: integer;
  MyDC: HDC;
  LogOp: integer;
begin
  if Assigned(FCanvas) then with FCanvas do begin
    if (AColor = clBlack) then LogOp:= Blackness
    else if (AColor = clWhite) then LogOp:= Whiteness
    else begin
      FCanvas.Brush.Color:= AColor;
      LogOp:= PATCOPY;
    end; {else}
    MyDC:= FCanvas.Handle;
    for i:= 0 to 3 do begin
      case (CelPart and $c0c0) of   //fe------76------
        $0040: Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
        $0080: Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        $00c0: begin
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        end;
        $4000: Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
        $4040: begin
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
        end; {44:}
        $4080: begin
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        end;
        $40c0: begin
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        end;
        $8000: Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
        $8040: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
        end; {84:}
        $8080: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        end; {88:}
        $80c0: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        end; {84:}
        $c000: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
        end;
        $c040: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
        end; {c4:}
        $c080: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
        end;
        $c0c0: begin
          Windows.PatBlt(MyDC,x1,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace2,y1,CelSize,CelSize,LogOp);
          Windows.PatBlt(MyDC,x1+CelSpace3,y1,CelSize,CelSize,LogOp);
        end;
      end; {case}
      //reset x,y vidmem pos in cases, 'cause x-end pos is variable.
      CelPart:= CelPart shl 2; //next row;
      Inc(y1,CelSpace);
    end; {for i} {easy to unroll later on}
  end; {if}
end;

procedure TMyDDSurface.DisplayBlackPartWithDC(x1,y1,AColor: integer);
begin
  DisplayCelPartWithDC(x1,y1,$FFFF,AColor);
end;

procedure TMyDDSurface.DisplayBlackPartGridWithDC(x1,y1,AColor: integer);
begin
  DisplayCelPartGridWithDC(x1,y1,$FFFF,AColor);
end;

//formating the color-field is the sole responsebility of the caller.
(*
procedure TMyDDSurface.PatBlt(x1,y1,AWidth, AHeight, AColor: integer);
var
  WritePos: PVidMem;
  x,xi,xb,xs,y: integer;
begin
  x1:= x1 * FBytesPerPixel;
  AWidth:= AWidth * FBytesPerPixel;
  Integer(WritePos):= Integer(FVidMem) + x1 + PitchLookup[y1];
  xi:= AWidth shr 2;
  xb:= AWidth and $03;
  xs:= xb shr 1;
  xb:= xb and $01;
  for y:= 1 to AHeight do begin
    x:= xi;
    while x > 0 do begin
      Dec(x);
      WritePos^.i:= AColor;
      Inc(integer(WritePos),4);
    end; {while}
    if xs > 0 then begin
      WritePos^.s:= AColor;
      Inc(integer(WritePos),2);
    end;
    if xb > 0 then begin
      WritePos^.b:= AColor;
      Inc(integer(WritePos),1);
    end; {for y}
    Inc(integer(WritePos),FPitch - AWidth);
  end; {while}
end;
(**)

{$ifdef win32}
procedure TMyDDSurface.PatBlt(x1,y1,AWidth, AHeight, AColor: integer);
var
  xi,xb: integer;
asm
    push   ebx
    push   esi
    push   edi
    mov    [y1],ecx
    mov    ecx,[AColor]
    //x1:= x1 * FBytesPerPixel;
    mov    ebx,[eax+TMyDDSurface.FBytesPerPixel]
    mov    esi,[AWidth]
    imul   edx,ebx
    //AWidth:= AWidth * FBytesPerPixel;
    mov    edi,[y1]
    imul   esi,ebx
    mov    [AWidth],esi
    //Integer(WritePos):= Integer(FVidMem) + x1 + PitchLookup[y1];
    mov    esi,[eax+TMyDDSurface.FVidMem]
    add    esi,edx
    mov    edx,[AWidth]
    add    esi, dword ptr [4*edi+PitchLookUp]
    //xi:= AWidth shr 2;
    shr    edx,02h
    mov    [xi],edx
    //xb:= AWidth and $03;
    mov    edx,[AWidth]
    and    edx,03h
    mov    [xb],edx
    //for y:= 1 to AHeight do begin
    mov    edi,[AHeight]
    test   edi,edi
    jle    @@Ende
@@ForY:
    //x:= xi;
    mov    edx,[xi]
    //while x > 0 do begin
    test   edx,edx
    jle    @@endWhileX
@@WhileX:
    //Dec(x);
    //WritePos^.i:= (WritePos^.i xor AColor);
    mov    [esi],ecx
    //Inc(integer(WritePos),4);
    add    esi,4
    //while x > 0 do begin
    dec    edx
    jnz    @@WhileX
@@endWhileX:
    //if xs > 0 then begin
    mov    edx, dword ptr [xb]
    cmp    edx,1
    jle     @@IfXb
    //WritePos^.s:= (WritePos^.s xor word(AColor
    mov    [esi],cl
    mov    [esi+1],ch
    //Inc(integer(WritePos),2);
    add    esi,2
@@IfXb:
    //if xb > 0 then begin
    and    edx,1
    jz    @@NextLine
    //WritePos^.b:= (WritePos^.b xor byte(AColor
    mov    [esi],cl
    //Inc(integer(WritePos),1);
    inc    esi
@@NextLine:
    //Inc(integer(WritePos),FPitch - AWidth);
    mov    edx,[eax+TMyDDSurface.FPitch]
    sub    edx,[AWidth]
    add    esi,edx
    //for y:= 1 to AHeight do begin
    dec    edi
    jnz    @@ForY
    //end;
@@Ende:
    pop    edi
    pop    esi
    pop    ebx
end; {asm}
{$endif}

procedure TMyDDSurface.PatBlt24(x1,y1,AWidth, AHeight, AColor: integer);
var
  WritePos: PVidMem;
  x,x3,y: integer;
  Color1: byte;
  Color23: SmallInt;
begin
  //FillRect(Rect(x1,y1,x1+AWidth,y1+AHeight),AColor);
  Color1:= AColor and $ff;
  Color23:= (AColor shr 8) and $ffff;
  x1:= x1 * FBytesPerPixel;
  x3:= AWidth;
  AWidth:= AWidth * FBytesPerPixel;
  NativeInt(WritePos):= NativeInt(FVidMem) + x1 + PitchLookup[y1];
  for y:= 1 to AHeight do begin
    x:= x3;
    //while x > 1 do begin
    while x > 0 do begin
      Dec(x);
      //WritePos^.i:= AColor;
      WritePos^.b:= Color1;
      Inc(NativeInt(WritePos),1);
      WritePos^.s:= Color23;
      Inc(NativeInt(WritePos),2);
    end; {while}
    //place last pixel.
    (*WritePos^.s:= AColor;
    Inc(integer(WritePos),2);
    WritePos^.b:= TVidMem(AColor).c[2];
    Inc(integer(WritePos),1);  (**)
    Inc(NativeInt(WritePos),FPitch - AWidth);
  end; {for y}
end;


{$ifdef win32}
//formating the color-field is the sole responsebility of the caller.
procedure TMyDDSurface.XorBlt(x1,y1,AWidth, AHeight, AColor: integer);
var
  xi,xb: integer;
asm
    push   ebx
    push   esi
    push   edi
    mov    [y1],ecx
    mov    ecx,[AColor]
    //x1:= x1 * FBytesPerPixel;
    mov    ebx,[eax+TMyDDSurface.FBytesPerPixel]
    mov    esi,[AWidth]
    imul   edx,ebx
    //AWidth:= AWidth * FBytesPerPixel;
    mov    edi,[y1]
    imul   esi,ebx
    mov    [AWidth],esi
    //Integer(WritePos):= Integer(FVidMem) + x1 + PitchLookup[y1];
    mov    esi,[eax+TMyDDSurface.FVidMem]
    add    esi,edx
    mov    edx,[AWidth]
    add    esi, dword ptr [4*edi+PitchLookUp]
    //xi:= AWidth shr 2;
    shr    edx,02h
    mov    [xi],edx
    //xb:= AWidth and $03;
    mov    edx,[AWidth]
    and    edx,03h
    mov    [xb],edx
    //for y:= 1 to AHeight do begin
    mov    edi,[AHeight]
    test   edi,edi
    jle    @@Ende
@@ForY:
    //x:= xi;
    mov    edx,[xi]
    //while x > 0 do begin
    test   edx,edx
    jle    @@endWhileX
@@WhileX:
    //Dec(x);
    //WritePos^.i:= (WritePos^.i xor AColor);
    xor    [esi],ecx
    //Inc(integer(WritePos),4);
    add    esi,4
    //while x > 0 do begin
    dec    edx
    jnz    @@WhileX
@@endWhileX:
    //if xs > 0 then begin
    mov    edx, dword ptr [xb]
    cmp    edx,1
    jle     @@IfXb
    //WritePos^.s:= (WritePos^.s xor word(AColor
    xor    [esi],cl
    xor    [esi+1],ch
    //Inc(integer(WritePos),2);
    add    esi,2
@@IfXb:
    //if xb > 0 then begin
    and    edx,1
    jz    @@NextLine
    //WritePos^.b:= (WritePos^.b xor byte(AColor
    xor    [esi],cl
    //Inc(integer(WritePos),1);
    inc    esi
@@NextLine:
    //Inc(integer(WritePos),FPitch - AWidth);
    mov    edx,[eax+TMyDDSurface.FPitch]
    sub    edx,[AWidth]
    add    esi,edx
    //for y:= 1 to AHeight do begin
    dec    edi
    jnz    @@ForY
    //end;
@@Ende:
    pop    edi
    pop    esi
    pop    ebx
  end; {asm}
  (*x1:= x1 * FBytesPerPixel;
  AWidth:= AWidth * FBytesPerPixel;
  Integer(WritePos):= Integer(FVidMem) + x1 + PitchLookup[y1];
  xi:= AWidth shr 2;
  xb:= AWidth and $03;
  xs:= xb shr 1;
  xb:= xb and $01;
  for y:= 1 to AHeight do begin
    x:= xi;
    while x > 0 do begin
      Dec(x);
      WritePos^.i:= (WritePos^.i xor AColor);
      Inc(integer(WritePos),4);
    end; {while}
    if xs > 0 then begin
      WritePos^.s:= (WritePos^.s xor word(AColor));
      Inc(integer(WritePos),2);
    end;
    if xb > 0 then begin
      WritePos^.b:= (WritePos^.b xor byte(AColor));
      Inc(integer(WritePos),1);
    end; {for y}
    Inc(integer(WritePos),FPitch - AWidth);
  end; {while}(**)
//end;

{$endif}

procedure TMyDDSurface.XorBlt24(x1,y1,AWidth, AHeight, AColor: integer);
var
  WritePos: PVidMem;
  x,x3,y: integer;
  Color1: byte;
  Color23: SmallInt;
begin
  //FillRect(Rect(x1,y1,x1+AWidth,y1+AHeight),AColor);
  Color1:= AColor and $ff;
  Color23:= (AColor shr 8) and $ffff;
  x1:= x1 * FBytesPerPixel;
  x3:= AWidth;
  AWidth:= AWidth * FBytesPerPixel;
  NativeInt(WritePos):= NativeInt(FVidMem) + x1 + PitchLookup[y1];
  for y:= 1 to AHeight do begin
    x:= x3;
    //while x > 1 do begin
    while x > 0 do begin
      Dec(x);
      //WritePos^.i:= AColor;
      WritePos^.b:= WritePos^.b xor Color1;
      Inc(NativeInt(WritePos),1);
      WritePos^.s:= WritePos^.s xor Color23;
      Inc(NativeInt(WritePos),2);
    end; {while}
    Inc(NativeInt(WritePos),FPitch - AWidth);
  end; {for y}
end;


procedure TMyDDSurface.FillWindow(AWindow: TControl; ARect: TRect; AColor: integer; wait: boolean);
begin
  if (FDirectDrawEnabled = DDFast) then begin
    with ARect do begin
      TopLeft:= AWindow.ClientToScreen(TopLeft);
      BottomRight:= AWindow.ClientToScreen(BottomRight);
    end; {with}
    FillRect(ARect, AColor, wait)
  end {if}
  else begin
    Lock(TMyCustomControl(AWindow).Canvas);
    if Assigned(FCanvas) then with FCanvas do begin
      Brush.Style:= bsSolid;
      Brush.Color:= AColor;
      FillRect(ARect);
    end; {if}
    Unlock;
  end; {else}
end;

//This call does not work yet, do not use.
(*procedure TMyDDSurface.FillRectXor(ARect: TRect; AColor: integer);
var
  DDBltFx: TDDBltFx;
  //DDResult: HResult;
  WasLocked: Boolean;
  OldRop2: integer;
begin
  WasLocked:= Locked;
  if (DirectDrawEnabled = [DDFast]) then begin
    UnLock;
    with DDBltFx do begin
      dwSize:= SizeOf(DDBltFx);
      dwFillColor:= AColor;
      dwROP:= R2_XorPen;
    end; {with}
    {DDResult:= }FDDSurface.Blt(ARect, nil, nil,
                 DDBlt_Wait or DDBlt_ColorFill or DDBLT_ROP,@DDBltFx);
    //ignore errors...
    {if (DDResult <> DD_OK) then
      raise Exception.Create('Blt failed: errorcode '+IntToHex(DDResult,8));
    {}
  end {if DDraw}
  else if Locked then begin
    FCanvas.Brush.Color:= AColor;
    OldROP2:= SetRop2(FCanvas.Handle,R2_XorPen);
    with ARect do FCanvas.Rectangle(Left,Top,Right,Bottom);
    SetROP2(FCanvas.Handle,OldROP2);
  end; {if}

  if WasLocked then Lock(FCanvas) else UnLock;
end; (**)

procedure TMyDDSurface.FillRect(ARect: TRect; AColor: integer; Wait: boolean);
var
  DDBltFx: TDDBltFx;
  DDResult: HResult;
  WasLocked: Boolean;
  WaitStatus: THandle;
  Flags: integer;
begin
  WasLocked:= Locked;
  if (DirectDrawEnabled = DDFast) then begin
    UnLock;
    with DDBltFx do begin
      dwSize:= SizeOf(DDBltFx);
      dwFillColor:= AColor;
    end; {with}
    Flags:= DDBlt_ColorFill;
    if Wait then Flags:= Flags or DDBlt_Wait;
    DDResult:= FDDSurface.Blt(ARect, nil, nil,Flags,@DDBltFx);
    if DDResult = DD_OK then begin
    //ignore errors, but do not enter in endless loop if there is an error.

    //wait until blit is ready before doing anything else.
      repeat WaitStatus:= FDDSurface.GetBltStatus(DDGBS_ISBLTDONE);
      until (WaitStatus <> DDERR_WASSTILLDRAWING);
    end; {if}
  end {if DDraw}
  else if Locked then begin
    FCanvas.Brush.Color:= AColor;
    with ARect do FCanvas.Rectangle(Left,Top,Right,Bottom);
  end; {if}

  if WasLocked then Lock(FCanvas); //else Unlock;
end;

procedure TMyDDSurface.DisplayChange(BitsPerPixel,Width,Height: integer);
var
  OldDirectDrawEnabled:  TDDStates;
begin
  FBytesPerPixel:= BitsPerPixel div 8;
  if (BitsPerPixel < 8) and (FDirectDrawEnabled = DDFast) then begin
    UnLock;
    raise Exception.Create('I cannot work with 16 color''s, sorry'+#10+#13+
    '256 colors is the minimum');
  end; {if}
  //Force recreation of DisplaySurfaces.
  OldDirectDrawEnabled:= DirectDrawEnabled;
  DirectDrawEnabled:= 0;
  DirectDrawEnabled:= OldDirectDrawEnabled;
  InitDrawRoutines(CelSpace,GridEnabled);
end;

procedure TMyDDSurface.InitDrawRoutines(ACelSpace: integer; GridOn: Boolean);
begin
  CelSpace:= ACelSpace;
  CelSpace2:= CelSpace * 2;
  CelSpace3:= CelSpace * 3;
  CelSpace4:= CelSpace * 4;
  CelSpace8:= CelSpace * 8;
  CelSpace12:= CelSpace * 12;
  CelSpace16:= CelSpace * 16;
  GridEnabled:= GridOn;
  if CelSpace < 4 then CelSize:= CelSpace
  //the size of the gridlines is hereby fixed at 1 pixel width/height
  else CelSize:= CelSpace - integer(GridOn);
  if (FDirectDrawEnabled = DDFast) then begin
    if FBytesPerPixel = 3 then FDisplayCelPartXor:= DisplayCelPartXorDDraw_24
    else FDisplayCelPartXor:= DisplayCelPartXorDDraw;
    case CelSpace of
      1: case FBytesPerPixel of
        1: begin {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart1x1;
          FDisplayBlackPart:= DisplayBlackPart1x1;
          {$endif}
        end;
        2: begin {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart1x1_16;
          FDisplayBlackPart:= DisplayBlackPart1x1_16;  {$endif}
        end;
        3: begin {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart1x1_24;
          FDisplayBlackPart:= DisplayBlackPart1x1_24; {$endif}
        end;
        4: begin  {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart1x1_32;
          FDisplayBlackPart:= DisplayBlackPart1x1_32; {$endif}
        end;
      end; {1:}
      2: case FBytesPerPixel of
        1: begin  {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart2x2;
          FDisplayBlackPart:= DisplayBlackPart2x2;  {$endif}
        end;
        2: begin {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart2x2_16;
          FDisplayBlackPart:= DisplayBlackPart2x2_16;  {$endif}
        end;
        3: begin  {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart2x2_24;
          FDisplayBlackPart:= DisplayBlackPart2x2_24; {$endif}
        end;
        4: begin  {$ifdef win32}
          FDisplayCelPart:= DisplayCelPart2x2_32;
          FDisplayBlackPart:= DisplayBlackPart2x2_32;  {$endif}
        end;
      end;
      3: begin
        if FBytesPerPixel = 3 then begin
          FDisplayCelPart:= DisplayCelPart2plus_24;
          FDisplayBlackPart:= DisplayBlackPart2plus_24;
        end {if}
        else begin
          FDisplayCelPart:= DisplayCelPart2plus;
          FDisplayBlackPart:= DisplayBlackPart2plus;
        end; {else}
      end;
      else if GridOn then begin
        if FBytesPerPixel = 3 then begin
          FDisplayCelPart:= DisplayCelPartGrid_24;
          FDisplayBlackPart:= DisplayBlackPartGrid_24;
        end {if}
        else begin
          FDisplayCelPart:= DisplayCelPartGrid;
          FDisplayBlackPart:= DisplayBlackPartGrid;
        end; {else}
      end {else if}
      else begin
        if FBytesPerPixel = 3 then begin
          FDisplayCelPart:= DisplayCelPart2plus_24;
          FDisplayBlackPart:= DisplayBlackPart2plus_24;
        end {if}
        else begin
          FDisplayCelPart:= DisplayCelPart2plus;
          FDisplayBlackPart:= DisplayBlackPart2plus;
        end; {else}
      end;  {else}
    end;
  end
  else begin
    FDisplayCelPartXor:= DisplayCelPartXorDC;
    if (GridEnabled and (CelSpace >= 4)) then begin
      FDisplayCelPart:= DisplayCelPartGridWithDC;
      FDisplayBlackPart:= DisplayBlackPartGridWithDC;
    end {if}
    else begin
      FDisplayCelPart:= DisplayCelPartWithDC;
      FDisplayBlackPart:= DisplayBlackPartWithDC;
    end; {else}
  end; {else}
end;

procedure TMyDDSurface.SuspendDirectDraw(Suspend: boolean);
const
  TempDisplayCelPart: TDisplayCelPart = nil;
  TempDisplayBlackPart: TDisplayBlackPart = nil;
  TempDisplayCelPartXor: TDisplayCelPart = nil;
begin
  if Suspend then begin
    TempDisplayCelPart:= FDisplayCelPart;
    TempDisplayBlackPart:= FDisplayBlackPart;
    TempDisplayCelPartXor:= FDisplayCelPartXor;
    FDisplayCelPartXor:= DisplayCelPartXorDC;
    if (GridEnabled and (CelSpace >= 4)) then begin
      FDisplayCelPart:= DisplayCelPartGridWithDC;
      FDisplayBlackPart:= DisplayBlackPartGridWithDC;
    end {if}
    else begin
      FDisplayCelPart:= DisplayCelPartWithDC;
      FDisplayBlackPart:= DisplayBlackPartWithDC;
    end; {else}
  end
  else begin
    if Assigned(TempDisplayCelPart) then begin
      FDisplayCelPart:= TempDisplayCelPart;
      FDisplayBlackPart:= TempDisplayBlackPart;
      FDisplayCelPartXor:= TempDisplayCelPartXor;
    end
    else InitDrawRoutines(CelSpace,GridEnabled);
  end;
end;

procedure TMyDDSurface.SetBounds(Value: TRect);
begin
  FBounds:= Value;
end;

initialization
finalization
end.