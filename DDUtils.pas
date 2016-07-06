unit DDUtils;

interface

uses Windows, SysUtils, Classes, DDraw;

{ DDCopyBitmap
  - Draw a bitmap into a DirectDrawSurface }
procedure DDCopyBitmap(Surface : IDirectDrawSurface;
                        Bitmap  : HBITMAP;
                        x, y, Width, Height : integer);

{ DDLoadBitmap
  - Create a DirectDrawSurface from a bitmap resource or file. }
function DDLoadBitmap(DirectDraw       : IDirectDraw;
                        const BitmapName : string;
                        Width            : integer;
                        Height           : integer) : IDirectDrawSurface;

{ DDReLoadBitmap
  - Load a bitmap from a file or resource into a directdraw surface.
  - Normally used to re-load a surface after a restore. }
function DDReLoadBitmap(Surface: IDirectDrawSurface;
                        const BitmapName: string): HResult;

{ DDLoadPalette
  - Create a DirectDraw palette object from a bitmap resource or file.
  - If the resource doesn't exist or '' is passed create a default 332 palette. }
function DDLoadPalette(DirectDraw: IDirectDraw;
                       const BitmapName : string) : IDirectDrawPalette;

{ DDColorMatch
  - Convert an RGB color to a pysical color.
  - Works by letting GDI SetPixel() do the color matching
    then we lock the memory and see what it got mapped to. }
function DDColorMatch(Surface: IDirectDrawSurface;
                      RGB: TColorRef) : integer;

{ DDSetColorKey
  - Set a color key for a surface, given an RGB.
  - If you pass CLR_INVALID as the color key, the pixel
    in the upper-left corner will be used. }
function DDSetColorKey(Surface : IDirectDrawSurface;
                       RGB     : TColorRef) : HResult;

implementation

procedure DDCopyBitmap(Surface : IDirectDrawSurface;
                       Bitmap  : HBITMAP;
                       x, y, Width, Height : integer);
var
  ImageDC     : HDC;
  DC          : HDC;
  BM          : Windows.TBitmap;
  SurfaceDesc : TDDSurfaceDesc;
begin
  if (Surface = nil) or (Bitmap = 0) then
    raise Exception.Create('Invalid parameters for DDCopyBitmap');

  // make sure this surface is restored.
  Surface.Restore;

  //  select bitmap into a memoryDC so we can use it.
  ImageDC:= CreateCompatibleDC(0);
  try
    SelectObject(ImageDC, Bitmap);

    // get size of the bitmap
    GetObject(Bitmap, SizeOf(BM), @BM);
    if Width = 0 then Width:= BM.bmWidth;
    if Height = 0 then Height:= BM.bmHeight;

    // get size of surface.
    SurfaceDesc.dwSize:= SizeOf(SurfaceDesc);
    SurfaceDesc.dwFlags:= DDSD_HEIGHT or DDSD_WIDTH;
    Surface.GetSurfaceDesc(SurfaceDesc);

    if Surface.GetDC(DC) <> DD_OK then
      Raise Exception.Create('GetDC failed for DirectDraw surface');
    try
      StretchBlt(DC, 0, 0, SurfaceDesc.dwWidth, SurfaceDesc.dwHeight,
                  ImageDC, x, y, Width, Height, SRCCOPY);
      finally Surface.ReleaseDC(DC);
    end;
    finally DeleteDC(ImageDC);
  end;
end;

function DDLoadBitmap(DirectDraw       : IDirectDraw;
                      const BitmapName : string;
                      Width            : integer;
                      Height           : integer) : IDirectDrawSurface;

var
  Bitmap      : HBitmap;
  BM          : Windows.TBitmap;
  SurfaceDesc : TDDSurfaceDesc;
begin
  //  try to load the bitmap as a resource, if that fails, try it as a file
  Bitmap:= LoadImage(GetModuleHandle(nil), PChar(BitmapName),
                     IMAGE_BITMAP, Width, Height, LR_CREATEDIBSECTION);
  try
    if Bitmap = 0 then
      Bitmap:= LoadImage(0, PChar(BitmapName), IMAGE_BITMAP, Width, Height,
                           LR_LOADFROMFILE or LR_CREATEDIBSECTION);

    if Bitmap = 0 then
      Raise Exception.CreateFmt('Unable to load bitmap %s', [ BitmapName ]);

    // get size of the bitmap
    GetObject(Bitmap, SizeOf(BM), @BM);

    // create a DirectDrawSurface for this bitmap
    FillChar(SurfaceDesc, SizeOf(SurfaceDesc), 0);
    with SurfaceDesc do begin
      dwSize:= SizeOf(SurfaceDesc);
      dwFlags:= DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH;
      ddsCaps.dwCaps:= DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      dwWidth:= BM.bmWidth;
      dwHeight:= BM.bmHeight;
    end;
    if DirectDraw.CreateSurface(SurfaceDesc, Result, nil) = HResult(DDERR_OUTOFVIDEOMEMORY)
    then with SurfaceDesc do begin
      ddsCaps.dwCaps:= DDSCAPS_OFFSCREENPLAIN;
      if DirectDraw.CreateSurface(SurfaceDesc, Result, nil) <> DD_OK then
        raise Exception.Create('CreateSurface failed');
    end; {if with}

    DDCopyBitmap(Result, Bitmap, 0, 0, 0, 0);
    finally if Bitmap <> 0 then DeleteObject(Bitmap);
  end; {try}
end;


function DDReLoadBitmap(Surface          : IDirectDrawSurface;
                        const BitmapName : string): HResult;
var
  Bitmap : HBitmap;
begin
  Result:= DD_OK;
  //  try to load the bitmap as a resource, if that fails, try it as a file
  Bitmap:= LoadImage(GetModuleHandle(nil), PChar(BitmapName),
                     IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
  try
    if Bitmap = 0 then
      Bitmap:= LoadImage(0, PChar(BitmapName), IMAGE_BITMAP,
                           0, 0, LR_LOADFROMFILE or LR_CREATEDIBSECTION);
    if Bitmap = 0 then Result:= HResult(DDERR_EXCEPTION);
    DDCopyBitmap(Surface, Bitmap, 0, 0, 0, 0);
    finally DeleteObject(Bitmap);
  end; {try}
end;


function DDLoadPalette(DirectDraw       : IDirectDraw;
                       const BitmapName : string) : IDirectDrawPalette;

type
  TRGB = array[ 0..255 ] of TRGBQuad;
  PRGB = ^TRGB;
var
  i, n             : integer;
  h                : HRsrc;
  BitmapInfo       : PBitmapInfo;
  APE              : array[0..255] of TPaletteEntry;
  RGB              : PRGB;
  bfHeader         : TBitmapFileHeader;
  biHeader         : TBitmapInfoHeader;
  Temp             : byte;
begin
  // build a 332 palette as the default.
  for i:= 0 to 255 do with APE[i] do begin
    peRed:= (((i shr 5) and $07) * 255 div 7);
    peGreen:= (((i shr 2) and $07) * 255 div 7);
    peBlue:= ((i and $03) * 255 div 3);
    peFlags:= 0;
  end; {for i}
  // get a pointer to the bitmap resource.
  if BitmapName <> '' then begin
    h:= FindResource(0, PChar(BitmapName), RT_BITMAP);
    if h <> 0 then begin
      BitmapInfo:= PBitmapInfo(LockResource(LoadResource(0, h)));
      RGB:= PRGB(@BitmapInfo^.bmiColors);
      if (BitmapInfo = nil) or
         (BitmapInfo^.bmiHeader.biSize < sizeof(TBITMAPINFOHEADER)) then n:= 0
      else if (BitmapInfo^.bmiHeader.biBitCount > 8) then n:= 0
      else if (BitmapInfo^.bmiHeader.biClrUsed = 0) then
         n:= 1 shl BitmapInfo^.bmiHeader.biBitCount
      else n:= BitmapInfo^.bmiHeader.biClrUsed;

      //  a DIB color table has its colors stored BGR not RGB
      //  so flip them around.
      for i:= 0 to n - 1 do with APE[i], RGB^[i] do begin
        peRed:= rgbRed;
        peGreen:= rgbGreen;
        peBlue:= rgbBlue;
        peFlags:= 0;
      end; {for i}
    end {if}
    else begin
      with TFileStream.Create(BitmapName, fmOpenRead) do try
        Read(bfHeader, SizeOf(bfHeader));
        Read(biHeader, SizeOf(biHeader));
        Read(APE, SizeOf(APE));
        finally Free;
      end; {with try}

      //  get the number of colors in the color table
      if biHeader.biSize <> SizeOf(TBitmapInfoHeader) then n:= 0
      else if biHeader.biBitCount > 8 then n:= 0
      else if biHeader.biClrUsed = 0 then n:= 1 shl biHeader.biBitCount
      else n:= biHeader.biClrUsed;

      //  a DIB color table has its colors stored BGR not RGB
      //  so flip them around.
      for i:= 0 to n - 1 do with APE[i] do begin
        Temp:= peRed;
        peRed:= peBlue;
        peBlue:= Temp;
      end; {for i}
    end; {else}
  end; {if}

  // create the DD palette
  if DirectDraw.CreatePalette(DDPCAPS_8BIT, @APE[0], Result, nil) <> DD_OK then
    raise Exception.Create('DirectDraw.CreatePalette failed');
end;


function DDColorMatch(Surface : IDirectDrawSurface;
                      RGB     : TColorRef) : integer;
var
  TempValue   : TColorRef;
  DC          : HDC;
  SurfaceDesc : TDDSurfaceDesc;
  DDResult    : HResult;
begin
  TempValue:= 0;
  Result:= Integer(CLR_INVALID);

  //  use GDI SetPixel to color match for us
  if (RGB <> CLR_INVALID) and (Surface.GetDC(DC) = DD_OK) then try
    TempValue:= GetPixel(DC, 0, 0);      // save current pixel value
    SetPixel(DC, 0, 0, RGB);              // set our value
    finally Surface.ReleaseDC(DC);
  end; {try}

  // now lock the surface so we can read back the converted color
  SurfaceDesc.dwSize:= sizeof(SurfaceDesc);
  try
    repeat
      DDResult:= Surface.Lock(nil, @SurfaceDesc, 0, 0);
    until DDResult <> HResult(DDERR_WASSTILLDRAWING);

    if DDResult = DD_OK then begin
      // get value and mask it to bpp
      Result:= PInteger(SurfaceDesc.lpSurface)^ and
               (1 shl SurfaceDesc.ddpfPixelFormat.dwRGBBitCount) - 1;
    end; {if}
    finally Surface.Unlock(nil);
  end;

  //  now put the color that was there back.
  if (RGB <> CLR_INVALID) and (Surface.GetDC(DC) = DD_OK) then try
    SetPixel(DC, 0, 0, TempValue);
    finally Surface.ReleaseDC(DC);
  end; {if try}
end;

function DDSetColorKey(Surface : IDirectDrawSurface;
                         RGB     : TColorRef) : HResult;
var
  ColorKey : TDDColorKey;
begin
  ColorKey.dwColorSpaceLowValue:= DDColorMatch(Surface, RGB);
  ColorKey.dwColorSpaceHighValue:= ColorKey.dwColorSpaceLowValue;
  Result:= Surface.SetColorKey(DDCKEY_SRCBLT, ColorKey);
end;

end.
