unit DropLife32Source;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX,
  DropSource, DropTarget, LifeBox, LifeGen, LifeConst, LifeUtil;


type
  TDropLife32Source = class(TDropSource)
  private
    FUniverse: TUniverse;
  protected
    function DoGetData(const FormatEtcIn: TFormatEtc;
             out Medium: TStgMedium):HResult; override;
    function CutOrCopyToClipboard: boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Universe: String read FUniverse write FUniverse;
  end;

  TDropLife32Target = class(TDropTarget)
  private
    FUniverse: TUniverse;
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    function HasValidFormats: boolean; override;
  public
    function PasteFromClipboard: longint; override;
    property Universe: TUniverse read FUniverse write FUniverse;
  end;

procedure Register;

implementation

constructor TDropLife32Source.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  AddFormatEtc(CF_TEXT, nil, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_LIFE32, nil, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  //These next two formats have been commented out (for the time being)
  //as they interfer with text drag and drop in Word97.
  //AddFormatEtc(CF_FILEGROUPDESCRIPTOR, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  //AddFormatEtc(CF_FILECONTENTS, NIL, DVASPECT_CONTENT, 0, TYMED_HGLOBAL);
end;
// -----------------------------------------------------------------------------

function TDropTextSource.CutOrCopyToClipboard: boolean;
var
  FormatEtcIn: TFormatEtc;
  Medium: TStgMedium;
begin
  FormatEtcIn.cfFormat := CF_TEXT;
  FormatEtcIn.dwAspect := DVASPECT_CONTENT;
  FormatEtcIn.tymed := TYMED_HGLOBAL;
  if not(Assigned(Universe) then result:= false
  else if GetData(formatetcIn,Medium) = S_OK then begin
    Clipboard.SetAsHandle(CF_TEXT,Medium.hGlobal);
    result:= true;
  end
  else result:= false;
end;
// -----------------------------------------------------------------------------

function TDropTextSource.DoGetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium):HRESULT;
var
  pText: PChar;

begin
  Medium.tymed := 0;
  Medium.UnkForRelease := nil;
  Medium.hGlobal := 0;
  Result:= S_OK;
  if Assigned(FUniverse) then begin
    if (FormatEtcIN.cfFormat = CF_LIFE32) then begin
      FUniverse.
    end
    //--------------------------------------------------------------------------
    else if (FormatEtcIn.cfFormat = CF_TEXT) then begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(FText)+1);
      if (Medium.hGlobal <> 0) then begin
        medium.tymed := TYMED_HGLOBAL;
        pText := PChar(GlobalLock(Medium.hGlobal));
        try
          StrCopy(pText, PChar(fText));
        finally
          GlobalUnlock(Medium.hGlobal);
        end; {try}
      end {if}
      else result:= E_OUTOFMEMORY;
    end; {if CF_TEXT}
  end  {if assigned}
  else result:= E_UNEXPECTED;
end;


function TDropTextTarget.PasteFromClipboard: longint;
var
  Global: HGlobal;
  TextPtr: pChar;
begin
  result := DROPEFFECT_NONE;
  if not ClipBoard.HasFormat(CF_TEXT) then exit;
  Global := Clipboard.GetAsHandle(CF_TEXT);
  TextPtr := GlobalLock(Global);
  fText := TextPtr;
  GlobalUnlock(Global);
  result := DROPEFFECT_COPY;
end;
// -----------------------------------------------------------------------------

function TDropTextTarget.HasValidFormats: boolean;
begin
  result := (fDataObj.QueryGetData(TextFormatEtc) = S_OK);
end;
// -----------------------------------------------------------------------------

procedure TDropTextTarget.ClearData;
begin
  fText := '';
end;
// -----------------------------------------------------------------------------

function TDropTextTarget.DoGetData: boolean;
var
  medium: TStgMedium;
  cText: pchar;
begin
  result := false;
  medium.hGlobal:= 0;
  if fText <> '' then result := true // already got it!
  else if (fDataObj.GetData(TextFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then exit;
      cText := PChar(GlobalLock(medium.HGlobal));
      fText := cText;
      GlobalUnlock(medium.HGlobal);
      result := true;
    finally
      ReleaseStgMedium(medium);
    end;
  end
  else
    result := false;
end;




procedure Register;
begin
  RegisterComponents('Samples', [TDropLife32Source]);
  RegisterComponents('Samples', [TDropLife32Target]);
end;

end.
