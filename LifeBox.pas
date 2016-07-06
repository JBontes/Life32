unit LifeBox;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Classes, Windows,  SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Dialogs,
  Forms, Messages, ShellAPI,
  LifeGen, LifeCel, LifeUtil, LifeConst, Snapshot, ActiveX;

type
  TDropFileEvent = procedure(Sender: TObject; var Msg: TWMDropFiles) of object;

  TMouseDown = (mdNone, mdLeft, mdRight, mdRightDrag, mdDragDrop, mdTorusResize);

  TLifeBox = class;
  TDataObject = class;

  ILife32CutOut = interface(IUnknown)
    ['{09871F80-94EF-11D1-AAE6-E27B60467027}']
    function GetLifeBoxClipRect: TRect; stdcall;
    function GetCutOut: TUniverse; stdcall;
    function IsSpecialDrag: WordBool; stdcall;
    function SourceID: integer; stdcall;
  end;

  TDropTarget = class(TInterfacedObject, IDropTarget)
  private
    MyLifeBox: TLifeBox;
    MyCutOut: TUniverse;
    CanAccept: Boolean;
    FKeyPressed: byte;
    Target: TPoint;
    ClipRect: TRect;
    DrawRect: TRect;
    Oldpt: TPoint;
    IsSpecialDrag: boolean;
    function GetTarget(Cursorpt: TPoint): TPoint;
  protected
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    procedure SetKeyPressed(value: byte);
  public
    constructor Create(AParent: TLifeBox);
    destructor Destroy; override;
    property KeyPressed: byte read FKeyPressed write SetKeyPressed;
  end;

  TDropSource = class(TInterfacedObject, IDropSource)
  private
    MyLifeBox: TLifeBox;
    MyTarget: TDropTarget;
    FKey: longint;
    MouseButton: longint;
    KeyboardDelay: integer;
  protected
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  public
    constructor Create(AParent: TLifeBox; ATarget: TDropTarget);
    destructor Destroy; override;
    property Key: longint read FKey;
  end;

  TDataObject = class(TInterfacedObject, IDataObject, ILife32Cutout)
  private
    MyLifeBox: TLifeBox;
    CutOut: TUniverse;
  protected
    function GetLifeBoxClipRect: TRect; stdcall;
    function GetCutOut: TUniverse; stdcall;
    function IsSpecialDrag: WordBool; stdcall;
    function SourceID: integer; stdcall;
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  public
    constructor Create(AParent: TLifeBox; ACutOut: TUniverse);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  end;

  TChangePatternEvent = procedure(Sender: TObject; Change: integer) of object;
  TShowDragEvent = procedure(Sender: TObject; DragOffset: TPoint) of object;

  TCanPaint = (cpCanPaint, cpDialogShowing, cpDontPaint, cpInit);

  TLifeBox = class(TPanel)
  private
    SourceID: integer;
    FPixelsPerCel, PPC: integer;
    FIsPaused: boolean;
    FNegZoom: integer;
    FFillPercentage: integer;

    FDropOffset: TPoint;

    FCelColor: TColor;
    FBackColor: TColor;
    FGridColor: TColor;
    FGrid2Color: TColor;
    FSelRectColor: TColor;
    FZoomRectColor: TColor;
    FDragColor: TColor;
    FTorusColor: TColor;
    FWhiteDisplay: boolean;

    HandX, HandY: integer;
    DrawX,DrawY: integer;
    OldDrawX, OldDrawY: integer;
    FSelectionRect: TRect;
    FOrgSelectionRect: TRect;
    FSelectionVisible: boolean;

    CursorDirty: Boolean;
    viewChanged: boolean;
    Dragstate: Boolean;
    FGrid: boolean;
    FCanPaint: TCanPaint;
    FFrameDropTime: integer;
    FSmallScroll: integer;
    FBoldGridSpacing: integer;
    FHandScroll: integer;
    FIsLimited: boolean;

    FFreezeSelection: Boolean;
    FSelectionExcluded: Boolean;
    FCutOut: TUniverse;

    FRevision: integer;
    FPatternID: integer;
    FMostRecentPatternID: integer;

    FUniverse: TUniverse;
    FCopyUniverse: TUniverse;
    FWidthCels, FHeightCels: integer;

    FOnPaint: TNotifyEvent;
    FOnMustPause: TNotifyEvent;
    FOnChangePattern: TChangePatternEvent;
    FOnEditorModeChange: TNotifyEvent;
    FOnPasteModeChange: TNotifyEvent;
    FOnDropFiles: TDropFileEvent;
    FOnZoomChange: TNotifyEvent;
    FOnRuleChange: TRuleChangeEvent;
    FOnSaveProgress: TSaveProgressEvent;
    FOnShowDrag: TShowDragEvent;
    FAfterInfoChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FScrolling: Boolean;
    FPasteMode: TPasteMode;
    FEditorMode: integer;
    FDropTarget: TDropTarget;
    FDropSource: TDropSource;
    FDropData: TDataObject;
    FInSelection: Boolean;
    FScreenSaverActive: Boolean;
    FMyDC: HDC;

    function Display(ARect: TRect): boolean;
    procedure RedrawBackground(ARect: TRect);

    procedure KeepWithinBounds(var x,y: integer);

    procedure DrawSelRect;
    procedure EraseSelRect(ClearSel: boolean);
    procedure RedrawLifeRect(ARect: TRect);
    procedure CorrectSelRect(Shrink: boolean);
    procedure FakeLifeLine(x1,y1,x2,y2: integer);
    function ConvertColor(OldColor, WinColor: integer): integer;
    function GetLifeBoxClipRect: TRect;

    property WhiteDisplay: boolean read FWhiteDisplay write FWhiteDisplay;
   protected
    IsMouseDown: TMouseDown;

    procedure CreateWnd; override;
    //procedure Loaded; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWnd; override;

    procedure SetPatternID(Value: integer);
    procedure SetIsPaused(Value: Boolean);
    procedure SetUniverse(Value: TUniverse);
    procedure SetGrid(Value: Boolean);
    procedure SetEditorMode(Value: integer);
    procedure SetPasteMode(Value: TPasteMode);
    procedure SetScrolling(Value: Boolean);

    procedure SetBoldGridSpacing(Value: integer);
    procedure SetPixelsPerCel(Value: integer);
    function GetPixelsPerCel: integer;
    function GetIsCounting: boolean;

    procedure SetXScroll(value: integer);
    function GetXScroll: integer;
    procedure SetYScroll(value: integer);
    function GetYScroll: integer;
    procedure SetOnRuleChange(Value: TRuleChangeEvent);
    procedure SetOnSaveProgress(Value: TSaveProgressEvent);
    procedure SetAfterInfoChange(Value: TNotifyEvent);

    function IsCursorAtTorusEdge: boolean;
    procedure ShowTorusResizeCursor(Show: boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMChar(var Msg: TWMChar); message WM_Char;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
    procedure Paint; override;
    procedure Validate;
    procedure SetSelectionRect(value: TRect);
    procedure SetCanPaint(Value: TCanPaint);
    function GetGeneration: Integer;
    procedure SetGeneration(Value: integer);
    function GetFramedropTime: integer;

    function GetZoomRectColor: TColor;
    function GetSelRectColor: TColor;

    procedure SetCelColor(Value: TColor);
    procedure SetGridColor(Value: TColor);
    procedure SetGrid2Color(Value: TColor);
    procedure SetBackColor(Value: TColor);
    procedure SetDragColor(Value: TColor);
    procedure SetTorusColor(Value: TColor);
    procedure SetSelRectColor(Value: TColor);
    procedure SetZoomRectColor(Value: TColor);
    procedure SetScreenSaverActive(Value: Boolean);
    function IsScreenSaverStillActive: Boolean;
    procedure SetLimit(Value: TRect);
    function GetLimit: TRect;
    procedure SetIsLimited(Value: boolean);
    procedure SetTorusKind(Value: TTorusKind);
    procedure SetDeadEdges(Value: boolean);
    function GetTorusKind: TTorusKind;
    function GetDeadEdges: boolean;
  public
    oX,oY: integer;
    xorig, yorig: integer;   // universe location at the upper left of the viewing window

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Invalidate; override;
    procedure RefreshColors(DoRedraw: boolean);
    procedure RedrawCursorArea(x,y: integer);
    procedure DisplayCutOut(APos: TPoint; ACutOut: TUniverse);

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure MoveToFast(NewX,NewY: integer);
    procedure MoveTo(NewX,NewY: integer);
    function MoveByFast(Dx,Dy: integer): boolean;
    function MoveBy(Dx,Dy: integer): boolean;
    procedure CenterOnPattern(Redraw: Boolean);
    function CelsAcross: integer;
    function CelsDown: integer;
    procedure ClientToBox(var x,y: integer);
    procedure BoxToClient(var x,y: integer);
    procedure BoxToCelFast(var x,y: integer);
    procedure BoxToCel(var x,y: integer);
    procedure CelToBox(var x,y: integer);
    procedure ClientToCelFast(var x,y: integer);
    procedure ClientToCel(var x,y: integer);
    procedure CelToClient(var x,y: integer);
    function CelToClientRect(ARect: TRect): TRect;
    //procedure GetCelCount(OnReady: TNotifyEvent);

    procedure RedrawCel(x, y: integer);
    procedure DrawCel(x, y: integer; state: boolean);
    procedure DrawGrid(ARect: TRect);
    function IsCelOn(x, y: integer): boolean;
    procedure InvertCel(x, y: integer);
    procedure Clear(MoveIt, ClearDesc, Snapshot: boolean);
    procedure LoadFromFile(AFileName: string);
    procedure ChangeCel(x, y: integer; state: boolean);
    procedure ChangeDrawCel(x,y: integer; state: boolean);
    procedure DrawLine(x1,y1,x2,y2: integer; XorIt: Boolean);
    function UpdateAll: boolean;
    procedure RedrawAll;
    procedure Redraw;
    procedure Generate(Direction: Boolean);
    procedure RandomDot;

    procedure MirrorHorz;
    procedure MirrorVert;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;

    procedure DrawBox;
    procedure FillRandom;
    procedure FillBlack;
    procedure InvertSelection;

    procedure ZoomToSelection(MaxFit: integer);
    procedure ZoomToFit(Redraw: Boolean; MaxFit: integer);
    procedure SelectAll(Redraw: Boolean);
    procedure CutSelection;
    procedure CopySelection;
    procedure PasteSelection;
    procedure ClearSelection;
    procedure ClearOutsideSelection;

    procedure MirrorSelHorz;
    procedure MirrorSelVert;
    procedure RotateSel90;
    procedure RotateSel180;
    procedure RotateSel270;

    procedure InsertShape(AShape: TUniverse);
    function GetCutout: TUniverse;
    procedure SaveShape(AShape: TUniverse; AFilename: string; FileFormat: integer);
    //Saves the entire universe to file.
    procedure SaveToFile(AFilename: string; FileFormat: integer; IncludeTorusData: boolean);

    procedure DoPaint(Sender: TObject);
    function EnableScreen: boolean;
    function CanPlay: Boolean;
    function IsEmpty: boolean;

    procedure ClearOutsideTorus;
    procedure ExcludeSelection;
    procedure IncludeSelection;
    procedure CancelSelection;
    procedure HideSelection;
    procedure ShowSelection;
    function MakeSnapshotCopy: TSnapshot;
    function MakeSnapshotDummy: TSnapshot;
    procedure RewindToSnapshot(ASnapshot: TSnapshot);
    procedure RewindToSnapshotSilent(ASnapshot: TSnapshot);
    function NewUniverse: TUniverse;

    procedure DoAutoScroll(APoint: TPoint; MoveCursor: Boolean);
    procedure DrawDropRect(ARect: TRect);
    procedure DrawDropTarget(x,y: integer); //x,y in cell coordinates.

    property DropOffset: TPoint read FDropOffset write FDropOffset;
    property FreezeSelection: Boolean read FFreezeSelection write FFreezeSelection;
    property PatternID: integer read FPatternID write SetPatternID;
    property MostRecentPatternID: integer read FMostRecentPatternID;
    property Revision: integer read FRevision write FRevision;
    property SelectionRect: TRect read FSelectionRect write SetSelectionRect;
    property OrgSelectionRect: TRect read FOrgSelectionRect;
    property SelectionVisible: boolean read FSelectionVisible;
    property FillPercentage: integer read FFillPercentage write FFillPercentage;
    property IsPaused: Boolean read FIsPaused write SetIsPaused;
    property CanPaint: TCanPaint read FCanPaint write SetCanPaint;
    property Scrolling: Boolean read FScrolling write SetScrolling;
    property Universe: TUniverse read FUniverse write SetUniverse;
    property PasteMode: TPasteMode read FPasteMode write SetPasteMode;
    property EditorMode: integer read FEditorMode write SetEditorMode;
    property Generation: integer read GetGeneration write SetGeneration;
    property XScroll: integer read GetXScroll write SetXScroll;
    property YScroll: integer read GetYScroll write SetYScroll;
    property SmallScroll: integer read FSmallScroll write FSmallScroll default 2;
    property HandScroll: integer read FHandScroll write FHandScroll default 2;
    property Canvas;
    property CelColor: TColor read FCelColor write SetCelColor;
    property GridColor: TColor read FGridColor write SetGridColor;
    property Grid2Color: TColor read FGrid2Color write SetGrid2Color;
    property BackColor: TColor read FBackColor write SetBackColor;
    property SelRectColor: TColor read GetSelRectColor write SetSelRectColor;
    property ZoomRectColor: TColor read GetZoomRectColor write SetZoomRectColor;
    property DragColor: TColor read FDragColor write SetDragColor;
    property TorusColor: TColor read FTorusColor write SetTorusColor;
    property PixelsPerCel: integer read GetPixelsPerCel write SetPixelsPerCel;
    property FrameDropTime: integer read GetFrameDropTime write FFrameDropTime;
    property Grid: Boolean read FGrid write SetGrid default false;
    property BoldGridSpacing: integer read FBoldGridSpacing write SetBoldGridSpacing;
    property ScreenSaverActive: Boolean read FScreenSaverActive write
             SetScreenSaverActive;
    property Limit: TRect read GetLimit write SetLimit;
    property IsLimited: boolean read FIsLimited write SetIsLimited;
    property TorusKind: TTorusKind read GetTorusKind write SetTorusKind;
    property DeadEdges: boolean read GetDeadEdges write SetDeadEdges;
    property IsCounting: boolean read GetIsCounting;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnMustPause: TNotifyEvent read FOnMustPause write FOnMustPause;
    property OnChangePattern: TChangePatternEvent read FOnChangePattern
             write FOnChangePattern;
    property OnEditorModeChange: TNotifyEvent read FOnEditorModeChange
             write FOnEditorModeChange;
    property OnPasteModeChange: TNotifyEvent read FOnPasteModeChange
             write FOnPasteModeChange;
    property OnRuleChange: TRuleChangeEvent read FOnRuleChange write SetOnRuleChange;
    property OnSaveProgress: TSaveProgressEvent read FOnSaveProgress write SetOnSaveProgress;
    property OnDropFiles: TDropFileEvent read FOnDropFiles write FOnDropFiles;
    property OnZoomChange: TNotifyEvent read FOnZoomChange write FOnZoomChange;
    property OnShowDrag: TShowDragEvent read FOnShowDrag write FOnShowDrag;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property AfterInfoChange: TNotifyEvent read FAfterInfoChange write SetAfterInfoChange;
    property OnKeyPress;
    property OnResize;
  end;

  procedure Register;
  procedure DisplayChange(BitsPerPixel,Width,Height: integer);


implementation

uses
  System.Types, System.UITypes;

const
  IsOleActive: boolean = false;


procedure Register;
begin
  RegisterComponents('Johan', [TLifeBox]);
end;

procedure SwapPoints(var a,b: integer);
var
  Swapper: integer;
begin
  Swapper:= a;
  a:= b;
  b:= Swapper;
end;

constructor TDropSource.Create(AParent: TLifeBox; ATarget: TDropTarget);
begin
  inherited Create;
  MyLifeBox:= AParent;
  MyTarget:= ATarget;
  MouseButton:= 0;
  //SystemParametersInfo(SPI_GETKEYBOARDDELAY,0,@KeyboardDelay,0)
  KeyboardDelay:= 300;
end;

destructor TDropSource.Destroy;
begin
  while (RefCount > 1) do _Release;
  inherited Destroy;
end;


function TDropSource.QueryContinueDrag(fEscapePressed: BOOL;
                                       grfKeyState: Longint): HResult;
type
  TKeys = set of byte;
var
  APos: TPoint;
  Offset: integer;
  Keys: TKeys;
  CurrentTickCount: integer;
  EnterPressed: Boolean;
  KeyState: TKeyboardstate;
const
  TimeTillRepeating: integer = 0;

  procedure CheckKeyBoard;
  begin
    GetKeyBoardState(KeyState);

    if (KeyState[vk_Left] and $fe) <> 0 then Keys:= Keys + [vk_Left];
    if (KeyState[vk_Right] and $fe) <> 0 then Keys:= Keys + [vk_Right];
    if (KeyState[vk_Down] and $fe) <> 0 then Keys:= Keys + [vk_Down];
    if (KeyState[vk_Up] and $fe) <> 0 then Keys:= Keys + [vk_Up];
    if (KeyState[vk_numpad1] and $fe) <> 0 then Keys:= Keys + [vk_Left,vk_Down];
    if (KeyState[vk_numpad2] and $fe) <> 0 then Keys:= Keys + [vk_Down];
    if (KeyState[vk_numpad3] and $fe) <> 0 then Keys:= Keys + [vk_Down,vk_Right];
    if (KeyState[vk_numpad4] and $fe) <> 0 then Keys:= Keys + [vk_left];
    if (KeyState[vk_numpad6] and $fe) <> 0 then Keys:= Keys + [vk_Right];
    if (KeyState[vk_numpad7] and $fe) <> 0 then Keys:= Keys + [vk_up,vk_Left];
    if (KeyState[vk_numpad8] and $fe) <> 0 then Keys:= Keys + [vk_up];
    if (KeyState[vk_numpad9] and $fe) <> 0 then Keys:= Keys + [vk_up,vk_Right];
    //Store the keys the droptarget should handle in FKey.
    //Dummy:= GetAsyncKeyState(vk_X);
    if (KeyState[vk_X] and $fe) <> 0 then fKey:= vk_x;
    if (KeyState[vk_y] and $fe) <> 0 then fKey:= vk_y;
    if (KeyState[vk_1] and $fe) <> 0 then fKey:= vk_1;
    if (KeyState[vk_2] and $fe) <> 0 then fKey:= vk_2;
    if (KeyState[vk_L] and $fe) <> 0 then fKey:= vk_2;
    if (KeyState[vk_9] and $fe) <> 0 then fKey:= vk_9;
    if (KeyState[vk_R] and $fe) <> 0 then fKey:= vk_9;


    if FKey <> 0 then begin
      if assigned(MyTarget) then MyTarget.KeyPressed:= FKey;
      FKey:= 0;
    end;
  end;

begin
  GetCursorPos(APos);
  //Check the enter key. if it is pressed, then drop.
  Enterpressed:= ((GetASyncKeyState(vk_Return) and $FFFE) <> 0);
  //Now check the cursor keys.
  Keys:= [];
  CheckKeyboard;
  if (Keys <> []) then begin
    CurrentTickCount:= MyGetTickCount;
    if (TimeTillRepeating = 0) or (TimeTillRepeating <= CurrentTickCount) then begin
      if TimeTillRepeating = 0 then
        TimeTillRepeating:= CurrentTickCount + KeyboardDelay;
      Offset:= MyLifeBox.FPixelsPerCel;
      if (vk_Right in Keys) then Inc(APos.x,Offset);
      if (vk_Left in Keys) then Dec(APos.x,Offset);
      if (vk_Down in Keys) then Inc(APos.y,Offset);
      if (vk_Up in Keys) then Dec(APos.y,Offset);
      SetCursorPos(APos.x,APos.y);
    end; {if}
  end {if}
  else TimeTillRepeating:= 0;
  Result:= S_OK;
  //if both mousebuttons are pressed (shord click), cancel the drag.
  if ((grfKeyState and (MK_LButton or MK_RButton)) =
     (MK_LButton or MK_RButton)) then
    Result:= DRAGDROP_S_CANCEL
  //also if <esc> is pressed, cancel.
  else if fEscapePressed then Result:= DRAGDROP_S_CANCEL;

  //if the button released is the same one that started the drag
  //then drop the bomb.
  if Result = S_OK then begin
    if ((MouseButton and MK_LButton) = MK_LButton) and
       ((grfKeyState and MK_LButton) <> MK_LButton) then
      Result:= DRAGDROP_S_DROP
    else if ((MouseButton and MK_RButton) = MK_RButton) and
            ((grfKeyState and MK_RButton) <> MK_RButton) then
      Result:= DRAGDROP_S_DROP
    else if EnterPressed then Result:= DRAGDROP_S_DROP;
  end;
  if Result = S_OK then MouseButton:= grfKeyState
  else MouseButton:= 0;
end;


function TDropSource.GiveFeedback(dwEffect: Longint): HResult;
begin
  //This is a bit of a hack, more advanced stuff will follow.
  Result:= DRAGDROP_S_USEDEFAULTCURSORS;
end;


{IDataObject}
constructor TDataObject.Create(AParent: TLifeBox; ACutOut: TUniverse);
begin
  inherited Create;
  MyLifeBox:= AParent;
  CutOut:= ACutOut;
  CutOut.AddRef;
end;

procedure TDataObject.BeforeDestruction;
begin
  {do nothing}
end;

destructor TDataObject.Destroy;
begin
  CutOut.Release;
  inherited Destroy;
end;

function TDataObject.GetLifeBoxClipRect: TRect;
begin
  if Assigned(CutOut) then Result:= CutOut.ClipRect
  else Result:= Rect(0,0,0,0);
end;

function TDataObject.GetCutOut: TUniverse;
begin
  Result:= CutOut;
end;

function TDataObject.IsSpecialDrag: WordBool;
begin
  Result:= false;
end;

function TDataObject.SourceID: integer;
begin
  Result:= integer(MyLifeBox.SourceID);
end;

{IDataObject}
function TDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult;
var
  AText: TStringList;
  ABitmap: TBitmap;
  TempText: PChar;
  Size: integer;
  Dest: PChar;
begin
  Result:= S_OK;
  AText:= nil;
  ABitmap:= nil;
  Medium.unkForRelease:= nil;
  with FormatetcIn do begin {FormatETC}
    case cfFormat of
      cf_Text, CF_OEMText, cf_Locale: begin
        AText:= CutOut.SaveToStringList(smDefault,false);
      end;
      cf_Bitmap, CF_DIB: begin
        ABitmap:= CutOut.SaveToBitmap;
        if cfFormat = cf_DIB then ABitmap.HandleType:= bmDIB
        else ABitmap.HandleType:= bmDDB;
      end;
      else Result:= E_INVALIDARG;
    end; {case}
    if Result = S_OK then begin
      if ((TYMED and TYMED_HGlobal) = TYMED_HGlobal) then case cfFormat of
        cf_Text, cf_OEMText, cf_Locale: begin
          TempText:= AText.GetText; //HGlobal can be a pointer.
          Size:= StrLen(TempText);
          if Medium.HGlobal = 0 then Medium.HGlobal:= GlobalAlloc(GHnd, Size+1);
          if GlobalSize(Medium.HGlobal) <= Size then Result:= STG_E_MEDIUMFULL
          else try
            Dest:= GlobalLock(Medium.hGlobal);
            CopyMemory(Dest,TempText,Size+1);
            Medium.tymed:= TYMED_HGlobal;
            finally GlobalUnLock(Medium.hGlobal);
          end; {else try}
        end; {cf_Text..}
        cf_Bitmap, cf_DIB: Result:= E_INVALIDARG;
      end {if TYMED_HGlobal}
      else if ((TYMED and TYMED_GDI) = TYMED_GDI) then case cfFormat of
        cf_Text, cf_OEMText, cf_Locale: Result:= E_INVALIDARG;
        cf_Bitmap, cf_DIB: begin
          Medium.hBitmap:= ABitmap.Handle;
          Medium.tymed:= tymed_GDI;
        end; {cf_Bitmap..}
      end {else}
      else Result:= E_INVALIDARG;
    end; {if}
  end; {with}
  AText.Free;
  ABitmap.Free;
end;

function TDataObject.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult;
begin
  Result:= GetData(formatetc, medium);
end;

function TDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  with Formatetc do begin
    case cfFormat of
      cf_Text, cf_Bitmap, CF_OEMTEXT, CF_DIB, CF_LOCALE: Result:= S_OK;
      else Result:= E_INVALIDARG;
    end; {case}
    if (Result = S_OK) then begin
      if ((TYMED and TYMED_HGlobal) = TYMED_HGlobal) then {OK}
      else if ((TYMED and TYMED_GDI) = TYMED_GDI) then begin
        if ((cfFormat <> CF_Bitmap) and (cfFormat <> cf_DIB)) then
          Result:= E_INVALIDARG
      end
      else Result:= E_INVALIDARG;
    end; {if}
  end; {with}
end;


function TDataObject.SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult;
begin
  Result:= E_NOTIMPL;
end;

function TDataObject.EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult;
begin
  //Let the OLE lib do the enumeration from the Registry.
  Result:= OLE_S_USEREG;
end;

function TDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult;
begin
  formatetcOut:= PFormatETC(nil)^;
  Result:= DATA_S_SAMEFORMATETC;
end;

function TDataObject.DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result:= OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.DUnadvise(dwConnection: Longint): HResult;
begin
  Result:= OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result:= OLE_E_ADVISENOTSUPPORTED;
end;

constructor TDropTarget.Create(AParent: TLifeBox);
begin
  inherited Create;
  MyLifeBox:= AParent;
  CanAccept:= false;
end;

destructor TDropTarget.Destroy;
begin
  while (RefCount > 1) do _Release;
  inherited Destroy;
end;

function TDropTarget.GetTarget(Cursorpt: TPoint): TPoint;
begin
  Result:= Cursorpt;
  Result:= MyLifeBox.ScreenToClient(Result);
  with Result do begin
    MyLifeBox.ClientToCel(x,y);
    with MyLifeBox do begin
      Inc(x,DropOffset.x);
      Inc(y,DropOffset.y);
    end; {with}
  end; {with}
end;


function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult;
var
  FormatETC: TFormatETC;
  Life32CutOut: ILife32CutOut;
begin
  with FormatETC do begin
    cfFormat:= cf_text;
    ptd:= nil;
    dwAspect:= DVASPECT_CONTENT;
    lindex:= -1;
    tymed:= TYMED_HGLOBAL;
  end; {with}
  CanAccept:= dataObj.QueryGetData(FormatETC) = S_OK;
  if CanAccept then with Target do begin
    Target:= GetTarget(pt);
    dataObj.QueryInterface(ILife32CutOut,Life32CutOut);
    if Assigned(Life32CutOut) then begin
      //Life32CutOut:= dataObj as ILife32CutOut;
      if ((grfKeyState and MK_Control) = MK_Control) then dwEffect:= DROPEFFECT_COPY
      else dwEffect:= DROPEFFECT_MOVE;
      IsSpecialDrag:= Life32CutOut.IsSpecialDrag or (Life32CutOut.SourceID <> MyLifeBox.SourceID);
      if IsSpecialDrag then dwEffect:= DROPEFFECT_COPY;
      ClipRect:= Life32CutOut.GetLifeBoxClipRect;
      with ClipRect do OffsetRect(ClipRect,-Left, -Top);
      MyCutOut:= Life32CutOut.GetCutOut;
      DrawRect:= ClipRect;
      OffsetRect(DrawRect,x,y);
      MyLifeBox.DrawDropRect(DrawRect);
      MyLifeBox.DisplayCutOut(Target,MyCutOut);
    end
    else begin
      if ((grfKeyState and MK_Shift) = MK_Shift) then dwEffect:= DROPEFFECT_MOVE
      else dwEffect:= DROPEFFECT_COPY;
      MyLifeBox.DrawDropTarget(x,y);
      ClipRect:= Rect(0,0,0,0);
      DrawRect:= ClipRect;
    end; {else}
  end {if}
  else dwEffect:= DROPEFFECT_NONE;
  OldPt:= Target;
  Result:= S_OK;
end;

function TDropTarget.DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult;
type
  TKeys = set of byte;
var
  Keys: TKeys;

  procedure ManipulateCutOut;
  begin
    if (KeyPressed = vk_x) then MyCutOut.FlipX
    else if (KeyPressed = vk_y) then MyCutOut.FlipY
    else if (KeyPressed = vk_1) then MyCutOut.Rotate180
    else if (KeyPressed = vk_2) then MyCutOut.Rotate270
    else if (KeyPressed = vk_9) then MyCutOut.Rotate90;
  end;

begin
  Keys:= [];
  if CanAccept then with Target do begin
    //Second draw clears, not while playing tough Mmm.
    if not IsMyRectEmpty(ClipRect) then begin
      Target:= GetTarget(pt);
      if (Target.x <> OldPt.x) or (Target.y <> OldPt.y) or (KeyPressed <> 0) then begin
        MyLifeBox.DrawDropRect(DrawRect); //Erase old dropRect;
        MyLifeBox.DisplayCutOut(OldPt,MyCutOut);
        ManipulateCutOut;
        MyLifeBox.DoAutoScroll(pt,false);
        DrawRect:= MyCutOut.ClipRect;
        OffsetRect(DrawRect,-DrawRect.Left, -DrawRect.Top);
        OffsetRect(DrawRect,x,y);
        //with MyLifeBox.DropOffset do OffsetRect(DrawRect,x,y);
        oldPt:= Point(x,y);
        MyLifeBox.DisplayCutOut(OldPt,MyCutOut);
        MyLifeBox.DrawDropRect(DrawRect);
      end; {if}
      if ((grfKeyState and MK_Control) = MK_Control) then
        dwEffect:= DROPEFFECT_COPY
      else dwEffect:= DROPEFFECT_MOVE;
      if IsSpecialDrag then begin
        dwEffect:= DROPEFFECT_COPY;
      end;

    end {if}
    else begin
      if ((grfKeyState and MK_Shift) = MK_Shift) then
        dwEffect:= DROPEFFECT_MOVE
      else dwEffect:= DROPEFFECT_COPY;
      Target:= GetTarget(pt);
      if (Target.x <> OldPt.x) or (Target.y <> OldPt.y) then begin
        MyLifeBox.DrawDropTarget(OldPt.x,OldPt.y);
        MyLifeBox.DrawDropTarget(x,y);
      end; {if}
    end; {else}
  end {if}
  else dwEffect:= DROPEFFECT_NONE;
  OldPt:= Target;
  Result:= S_OK;
end;

function TDropTarget.DragLeave: HResult;
begin
  CanAccept:= false;
  MyLifeBox.DrawDropRect(DrawRect); //Erase old dropRect;
  if Assigned(MyCutOut) then begin
    MyLifeBox.DisplayCutOut(OldPt,MyCutOut);
    MyCutOut:= nil;
  end;
  Result:= S_OK;
  If Assigned(MyLifeBox.OnEndDrag) then MyLifeBox.OnEndDrag(MyLifeBox,MyLifeBox,0,0);
end;

function TDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint;
                          pt: TPoint; var dwEffect: Longint): HResult;
const
  MK_Alt = 32;
var
  Life32CutOut: ILife32CutOut;
  Status: HResult;
  DataSource: PChar;
  LifeLines: TStringList;
  CutOut: TUniverse;
  TryRect: TRect;
  CanPaste: Boolean;
  x,y: integer;

  function GetCutoutViaOLE: HResult;
  var
    FormatETC: TFormatETC;
    StgMedium: TStgMedium;
  begin
    if ((grfKeyState and MK_Shift) = MK_Shift) then dwEffect:= DROPEFFECT_MOVE
    else dwEffect:= DROPEFFECT_COPY;


    with FormatETC do begin
      cfFormat:= cf_text;
      ptd:= nil;
      dwAspect:= DVASPECT_CONTENT;
      lindex:= -1;
      tymed:= TYMED_HGLOBAL;
    end; {with}
    status:= dataObj.QueryGetData(FormatETC);
    if status = S_OK then begin
      status:= dataObj.GetData(FormatEtc,StgMedium);
      if Status = S_OK then with StgMedium do try
        DataSource:= GlobalLock(hGlobal);
        if Assigned(DataSource) then try
          LifeLines:= TStringList.Create;
          LifeLines.SetText(DataSource);
          CutOut:= TUniverse.Create(MyLifeBox.Universe.RuleString,
                                    MyLifeBox.Universe.Neighborhood);
          CutOut.OnRuleChange:= MyLifeBox.Universe.OnRuleChange;
          CutOut.LoadFromStringList(LifeLines);
          finally GlobalUnlock(hGlobal);
        end; {try}
      finally ReleaseStgMedium(StgMedium);
      end; {with}
    end;
    Result:= Status;
  end; {GetCutoutViaOLE}

begin
  try
    Status:= -1; //negative values indicate failure
    Life32CutOut:= dataObj as ILife32CutOut;
    if ((grfKeyState and MK_Control) = MK_Control) then dwEffect:= DROPEFFECT_COPY
    else dwEffect:= DROPEFFECT_MOVE;
    //if we use the scrapbook drag, always copy.
    if IsSpecialDrag then dwEffect:= DROPEFFECT_COPY;
    CutOut:= Life32CutOut.GetCutOut;
    Status:= S_OK;
  except
    Status:= GetCutoutViaOLE;
  end; {try}
  if Status = S_OK then begin
    if (grfKeyState and MK_Alt) = MK_Alt then MyLifeBox.Clear(true,false,true);
    pt:= MyLifeBox.ScreenToClient(pt);
    x:= pt.x; y:= pt.y;

    MyLifeBox.ClientToCel(x,y);
    Inc(x,MyLifeBox.DropOffset.x);
    Inc(y,MyLifeBox.DropOffset.y);
    CanPaste:= true;
    if (MyLifeBox.PasteMode = lpmError) then with CutOut.ClipRect do begin
      TryRect:= Rect(x,y,x+Right-Left,y+Bottom-Top);
      MyLifeBox.Universe.ShrinkSelRect(TryRect);
      if not IsMyRectEmpty(TryRect) then CanPaste:= false;
    end; {if}
    if CanPaste then begin
      if dwEffect = DROPEFFECT_MOVE then begin
        MyLifeBox.ClearSelection;
        //MyLifeBox.Universe.FillRect(MyLifeBox.FSelectionRect, faClear);
      end; {if}
      with CutOut.ClipRect do    //!!!!!!!!!!!!!!@@@@@@@@@@@
        MyLifeBox.SelectionRect:= Rect(x,y,x+Abs(Right-Left),y+Abs(Bottom-Top));
      MyLifeBox.InsertShape(CutOut);
      if Assigned(MyLifeBox.OnDragDrop) then MyLifeBox.OnDragDrop(MyLifeBox,MyLifeBox,0,0);
    end; {if CanPaste}
    MyLifeBox.RedrawAll; //Also draws selectionRect.
  end; {if}
  Result:= Status;
end;

procedure TDropTarget.SetKeyPressed(value: byte);
var
  CursorPos: TPoint;
  Dummy: longint;
begin
  FKeyPressed:= value;
  GetCursorPos(CursorPos);
  DragOver(0,CursorPos,dummy);
  FKeyPressed:= 0;
end;

constructor TLifeBox.Create(AOwner: TComponent);
var
  Ole_Error: HResult;
begin
  inherited Create(AOwner);
  SourceID:= Random(MaxInt);
  if not(csDesigning in ComponentState) then begin
    Application.Tag:= 1; {enable DDraw}
    if not IsOLEActive then begin
      Ole_error:= OleInitialize(nil);
      IsOleActive:= (Ole_error = S_OK) or (Ole_error = S_False);
      if not(IsOleActive) then ShowMessage(IntToStr(Ole_Error));
    end; {if}
  end; {if}
  Parent:= TWinControl(AOwner);
  FCanPaint:= cpInit;
  //Generation:= 0;  objects vars are automatically set to 0.
  //FFrameDropTime:= 0; //no framedropping
  FOnPaint:= nil;
  FPixelsPerCel:= 2;
  Fgrid:= false;
  FPasteMode:= lpmOr;
  FScrolling:= false;
  FEditorMode:= emSelect;
  FSmallScroll:= 2;
  FHandScroll:= 2;
  FBoldGridSpacing:= DefaultBoldGridSpacing;
  FUniverse:= TUniverse.Create('',nbDefault);
  Universe.OnRuleChange:= OnRuleChange;
  CursorDirty:= true;
  ControlStyle:= ControlStyle + [csOpaque];
  if IsOleActive then FDropTarget:= TDropTarget.Create(Self);
  if IsOleActive then FDropSource:= TDropSource.Create(Self, FDropTarget);
  OnPaint:= DoPaint;

  FillPercentage:= 33;

  Canvas.Brush.Color:= CelColor;
  Color:= BackColor;
  Caption:= '';
  //BevelInner:= bvNone;
  //BevelOuter:= bvNone;

  oY:= 1;
  oX:= 1;
  //FNegZoom:= 0;

  FCelColor:= DefaultCelColor;
  FBackColor:= DefaultBackColor;
  FGridColor:= DefaultGridColor;
  FGrid2Color:= DefaultGrid2Color;
  FSelRectColor:= DefaultSelRectColor;
  FZoomRectColor:= DefaultZoomRectColor;
  FDragColor:= DefaultDragColor;
  FTorusColor:= DefaultTorusColor;
  IsMouseDown:= mdNone;
  //SelectionRect:= Rect(0,0,0,0);
  InitDDraw;
end;


destructor TLifeBox.Destroy;
begin
  if Assigned(FUniverse) then FUniverse.Release;
  if IsOleActive then begin
    FDropSource.Free;
    //FDropTarget.Free; The revokedragdrop call will destroy the droptarget
    //so we don't need to do that here.
  end;
  inherited Destroy;
end;

procedure TLifeBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style:= Params.WindowClass.Style
    or CS_OwnDC and not CS_HRedraw and not CS_VRedraw;
  Params.WinClassName:= 'Life32LifeBox';
end;

procedure TLifeBox.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
end;

procedure TLifeBox.CreateWnd;
begin
  inherited CreateWnd;
  if Parent <> nil then begin
    if (not (csDesigning in componentState)) and
       (not (csDestroying in ComponentState)) then begin
      if IsOleActive then RegisterDragDrop(Handle, FDropTarget);
      DragAcceptFiles(Handle,true);
    end;
  end;
  FMyDC:= GetDC(Handle); 
  RefreshColors(NoRedraw);
end;

procedure TLifeBox.DestroyWnd;
begin
  try
    if IsOleActive then try
      RevokeDragDrop(Handle);
      except {ignore}
    end; {try}
    finally
    ReleaseDC(Handle,FMyDC);
  end;
  inherited DestroyWnd;
end;

//procedure TLifeBox.Loaded;  //Hack ??
//begin
//  inherited Loaded;
//end;

procedure TLifeBox.DoPaint(Sender: TObject);
begin
  if (CanPaint = cpCanPaint) then begin
    WhiteDisplay:= true;
    Redraw;
  end;
end;

procedure TLifeBox.Paint;
const
  NoRedraw = false;
begin
  case CanPaint of
    cpCanPaint: begin
      RefreshColors(NoRedraw);
      //inherited Paint;
      //DoPaint(Self);
      if Assigned(FOnPaint) then FOnPaint(Self)
      else DoPaint(Self);
      Validate;
    end; {cpCanPaint}
    cpDialogShowing: begin
      FCanPaint:= cpCanPaint; //remember DDraw is disabled now.
      Paint;
      FCanPaint:= cpDialogShowing;
    end;
    else Validate;
  end; {case}
end;

procedure TLifeBox.Validate;
begin
  ValidateRect(Handle,nil);
end;

procedure TLifeBox.Invalidate;
begin
  if (CanPaint = cpCanPaint) then inherited Invalidate;
end;

procedure TLifeBox.Init;
begin
  MoveTo(0, 0);
  Canvas.Brush.Color:= CelColor;
  FWidthCels:= (Width div FPixelsPerCel) shl FNegZoom;
  FHeightCels:= (Height div FPixelsPerCel) shl FNegZoom;
end;

var
  KBLeftButtonDown: boolean = false;

procedure TLifeBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Diagonal: integer;
  CursorPos: TPoint;
  sms: integer;
  ClearKey: boolean;
const
  OldCursorPos: TPoint = (x:0;y:0);
begin
  ClearKey:= true;
  if FWidthCels > FHeightCels then Diagonal:= FHeightCels div 3
  else Diagonal:= FWidthCels div 3;
  sms:= SmallScroll;
  if (PixelsPerCel < 1) then sms:= (sms * - PixelsPerCel);
  case key of
    //if escape is pressed while dragging, simulate a chord-click.
    //This is not a hack, as the chord click is documented.
    vk_Escape:begin
      if (IsMouseDown = mdRightDrag) then MouseUp(mbLeft,Shift,0,0)
      else if (IsMouseDown = mdLeft) then MouseUp(mbRight,Shift,0,0);
    end; {vk_Escape} //@@@@@ add code to cancel drawing a line.
    vk_numpad1..vk_numpad9,vk_Left, vk_Right, vk_Up, vk_Down: begin
      case key of
        vk_numpad9: MoveBy((Diagonal),-(Diagonal));
        vk_numpad8: MoveBy(0,-(FHeightCels div 3));
        vk_numpad7: MoveBy(-(Diagonal),-(Diagonal));
        vk_numpad6: MoveBy((FWidthCels div 3),0);
        vk_numpad5: CenterOnPattern(true);
        vk_numpad4: MoveBy(-(FWidthCels div 3),0);
        vk_numpad3: MoveBy((Diagonal),(Diagonal));
        vk_numpad2: MoveBy(0,(FHeightCels div 3));
        vk_numpad1: MoveBy(-(Diagonal),(Diagonal));
        vk_Left: MoveBy(-sms,0);
        vk_Right: MoveBy(sms,0);
        vk_up: MoveBy(0,-sms);
        vk_Down: MoveBy(0,sms);
      end; {case}
      if KBLeftButtonDown then begin
        GetCursorPos(CursorPos);
        CursorPos:= ScreenToClient(CursorPos);
        SendMessage(Self.Handle,wm_MouseMOve,0,
        Integer(SmallPoint(CursorPos.x,CursorPos.y)));
      end; {if}
    end; {vk_numpad1..vknumpad9}
    vk_numpad0, vk_Insert: begin
      GetCursorPos(CursorPos);
      CursorPos:= ScreenToClient(CursorPos);
      if {(OldCursorPos.x <> CursorPos.x) or (OldCursorPos.y <> CursorPos.y)}true then begin
        if not KBLeftButtonDown then
          //MouseDown(mbLeft,Shift,CursorPos.x,CursorPos.y)
          SendMessage(Self.Handle,wm_LButtonDown,{Integer(Shift)}0,Integer(SmallPoint(CursorPos.x,CursorPos.y)));

        //else //MouseMove(Shift,CursorPos.x,CursorPos.y);
        //SendMessage(Self.Handle,wm_MouseMove,0{integer(shift)},Integer(SmallPoint(CursorPos.x,CursorPos.y)));
        KBLeftButtonDown:= true;
      end; {if}
      OldCursorPos:= CursorPos;
      //Key:= 0;
    end; {vk_numpad0}

    vk_1,vk_2,vk_9,vk_X, vk_Y: begin
      if not(ssAlt in Shift) and not (ssCtrl in Shift) and SelectionVisible then begin
        case Key of
          vk_1: RotateSel180;
          vk_2: RotateSel270;
          vk_9: RotateSel90;
          vk_X: MirrorSelHorz;
          vk_Y: MirrorSelVert;
        end; {case}
        FOrgSelectionRect:= Rect(0,0,0,0);
        RedrawAll;
      end {if}
      else ClearKey:= false;
    end; {1,2,9,x,y}
    else ClearKey:= false;
  end; {case}
  if ClearKey then Key:= 0;
  //inherited;
end;

procedure TLifeBox.KeyUp(var Key: Word; Shift: TShiftState);
var
  CursorPos: TPoint;
begin
  case key of
    vk_numpad0, vk_Insert: begin
      GetCursorPos(CursorPos);
      CursorPos:= ScreenToClient(CursorPos);
      MouseUp(mbLeft,Shift,CursorPos.x,CursorPos.y);
      KBLeftButtonDown:= false;
      //Key:= 0;
    end; {vk_numpad0}
  end; {case}
  //inherited;
end;

procedure TLifeBox.MoveToFast(NewX, NewY: integer);
begin
  //Warparound of universe;
  KeepWithInBounds(Newx,Newy);
  //SetPixelSize;

  xorig:= NewX-(((Width div FPixelsPerCel) shl FNegZoom) div 2);
  yorig:= NewY-(((Height div FPixelsPerCel) shl FNegZoom) div 2);
  Universe.Centerpoint:= Point(NewX, NewY);
end;

procedure TLifeBox.MoveTo(NewX, NewY: integer);
begin
  MoveToFast(NewX,NewY);
  //to avoid shadowimages, this does cause flicker *sigh*
  {if GetPixelsPerCel <= -2 then }WhiteDisplay:= true;
  Redraw;
end;

function TLifeBox.MoveByFast(Dx,Dy: integer): boolean;
var
  OldX, OldY: integer;
  UpdateRect: TRect;
  //DDColor: TColor;
  ZoomScroll: integer;
  ZoomMask: integer;
  CheckDX, CheckDY: integer;

begin
  Result:= false;
  if (Dx or Dy) <> 0 then begin

    //Make sure shift takes place with multiples of the zoomfactor
    //in negagive zooms.
    //Make sure the function rounds up, not down by adding the rounddown
    //factor -1 (that way round numbers will be increased just enough
    //to be rounded down to their previous value, and all other
    //values will be increased to the nearest round numbers.
    ZoomScroll:= (1 shl FNegZoom)-1;
    //masks off the lower bits, higher will always be one (-1 = $FFFFFFF).
    ZoomMask:= (-1 shl FNegZoom);
    //Masking negative numbers can only increase their absolute value,
    //so only affect positive numbers.
    if (Dx > 0) then Inc(Dx,ZoomScroll);
    Dx:= DX and ZoomMask;
    if (Dy > 0) then Inc(Dy,ZoomScroll);
    Dy:= Dy and ZoomMask;
    OldX:= XOrig+(((Width div FPixelsPerCel) shl FNegZoom) div 2);
    OldY:= YOrig+(((Height div FPixelsPerCel) shl FNegZoom) div 2);

    //Check to make sure torus-limitation does not do strange things to
    //the DX and DY.
    CheckDX:= OldX+Dx; CheckDY:= OldY+Dy;
    KeepWithInBounds(CheckDX, CheckDY);
    CheckDX:= CheckDX - OldX; CheckDY:= CheckDY - OldY;
    //ONLY if the signs are the same then scroll.
    if (CheckDX = DX) and (CheckDY = DY) then begin

      ScrollWindowEx(Handle,
                    -(Dx*FPixelsPerCel) div (1 shl FNegZoom),
                    -(Dy*FPixelsPerCel) div (1 shl FNegZoom),
                    nil,nil,0,@UpdateRect,{sw_erase or sw_invalidate}0);
      WhiteDisplay:= true;
      MoveToFast(OldX+Dx,OldY+Dy);
      Result:= true;
    end;
  end;
end;

function TLifeBox.MoveBy(Dx,Dy: integer): boolean;
var
  ReshowSel: Boolean;
begin
  Result:= false;
  if (Dx or Dy) <> 0 then begin
    ReshowSel:= SelectionVisible;
    CursorDirty:= true;
    if ReshowSel then HideSelection;
    Result:= MoveByFast(Dx,Dy);
    Redraw;
    if ReshowSel then ShowSelection;
  end;
end;


procedure TLifeBox.SetBoldGridSpacing(Value: integer);
begin
  if (Value <> FBoldGridSpacing) and (Value > 0) then begin
    FBoldGridSpacing:= Value;
    SetPixelsPerCel(PixelsPerCel); //force redrawing of grid.
  end; {if}
end;

procedure TLifeBox.SetPixelsPerCel(Value: integer);
var
  OldPixelSize: integer;
  OldNegZoom: integer;
begin
  //no test for same value because of grid.
  if Value = 0 then begin
    if PixelsPerCel > 0 then Value:= -1
    else Value:= 1;
  end; {if}
  if ((Value <= MaxZoom) and (Value >= MinZoom)) then begin
    OldPixelSize:= FPixelsPerCel;
    OldNegZoom:= FNegZoom;
    //if Value < 4 then Grid:= false;
    if Value < 0 then if EditorMode= emDraw then EditorMode:= emSelect;

    if Value > 0 then begin
      FPixelsPerCel:= Value;
      FNegZoom:= 0;
    end
    else begin
      FPixelsPerCel:= 1;
      FNegZoom:= Abs(Value);
    end; {else}
    if OldPixelSize = 0 then OldPixelSize:= FPixelsPerCel;
    //typecast Boole->byte, Pascal:boole:true always 1,
    //Pascal:boole:false always 0.
    PPC := FPixelsPerCel - Byte((FPixelsPerCel > 3) and (FGrid));

    Inc(Xorig,((((width div OldPixelSize) shl OldNegZoom) -
      ((Width div FPixelsPerCel) shl FNegZoom)) div 2));
    Inc(Yorig,((((height div OldPixelSize) shl OldNegZoom) -
      ((Height div FPixelsPerCel) shl FNegZoom)) div 2));

    LifeCel.SetCelSpace(FPixelsPerCel,FGrid);
    if Assigned(OnZoomChange) then OnZoomChange(Self)
    else RedrawAll;
  end; {if}
end;

function TLifeBox.GetPixelsPerCel: integer;
begin
  Result:= FPixelsPerCel;
  if FNegZoom > 0 then Result:= Result * -FNegZoom;
end;

function TLifeBox.GetIsCounting: boolean;
begin
  Result:= Universe.IsCounting;
end;

function TLifeBox.CelsAcross: integer;
begin
  Result:= (width div FPixelsPerCel) shl FNegZoom;
end;

function TLifeBox.CelsDown: integer;
begin
  Result:= (Height div FPixelsPerCel) shl FNegZoom;
end;

procedure TLifeBox.ClientToBox(var x,y: integer);
begin
  x:= ((x-oX) div FPixelsPerCel) shl FNegZoom;
  y:= ((y-oY) div FPixelsPerCel) shl FNegZoom;
end;

procedure TLifeBox.BoxToClient(var x,y: integer);
begin
  x:= ((x * FPixelsPerCel) div (1 shl FNegZoom)) + ox;
  y:= ((y * FPixelsPerCel) div (1 shl FNegZoom)) + oy;
end;

procedure TLifeBox.KeepWithinBounds(var x,y: integer);
var
  wx,wy: smallint;
  rx,ry: integer;
  //DisplayRect: TRect;
begin
  //this code assumes that $7ffff is +max and $80000 is -max.
  rx:= x and $0000000f;
  wx:= x shr 4; //- if near bound, + of not near bound.
  x:= wx; //sign extension should do the trick.
  x:= (x * 16) or rx; //put the pieces back together.

  ry:= y and $0000000f;
  wy:= y shr 4; //- if near bound, + of not near bound.
  y:= wy; //sign extension should do the trick.
  y:= (y * 16) or ry; //put the pieces back together.
  if Universe.IsLimited then begin
    with Universe.Limit do begin
      if (Universe.TorusKind = tk_All) or
         (Universe.TorusKind = tk_LeftRight) then begin
        if x < Left then x:= Left;
        if x > Right then x:= right;
      end;
      if (Universe.TorusKind = tk_All) or
         (Universe.TorusKind = tk_UpDown) then begin
        if y < top then y:= top;
        if y > bottom then y:= bottom;
      end;
    end; {with}
  end; {if}
end;

procedure TLifeBox.BoxToCelFast(var x,y: integer);
begin
  Inc(x,XOrig);
  Inc(y,YOrig);
end;

procedure TLifeBox.BoxToCel(var x,y: integer);
begin
  BoxToCelFast(x,y);
  KeepWithinBounds(x,y);
end;

procedure TLifeBox.CelToBox(var x,y: integer);
begin
  Dec(x,XOrig);
  Dec(y,YOrig);
end;

procedure TLifeBox.ClientToCelFast(var x,y: integer);
begin
  ClientToBox(x,y);
  BoxToCelFast(x,y);
end;

procedure TLifeBox.ClientToCel(var x,y: integer);
begin
  ClientToBox(x,y);
  BoxToCel(x,y);
end;

procedure TLifeBox.CelToClient(var x,y: integer);
begin
  CelToBox(x,y);
  BoxToClient(x,y);
end;

function TLifeBox.CelToClientRect(ARect: TRect): TRect;
begin
  Result:= ARect;
  with Result do begin
    CelToClient(Left,Top);
    CelToClient(Right,Bottom);
  end; {with}
end;

(*procedure TLifeBox.GetCelCount(OnReady: TNotifyEvent);
begin
  if Assigned(Universe) then begin
    FCountUniverse:= Universe;
    FCountUniverse.GetCelCount(OnReady);
  end;
end; (**)


procedure TLifeBox.RedrawCel(x, y: integer);
begin
  DrawCel(x,y, IsCelOn(x,y));
end;

procedure TLifeBox.DrawCel(x, y: integer; state: boolean);
var
  gx,gy: integer;
  OldColor: TColor;
begin
  gx := (((x-Xorig)*FPixelsPerCel) div (1 shl FNegZoom));
  gy := (((y-Yorig)*FPixelsPerCel) div (1 shl FNegZoom));

  if(state) then PatBlt(Canvas.Handle, Gx+oX, gy+oY, PPC, PPC, PATCOPY)
  else begin
    OldColor:= Canvas.Brush.Color;
    Canvas.Brush.Color:= BackColor;
    PatBlt(Canvas.Handle,gx+oX, gy+oY, PPC, PPC,PATCOPY);
    Canvas.Brush.Color:= OldColor;
  end;
end;

procedure TLifeBox.DrawGrid(ARect: TRect);
var
  i: integer;
  x,y,x1,y1: integer;
  GridRect: TRect;
  LineRect: TRect;
  ClipRect: TRect;
  Corr: TPoint;
  Corr2: TPoint;
  DDColor, DD2Color, TheColor: integer;
begin
  if IsLimited then begin
    GridRect:= Limit;
    Inc(GridRect.Right); Inc(GridRect.Bottom);
    CelToClient(GridRect.Left,GridRect.Top);
    CelToClient(GridRect.Right,GridRect.Bottom);
    if TorusKind = tk_LeftRight then with GridRect do begin
      Top:= ARect.Top; Bottom:= ARect.Bottom;
    end
    else if TorusKind = tk_UpDown then with GridRect do begin
      Left:= ARect.Left; Right:= ARect.Right;
    end;
    IntersectRect(ARect,GridRect,ARect);
  end; {if IsLimited}
  if (not (MyDDSurface.DirectDrawEnabled = DDFast)) and (not FScrolling) then begin
    IntersectRect(ARect,ARect,GetLifeBoxClipRect);
	  Canvas.Pen.Color:= GridColor;

	  if((PixelsPerCel > 3) and FGrid) then begin
      x1:= (ARect.Left div PixelsPerCel);
      Canvas.Pen.Color:= GridColor;
	    for i:= (ARect.Top div PixelsPerCel) to (ARect.Bottom div PixelsPerCel) do begin
        y:= (i*PixelsPerCel);
        y1:= y+1;
        ClientToCel(x1,y1);
        if (y1 mod FBoldGridSpacing) <> 0 then begin
          Canvas.MoveTo(ARect.Left, y);
          Canvas.LineTo(ARect.Right, y);
        end; {if}
	    end; {for i}
      y1:= (ARect.Top div PixelsPerCel);
	    for i:= (ARect.Left div PixelsPerCel) to (ARect.Right div PixelsPerCel) do begin
        x:= (i*PixelsPerCel);
        x1:= x+1;
        ClientToCel(x1,y1);
        if (x1 mod FBoldGridSpacing) = 0 then Canvas.Pen.Color:= Grid2Color
        else Canvas.Pen.Color:= GridColor;
	      Canvas.MoveTo(x, ARect.Top);
	      Canvas.LineTo(x, ARect.Bottom);
	    end; {for i}
      //Draw the horizontal 5th grid lines separatly, to avoid these
      //lines from being drawn over by vertical lines.
      x1:= (ARect.Left div PixelsPerCel);
      Canvas.Pen.Color:= Grid2Color;
      for i:= (ARect.Top div PixelsPerCel) to (ARect.Bottom div PixelsPerCel) do begin
        y:= (i*PixelsPerCel);
        y1:= y+1;
        ClientToCel(x1,y1);
        if (y1 mod FBoldGridSpacing) = 0 then begin
          Canvas.MoveTo(ARect.Left, y);
	        Canvas.LineTo(ARect.Right, y);
        end; {if}
	    end; {for i}
	  end; {if} (**)
  end {if}
  else if (MyDDSurface.DirectDrawEnabled = DDFast) and (not Scrolling) then begin
    DDColor:= MyDDSurface.GetDDNearestColor(GridColor);
    DD2Color:= MyDDSurface.GetDDNearestColor(Grid2Color);
	  Corr.X:= 0;
	  Corr.y:= 0;
	  Corr:= ClientToScreen(Corr);
    Corr2:= Corr;
	  Corr.X:= Corr.x mod PixelsPerCel;
	  Corr.Y:= Corr.Y mod PixelsPerCel;

	  ClipRect:= GetLifeBoxClipRect;
    IntersectRect(ClipRect,ClipRect,ARect);
	  with ClipRect do begin
	    TopLeft:= ClientToScreen(TopLeft);
	    BottomRight:= ClientToScreen(BottomRight);
	  end; { with }

	  if((PixelsPerCel > 3) and FGrid) then begin
	    //horizontal lines
      x:= 0;
      TheColor:= DDColor;
	    for i:= (ClipRect.Top div PixelsPerCel) to (ClipRect.Bottom div PixelsPerCel)
	    do with LineRect do begin
        y:= (i*PixelsPerCel)+Corr.Y;
        y1:= (y - Corr2.y)+1;
	      Left:= ClipRect.Left;
	      Top:= y;
	      Right:= ClipRect.Right;
	      Bottom:= y+1;
        ClientToCel(x,y1);
        if (y1 mod FBoldGridSpacing) <> 0 then begin
          if (Bottom <= ClipRect.Bottom) and (Top >= ClipRect.Top) then
            MyDDSurface.FillRect(LineRect,TheColor,true);
        end; {if}
	    end; {for i}

	    //vertical lines
      y:= 0;
	    for i:= (ClipRect.Left div PixelsPerCel) to (ClipRect.Right div PixelsPerCel)
	    do with LineRect do begin
        x:= (i*PixelsPerCel)+Corr.X;
        x1:= (x - Corr2.x)+1;
	      Left:= x;
	      Top:= ClipRect.Top;
	      Right:= x+1;
	      Bottom:= ClipRect.Bottom;
        ClientToCel(x1,y);
        if (x1 mod FBoldGridSpacing) = 0 then TheColor:= DD2Color
        else TheColor:= DDColor;
	      if (Left >= ClipRect.Left) and (Right <= ClipRect.Right) then
	        MyDDSurface.FillRect(LineRect,TheColor,true);
	    end; {for i}
      //Draw the horizontal 5th grid lines separatly, to avoid these
      //lines from being drawn over by vertical lines.
      x:= 0;
      TheColor:= DD2Color;
	    for i:= (ClipRect.Top div PixelsPerCel) to (ClipRect.Bottom div PixelsPerCel)
	    do with LineRect do begin
        y:= (i*PixelsPerCel)+Corr.Y;
        y1:= (y - Corr2.y)+1;
	      Left:= ClipRect.Left;
	      Top:= y;
	      Right:= ClipRect.Right;
	      Bottom:= y+1;
        ClientToCel(x,y1);
        if (y1 mod FBoldGridSpacing) = 0 then begin
  	      if (Bottom <= ClipRect.Bottom) and (Top >= ClipRect.Top) then
	          MyDDSurface.FillRect(LineRect,TheColor,true);
        end; {if}
	    end; {for i}
	  end; {if}
  end; {else}
end;

function TLifeBox.IsCelOn(x, y: integer): boolean;
begin
  Result:= Universe.CelState(x,y);
end;

procedure TLifeBox.InvertCel(x, y: integer);
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpDraw);
  Universe.ChangeCel(x,y, not(Universe.CelState(x,y)));
  RedrawCel(x,y);
end;

procedure TLifeBox.ChangeDrawCel(x,y: integer; state: boolean);
begin
  ChangeCel(x,y,state);
  RedrawCel(x,y);
end;

procedure TLifeBox.ChangeCel(x, y: integer; state: boolean);
begin
  Universe.ChangeCel(x,y,state);
end;

procedure TLifeBox.Clear(MoveIt, ClearDesc, Snapshot: boolean);
begin
  if Assigned(OnChangePattern) and Snapshot then OnChangePattern(Self,cpClear);
  Generation:= 0;
  CancelSelection;
  Universe.Clear(false);
  if MoveIt then MoveToFast(0,0);
  //Refresh;
  RedrawAll;
end;

procedure TLifeBox.LoadFromFile(AFileName: string);
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpOpen);
  Generation:= 0;
  CancelSelection;
  Universe.Clear(true);
  MoveToFast(0,0);
  Universe.LoadFromFile(AFileName);
end;

function TLifeBox.EnableScreen;
begin
  try
    MyDDSurface.Unlock;
    except  {ignore errors}
  end; {try}
  Result:= not(MyDDSurface.Locked);
end;

function TLifeBox.CanPlay: boolean;
begin
  if Assigned(Universe) then Result:= Universe.CanPlay
  else Result:= false;
end;

function TLifeBox.IsEmpty: boolean;
begin
  if Assigned(Universe) then Result:= Universe.IsEmpty
  else Result:= true;
end;

function TLifeBox.IsScreenSaverStillActive: Boolean;
const
  ScreenSaverName = 'screen-saver';
  //HasSaverBeenUp: Boolean = false;
var
  hDeskTop: THandle;
begin
  hDesktop:= OpenDesktop(ScreenSaverName,// desktop name where screen saver runs
           0,FALSE,MAXIMUM_ALLOWED);     // open for all possible access
  Result:= true;
  if hDeskTop = 0 then begin
    if(GetLastError() = ERROR_ACCESS_DENIED) then Result:=True
    else Result:= false;
  end
  else CloseDeskTop(HDesktop);
  //if (Result = false) then begin
  //  //Wait with reporting saver off until we've seen it first.
  // if not(HasSaverBeenUp) then Result:= true;
  //end; {if}
end;

function TLifeBox.Display(ARect: TRect): boolean;
const
  LastDisplay: integer = 0;
  LastGenDisplayed: integer = 0;
var
  nextc, Cel: TLifeCel;
  x1,y1: integer;
  DisplayQ: Boolean;
  Display: TLifeCel;
  Now: integer;
  CanShowFrame: boolean;
  ShowMe: Boolean;
  MaxXX, MaxYY: integer;
  XOrigDiv16, YOrigDiv16: integer;
  TorusRect: TRect;


  procedure DisplayZoom1;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);

        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_1Off;
          Cel.DisplayZoom_1On;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom2;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_2Off;
          Cel.DisplayZoom_2On;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom3;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_3Off;
          Cel.DisplayZoom_3On;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom4;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_4Off;
          Cel.DisplayZoom_4On;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom5;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_5Off;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
    Cel:= Universe.Display.DisplayNext;
    if CanShowFrame and ((Generation mod 2 = 0) or IsPaused) then
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      Cel.DisplayZoom_5On;
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom6;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_6Off;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
    Cel:= Universe.Display.DisplayNext;
    if CanShowFrame and ((Generation mod 2 = 0) or IsPaused) then
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      Cel.DisplayZoom_6On;
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom7;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_7Off;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
    Cel:= Universe.Display.DisplayNext;
    if CanShowFrame and ((Generation mod 2 = 0) or IsPaused) then
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      Cel.DisplayZoom_7On;
      Cel:= Nextc;
    end; {while}
  end;

  procedure DisplayZoom8;
  begin
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      if ((Cel.Flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;

      if (CanShowFrame or ShowMe) then begin
        //x1:= Cel.x*16 - xorig;
        //y1:= Cel.y*16 - yorig;
        x1:= smallint(Cel.coor.x - xorigdiv16);
        y1:= smallint(Cel.coor.y - yorigdiv16);


        //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
        //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
        if((x1+1 < 0) or (x1 > MaxXX) or
           (y1+1 < 0) or (y1 > MaxYY)) then
          Universe.removeFromDisplay(Cel)
        else if ((Generation mod 2 = 0) or IsPaused or ShowMe) then begin
          Cel.DisplayZoom_8Off;
        end;
        ShowMe:= false;
      end; {if CanShow}
      Cel:= Nextc;
    end; {while}
    Cel:= Universe.Display.DisplayNext;
    if CanShowFrame and ((Generation mod 2 = 0) or IsPaused) then
    while (Cel <> Display) do begin
      nextc:= Cel.DisplayNext;
      Cel.DisplayZoom_8On;
      Cel:= Nextc;
    end; {while}
  end;


  procedure DisplayNormal;
  begin
    while (Cel <> Display) do begin
      //First Display OFF cells.
      nextc:=Cel.DisplayNext;

      if ((Cel.flags and $02) <> 0) then Universe.RemoveFromDisplay(Cel);

      x1:= smallint(Cel.coor.x - xorigdiv16);
      y1:= smallint(Cel.coor.y - yorigdiv16);

      if((x1+1 < 0) or (x1 > MaxXX) or
         (y1+1 < 0) or (y1 > MaxYY)) then
        Universe.removeFromDisplay(Cel)
      else begin
        Cel.DisplayOn;
        Cel.DisplayOff;
      end; {else}
      Cel:= nextc;
    end; {while}
  end;

  procedure DisplayFrameDropOffs;
  begin
    Cel:= Universe.Display.DisplayNext;
    while (Cel <> Display) do begin
      nextc:=Cel.DisplayNext;
      ShowMe:= false;
      if ((Cel.flags and $02) <> 0) then begin
        Universe.RemoveFromDisplay(Cel);
        ShowMe:= true;
      end;
      x1:= smallint(Cel.coor.x - xorigdiv16);
      y1:= smallint(Cel.coor.y - yorigdiv16);

      if((x1+1 < 0) or (x1 > MaxXX) or
         (y1+1 < 0) or (y1 > MaxYY)) then
        Universe.removeFromDisplay(Cel)
      else if ShowMe then Cel.DisplayAllOff;
      Cel:= nextc;
    end; {while}
  end;

  procedure DisplayAll;
  begin
    while (Cel <> Display) do begin
      //First Display OFF cells.
      nextc:=Cel.DisplayNext;

      if ((Cel.flags and $02) <> 0) then Universe.RemoveFromDisplay(Cel);

      //x1:= Cel.x*16 - xorig;
      //y1:= Cel.y*16 - yorig;
      x1:= smallint(Cel.coor.x - xorigdiv16);
      y1:= smallint(Cel.coor.y - yorigdiv16);


      //if((x1+16 < 0) or (x1+1 > ({width div FPixelsPerCel}FWidthCels)) or
      //   (y1+16 < 0) or (y1+1 > ({height div FPixelsPerCel}FHeightCels))) then
      if((x1+1 < 0) or (x1 > MaxXX) or
         (y1+1 < 0) or (y1 > MaxYY)) then
        Universe.removeFromDisplay(Cel)
      else begin
        Cel.DisplayAllOn;
        Cel.DisplayAllOff;
      end; {else}
      Cel:= nextc;
    end; {while}
  end;


//Start of main display routine.
begin
  //if we are in special go slow when screen-saving mode, check if this
  //nightmare has ended yet.
  if ScreenSaverActive then begin
    ScreenSaverActive:= IsScreenSaverStillActive;
  end;
  //Indicate succes
  Result:= true;
  if (CanPaint = cpCanPaint) then begin
    MaxXX:= ((FWidthCels) div 16)+1;
    MaxYY:= ((FHeightCels) div 16)+1;
    XorigDiv16:= XOrig div 16;
    YOrigDiv16:= YOrig div 16;
    ShowMe:= false;
    if LastGenDisplayed = 0 then LastGenDisplayed:= Universe.Counter - 1;
    DisplayQ:= Universe.QCycle;
    Display:= Universe.Display;
    Cel:= Display.DisplayNext;
    //xd:= ox-((Xorig*FPixelsPerCel) div (1 shl FNegZoom));
    //yd:= oy-((Yorig*FPixelsPerCel) div (1 shl FNegZoom));
    CanShowFrame:= true;
    if (FrameDropTime > 0) and not IsPaused then begin
      Now:= MyGetTickCount;
      CanShowFrame:= (Now - LastDisplay) >= FrameDropTime;
      if CanShowFrame then LastDisplay:= Now;
      Result:= CanShowFrame;
    end; {if}

    //if in torusmode then limit the displayrect to the size of the torus.
    if (Self.IsLimited) then with TorusRect do begin
      TorusRect:= Limit;
      Inc(Bottom); Inc(Right);
      if TorusKind = tk_LeftRight then begin
        Top:= MinY; Bottom:= MaxY;
      end
      else if TorusKind = tk_UpDown then begin
        Left:= MinX; Right:= MaxX;
      end;
      CelToClient(Left, Top);
      CelToClient(Right,Bottom);
      IntersectRect(ARect,TorusRect,ARect);
    end; {if with}
    LifeCel.SetBounds(Self, ARect);
    LifeCel.SetDisplayProps(XOrig, YOrig,ox,oy,FNegZoom,DisplayQ);
    try
      MyDDSurface.Lock(Self.Canvas);
      if FNegZoom > 0 then begin
        //****************************************************
        //Display negative zooms
        //****************************************************
        case FNegZoom of
          1: DisplayZoom1;
          2: DisplayZoom2;
          3: DisplayZoom3;
          4: DisplayZoom4;
          5: DisplayZoom5;
          6: DisplayZoom6;
          7: DisplayZoom7;
          8: DisplayZoom8;
        end; {case}
      end {if}
      //Display the normal zoom.
      else case ViewChanged of
        //only show changes
        //no frames dropped, so only display diff
        false: begin
          if (LastGenDisplayed - Universe.Counter = -1) and CanShowFrame then begin
            //Display NORMAL
            DisplayNormal;
          end
          //Display with frame dropping
          {display all, because a few frames are dropped}
          else if (CanShowFrame) then DisplayAll//DisplayAfterFrameDrop;
          //Display the off cells when dropping frames.
          else DisplayFrameDropOffs;
        end; {false}
        //Display all, because of full refresh.
        true: DisplayAll;
      end; {else case}
      finally MyDDSurface.UnLock;
    end; {try}
    if CanShowFrame then LastGenDisplayed:= Universe.Counter;
  end;
end;

procedure TLifeBox.DisplayCutOut(APos: TPoint; ACutOut: TUniverse);
var
  nextc, Cel: TLifeCel;
  xd,yd: integer;
  DisplayQ: Boolean;
  Display: TLifeCel;
  BoundsRect: TRect;
  RelOffset: TPoint;
begin
  if Assigned(ACutOut) then begin
    ACutOut.FreshenView;
    if (CanPaint = cpCanPaint) then begin
      DisplayQ:= ACutOut.QCycle;
      Display:= ACutOut.Display;
      Cel:= Display.DisplayNext;
      xd:= ((XOrig  - APos.x + ACutOut.ClipRect.Left) * FPixelsPerCel);
      xd:= xd div (1 shl FNegZoom);
      xd:= ox-xd;
      yd:= oy-(((Yorig - APos.y + ACutOut.ClipRect.Top) *FPixelsPerCel) div (1 shl FNegZoom));

      //Make sure nothing gets displayed outside the Cutout's cliprect.
      BoundsRect:= ACutOut.ClipRect;
      OffsetRect(BoundsRect,-BoundsRect.Left,-BoundsRect.Top);
      OffsetRect(BoundsRect,APos.X,APos.y);
      RelOffset.x:= BoundsRect.Left - SelectionRect.Left;
      RelOffset.y:= BoundsRect.Top - SelectionRect.Top;
      if Assigned(FOnShowDrag) then OnShowDrag(Self,RelOffset);
      with BoundsRect do begin
        CelToClient(Left,Top);
        CelToClient(Right,Bottom);
      end; {with}
      IntersectRect(BoundsRect,BoundsRect,GetLifeBoxClipRect);

      //repeated code, so I can avoid 'if then' in key loop.
      try
        MyDDSurface.Lock(Self.Canvas);
        LifeCel.SetBounds(Self, BoundsRect);
        if FNegZoom > 0 then begin
          while (Cel <> Display) do begin
            nextc:= Cel.DisplayNext;
            case FNegZoom of
              1: Cel.DisplayZoom_1Drag(xd,yd,DisplayQ);
              2: Cel.DisplayZoom_2Drag(xd,yd,DisplayQ);
              3: Cel.DisplayZoom_3Drag(xd,yd,DisplayQ);
              4: Cel.DisplayZoom_4Drag(xd,yd,DisplayQ);
              5: Cel.DisplayZoom_5Drag(xd,yd,DisplayQ);
              6: Cel.DisplayZoom_6Drag(xd,yd,DisplayQ);
              7: Cel.DisplayZoom_7Drag(xd,yd,DisplayQ);
              8: Cel.DisplayZoom_8Drag(xd,yd,DisplayQ);
            end; {case}
            Cel:= Nextc;
          end; {while}
        end {if}
        else begin
          while (Cel <> Display) do begin
            nextc:=Cel.DisplayNext;
            Cel.DisplayDrag(xd,yd,DisplayQ);
            Cel:= nextc;
          end; {while}
        end; {else}
        finally MyDDSurface.UnLock;
      end; {try}
    end; {if canpaint}
  end; {if assigned}
end;

procedure TLifeBox.Redraw;
var
  ARect: TRect;
begin
  if (CanPaint = cpCanPaint) then begin
    FWidthCels:= (Width div FPixelsPerCel) shl FNegZoom;
    FHeightCels:= (Height div FPixelsPerCel) shl FNegZoom;
    ARect:= GetLifeBoxClipRect;
    if not(IsMyRectEmpty(ARect)) then begin //ARect:= ClientRect;
      if (SelectionVisible or WhiteDisplay) then RedrawBackground(ARect);
      WhiteDisplay:= false;
      if (Grid) {and (not WhiteDisplay)} then DrawGrid(ARect);
      Canvas.Brush.Color:= CelColor;
      ViewChanged:= true;
      //RedrawBackground(ARect);
      Universe.FreshenView;
      Display(ARect);
      ViewChanged:= false;
      if SelectionVisible then DrawSelRect;
    end;
    Validate;
  end; {if}
end;

procedure TLifeBox.RedrawAll;
begin
  CursorDirty:= true;
  //prevent double clearing by Redraw when a selectionrect is there.
  WhiteDisplay:= true;
  Redraw;
end;

function TLifeBox.UpdateAll: boolean;
var
  ARect: TRect;
begin
  if(viewChanged) then Universe.freshenView();
  //ARect:= GetLifeBoxClipRect;
  ARect:= GetLifeBoxClipRect;
  Result:= display(ARect);
  ViewChanged:= false;
end;


procedure TLifeBox.RedrawBackground(ARect: TRect);
var
  DDColor: TColor;
  LimitRect: TRect;
  GridRect: TRect;
  DrawRect: TRect;
  CanvasClipRect: TRect;

  procedure ShrinkRect(var ARect: TRect; InRect, SRect: TRect);
  begin
    ARect:= InRect;
    if ARect.Top < SRect.Top then ARect.Top:= SRect.Top;
    if ARect.Bottom > SRect.Bottom then ARect.Bottom:= SRect.Bottom;
    if ARect.Left < SRect.Left then ARect.Left:= SRect.Left;
    if ARect.Right > SRect.Right then ARect.Right:= SRect.Right;
  end; {shrinkbackground}

begin
  if (CanPaint = cpCanPaint) {and not(IsMyRectEmpty(ARect))} then begin
    GridRect:= ARect;
    if Self.IsLimited then begin
      DDColor:= MyDDSurface.GetDDNearestColor(TorusColor);
      LimitRect:= Self.Limit;
      with LimitRect do begin
        CanvasClipRect:= GetLifeBoxClipRect;
        Inc(Right); Inc(Bottom);
        if TorusKind = tk_LeftRight then begin
          Top:= MinY; Bottom:= MaxY;
        end {if}
        else if TorusKind = tk_UpDown then begin
          Left:= MinX; Right:= MaxX;
        end; {else if}
        CelToClient(Left,Top);
        CelToClient(Right,Bottom);
        DrawRect:= Rect(0,0,Width,Top);
        ShrinkRect(DrawRect,DrawRect,CanvasClipRect);
        MyDDSurface.FillWindow(Self,DrawRect,DDColor,true); //top
        DrawRect:= Rect(0,Top,Left,Height);
        ShrinkRect(DrawRect,DrawRect,CanvasClipRect);
        MyDDSurface.FillWindow(Self,DrawRect,DDColor,true); //left
        DrawRect:= Rect(Right,Top,Width,Height);
        ShrinkRect(DrawRect,DrawRect,CanvasClipRect);
        MyDDSurface.FillWindow(Self,DrawRect,DDColor,true); //right
        DrawRect:= Rect(Left,Bottom,Right,Height);
        ShrinkRect(DrawRect,DrawRect,CanvasClipRect);
        MyDDSurface.FillWindow(Self,DrawRect,DDColor,true);//bottom
        GridRect:= LimitRect;
      end; {with}   (**)
      ShrinkRect(ARect,ARect,LimitRect);
    end; {if}
    DDColor:= MyDDSurface.GetDDNearestColor(BackColor);
    MyDDSurface.FillWindow(Self,ARect,DDColor,true);
    DrawGrid(GridRect);
  end; {if}
end;

function CorrectDrawRect(var DrawRect: TRect): TRect;
begin
  with DrawRect do begin
    if Top > Bottom then SwapPoints(Top,Bottom);
    if Left > Right then SwapPoints(Left,Right);
  end; {with}
  Result:= DrawRect;
end;

procedure TLifeBox.DrawSelRect;
var
  DrawRect: TRect;
  OldPen: TPen;
  DrawBoth: boolean;
  UseColor: TColor;

  //Windows '95/98 GDI cannot work with 32 bits numbers. It sizes them
  //to 16 bits internally. But that does not work OK all of the time.
  //So we'll help it along a little, so large numbers are not converted
  //to negative numbers by that dumb 16 bit GDI engine.
  function SizeDown(A: integer): integer;
  begin
    Result:= a;
    if Result > 16000 then Result:= 16000
    else if Result < -16000 then Result:= -16000;
  end;

  procedure DrawTheDrawRect;
  var
    OldStyle: TBrushStyle;
  begin
    CorrectDrawRect(DrawRect);
	  with DrawRect do begin
	    if not(IsMyRectEmpty(DrawRect)) then with Canvas do begin
	      //Inc(Right); Inc(Bottom);
  	    CelToClient(Left,Top);
	      CelToClient(Right,Bottom);
        OldStyle:= Brush.Style;
        Brush.Style:= bsClear;
        SizeDown(Right); SizeDown(Left); SizeDown(Bottom); SizeDown(Top);
        //Round down numbers because windows '95 works with
        //16 bit integer numers internally.
        Rectangle(Left+1,Top+1,Right,Bottom);
        Brush.Style:= OldStyle;
      end;
	  end; {with DrawRect}
    //with DrawRect do
  end;

begin
  FSelectionVisible:= true;
  //SelectionRect contains the coordinates in celunits.
  if IsMyRectEmpty(SelectionRect) then begin
    DrawRect:= OrgSelectionRect;
    DrawBoth:= false;
  end
  else begin
    DrawRect:= SelectionRect;
    DrawBoth:= (FSelectionRect.Top <> FOrgSelectionRect.Top) or
               (SelectionRect.Left <> OrgSelectionRect.Left) or
               (SelectionRect.Right <> OrgSelectionRect.Right) or
               (SelectionRect.Bottom <> OrgSelectionRect.Bottom);
  end;

  if (EditorMode = emCursorZoom) then UseColor:= FZoomRectColor
  else
  UseColor:= FSelRectColor;

  with Canvas do begin
  //Set Pen and Brush correctly
    OldPen:= TPen.Create;
    OldPen.Assign(Pen);
    with Pen do begin
      Color:= UseColor;
      Style:= psSolid;
      Width:= 2;
      Mode:= pmXor;
    end; {with Pen}

    //Now draw the Rect.
    DrawTheDrawRect;
    if DrawBoth then begin
      DrawRect:= OrgSelectionRect;
      Pen.Color:= clAqua;
      DrawTheDrawRect
    end; {if}

    Pen.Assign(OldPen);
    OldPen.Free;
  end; {with canvas}
end;

procedure TLifeBox.FakeLifeLine(x1,y1,x2,y2: integer);
var
  OldPen: TPen;
begin
  BoxToClient(x1,y1);
  BoxToClient(x2,y2);
  with Canvas do begin
    OldPen:= TPen.Create;
    OldPen.Assign(Pen);
    with Pen do begin
      Color:= FSelRectColor;
      Style:= psSolid;
      Width:= 1;
      Mode:= pmXor;
    end; {with Pen}
    MoveTo(x1,y1);
    LineTo(x2,y2);
    Pen.Assign(OldPen);
    OldPen.Free;
  end; {with Canvas}
end;

procedure TLifeBox.ClearOutsideTorus;
var
  LimitRect: TRect;
  TempRect: TRect;
  OldSel: TRect;
  Stop: boolean;
begin
  if IsLimited then begin
    //@@@@@@ Hack alert using selectionrect!
    LimitRect:= Limit;
    Inc(LimitRect.Right); Inc(LimitRect.Bottom);
    //Do we need to clear at all?
    TempRect:= Universe.GetBoundingBox;
    Stop:= IsRectEmpty(TempRect);
    if PtInRect(LimitRect,Point(TempRect.Left,TempRect.Top)) and
       PtInRect(LimitRect,Point(TempRect.Right,TempRect.Bottom)) then
      Stop:= true;
    if Stop then exit;

    OldSel:= FSelectionRect;
    case TorusKind of
      tk_All: FSelectionRect:= LimitRect;
      tk_LeftRight: begin
        Universe.ResetBoundingBox;
        FSelectionRect:= Universe.ClipRect;
        TempRect:= SelectionRect;
        TempRect.Left:= LimitRect.Left;
        TempRect.Right:= LimitRect.Right;
        FSelectionRect:= TempRect;
      end;
      tk_UpDown: begin
        Universe.ResetBoundingBox;
        FSelectionRect:= Universe.ClipRect;
        TempRect:= SelectionRect;
        TempRect.Top:= LimitRect.Top;
        TempRect.Bottom:= LimitRect.Bottom;
        FSelectionRect:= TempRect;
      end;
    end; {case}
    ClearOutsideSelection;
    FSelectionRect:= OldSel;
  end; {if}
end;

procedure TLifeBox.ExcludeSelection;
begin
  //do not animate the selection with the rest of the universe.
  if FreezeSelection and not(FSelectionExcluded) and SelectionVisible then begin
    FCutOut:= Universe.CutRect(SelectionRect, cNoClipboard);
    RedrawBackground(CelToClientRect(SelectionRect));
    FSelectionExcluded:= true;
  end; {if}
end;

procedure TLifeBox.IncludeSelection;
begin
  //cancel state where the selection is not animated along with the rest of the
  //universe.
  if FreezeSelection and FSelectionExcluded and SelectionVisible then begin
    Universe.InsertShape(FCutOut, SelectionRect.TopLeft,Pastemode);
    Self.RedrawAll;
    FCutOut.Free;
    FCutOut:= nil;
    FSelectionExcluded:= false;
  end; {if}
end;

procedure TLifeBox.CancelSelection;
begin
  IncludeSelection;
  EraseSelRect(cClearSel);
end;

procedure TLifeBox.HideSelection;
begin
  EraseSelRect(cKeepSel);
  FOrgSelectionRect:= Rect(0,0,0,0);
end;

procedure TLifeBox.ShowSelection;
begin
  DrawSelRect;
end;

function TLifeBox.MakeSnapshotCopy: TSnapshot;
begin
  Result:= nil;
  try
    Result:= Universe.MakeSnapshotCopy;
    if Assigned(Result) then begin
      Result.PatternID:= PatternID;
      Result.Revision:= Revision;
      Result.TimeStamp:= Now;
    end; {if}
    except begin
      try
        Result.Free;
        except {ignore}
      end;
      Result:= nil;
    end; {except}
  end;
end;

function TLifeBox.MakeSnapshotDummy: TSnapshot;
begin
  Result:= nil;
  try
    Result:= TSnapshot.Create;
    Result.Generation:= Generation;
    Result.PatternID:= PatternID;
    Result.Revision:= Revision;
    Result.TimeStamp:= Now;
    except begin
      Result.Free;
      Result:= nil;
    end; {except}
  end; {try}
end;

procedure TLifeBox.RewindToSnapshot(ASnapShot: TSnapShot);
begin
  if Assigned(ASnapshot) then begin
    if Assigned(OnChangePattern) then OnChangePattern(Self,cpRewind);
    RewindToSnapshotSilent(ASnapshot);
    WhiteDisplay:= true;
    Redraw;
  end;
end;

procedure TLifeBox.RewindToSnapshotSilent(ASnapshot: TSnapshot);
begin
  ASnapShot.Seek(0,soFromBeginning);
  Universe.RewindToSnapshot(ASnapShot);
  PatternID:= ASnapShot.PatternID;
  Revision:= ASnapShot.Revision;
end;

procedure TLifeBox.DoAutoScroll(APoint: TPoint; MoveCursor: boolean);
var
  InnerRect: TRect;
  OuterRect: TRect;
  QueryRect: TRect;
  Dx,Dy: integer;
begin
  OuterRect:= ClientRect;
  with OuterRect do begin
    TopLeft:= ClientToScreen(TopLeft);
    BottomRight:= ClientToScreen(BottomRight);
    InnerRect.Left:= Left + 20;
    InnerRect.Top:= Top + 20;
    InnerRect.Right:= Right - 20;
    InnerRect.Bottom:= Bottom - 20;
  end; {with}
  dx:= 0; dy:= 0;
  if PtInRect(OuterRect,APoint) and not PtInRect(InnerRect,APoint) then begin
    //Try Upper
    QueryRect:= OuterRect;
    QueryRect.Bottom:= InnerRect.Top;
    if PtInRect(QueryRect,APoint) then Inc(dy,APoint.y-QueryRect.Bottom);
    //Try Bottom
    QueryRect:= OuterRect;
    QueryRect.Top:= InnerRect.Bottom;
    if PtInRect(QueryRect,APoint) then Inc(dy,APoint.y-QueryRect.Top);
    //Try Left
    QueryRect:= OuterRect;
    QueryRect.Right:= InnerRect.Left;
    if PtInRect(QueryRect,APoint) then Inc(dx,APoint.x-QueryRect.Right);
    QueryRect:= OuterRect;
    QueryRect.Left:= InnerRect.Right;
    if PtInRect(QueryRect,APoint) then Inc(dx,APoint.x-QueryRect.Left);
    dx:= (dx div FPixelsPerCel) shl FNegZoom;
    dy:= (dy div FPixelsPerCel) shl FNegZoom;
    if (dx or dy) <> 0 then begin
      MoveBy(dx*2,dy*2);
      if MoveCursor then SetCursorPos(APoint.x,APoint.y);
    end;
  end; {if}
end;

procedure TLifeBox.EraseSelRect(ClearSel: boolean);
var
  DrawRect: TRect;
  OldOrgSel: TRect;
  OldSel: TRect;
begin
  if UnionRect(DrawRect,OrgSelectionRect,FSelectionRect) then begin
    CorrectDrawRect(DrawRect);
    if SelectionVisible then begin
      FSelectionVisible:= false;
      RedrawLifeRect(DrawRect);
    end;
  end
  else FSelectionVisible:= false;
  if ClearSel then SelectionRect:= Rect(0,0,0,0);

  OldOrgSel:= OrgSelectionRect;
  OldSel:= SelectionRect;
  FSelectionRect:= Rect(0,0,0,0);
  FOrgSelectionRect:= Rect(0,0,0,0);

  FSelectionRect:= OldSel;
  FOrgSelectionRect:= OldOrgSel;
end;

procedure TLifeBox.DrawDropRect(ARect: TRect);
var
  OldBrush: TBrush;
  OldPen: TPen;

  procedure DrawTheDrawRect;
  begin
	  with ARect do begin
	    if not(IsMyRectEmpty(ARect)) then begin
	      //Inc(Right); Inc(Bottom);
  	    CelToClient(Left,Top);
	      CelToClient(Right,Bottom);
        Canvas.Rectangle(Left+1,Top+1,Right,Bottom);
      end;
	  end; {with DrawRect}
    //with DrawRect do
  end;

begin
  //Set Pen and Brush correctly
  with Canvas do begin
    OldBrush:= TBrush.Create;
    OldBrush.Assign(Brush);
    Brush.Handle:= GetStockObject(Null_Brush);
    OldPen:= TPen.Create;
    OldPen.Assign(Pen);
    with Pen do begin
      Color:= clBlue;
      Style:= psSolid;
      Width:= 2;
      Mode:= pmNotXor;
    end; {with Pen}

    //Now draw the Rect.
    DrawTheDrawRect;

    Pen.Assign(OldPen);
    OldPen.Free;
    Brush.Assign(OldBrush);
    OldBrush.Free;
  end; {with canvas}
end;

procedure TLifeBox.RedrawLifeRect(ARect: TRect);
var
  ClearRect: TRect;
  ClipRect: TRect;
begin
  if (CanPaint = cpCanPaint) then begin
    with ARect do begin
      //if not(IsMyRectEmpty(ARect)) then begin
      //  Inc(Right); Inc(Bottom);
      //end; {if}
      CelToClient(Left,Top);
      CelToClient(Right,Bottom);
      Dec(Left);
      Dec(Top);
      Inc(Right);
      Inc(Bottom);
    end; {with DrawRect}
    ClipRect:= GetLifeBoxClipRect;
    if not IsMyRectEmpty(ClipRect) then IntersectRect(ClearRect,ClipRect,ARect)
    else IntersectRect(ClearRect,Self.ClientRect,ARect);
    RedrawBackground(ClearRect);
    ViewChanged:= true;
    Universe.FreshenView;
    Display(ClearRect);
    ViewChanged:= false;
  end;
end;

procedure TLifeBox.DrawDropTarget(x,y: integer);
var
  OldPen: TPen;
begin
  with Canvas do begin
    OldPen:= TPen.Create;
    OldPen.Assign(Pen);
    with Pen do begin
      Color:= clBlue;
      Style:= psSolid;
      Width:= 2;
      Mode:= pmNotXor;
    end; {with Pen}
    CelToClient(x,y);
    MoveTo(x,y); LineTo(x,y+10);
    moveTo(x+10,y); LineTo(x,y);
    Pen.Assign(OldPen);
    OldPen.Free;
  end; {with}
end;

procedure TLifeBox.CorrectSelRect(Shrink: boolean);
begin
  with FSelectionRect do begin
    if Top > Bottom then SwapPoints(Top,Bottom);
    if Left > Right then SwapPoints(Left,Right);
  end; {with}
  //Save unshrunken rect for other purposes
  FOrgSelectionRect:= SelectionRect;
  if Shrink then Universe.ShrinkSelRect(FSelectionRect);
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;

//Draw a line using Bresenham's algorithm
procedure TLifeBox.DrawLine(x1,y1,x2,y2: integer; XorIt: boolean);

  procedure DrawDot(Celx,Cely: integer);
  begin
    if XorIt then ChangeDrawCel(CelX,CelY,Universe.CelState(CelX,CelY) xor true)
    else ChangeDrawCel(Celx,Cely,DragState);
  end;

  //Draws a line in octant 0 or 3 ( |DeltaX| >= DeltaY )
  procedure Octant0(x0,y0,DeltaX,DeltaY,XDirection: integer);
  var
    DeltaYx2: integer;
    DeltaYx2MinusDeltaXx2: integer;
    ErrorTerm: integer;
  begin
    DeltaYx2:= DeltaY *2;
    DeltaYx2MinusDeltaXx2:= DeltaYx2 - (DeltaX * 2);
    ErrorTerm:= DeltaYx2 - DeltaX;
    DrawDot(x0,y0);
    while (DeltaX > 0) do begin
      Dec(DeltaX);
      if (ErrorTerm >= 0) then begin
        Inc(y0);
        Inc(ErrorTerm, DeltaYx2MinusDeltaXx2);
      end {if}
      else begin
        Inc(ErrorTerm, DeltaYx2);
      end; {else}
      Inc(x0,XDirection);
      DrawDot(x0,y0);
    end; {while}
  end;

  procedure Octant1(x0,y0,DeltaX,DeltaY, XDirection: integer);
  var
    DeltaXx2: integer;
    DeltaXx2MinusDeltaYx2: integer;
    ErrorTerm: integer;
  begin
    DeltaXx2:= DeltaX * 2;
    DeltaXx2MinusDeltaYx2:= DeltaXx2 - (DeltaY * 2);
    ErrorTerm:= DeltaXx2 - DeltaY;
    DrawDot(x0,y0);
    while (DeltaY > 0) do begin
      Dec(DeltaY);
      if (ErrorTerm >= 0) then begin
        Inc(x0,XDirection);
        Inc(ErrorTerm, DeltaXx2MinusDeltaYx2);
      end {if}
      else begin
        Inc(ErrorTerm, DeltaXx2);
      end; {else}
      Inc(y0);
      DrawDot(x0,y0);
    end; {while}
  end;

  procedure DrawVertLine(x0,y0,DeltaY,YDirection: integer);
  begin
  end;

  procedure DrawHorzLine(x0,y0,DeltaX,XDirection: integer);
  begin
  end;

var
  DeltaX, DeltaY: integer;
  x,y: integer;
  Temp: integer;
  LineDrawState: TLineDrawState;
begin
  BoxToCel(x1,y1);
  BoxToCel(x2,y2);
  if (y1 > y2) then begin
    temp:= y1;
    y1:= y2;
    y2:= temp;
    Temp:= x1;
    x1:= x2;
    x2:= Temp;
  end; {if}

  DeltaX:= x2 - x1;
  DeltaY:= y2 - y1;
  LineDrawState:= TlineDrawState(byte(DragState and not XorIt) or byte(XorIt)*2);
  if (DeltaX or DeltaY) = 0 then DrawDot(x1,y1)
  else if DeltaX = 0 then begin
    Universe.DrawLine(x1,y1,x2,y2,LineDrawState);
    for y:= Min(y1,y2) to Max(y1,y2) do RedrawCel(x1,y);
  end
  else if DeltaY = 0 then begin
    Universe.DrawLine(x1,y1,x2,y2,LineDrawState);
    for x:= Min(x1,x2) to Max(x1,x2) do RedrawCel(x,y1);
  end
  else if (DeltaX > 0) then begin
    if (DeltaX > DeltaY) then Octant0(x1,y1,DeltaX,DeltaY,1)
    else Octant1(x1,y1,DeltaX,DeltaY,1);
  end {if}
  else begin
    DeltaX:= -DeltaX; //absolute value.
    if (DeltaX > DeltaY) then Octant0(x1,y1,DeltaX,DeltaY,-1)
    else Octant1(x1,y1,DeltaX,DeltaY,-1);
  end; {else}
end;

procedure TLifeBox.Generate(Direction: Boolean);
begin
  Universe.Generate(Direction);
end;

procedure TLifeBox.RandomDot;
var
  XOffset, YOffset: integer;
begin
  XOffset:= Fastrandom(FWidthCels);
  YOffset:= Fastrandom(FHeightCels);
  Universe.ChangeCel(Xorig+XOffset, YOrig+YOffset, true);
end;

procedure TLifeBox.FillRandom;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  Universe.FillRandom(OrgSelectionRect,FillPercentage,PasteMode);
end;
 
procedure TLifeBox.DrawBox;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  Universe.DrawBox(OrgSelectionRect);
end;

procedure TLifeBox.FillBlack;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  Universe.FillRect(OrgSelectionRect,faFill);
end;

procedure TLifeBox.InvertSelection;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  Universe.FillRect(OrgSelectionRect,faInvert);
end;

procedure TLifeBox.ZoomToSelection(MaxFit: integer);
var
  ZoomX, ZoomY: integer;
  ARect: TRect;
begin
  try
    if IsMyRectEmpty(SelectionRect) then ARect:= OrgSelectionRect
    else ARect:= SelectionRect;
    if not Universe.IsSelEmpty(ARect) then with ARect do begin
      MoveToFast((Right+Left) div 2, (Bottom+Top) div 2);
      ZoomX:= Right - Left;
      ZoomY:= Bottom - Top;

      if (ZoomX > Width) then begin
        ZoomX:= (ZoomX div Width);
        if ZoomX > 127 then ZoomX:= -8
        else if ZoomX > 63 then ZoomX:= -7
        else if ZoomX > 31 then ZoomX:= -6
        else if ZoomX > 15 then ZoomX:= -5
        else if ZoomX > 7 then ZoomX:= -4
        else if ZoomX > 3 then ZoomX:= -3
        else if ZoomX > 1 then ZoomX:= -2
        else ZoomX:= -1;
      end
      else ZoomX:= (Width div Max(ZoomX,1));
      if (ZoomY > Height) then begin
        ZoomY:= (ZoomY div Height);
        if ZoomY > 127 then ZoomY:= -8       //  1/256 zoom.
        else if ZoomY > 63 then ZoomY:= -7        //  1/128 zoom.
        else if ZoomY > 31 then zoomY:= -6   //  1/64 zoom.
        else if ZoomY > 15 then ZoomY:= -5   //  1/32 zoom.
        else if ZoomY > 7 then ZoomY:= -4    //  1/16
        else if ZoomY > 3 then ZoomY:= -3    //  1/8
        else if ZoomY > 1 then ZoomY:= -2    //  1/4
        else ZoomY:= -1;                     //  1/2
      end
      else ZoomY:= (Height div Max(ZoomY,1));
      ZoomX:= Max(ZoomX,MinZoom); ZoomX:= Min(ZoomX,MaxZoom);
      ZoomY:= Max(ZoomY,MinZoom); ZoomY:= Min(ZoomY,MaxZoom);
      PixelsPerCel:= Min(Min(ZoomX,ZoomY),MaxFit);
    end {if with}
    else ZoomToFit(true, MaxFit);
    except {ignore}
  end;
end;

procedure TLifeBox.ZoomToFit(Redraw: Boolean; MaxFit: integer);
var
  OldSel: TRect;
  OldCanPaint: TCanpaint;
begin
  OldSel:= FSelectionRect;
  OldCanPaint:= FCanPaint;
  if Redraw then FCanPaint:= cpCanPaint
  else FCanPaint:= cpDontPaint;
  SelectAll(false);
  HideSelection;
  if not IsMyRectEmpty(SelectionRect) then ZoomToSelection(MaxFit);
  FSelectionRect:= OldSel;
  FCanPaint:= OldCanPaint;
end;

procedure TLifeBox.CenterOnPattern(Redraw: Boolean);
var
  MiddleX, MiddleY: integer;
  ARect: TRect;
begin
  ARect:= Universe.GetBoundingBoxFast;
  if IsMyRectEmpty(ARect) then ARect:= Rect(0,0,0,0);
  with ARect do begin
	  MiddleX:= (Right + Left) div 2;
	  MiddleY:= (Top + Bottom) div 2;
    //WhiteDisplay:= true;
    if Redraw then MoveTo(MiddleX,MiddleY)
    else MoveToFast(MiddleX, MiddleY);
  end; {with}
end;

procedure TLifeBox.SelectAll(Redraw: Boolean);
begin
  Universe.ResetBoundingBox;
  CancelSelection;
  SelectionRect:= Universe.ClipRect;
  if Redraw then DrawSelRect;
end;

procedure TLifeBox.CutSelection;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  if Assigned(FCopyUniverse) then try FCopyUniverse.Free except {ignore} end;
  FCopyUniverse:= Universe.CutRect(SelectionRect, cUseClipboard);
  RedrawBackground(CelToClientRect(SelectionRect));
end;

procedure TLifeBox.CopySelection;
begin
  if Assigned(FCopyUniverse) then try FCopyUniverse.Free except {ignore} end;
  FCopyUniverse:= Universe.CopyRect(SelectionRect,cUseClipboard);
end;

procedure TLifeBox.PasteSelection;
var
  PasteRect: TRect;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  if SelectionRect.Top < OrgSelectionRect.Top then
    PasteRect:= OrgSelectionRect
  else PasteRect:= SelectionRect;
  if SelectionRect.Left < OrgSelectionRect.Left then
    PasteRect:= OrgSelectionRect
  else PasteRect:= SelectionRect;
  Universe.PasteRect(FCopyUniverse,PasteRect.TopLeft, PasteMode);
  RedrawBackground(CelToClientRect(OrgSelectionRect));
end;

procedure TLifeBox.ClearSelection;
var
  ARect: TRect;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  Universe.FillRect(SelectionRect, faClear);
  ARect:= CelToClientRect(SelectionRect);
  IntersectRect(ARect,ARect,GetLifeBoxClipRect);
  RedrawBackground(ARect);
  SelectionRect:= Rect(0,0,0,0);
end;

procedure TLifeBox.ClearOutsideSelection;
var
  OldSelRect: TRect;
begin
  if Assigned(FCopyUniverse) then try FCopyUniverse.Free except {ignore} end;
  FCopyUniverse:= Universe.CopyRect(FSelectionRect,cNoClipboard);
  OldSelRect:= FSelectionRect;
  Clear(false,false,false);
  FSelectionRect:= OldSelRect;
  Universe.InsertShape(FCopyUniverse,FSelectionRect.TopLeft, PasteMode);
  Redraw;
  //SelectionRect:= Rect(0,0,0,0);
end;

procedure TLifeBox.MirrorSelVert;
const
  NoClipboard = false;
var
  AShape: TUniverse;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  AShape:= Universe.CutRect(SelectionRect, NoClipBoard);
  AShape.FlipY;
  Universe.InsertShape(AShape, SelectionRect.TopLeft, lpmOr);
  AShape.Free;
end;

procedure TLifeBox.MirrorSelHorz;
const
  NoClipboard = false;
var
  AShape: TUniverse;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  AShape:= Universe.CutRect(SelectionRect, NoClipboard);
  AShape.FlipX;
  Universe.InsertShape(AShape,SelectionRect.TopLeft, lpmOr);
  AShape.Free;
end;

procedure TLifeBox.RotateSel90;
const
  NoClipboard = false;
var
  AShape: TUniverse;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  AShape:= Universe.CutRect(SelectionRect, NoClipboard);
  AShape.Rotate90;
  Universe.InsertShape(AShape, SelectionRect.TopLeft, PasteMode);
  AShape.Free;
  with SelectionRect do
    SelectionRect:= Rect(Left,Top,Left + (Bottom - Top),Top + (Right - Left));
end;

procedure TLifeBox.RotateSel180;
const
  NoClipboard = false;
var
  AShape: TUniverse;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  AShape:= Universe.CutRect(SelectionRect, NoClipboard);
  AShape.Rotate180;
  Universe.InsertShape(AShape, SelectionRect.TopLeft, lpmOr);
  AShape.Free;
end;

procedure TLifeBox.RotateSel270;
const
  NoClipboard = false;
var
  AShape: TUniverse;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  AShape:= Universe.CutRect(SelectionRect, NoClipboard);
  AShape.Rotate270;
  Universe.InsertShape(AShape, SelectionRect.TopLeft, PasteMode);
  AShape.Free;
  with SelectionRect do
    SelectionRect:= Rect(Left,Top,Left + (Bottom - Top),Top + (Right - Left));
end;

procedure TLifeBox.MirrorHorz;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpRotate);
  Universe.ClipRect:= SelectionRect;
  Universe.FlipX;
  SelectionRect:= Universe.ClipRect;
end;

procedure TLifeBox.MirrorVert;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpRotate);
  Universe.ClipRect:= SelectionRect;
  Universe.FlipY;
  SelectionRect:= Universe.ClipRect;
end;

procedure TLifeBox.Rotate90;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpRotate);
  Universe.ClipRect:= SelectionRect;
  Universe.Rotate90;
  SelectionRect:= Universe.ClipRect;
  Limit:= Universe.Limit;
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
end;

procedure TLifeBox.Rotate180;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpRotate);
  Universe.ClipRect:= SelectionRect;
  Universe.Rotate180;
  SelectionRect:= Universe.ClipRect;
  Limit:= Universe.Limit;
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
end;

procedure TLifeBox.Rotate270;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpRotate);
  Universe.ClipRect:= SelectionRect;
  Universe.Rotate270;
  SelectionRect:= Universe.ClipRect;
  Limit:= Universe.Limit;
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
end;

procedure TLifeBox.InsertShape(AShape: TUniverse);
var
  NewSel: TRect;
begin
  if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
  //AShape.ResetBoundingBox;
  Universe.InsertShape(AShape, OrgSelectionRect.TopLeft, PasteMode);
  NewSel:= AShape.ClipRect;
  OffsetRect(NewSel,-NewSel.Left,-NewSel.Top);
  OffsetRect(NewSel,OrgSelectionRect.Left, OrgSelectionRect.Top);
  SelectionRect:= NewSel;
end;

function TLifeBox.GetCutout: TUniverse;
begin
  if IsMyRectEmpty(SelectionRect) then Result:= nil
  else Result:= Universe.CopyRect(SelectionRect, cNoClipboard);
end;

procedure TLifeBox.SaveShape(AShape: TUniverse; AFilename: string; FileFormat: integer);
begin
  if Assigned(AShape) then begin
    AShape.SaveToFile(AFileName, FileFormat, false)
  end; {if}
end;

procedure TLifeBox.SaveToFile(AFilename: string; FileFormat: integer; IncludeTorusData: boolean);
begin
  Universe.ResetBoundingBox; //fixes up the Cliprect of the universe.
  Universe.SaveToFile(AFileName, Fileformat, IncludeTorusData);
end;

procedure TLifeBox.SetSelectionRect(Value: TRect);
begin
  FSelectionRect:= Value;
  FOrgSelectionRect:= Value;
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;

procedure TLifeBox.SetCanPaint(Value: TCanPaint);
begin
  if (csDesigning in ComponentState) then Value:= cpDontPaint;
  if Value <> FCanPaint then begin
    if (Value <> cpDialogShowing) and (FCanPaint = cpDialogShowing) then
      MyDDSurface.DirectDrawEnabled:= MyDDSurface.DirectDrawEnabled and not DDTempDisabled;
    FCanPaint:= Value;
    if (FCanPaint = cpDialogShowing) then begin
      MyDDSurface.DirectDrawEnabled:= MyDDSurface.DirectDrawEnabled or DDTempDisabled;
    end {if}
    else if (FCanPaint = cpCanPaint) then Paint
    else ValidateRect(Handle,nil);
  end;
end;

function TLifeBox.NewUniverse: TUniverse;
begin
  Result:= TUniverse.Create(Universe.RuleString, Universe.Neighborhood);
  Result.OnRuleChange:= Universe.OnRuleChange;
  Result.OnSaveProgress:= Universe.OnSaveProgress;
end;

procedure TLifeBox.SetUniverse(Value: TUniverse);
begin
  //when replacing a universe, the function replacing it should take care
  //of making snapshot, if it kills the old universe.
  repeat {do nothing}
  until not(FUniverse.Locked);
  FUniverse.Release;
  FUniverse:= Value;
  FUniverse.AddRef;
  FUniverse.Description.OnChange:= AfterInfoChange;
  MoveToFast(FUniverse.Centerpoint.x, FUniverse.Centerpoint.y);
end;                          

procedure TLifeBox.SetGrid(Value: Boolean);
begin
  if Value <> FGrid then begin
    //if (Value and not(PixelsPerCel < 4)) or not(value) then begin
      FGrid:= Value;
      SetPixelsPerCel(GetPixelsPerCel);
    //end; {if}
  end; {if}
end;

procedure TLifeBox.SetPatternID(Value: integer);
begin
  if Value <> PatternID then begin
    FPatternID:= value;
    if FPatternID > FMostRecentPatternID then
      FMostRecentPatternID:= FPatternID;
  end; {if}
end;

procedure TLifeBox.SetIsPaused(Value: Boolean);
begin
  if Value <> FIsPaused then begin
    FIsPaused:= Value;
    EditorMode:= EditorMode; //Set correct cursor
  end; {if}
end;

procedure TLifeBox.SetEditorMode(Value: integer);
begin
  if (FEditorMode <> value) and (FEditorMode = emHand) then Scrolling:= false;
  FEditorMode:= value;
  case value of
    emDraw: begin
      //if IsPaused then Cursor:= crColorDraw
      {else} Cursor:= crDraw;
      CancelSelection;
    end; {emDraw:}
    emSelect: begin
      {if IsPaused then Cursor:= crColorCross
      else} Cursor:= crCross;
    end; {emSelect:}
    emCursorZoom: begin
      {if IsPaused then Cursor:= crColorZoom
      else} Cursor:= crZoom;
    end; {emCursorZoom:}
    emHand: begin
      {if IsPaused then Cursor:= crColorHand
      else} Cursor:= crHand;
    end; {emHand:}
    else SetEditorMode(emSelect);
  end; {case}
  if Assigned(OnEditorModeChange) then OnEditorModeChange(Self);
end;

procedure TLifeBox.SetPasteMode(Value: TPasteMode);
begin
  if (Value <> FPasteMode) and ((Value >= lpmOr) and (Value <= lpmError)) then begin
    FPasteMode:= value;
    if Assigned(FOnPasteModeChange) then FOnPasteModeChange(Self);
  end; {if}
end;

procedure TLifeBox.SetScrolling(Value: Boolean);
begin
  if Value <> FScrolling then begin
    FScrolling:= Value;
    if (not FScrolling) and FGrid then DrawGrid(ClientRect);
  end; {if}
end;

function TLifeBox.GetGeneration: Integer;
begin
  if Assigned(Universe) then Result:= Universe.Counter
  else Result:= 0;
end;

procedure TLifeBox.SetGeneration(Value: integer);
begin
  if Assigned(Universe) then Universe.Counter:= Value;
end;

function TLifeBox.GetFrameDropTime: integer;
begin
  //if (Grid) or (PixelsPerCel > 2) then Result:= 0
  //else
  Result:= FFramedropTime;
end;

procedure TLifeBox.SetXScroll(value: integer);
begin
  if value <> XScroll then MoveBy(value - XScroll,0);
end;

function TLifeBox.GetXScroll: integer;
begin
  Result:= Xorig + (((Width div FPixelsPerCel) shl FNegZoom) div 2)
end;

procedure TLifeBox.SetYScroll(value: integer);
begin
  if value <> YScroll then MoveBy(0, value - YScroll);
end;

function TLifeBox.GetYScroll: integer;
begin
  Result:= Yorig + (((Height div FPixelsPerCel) shl FNegZoom) div 2)
end;

procedure TLifeBox.SetOnRuleChange(Value: TRuleChangeEvent);
begin
  FOnRuleChange:= Value;
  if Assigned(Universe) then Universe.OnRuleChange:= Value;
end;

procedure TLifeBox.SetOnSaveProgress(Value: TSaveProgressEvent);
begin
  FOnSaveProgress:= Value;
  if Assigned(Universe) then Universe.OnSaveProgress:= Value;
end;

procedure TLifeBox.SetAfterInfoChange(Value: TNotifyEvent);
begin
  FAfterInfoChange:= Value;
  if Assigned(Universe) then Universe.Description.OnChange:= Value;
end;

procedure TLifeBox.RefreshColors(DoRedraw: boolean);
var
  OldCanPaint: TCanPaint;
begin
  OldCanPaint:= FCanPaint;
  FCanPaint:= cpDontPaint;
  //refresh those colors used by DDraw.
  SetCelColor(CelColor);
  SetBackColor(BackColor);
  SetDragColor(DragColor);
  SetTorusColor(TorusColor);
  FCanPaint:= OldCanPaint;
  if DoRedraw then begin
    WhiteDisplay:= true;
    Redraw;
  end;
end;

procedure TLifeBox.SetScreenSaverActive(Value: Boolean);
begin
  if ScreenSaverActive <> Value then begin
    FScreenSaverActive:= Value;
    with MyDDSurface do begin
      if Value then DirectDrawEnabled:= DirectDrawEnabled or DDScreenSaver
      else DirectDrawEnabled:= DirectDrawEnabled and not DDScreenSaver;
    end; {with}
  end; {if}
end;

procedure TLifeBox.SetLimit(Value: TRect);
var
  ALimit: TRect;
begin
  ALimit:= Universe.Limit;
  if not(CompareMem(@value,@ALimit,SizeOf(Value))) then begin
    //reject impossible limitrects.
    with Value do if (Left < Right) and (Top < Bottom) then begin
      Universe.Limit:= value;
      if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
      //ClearOutsideTorus;
    end;
  end; {if}
end;

function TLifeBox.GetLimit: TRect;
begin
  Result:= Universe.Limit;
end;

procedure TLifeBox.SetIsLimited(Value: boolean);
begin
  if value <> FIsLimited then begin
    FIsLimited:= value;
    Universe.IsLimited:= value;
    if Universe.IsLimited then ClearOutsideTorus;
    if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
    WhiteDisplay:= true;
    RedrawAll;
  end; {if}
end;

procedure TLifeBox.SetTorusKind(Value: TTorusKind);
begin
  if value <> Universe.TorusKind then begin
    Universe.TorusKind:= value;
    if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
    RedrawAll;
  end;
end;

procedure TLifeBox.SetDeadEdges(Value: boolean);
begin
  if Value <> Universe.DeadEdges then begin
    Universe.DeadEdges:= Value;
    if Assigned(OnChangePattern) then OnChangePattern(Self,cpTorus);
  end;
end;

function TLifeBox.GetTorusKind: TTorusKind;
begin
  Result:= Universe.TorusKind;
end;

function TLifeBox.GetDeadEdges: boolean;
begin
  Result:= Universe.DeadEdges;
end;

function TLifeBox.ConvertColor(OldColor, WinColor: integer): integer;
begin
  Result:= MyDDSurface.GetDDNearestColor(WinColor);
  if (Result = Integer(ColorError)) then Result:= OldColor;
end;

function TLifeBox.GetLifeBoxClipRect: TRect;
begin
  //Window has style CS_OWNDC, this speeds up action by 7%.
  GetClipBox(FMyDC,Result);
  {if we disregard all further clipping, we can speed up by 2-3%, not worth it}
end;

procedure TLifeBox.SetCelColor(Value: TColor);
begin
  FCelColor:= Value;
  if not (csDesigning in componentstate) then begin
    if not(Assigned(MyDDSurface)) then InitDDraw; //Set up DirectDraw;
    LifeCel.ForeColor:= ConvertColor(LifeCel.ForeColor,Value);
  end; {if}
end;

procedure TLifeBox.SetBackColor(Value: TColor);
begin
  FBackColor:= Value;
  if not (csDesigning in componentstate) then begin
    if not(Assigned(MyDDSurface)) then InitDDraw; //Set up DirectDraw;
    LifeCel.BackColor:= ConvertColor(LifeCel.BackColor,Value);
  end; {if}
end;

procedure TLifeBox.SetDragColor(Value: TColor);
begin
  FDragColor:= Value;
  if not (csDesigning in componentstate) then begin
    if not(Assigned(MyDDSurface)) then InitDDraw; //Set up DirectDraw;
    LifeCel.DragColor:=
      (ConvertColor(LifeCel.DragColor,Value) xor ConvertColor(LifeCel.BackColor,BackColor));
  end; {if}
end;

procedure TLifeBox.SetTorusColor(Value: TColor);
begin
  FTorusColor:= Value;
  if not (csDesigning in componentstate) then begin
    if not(Assigned(MyDDSurface)) then InitDDraw; //Set up DirectDraw;
    LifeCel.TorusColor:= ConvertColor(LifeCel.TorusColor,Value);
  end; {if}
end;

procedure TLifeBox.SetSelRectColor(Value: TColor);
begin
  with MyDDSurface do begin
    FSelRectColor:= Value xor FBackColor;
  end; {with}
end;

procedure TLifeBox.SetZoomRectColor(Value: TColor);
begin
  with MyDDSurface do begin
    FZoomRectColor:= Value xor FBackColor;
  end; {with}
end;

function TLifeBox.GetZoomRectColor: TColor;
begin
  with MyDDSurface do begin
    Result:= FZoomRectColor xor FBackColor;
  end; {with}
end;

function TLifeBox.GetSelRectColor: TColor;
begin
  with MyDDSurface do begin
    Result:= FSelRectColor xor FBackColor;
  end; {with}
end;

procedure TLifeBox.SetGridColor(Value: TColor);
begin
  if FGridColor <> Value then begin
    FGridColor:= Value;
    if not (csDesigning in componentstate) and Assigned(MyDDSurface) then Redraw;
  end; {if}
end;

procedure TLifeBox.SetGrid2Color(Value: TColor);
begin
  if FGrid2Color <> Value then begin
    FGrid2Color:= Value;
    if not (csDesigning in componentstate) and Assigned(MyDDSurface) then Redraw;
  end; {if}
end;

procedure TLifeBox.RedrawCursorArea(x,y: integer);
var
  TheRect: TRect;
  OldCanPaint: TCanPaint;
begin
  if (MyDDSurface.DirectDrawEnabled = DDFast) and (Cursor > crMonochrome) then begin
    OldCanPaint:= CanPaint;
    FCanpaint:= cpCanPaint;
    CursorDirty:= false;
    //ACursor is 32x32, this should do the trick (80x80)
    TheRect:= Rect(x-40,y-40,x+40,y+40);
    IntersectRect(TheRect, TheRect,ClientRect);
    with TheRect do begin
      ClientToCel(Left,Top);
      ClientToCel(Right,Bottom);
    end; {with}
    //OldDDEnabled:= MyDDSurface.DirectDrawEnabled;
    //MyDDSurface.DirectDrawEnabled:= false;
    ShowCursor(False);
    RedrawLifeRect(TheRect);
    ShowCursor(true);
    //MyDDSurface.DirectDrawEnabled:= OldDDEnabled;
    FCanPaint:= OldCanpaint;
    //ValidateRect(Handle,nil);
  end;
end;

procedure TLifeBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACutOut: TUniverse;
  OKEffect, ChosenEffect: integer;
  BoxX, BoxY: integer;
  CelX,CelY: integer;
  MousePt: TPoint;
  StartDragHack: TStartDragHack;
begin
  //Quit mouse action in case of a chord click.
  if ((Button = mbRight) and (IsMouseDown = mdLeft)) or
     ((Button = mbLeft) and (IsMouseDown = mdRightDrag)) then begin
    MouseUp(Button,Shift,x,y);
    exit;
  end; {if Chord click}
  if Button = mbLeft then IsMouseDown:= mdLeft
  else if Button = mbRight then begin
    if not(SelectionVisible) then IsMouseDown:= mdRightDrag
    else IsMouseDown:= mdRight;
  end; {else}

  if (IsMouseDown = mdLeft) or (IsMouseDown = mdRightDrag) then begin
    CelX:= X; CelY:= y;
    MousePt.x:= x; MousePt.y:= y;
    ClientToCel(Celx,Cely);
    ClientToCelFast(MousePt.x,MousePt.y);
    if (IsMouseDown = mdLeft) and (Universe.IsLimited) then begin
      if IsCursorAtTorusEdge then begin
        IsMouseDown:= mdTorusResize;//Enter torus drag mode.
        DropOffset:= MousePt;       //Remember where we started dragging.
      end;
    end;
    //If not in Torus drag mode then go test for other stuff.
    if (IsMouseDown <> mdTorusResize) then case EditorMode of
      emDraw: begin
        //if drawing lines let old dragstate prevail.
        if (ssShift in Shift) then
          DragState:= DragState or not(IsCelOn(Celx,Cely))
        else Dragstate:= not(IsCelOn(Celx,Cely));
        Canvas.Brush.Color:= CelColor;
        BoxX:= x; BoxY:= y;
        ClientToBox(BoxX,BoxY);
        DrawX:= Boxx; DrawY:= Boxy;
        OldDrawX:= DrawX; OldDrawY:= DrawY;
        {if not(ssShift in Shift) then} InvertCel(Celx,Cely);
      end; {emDraw:}
      emSelect: begin
        if not(IsPaused) then begin
          if Assigned(OnMustPause) and (IsMouseDown = mdLeft) then OnMustPause(Self); //make the animation stop.
        end;
        if (FInSelection) and not (IsMyRectEmpty(SelectionRect)) and IsOleActive
        then begin
          ACutOut:= Universe.CopyRect(SelectionRect, cNoClipboard);
          FDropData:= TDataObject.Create(Self,ACutOut);
          FDropData._AddRef; //Add a ref, so the free will not fail later.
          //FDropData._AddRef; //Add a 2nd ref, so the free will not fail later.
          try
            GetCursorPos(MousePt);
            MousePt:= ScreenToClient(MousePt);
            with MousePt do begin
              ClientToCel(x,y);
              x:= SelectionRect.Left - x;
              y:= SelectionRect.Top - y;
              DropOffset:= MousePt;
            end; {with}
            OKEffect:= DROPEFFECT_COPY or DROPEFFECT_MOVE;
            //DoDragDrop takes care of everything.
            //if Assigned(OnChangePattern) then OnChangePattern(Self,cpSelect);
            StartDragHack:= TStartDragHack.Create;
            DoDragDrop(FDropData, FDropSource,OKEffect, ChosenEffect);
            StartDragHack.Free;
            //Clear Dropoffset for next time
            DropOffset:= Point(0,0);

            IsMouseDown:= mdDragDrop; //Tell mouseup that data has been dragged OK.
            MouseUp(mbLeft, Shift,x,y);
            finally begin
              //FDropData._Release;
              FDropData.Free;
              FDropData:= nil;
              ACutOut.Release;
            end; {finally}
          end; {try}
        end {if}
        else begin
          CancelSelection;
          CursorDirty:= true;
          MouseCapture:= true;
          SelectionRect:= Rect(Celx,Cely,Celx,Cely);
        end; {else}
      end; {emSelect:}
      emHand: begin
        //Set starting point for movement.
        Scrolling:= true;
        MouseCapture:= false;
        Cursor:= crHandGrab;
        MouseCapture:= true;
        HandX:= Celx;
        HandY:= Cely;
      end; {emHand:}
      emCursorZoom: begin //same as emSelect.
        CancelSelection;
        MouseCapture:= true;
        SelectionRect:= Rect(Celx,Cely,Celx,Cely);
      end; {emCursorZoom:}
    end; {if not Resizing torus then case}
  end; {if mdLeft or mdRightDrag}
  inherited;
end;

function TLifeBox.IsCursorAtTorusEdge: boolean;
var
  MousePt: TPoint;
  LargeRect: TRect;
  SmallRect: TRect;
  ARect: TRect;
  Margin: integer;
begin
  GetCursorPos(MousePt);
  MousePt:= ScreenToClient(MousePt);
  with MousePt do begin
    ClientToCelFast(x,y);
  end; {with}
  ARect:= Universe.Limit;
  Inc(ARect.Right); Inc(ARect.Bottom);
  SmallRect:= ARect;
  LargeRect:= ARect;
  if PixelsPerCel < 0 then Margin:= 12 * (1 shl ABS(PixelsPerCel))
  else if PixelsPerCel > 4 then Margin:= 2
  else Margin:= 12 div PixelsPerCel;
  with LargeRect do begin
    Dec(Top,Margin); Dec(Left,Margin); Inc(Right,Margin); Inc(Bottom,Margin);
  end; {with}
  Result:= PtInRect(LargeRect,MousePt) and not PtInRect(SmallRect,MousePt);
end;

procedure TLifeBox.ShowTorusResizeCursor(Show: boolean);
var
  MousePt: TPoint;
  SmallRect: TRect;
const
  OldCursor: integer = $FFFFF;
begin
  GetCursorPos(MousePt);
  MousePt:= ScreenToClient(MousePt);
  with MousePt do begin
    ClientToCelFast(x,y);
  end; {with}
  SmallRect:= Universe.Limit;
  case Show of
    true: begin
      OldCursor:= Cursor;
      if (MousePt.x >= SmallRect.Right) or (MousePt.x <= SmallRect.Left) then
        Cursor:= crHSplit
      else Cursor:= crVSplit;
    end;
    false: begin
      //Force adaption of cursor.
      if (OldCursor <> $FFFFF) then EditorMode:= EditorMode;
    end;
  end; {case}
end;

procedure TLifeBox.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  OldX: integer = 0;
  OldY: integer = 0;
var
  MouseCel: TPoint;
  MousePt: TPoint;
  Boxx, Boxy: integer;
  OffsetX, OffsetY: integer;
  x1,y1: integer;
  TheRect: TRect;
begin
  MouseCel.X:= x;
  MouseCel.Y:= y;
  MousePt:= MouseCel;
  ClientToCel(MouseCel.X,MouseCel.Y);
  ClientToCelFast(MousePt.x,MousePt.y);

  //Fix DirectX related drawing issues
  if CursorDirty then begin
    RedrawCursorArea(OldX,OldY);
  end; {if}

  //If just mousing around, maybe we should show the resizecursor?
  if (IsMouseDown = mdNone) and (Universe.IsLimited) then begin
    ShowTorusResizeCursor(IsCursorAtTorusEdge);
  end;
  //Drag the edge of the torus.
  if (IsMouseDown = mdTorusResize) then begin
    //First determine offset from previous position.
    OffsetX:= MousePt.x - DropOffset.x;
    OffsetY:= MousePt.y - DropOffset.y;
    //Now determine with side we are dragging and adjust accordingly.
    TheRect:= Limit;
    with TheRect do begin
      if DropOffset.x >= Right then Inc(Right,OffsetX)
      else if DropOffset.x <= Left then Inc(Left,OffsetX)
      else if DropOffset.y >= Bottom then Inc(Bottom,OffsetY)
      else if DropOffset.y <= Top then inc(Top,OffsetY);
    end; {with}
    Limit:= TheRect;
    DropOffset:= MousePt;
    Self.RedrawAll;
  end; {if Resizing Torus}
  //Do stuff while holding down the mouse (drawing, dragging etc).
  if (IsMouseDown = mdLeft) or (IsMouseDown = mdRightDrag) then begin
    case EditorMode of
      emDraw: begin
        Boxx:= x; Boxy:= y;
        ClientToBox(Boxx,Boxy);
        if (BoxX <> DrawX) or (BoxY <> DrawY) then begin
          if (ssShift in Shift) then begin
            if (BoxX <> OldDrawX) or (BoxY <> OldDrawY) then begin
              if Self.PixelsPerCel <=1 then begin
                FakeLifeLine(DrawX,DrawY,OldDrawX,OldDrawY);
                FakeLifeLine(DrawX,DrawY,BoxX,BoxY);
              end
              else begin
                DrawLine(DrawX,DrawY,OldDrawX,OldDrawY,true);
                DrawLine(DrawX,DrawY,BoxX,BoxY,true);
              end; {else}
              OldDrawX:= BoxX; OldDrawY:= BoxY;
            end; {if}
          end {if}
          else begin
            DrawLine(DrawX,DrawY,BoxX,BoxY,false);
            DrawX:= BoxX; DrawY:= BoxY;
          end; {else}
        end; {if}
      end; {emDraw:}

      emSelect, emCursorZoom:begin
        if not(IsPaused) then begin
          if Assigned(OnMustPause) and (EditorMode = emSelect) then OnMustPause(Self); //make the animation stop.
        end;
        DoAutoScroll(ClientToScreen(Point(x,y)),true);
        DrawSelRect;  //works with notXorPen, so second draw erases the line.
        with SelectionRect do begin
          if ((MouseCel.X+1) > Left) then x1:= MouseCel.X+1 else x1:= MouseCel.X;
          if ((MouseCel.Y+1) > Top) then y1:= MouseCel.y+1 else y1:= MouseCel.y;
          SelectionRect:= Rect(Left,Top,X1,Y1);
        end; {with}
        DrawSelRect;
      end; {emSelect:}
      emHand: if (HandX <> MouseCel.X) or (HandY <> MouseCel.Y) then begin
        OffsetX:= (HandX-MouseCel.X)*HandScroll;
        OffsetY:= (HandY-MouseCel.Y)*HandScroll;
        if not(MoveBy(OffsetX,OffsetY)) then begin
          OffsetX:= 0;
          OffsetY:= 0;
        end;
        HandX:= MouseCel.X+OffsetX;
        HandY:= MouseCel.Y+OffsetY;
      end; {emHand:}
    end; {case}
  end; {if}
  //If we are inside the selection, show an arrow to indicate 'dragability'.
  //There's no need for special testing, this will not coincide with any other
  //event listed above.
  case EditorMode of
    emSelect: begin
      if IsMyRectEmpty(SelectionRect) then TheRect:= Rect(0,0,0,0)//OrgSelectionRect
      else TheRect:= SelectionRect;
      if not(SelectionVisible) then begin
        TheRect:= Rect(0,0,0,0);
        FOrgSelectionRect:= Rect(0,0,0,0);
      end; {if}
      FInSelection:= false; //The default.
      if (PtInRect(TheRect,Point(MousePt.X,MousePt.Y))) then begin
        Screen.Cursor:= crMyArrow;
        FInSelection:= true;
      end {if}
      else begin
        //Screen.Cursor:= Self.Cursor;
        Screen.Cursor:= crDefault;
        FInSelection:= false;
      end; {else}
    end; {emSelect:}
    else Screen.Cursor:= crDefault;
  end; {case}
  OldX:= X; OldY:= Y;
  //inherited
  if Assigned(OnMouseMove) then OnMouseMove(Self,Shift,x,y);
end;


procedure TLifeBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PopupPoint: TPoint;
  ZoomX, ZoomY: integer;
  CelX,CelY: integer;
  BoxX, BoxY: integer;
begin
  inherited;
  CelX:= x; CelY:= y;
  ClientToCel(CelX,CelY);
  if (IsMouseDown <> mdNone) then begin
    if ((IsMouseDown = mdRightDrag) and (Button = mbLeft)) or
       ((IsMouseDown = mdLeft) and (Button = mbRight)) then begin
      //Both buttons are pressed, so
      //cancel the current mode (if applicable).
      case EditorMode of
        emDraw: if (ssShift in Shift) then begin
          //Erase last line.
          if PixelsPerCel <= 1 then FakeLifeLine(DrawX,DrawY,OldDrawX,OldDrawy)
          else DrawLine(DrawX,DrawY,OldDrawX,OldDrawY,true);
        end; {emDraw:}
        emSelect , emCursorZoom: CancelSelection;
        emHand: begin
          MouseCapture:= false;
          {if IsPaused then Cursor:= crColorHand
          else} Cursor:= crHand;
          Scrolling:= false;
        end; {emHand:}
      end; {case}
      IsMouseDown:= mdNone;
      MouseCapture:= false;
      exit;
    end {if}
    else if (IsMouseDown = mdTorusResize) then begin {do nothing} end
    else if (IsMouseDown = mdDragDrop) then begin {do nothing} end
    else if (Button = mbLeft) or ((Button = mbRight) and (IsMouseDown = mdRightDrag))
    then begin
      case EditorMode of
        emDraw: if (ssShift in Shift) then begin
          Boxx:= x; Boxy:= y;
          ClientToBox(Boxx,Boxy);
          //erase last xorline
          if PixelsPerCel <= 1 then FakeLifeLine(DrawX,DrawY,OldDrawX,OldDrawY)
          else DrawLine(DrawX,DrawY,OldDrawX,OldDrawY,true);
          DrawLine(DrawX,DrawY,BoxX,BoxY,false);
        end;
        emSelect:
        with SelectionRect do if not(IsMyRectEmpty(SelectionRect)) then begin
          HideSelection;
          CorrectSelRect(not(ssShift in Shift));
          DrawSelRect;
        end; {emSelect:}
        emCursorZoom: begin
          CanPaint:= cpDontPaint;
          with SelectionRect do if not(IsMyRectEmpty(SelectionRect)) then begin
            //HideSelection;
            //CorrectSelRect;
            //if IsMyRectEmpty(SelectionRect) then SelectionRect:= OrgSelectionRect;
            with SelectionRect do begin
              ZoomX:= Right - Left;
              ZoomY:= Bottom - Top;
              if ((ZoomX + ZoomY > 0) and (ZoomX > 0) and (ZoomY > 0)) and (Button = mbLeft) then
                ZoomToSelection(MaxZoom)
              else begin
                MoveToFast(CelX,CelY);
                PixelsPerCel:= PixelsPerCel - 1;
                CursorDirty:= true;
              end; {else}
            end; {with}
            CancelSelection;
          end {if}
          else begin  //Empty selectionrect, zoom out
            MoveTo(CelX,CelY);
            PixelsPerCel:= PixelsPerCel - 1;
            CursorDirty:= true;
          end; {else}
          Validate;
          CanPaint:= cpCanPaint;
        end; {emCursorZoom:}
        emHand: begin
          MouseCapture:= false;
          {if IsPaused then Cursor:= crColorHand
          else} Cursor:= crHand;
          Scrolling:= false;
        end;
      end; {case}
    end; {if}
    if (Button = mbRight) and (EditorMode = emSelect) then begin
      PopupPoint:= ClientToScreen(Point(x,y));
      with PopupPoint do begin
        if Assigned(PopupMenu) then PopupMenu.Popup(x,y);
      end; {with}
      //EraseSelRect(cKeepSel);
    end; {if}
    IsMouseDown:= mdNone;
    MouseCapture:= False;
  end; {if}
end;

procedure TLifeBox.WMChar(var Msg: TWMChar);     //@@@@@@@@@@@@@@@@
var
  KeyCode: Char;
begin
  KeyCode:= Char(Msg.CharCode);
  KeyPress(Keycode);
end;

procedure TLifeBox.WMDropFiles(var Msg: TWMDropFiles);
begin
  if Assigned(FOnDropFiles) then begin
    if Assigned(OnChangePattern) then OnChangePattern(Self,cpOpen);
    FOnDropFiles(Self,Msg);
    CursorDirty:= true;
  end;
end;

procedure DisplayChange(BitsPerPixel,Width,Height: integer);
begin
  LifeCel.DisplayChange(BitsPerPixel,Width,Height);
end;

initialization
finalization                        
  if IsOleActive then try
    IsOleActive:= false;
    OleUninitialize;
    except {ignore}
  end; {try}
end.
