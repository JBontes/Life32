unit NewTabset;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Consts;

type
TScrollBtn = (sbLeft, sbRight);

  TScroller = class(TCustomControl)
  private
    { property usage }
    FMin: Longint;
    FMax: Longint;
    FPosition: Longint;
    FOnClick: TNotifyEvent;
    FChange: Integer;

    { private usage }
    Bitmap: TBitmap;
    Pressed: Boolean;
    Down: Boolean;
    Current: TScrollBtn;
    pWidth: Integer;
    pHeight: Integer;

    { property access methods }
    procedure SetMin(Value: Longint);
    procedure SetMax(Value: Longint);
    procedure SetPosition(Value: Longint);

    { private methods }
    function CanScrollLeft: Boolean;
    function CanScrollRight: Boolean;
    procedure DoMouseDown(X: Integer);
    procedure WMLButtonDown(var Message: TWMLButtonDown);
      message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouseMove);
      message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp);
      message WM_LBUTTONUP;
    procedure WMSize(var Message: TWMSize);
      message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Min: Longint read FMin write SetMin default 0;
    property Max: Longint read FMax write SetMax default 0;
    property Position: Longint read FPosition write SetPosition default 0;
    property Change: Integer read FChange write FChange default 1;
  end;  (**)

  TNewTabSet = class;

  TTabList = class(TStringList)
  private
    Tabs: TNewTabSet;
  public
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    function Add(const S: string): Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
  end;  (**)

  { eash TEdgeType is made up of one or two of these parts }
  TEdgePart = (epSelectedLeft, epUnselectedLeft, epSelectedRight,
    epUnselectedRight);

  { represents the intersection between two tabs, or the edge of a tab }
  TEdgeType = (etNone, etFirstIsSel, etFirstNotSel, etLastIsSel, etLastNotSel,
    etNotSelToSel, etSelToNotSel, etNotSelToNotSel);

  TTabStyle = (tsStandard, tsOwnerDraw);

  TTabOrientation = (toUp, toDown);

  TMeasureTabEvent = procedure(Sender: TObject; Index: Integer;
    var TabWidth: Integer) of object;
  TDrawTabEvent = procedure(Sender: TObject; TabCanvas: TCanvas; R: TRect;
    Index: Integer; Selected: Boolean) of object;
  TTabChangeEvent = procedure(Sender: TObject; NewTab: Integer;
    var AllowChange: Boolean) of object;

  TNewTabSet = class(TCustomControl)
  private
    { property instance variables }
    FStartMargin: Integer;
    FEndMargin: Integer;
    FTabs: TStrings;
    FTabIndex: Integer;
    FFirstIndex: Integer;
    FOrientation: TTabOrientation;
    FVisibleTabs: Integer;
    FSelectedColor: TColor;
    FUnselectedColor: TColor;
    FBackgroundColor: TColor;
    FDitherBackground: Boolean;
    FAutoScroll: Boolean;
    FStyle: TTabStyle;
    FOwnerDrawHeight: Integer;
    FOnMeasureTab: TMeasureTabEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnChange: TTabChangeEvent;

    { private instance variables }

    ImageList: TImageList;
    MemBitmap: TBitmap;   { used for off-screen drawing }
    BrushBitmap: TBitmap; { used for background pattern }

    TabPositions: TList;
    FTabHeight: Integer;
    Scroller: TScroller;
    FDoFix: Boolean;

    { property access methods }
    procedure SetSelectedColor(Value: TColor);
    procedure SetUnselectedColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetDitherBackground(Value: Boolean);
    procedure SetOrientation(Value: TTabOrientation);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetStartMargin(Value: Integer);
    procedure SetEndMargin(Value: Integer);
    procedure SetTabIndex(Value: Integer);
    procedure SetFirstIndex(Value: Integer);
    procedure SetTabList(Value: TStrings);
//    function GetTabCount: Integer;
//    function GetTabName(Value: Integer): String;
//    procedure SetTabName(Value: Integer; const AName: String);
    procedure SetTabStyle(Value: TTabStyle);
    procedure SetTabHeight(Value: Integer);

    { private methods }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure PaintEdge(X, Y, H: Integer; Edge: TEdgeType);
    procedure CreateBrushPattern(Bitmap: TBitmap);
    function CalcTabPositions(Start, Stop: Integer; Canvas: TCanvas;
      First: Integer): Integer;
    procedure CreateScroller;
    procedure InitBitmaps;
    procedure DoneBitmaps;
    procedure CreateEdgeParts;
    procedure FixTabPos;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure ScrollClick(Sender: TObject);
    procedure ReadIntData(Reader: TReader);
    procedure ReadBoolData(Reader: TReader);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
      Selected: Boolean); virtual;
    function CanChange(NewIndex: Integer): Boolean;
    procedure MeasureTab(Index: Integer; var TabWidth: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function ItemAtPos(Pos: TPoint): Integer;
    function ItemRect(Item: Integer): TRect;
    procedure SelectNext(Direction: Boolean);
    property Canvas;
    property FirstIndex: Integer read FFirstIndex write SetFirstIndex default 0;
  published
    property Align;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property DitherBackground: Boolean read FDitherBackground write SetDitherBackground default True;
    property DragMode;
    property Enabled;
    property EndMargin: Integer read FEndMargin write SetEndMargin default 5;
    property Font;
    property Orientation: TTabOrientation read FOrientation write SetOrientation;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StartMargin: Integer read FStartMargin write SetStartMargin default 5;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clBtnFace;
    property Style: TTabStyle read FStyle write SetTabStyle default tsStandard;
    property TabHeight: Integer read FOwnerDrawHeight write SetTabHeight default 20;
    property Tabs: TStrings read FTabs write SetTabList;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property UnselectedColor: TColor read FUnselectedColor write SetUnselectedColor default clWindow;
    property Visible;
    property VisibleTabs: Integer read FVisibleTabs;
    property OnClick;
    property OnChange: TTabChangeEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMeasureTab: TMeasureTabEvent read FOnMeasureTab write FOnMeasureTab;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  System.Types;

procedure Register;
begin
  RegisterComponents('Johan',[TNewTabSet]);
end;

const
  EdgeWidth = 9;  { This controls the angle of the tab edges }

type
  TTabPos = record
    Size, StartPos: Word;
  end;

{ TScroller }

constructor TScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle + [csOpaque];
  Bitmap:= TBitmap.Create;
  pWidth:= 24;
  pHeight:= 13;
  FMin:= 0;
  FMax:= 0;
  FPosition:= 0;
  FChange:= 1;
end;

destructor TScroller.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

procedure TScroller.Paint;
begin
  with Canvas do
  begin
    { paint left button }
    if CanScrollLeft then
    begin
      if Down and (Current = sbLeft) then
        Bitmap.Handle:= LoadBitmap(HInstance, 'SBLEFTDN2')
      else Bitmap.Handle:= LoadBitmap(HInstance, 'SBLEFT2');
    end
    else
      Bitmap.Handle:= LoadBitmap(HInstance, 'SBLEFTDIS2');
    Draw(0, 0, Bitmap);

    { paint right button }
    if CanScrollRight then
    begin
      if Down and (Current = sbRight) then
        Bitmap.Handle:= LoadBitmap(HInstance, 'SBRIGHTDN2')
      else Bitmap.Handle:= LoadBitmap(HInstance, 'SBRIGHT2');
    end
    else
      Bitmap.Handle:= LoadBitmap(HInstance, 'SBRIGHTDIS2');
    Draw((pWidth div 2) - 1, 0, Bitmap);
  end;
end;

procedure TScroller.WMSize(var Message: TWMSize);
begin
  inherited;
  Width:= pWidth - 1;
  Height:= pHeight;
end;

procedure TScroller.SetMin(Value: Longint);
begin
  if Value < FMax then FMin:= Value;
end;

procedure TScroller.SetMax(Value: Longint);
begin
  if Value > FMin then FMax:= Value;
end;

procedure TScroller.SetPosition(Value: Longint);
begin
  if Value <> FPosition then
  begin
    if Value < Min then Value:= Min;
    if Value > Max then Value:= Max;
    FPosition:= Value;
    Invalidate;
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

function TScroller.CanScrollLeft: Boolean;
begin
  Result:= Position > Min;
end;

function TScroller.CanScrollRight: Boolean;
begin
  Result:= Position < Max;
end;

procedure TScroller.DoMouseDown(X: Integer);
begin
  if X < pWidth div 2 then Current:= sbLeft
  else Current:= sbRight;
  case Current of
    sbLeft: if not CanScrollLeft then Exit;
    sbRight: if not CanScrollRight then Exit;
  end;
  Pressed:= True;
  Down:= True;
  Invalidate;
  SetCapture(Handle);
end;

procedure TScroller.WMLButtonDown(var Message: TWMLButtonDown);
begin
  DoMouseDown(Message.XPos);
end;

procedure TScroller.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  DoMouseDown(Message.XPos);
end;

procedure TScroller.WMMouseMove(var Message: TWMMouseMove);
var
  P: TPoint;
  R: TRect;
begin
  if Pressed then
  begin
    P:= Point(Message.XPos, Message.YPos);
    R:= Rect(0, 0, pWidth div 2, pHeight);
    if Current = sbRight then OffsetRect(R, pWidth div 2, 0);
    if PtInRect(R, P) <> Down then
    begin
      Down:= not Down;
      Invalidate;
    end;
  end;
end;

procedure TScroller.WMLButtonUp(var Message: TWMLButtonUp);
var
  NewPos: Longint;
begin
  ReleaseCapture;
  Pressed:= False;

  if Down then
  begin
    Down:= False;
    NewPos:= Position;
    case Current of
      sbLeft: Dec(NewPos, Change);
      sbRight: Inc(NewPos, Change);
    end;
    Position:= NewPos;
  end;
end;


{ TTabList }

function TTabList.Add(const S: string): Integer;
begin
  Result:= inherited Add(S);
  if Tabs <> nil then
    Tabs.Invalidate;
end;

procedure TTabList.Insert(Index: Integer; const S: string);
begin
  inherited Insert(Index, S);
  if Tabs <> nil then
  begin
    if Index <= Tabs.FTabIndex then Inc(Tabs.FTabIndex);
    Tabs.Invalidate;
  end;
end;

procedure TTabList.Delete(Index: Integer);
var
  OldIndex: Integer;
begin
  OldIndex:= Tabs.Tabindex;
  inherited Delete(Index);

  if OldIndex < Count then Tabs.FTabIndex:= OldIndex
  else Tabs.FTabIndex:= Count - 1;
  Tabs.Invalidate;
  Tabs.Invalidate;
  if OldIndex = Index then Tabs.Click;  { deleted selected tab }
end;

procedure TTabList.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  if Tabs <> nil then
    Tabs.Invalidate;
end;

procedure TTabList.Clear;
begin
  inherited Clear;
  Tabs.FTabIndex:= -1;
  Tabs.Invalidate;
end;

procedure TTabList.AddStrings(Strings: TStrings);
begin
  SendMessage(Tabs.Handle, WM_SETREDRAW, 0, 0);
  inherited AddStrings(Strings);
  SendMessage(Tabs.Handle, WM_SETREDRAW, 1, 0);
  Tabs.Invalidate;
end;

{ TNewTabset }

constructor TNewTabSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= [csCaptureMouse, csDoubleClicks, csOpaque];
  Width:= 185;
  Height:= 21;

  TabPositions:= TList.Create;
  FTabHeight:= 20;

  FTabs:= TTabList.Create;
  TTabList(FTabs).Tabs:= Self;
  InitBitmaps;

  CreateScroller;

  FTabIndex:= -1;
  FFirstIndex:= 0;
  FVisibleTabs:= 0;  { set by draw routine }
  FStartMargin:= 5;
  FEndMargin:= 5;

  { initialize default values }
  FSelectedColor:= clBtnFace;
  FUnselectedColor:= clWindow;
  FBackgroundColor:= clBtnFace;
  FDitherBackground:= True;
  CreateBrushPattern(BrushBitmap);
  FAutoScroll:= True;
  FStyle:= tsStandard;
  FOwnerDrawHeight:= 20;

  ParentFont:= False;
  Font.Name:= string(DefFontData.Name);
  Font.Height:= DefFontData.Height;
  Font.Style:= [];

  { create the edge bitmaps }
  CreateEdgeParts;
end;

procedure TNewTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style:= style and not (CS_VREDRAW or CS_HREDRAW);
end;

procedure TNewTabset.CreateScroller;
begin
  Scroller:= TScroller.Create(Self);
  with Scroller do
  begin
    Parent:= Self;
    Top:= 3;
    Min:= 0;
    Max:= 0;
    Position:= 0;
    Visible:= False;
    OnClick:= ScrollClick;
  end;
end;

procedure TNewTabset.InitBitmaps;
begin
  MemBitmap:= TBitmap.Create;
  BrushBitmap:= TBitmap.Create;
end;

destructor TNewTabset.Destroy;
begin
  FTabs.Free;
  TabPositions.Free;
  DoneBitmaps;
  inherited Destroy;
end;

procedure TNewTabset.DoneBitmaps;
begin
  MemBitmap.Free;
  BrushBitmap.Free;
  ImageList.Free;
end;

procedure TNewTabset.ScrollClick(Sender: TObject);
begin
  FirstIndex:= TScroller(Sender).Position;
end;

{ cache the tab position data, and return number of visible tabs }
function TNewTabset.CalcTabPositions(Start, Stop: Integer; Canvas: TCanvas;
  First: Integer): Integer;
var
  Index: Integer;
  TabPos: TTabPos;
  W: Integer;
begin
  TabPositions.Count:= 0;  { erase all previously cached data }
  Index:= First;
  while (Start < Stop) and (Index < Tabs.Count) do
    with Canvas do
    begin
      TabPos.StartPos:= Start;
      W:= TextWidth(Tabs[Index]);

      { Owner }
      if (FStyle = tsOwnerDraw) then MeasureTab(Index, W);

      TabPos.Size:= W;
      Inc(Start, TabPos.Size + EdgeWidth);    { next usable position }

      if Start <= Stop then
      begin
        TabPositions.Add(Pointer(TabPos));    { add to list }
        Inc(Index);
      end;
    end;
  Result:= Index - First;
end;

function TNewTabset.ItemAtPos(Pos: TPoint): Integer;
var
  TabPos: TTabPos;
  i: Integer;
  found: boolean;
begin
  found:= false;
  Result:= -1;
  if (Pos.Y < 0) or (Pos.Y > ClientHeight) then exit;
  i:= 0;
  while (i < TabPositions.Count) and not(Found) do begin
    Pointer(TabPos):= TabPositions[I];
    if (Pos.X >= TabPos.StartPos) and (Pos.X <= TabPos.StartPos + TabPos.Size) then begin
      Result:= i+FirstIndex;
    end; {if}
    inc(i);
  end; {while}
end;

function TNewTabset.ItemRect(Item: Integer): TRect;
var
  TabPos: TTabPos;
begin
  Item:= Item - FirstIndex;
  if (TabPositions.Count > 0) and (Item >= 0) and (Item < TabPositions.Count) then begin
    Pointer(TabPos):= TabPositions[Item];
    Result:= Rect(TabPos.StartPos, 0, TabPos.StartPos + TabPos.Size, FTabHeight);
    InflateRect(Result, 1, -2);
  end
  else Result:= Rect(0, 0, 0, 0);
end;

procedure TNewTabset.Paint;
var
  TabStart, LastTabPos: Integer;
  TabPos: TTabPos;
  Tab: Integer;
  Leading: TEdgeType;
  Trailing: TEdgeType;
  isFirst, isLast, isSelected, isPrevSelected: Boolean;
  R: TRect;
  Y: integer;
begin
  if not HandleAllocated then Exit;

  { Set the size of the off-screen bitmap.  Make sure that it is tall enough to
    display the entire tab, even if the screen won't display it all.  This is
    required to avoid problems with using FloodFill. }
  MemBitmap.Width:= ClientWidth;
  if ClientHeight < FTabHeight + 5 then MemBitmap.Height:= FTabHeight + 5
  else MemBitmap.Height:= ClientHeight;

  MemBitmap.Canvas.Font:= Self.Canvas.Font;

  TabStart:= StartMargin + EdgeWidth;        { where does first text appear? }
  LastTabPos:= Width - EndMargin;            { tabs draw until this position }
  Scroller.Left:= Width - Scroller.Width - 2;

  { do initial calculations for how many tabs are visible }
  FVisibleTabs:= CalcTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
    FirstIndex);

  { enable the scroller if FAutoScroll = True and not all tabs are visible }
  if AutoScroll and (FVisibleTabs < Tabs.Count) then
  begin
    Dec(LastTabPos, Scroller.Width - 4);
    { recalc the tab positions }
    FVisibleTabs:= CalcTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
      FirstIndex);

    { set the scroller's range }
    Scroller.Visible:= True;
    ShowWindow(Scroller.Handle, SW_SHOW);
    Scroller.Min:= 0;
    Scroller.Max:= Tabs.Count - VisibleTabs;
    Scroller.Position:= FirstIndex;
  end
  else if VisibleTabs >= Tabs.Count then begin
    Scroller.Visible:= False;
    ShowWindow(Scroller.Handle, SW_HIDE);
  end;

  if FDoFix then begin
    FixTabPos;
    FVisibleTabs:= CalcTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
      FirstIndex);
  end;
  FDoFix:= False;

  { draw background of tab area }
  with MemBitmap.Canvas do begin
    Brush.Bitmap:= BrushBitmap;
    FillRect(Rect(0, 0, MemBitmap.Width, MemBitmap.Height));

    Pen.Width:= 1;
    if FOrientation = toDown then begin
      Pen.Color:= clBtnShadow;
      MoveTo(0, 0);
      LineTo(MemBitmap.Width + 1, 0);

      Pen.Color:= clWindowFrame;
      MoveTo(0, 1);
      LineTo(MemBitmap.Width + 1, 1);
    end
    else begin //toUp
      Pen.Color:= clBtnHighLight;
      MoveTo(0, MemBitmap.Height-1);
      LineTo(MemBitmap.Width + 1, MemBitmap.Height-1);

      Pen.Color:= clBtnFace;
      MoveTo(0, MemBitmap.Height-2);
      LineTo(MemBitmap.Width + 1, MemBitmap.Height-2);
    end;
  end;

  if FOrientation = toDown then Y:= 0 else Y:= MemBitmap.Height - FTabHeight;
  for Tab:= 0 to TabPositions.Count - 1 do
  begin
    Pointer(TabPos):= TabPositions[Tab];

    isFirst:= Tab = 0;
    isLast:= Tab = VisibleTabs - 1;
    isSelected:= Tab + FirstIndex = TabIndex;
    isPrevSelected:= (Tab + FirstIndex) - 1 = TabIndex;

    { Rule: every tab paints its leading edge, only the last tab paints a
      trailing edge }
    Trailing:= etNone;

    if isLast then begin
      if isSelected then Trailing:= etLastIsSel
      else Trailing:= etLastNotSel;
    end;

    if isFirst then begin
      if isSelected then Leading:= etFirstIsSel
      else Leading:= etFirstNotSel;
    end
    else begin { not first }
      if isPrevSelected then Leading:= etSelToNotSel
      else if isSelected then Leading:= etNotSelToSel
      else Leading:= etNotSelToNotSel;
    end;

    { draw leading edge }
    if Leading <> etNone then
      PaintEdge(TabPos.StartPos - EdgeWidth, Y, FTabHeight - 1, Leading);

    { set up the canvas }
    R:= Rect(TabPos.StartPos, Y, TabPos.StartPos + TabPos.Size, Y+FTabHeight);
    with MemBitmap.Canvas do begin
      if isSelected then Brush.Color:= SelectedColor
      else Brush.Color:= UnselectedColor;
      ExtTextOut(Handle, TabPos.StartPos, 2, ETO_OPAQUE, @R, nil, 0, nil);
    end;

    { restore font for drawing the text }
    MemBitmap.Canvas.Font:= Self.Canvas.Font;

    { Owner }
    if (FStyle = tsOwnerDraw) then
      DrawTab(MemBitmap.Canvas, R, Tab + FirstIndex, isSelected)
    else begin
      with MemBitmap.Canvas do begin
        Inc(R.Top, 2);
        if FOrientation = toUp then Inc(R.Top);
        DrawText(Handle, PChar(Tabs[Tab + FirstIndex]),
          Length(Tabs[Tab + FirstIndex]), R, DT_CENTER);
      end;
    end;

    { draw trailing edge  }
    if Trailing <> etNone then
      PaintEdge(TabPos.StartPos + TabPos.Size, y, FTabHeight - 1, Trailing);

    { draw connecting lines above and below the text }

    if Orientation = toDown then with MemBitmap.Canvas do begin
      Pen.Color:= clWindowFrame;
      MoveTo(TabPos.StartPos, FTabHeight - 1);
      LineTo(TabPos.StartPos + TabPos.Size, FTabHeight - 1);

      if isSelected then begin
        Pen.Color:= clBtnShadow;
        MoveTo(TabPos.StartPos, FTabHeight - 2);
        LineTo(TabPos.StartPos + TabPos.Size, FTabHeight - 2);
      end
      else begin
        Pen.Color:= clWindowFrame;
        MoveTo(TabPos.StartPos, 1);
        LineTo(TabPos.StartPos + TabPos.Size, 1);

        Pen.Color:= clBtnShadow;
        MoveTo(TabPos.StartPos, 0);
        LineTo(TabPos.StartPos + TabPos.Size + 1, 0);
      end;
    end {if with}
    else with MemBitmap.Canvas do begin //toUp
      Pen.Color:= clWindowFrame;
      MoveTo(TabPos.StartPos, MemBitmap.Height - FTabHeight + 0);
      LineTo(TabPos.StartPos + TabPos.Size, MemBitmap.Height - FTabHeight + 0);

      if isSelected then begin
        Pen.Color:= clBtnHighlight;
        MoveTo(TabPos.StartPos, MemBitmap.Height - FTabHeight + 1);
        LineTo(TabPos.StartPos + TabPos.Size+1, MemBitmap.Height - FTabHeight + 1);
      end
      else begin
        Pen.Color:= clBtnFace;
        MoveTo(TabPos.StartPos-1, MemBitmap.Height - 2);
        LineTo(TabPos.StartPos + TabPos.Size, MemBitmap.Height - 2);

        Pen.Color:= clBtnHighLight;
        MoveTo(TabPos.StartPos, MemBitmap.Height - 1);
        LineTo(TabPos.StartPos + TabPos.Size + 1, MemBitmap.Height - 1);
      end;
    end;
  end;

  { draw onto the screen }
  Canvas.Draw(0, 0, MemBitmap);
end;

procedure TNewTabset.CreateEdgeParts;
var
  H: Integer;
  Working: TBitmap;
  EdgePart: TEdgePart;
  MaskColor: TColor;

  procedure DrawUL(Canvas: TCanvas);
  begin
    if FOrientation = toDown then with Canvas do begin
      Pen.Color:= clBtnShadow;
      PolyLine([Point(0, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color:= UnselectedColor;
      Brush.Color:= UnselectedColor;
      Polygon([Point(3,1), Point(EdgeWidth - 1, H), Point(EdgeWidth, H),
        Point(EdgeWidth, 1), Point(3, 1)]);

      Pen.Color:= clWindowFrame;
      PolyLine([Point(0, 1), Point(EdgeWidth + 1, 1), Point(3, 1),
        Point(EdgeWidth - 1, H), Point(EdgeWidth, H)]);
    end
    else with Canvas do begin //ToUp
      Pen.Color:= clBtnHighLight;
      PolyLine([Point(0, H), Point(EdgeWidth + 1, H)]);

      Pen.Color:= UnselectedColor;
      Brush.Color:= UnselectedColor;
      Polygon([Point(3,H-1), Point(EdgeWidth - 1, 0), Point(EdgeWidth, 0),
        Point(EdgeWidth, H-1), Point(3, H-1)]);

      Pen.Color:= clBtnFace;
      PolyLine([Point(0, H-1), Point(EdgeWidth + 1, H-1)]);
      Pen.Color:= clWindowFrame;
      PolyLine([Point(3, H-1),
        Point(EdgeWidth - 1, 0), Point(EdgeWidth, 0)]);
    end;
  end;

  procedure DrawSR(Canvas: TCanvas);
  begin
    if FOrientation = toDown then with Canvas do begin
      Pen.Color:= SelectedColor;
      Brush.Color:= SelectedColor;
      Polygon([Point(EdgeWidth - 3, 1), Point(2, H), Point(0, H),
        Point(0, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color:= clBtnShadow;
      PolyLine([Point(EdgeWidth - 3, 0), Point(EdgeWidth + 1, 0),
        Point(EdgeWidth - 3, 1), Point(1, H), Point(0, H - 2)]);

      Pen.Color:= clWindowFrame;
      PolyLine([Point(EdgeWidth, 1), Point(EdgeWidth - 2, 1), Point(2, H),
        Point(-1, H)]);
    end
    else with Canvas do begin //toUp
      Pen.Color:= SelectedColor;
      Brush.Color:= SelectedColor;
      Polygon([Point(EdgeWidth - 3, H-1), Point(2, 0), Point(0, 0),
        Point(0, H), Point(EdgeWidth + 1, H)]);

      Pen.Color:= clBtnShadow;
      PolyLine([Point(EdgeWidth - 3, H-1), Point(1, 0), Point(0, 2)]);
      Pen.Color:= clBtnHighLight;
      PolyLine([Point(EdgeWidth - 3, H), Point(EdgeWidth + 1, H),
        Point(EdgeWidth - 3, H-1)]);

      Pen.Color:= clWindowFrame;
      PolyLine([Point(EdgeWidth, H-1), Point(EdgeWidth - 2, H-1), Point(2, 0),
        Point(-1, 0)]);
      Pen.Color:= clBtnFace;
      PolyLine([Point(EdgeWidth, H-1), Point(EdgeWidth - 2, H-1)]);
    end;
  end;

  procedure DrawSL(Canvas: TCanvas);
  begin
    if FOrientation = toDown then with Canvas do begin
      Pen.Color:= SelectedColor;
      Brush.Color:= SelectedColor;
      Polygon([Point(3, 0), Point(EdgeWidth - 1, H), Point(EdgeWidth, H),
        Point(EdgeWidth, 0), Point(3, 0)]);

      Pen.Color:= clBtnShadow;
      PolyLine([Point(0, 0), Point(4, 0)]);

      Pen.Color:= clBtnHighlight;
      PolyLine([Point(4, 1), Point(EdgeWidth, H + 1)]);

      Pen.Color:= clWindowFrame;
      PolyLine([Point(0, 1), Point(3, 1), Point(EdgeWidth - 1, H),
        Point(EdgeWidth, H)]);
    end
    else with Canvas do begin //toUp
      Pen.Color:= SelectedColor;
      Brush.Color:= SelectedColor;
      Polygon([Point(3, H), Point(EdgeWidth - 1, 0), Point(EdgeWidth, 0),
        Point(EdgeWidth, H), Point(3, H)]);

      Pen.Color:= clBtnHighLight;
      PolyLine([Point(0, H), Point(4, H)]);

      Pen.Color:= clBtnHighlight;
      PolyLine([Point(4, H-1), Point(EdgeWidth, -1)]);

      Pen.Color:= clBtnFace;
      PolyLine([Point(0, H-1), Point(3, H-1)]);
      Pen.Color:= clWindowFrame;
      PolyLine([Point(3, H-1), Point(EdgeWidth - 1, 0),
        Point(EdgeWidth, 0)]);
    end;
  end;

  procedure DrawUR(Canvas: TCanvas);
  begin
    if FOrientation = toDown then with Canvas do begin
      Pen.Color:= clBtnShadow;
      PolyLine([Point(-1, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color:= UnselectedColor;
      Brush.Color:= UnselectedColor;
      Polygon([Point(EdgeWidth - 3, 1), Point(1, H), Point(0, H),
        Point(0, 1), Point(EdgeWidth - 3, 1)]);

      { workaround for bug in S3 driver }
      Pen.Color:= clBtnShadow;
      PolyLine([Point(-1, 0), Point(EdgeWidth + 1, 0)]);

      Pen.Color:= clWindowFrame;
      PolyLine([Point(0, 1), Point(EdgeWidth + 1, 1), Point(EdgeWidth - 2, 1),
        Point(2, H), Point(-1, H)]);
    end
    else with Canvas do begin
      Pen.Color:= clBtnShadow;
      PolyLine([Point(-1, H), Point(EdgeWidth + 1, H)]);

      Pen.Color:= UnselectedColor;
      Brush.Color:= UnselectedColor;
      Polygon([Point(EdgeWidth - 3, H-1), Point(1, 0), Point(0, 0),
        Point(0, H-1), Point(EdgeWidth - 3, H-1)]);

      { workaround for bug in S3 driver }
      Pen.Color:= clBtnHighLight;
      PolyLine([Point(-1, H), Point(EdgeWidth + 1, H)]);

      Pen.Color:= clBtnFace;
      PolyLine([Point(0, H-1), Point(EdgeWidth + 1, H-1)]);
      Pen.Color:= clWindowFrame;
      PolyLine([Point(EdgeWidth - 2, H-1),
        Point(2, 0), Point(-1, 0)]);
    end;
  end;

var
  TempList: TImageList;
  SaveHeight: Integer;
begin
  MemBitmap.Canvas.Font:= Font;

  { Owner }
  SaveHeight:= FTabHeight;
  try
    if FStyle = tsOwnerDraw then FTabHeight:= FOwnerDrawHeight
    else FTabHeight:= MemBitmap.Canvas.TextHeight('T') + 4;

    H:= FTabHeight - 1;

    TempList:= TImageList.CreateSize(EdgeWidth, FTabHeight); {exceptions}
  except
    FTabHeight:= SaveHeight;
    raise;
  end;
  ImageList.Free;
  ImageList:= TempList;

  Working:= TBitmap.Create;
  try
    Working.Width:= EdgeWidth;
    Working.Height:= FTabHeight;
    MaskColor:= clOlive;

    for EdgePart:= Low(TEdgePart) to High(TEdgePart) do
    begin
      with Working.Canvas do
      begin
        Brush.Color:= MaskColor;
        Brush.Style:= bsSolid;
        FillRect(Rect(0, 0, EdgeWidth, FTabHeight));
      end;
      case EdgePart of
        epSelectedLeft: DrawSL(Working.Canvas);
        epUnselectedLeft: DrawUL(Working.Canvas);
        epSelectedRight: DrawSR(Working.Canvas);
        epUnselectedRight: DrawUR(Working.Canvas);
      end;
      ImageList.AddMasked(Working, MaskColor);
    end;
  finally
    Working.Free;
  end;
end;

procedure TNewTabset.PaintEdge(X, Y, H: Integer; Edge: TEdgeType);
begin
  MemBitmap.Canvas.Brush.Color:= clWhite;
  MemBitmap.Canvas.Font.Color:= clBlack;
  case Edge of
    etFirstIsSel:
      ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epSelectedLeft));
    etLastIsSel:
      ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epSelectedRight));
    etFirstNotSel:
      ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epUnselectedLeft));
    etLastNotSel:
      ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epUnselectedRight));
    etNotSelToSel:
      begin
        ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epUnselectedRight));
         ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epSelectedLeft));
      end;
    etSelToNotSel:
      begin
        ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epUnselectedLeft));
         ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epSelectedRight));
      end;
    etNotSelToNotSel:
      begin
        ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epUnselectedLeft));
         ImageList.Draw(MemBitmap.Canvas, X, Y, Ord(epUnselectedRight));
      end;
  end;
end;

procedure TNewTabset.CreateBrushPattern(Bitmap: TBitmap);
var
  X, Y: Integer;
begin
  Bitmap.Width:= 8;
  Bitmap.Height:= 8;
  with Bitmap.Canvas do
  begin
    Brush.Style:= bsSolid;
    Brush.Color:= FBackgroundColor;
    FillRect(Rect(0, 0, Width, Height));
    if FDitherBackground then
      for Y:= 0 to 7 do
        for X:= 0 to 7 do
          if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
            Pixels[X, Y]:= clWhite;     { on even/odd rows }
  end;
end;

procedure TNewTabset.FixTabPos;
var
  FLastVisibleTab: Integer;

  function GetRightSide: Integer;
  begin
    Result:= Width - EndMargin;
    if AutoScroll and (FVisibleTabs < Tabs.Count - 1) then
      Dec(Result, Scroller.Width + 4);
  end;

  function ReverseCalcNumTabs(Start, Stop: Integer; Canvas: TCanvas;
    Last: Integer): Integer;
  var
    W: Integer;
  begin
    if HandleAllocated then begin
      Result:= Last;
      while (Start >= Stop) and (Result >= 0) do with Canvas do begin
        W:= TextWidth(Tabs[Result]);
        if (FStyle = tsOwnerDraw) then MeasureTab(Result, W);
        Dec(Start, W + EdgeWidth);    { next usable position }
        if Start >= Stop then Dec(Result);
      end;
      if (Start < Stop) or (Result < 0) then Inc(Result);
    end
    else Result:= FFirstIndex;
  end;

begin
  if Tabs.Count > 0 then
  begin
    FLastVisibleTab:= FFirstIndex + FVisibleTabs - 1;
    if FTabIndex > FLastVisibleTab then
      FFirstIndex:= ReverseCalcNumTabs(GetRightSide, StartMargin + EdgeWidth,
        Canvas, FTabIndex)
    else if (FTabIndex >= 0) and (FTabIndex < FFirstIndex) then
      FFirstIndex:= FTabIndex;
  end;
end;

procedure TNewTabset.SetSelectedColor(Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor:= Value;
    CreateEdgeParts;
    Invalidate;
  end;
end;

procedure TNewTabset.SetUnselectedColor(Value: TColor);
begin
  if Value <> FUnselectedColor then begin
    FUnselectedColor:= Value;
    CreateEdgeParts;
    Invalidate;
  end;
end;

procedure TNewTabset.SetBackgroundColor(Value: TColor);
begin
  if Value <> FBackgroundColor then begin
    FBackgroundColor:= Value;
    CreateBrushPattern(BrushBitmap);
    MemBitmap.Canvas.Brush.Style:= bsSolid;
    Invalidate;
  end;
end;

procedure TNewTabset.SetDitherBackground(Value: Boolean);
begin
  if Value <> FDitherBackground then begin
    FDitherBackground:= Value;
    CreateBrushPattern(BrushBitmap);
    MemBitmap.Canvas.Brush.Style:= bsSolid;
    Invalidate;
  end;
end;

procedure TNewTabset.SetOrientation(Value: TTabOrientation);
begin
  if Value <> FOrientation then begin
    FOrientation:= Value;
    CreateEdgeParts;
    Invalidate;
  end;
end;

procedure TNewTabset.SetAutoScroll(Value: Boolean);
begin
  if Value <> FAutoScroll then begin
    FAutoScroll:= Value;
    Scroller.Visible:= False;
    ShowWindow(Scroller.Handle, SW_HIDE);
    Invalidate;
  end;
end;

procedure TNewTabset.SetStartMargin(Value: Integer);
begin
  if Value <> FStartMargin then begin
    FStartMargin:= Value;
    Invalidate;
  end;
end;

procedure TNewTabset.SetEndMargin(Value: Integer);
begin
  if Value <> FEndMargin then begin
    FEndMargin:= Value;
    Invalidate;
  end;
end;

function TNewTabset.CanChange(NewIndex: Integer): Boolean;
begin
  Result:= True;
  if Assigned(FOnChange) then FOnChange(Self, NewIndex, Result);
end;

procedure TNewTabset.SetTabIndex(Value: Integer);
begin
  if Value <> FTabIndex then begin
    if (Value < -1) or (Value >= Tabs.Count) then
      raise Exception.Create(SInvalidTabIndex);
    if CanChange(Value) then begin
      FTabIndex:= Value;
      FixTabPos;
      Click;
      Invalidate;
    end;
  end;
end;

procedure TNewTabset.SelectNext(Direction: Boolean);
var
  NewIndex: Integer;
begin
  if Tabs.Count > 1 then begin
    NewIndex:= TabIndex;
    if Direction then Inc(NewIndex)
    else Dec(NewIndex);
    if NewIndex = Tabs.Count then NewIndex:= 0
    else if NewIndex < 0 then NewIndex:= Tabs.Count - 1;
    SetTabIndex(NewIndex);
  end;
end;

procedure TNewTabset.SetFirstIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < Tabs.Count) then begin
    FFirstIndex:= Value;
    Invalidate;
  end;
end;

procedure TNewTabset.SetTabList(Value: TStrings);
begin
  FTabs.Assign(Value);
  FTabIndex:= -1;
  if FTabs.Count > 0 then TabIndex:= 0
  else Invalidate;
end;

procedure TNewTabset.SetTabStyle(Value: TTabStyle);
begin
  if Value <> FStyle then begin
    FStyle:= Value;
    CreateEdgeParts;
    Invalidate;
  end;
end;

procedure TNewTabset.SetTabHeight(Value: Integer);
var
  SaveHeight: Integer;
begin
  if Value <> FOwnerDrawHeight then begin
    SaveHeight:= FOwnerDrawHeight;
    try
      FOwnerDrawHeight:= Value;
      CreateEdgeParts;
      Invalidate;
    except
      FOwnerDrawHeight:= SaveHeight;
      raise;
    end;
  end;
end;

procedure TNewTabset.DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
  Selected: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabCanvas, R, Index, Selected);
end;

procedure TNewTabset.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TNewTabset.MeasureTab(Index: Integer; var TabWidth: Integer);
begin
  if Assigned(FOnMeasureTab) then
    FOnMeasureTab(Self, Index, TabWidth);
end;

procedure TNewTabset.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  TabPos: TTabPos;
  I: Integer;
  Extra: Integer;
  MinLeft: Integer;
  MaxRight: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (Y <= FTabHeight) then begin
    if Y < FTabHeight div 2 then Extra:= EdgeWidth div 3
    else Extra:= EdgeWidth div 2;

    for I:= 0 to TabPositions.Count - 1 do begin
      Pointer(TabPos):= TabPositions[I];
      MinLeft:= TabPos.StartPos - Extra;
      MaxRight:= TabPos.StartPos + TabPos.Size + Extra;
      if (X >= MinLeft) and (X <= MaxRight) then begin
        SetTabIndex(FirstIndex + I);
        Break;
      end;
    end;
  end;
end;

procedure TNewTabset.WMSize(var Message: TWMSize);
var
  NumVisTabs, LastTabPos: Integer;

  function CalcNumTabs(Start, Stop: Integer; Canvas: TCanvas;
    First: Integer): Integer;
  var
    W: Integer;
  begin
    Result:= First;
    while (Start < Stop) and (Result < Tabs.Count) do with Canvas do begin
      W:= TextWidth(Tabs[Result]);
      if (FStyle = tsOwnerDraw) then MeasureTab(Result, W);
      Inc(Start, W + EdgeWidth);    { next usable position }
      if Start <= Stop then Inc(Result);
    end;
  end;

begin
  inherited;
  if Tabs.Count > 1 then begin
    LastTabPos:= Width - EndMargin;
    NumVisTabs:= CalcNumTabs(StartMargin + EdgeWidth, LastTabPos, Canvas, 0);
    if (FTabIndex = Tabs.Count) or (NumVisTabs > FVisibleTabs) or
      (NumVisTabs = Tabs.Count) then FirstIndex:= Tabs.Count - NumVisTabs;
    FDoFix:= True;
  end;
  Invalidate;
end;

procedure TNewTabset.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  CreateEdgeParts;
  CreateBrushPattern(BrushBitmap);
  MemBitmap.Canvas.Brush.Style:= bsSolid;
  { Windows follows this message with a WM_PAINT }
end;

procedure TNewTabset.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font:= Font;
  CreateEdgeParts;
  Invalidate;
end;

procedure TNewTabset.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result:= DLGC_WANTALLKEYS;
end;

procedure TNewTabset.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I:= 0 to FTabs.Count - 1 do begin
    if IsAccel(Message.CharCode, FTabs[I]) then
    begin
      Message.Result:= 1;
      if FTabIndex <> I then SetTabIndex(I);
      Exit;
    end;
  end;
  inherited;
end;

procedure TNewTabset.DefineProperties(Filer: TFiler);
begin
  { Can be removed after version 1.0 }
  if Filer is TReader then inherited DefineProperties(Filer);
  Filer.DefineProperty('TabOrder', ReadIntData, nil, False);
  Filer.DefineProperty('TabStop', ReadBoolData, nil, False);
end;

procedure TNewTabset.ReadIntData(Reader: TReader);
begin
  Reader.ReadInteger;
end;

procedure TNewTabset.ReadBoolData(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

end.

