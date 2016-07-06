unit Unit2;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Windows, ComObj, ActiveX, Comctrls, Life32_TLB,
  unit1, LifeBox, LifeGen, snapshot, StdVcl;
type
  TLifeApplication = class;
  TLifeWindow = class;
  TLifeUniverse = class;
  TLifeSelection = class;
  TLifeToolbar = class;
  TLifeStatusbar = class;
  TLifeWindows = class;
  TLifeScrapbook = class;
  TLifeScraplet = class;
  TLifeSnapshots = class;
  TLifeSidepanel = class;
  TLifeSnapshot = class;

  TLifeSelection = class(TAutoObject, ILifeSelection)
  private
    FMyRect: TRect;
    FLifeBox: TLifeBox;
    procedure Init(ALifeBox: TLifeBox); virtual;
    property MyRect: TRect read FMyRect write FMyRect;
  protected
    function Get_Bottom: Integer; safecall;
    function Get_Left: Integer; safecall;
    function Get_Top: Integer; safecall;
    function Get_Right: Integer; safecall;
    procedure Clear; safecall;
    procedure Copy; safecall;
    procedure Cut; safecall;
    procedure DrawBox; safecall;
    procedure FillBlack; safecall;
    procedure FillRandom; safecall;
    procedure Invert; safecall;
    procedure Paste; safecall;
    procedure Set_Bottom(Value: Integer); safecall;
    procedure Set_Left(Value: Integer); safecall;
    procedure Set_Top(Value: Integer); safecall;
    procedure Set_Right(Value: Integer); safecall;
    function Get_CellCount: Integer; safecall;
    function Get_AsText: WideString; safecall;
    procedure MoveBy(dx, dy: Integer); safecall;
    procedure Set_AsText(const Value: WideString); safecall;
    procedure ClearOutsideSelection; safecall;
    procedure MirrorHorz; safecall;
    procedure MirrorVert; safecall;
    procedure Rotate180; safecall;
    procedure Rotate270; safecall;
    procedure Rotate90; safecall;
    function Get_Universe: LifeUniverse; safecall;
  end;

  TLifeApplication = class(TAutoObject, ILifeApplication)
  private
    MainForm: TLife32MainForm;
    MyLifeBox: TLifeWindow;
    FToolbar: TLifeToolbar;
    FStatusbar: TLifeStatusbar;
    FSidepanel: TLifeSidepanel;
  protected
    function Get_ActiveWindow: LifeWindow; safecall;
    procedure Close; safecall;
    procedure Pause; safecall;
    procedure Play(step: Integer); safecall;
    procedure RegisterKeyHandler(const Name: WideString); safecall;
    procedure UnregisterKeyhandler(const Name: WideString); safecall;
    procedure SkipTo(Generation: Integer); safecall;
    function Get_PlaySpeed: Integer; safecall;
    procedure Set_PlaySpeed(Value: Integer); safecall;
    function Get_ShowScrollbar: WordBool; safecall;
    function Get_Statusbar: LifeStatusbar; safecall;
    function Get_Toolbar: LifeToolbar; safecall;
    procedure Set_ShowScrollbar(Value: WordBool); safecall;
    function Get_CursorMode: eCursorMode; safecall;
    function Get_PasteMode: ePasteMode; safecall;
    procedure Set_CursorMode(Value: eCursorMode); safecall;
    procedure Set_PasteMode(Value: ePasteMode); safecall;
    function Get_DirectXEnabled: WordBool; safecall;
    procedure Set_DirectXEnabled(Value: WordBool); safecall;
    function Get_Speed: Integer; safecall;
    procedure Set_Speed(Value: Integer); safecall;
    function Get_WindowState: eWindowState; safecall;
    procedure Set_WindowState(Value: eWindowState); safecall;
    procedure SetFocus; safecall;
    procedure MessageBox(const Message: WideString); safecall;
    function Ask(const Message: WideString): WideString; safecall;
    function Get_Zoom: Integer; safecall;
    procedure Set_Zoom(Value: Integer); safecall;
    function Get_Sidepanel: LifeSidepanel; safecall;
    function Get_FrameDropInterval: Integer; safecall;
    procedure Set_FrameDropInterval(Value: Integer); safecall;
    function Get_Colors: LifeColors; safecall;
    function Get_Windows: LifeWindows; safecall;
    function Get_Scrapbook: LifeScrapbook; safecall;
    procedure Set_Scrapbook(const Value: LifeScrapbook); safecall;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    property ActiveWindow: LifeWindow read Get_ActiveWindow;
  end;

  TLifeWindow = class(TAutoObject, ILifeWindow)
  private
    FLifeBox: TLifeBox;
    FUniverse: TLifeUniverse;
    FApplication: TLifeApplication;
    MainForm: TLife32MainForm;
    FSelection: TLifeSelection;
    procedure Init(ALifeBox: TLifeBox; MyApp: TLifeApplication); virtual;
  protected
    function Get_Application: LifeApplication; safecall;
    function Get_Height: Integer; safecall;
    function Get_Selection: LifeSelection; safecall;
    function Get_Universe: LifeUniverse; safecall;
    function Get_Width: Integer; safecall;
    procedure CenterOnPattern; safecall;
    procedure MoveBy(dx, dy: Integer); safecall;
    procedure MoveTo(x, y: Integer); safecall;
    procedure RandomDot; safecall;
    procedure SelectAll; safecall;
    procedure ZoomToFit; safecall;
    procedure ZoomToSelection; safecall;
    procedure Redraw; safecall;
    function Get_ShowGrid: WordBool; safecall;
    procedure Set_ShowGrid(Value: WordBool); safecall;
    function Get_Zoom: Integer; safecall;
    procedure Set_Zoom(Value: Integer); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function MakeSnapshot: LifeSnapshots; safecall;
    function Get_Snapshots: LifeSnapshots; safecall;
    procedure Set_Snapshots(const Value: LifeSnapshots); safecall;
  public
    destructor Destroy; override;
  end;

  TLifeUniverse = class(TAutoObject, ILifeUniverse)
  private
    FUniverse: TUniverse;
    FIsClone: boolean;
    procedure Init(AUniverse: TUniverse); virtual;
  protected
    function Get_Generation: Integer; safecall;
    function Get_Rules: WideString; safecall;
    procedure Set_Rules(const Value: WideString); safecall;
    procedure SaveToFile(const AFile: WideString); safecall;
    procedure LoadFromFile(const AFile: WideString); safecall;
    function CellState(x, y: Integer): WordBool; safecall;
    procedure ChangeCell(x, y: Integer; State: WordBool); safecall;
    function Get_CellCount: Integer; safecall;
    function Get_Height: Integer; safecall;
    function Get_Width: Integer; safecall;
    function Clone: LifeUniverse; safecall;
    procedure DrawLine(x1, y1, x2, y2: Integer; Fill: WordBool); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_Rule: WideString; safecall;
    procedure Set_Rule(const Value: WideString); safecall;
  public
    property Generation: Integer read Get_Generation;
    property Rules: WideString read Get_Rules write Set_Rules;
    property IsClone: boolean read FIsClone;
    destructor Destroy; override;
  end;

  TLifeToolbar = class(TAutoObject, ILifeToolbar)
  private
    MainForm: TLife32MainForm;
    procedure Init(AMainForm: TLife32MainForm);
  protected
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
  end;

  TLifeStatusbar = class(TAutoObject, ILifeStatusbar)
  private
    MainForm: TLife32MainForm;
    procedure Init(AMainForm: TLife32MainForm);
  protected
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
  end;

  {$HINTS OFF}
  TLifeWindows = class(TAutoObject, ILifeWindows)
  private
    MainForm: TLife32MainForm;
    procedure Init(AMainForm: TLife32MainForm);
  protected
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): LifeWindow; safecall;
    function New: LifeWindow; safecall;
    function Next: LifeWindow; safecall;
    function Previous: LifeWindow; safecall;
    procedure Delete(index: Integer); safecall;
    procedure Set_Item(index: Integer; const Value: LifeWindow); safecall;
  end;

  TLifeScrapbook = class(TAutoObject, ILifeScrapbook)
  private
    Scrapbook: TListView;
    procedure Init(AScrapbook: TListView);
  protected
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): LifeScraplet; safecall;
    function Insert(const Universe: LifeUniverse): LifeScraplet; safecall;
    function Next: LifeScraplet; safecall;
    function Previous: LifeScraplet; safecall;
    procedure Delete(index: Integer); safecall;
    procedure Set_Count(Value: Integer); safecall;
    procedure Set_Item(index: Integer; const Value: LifeScraplet); safecall;
  end;

  TLifeScraplet = class(TAutoObject, ILifeScraplet)
  private
    ListItem: TListItem;
    procedure Init(AListItem: TListItem);
  protected
    function Get_Caption: WideString; safecall;
    function Get_ShortcutKey: WideString; safecall;
    function Get_Universe: LifeUniverse; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_ShortcutKey(const Value: WideString); safecall;
    procedure Set_Universe(const Value: LifeUniverse); safecall;
  end;
{$HINTS ON}

  TLifeSnapshots = class(TAutoObject, ILifeSnapshots)
  private
    SnapshotList: TSnapshotList;
    procedure Init(ASnapshotList: TSnapshotList);
  protected
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): LifeSnapshot; safecall;
    function Get_Window: LifeWindow; safecall;
    procedure Delete(index: Integer); safecall;
    procedure MakeSnapshot(Cause: Integer); safecall;
    procedure RevertToSnapshot(index: Integer); safecall;
    procedure SortBy(SortKey: Integer; Ascending: WordBool); safecall;
  end;

  TLifeSidepanel = class(TAutoObject, ILifeSidepanel)
  private
    MainForm: TLife32MainForm;
    procedure Init(AMainForm: TLife32MainForm);
  protected
    function Get_PageIndex: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_Width: Integer; safecall;
    procedure Set_PageIndex(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure Set_Width(Value: Integer); safecall;
  end;

{$HINTS OFF}
  TLifeSnapshot = class(TAutoObject, ILifeSnapshot)
  private
    ListItem: TListItem;
    procedure Init(AListItem: TListItem);
  protected
    function Get_Cause: Integer; safecall;
    function Get_Filename: Integer; safecall;
    function Get_Generation: Integer; safecall;
    function Get_PatternNumber: Integer; safecall;
    function Get_RevisionNumber: Integer; safecall;
    procedure Set_Cause(Value: Integer); safecall;
    procedure Set_Filename(Value: Integer); safecall;
    procedure Set_Generation(Value: Integer); safecall;
    procedure Set_PatternNumber(Value: Integer); safecall;
    procedure Set_RevisionNumber(Value: Integer); safecall;
  end;
{$HINTS ON}

implementation

uses
  Classes, ComServ, SysUtils, Forms, Dialogs, LifeConst, LifeCel, Menus;

procedure TLifeApplication.Initialize;
begin
  //MessageBox(0,PChar(IntToStr(ComServer.ObjectCount)),'ObjectCount',0);
  inherited Initialize;
  MainForm:= Application.MainForm as TLife32MainForm;
end;

destructor TLifeApplication.Destroy;
begin
  MyLifeBox.Free;
  inherited Destroy;
end;

function TLifeApplication.Get_ActiveWindow: LifeWindow;
begin
  if MyLifeBox = nil then begin
    MyLifeBox:= TLifeWindow.Create;
    MyLifeBox.Init(MainForm.LifeBox1, Self);
  end; {if}
  Result:= MyLifeBox as ILifeWindow;
  Result._AddRef;
end;

function TLifeApplication.Get_Statusbar: LifeStatusbar;
begin
  if FStatusbar = nil then begin
    FStatusbar:= TLifeStatusbar.Create;
    FStatusbar.Init(MainForm);
  end; {if}
  Result:= FStatusbar as ILifeStatusBar;
  Result._AddRef;
end;

function TLifeApplication.Get_Toolbar: LifeToolbar;
begin
  if FToolbar = nil then begin
    FToolbar:= TLifeToolbar.Create;
    FToolbar.Init(MainForm);
  end; {if}
  Result:= FToolbar as ILifeToolBar;
  Result._AddRef;
end;

function TLifeApplication.Get_ShowScrollbar: WordBool;
begin
  Result:= not(MainForm.HideScrollbars);
end;

procedure TLifeApplication.Set_ShowScrollbar(Value: WordBool);
begin
  MainForm.HideScrollbars:= not(value);
end;

function TLifeApplication.Get_PlaySpeed: Integer;
begin
  Result:= MainForm.PlaySpeed;
end;

procedure TLifeApplication.Set_PlaySpeed(Value: Integer);
begin
  MainForm.PlaySpeed:= Value;
end;

procedure TLifeApplication.Play(step: Integer);
var
  TargetGen: integer;
begin
  if MainForm = nil then begin
    MainForm:= Application.MainForm as TLife32MainForm;
  end;
  if step = 0 then MainForm.PlayButton.Click
  else begin
    TargetGen:= MainForm.Generation + step;
    MainForm.Generation:= TargetGen;
  end; {else}
end;

procedure TLifeApplication.SkipTo(Generation: Integer);
begin
  MainForm.ShowGenerations:= true;
  MainForm.Generation:= Generation;
end;

procedure TLifeApplication.Pause;
begin
  if MainForm = nil then begin
    MainForm:= Application.MainForm as TLife32MainForm;
  end;
  MainForm.PauseButton.Click;
end;

procedure TLifeApplication.Close;
begin
  Pause;
  Free;
  MainForm.Close;
end;

procedure TLifeApplication.RegisterKeyHandler(const Name: WideString);
begin

end;

procedure TLifeApplication.UnregisterKeyhandler(const Name: WideString);
begin

end;

procedure TLifeWindow.init(ALifeBox: TLifeBox; MyApp: TLifeApplication);
begin
  FLifeBox:= ALifeBox;
  FApplication:= MyApp;
  MainForm:= (MyApp.MainForm as TLife32MainForm);
end;

destructor TLifeWindow.Destroy;
begin
  FSelection.Free;
  FUniverse.Free;
  inherited Destroy;
end;

function TLifeWindow.Get_Application: LifeApplication;
begin
  Result:= FApplication as ILifeApplication;
  Result._AddRef;
end;

function TLifeWindow.Get_Universe: LifeUniverse;
begin
  if not(Assigned(FUniverse)) then begin
    FUniverse:= TLifeUniverse.Create;
    FUniverse.Init(FLifeBox.Universe);
  end; {if}
  Result:= FUniverse as ILifeUniverse;
  FUniverse._AddRef;
end;

function TLifeWindow.Get_Selection: LifeSelection;
begin
  if FSelection = nil then begin
    FSelection:= TLifeSelection.Create;
    FSelection.Init(FLifeBox);
  end; {if}
  Result:= FSelection as ILifeSelection;
  Result._AddRef;
end;

function TLifeWindow.Get_ShowGrid: WordBool;
begin
  Result:= FLifeBox.Grid;
end;

procedure TLifeWindow.Set_ShowGrid(Value: WordBool);
begin
  FLifeBox.Grid:= Value;
end;

function TLifeWindow.Get_Height: Integer;
begin
  Result:= FLifeBox.CelsDown;
end;

function TLifeWindow.Get_Width: Integer;
begin
  Result:= FLifeBox.CelsAcross;
end;

procedure TLifeWindow.ZoomToFit;
begin
  FLifeBox.ZoomToFit(true,10);
end;

procedure TLifeWindow.CenterOnPattern;
begin
  FLifeBox.CenterOnPattern(true);
end;

procedure TLifeWindow.MoveBy(dx, dy: Integer);
begin
  FLifeBox.MoveBy(dx,dy);
end;

procedure TLifeWindow.MoveTo(x, y: Integer);
begin
  FLifeBox.MoveTo(x,y);
end;

procedure TLifeWindow.SelectAll;
begin
  FLifeBox.SelectAll(true);
  if not Assigned(FSelection) then Get_Selection;
  FSelection.MyRect:= FlifeBox.SelectionRect;
end;

procedure TLifeWindow.ZoomToSelection;
begin
  FLifeBox.ZoomToSelection(MaxZoom);
end;

procedure TLifeWindow.RandomDot;
begin
  FLifeBox.RandomDot;
end;

procedure TLifeWindow.Redraw;
begin
  FLifeBox.RedrawAll;
end;

procedure TLifeUniverse.Init(AUniverse: TUniverse);
begin
  FUniverse:= AUniverse;
end;

destructor TLifeUniverse.Destroy;
begin
  if IsClone then FUniverse.Free;
  inherited Destroy;
end;

function TLifeUniverse.Get_Generation: Integer;
begin
  Result:= FUniverse.Counter;
end;

function TLifeUniverse.Get_Rules: WideString;
begin
  Result:= FUniverse.RuleString;
end;

procedure TLifeUniverse.Set_Rules(const Value: WideString);
begin
  FUniverse.RuleString:= value;
end;

function TLifeUniverse.Get_CellCount: Integer;
begin
  Result:= FUniverse.CountCelsNow;
end;

function TLifeUniverse.Get_Height: Integer;
var
  BoundingBox: TRect;
begin
  BoundingBox:= FUniverse.GetBoundingBox;
  Result:= BoundingBox.Bottom - BoundingBox.Top;
end;

function TLifeUniverse.Get_Width: Integer;
var
  BoundingBox: TRect;
begin
  BoundingBox:= FUniverse.GetBoundingBox;
  Result:= BoundingBox.Right - BoundingBox.Left;
end;

procedure TLifeUniverse.SaveToFile(const AFile: WideString);
begin
  FUniverse.SaveToFile(AFile,1,false);
end;

procedure TLifeUniverse.LoadFromFile(const AFile: WideString);
begin
  FUniverse.LoadFromFile(AFile);
end;

function TLifeUniverse.CellState(x, y: Integer): WordBool;
begin
  Result:= FUniverse.CelState(x,y)
end;

procedure TLifeUniverse.ChangeCell(x, y: Integer; State: WordBool);
begin
  FUniverse.ChangeCel(x,y,State);
end;

procedure TLifeUniverse.DrawLine(x1, y1, x2, y2: Integer; Fill: WordBool);
begin
  if Fill then FUniverse.DrawLine(x1,y1,x2,y2,lds_on)
  else FUniverse.DrawLine(x1,y1,x2,y2,lds_off);
end;

function TLifeUniverse.Clone: LifeUniverse;
var
  AUniverse: TLifeUniverse;
begin
  AUniverse:= TLifeUniverse.Create;
  try
    AUniverse.Init(FUniverse.Clone);
    AUniverse.FIsClone:= true;
    Result:= AUniverse as ILifeUniverse;
    Result._AddRef;
  except
    Result:= nil;
  end;
end;

procedure TLifeSelection.Init(ALifeBox: TLifeBox);
begin
  FLifeBox:= ALifeBox;
end;

function TLifeSelection.Get_Left: Integer;
begin
  Result:= FMyRect.Left;
end;

function TLifeSelection.Get_Top: Integer;
begin
  Result:= FMyRect.Top;
end;

function TLifeSelection.Get_Right: Integer;
begin
  Result:= FMyRect.Right;
end;

function TLifeSelection.Get_Bottom: Integer;
begin
  Result:= FMyRect.Bottom;
end;

procedure TLifeSelection.Set_Left(Value: Integer);
begin
  FMyRect.Left:= Value;
  FLifeBox.SelectionRect:= MyRect;
end;

procedure TLifeSelection.Set_Top(Value: Integer);
begin
  FMyRect.Top:= Value;
  FLifeBox.SelectionRect:= MyRect;
end;

procedure TLifeSelection.Set_Right(Value: Integer);
begin
  FMyRect.Right:= value;
  FLifeBox.SelectionRect:= MyRect;
end;

procedure TLifeSelection.Set_Bottom(Value: Integer);
begin
  FMyRect.Bottom:= value;
  FLifeBox.SelectionRect:= MyRect;
end;

function TLifeSelection.Get_CellCount: Integer;
var
  TempCutOut: TUniverse;
begin
  TempCutOut:= FLifeBox.Universe.CopyRect(MyRect,cNoClipboard);
  Result:= TempCutOut.CountCelsNow;
  TempCutOut.Free;
end;

function TLifeSelection.Get_AsText: WideString;
var
  CutOut: TUniverse;
  TempStringList: TStringlist;
begin
  Result:= '#c !error occurred';
  CutOut:= FLifeBox.Universe.CopyRect(MyRect,cNoClipBoard);
  CutOut.ResetBoundingBox;
  try
    TempStringList:= CutOut.SaveToStringList(smDefault,false);
  finally
    CutOut.Free;
  end; {try}
  try
    Result:= TempStringList.Text;
  finally
    TempStringList.Free;
  end; {try} (**)
end;

procedure TLifeSelection.Set_AsText(const Value: WideString);
var
  TempStringList: TStringlist;
  CutOut: TUniverse;
begin
  TempStringList:= TStringList.Create;
  try
    TempStringList.Text:= Value;
    CutOut:= TUniverse.Create('',nbDefault); //Rules don't matter
    try
      CutOut.LoadFromStringList(TempStringList);
      //start inserting at top-left of selection and ignore offset in Text.
      CutOut.ResetBoundingBox;
      FLifeBox.InsertShape(CutOut);
    finally
      CutOut.Free;
    end;
  finally
    TempStringList.Free;
  end;
end;

procedure TLifeSelection.Clear;
begin
  FLifeBox.ClearSelection;
end;

procedure TLifeSelection.Copy;
begin
  FLifeBox.CopySelection;
end;

procedure TLifeSelection.Cut;
begin
  FLifeBox.CutSelection;
end;

procedure TLifeSelection.DrawBox;
begin
  FLifeBox.DrawBox;
end;

procedure TLifeSelection.FillBlack;
begin
  FLifeBox.FillBlack;
end;

procedure TLifeSelection.FillRandom;
begin
  FLifeBox.FillRandom;
end;

procedure TLifeSelection.Invert;
begin
  FLifeBox.InvertSelection;
end;

procedure TLifeSelection.Paste;
begin
  FLifeBox.PasteSelection
end;

procedure TLifeSelection.MoveBy(dx, dy: Integer);
var
  CutOut: TUniverse;
begin
  CutOut:= FLifeBox.Universe.CutRect(MyRect,cNoClipboard);
  try
    OffsetRect(FMyRect,dx,dy);
    FLifeBox.SelectionRect:= MyRect;
    FLifeBox.InsertShape(CutOut);
  finally
    CutOut.Free;
  end;
end;

procedure TLifeSelection.ClearOutsideSelection;
begin
  FLifeBox.ClearOutsideSelection;
end;

procedure TLifeSelection.MirrorHorz;
begin
  FLifeBox.MirrorSelHorz;
end;

procedure TLifeSelection.MirrorVert;
begin
  FLifeBox.MirrorSelVert;
end;

procedure TLifeSelection.Rotate180;
begin
  FLifeBox.RotateSel180;
end;

procedure TLifeSelection.Rotate270;
begin
  FLifeBox.RotateSel270;
end;

procedure TLifeSelection.Rotate90;
begin
  FLifeBox.RotateSel90;
end;

procedure TLifeToolbar.Init(AMainForm: TLife32MainForm);
begin
  MainForm:= AMainForm;
end;

function TLifeToolbar.Get_Visible: WordBool;
begin
  Result:= not(MainForm.HideScrollbars);
end;

procedure TLifeToolbar.Set_Visible(Value: WordBool);
begin
  MainForm.HideScrollbars:= not(value);
end;

procedure TLifeStatusbar.Init(AMainForm: TLife32MainForm);
begin
  MainForm:= AMainForm;
end;

function TLifeStatusbar.Get_Visible: WordBool;
begin
  Result:= MainForm.Statusbar.Visible;
end;

procedure TLifeStatusbar.Set_Visible(Value: WordBool);
begin
  MainForm.Statusbar.Visible:= value;
end;

function TLifeUniverse.Get_Description: WideString;
begin
  Result:= FUniverse.Description.Text;
end;

procedure TLifeUniverse.Set_Description(const Value: WideString);
begin
  FUniverse.Description.Text:= Value;
end;

function TLifeApplication.Get_CursorMode: eCursorMode;
begin
  Result:= MainForm.LifeBox1.EditorMode;
end;

function TLifeApplication.Get_PasteMode: ePasteMode;
begin
  Result:= MainForm.LifeBox1.PasteMode;
end;

procedure TLifeApplication.Set_CursorMode(Value: eCursorMode);
begin
  MainForm.LifeBox1.EditorMode:= value;
end;

procedure TLifeApplication.Set_PasteMode(Value: ePasteMode);
begin
  MainForm.LifeBox1.PasteMode:= value;
end;

function TLifeApplication.Get_DirectXEnabled: WordBool;
begin
  Result:= (DDFast and LifeCel.MyDDSurface.DirectDrawEnabled) = DDFast;
end;

procedure TLifeApplication.Set_DirectXEnabled(Value: WordBool);
begin
  MyDDSurface.DirectDrawEnabled:= MyDDSurface.DirectDrawEnabled or DDfast;
end;

function TLifeApplication.Get_Speed: Integer;
begin
  Result:= MainForm.PlaySpeed;
end;

procedure TLifeApplication.Set_Speed(Value: Integer);
begin
  MainForm.PlaySpeed:= value;
end;

function TLifeWindow.Get_Zoom: Integer;
begin
  Result:= Self.FLifeBox.PixelsPerCel;
end;

procedure TLifeWindow.Set_Zoom(Value: Integer);
begin
  FLifeBox.PixelsPerCel:= value;
end;

function TLifeApplication.Get_WindowState: eWindowState;
begin
  case MainForm.WindowState of
    wsMaximized: result:= ewsMaximized;
    wsMinimized: result:= ewsMinimized;
    wsNormal: result:= ewsNormal;
  end; {case}
end;

procedure TLifeApplication.Set_WindowState(Value: eWindowState);
begin
  case Value of
    ewsMaximized: MainForm.WindowState:= wsMaximized;
    ewsMinimized: MainForm.WindowState:= wsMinimized;
    ewsNormal:  MainForm.WindowState:= wsNormal;
  end; {case}
end;

procedure TLifeApplication.SetFocus;
begin
  Self.MainForm.SetFocus;
end;

procedure TLifeApplication.MessageBox(const Message: WideString);
begin
  ShowMessage(message);
end;

function TLifeApplication.Ask(const Message: WideString): WideString;
begin
  Result:= Self.MainForm.Ask(Message);
end;

function TLifeApplication.Get_Zoom: Integer;
begin
  Result:= MainForm.LifeBox1.PixelsPerCel;
end;

procedure TLifeApplication.Set_Zoom(Value: Integer);
begin
  MainForm.LifeBox1.PixelsPerCel:= value;
end;

function TLifeWindow.Get_Caption: WideString;
begin
  Result:= MainForm.Caption;
end;

procedure TLifeWindow.Set_Caption(const Value: WideString);
begin
  MainForm.Caption:= Value;
end;

function TLifeSelection.Get_Universe: LifeUniverse;
var
  Temp: TLifeUniverse;
begin
  Temp:= TLifeUniverse.Create;
  Temp.Init(FLifeBox.GetCutOut);
  Result:= Temp as ILifeUniverse;
  if Assigned(Result) then Result._AddRef;
end;

function TLifeWindow.MakeSnapshot: LifeSnapshots;
var
  Temp: TLifeSnapshots;
begin
  Temp:= TLifeSnapShots.Create;
  Temp.Init(FApplication.MainForm.RewindList1);
  FApplication.MainForm.MakeSnapshot1.Click;
  Result:= Temp as ILifeSnapshots;
  if Assigned(Result) then Result._AddRef;
end;

function TLifeApplication.Get_Sidepanel: LifeSidepanel;
begin
  if FSidepanel = nil then begin
    FSidepanel:= TLifeSidepanel.Create;
    FSidepanel.Init(MainForm);
  end;
  Result:= FSidepanel as ILifeSidepanel;
  Result._AddRef;
end;


function TLifeWindow.Get_Snapshots: LifeSnapshots;
begin

end;

procedure TLifeWindow.Set_Snapshots(const Value: LifeSnapshots);
begin

end;

procedure TLifeWindows.Init(AMainForm: TLife32MainForm);
begin
  MainForm:= AMainForm;
end;

function TLifeWindows.Get_Count: Integer;
begin
  Result:= MainForm.UniverseTabset.Tabs.Count;
end;

function TLifeWindows.Get_Item(index: Integer): LifeWindow;
begin

end;

function TLifeWindows.New: LifeWindow;
begin
  MainForm.Newsheet1Click(MainForm);
end;

function TLifeWindows.Next: LifeWindow;
begin
  with MainForm do begin
    if (UniverseTabset.TabIndex < UniverseTabset.Tabs.Count -1) then
      UniverseTabset.SelectNext(true);
  end; {with}
end;

function TLifeWindows.Previous: LifeWindow;
begin
  with MainForm do begin
    if (UniverseTabset.TabIndex > 0) then
      UniverseTabset.SelectNext(false);
  end; {with}
end;

procedure TLifeWindows.Delete(index: Integer);
begin

end;

procedure TLifeWindows.Set_Item(index: Integer; const Value: LifeWindow);
begin

end;

procedure TLifeScrapbook.Init(AScrapbook: TListView);
begin
  Scrapbook:= AScrapbook;
end;

function TLifeScrapbook.Get_Count: Integer;
begin
  Result:= Scrapbook.Items.Count;
end;

function TLifeScrapbook.Get_Item(index: Integer): LifeScraplet;
begin

end;

function TLifeScrapbook.Insert(const Universe: LifeUniverse): LifeScraplet;
begin

end;

function TLifeScrapbook.Next: LifeScraplet;
begin

end;

function TLifeScrapbook.Previous: LifeScraplet;
begin

end;

procedure TLifeScrapbook.Delete(index: Integer);
begin

end;

procedure TLifeScrapbook.Set_Count(Value: Integer);
begin

end;

procedure TLifeScrapbook.Set_Item(index: Integer;
  const Value: LifeScraplet);
begin

end;

procedure TLifeScraplet.Init(AListItem: TListItem);
begin
  ListItem:= AListItem;
end;

function TLifeScraplet.Get_Caption: WideString;
begin
  Result:= ListItem.Caption;
end;

function TLifeScraplet.Get_ShortcutKey: WideString;
begin
  Result:= ShortcutToText(ListItem.OverlayIndex);
end;

function TLifeScraplet.Get_Universe: LifeUniverse;
begin

end;

procedure TLifeScraplet.Set_Caption(const Value: WideString);
begin

end;

procedure TLifeScraplet.Set_ShortcutKey(const Value: WideString);
begin
  try
    ListItem.OverlayIndex:= TextToShortcut(Value);
  except {do nothing}
  end;
end;

procedure TLifeScraplet.Set_Universe(const Value: LifeUniverse);
begin

end;

procedure TLifeSidepanel.Init(AMainForm: TLife32MainForm);
begin
  MainForm:= AMainForm;
end;

function TLifeSidepanel.Get_PageIndex: Integer;
begin
  Result:= MainForm.SidePageControl.ActivePage.PageIndex;
end;

function TLifeSidepanel.Get_Visible: WordBool;
begin
  Result:= MainForm.LeftPanel.Width > 1;
end;

function TLifeSidepanel.Get_Width: Integer;
begin
  Result:= MainForm.LeftPanel.Width;
end;

procedure TLifeSidepanel.Set_PageIndex(Value: Integer);
begin
  MainForm.SidePanelTabs.TabIndex:= Value;
end;

procedure TLifeSidepanel.Set_Visible(Value: WordBool);
begin
  case Value of
    true: MainForm.ShowSideButtonClick(MainForm);
    false: MainForm.HideSideButtonClick(MainForm);
  end; {case}
end;

procedure TLifeSidepanel.Set_Width(Value: Integer);
begin
  if Value < 1 then Value:= 1;
  if Value > (MainForm.Width - 100) then Value:= (MainForm.Width - 100);
  if Value < 1 then Value:= 1;
  MainForm.LeftPanel.Width:= Value;
end;

procedure TLifeSnapshots.Init(ASnapshotList: TSnapshotList);
begin
  SnapshotList:= ASnapshotList;
end;

function TLifeSnapshots.Get_Count: Integer;
begin

end;

function TLifeSnapshots.Get_Item(index: Integer): LifeSnapshot;
begin

end;

function TLifeSnapshots.Get_Window: LifeWindow;
begin

end;

procedure TLifeSnapshots.Delete(index: Integer);
begin

end;

procedure TLifeSnapshots.MakeSnapshot(Cause: Integer);
begin

end;

procedure TLifeSnapshots.RevertToSnapshot(index: Integer);
begin

end;

procedure TLifeSnapshots.SortBy(SortKey: Integer; Ascending: WordBool);
begin

end;

procedure TLifeSnapshot.Init(AListItem: TListItem);
begin
  ListItem:= AListItem;
end;

function TLifeSnapshot.Get_Cause: Integer;
begin

end;

function TLifeSnapshot.Get_Filename: Integer;
begin

end;

function TLifeSnapshot.Get_Generation: Integer;
begin

end;

function TLifeSnapshot.Get_PatternNumber: Integer;
begin

end;

function TLifeSnapshot.Get_RevisionNumber: Integer;
begin

end;

procedure TLifeSnapshot.Set_Cause(Value: Integer);
begin

end;

procedure TLifeSnapshot.Set_Filename(Value: Integer);
begin

end;

procedure TLifeSnapshot.Set_Generation(Value: Integer);
begin

end;

procedure TLifeSnapshot.Set_PatternNumber(Value: Integer);
begin

end;

procedure TLifeSnapshot.Set_RevisionNumber(Value: Integer);
begin

end;

function TLifeUniverse.Get_Rule: WideString;
begin

end;

procedure TLifeUniverse.Set_Rule(const Value: WideString);
begin

end;

function TLifeApplication.Get_FrameDropInterval: Integer;
begin

end;

procedure TLifeApplication.Set_FrameDropInterval(Value: Integer);
begin

end;

function TLifeApplication.Get_Colors: LifeColors;
begin

end;

function TLifeApplication.Get_Windows: LifeWindows;
begin

end;

function TLifeApplication.Get_Scrapbook: LifeScrapbook;
begin

end;

procedure TLifeApplication.Set_Scrapbook(const Value: LifeScrapbook);
begin

end;

initialization
  try
    TAutoObjectFactory.Create(ComServer, TLifeApplication, Class_LifeApplication, ciMultiInstance);
    TAutoObjectFactory.Create(ComServer, TLifeWindow, Class_LifeWindow, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeUniverse, Class_LifeUniverse, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeSelection, Class_LifeSelection, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeToolbar, Class_LifeToolbar, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeStatusbar, Class_LifeStatusbar, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeWindows, Class_LifeWindows, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeScrapbook, Class_LifeScrapbook, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeScraplet, Class_LifeScraplet, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeSnapshots, Class_LifeSnapshots, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeSidepanel, Class_LifeSidepanel, ciInternal);
    TAutoObjectFactory.Create(ComServer, TLifeSnapshot, Class_LifeSnapshot, ciInternal);
  except
    {ignore}
  end;
end.
