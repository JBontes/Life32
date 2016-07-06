unit life32_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 13-2-2006 16:47:16 from Type Library described below.

// ************************************************************************  //
// Type Lib: H:\BORLAND\Delphi32\Johan\Life\life32.tlb (1)
// LIBID: {74BEE5C0-5B2D-11D3-8F8B-0080C747848B}
// LCID: 0
// Helpfile: 
// HelpString: Life32 Library
// DepndLst: 
//   (1) v1.0 stdole, (I:\WINNT\System32\stdole32.tlb)
// Errors:
//   Hint: Parameter 'Message' of ILifeApplication.Ask changed to 'Message_'
//   Hint: Parameter 'y' of ILifeUniverse.CellState changed to 'y_'
//   Hint: Parameter 'index' of ILifeWindows.Item changed to 'index_'
//   Hint: Parameter 'index' of ILifeWindows.Item changed to 'index_'
//   Hint: Parameter 'index' of ILifeScrapbook.Item changed to 'index_'
//   Hint: Parameter 'index' of ILifeScrapbook.Item changed to 'index_'
//   Hint: Parameter 'Universe' of ILifeScrapbook.Insert changed to 'Universe_'
//   Hint: Parameter 'index' of ILifeSnapshots.Item changed to 'index_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  life32MajorVersion = 1;
  life32MinorVersion = 0;

  LIBID_life32: TGUID = '{74BEE5C0-5B2D-11D3-8F8B-0080C747848B}';

  IID_ILifeApplication: TGUID = '{74BEE5C1-5B2D-11D3-8F8B-0080C747848B}';
  CLASS_LifeApplication: TGUID = '{74BEE5C2-5B2D-11D3-8F8B-0080C747848B}';
  IID_ILifeWindow: TGUID = '{BD736540-5B3F-11D3-8F8B-0080C747848B}';
  CLASS_LifeWindow: TGUID = '{BD736541-5B3F-11D3-8F8B-0080C747848B}';
  IID_ILifeUniverse: TGUID = '{DA234E40-5BED-11D3-8F8B-0080C747848B}';
  CLASS_LifeUniverse: TGUID = '{DA234E41-5BED-11D3-8F8B-0080C747848B}';
  IID_ILifeSelection: TGUID = '{281E9FE0-CC97-11D3-AAE7-E20AA055AF17}';
  CLASS_LifeSelection: TGUID = '{281E9FE1-CC97-11D3-AAE7-E20AA055AF17}';
  IID_ILifeToolbar: TGUID = '{9E508F00-CF96-11D3-AAE7-9B21CBD246E8}';
  CLASS_LifeToolbar: TGUID = '{9E508F01-CF96-11D3-AAE7-9B21CBD246E8}';
  IID_ILifeStatusbar: TGUID = '{9E508F03-CF96-11D3-AAE7-9B21CBD246E8}';
  CLASS_LifeStatusbar: TGUID = '{9E508F04-CF96-11D3-AAE7-9B21CBD246E8}';
  IID_ILifeWindows: TGUID = '{27D67020-1683-11D6-9C30-80AA4AC1B44D}';
  CLASS_LifeWindows: TGUID = '{27D67021-1683-11D6-9C30-80AA4AC1B44D}';
  IID_ILifeScrapbook: TGUID = '{27D67022-1683-11D6-9C30-80AA4AC1B44D}';
  CLASS_LifeScrapbook: TGUID = '{27D67023-1683-11D6-9C30-80AA4AC1B44D}';
  IID_ILifeScraplet: TGUID = '{27D67024-1683-11D6-9C30-80AA4AC1B44D}';
  CLASS_LifeScraplet: TGUID = '{27D67025-1683-11D6-9C30-80AA4AC1B44D}';
  IID_ILifeSnapshots: TGUID = '{27D67026-1683-11D6-9C30-80AA4AC1B44D}';
  CLASS_LifeSnapshots: TGUID = '{27D67027-1683-11D6-9C30-80AA4AC1B44D}';
  IID_ILifeSidepanel: TGUID = '{27D67028-1683-11D6-9C30-80AA4AC1B44D}';
  CLASS_LifeSidepanel: TGUID = '{27D67029-1683-11D6-9C30-80AA4AC1B44D}';
  IID_ILifeSnapshot: TGUID = '{27D6702A-1683-11D6-9C30-80AA4AC1B44D}';
  CLASS_LifeSnapshot: TGUID = '{27D6702B-1683-11D6-9C30-80AA4AC1B44D}';
  IID_ILifeColors: TGUID = '{FDD87140-1A4E-11D6-9C30-B09045C10000}';
  CLASS_LifeColors: TGUID = '{FDD87141-1A4E-11D6-9C30-B09045C10000}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum ePasteMode
type
  ePasteMode = TOleEnum;
const
  lpmOr = $000007D1;
  lpmPut = $000007D2;
  lpmXor = $000007D3;
  lpmError = $000007D4;

// Constants for enum eCursorMode
type
  eCursorMode = TOleEnum;
const
  emDraw = $000007DA;
  emSelect = $000007DB;
  emHand = $000007DD;

// Constants for enum eWindowState
type
  eWindowState = TOleEnum;
const
  ewsMaximized = $00000001;
  ewsMinimized = $00000002;
  ewsNormal = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ILifeApplication = interface;
  ILifeApplicationDisp = dispinterface;
  ILifeWindow = interface;
  ILifeWindowDisp = dispinterface;
  ILifeUniverse = interface;
  ILifeUniverseDisp = dispinterface;
  ILifeSelection = interface;
  ILifeSelectionDisp = dispinterface;
  ILifeToolbar = interface;
  ILifeToolbarDisp = dispinterface;
  ILifeStatusbar = interface;
  ILifeStatusbarDisp = dispinterface;
  ILifeWindows = interface;
  ILifeWindowsDisp = dispinterface;
  ILifeScrapbook = interface;
  ILifeScrapbookDisp = dispinterface;
  ILifeScraplet = interface;
  ILifeScrapletDisp = dispinterface;
  ILifeSnapshots = interface;
  ILifeSnapshotsDisp = dispinterface;
  ILifeSidepanel = interface;
  ILifeSidepanelDisp = dispinterface;
  ILifeSnapshot = interface;
  ILifeSnapshotDisp = dispinterface;
  ILifeColors = interface;
  ILifeColorsDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  LifeApplication = ILifeApplication;
  LifeWindow = ILifeWindow;
  LifeUniverse = ILifeUniverse;
  LifeSelection = ILifeSelection;
  LifeToolbar = ILifeToolbar;
  LifeStatusbar = ILifeStatusbar;
  LifeWindows = ILifeWindows;
  LifeScrapbook = ILifeScrapbook;
  LifeScraplet = ILifeScraplet;
  LifeSnapshots = ILifeSnapshots;
  LifeSidepanel = ILifeSidepanel;
  LifeSnapshot = ILifeSnapshot;
  LifeColors = ILifeColors;


// *********************************************************************//
// Interface: ILifeApplication
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {74BEE5C1-5B2D-11D3-8F8B-0080C747848B}
// *********************************************************************//
  ILifeApplication = interface(IDispatch)
    ['{74BEE5C1-5B2D-11D3-8F8B-0080C747848B}']
    function Get_Sidepanel: LifeSidepanel; safecall;
    function Get_Toolbar: LifeToolbar; safecall;
    function Get_Statusbar: LifeStatusbar; safecall;
    function Get_Colors: LifeColors; safecall;
    function Get_Scrapbook: LifeScrapbook; safecall;
    function Get_Windows: LifeWindows; safecall;
    function Get_ActiveWindow: LifeWindow; safecall;
    procedure Play(step: Integer); safecall;
    procedure SkipTo(Generation: Integer); safecall;
    procedure Pause; safecall;
    procedure Close; safecall;
    procedure RegisterKeyHandler(const Name: WideString); safecall;
    procedure UnregisterKeyhandler(const Name: WideString); safecall;
    function Get_PlaySpeed: Integer; safecall;
    procedure Set_PlaySpeed(PlaySpeed: Integer); safecall;
    function Get_Zoom: Integer; safecall;
    procedure Set_Zoom(Zoom: Integer); safecall;
    function Get_FrameDropInterval: Integer; safecall;
    procedure Set_FrameDropInterval(FrameDropInterval: Integer); safecall;
    function Get_ShowScrollbar: WordBool; safecall;
    procedure Set_ShowScrollbar(ShowScrollbar: WordBool); safecall;
    function Get_DirectXEnabled: WordBool; safecall;
    procedure Set_DirectXEnabled(DirectXEnabled: WordBool); safecall;
    function Get_CursorMode: eCursorMode; safecall;
    procedure Set_CursorMode(CursorMode: eCursorMode); safecall;
    function Get_PasteMode: ePasteMode; safecall;
    procedure Set_PasteMode(PasteMode: ePasteMode); safecall;
    function Get_WindowState: eWindowState; safecall;
    procedure Set_WindowState(WindowState: eWindowState); safecall;
    procedure SetFocus; safecall;
    procedure MessageBox(const Message: WideString); safecall;
    function Ask(const Message: WideString): WideString; safecall;
    property Sidepanel: LifeSidepanel read Get_Sidepanel;
    property Toolbar: LifeToolbar read Get_Toolbar;
    property Statusbar: LifeStatusbar read Get_Statusbar;
    property Colors: LifeColors read Get_Colors;
    property Scrapbook: LifeScrapbook read Get_Scrapbook;
    property Windows: LifeWindows read Get_Windows;
    property ActiveWindow: LifeWindow read Get_ActiveWindow;
    property PlaySpeed: Integer read Get_PlaySpeed write Set_PlaySpeed;
    property Zoom: Integer read Get_Zoom write Set_Zoom;
    property FrameDropInterval: Integer read Get_FrameDropInterval write Set_FrameDropInterval;
    property ShowScrollbar: WordBool read Get_ShowScrollbar write Set_ShowScrollbar;
    property DirectXEnabled: WordBool read Get_DirectXEnabled write Set_DirectXEnabled;
    property CursorMode: eCursorMode read Get_CursorMode write Set_CursorMode;
    property PasteMode: ePasteMode read Get_PasteMode write Set_PasteMode;
    property WindowState: eWindowState read Get_WindowState write Set_WindowState;
  end;

// *********************************************************************//
// DispIntf:  ILifeApplicationDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {74BEE5C1-5B2D-11D3-8F8B-0080C747848B}
// *********************************************************************//
  ILifeApplicationDisp = dispinterface
    ['{74BEE5C1-5B2D-11D3-8F8B-0080C747848B}']
    property Sidepanel: LifeSidepanel readonly dispid 21;
    property Toolbar: LifeToolbar readonly dispid 9;
    property Statusbar: LifeStatusbar readonly dispid 11;
    property Colors: LifeColors readonly dispid 23;
    property Scrapbook: LifeScrapbook readonly dispid 25;
    property Windows: LifeWindows readonly dispid 24;
    property ActiveWindow: LifeWindow readonly dispid 1;
    procedure Play(step: Integer); dispid 2;
    procedure SkipTo(Generation: Integer); dispid 7;
    procedure Pause; dispid 3;
    procedure Close; dispid 4;
    procedure RegisterKeyHandler(const Name: WideString); dispid 5;
    procedure UnregisterKeyhandler(const Name: WideString); dispid 6;
    property PlaySpeed: Integer dispid 8;
    property Zoom: Integer dispid 20;
    property FrameDropInterval: Integer dispid 22;
    property ShowScrollbar: WordBool dispid 10;
    property DirectXEnabled: WordBool dispid 14;
    property CursorMode: eCursorMode dispid 12;
    property PasteMode: ePasteMode dispid 13;
    property WindowState: eWindowState dispid 16;
    procedure SetFocus; dispid 17;
    procedure MessageBox(const Message: WideString); dispid 18;
    function Ask(const Message: WideString): WideString; dispid 19;
  end;

// *********************************************************************//
// Interface: ILifeWindow
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {BD736540-5B3F-11D3-8F8B-0080C747848B}
// *********************************************************************//
  ILifeWindow = interface(IDispatch)
    ['{BD736540-5B3F-11D3-8F8B-0080C747848B}']
    function Get_Application: LifeApplication; safecall;
    function Get_Universe: LifeUniverse; safecall;
    function Get_Selection: LifeSelection; safecall;
    function Get_Snapshots: LifeSnapshots; safecall;
    procedure Set_Snapshots(const Snapshots: LifeSnapshots); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Caption: WideString); safecall;
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    procedure MoveTo(x: Integer; y: Integer); safecall;
    procedure MoveBy(dx: Integer; dy: Integer); safecall;
    procedure ZoomToFit; safecall;
    procedure ZoomToSelection; safecall;
    procedure CenterOnPattern; safecall;
    procedure SelectAll; safecall;
    procedure RandomDot; safecall;
    procedure Redraw; safecall;
    property Application: LifeApplication read Get_Application;
    property Universe: LifeUniverse read Get_Universe;
    property Selection: LifeSelection read Get_Selection;
    property Snapshots: LifeSnapshots read Get_Snapshots write Set_Snapshots;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Width: Integer read Get_Width;
    property Height: Integer read Get_Height;
  end;

// *********************************************************************//
// DispIntf:  ILifeWindowDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {BD736540-5B3F-11D3-8F8B-0080C747848B}
// *********************************************************************//
  ILifeWindowDisp = dispinterface
    ['{BD736540-5B3F-11D3-8F8B-0080C747848B}']
    property Application: LifeApplication readonly dispid 3;
    property Universe: LifeUniverse readonly dispid 2;
    property Selection: LifeSelection readonly dispid 12;
    property Snapshots: LifeSnapshots dispid 17;
    property Caption: WideString dispid 15;
    property Width: Integer readonly dispid 8;
    property Height: Integer readonly dispid 9;
    procedure MoveTo(x: Integer; y: Integer); dispid 4;
    procedure MoveBy(dx: Integer; dy: Integer); dispid 5;
    procedure ZoomToFit; dispid 6;
    procedure ZoomToSelection; dispid 13;
    procedure CenterOnPattern; dispid 1;
    procedure SelectAll; dispid 7;
    procedure RandomDot; dispid 10;
    procedure Redraw; dispid 14;
  end;

// *********************************************************************//
// Interface: ILifeUniverse
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DA234E40-5BED-11D3-8F8B-0080C747848B}
// *********************************************************************//
  ILifeUniverse = interface(IDispatch)
    ['{DA234E40-5BED-11D3-8F8B-0080C747848B}']
    function Get_Generation: Integer; safecall;
    function Get_Rules: WideString; safecall;
    procedure Set_Rules(const Rules: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Description: WideString); safecall;
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function Get_CellCount: Integer; safecall;
    procedure SaveToFile(const AFile: WideString); safecall;
    procedure LoadFromFile(const AFile: WideString); safecall;
    procedure ChangeCell(x: Integer; y: Integer; State: WordBool); safecall;
    function CellState(x: Integer; y: Integer): WordBool; safecall;
    procedure DrawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer; Fill: WordBool); safecall;
    function Clone: LifeUniverse; safecall;
    property Generation: Integer read Get_Generation;
    property Rules: WideString read Get_Rules write Set_Rules;
    property Description: WideString read Get_Description write Set_Description;
    property Width: Integer read Get_Width;
    property Height: Integer read Get_Height;
    property CellCount: Integer read Get_CellCount;
  end;

// *********************************************************************//
// DispIntf:  ILifeUniverseDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DA234E40-5BED-11D3-8F8B-0080C747848B}
// *********************************************************************//
  ILifeUniverseDisp = dispinterface
    ['{DA234E40-5BED-11D3-8F8B-0080C747848B}']
    property Generation: Integer readonly dispid 1;
    property Rules: WideString dispid 2;
    property Description: WideString dispid 12;
    property Width: Integer readonly dispid 7;
    property Height: Integer readonly dispid 8;
    property CellCount: Integer readonly dispid 9;
    procedure SaveToFile(const AFile: WideString); dispid 3;
    procedure LoadFromFile(const AFile: WideString); dispid 4;
    procedure ChangeCell(x: Integer; y: Integer; State: WordBool); dispid 5;
    function CellState(x: Integer; y: Integer): WordBool; dispid 6;
    procedure DrawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer; Fill: WordBool); dispid 10;
    function Clone: LifeUniverse; dispid 11;
  end;

// *********************************************************************//
// Interface: ILifeSelection
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {281E9FE0-CC97-11D3-AAE7-E20AA055AF17}
// *********************************************************************//
  ILifeSelection = interface(IDispatch)
    ['{281E9FE0-CC97-11D3-AAE7-E20AA055AF17}']
    function Get_Universe: LifeUniverse; safecall;
    function Get_Left: Integer; safecall;
    procedure Set_Left(Left: Integer); safecall;
    function Get_Top: Integer; safecall;
    procedure Set_Top(Top: Integer); safecall;
    function Get_Right: Integer; safecall;
    procedure Set_Right(Right: Integer); safecall;
    function Get_Bottom: Integer; safecall;
    procedure Set_Bottom(Bottom: Integer); safecall;
    function Get_CellCount: Integer; safecall;
    function Get_AsText: WideString; safecall;
    procedure Set_AsText(const AsText: WideString); safecall;
    procedure Cut; safecall;
    procedure Copy; safecall;
    procedure Paste; safecall;
    procedure Clear; safecall;
    procedure ClearOutsideSelection; safecall;
    procedure Invert; safecall;
    procedure FillBlack; safecall;
    procedure FillRandom; safecall;
    procedure DrawBox; safecall;
    procedure MirrorHorz; safecall;
    procedure MirrorVert; safecall;
    procedure Rotate90; safecall;
    procedure Rotate180; safecall;
    procedure Rotate270; safecall;
    procedure MoveBy(dx: Integer; dy: Integer); safecall;
    property Universe: LifeUniverse read Get_Universe;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Right: Integer read Get_Right write Set_Right;
    property Bottom: Integer read Get_Bottom write Set_Bottom;
    property CellCount: Integer read Get_CellCount;
    property AsText: WideString read Get_AsText write Set_AsText;
  end;

// *********************************************************************//
// DispIntf:  ILifeSelectionDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {281E9FE0-CC97-11D3-AAE7-E20AA055AF17}
// *********************************************************************//
  ILifeSelectionDisp = dispinterface
    ['{281E9FE0-CC97-11D3-AAE7-E20AA055AF17}']
    property Universe: LifeUniverse readonly dispid 22;
    property Left: Integer dispid 1;
    property Top: Integer dispid 2;
    property Right: Integer dispid 3;
    property Bottom: Integer dispid 4;
    property CellCount: Integer readonly dispid 13;
    property AsText: WideString dispid 15;
    procedure Cut; dispid 5;
    procedure Copy; dispid 6;
    procedure Paste; dispid 7;
    procedure Clear; dispid 8;
    procedure ClearOutsideSelection; dispid 16;
    procedure Invert; dispid 9;
    procedure FillBlack; dispid 10;
    procedure FillRandom; dispid 11;
    procedure DrawBox; dispid 12;
    procedure MirrorHorz; dispid 17;
    procedure MirrorVert; dispid 18;
    procedure Rotate90; dispid 19;
    procedure Rotate180; dispid 20;
    procedure Rotate270; dispid 21;
    procedure MoveBy(dx: Integer; dy: Integer); dispid 14;
  end;

// *********************************************************************//
// Interface: ILifeToolbar
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {9E508F00-CF96-11D3-AAE7-9B21CBD246E8}
// *********************************************************************//
  ILifeToolbar = interface(IDispatch)
    ['{9E508F00-CF96-11D3-AAE7-9B21CBD246E8}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
  end;

// *********************************************************************//
// DispIntf:  ILifeToolbarDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {9E508F00-CF96-11D3-AAE7-9B21CBD246E8}
// *********************************************************************//
  ILifeToolbarDisp = dispinterface
    ['{9E508F00-CF96-11D3-AAE7-9B21CBD246E8}']
    property Visible: WordBool dispid 1;
  end;

// *********************************************************************//
// Interface: ILifeStatusbar
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {9E508F03-CF96-11D3-AAE7-9B21CBD246E8}
// *********************************************************************//
  ILifeStatusbar = interface(IDispatch)
    ['{9E508F03-CF96-11D3-AAE7-9B21CBD246E8}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
  end;

// *********************************************************************//
// DispIntf:  ILifeStatusbarDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {9E508F03-CF96-11D3-AAE7-9B21CBD246E8}
// *********************************************************************//
  ILifeStatusbarDisp = dispinterface
    ['{9E508F03-CF96-11D3-AAE7-9B21CBD246E8}']
    property Visible: WordBool dispid 1;
  end;

// *********************************************************************//
// Interface: ILifeWindows
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67020-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeWindows = interface(IDispatch)
    ['{27D67020-1683-11D6-9C30-80AA4AC1B44D}']
    function Get_Item(index: Integer): LifeWindow; safecall;
    procedure Set_Item(index: Integer; const index_: LifeWindow); safecall;
    function Get_Count: Integer; safecall;
    function Next: LifeWindow; safecall;
    function Previous: LifeWindow; safecall;
    function New: LifeWindow; safecall;
    procedure Delete(index: Integer); safecall;
    property Item[index: Integer]: LifeWindow read Get_Item write Set_Item;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ILifeWindowsDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67020-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeWindowsDisp = dispinterface
    ['{27D67020-1683-11D6-9C30-80AA4AC1B44D}']
    property Item[index: Integer]: LifeWindow dispid 1;
    property Count: Integer readonly dispid 2;
    function Next: LifeWindow; dispid 3;
    function Previous: LifeWindow; dispid 4;
    function New: LifeWindow; dispid 5;
    procedure Delete(index: Integer); dispid 6;
  end;

// *********************************************************************//
// Interface: ILifeScrapbook
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67022-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeScrapbook = interface(IDispatch)
    ['{27D67022-1683-11D6-9C30-80AA4AC1B44D}']
    function Get_Item(index: Integer): LifeScraplet; safecall;
    procedure Set_Item(index: Integer; const index_: LifeScraplet); safecall;
    function Get_Count: Integer; safecall;
    procedure Set_Count(Count: Integer); safecall;
    function Next: LifeScraplet; safecall;
    function Previous: LifeScraplet; safecall;
    function Insert(const Universe: LifeUniverse): LifeScraplet; safecall;
    procedure Delete(index: Integer); safecall;
    property Item[index: Integer]: LifeScraplet read Get_Item write Set_Item;
    property Count: Integer read Get_Count write Set_Count;
  end;

// *********************************************************************//
// DispIntf:  ILifeScrapbookDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67022-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeScrapbookDisp = dispinterface
    ['{27D67022-1683-11D6-9C30-80AA4AC1B44D}']
    property Item[index: Integer]: LifeScraplet dispid 1;
    property Count: Integer dispid 2;
    function Next: LifeScraplet; dispid 3;
    function Previous: LifeScraplet; dispid 4;
    function Insert(const Universe: LifeUniverse): LifeScraplet; dispid 5;
    procedure Delete(index: Integer); dispid 6;
  end;

// *********************************************************************//
// Interface: ILifeScraplet
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67024-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeScraplet = interface(IDispatch)
    ['{27D67024-1683-11D6-9C30-80AA4AC1B44D}']
    function Get_Universe: LifeUniverse; safecall;
    procedure Set_Universe(const Universe: LifeUniverse); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Caption: WideString); safecall;
    function Get_ShortcutKey: WideString; safecall;
    procedure Set_ShortcutKey(const ShortcutKey: WideString); safecall;
    property Universe: LifeUniverse read Get_Universe write Set_Universe;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ShortcutKey: WideString read Get_ShortcutKey write Set_ShortcutKey;
  end;

// *********************************************************************//
// DispIntf:  ILifeScrapletDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67024-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeScrapletDisp = dispinterface
    ['{27D67024-1683-11D6-9C30-80AA4AC1B44D}']
    property Universe: LifeUniverse dispid 1;
    property Caption: WideString dispid 2;
    property ShortcutKey: WideString dispid 3;
  end;

// *********************************************************************//
// Interface: ILifeSnapshots
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67026-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeSnapshots = interface(IDispatch)
    ['{27D67026-1683-11D6-9C30-80AA4AC1B44D}']
    function Get_Window: LifeWindow; safecall;
    function Get_Item(index: Integer): LifeSnapshot; safecall;
    function Get_Count: Integer; safecall;
    procedure MakeSnapshot(Cause: Integer); safecall;
    procedure RevertToSnapshot(index: Integer); safecall;
    procedure SortBy(SortKey: Integer; Ascending: WordBool); safecall;
    procedure Delete(index: Integer); safecall;
    property Window: LifeWindow read Get_Window;
    property Item[index: Integer]: LifeSnapshot read Get_Item;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ILifeSnapshotsDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67026-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeSnapshotsDisp = dispinterface
    ['{27D67026-1683-11D6-9C30-80AA4AC1B44D}']
    property Window: LifeWindow readonly dispid 1;
    property Item[index: Integer]: LifeSnapshot readonly dispid 3;
    property Count: Integer readonly dispid 4;
    procedure MakeSnapshot(Cause: Integer); dispid 5;
    procedure RevertToSnapshot(index: Integer); dispid 6;
    procedure SortBy(SortKey: Integer; Ascending: WordBool); dispid 7;
    procedure Delete(index: Integer); dispid 8;
  end;

// *********************************************************************//
// Interface: ILifeSidepanel
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67028-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeSidepanel = interface(IDispatch)
    ['{27D67028-1683-11D6-9C30-80AA4AC1B44D}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    function Get_Width: Integer; safecall;
    procedure Set_Width(Width: Integer); safecall;
    function Get_PageIndex: Integer; safecall;
    procedure Set_PageIndex(PageIndex: Integer); safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Width: Integer read Get_Width write Set_Width;
    property PageIndex: Integer read Get_PageIndex write Set_PageIndex;
  end;

// *********************************************************************//
// DispIntf:  ILifeSidepanelDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D67028-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeSidepanelDisp = dispinterface
    ['{27D67028-1683-11D6-9C30-80AA4AC1B44D}']
    property Visible: WordBool dispid 1;
    property Width: Integer dispid 2;
    property PageIndex: Integer dispid 3;
  end;

// *********************************************************************//
// Interface: ILifeSnapshot
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D6702A-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeSnapshot = interface(IDispatch)
    ['{27D6702A-1683-11D6-9C30-80AA4AC1B44D}']
    function Get_Cause: Integer; safecall;
    procedure Set_Cause(Cause: Integer); safecall;
    function Get_Generation: Integer; safecall;
    procedure Set_Generation(Generation: Integer); safecall;
    function Get_PatternNumber: Integer; safecall;
    procedure Set_PatternNumber(PatternNumber: Integer); safecall;
    function Get_RevisionNumber: Integer; safecall;
    procedure Set_RevisionNumber(RevisionNumber: Integer); safecall;
    function Get_Filename: Integer; safecall;
    procedure Set_Filename(Filename: Integer); safecall;
    property Cause: Integer read Get_Cause write Set_Cause;
    property Generation: Integer read Get_Generation write Set_Generation;
    property PatternNumber: Integer read Get_PatternNumber write Set_PatternNumber;
    property RevisionNumber: Integer read Get_RevisionNumber write Set_RevisionNumber;
    property Filename: Integer read Get_Filename write Set_Filename;
  end;

// *********************************************************************//
// DispIntf:  ILifeSnapshotDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {27D6702A-1683-11D6-9C30-80AA4AC1B44D}
// *********************************************************************//
  ILifeSnapshotDisp = dispinterface
    ['{27D6702A-1683-11D6-9C30-80AA4AC1B44D}']
    property Cause: Integer dispid 1;
    property Generation: Integer dispid 2;
    property PatternNumber: Integer dispid 3;
    property RevisionNumber: Integer dispid 4;
    property Filename: Integer dispid 5;
  end;

// *********************************************************************//
// Interface: ILifeColors
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {FDD87140-1A4E-11D6-9C30-B09045C10000}
// *********************************************************************//
  ILifeColors = interface(IDispatch)
    ['{FDD87140-1A4E-11D6-9C30-B09045C10000}']
    function Get_ForeGround: Integer; safecall;
    procedure Set_ForeGround(ForeGround: Integer); safecall;
    function Get_BackGround: Integer; safecall;
    procedure Set_BackGround(BackGround: Integer); safecall;
    function Get_NormalGrid: Integer; safecall;
    procedure Set_NormalGrid(NormalGrid: Integer); safecall;
    function Get_BoldGrid: Integer; safecall;
    procedure Set_BoldGrid(BoldGrid: Integer); safecall;
    function Get_SelectionRect: Integer; safecall;
    procedure Set_SelectionRect(SelectionRect: Integer); safecall;
    function Get_ZoomRect: Integer; safecall;
    procedure Set_ZoomRect(ZoomRect: Integer); safecall;
    function Get_OutsideTorus: Integer; safecall;
    procedure Set_OutsideTorus(OutsideTorus: Integer); safecall;
    function Get_DragCells: Integer; safecall;
    procedure Set_DragCells(DragCells: Integer); safecall;
    property ForeGround: Integer read Get_ForeGround write Set_ForeGround;
    property BackGround: Integer read Get_BackGround write Set_BackGround;
    property NormalGrid: Integer read Get_NormalGrid write Set_NormalGrid;
    property BoldGrid: Integer read Get_BoldGrid write Set_BoldGrid;
    property SelectionRect: Integer read Get_SelectionRect write Set_SelectionRect;
    property ZoomRect: Integer read Get_ZoomRect write Set_ZoomRect;
    property OutsideTorus: Integer read Get_OutsideTorus write Set_OutsideTorus;
    property DragCells: Integer read Get_DragCells write Set_DragCells;
  end;

// *********************************************************************//
// DispIntf:  ILifeColorsDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {FDD87140-1A4E-11D6-9C30-B09045C10000}
// *********************************************************************//
  ILifeColorsDisp = dispinterface
    ['{FDD87140-1A4E-11D6-9C30-B09045C10000}']
    property ForeGround: Integer dispid 1;
    property BackGround: Integer dispid 2;
    property NormalGrid: Integer dispid 3;
    property BoldGrid: Integer dispid 4;
    property SelectionRect: Integer dispid 5;
    property ZoomRect: Integer dispid 6;
    property OutsideTorus: Integer dispid 7;
    property DragCells: Integer dispid 9;
  end;

// *********************************************************************//
// The Class CoLifeApplication provides a Create and CreateRemote method to          
// create instances of the default interface ILifeApplication exposed by              
// the CoClass LifeApplication. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeApplication = class
    class function Create: ILifeApplication;
    class function CreateRemote(const MachineName: string): ILifeApplication;
  end;

// *********************************************************************//
// The Class CoLifeWindow provides a Create and CreateRemote method to          
// create instances of the default interface ILifeWindow exposed by              
// the CoClass LifeWindow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeWindow = class
    class function Create: ILifeWindow;
    class function CreateRemote(const MachineName: string): ILifeWindow;
  end;

// *********************************************************************//
// The Class CoLifeUniverse provides a Create and CreateRemote method to          
// create instances of the default interface ILifeUniverse exposed by              
// the CoClass LifeUniverse. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeUniverse = class
    class function Create: ILifeUniverse;
    class function CreateRemote(const MachineName: string): ILifeUniverse;
  end;

// *********************************************************************//
// The Class CoLifeSelection provides a Create and CreateRemote method to          
// create instances of the default interface ILifeSelection exposed by              
// the CoClass LifeSelection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeSelection = class
    class function Create: ILifeSelection;
    class function CreateRemote(const MachineName: string): ILifeSelection;
  end;

// *********************************************************************//
// The Class CoLifeToolbar provides a Create and CreateRemote method to          
// create instances of the default interface ILifeToolbar exposed by              
// the CoClass LifeToolbar. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeToolbar = class
    class function Create: ILifeToolbar;
    class function CreateRemote(const MachineName: string): ILifeToolbar;
  end;

// *********************************************************************//
// The Class CoLifeStatusbar provides a Create and CreateRemote method to          
// create instances of the default interface ILifeStatusbar exposed by              
// the CoClass LifeStatusbar. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeStatusbar = class
    class function Create: ILifeStatusbar;
    class function CreateRemote(const MachineName: string): ILifeStatusbar;
  end;

// *********************************************************************//
// The Class CoLifeWindows provides a Create and CreateRemote method to          
// create instances of the default interface ILifeWindows exposed by              
// the CoClass LifeWindows. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeWindows = class
    class function Create: ILifeWindows;
    class function CreateRemote(const MachineName: string): ILifeWindows;
  end;

// *********************************************************************//
// The Class CoLifeScrapbook provides a Create and CreateRemote method to          
// create instances of the default interface ILifeScrapbook exposed by              
// the CoClass LifeScrapbook. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeScrapbook = class
    class function Create: ILifeScrapbook;
    class function CreateRemote(const MachineName: string): ILifeScrapbook;
  end;

// *********************************************************************//
// The Class CoLifeScraplet provides a Create and CreateRemote method to          
// create instances of the default interface ILifeScraplet exposed by              
// the CoClass LifeScraplet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeScraplet = class
    class function Create: ILifeScraplet;
    class function CreateRemote(const MachineName: string): ILifeScraplet;
  end;

// *********************************************************************//
// The Class CoLifeSnapshots provides a Create and CreateRemote method to          
// create instances of the default interface ILifeSnapshots exposed by              
// the CoClass LifeSnapshots. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeSnapshots = class
    class function Create: ILifeSnapshots;
    class function CreateRemote(const MachineName: string): ILifeSnapshots;
  end;

// *********************************************************************//
// The Class CoLifeSidepanel provides a Create and CreateRemote method to          
// create instances of the default interface ILifeSidepanel exposed by              
// the CoClass LifeSidepanel. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeSidepanel = class
    class function Create: ILifeSidepanel;
    class function CreateRemote(const MachineName: string): ILifeSidepanel;
  end;

// *********************************************************************//
// The Class CoLifeSnapshot provides a Create and CreateRemote method to          
// create instances of the default interface ILifeSnapshot exposed by              
// the CoClass LifeSnapshot. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeSnapshot = class
    class function Create: ILifeSnapshot;
    class function CreateRemote(const MachineName: string): ILifeSnapshot;
  end;

// *********************************************************************//
// The Class CoLifeColors provides a Create and CreateRemote method to          
// create instances of the default interface ILifeColors exposed by              
// the CoClass LifeColors. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLifeColors = class
    class function Create: ILifeColors;
    class function CreateRemote(const MachineName: string): ILifeColors;
  end;

implementation

uses ComObj;

class function CoLifeApplication.Create: ILifeApplication;
begin
  Result := CreateComObject(CLASS_LifeApplication) as ILifeApplication;
end;

class function CoLifeApplication.CreateRemote(const MachineName: string): ILifeApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeApplication) as ILifeApplication;
end;

class function CoLifeWindow.Create: ILifeWindow;
begin
  Result := CreateComObject(CLASS_LifeWindow) as ILifeWindow;
end;

class function CoLifeWindow.CreateRemote(const MachineName: string): ILifeWindow;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeWindow) as ILifeWindow;
end;

class function CoLifeUniverse.Create: ILifeUniverse;
begin
  Result := CreateComObject(CLASS_LifeUniverse) as ILifeUniverse;
end;

class function CoLifeUniverse.CreateRemote(const MachineName: string): ILifeUniverse;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeUniverse) as ILifeUniverse;
end;

class function CoLifeSelection.Create: ILifeSelection;
begin
  Result := CreateComObject(CLASS_LifeSelection) as ILifeSelection;
end;

class function CoLifeSelection.CreateRemote(const MachineName: string): ILifeSelection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeSelection) as ILifeSelection;
end;

class function CoLifeToolbar.Create: ILifeToolbar;
begin
  Result := CreateComObject(CLASS_LifeToolbar) as ILifeToolbar;
end;

class function CoLifeToolbar.CreateRemote(const MachineName: string): ILifeToolbar;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeToolbar) as ILifeToolbar;
end;

class function CoLifeStatusbar.Create: ILifeStatusbar;
begin
  Result := CreateComObject(CLASS_LifeStatusbar) as ILifeStatusbar;
end;

class function CoLifeStatusbar.CreateRemote(const MachineName: string): ILifeStatusbar;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeStatusbar) as ILifeStatusbar;
end;

class function CoLifeWindows.Create: ILifeWindows;
begin
  Result := CreateComObject(CLASS_LifeWindows) as ILifeWindows;
end;

class function CoLifeWindows.CreateRemote(const MachineName: string): ILifeWindows;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeWindows) as ILifeWindows;
end;

class function CoLifeScrapbook.Create: ILifeScrapbook;
begin
  Result := CreateComObject(CLASS_LifeScrapbook) as ILifeScrapbook;
end;

class function CoLifeScrapbook.CreateRemote(const MachineName: string): ILifeScrapbook;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeScrapbook) as ILifeScrapbook;
end;

class function CoLifeScraplet.Create: ILifeScraplet;
begin
  Result := CreateComObject(CLASS_LifeScraplet) as ILifeScraplet;
end;

class function CoLifeScraplet.CreateRemote(const MachineName: string): ILifeScraplet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeScraplet) as ILifeScraplet;
end;

class function CoLifeSnapshots.Create: ILifeSnapshots;
begin
  Result := CreateComObject(CLASS_LifeSnapshots) as ILifeSnapshots;
end;

class function CoLifeSnapshots.CreateRemote(const MachineName: string): ILifeSnapshots;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeSnapshots) as ILifeSnapshots;
end;

class function CoLifeSidepanel.Create: ILifeSidepanel;
begin
  Result := CreateComObject(CLASS_LifeSidepanel) as ILifeSidepanel;
end;

class function CoLifeSidepanel.CreateRemote(const MachineName: string): ILifeSidepanel;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeSidepanel) as ILifeSidepanel;
end;

class function CoLifeSnapshot.Create: ILifeSnapshot;
begin
  Result := CreateComObject(CLASS_LifeSnapshot) as ILifeSnapshot;
end;

class function CoLifeSnapshot.CreateRemote(const MachineName: string): ILifeSnapshot;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeSnapshot) as ILifeSnapshot;
end;

class function CoLifeColors.Create: ILifeColors;
begin
  Result := CreateComObject(CLASS_LifeColors) as ILifeColors;
end;

class function CoLifeColors.CreateRemote(const MachineName: string): ILifeColors;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LifeColors) as ILifeColors;
end;

end.
