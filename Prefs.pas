unit Prefs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Tabnotbk, LifeBox, ExtCtrls, Buttons,
  NewExplButton, LifeCel, LifeLoad, Registry, LifeUtil, LifeConst, ColorGrd,
  Spin, RuleConst, LifeRules, Mask {, Shell_DDE};

type
  TFormPrefs = class(TForm)
    Panel1: TPanel;
    Panel13: TPanel;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    Bevel50: TBevel;
    TabbedNotebook1: TTabbedNotebook;
    Bevel46: TBevel;
    NotInstalledLabel: TLabel;
    Bevel48: TBevel;
    Bevel49: TBevel;
    Image18: TImage;
    FramedropLabel: TLabel;
    Bevel52: TBevel;
    Label84: TLabel;
    Bevel54: TBevel;
    LabelSpeed: TLabel;
    FasterButton: TNewExplButton;
    SlowerButton: TNewExplButton;
    Label86: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    LabelMem: TLabel;
    Image19: TImage;
    Shape1: TShape;
    Bevel56: TBevel;
    Label87: TLabel;
    CBDDraw: TCheckBox;
    CBFrameDropTime: TComboBox;
    UDFrameDropTime: TUpDown;
    SpeedTrack: TTrackBar;
    CheckBox1: TCheckBox;
    MemTrack: TTrackBar;
    Memo2: TMemo;
    Bevel61: TBevel;
    Bevel58: TBevel;
    Label88: TLabel;
    ZoomLabel: TLabel;
    CBZoomButton: TNewExplButton;
    Label89: TLabel;
    Label12: TLabel;
    ScrollLabel: TLabel;
    Label5: TLabel;
    HandLabel: TLabel;
    Bevel62: TBevel;
    Label90: TLabel;
    GridButton: TNewExplButton;
    Label6: TLabel;
    BoldGridLabel: TLabel;
    Bevel64: TBevel;
    Label91: TLabel;
    cbDefaultToolTipColor: TCheckBox;
    CBZoom: TComboBox;
    UDZoom: TUpDown;
    UDSmallScroll: TUpDown;
    SmallScrollEdit: TEdit;
    HandScrollEdit: TEdit;
    UDHandScroll: TUpDown;
    UDBoldGrid: TUpDown;
    BoldGridEdit: TEdit;
    RulePageControl: TPageControl;
    RuleSheet: TTabSheet;
    GroupBox8: TGroupBox;
    Bevel3: TBevel;
    InfoMemo: TMemo;
    GroupBox4: TGroupBox;
    RuleListView: TListView;
    ShortInfoEdit: TEdit;
    AddRuleButton: TButton;
    RemoveRuleButton: TButton;
    ChangeRuleButton: TButton;
    GroupBox27: TGroupBox;
    CBNeighborhood: TComboBox;
    cbN7: TCheckBox;
    cbN5: TCheckBox;
    cbN3: TCheckBox;
    cbN1: TCheckBox;
    cbN0: TCheckBox;
    cbN8: TCheckBox;
    cbN4: TCheckBox;
    cbN6: TCheckBox;
    cbN2: TCheckBox;
    NTSSheet: TTabSheet;
    Bevel43: TBevel;
    Sc2: TNewExplButton;
    Sc3: TNewExplButton;
    Sc4: TNewExplButton;
    Sc5: TNewExplButton;
    Sc6: TNewExplButton;
    Sc7: TNewExplButton;
    Sc1: TNewExplButton;
    Se2: TNewExplButton;
    Se3: TNewExplButton;
    Se4: TNewExplButton;
    Se5: TNewExplButton;
    Se6: TNewExplButton;
    Se7: TNewExplButton;
    Se1: TNewExplButton;
    Sk2: TNewExplButton;
    Sk3: TNewExplButton;
    Sk4: TNewExplButton;
    Sk5: TNewExplButton;
    Sk6: TNewExplButton;
    Sa2: TNewExplButton;
    Sa3: TNewExplButton;
    Sa4: TNewExplButton;
    Sa5: TNewExplButton;
    Sa6: TNewExplButton;
    Si2: TNewExplButton;
    Si3: TNewExplButton;
    Si4: TNewExplButton;
    Si5: TNewExplButton;
    Si6: TNewExplButton;
    Sv2: TNewExplButton;
    Sv3: TNewExplButton;
    Sv4: TNewExplButton;
    Sv5: TNewExplButton;
    Sv6: TNewExplButton;
    Sy3: TNewExplButton;
    Sy4: TNewExplButton;
    Sy5: TNewExplButton;
    Sq3: TNewExplButton;
    Sq4: TNewExplButton;
    Sq5: TNewExplButton;
    Sj3: TNewExplButton;
    Sj4: TNewExplButton;
    Sj5: TNewExplButton;
    Sr3: TNewExplButton;
    Sr4: TNewExplButton;
    Sr5: TNewExplButton;
    St4: TNewExplButton;
    Sw4: TNewExplButton;
    Sz4: TNewExplButton;
    Bevel22: TBevel;
    Bevel23: TBevel;
    Bevel24: TBevel;
    Bevel25: TBevel;
    Bevel26: TBevel;
    Bevel27: TBevel;
    Bevel28: TBevel;
    Bevel29: TBevel;
    Bevel30: TBevel;
    Bevel31: TBevel;
    Bevel32: TBevel;
    Bevel33: TBevel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Bevel34: TBevel;
    Bevel35: TBevel;
    Bevel36: TBevel;
    Bevel37: TBevel;
    Bevel38: TBevel;
    Bevel39: TBevel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Bevel44: TBevel;
    Bevel45: TBevel;
    NTBSheet: TTabSheet;
    Bc2: TNewExplButton;
    Bc3: TNewExplButton;
    Bc4: TNewExplButton;
    Bc5: TNewExplButton;
    Bc6: TNewExplButton;
    Bc7: TNewExplButton;
    Bc1: TNewExplButton;
    Be2: TNewExplButton;
    Be3: TNewExplButton;
    Be4: TNewExplButton;
    Be5: TNewExplButton;
    Be6: TNewExplButton;
    Be7: TNewExplButton;
    Be1: TNewExplButton;
    Bk2: TNewExplButton;
    Bk3: TNewExplButton;
    Bk4: TNewExplButton;
    Bk5: TNewExplButton;
    Bk6: TNewExplButton;
    Ba2: TNewExplButton;
    Ba3: TNewExplButton;
    Ba4: TNewExplButton;
    Ba5: TNewExplButton;
    Ba6: TNewExplButton;
    Bi2: TNewExplButton;
    Bi3: TNewExplButton;
    Bi4: TNewExplButton;
    Bi5: TNewExplButton;
    Bi6: TNewExplButton;
    Bv2: TNewExplButton;
    Bv3: TNewExplButton;
    Bv4: TNewExplButton;
    Bv5: TNewExplButton;
    Bv6: TNewExplButton;
    By3: TNewExplButton;
    By4: TNewExplButton;
    By5: TNewExplButton;
    Bq3: TNewExplButton;
    Bq4: TNewExplButton;
    Bq5: TNewExplButton;
    Bj3: TNewExplButton;
    Bj4: TNewExplButton;
    Bj5: TNewExplButton;
    Br3: TNewExplButton;
    Br4: TNewExplButton;
    Br5: TNewExplButton;
    Bt4: TNewExplButton;
    Bw4: TNewExplButton;
    Bz4: TNewExplButton;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Bevel14: TBevel;
    Bevel15: TBevel;
    Label30: TLabel;
    Label33: TLabel;
    Label35: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Bevel16: TBevel;
    Bevel17: TBevel;
    Bevel18: TBevel;
    Bevel19: TBevel;
    Bevel20: TBevel;
    Bevel21: TBevel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Bevel40: TBevel;
    Label74: TLabel;
    Label75: TLabel;
    Bevel41: TBevel;
    Bevel42: TBevel;
    RuleEdit: TEdit;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    BB1: TCheckBox;
    BB2: TCheckBox;
    BB3: TCheckBox;
    BB4: TCheckBox;
    BB5: TCheckBox;
    BB6: TCheckBox;
    BB7: TCheckBox;
    BB8: TCheckBox;
    SS0: TCheckBox;
    SS1: TCheckBox;
    SS2: TCheckBox;
    SS3: TCheckBox;
    SS4: TCheckBox;
    SS5: TCheckBox;
    SS6: TCheckBox;
    SS7: TCheckBox;
    SS8: TCheckBox;
    ForeShape: TShape;
    ForeBevel: TBevel;
    Bevel66: TBevel;
    Label92: TLabel;
    BackShape: TShape;
    BackBevel: TBevel;
    Bevel68: TBevel;
    Label93: TLabel;
    GridShape: TShape;
    GridBevel: TBevel;
    Bevel70: TBevel;
    Label94: TLabel;
    Grid2Shape: TShape;
    Grid2Bevel: TBevel;
    Label95: TLabel;
    Panel16: TPanel;
    Image10: TImage;
    Panel17: TPanel;
    Image12: TImage;
    Panel18: TPanel;
    Image13: TImage;
    Panel19: TPanel;
    Image14: TImage;
    Panel20: TPanel;
    Image15: TImage;
    Panel21: TPanel;
    Image16: TImage;
    Panel22: TPanel;
    Image17: TImage;
    ForeColorGrid: TColorGrid;
    Panel5: TPanel;
    Image1: TImage;
    BackColorGrid: TColorGrid;
    GridColorGrid: TColorGrid;
    Grid2ColorGrid: TColorGrid;
    SelRectShape: TShape;
    Bevel1: TBevel;
    Bevel74: TBevel;
    SelRectGrid: TColorGrid;
    Label96: TLabel;
    TorusShape: TShape;
    TorusBevel: TBevel;
    TorusColorGrid: TColorGrid;
    Bevel76: TBevel;
    Label97: TLabel;
    ZoomRectShape: TShape;
    Bevel2: TBevel;
    ZoomRectGrid: TColorGrid;
    Bevel78: TBevel;
    Label98: TLabel;
    DragShape: TShape;
    DragCelBevel: TBevel;
    DragGrid: TColorGrid;
    Bevel80: TBevel;
    Label99: TLabel;
    Label10: TLabel;
    Label22: TLabel;
    bmp_snapshottypes: TImage;
    Label31: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label21: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    cbAutoRewindStep: TCheckBox;
    cbAutoRewindNew: TCheckBox;
    RichEdit1: TRichEdit;
    cbAutoRewindOpen: TCheckBox;
    cbAutoRewindPlay: TCheckBox;
    cbAutoRewindDraw: TCheckBox;
    cbAutoRewindEdit: TCheckBox;
    cbAutoRewindRewind: TCheckBox;
    cbAutoRewindEveryX: TCheckBox;
    cbAutoRewindRotate: TCheckBox;
    AutoRewindEdit: TEdit;
    SpinButton1: TSpinButton;
    Bevel84: TBevel;
    Label101: TLabel;
    cbFreeze: TCheckBox;
    Label100: TLabel;
    Bevel47: TBevel;
    Label1: TLabel;
    UDFillRandom: TUpDown;
    EditRandomFill: TEdit;
    Label2: TLabel;
    Label102: TLabel;
    Bevel53: TBevel;
    Label8: TLabel;
    Label9: TLabel;
    EditPictureWidth: TEdit;
    UDPictureWidth: TUpDown;
    Label103: TLabel;
    Bevel55: TBevel;
    Label78: TLabel;
    ExtListView: TListView;
    Memo1: TMemo;
    FileExtEdit: TEdit;
    ButtonAddFileExt: TButton;
    Bevel57: TBevel;
    Label104: TLabel;
    Label85: TLabel;
    EditSnapshotTimeOut: TEdit;
    UDSnapshotTimeOut: TUpDown;
    cbSnapshotTimeOut: TCheckBox;
    Label105: TLabel;
    DrawModeButton: TNewExplButton;
    SelectModeButton: TNewExplButton;
    ZoomModeButton: TNewExplButton;
    ScrollModeButton: TNewExplButton;
    PasteOrButton: TNewExplButton;
    PastePutButton: TNewExplButton;
    PasteXorButton: TNewExplButton;
    PasteErrorButton: TNewExplButton;
    Bevel60: TBevel;
    Label106: TLabel;
    Label107: TLabel;
    Label24: TLabel;
    MaxFitLabel: TLabel;
    cbZoomToFit: TCheckBox;
    cbMaxFit: TComboBox;
    UDMaxFit: TUpDown;
    cbReshowSkip: TCheckBox;
    cbHideSidePanel: TCheckBox;
    cbShowSidepanelOnStartup: TCheckBox;
    Label108: TLabel;
    Bevel65: TBevel;
    Label109: TLabel;
    Bevel67: TBevel;
    Bevel72: TBevel;
    DescriptionMemo: TMemo;
    Panel2: TPanel;
    Label7: TLabel;
    Label11: TLabel;
    GenerationLabel: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    NWLabel: TLabel;
    SELabel: TLabel;
    Label25: TLabel;
    WidthLabel: TLabel;
    Label26: TLabel;
    HeightLabel: TLabel;
    AreaLabel: TLabel;
    Label27: TLabel;
    CelCountLabel: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    LabelSelNW: TLabel;
    LabelSelSE: TLabel;
    Label32: TLabel;
    LabelSelWidth: TLabel;
    Label34: TLabel;
    LabelSelHeight: TLabel;
    Label36: TLabel;
    LabelSelArea: TLabel;
    Bevel69: TBevel;
    Label110: TLabel;
    Label111: TLabel;
    Bevel71: TBevel;
    Label112: TLabel;
    Bevel51: TBevel;
    Bevel59: TBevel;
    Label113: TLabel;
    LabelNonTot: TLabel;
    ImageBendArrow: TImage;
    Bevel63: TBevel;
    procedure SpeedTrackChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure CBFrameDropTimeKeyPress(Sender: TObject; var Key: Char);
    procedure UDFrameDropTimeClick(Sender: TObject; Button: TUDBtnType);
    procedure CBFrameDropTimeDropDown(Sender: TObject);
    procedure CBFrameDropTimeChange(Sender: TObject);
    procedure CBZoomDropDown(Sender: TObject);
    procedure BB1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridButtonClick(Sender: TObject);
    procedure CBZoomChange(Sender: TObject);
    procedure RuleListBoxClick(Sender: TObject);
    procedure UDZoomClick(Sender: TObject; Button: TUDBtnType);
    procedure CBZoomButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbScrollChange(Sender: TObject);
    procedure cbScrollKeyPress(Sender: TObject; var Key: Char);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure ForeColorGridChange(Sender: TObject);
    procedure BackColorGridChange(Sender: TObject);
    procedure GridColorGridChange(Sender: TObject);
    procedure SmallScrollEditClick(Sender: TObject);
    procedure DrawModeButtonClick(Sender: TObject);
    procedure HandScrollEditChange(Sender: TObject);
    procedure CBZoomKeyPress(Sender: TObject; var Key: Char);
    procedure EditRandomFillChange(Sender: TObject);
    procedure Grid2ColorGridChange(Sender: TObject);
    procedure PasteOrButtonClick(Sender: TObject);
    procedure BoldGridEditChange(Sender: TObject);
    procedure Label15Click(Sender: TObject);
    procedure SelRectGridChange(Sender: TObject);
    procedure ZoomRectGridChange(Sender: TObject);
    procedure DragGridChange(Sender: TObject);
    procedure AutoRewindEditChange(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure AutoRewindEditExit(Sender: TObject);
    procedure cbMaxFitChange(Sender: TObject);
    procedure cbMaxFitDropDown(Sender: TObject);
    procedure cbMaxFitKeyPress(Sender: TObject; var Key: Char);
    procedure UDMaxFitClick(Sender: TObject; Button: TUDBtnType);
    procedure EditPictureWidthChange(Sender: TObject);
    procedure RemoveRuleButtonClick(Sender: TObject);
    procedure InfoMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ChangeRuleButtonClick(Sender: TObject);
    procedure AddRuleButtonClick(Sender: TObject);
    procedure EditLimitBottomKeyPress(Sender: TObject; var Key: Char);
    procedure SpinButton2DownClick(Sender: TObject);
    procedure SpinButton2UpClick(Sender: TObject);
    procedure TorusColorGridChange(Sender: TObject);
    procedure CBNeighborhoodChange(Sender: TObject);
    procedure RuleEditChange(Sender: TObject);
    procedure RuleEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RuleEditKeyPress(Sender: TObject; var Key: Char);
    procedure Bc1Click(Sender: TObject);
    procedure cbN8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MemTrackChange(Sender: TObject);
    procedure ExtListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ButtonAddFileExtClick(Sender: TObject);
    procedure cbSnapshotTimeOutClick(Sender: TObject);
    procedure Image18Click(Sender: TObject);
    procedure MemTrackEnter(Sender: TObject);
  private
    RuleList: TRuleList;
    LifeRules: TLifeRules;
    OldFrameDropText: string;
    OldZoom: integer;

    EditorModeOn: integer;
    GridOn: Boolean;
    //OldRules: string;
    //NewRules: string;
    Speed: integer;
    MemPerBlock: integer;

    Same: array [0..9] of TCheckBox;
    Birth: array[1..9] of TCheckBox;
    NonTot: TNonTotArray;
    NonTotCB: array [false..true,1..7,0..13] of TNewExplButton;
    Speeds: TSpeedsArray;
    OldEditmode: integer;
    OldPasteMode: integer;
    //FA: TFileAssociation;

    procedure UpdateRuleChange;
    procedure SaveDlgSettings;
    procedure GetDlgSettings;
    procedure InitFileAssociations;
    //procedure SaveFileAssociations;
    procedure WMHelp(var Msg: TWMHelp); message WM_Help;
  public
    MyLifeBox: TLifeBox;
  end;

  procedure SaveSettings;
  procedure GetSettings;
  procedure SaveMemPerBlock(value: integer);
  function GetMemPerBlock: integer;
  procedure LifeClosesOK;
  procedure DisableDDrawOnCrash;
  function GetLastFile: string;
  procedure SetLastFile(const LastFile: string);


  function GetMRUList: TStringList;
  procedure RemoveMRUFile(index: integer);
  function GetMRUFilename(index: integer): string;
  procedure AddMRUFile(AFilename: string);

var
  FormPrefs: TFormPrefs;
  PatternPath: string;

implementation

{$if CompilerVersion > 24}
{$define DoStyles}
{$endif}

uses
  Unit1, IniFiles, system.UITypes
  {$ifdef dostyles}
  ,VCL.Styles, VCL.Themes
  {$endif}
  ;

const
  Number  = ['0','1','2','3','4','5','6','7','8','9',#127,#9,#10,#13,#8];
  NotNumber = ['a'..'z','A'..'Z',',','.','<','>','/','?'];
{$R *.DFM}

{$ifdef dostyles}

type
  TTabBackgroundStyleHook = class(VCL.ComCtrls.TTabControlStyleHook)
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
  end;

  procedure TTabBackgroundStyleHook.PaintBackground(Canvas: TCanvas);
  var
    ClientRect: TRect;
    OldBrush: TColor;
    OldPen: TColor;
  begin
    inherited;
    OldBrush:= Canvas.Brush.Color;
    OldPen:= Canvas.Pen.Color;
    Canvas.Brush.Color:= clBtnFace;
    Canvas.Pen.Color:= clBtnFace;
    ClientRect:= Control.ClientRect;
    ClientRect.Top:= ClientRect.Top +1;
    ClientRect.Left:= ClientRect.Left +1;
    ClientRect.Width:= ClientRect.Width-2;
    ClientRect.Height:= ClientRect.Height-2;
    Canvas.FillRect(ClientRect);
    Canvas.Brush.Color:= OldBrush;
    Canvas.Pen.Color:= OldPen;
  end;

{$endif}

procedure TFormPrefs.SpeedTrackChange(Sender: TObject);
var
  TempStr: string;
begin
  with SpeedTrack do begin
    Speed:= Speeds[Position];
    if (Position= 0) then LabelSpeed.Caption:= 'As fast as possible'
    else begin
      if (Position = 1) then TempStr:= '%d millisec to next gen'
      else TempStr:= '%d millisecs to next gen';
      FmtStr(TempStr,TempStr,[Speed]);
      LabelSpeed.Caption:= TempStr;
    end; {else}
    FasterButton.Enabled:= (Speed <> MaxSpeed);
    SlowerButton.Enabled:= (Speed <> MinSpeed);
  end; {with}
end;

procedure TFormPrefs.FormCreate(Sender: TObject);
var
  i: integer;
  TempFloat: extended;
  NewListItem: TListItem;
begin
  GetDlgSettings;
  Birth[1]:= BB1;
  Birth[2]:= BB2;
  Birth[3]:= BB3;
  Birth[4]:= BB4;
  Birth[5]:= BB5;
  Birth[6]:= BB6;
  Birth[7]:= BB7;
  Birth[8]:= BB8;

  Same[0]:= SS0;
  Same[1]:= SS1;
  Same[2]:= SS2;
  Same[3]:= SS3;
  Same[4]:= SS4;
  Same[5]:= SS5;
  Same[6]:= SS6;
  Same[7]:= SS7;
  Same[8]:= SS8;

  NonTotCB[true,1,1]:= Bc1;        NonTotCB[true,7,1]:= Bc7;
  NonTotCB[true,1,2]:= Be1;        NonTotCB[true,7,2]:= Be7;

  NonTotCB[true,2,1]:= Bc2;        NonTotCB[true,6,1]:= Bc6;
  NonTotCB[true,2,2]:= Be2;        NonTotCB[true,6,2]:= Be6;
  NonTotCB[true,2,3]:= Bk2;        NonTotCB[true,6,3]:= Bk6;
  NonTotCB[true,2,4]:= Ba2;        NonTotCB[true,6,4]:= Ba6;
  NonTotCB[true,2,5]:= Bi2;        NonTotCB[true,6,5]:= Bi6;
  NonTotCB[true,2,6]:= Bv2;        NonTotCB[true,6,6]:= Bv6;

  NonTotCB[true,3,1]:= Bc3;        NonTotCB[true,5,1]:= Bc5;
  NonTotCB[true,3,2]:= Be3;        NonTotCB[true,5,2]:= Be5;
  NonTotCB[true,3,3]:= Bk3;        NonTotCB[true,5,3]:= Bk5;
  NonTotCB[true,3,4]:= Ba3;        NonTotCB[true,5,4]:= Ba5;
  NonTotCB[true,3,5]:= Bi3;        NonTotCB[true,5,5]:= Bi5;
  NonTotCB[true,3,6]:= Bv3;        NonTotCB[true,5,6]:= Bv5;
  NonTotCB[true,3,7]:= By3;        NonTotCB[true,5,7]:= By5;
  NonTotCB[true,3,8]:= Bq3;        NonTotCB[true,5,8]:= Bq5;
  NonTotCB[true,3,9]:= Bj3;        NonTotCB[true,5,9]:= Bj5;
  NonTotCB[true,3,10]:= Br3;       NonTotCB[true,5,10]:= Br5;

  NonTotCB[true,4,1]:= Bc4;
  NonTotCB[true,4,2]:= Be4;
  NonTotCB[true,4,3]:= Bk4;
  NonTotCB[true,4,4]:= Ba4;
  NonTotCB[true,4,5]:= Bi4;
  NonTotCB[true,4,6]:= Bv4;
  NonTotCB[true,4,7]:= By4;
  NonTotCB[true,4,8]:= Bq4;
  NonTotCB[true,4,9]:= Bj4;
  NonTotCB[true,4,10]:= Br4;
  NonTotCB[true,4,11]:= Bt4;
  NonTotCB[true,4,12]:= Bw4;
  NonTotCB[true,4,13]:= Bz4;

  NonTotCB[false,1,1]:= Sc1;        NonTotCB[false,7,1]:= Sc7;
  NonTotCB[false,1,2]:= Se1;        NonTotCB[false,7,2]:= Se7;

  NonTotCB[false,2,1]:= Sc2;        NonTotCB[false,6,1]:= Sc6;
  NonTotCB[false,2,2]:= Se2;        NonTotCB[false,6,2]:= Se6;
  NonTotCB[false,2,3]:= Sk2;        NonTotCB[false,6,3]:= Sk6;
  NonTotCB[false,2,4]:= Sa2;        NonTotCB[false,6,4]:= Sa6;
  NonTotCB[false,2,5]:= Si2;        NonTotCB[false,6,5]:= Si6;
  NonTotCB[false,2,6]:= Sv2;        NonTotCB[false,6,6]:= Sv6;

  NonTotCB[false,3,1]:= Sc3;        NonTotCB[false,5,1]:= Sc5;
  NonTotCB[false,3,2]:= Se3;        NonTotCB[false,5,2]:= Se5;
  NonTotCB[false,3,3]:= Sk3;        NonTotCB[false,5,3]:= Sk5;
  NonTotCB[false,3,4]:= Sa3;        NonTotCB[false,5,4]:= Sa5;
  NonTotCB[false,3,5]:= Si3;        NonTotCB[false,5,5]:= Si5;
  NonTotCB[false,3,6]:= Sv3;        NonTotCB[false,5,6]:= Sv5;
  NonTotCB[false,3,7]:= Sy3;        NonTotCB[false,5,7]:= Sy5;
  NonTotCB[false,3,8]:= Sq3;        NonTotCB[false,5,8]:= Sq5;
  NonTotCB[false,3,9]:= Sj3;        NonTotCB[false,5,9]:= Sj5;
  NonTotCB[false,3,10]:= Sr3;       NonTotCB[false,5,10]:= Sr5;

  NonTotCB[false,4,1]:= Sc4;
  NonTotCB[false,4,2]:= Se4;
  NonTotCB[false,4,3]:= Sk4;
  NonTotCB[false,4,4]:= Sa4;
  NonTotCB[false,4,5]:= Si4;
  NonTotCB[false,4,6]:= Sv4;
  NonTotCB[false,4,7]:= Sy4;
  NonTotCB[false,4,8]:= Sq4;
  NonTotCB[false,4,9]:= Sj4;
  NonTotCB[false,4,10]:= Sr4;
  NonTotCB[false,4,11]:= St4;
  NonTotCB[false,4,12]:= Sw4;
  NonTotCB[false,4,13]:= Sz4;

  MyLifeBox:= Form1.MyLifeBox;
  MemPerBlock:= Form1.MemPerBlock;
  Speed:= Form1.PlaySpeed;
  Speeds:= Form1.GetPlaySpeeds;
  UDSmallScroll.Position:= Form1.SmallScroll;
  OldEditmode:= MyLifeBox.Editormode;
  case OldEditmode of
    emDraw: DrawmodeButton.Down:= true;
    emSelect: SelectModeButton.Down:= true;
    emHand: ScrollModeButton.Down:= true;
    emCursorZoom: ZoomModeButton.Down:= true;
  end; {case}
  OldPasteMode:= MyLifeBox.PasteMode;
  case OldPasteMode of
    lpmOr: PasteOrButton.Down:= true;
    lpmPut: PastePutButton.Down:= true;
    lpmXor: PasteXorButton.Down:= true;
    lpmError: PasteErrorButton.Down:= true;
  end; {case}
  cbZoomToFit.Checked:= Form1.ZoomToFit;
  UDMaxFit.Position:= Form1.MaxZoomToFit;
  cbReshowSkip.Checked:= Form1.ReshowSkip;
  cbHideSidePanel.Checked:= Form1.HidePanelOnPlay;
  cbShowSidepanelOnStartup.Checked:= Form1.ShowSidepanelOnStartup;
  AutoRewindEdit.Text:= IntToStr(Form1.SnapshotEvery);
  with SpeedTrack do begin
    Min:= 0;
    Max:= NumSpeeds-1;
    for i:= 0 to NumSpeeds-1 do if (Speed = Speeds[i]) then Position:= i;
  end; {with}
  with MemTrack do begin
    for i:= 0 to 4 do if MemPerBlock = MemsArray[i] then Position:= i;
  end; {with}
  
  with MyLifeBox do begin
    UDFramedropTime.Position:= FrameDropTime;
    UDZoom.Position:= PixelsPerCel;
    if (PixelsPerCel > 0) then cbZoom.Text:= IntToStr(PixelsPerCel)
    else cbZoom.Text:= '1/' + IntToStr(1 shl (-PixelsPerCel)); //this is because the 2 are not and should not be linked.
    UDHandScroll.Position:= HandScroll;
    UDFillRandom.Position:= FillPercentage;
    UDBoldGrid.Position:= BoldGridSpacing;
    cbFreeze.Checked:= FreezeSelection;
    OldZoom:= PixelsPerCel;
    //OldRules:= Universe.RuleString;
    LifeRules:= TLifeRules.Create;
    LifeRules.Rulestring:= Universe.RuleString;
    cbDefaultToolTipColor.checked:= Form1.OnlyUseDefaultToolTipColors;
    GridOn:= Grid;
    GridButton.Down:= GridOn;
    GridButton.Enabled:= (PixelsPerCel > 3);
    EditorModeOn:= EditorMode;
  end; {with}
  with LifeCel.MyDDSurface do begin
    CBDDraw.Checked:= ((DDFast and DirectDrawEnabled)=DDFast);
    if not CBDDraw.Checked then begin
      //test if DDraw is installed.
      if not(Installed) then begin
        CBDDraw.Visible:= false;
        NotInstalledLabel.Visible:= true;
      end {if}
    end; {if}
  end; {with}
  Speed:= Form1.PlaySpeed;
  //NewRules:= OldRules;
  UDPictureWidth.Position:= LifeLoad.MaxPictureWidth;

  cbAutoRewindNew.Checked:= Form1.AutoRewindWhat[cpClear];
  cbAutoRewindOpen.Checked:= Form1.AutoRewindWhat[cpOpen];
  cbAutoRewindPlay.Checked:= Form1.AutoRewindWhat[cpPlay];
  cbAutoRewindDraw.Checked:= Form1.AutoRewindWhat[cpDraw];
  cbAutoRewindEdit.Checked:= Form1.AutoRewindWhat[cpSelect];
  cbAutoRewindRewind.Checked:= Form1.AutoRewindWhat[cpRewind];
  cbAutoRewindStep.Checked:= Form1.AutoRewindWhat[cpStep];
  cbAutoRewindRotate.Checked:= Form1.AutoRewindWhat[cpRotate];
  cbAutoRewindEveryX.Checked:= Form1.AutoRewindWhat[cpAuto];
  UDSnapshotTimeOut.Position:= Form1.SnapshotTimer.Interval;
  cbSnapshotTimeOut.Checked:= not(Form1.NoSnapshotTimeOut);

  //Pattern properties stuff
  DescriptionMemo.Lines.Assign(Form1.MyLifeBox.Universe.Description);
  CelCountLabel.Caption:= Form1.CelLabel.Caption;
  with MyLifeBox.Universe.GetBoundingBox do begin
    NWLabel.Caption:= Format('(%d,%d)',[Left,Top]);
    SELabel.Caption:= Format('(%d,%d)',[Right,Bottom]);
    WidthLabel.Caption:= Format('%d',[Right-Left]);
    HeightLabel.Caption:= Format('%d',[Bottom-top]);
    TempFloat:= (abs(Right-Left))*abs((Bottom-Top));
    AreaLabel.Caption:= Format('%.0n',[TempFloat]);
    AreaLabel.Hint:= Format('%.0n',[TempFloat]);
  end; {with}
  with MyLifeBox.SelectionRect do begin
    LabelSelNW.Caption:= Format('(%d,%d)',[Left,Top]);
    LabelSelSE.Caption:= Format('(%d,%d)',[Right,Bottom]);
    LabelSelWidth.Caption:= Format('%d',[Right-Left]);
    LabelSelHeight.Caption:= Format('%d',[Bottom-top]);
    TempFloat:= (abs(Right-Left))*abs((Bottom-Top));
    LabelSelArea.Caption:= Format('%.0n',[TempFloat]);
    LabelSelArea.Hint:= Format('%.0n',[TempFloat]);
  end; {with}
  GenerationLabel.Caption:= IntToStr(MyLifeBox.Generation);
  //Rule stuff
  //***************************
  //Create the rulelist.
  RuleList:= TRuleList.Create;
  with RuleList do begin
    RuleListView.Items.Clear;
    i:= 0;
    while i < Count do begin
      NewListItem:= RuleListView.Items.Add;
      NewListItem.Caption:= TRuleItem(Items[i]).Rule;
      NewListItem.SubItems.Add(TRuleItem(Items[i]).Name);
      NewListItem.Data:= Items[i];
      Inc(i);
    end; {while}
  end; {with}
  //RuleEdit.Text:= OldRules;
  RuleEdit.Text:= LifeRules.Rulestring;
  NewListItem:= RuleListView.FindCaption(0,LifeRules.Rulestring,false,true,false);
  if NewListItem <> nil then begin
    InfoMemo.Text:= TRuleItem(NewListItem.Data).LongInfo;
    ShortInfoEdit.Text:= TRuleItem(NewListItem.Data).ShortInfo;
    NewListItem.Selected:= true;
  end;
  InitFileAssociations;
  //CBNeighborhood.Text:= NeighborHoodToStrLong(MyLifeBox.Universe.Neighborhood);
  //CBNeighborhoodChange(Self);
end;

procedure TFormPrefs.InitFileAssociations;
//var
  //i: integer;
  //Ext: string;
  //AnItem: TListItem;
begin
  ExtListView.Tag:= tBusy;
  //FA:= TFileAssociation.CreateFrom(Form1.SCInterface.FileAssociations[0]);
  //i:= FA.Exts.Count;
  //while i > 0 do begin
  //  Dec(i);
  //  Ext:= '.'+FA.Exts[i];
  //  AnItem:= ExtListView.FindCaption(0,Ext,false,true,true);
  // if Assigned(AnItem) then AnItem.Checked:= true;
  //end;
  ExtListView.Tag:= tFree;
end;

procedure TFormPrefs.FormShow(Sender: TObject);
var
  FoundAt: integer;
  SelAttr: TTextAttributes;
begin
  SpeedTrackChange(Self);
  MemTrackChange(Self);
  cbScrollChange(Self);
  HandScrollEditChange(Self);
  cbZoomChange(cbZoom);
  CBFrameDropTimeChange(cbFrameDropTime);
  CBNeighborhoodChange(self);

  //Show the 'before' word in bold.
  with RichEdit1 do begin //@@@@@@ Hack alert.
    FoundAt:= findText('before',0,MaxInt,[]);
    if FoundAt <> -1 then begin
      SelStart:= FoundAt;
      SelLength:= Length('before');
      SelAttr:= SelAttributes;
      SelAttr.Style:= SelAttr.Style + [fsBold];
      SelAttributes:= SelAttr;
      SelLength:= 0;
    end; {if}
  end; {with}

  with MyLifeBox do begin
    //Colors
    GridShape.Brush.Color:= GridColor;
    Grid2Shape.Brush.Color:= Grid2Color;
    BackShape.Brush.Color:= BackColor;
    ForeShape.Brush.Color:= CelColor;
    SelRectShape.Brush.Color:= SelRectColor;
    ZoomRectShape.Brush.Color:= ZoomRectColor;
    DragShape.Brush.Color:= DragColor;
    TorusShape.Brush.Color:= TorusColor;
  end; {with}
end;

procedure TFormPrefs.Label1Click(Sender: TObject);
begin
  SpeedTrack.Position:= SpeedTrack.Position - 1;
  //due to bug in delphi trackbar
  SpeedTrackChange(Self);
end;

procedure TFormPrefs.Label2Click(Sender: TObject);
begin
  SpeedTrack.Position:= SpeedTrack.Position + 1;
  //due to bug in delphi trackbar
  SpeedTrackChange(Self);
end;


procedure TFormPrefs.ButtonOKClick(Sender: TObject);
var
  sms: integer;
begin
  Form1.MyLifeBox.CanPaint:= cpDontPaint;
  with MyLifeBox do begin
    FreezeSelection:= cbFreeze.Checked;
    //Universe.RuleString:= NewRules;
    Universe.RuleString:= LifeRules.RuleString;
    FrameDropTime:= UDFramedropTime.Position;
    HandScroll:= UDHandScroll.Position;
    FillPercentage:= UDFillRandom.Position;
    BoldGridSpacing:= UDBoldGrid.Position;
    Grid:= GridOn;
    //EditorMode:= EditorModeOn;
    CelColor:= ForeShape.Brush.Color;
    BackColor:= BackShape.Brush.Color;
    GridColor:= GridShape.Brush.Color;
    Grid2Color:= Grid2Shape.Brush.Color;
    SelRectColor:= SelRectShape.Brush.Color;
    ZoomRectColor:= ZoomRectShape.Brush.Color;
    DragColor:= DragShape.Brush.Color;
    TorusColor:= TorusShape.Brush.Color;
  end; {with}
  Form1.CelZoom:= UDZoom.Position;
  Form1.ZoomToFit:= cbZoomToFit.Checked;
  Form1.ReshowSkip:= cbReshowSkip.Checked;
  Form1.HidePanelOnPlay:= cbHideSidePanel.Checked;
  Form1.ShowSidepanelOnStartup:= cbShowSidepanelOnStartup.Checked;
  Form1.MaxZoomToFit:= UDMaxFit.Position;
  Form1.OnlyUseDefaultToolTipColors:= cbDefaultToolTipColor.Checked;
  OldZoom:= Form1.CelZoom;
  Form1.PlaySpeed:= Speed;
  with LifeCel.MyDDSurface do try
    if CBDDraw.Checked then DirectDrawEnabled:= DirectDrawEnabled or DDFast
    else DirectDrawEnabled:= DirectDrawEnabled and not DDFast;
    except {ignore}
  end; {with try}
  LifeLoad.MaxPictureWidth:= UDPictureWidth.Position;
  try
    sms:= StrToInt(SmallScrollEdit.Text);
    if sms > 0 then Form1.SmallScroll:= sms;
    except {ignore}
  end; {try}

  Form1.AutoRewindWhat[cpClear]:= cbAutoRewindNew.Checked;
  Form1.AutoRewindWhat[cpOpen]:= cbAutoRewindOpen.Checked;
  Form1.AutoRewindWhat[cpPlay]:= cbAutoRewindPlay.Checked;
  Form1.AutoRewindWhat[cpDraw]:= cbAutoRewindDraw.Checked;
  Form1.AutoRewindWhat[cpSelect]:= cbAutoRewindEdit.Checked;
  Form1.AutoRewindWhat[cpRewind]:= cbAutoRewindRewind.Checked;
  Form1.AutoRewindWhat[cpStep]:= cbAutoRewindStep.Checked;
  Form1.AutoRewindWhat[cpRotate]:= cbAutoRewindRotate.Checked;
  try
    Form1.SnapshotEvery:= StrToInt(AutoRewindEdit.Text);
    except Form1.SnapshotEvery:= 0;
  end; {try}
  Form1.AutoRewindWhat[cpAuto]:=
    cbAutoRewindEveryX.Checked and (Form1.SnapshotEvery > 0);
  Form1.NoSnapshotTimeOut:= not(cbSnapshotTimeOut.Checked);
  Form1.SnapshotTimer.Interval:= UDSnapshotTimeOut.Position;

  //try
  //  sms:= StrToInt(BoldGridEdit.Text);
  //  if sms > 0 then MyLifeBox.BoldGridSpacing:= sms;
  //  except {ignore}
  //end; {try}
  SaveSettings;
  SaveMemPerBlock(MemPerBlock);
  //pattern properties stuff.
  Form1.MyLifeBox.Universe.Description.Assign(DescriptionMemo.Lines);
  //Form1.CurrentLifeBox.CanPaint:= cpDialogShowing;
  RuleList.SaveToRegistry;
  RuleList.SaveToMenu(Form1.RulePopup, Form1.N28Click);
  Form1.MyLifeBox.Universe.Neighborhood:= strToNeighBorhood(CBNeighborhood.Text);
  //File associations
  //Form1.Extensions:= FA.Exts.CommaText;
  //Form1.SCInterface.UnRegisterFileAssociations;
  //Form1.SCInterface.FileAssociations[0]:= FA.CSVConfig;
  //Form1.SCInterface.RegisterFileAssociations(true);
end;

procedure SaveSettings;
var
  MyReg: TCustomIniFile;
  i: integer;
  TempDDE: TDDStates;
begin
  MyReg:= OpenReg;
  if MyReg = nil then exit;
  try
  with MyReg do try
    TempDDE:= LifeCel.MyDDSurface.DirectDrawEnabled and not DDTempDisabled;
    WriteInteger('Settings','DirectDrawEnabled',TempDDE);
    WriteBool('Settings','ReshowSkip',Form1.ReshowSkip);
    WriteBool('Settings','HidePanelOnPlay',Form1.HidePanelOnPlay);
    WriteBool('Settings','ShowSidepanelOnStartup',Form1.ShowSidepanelOnStartup);
    WriteInteger('Settings','PlaySpeed',Form1.PlaySpeed);
    WriteInteger('Settings','CelZoom',Form1.CelZoom);
    WriteBool('Settings','ZoomToFit',Form1.ZoomToFit);
    WriteInteger('Settings','MaxZoomToFit', Form1.MaxZoomToFit);
    WriteBool('Settings','OnlyUseDefaultToolTipColors',Form1.OnlyUseDefaultToolTipColors);
    WriteString('Settings','PatternDir',Form1.SaveDialog1.InitialDir);
    WriteInteger('Settings','SmallScroll',Form1.SmallScroll);
    WriteInteger('Settings','SnapshotEvery',Form1.SnapshotEvery);
    WriteInteger('Settings','MaxPictureWidth',LifeLoad.MaxPictureWidth);
    WriteInteger('Settings','SnapshotTimeOut',Form1.SnapshotTimer.Interval);
    WriteBool('Settings','NoSnapshotTimeOut',Form1.NoSnapshotTimeOut);
    WriteString('Settings','Exe',Application.Exename);
    WriteString('Settings','Extensions',Form1.Extensions);
    for i:= cpFirst to cpLast do
      WriteBool('Settings',RewindName[i],Form1.AutoRewindWhat[i]);
    with Form1.LifeBox1 do begin
      WriteBool('Settings','FreezeSelection',FreezeSelection);
      WriteInteger('Settings','FillPercentage',FillPercentage);
      WriteInteger('Settings','Editormode',EditorMode);
      WriteInteger('Settings','Pastemode',PasteMode);
      WriteInteger('Settings','FrameDropTime',FrameDropTime);
      WriteBool('Settings','Grid',Grid);
      WriteInteger('Settings','BoldGridSpacing',BoldGridSpacing);
      WriteInteger('Settings','HandScroll',HandScroll);
      WriteInteger('Settings','CelColor',CelColor);
      WriteInteger('Settings','BackColor',BackColor);
      WriteInteger('Settings','GridColor',GridColor);
      WriteInteger('Settings','Grid2Color',Grid2Color);
      WriteInteger('Settings','SelRectColor',SelRectColor);
      WriteInteger('Settings','ZoomRectColor',ZoomRectColor);
      WriteInteger('Settings','DragColor',DragColor);
      WriteInteger('Settings','TorusColor',TorusColor);
      //Torus stuff
      WriteInteger('Settings','LimitLeft',Limit.Left);
      WriteInteger('Settings','LimitRight',Limit.Right);
      WriteInteger('Settings','LimitTop',Limit.Top);
      WriteInteger('Settings','LimitBottom',Limit.Bottom);
      WriteBool('Settings','IsLimited',IsLimited);
      WriteBool('Settings','DeadEdges',DeadEdges);
      WriteInteger('Settings','TorusKind',TorusKind);
    end; {with}
    finally MyReg.Free;
  end; {with}
  except
    {ignore}
  end;
end;

procedure GetSettings;
var
  i: integer;
  LimitRect: TRect;

  function RewindDefault(i: integer): boolean;
  begin
    if (i = cpStep) then Result:= false
    else Result:= true;
  end;

begin
  with OpenReg do try
    LifeCel.MyDDSurface.DirectDrawEnabled:=
      ReadInteger('Settings','DirectDrawEnabled',DefaultDirectDrawEnabled);
    Form1.ReshowSkip:= ReadBool('Settings','ReshowSkip',DefaultReshowSkip);
    Form1.HidePanelOnPlay:= ReadBool('Settings','HidePanelOnPlay',DefaultHidePanelOnPlay);
    Form1.ShowSidepanelOnStartup:=
      ReadBool('Settings','ShowSidepanelOnStartup',DefaultShowSidepanelOnStartup);
    Form1.PlaySpeed:= ReadInteger('Settings','PlaySpeed',DefaultSpeed);
    Form1.CelZoom:= ReadInteger('Settings','CelZoom',DefaultZoom);
    Form1.ZoomToFit:= ReadBool('Settings','ZoomToFit',DefaultZoomToFit);
    Form1.MaxZoomToFit:= ReadInteger('Settings','MaxZoomToFit',MaxZoom);
    Form1.OnlyUseDefaultToolTipColors:=
      ReadBool('Settings','OnlyUseDefaultToolTipColors',false);
    Form1.SmallScroll:= ReadInteger('Settings','SmallScroll',DefaultScroll);
    Form1.NoSnapshotTimeOut:= ReadBool('Settings','NoSnapshotTimeOut',DefaultNoSnapshotTimeOut);
    Form1.SnapshotTimer.Interval:= ReadInteger('Settings','SnapshotTimeOut',DefaultSnapshotTimeOut);
    if Form1.SnapshotTimer.Interval < MinSnapshotTimeOut then
      Form1.SnapshotTimer.Interval:= MinSnapshotTimeOut;
    Form1.SnapshotEvery:= ReadInteger('Settings','SnapshotEvery',0);
    LifeLoad.MaxPictureWidth:= ReadInteger('Settings','MaxPictureWidth',DefaultPictureWidth);
    //check here, as Lifeload is not an object
    if MaxPictureWidth > 80 then MaxPictureWidth:= 80
    else if MaxPictureWidth < 39 then MaxPictureWidth:= 39;
    Form1.SaveDialog1.InitialDir:= ReadString('Settings','PatternDir','');
    Form1.Extensions:= ReadString('Settings','Extensions',DefaultExtensions);
    if Form1.Extensions = '' then Form1.Extensions:= DefaultExtensions;
    for i:= cpFirst to cpLast do begin
      Form1.AutoRewindWhat[i]:= ReadBool('Settings',RewindName[i],RewindDefault(i));
    end; {for i}
    with Form1.MyLifeBox do begin
      FreezeSelection:= ReadBool('Settings','FreezeSelection',false);
      FillPercentage:= ReadInteger('Settings','FillPercentage',DefaultFillPercentage);
      Grid:= ReadBool('Settings','Grid',DefaultGrid);
      BoldGridSpacing:= ReadInteger('Settings','BoldGridSpacing',DefaultBoldGridSpacing);
      HandScroll:= ReadInteger('Settings','HandScroll',DefaultScroll);
      FrameDropTime:= ReadInteger('Settings','FrameDropTime',DefaultFramedropTime);
      EditorMode:= ReadInteger('Settings','Editormode',emSelect);
      PasteMode:= ReadInteger('Settings','Pastemode',lpmOr);
      CelColor:= ReadInteger('Settings','CelColor',DefaultCelColor);
      BackColor:= ReadInteger('Settings','BackColor',DefaultBackColor);
      GridColor:= ReadInteger('Settings','GridColor',DefaultGridColor);
      Grid2Color:= ReadInteger('Settings','Grid2Color',DefaultGrid2Color);
      SelRectColor:= ReadInteger('Settings','SelRectColor',DefaultSelRectColor);
      ZoomRectColor:= ReadInteger('Settings','ZoomRectColor',DefaultZoomRectColor);
      DragColor:= ReadInteger('Settings','DragColor',DefaultDragColor);
      TorusColor:= ReadInteger('Settings','TorusColor',DefaultTorusColor);
      //Torus stuff

      LimitRect.Left:= ReadInteger('Settings','LimitLeft',DefaultLimitRect.Left);
      LimitRect.Top:= ReadInteger('Settings','LimitTop',DefaultLimitRect.Top);
      LimitRect.Right:= ReadInteger('Settings','LimitRight',DefaultLimitRect.Right);
      LimitRect.Bottom:= ReadInteger('Settings','LimitBottom',DefaultLimitRect.Bottom);
      Limit:= LimitRect;
      IsLimited:= ReadBool('Settings','IsLimited',DefaultIsLimited);
      TorusKind:= ReadInteger('Settings','TorusKind',DefaultTorusKind);
      DeadEdges:= ReadBool('Settings','DeadEdges',DefaultDeadEdges);
    end; {with}
    finally
      Free;
  end; {try}
end;

procedure SaveMemPerBlock(value: integer);
var
  MyReg: TCustomIniFile;
begin
  MyReg:= OpenReg;
  try
  with MyReg do try
    WriteInteger('Settings','MemPerBlock',value);
  finally MyReg.Free;
  end; {with}
  except
    {ignore}
  end;
end;

function GetMemPerBlock: integer;
var
  MyReg: TCustomIniFile;
  i: integer;
  OK: boolean;
begin
  MyReg:= OpenReg;
  if MyReg = nil then exit(DefaultMemPerBlock);

  with MyReg do try
    Result:= ReadInteger('Settings','MemPerBlock',DefaultMemPerBlock);
  finally MyReg.Free;
  end; {with}
  OK:= false;
  for i:= 0 to 4 do begin
    if Result = MemsArray[i] then OK:= true;
  end; {for i}
  if not(OK) then Result:= DefaultMemPerBlock;
end;

procedure LifeClosesOK;
var
  TempDDE: TDDStates;
  i: byte;
begin
  with OpenReg do try
    TempDDE:= (LifeCel.MyDDSurface.DirectDrawEnabled and (not DDTempDisabled)) or DDExitOK;
    i:= byte((@TempDDE)^);
    WriteInteger('Settings','DirectDrawEnabled',i);
    finally Free;
  end; {with}
end;

procedure DisableDDrawOnCrash;
var
  TempDDE: TDDStates;
begin
  try
  with OpenReg do try
    TempDDE:= ReadInteger('Settings','DirectDrawEnabled', DDFast);
    if ((DDExitOK and TempDDE)=DDExitOK) then TempDDE:= TempDDE and not DDExitOK
    else TempDDE:= 0; //if not closed OK, disable directdraw.
    WriteInteger('Settings','DirectDrawEnabled',TempDDE);
    finally Free;
  end; {with}
  except
    {ignore}
  end;
end;

function GetLastFile: string;
var
  MRUFiles: TStringList;
begin
  MRUFiles:= GetMRUList;
  try
    if MRUFiles.count > 0 then Result:= MRUFiles[0]
    else Result:= '';
    finally MRUFiles.Free;
  end; {try}
end;

procedure SetLastFile(const LastFile: string);
begin
  AddMRUFile(LastFile);
end;

procedure TFormPrefs.ButtonCancelClick(Sender: TObject);
begin
  MyLifeBox.CanPaint:= cpDontPaint;
  //MyLifeBox.Universe.RuleString:= OldRules;
  MyLifeBox.Editormode:= OldEditmode;
  MyLifeBox.PasteMode:= OldPasteMode;
end;


procedure TFormPrefs.WMHelp(var Msg: TWMHelp);
//var
//  KeyWord: array[0..25] of char;
begin
  //KeyWord:= 'Settings'#0;
  WinHelp(Handle, 'Life32.hlp',Help_Context,
          HelpOnSettings + TabbedNoteBook1.PageIndex);
  //WinHelp(Handle, 'Life32.hlp', Help_PartialKey, DWord(@KeyWord[0]));
end;

procedure TFormPrefs.GetDlgSettings;
begin
  with OpenReg do try
    Left:= ReadInteger('Prefs.pas','Left',Screen.Width);
    Top:= ReadInteger('Prefs.pas','Top',Screen.Height);
    finally Free;
    Left:= Min(Left,Screen.Width - Width);
    Top:= Min(Top, Screen.Height - Height);
  end; {with}
end;


procedure TFormPrefs.SaveDlgSettings;
begin
  try
  with OpenReg do try
    WriteInteger('Prefs.pas','Left',Left);
    WriteInteger('Prefs.pas','Top',Top);
    finally Free;
  end; {with}
  except
    {ignore}
  end;
end;



procedure TFormPrefs.CBFrameDropTimeKeyPress(Sender: TObject; var Key: Char);
begin
  OldFrameDropText:= TComboBox(Sender).Text;
end;

procedure TFormPrefs.UDFrameDropTimeClick(Sender: TObject; Button: TUDBtnType);
begin
  CBFrameDropTime.Text:= IntToStr(UDFramedropTime.Position);
  CBFrameDropTimeChange(Sender);
end;

procedure TFormPrefs.CBFrameDropTimeDropDown(Sender: TObject);
var
  i: integer;
begin
  with TComboBox(Sender) do begin
    Items.Clear;
    for i:= 0 to 10 do Items.Add(IntToStr(i*2));
    for i:= 3 to 10 do Items.Add(IntToStr(i*10));
    for i:= 2 to 10 do Items.Add(IntToStr(i*100));
    try
      i:= StrToInt(Text);
      if i <= 20 then ItemIndex:= i div 2
      else if i <= 100 then ItemIndex:= 9 + (i div 10)
      else if i <= 1000 then ItemIndex:= 10 + 7 + (i div 100)
      else if i > 1000 then ItemIndex:= 10 + 7 + (1000 div 100);
      except ItemIndex:= 0;
    end; {try}
  end; {with}
end;

procedure TFormPrefs.CBFrameDropTimeChange(Sender: TObject);
var
  a: integer;
begin
  with CBFrameDropTime do try
    a:= StrToInt(Text);
    if a = 0 then FramedropLabel.Caption:= 'No framedropping'
    else if a = 1 then FramedropLabel.Caption:= 'Millisec to next frame'
    else FramedropLabel.Caption:= 'Millisecs to next frame';
    if a > 1000 then raise EConvertError.Create('');
    except if Text <> '' then Text:= OldFrameDropText;
  end; {with try}
end;


procedure TFormPrefs.CBZoomDropDown(Sender: TObject);
var
  i: integer;
begin
  with TComboBox(Sender) do begin
    Items.Clear;
    for i:= MinZoom to -1 do Items.Add('1/'+IntToStr(1 shl(-i)));
    for i:= 1 to MaxZoom do Items.Add(IntToStr(i));
  end; {with}
end;

(*procedure TFormPrefs.B1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARule: string;
  i: integer;
begin
  if Sender is TCheckBox then begin
    ARule:= '';
    for i:= 0 to 8 do if (Same[i].Checked and Same[i].Visible) then begin
      ARule:= ARule + IntToStr(i);
    end; {for i}
    ARule:= ARule + '/';
    for i:= 1 to 8 do if (Birth[i].Checked and Birth[i].Visible) then begin
      ARule:= ARule + IntToStr(i);
    end; {for i}
    if ARule <> '/' then try
      LifeRules.Rulestring:= ARule;
      RuleEdit.Text:= LifeRules.Rulestring;
      except {do nothing};
    end; {if try}
  end; {if}
end; (**)

procedure TFormPrefs.BB1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Down: boolean;
  Name: string;
  CB: TCheckbox;
begin
  CB:= TCheckBox(Sender);
  Down:= CB.Checked;
  Name:= CB.Name;
  if Length(Name) <> 3 then ShowMessage('internal error in Bc1Click');
  if (CB.Tag = 0) then RuleEdit.Text:= LifeRules.SetNTState(Name,Down);
end;


procedure TFormPrefs.GridButtonClick(Sender: TObject);
begin
  GridOn:= TSpeedButton(Sender).Down;
end;

procedure TFormPrefs.CBZoomChange(Sender: TObject);
var
  i: integer;
  ZoomTextOK: Boolean;
begin
  try
    ZoomTextOK:= true;
    if (Pos('/',cbZoom.text) <> 0) then begin
      if cbZoom.Text = '1/2' then udzoom.Position:= -1
      else if cbZoom.Text = '1/4' then UDZoom.Position:= -2
      else if cbZoom.Text = '1/8' then UDZoom.Position:= -3
      else if cbZoom.Text = '1/16' then UDZoom.Position:= -4
      else if cbZoom.Text = '1/32' then UDZoom.Position:= -5
      else if cbZoom.Text = '1/64' then UDZoom.Position:= -6
      else if cbZoom.Text = '1/128' then UDZoom.Position:= -7
      else if cbZoom.text = '1/256' then UDZoom.Position:= -8
      else ZoomTextOK:= false;
    end
    else begin
      i:= 0;
      try
        i:= StrToInt(cbZoom.Text)
        except ZoomTextOK:= false;
      end; {try}
      if (i = 0) or (i > MaxZoom) then ZoomTextOK:= false;
      if ZoomTextOK then UDZoom.Position:= i;
    end; {else}
    if ZoomTextOK then cbZoom.Font.Color:= clWindowText
    else cbZoom.Font.Color:= clRed;
    i:= UDZoom.Position;
    if i = 0 then i:= 1;
    GridButton.Enabled:= (i > 3);
    if (i=1) then ZoomLabel.Caption:= 'Pixel per cell'
    else ZoomLabel.Caption:= 'Pixels per cell';
   except {do nothing} ;
  end; {try}
end;

procedure TFormPrefs.RuleListBoxClick(Sender: TObject);
var
  TempStr: string;
  i: integer;
begin
  if (RuleListView.Selected <> nil) then begin
    TempStr:= RuleListView.Selected.Caption;
    i:= pos(#9,TempStr);
    if i <> 0 then TempStr:= Copy(TempStr,1,i-1);
    RuleEdit.Text:= TempStr;
    RuleEdit.OnChange(Sender);
    InfoMemo.Lines.Clear;
    with RuleListView.Selected do begin
      InfoMemo.Lines[0]:= TRuleItem(Data).LongInfo;
      ShortInfoEdit.Text:= TRuleItem(Data).ShortInfo;
    end;
    //Make sure the InfoMemo shows it's topmost line.
    InfoMemo.Lines.Insert(0,'');
    InfoMemo.Lines.Delete(0);
    ChangeRuleButton.Enabled:= false;
  end;
  RemoveRuleButton.Enabled:= (RuleListView.Selected <> nil);
end;

procedure TFormPrefs.UDZoomClick(Sender: TObject; Button: TUDBtnType);
begin
  //Skip the 0 value.
  with UDZoom do if Position = 0 then begin
    if Button = btNext then Position:= 1
    else Position:= -1;
  end; {if}
  case UDZoom.Position of
    1..10: cbZoom.Text:= IntToStr(UDZoom.Position);
    -1: cbZoom.Text:= '1/2';
    -2: cbZoom.Text:= '1/4';
    -3: cbZoom.Text:= '1/8';
    -4: cbZoom.Text:= '1/16';
    -5: cbZoom.Text:= '1/32';
    -6: cbZoom.Text:= '1/64';
    -7: cbZoom.Text:= '1/128';
  end; {case}
  cbZoomChange(Self);
end;

procedure TFormPrefs.CBZoomButtonClick(Sender: TObject);
begin
  CBZoom.DroppedDown:= not(CBZoom.DroppedDown);
end;

procedure TFormPrefs.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveDlgSettings;
  Form1.CelZoom:= OldZoom;
  MyLifeBox.RefreshColors(NoRedraw);
  RuleList.Free;
  //FA.Free;
  //FA:= nil;
end;

procedure TFormPrefs.cbScrollChange(Sender: TObject);
var
  sms: integer;
begin
  if SmallScrollEdit.Text <> '1' then ScrollLabel.Caption:= 'Cells'
  else ScrollLabel.caption:= 'Cell';
  try
    sms:= StrToInt(SmallScrollEdit.Text);
    if sms <= 0 then raise Exception.Create('0 not allowed here');
    SmallScrollEdit.Font.Color:= clWindowText;
    except SmallScrollEdit.Font.Color:= clRed;
  end; {try}
end;

procedure TFormPrefs.cbScrollKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(key,['0'..'9',#0..#31]) then key:= #0;
end;

procedure TFormPrefs.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  cbScrollChange(sender);
end;

function GetMRUFilename(index: integer): string;
var
  MRUList: string;
  MRUChar: char;
begin
  with OpenReg do try
    MRUList:= ReadString(cstrLastOpenedFiles,cstrMRUList,'');
    if MRUList = '' then Result:= ''
    else begin
      MRUChar:= MRUList[index];
      Result:= ReadString(cstrLastOpenedFiles,MRUChar,'');

      //bring newly opened file to the front of the list.
      Delete(MRUList,index,1);
      MRUList:= MRUChar + MRUList;
      WriteString(cstrLastOpenedFiles,cstrMRUList,MRUList);
    end;
    finally Free;
  end; {with}
end;

procedure RemoveMRUFile(Index: integer);
var
  MRUList: string;
begin
  with OpenReg do try
    MRUList:= ReadString(cStrLastOpenedFiles,cstrMRUList,'');
    if (Length(MRUList) >= Index) then begin
      Delete(MRUList,Index,1);
      WriteString(cStrLastOpenedFiles,cstrMRUList,MRUList);
    end; {if}
  finally Free;
  end; {with try}
end;

procedure AddMRUFile(AFilename: string);
var
  MRUList: string;
  i: integer;
  Match: integer;
  NewChar: char;
begin
  with OpenReg do try
    MRUList:= ReadString(cstrLastOpenedFiles,cstrMRUList,'');
    i:= 1;
    Match:= 0;
    while i <= Length(MRUList) do begin
      if CompareText(ReadString(cstrLastOpenedFiles,MRUList[i],''),AFileName) = 0 then begin
        Match:= i;
        i:= 10;
      end;
      inc(i);
    end; {while}
    if Match > 0 then begin
      NewChar:= MRUList[Match];
      Delete(MRUList,Match,1);
      MRUList:= NewChar + MRUList;
      WriteString(cstrLastOpenedFiles,cstrMRUList,MRUList);
    end {if}
    else begin
      if (Length(MRUList) < 10) then begin
        NewChar:= 'a';
        i:= 1;
        while i <= Length(MRUList) do begin
          if (Pos(NewChar,MRUList) = 0) then i:= Length(MRUList)
          else NewChar:= Succ(NewChar);
          Inc(i);
        end; {while}
      end {if}
      //if already more then 10 items in the list replace the last one.
      else begin
        NewChar:= MRUList[10];
        Delete(MRUList,10,1);
      end; {if}
      MRUList:= NewChar + MRUList;
      WriteString(cstrLastOpenedFiles,NewChar,AFileName);
      WriteString(cstrLastOpenedFiles,cstrMRUList,MRUList);
    end; {else}

    finally Free;
  end; {with}
end;

function GetMRUList: TStringList;
var
  MRUList: string;
  i: integer;
begin
  Result:= TStringList.Create;
  with OpenReg do try
    MRUList:= ReadString(cstrLastOpenedFiles,cstrMRUList,'');
    i:= 1;
    while i <= Length(MRUList) do begin
      Result.Add(ReadString(cstrLastOpenedFiles,MRUList[i],''));
      inc(i);
    end; { while }
  finally Free;
  end; { with }
end;

procedure TFormPrefs.ForeColorGridChange(Sender: TObject);
var
  TheColor: TColor;
begin
  TheColor:= ForeColorGrid.ForeGroundColor;
  if TheColor = BackShape.Brush.Color then
    BackShape.Brush.Color:= ForeShape.Brush.Color;
  ForeShape.Brush.Color:= TheColor;

  ForeColorGrid.ForegroundEnabled:= false;
  ForeColorGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.BackColorGridChange(Sender: TObject);
var
  TheColor: TColor;
begin
  TheColor:= BackColorGrid.ForegroundColor;
  if TheColor = ForeShape.Brush.Color then
    ForeShape.Brush.Color:= BackShape.Brush.Color;
  if TheColor = SelRectShape.Brush.Color then
    SelRectShape.Brush.Color:= BackShape.Brush.Color;
  if TheColor = ZoomRectShape.Brush.Color then
    ZoomRectShape.Brush.Color:= BackShape.Brush.Color;
  BackShape.Brush.Color:= TheColor;
  BackColorGrid.ForegroundEnabled:= false;
  BackColorGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.GridColorGridChange(Sender: TObject);
begin
  GridShape.Brush.Color:= GridColorGrid.ForegroundColor;
  GridColorGrid.ForegroundEnabled:= false;
  GridColorGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.SmallScrollEditClick(Sender: TObject);
begin
  TEdit(Sender).SelectAll;
end;

procedure TFormPrefs.DrawModeButtonClick(Sender: TObject);
begin
  MyLifeBox.Editormode:= TControl(Sender).Tag;
end;

procedure TFormPrefs.HandScrollEditChange(Sender: TObject);
var
  sms: integer;
begin
  if HandScrollEdit.Text <> '1' then HandLabel.Caption:= 'Cells'
  else HandLabel.caption:= 'Cell';
  try
    sms:= StrToInt(HandScrollEdit.Text);
    if sms <= 0 then raise Exception.Create('0 not allowed here');
    HandScrollEdit.Font.Color:= clWindowText;
    except HandScrollEdit.Font.Color:= clRed;
  end; {try}
end;

procedure TFormPrefs.CBZoomKeyPress(Sender: TObject; var Key: Char);
begin
  if cbZoom.Items.Count > 0 then begin
    cbZoom.Items.Clear;
    cbZoom.SelectAll;
  end;
  if not CharInSet(Key,['0'..'9',#0..#31,'/']) then Key:= #0;
end;

procedure TFormPrefs.EditRandomFillChange(Sender: TObject);
var
  i: integer;
begin
  try
    i:= ProcentToInt(EditRandomFill.Text);
    if (i < 1) or (i > 100) then
      raise Exception.Create('Must be between 1 and 100');
    EditRandomFill.Font.Color:= clWindowText;
    except EditRandomFill.Font.Color:= clRed;
  end; {try}
end;

procedure TFormPrefs.Grid2ColorGridChange(Sender: TObject);
begin
  Grid2Shape.Brush.Color:= Grid2ColorGrid.ForegroundColor;
  Grid2ColorGrid.ForegroundEnabled:= false;
  Grid2ColorGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.PasteOrButtonClick(Sender: TObject);
begin
  MyLifeBox.PasteMode:= TControl(sender).Tag;
end;

procedure TFormPrefs.BoldGridEditChange(Sender: TObject);
var
  sms: integer;
begin
  if BoldGridEdit.Text <> '1' then BoldGridLabel.Caption:= 'Cells'
  else BoldGridLabel.caption:= 'Cell';
  try
    sms:= StrToInt(BoldGridEdit.Text);
    if sms <= 0 then raise Exception.Create('0 not allowed here');
    BoldGridEdit.Font.Color:= clWindowText;
    except BoldGridEdit.Font.Color:= clRed;
  end; {try}
end;

procedure TFormPrefs.Label15Click(Sender: TObject);
var
  CheckBox: TCheckBox;
begin
  CheckBox:= nil;
  case (Sender as TComponent).Tag of
    0: CheckBox:= cbAutoRewindNew;
    1: CheckBox:= cbAutoRewindOpen;
    2: CheckBox:= cbAutoRewindPlay;
    3: CheckBox:= cbAutoRewindStep;
    4: CheckBox:= cbAutoRewindDraw;
    5: CheckBox:= cbAutoRewindEdit;
    6: CheckBox:= cbAutoRewindRewind;
    7: CheckBox:= cbAutoRewindRotate;
    8: CheckBox:= cbAutoRewindEveryX;
  end; {case}
  CheckBox.Checked:= not(CheckBox.Checked);
  CheckBox.SetFocus;
end;

procedure TFormPrefs.SelRectGridChange(Sender: TObject);
begin
  if SelRectGrid.ForeGroundColor <> BackShape.Brush.Color then
    SelRectShape.Brush.Color:= SelRectGrid.ForegroundColor;
  SelRectGrid.ForegroundEnabled:= false;
  SelRectGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.ZoomRectGridChange(Sender: TObject);
begin
  if ZoomRectGrid.ForeGroundColor <> BackShape.Brush.Color then
    ZoomRectShape.Brush.Color:= ZoomRectGrid.ForegroundColor;
  ZoomRectGrid.ForegroundEnabled:= false;
  ZoomRectGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.DragGridChange(Sender: TObject);
var
  NewColor: TColor;
begin
  NewColor:= DragGrid.ForeGroundColor;
  if (NewColor <> BackShape.Brush.Color) then
    DragShape.Brush.Color:= NewColor;
  DragGrid.ForegroundEnabled:= false;
  DragGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.AutoRewindEditChange(Sender: TObject);
var
  i: integer;
begin
  inherited;
  try
    i:= StrToInt(AutoRewindEdit.Text);
    if (i < 0) then raise Exception.Create('Out of range');
    if (i = 0) then cbAutoRewindEveryX.Checked:= false;
    AutoRewindEdit.Font.Color:= clWindowText;
  except
    AutoRewindEdit.Font.Color:= clRed;
  end; {try}
end;

procedure TFormPrefs.SpinButton1DownClick(Sender: TObject);
var
  i: integer;
begin
  try
    i:= StrToInt(AutoRewindEdit.Text);
    Dec(i);
    if i < 0 then i:= 0
    else if i = 1 then i:= 0;
  except
    i:= 0;
  end; {try}
  AutoRewindEdit.Text:= IntToStr(i);
end;

procedure TFormPrefs.SpinButton1UpClick(Sender: TObject);
var
  i: integer;
begin
  try
    i:= StrToInt(AutoRewindEdit.Text);
    Inc(i);
    if i < 0 then i:= 0
    else if i = 1 then i:= 2;
  except
    i:= 0;
  end; {try}
  AutoRewindEdit.Text:= IntToStr(i);
end;

procedure TFormPrefs.AutoRewindEditExit(Sender: TObject);
begin
  try
    if StrToInt(AutoRewindEdit.Text) = 1 then AutoRewindEdit.Text:= '2';
    except {ignore}
  end; {try}
end;

procedure TFormPrefs.cbMaxFitChange(Sender: TObject);
var
  i: integer;
begin
  try
    i:= StrToInt(cbMaxFit.Text);
    if (i > MaxZoom) or (i < 0) then i:= 0;
    except i:= 0;
  end; {try}
  if (i = 0) then cbMaxFit.Font.Color:= clRed
  else cbMaxFit.Font.Color:= clWindowText;
  if i = 1 then MaxFitLabel.Caption:= 'pixel'
  else MaxFitLabel.Caption:= 'pixels';
end;

procedure TFormPrefs.cbMaxFitDropDown(Sender: TObject);
var
  i: integer;
begin
  if cbMaxFit.Items.Count > 0 then begin
    cbMaxFit.Items.Clear;
  end;
  for i:= 1 to MaxZoom do cbMaxFit.Items.Add(IntToStr(i));
end;

procedure TFormPrefs.cbMaxFitKeyPress(Sender: TObject; var Key: Char);
begin
  if cbMaxFit.Items.Count > 0 then begin
    cbMaxFit.Items.Clear;
    cbMaxFit.SelectAll;
  end;
  if not CharInSet(key,[#0..#31,'0'..'9']) then key:= #0;
end;

procedure TFormPrefs.UDMaxFitClick(Sender: TObject; Button: TUDBtnType);
begin
  cbMaxFitChange(Self);
end;

procedure TFormPrefs.EditPictureWidthChange(Sender: TObject);
var
  i: integer;
begin
  try
    i:= ProcentToInt(EditPictureWidth.Text);
    if (i < UDPictureWidth.Min) or (i > UDPictureWidth.Max) then
      raise Exception.Create('Must be between 10 and 80');
    EditPictureWidth.Font.Color:= clWindowText;
    except EditPictureWidth.Font.Color:= clRed;
  end; {try}
end;

procedure TFormPrefs.RemoveRuleButtonClick(Sender: TObject);
var
  ItemDead: TListItem;
begin
  ItemDead:= RuleListView.Selected;
  if (ItemDead <> nil) then begin
    RuleList.Delete(RuleLIst.IndexOf(ItemDead.Data));
    TRuleItem(ItemDead.Data).Free;
    ItemDead.Delete;
    InfoMemo.Clear;
    ShortInfoEdit.Text:= '';
  end;
  RemoveRuleButton.Enabled:= false;
  RuleListView.SetFocus;
end;

procedure TFormPrefs.InfoMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (RuleListView.Selected <> nil) then ChangeRuleButton.Enabled:= true;
end;

procedure TFormPrefs.ChangeRuleButtonClick(Sender: TObject);
begin
  InfoMemo.SelectAll;
  with RuleListView do begin
    TRuleItem(Selected.Data).LongInfo:= InfoMemo.SelText;
    TRuleItem(Selected.Data).ShortInfo:= ShortInfoEdit.Text;
  end;
  InfoMemo.SelLength:= 0;
  ChangeRuleButton.Enabled:= false;
  RuleListView.SetFocus;
end;

procedure TFormPrefs.AddRuleButtonClick(Sender: TObject);
var
  NewRuleItem: TRuleItem;
  NewListItem: TListItem;
begin
  InfoMemo.SelectAll;
  NewRuleItem:= TRuleItem.Create(RuleEdit.Text,'',ShortInfoEdit.Text,
                                 InfoMemo.SelText);
  RuleList.Add(NewRuleItem);
  NewListItem:= RuleListView.Items.Add;
  NewListItem.Caption:= NewRuleItem.Rule;
  NewListItem.SubItems.Add(NewRuleItem.Name);
  NewListItem.Data:= NewRuleItem;
  InfoMemo.SelLength:= 0;
  AddRuleButton.Enabled:= false;
end;

procedure TFormPrefs.EditLimitBottomKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not CharInSet(key,['-','0'..'9',#0..#31]) then key:= #0;
end;

procedure TFormPrefs.SpinButton2DownClick(Sender: TObject);
var
  i: integer;
begin
  with Sender as TSpinButton do try
    i:= StrToInt(TEdit(FocusControl).Text);
    Dec(i);
    if i < MinX then i:= MinX;
    TEdit(FocusControl).Text:= IntToStr(i);
    except {ignore error}
  end;
end;

procedure TFormPrefs.SpinButton2UpClick(Sender: TObject);
var
  i: integer;
begin
  with Sender as TSpinButton do try
    i:= StrToInt(TEdit(FocusControl).Text);
    Inc(i);
    if i > MaxX then i:= MaxX;
    TEdit(FocusControl).Text:= IntToStr(i);
    except {ignore error}
  end;
end;

procedure TFormPrefs.TorusColorGridChange(Sender: TObject);
var
  NewColor: TColor;
begin
  NewColor:= TorusColorGrid.ForeGroundColor;
  if (NewColor <> BackShape.Brush.Color) then
    TorusShape.Brush.Color:= NewColor;
  TorusColorGrid.ForegroundEnabled:= false;
  TorusColorGrid.BackgroundEnabled:= false;
end;

procedure TFormPrefs.CBNeighborhoodChange(Sender: TObject);
var
  Prefix: string;
  RulePrefix: string;
  //Rule: string;
  NewNeighborhood: integer;
  MaxN: integer;
begin
  NewNeighborhood:= nbDefault;
  if CBNeighborhood.Tag = tFree then try
    CBNeighborhood.Tag:= tBusy;
    Prefix:= CBNeighborhood.Text;
    if Pos(':',Prefix) = 0 then Prefix:= Prefix + ':';
    Prefix:= GetPrefix(Prefix);
    NewNeighborhood:= StrToNeighborhood(Prefix);
    if NewNeighborhood <> nbMoore then begin
      RulePageControl.ActivePage:= RuleSheet;
      NTSSheet.TabVisible:= false;
      NTBSheet.TabVisible:= false;
      LabelNonTot.Visible:= true;
      ImageBendArrow.Visible:= true;
    end
    else begin
      NTSSheet.TabVisible:= true;
      NTBSheet.TabVisible:= true;
      LabelNonTot.Visible:= false;
      ImageBendArrow.Visible:= false;
    end;
    cbN0.Checked:= Bool(NewNeighborhood and bit0);
    cbN1.Checked:= Bool(NewNeighborhood and bit1);
    cbN2.Checked:= Bool(NewNeighborhood and bit2);
    cbN3.Checked:= Bool(NewNeighborhood and bit3);
    cbN4.Checked:= Bool(NewNeighborhood and bit4);
    cbN5.Checked:= Bool(NewNeighborhood and bit5);
    cbN6.Checked:= Bool(NewNeighborhood and bit6);
    cbN7.Checked:= Bool(NewNeighborhood and bit7);
    cbN8.Checked:= Bool(NewNeighborhood and bit8);
    //apply new prefix to rulestring as well.
    RulePrefix:= GetPrefix(RuleEdit.Text);
    if RulePrefix <> Prefix then begin
      RuleEdit.Text:= InsertPrefix(RuleEdit.Text,Prefix);
    end;
  finally CBNeighborhood.Tag:= tFree;
  end; {if try}
  MaxN:= MaxNeighborsInNeighborhood(NewNeighborhood);
  SS8.Visible:= MaxN > 7; BB8.Visible:= SS8.Visible;
  SS7.Visible:= MaxN > 6; BB7.Visible:= SS7.Visible;
  SS6.Visible:= MaxN > 5; BB6.Visible:= SS6.Visible;
  SS5.Visible:= MaxN > 4; BB5.Visible:= SS5.Visible;
  SS4.Visible:= MaxN > 3; BB4.Visible:= SS4.Visible;
  SS3.Visible:= MaxN > 2; BB3.Visible:= SS3.Visible;
  SS2.Visible:= MaxN > 1; BB2.Visible:= SS2.Visible;
  SS1.Visible:= MaxN > 0; BB1.Visible:= SS1.Visible;
end;

procedure TFormPrefs.UpdateRuleChange;
var
  i,j: integer;
  b: boolean;
  Button: TNewExplButton;
begin
  CBNeighborhood.Text:= NeighborhoodtoStr(LifeRules.Neighborhood);
  NonTot:= LifeRules.GetNonTot;
  for i:= 1 to 8 do Birth[i].Checked:= NonTot[true,i,0];
  for i:= 0 to 8 do Same[i].Checked:= NonTot[false,i,0];
  for b:= false to true do for i:= 1 to 7 do for j:= 1 to 13 do begin
    Button:= NonTotCB[b,i,j];
    if Assigned(Button) then begin
      if Button.down <> NonTot[b,i,j] then begin
        Button.down:= NonTot[b,i,j];
        Button.Tag:= 1; //Don't update the edit box.
        Button.OnClick(Button);
        Button.Tag:= 0; //update the edit box again.
      end;
    end; {if}
  end; {for}
end;

procedure TFormPrefs.RuleEditChange(Sender: TObject);
var
  FoundItem: TListItem;
  N: string;
begin
  try
    RuleEdit.Font.Color:= clBlack;
    //NewRules:= RuleEdit.Text;
    LifeRules.Rulestring:= RuleEdit.Text;
    if not(LifeRules.IsValidRule(RuleEdit.Text)) then begin
      RuleEdit.Font.Color:= clRed;
    end
    else begin
      UpdateRuleChange;
      //try and see if the entered rule is not already in the list.
      FoundItem:= RuleListView.FindCaption(0,RuleEdit.Text,false,true,false);
      AddRuleButton.Enabled:= (FoundItem = nil);
      if (FoundItem <> nil) then begin
        FoundItem.Selected:= true;
        with RuleListView.Selected do begin
          InfoMemo.Lines[0]:= TRuleItem(Data).LongInfo;
          ShortInfoEdit.Text:= TRuleItem(Data).ShortInfo;
        end; {with}
      end; {if}
      RemoveRuleButton.Enabled:= (FoundItem <> nil);
      if RuleEdit.Text = '' then AddRuleButton.Enabled:= false;
      N:= GetPrefix(RuleEdit.Text);
      if N <> '' then begin
        N:= NeighborHoodToStrLong(StrToNeighborhood(N));
        if N <> CBNeighborhood.Text then begin
          CBNeighborhood.Text:= N;
          CBNeighborhoodChange(Self);
          //now make sure prefix Nieghborhood and ruleedit are the same.
          if GetPrefix(CBNeighborhood.Text) <> GetPrefix(RuleEdit.Text) then
            RuleEdit.text:= InsertPrefix(RuleEdit.Text,GetPrefix(CBNeighborhood.text));
        end; {if}
      end; {if}
    end; {else}
    except begin
      RuleEdit.Font.Color:= clRed;
      AddRuleButton.Enabled:= false;
    end; {except}
  end; {try}
end;

procedure TFormPrefs.RuleEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  RuleListView.Selected:= nil;
end;

procedure TFormPrefs.RuleEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Upcase(key),ValidRules) then key:= #0;
end;

procedure TFormPrefs.Bc1Click(Sender: TObject);
var
  Down: boolean;
  Name: string;
  Button: TNewExplButton;
  GRect: TRect;
  OldCopyMode: Longint;
  NewBitmap: TBitmap;
begin
  Button:= Sender as TNewExplButton;
  //invert button image.
  with Button.Glyph do begin
    OldCopyMode:= Canvas.CopyMode;
    Canvas.CopyMode:= cmNotSrcCopy;
    NewBitmap:= TBitmap.Create;
    NewBitmap.Assign(Button.Glyph);
    GRect:= Rect(0,0,Width,Height);
    //due to copymode a simple inversion will be done.
    Canvas.CopyRect(GRect,NewBitmap.Canvas,GRect);
    NewBitmap.Free;
    Canvas.CopyMode:= OldCopyMode;
  end;
  //Name should look like 'ABx'
  //a = S for same or B for Birth
  //B = letter
  //x = Digit between 1 and 7
  Down:= Button.Down;
  Name:= Button.Name;
  if Length(Name) <> 3 then ShowMessage('internal error in Bc1Click');
  if (Button.Tag = 0) then RuleEdit.Text:= LifeRules.SetNTState(Name,Down);
end;

procedure TFormPrefs.cbN8MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  S: string;
begin
  if Sender is TCheckbox then begin
    i:= 0;
    if cbN0.Checked then i:= i or bit0;
    if cbN1.Checked then i:= i or bit1;
    if cbN2.Checked then i:= i or bit2;
    if cbN3.Checked then i:= i or bit3;
    if cbN4.Checked then i:= i or bit4;
    if cbN5.Checked then i:= i or bit5;
    if cbN6.Checked then i:= i or bit6;
    if cbN7.Checked then i:= i or bit7;
    if cbN8.Checked then i:= i or bit8;
    S:= NeighborhoodToStrLong(i);
    CBNeighborhood.Text:= S;
    CBNeighborhoodChange(Self);
  end;
end;

procedure TFormPrefs.MemTrackChange(Sender: TObject);
var
  TempStr: string;
begin
  with MemTrack do begin
    MemPerBlock:= MemsArray[Position];
    TempStr:= '%d KB per allocation unit';
    FmtStr(TempStr,TempStr,[MemPerBlock div 1024]);
    LabelMem.Caption:= TempStr;
  end; {with}
end;

procedure TFormPrefs.ExtListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  ExtString: string;
  Part: string;
  i: integer;
begin
  if ExtListView.Tag = tBusy then exit;
  ExtString:= '';
  i:= ExtListView.Items.Count;
  while i > 0 do begin
    Dec(i);
    if ExtListView.Items[i].Checked then begin;
      Part:= ExtListView.Items[i].Caption;
      //Remove the leading '.'
      Part:= Copy(Part,2,Length(Part)-1) + ',';
      ExtString:= ExtString + Part;
    end; {if}
  end; {while}
  //Remove the trailing ','
  ExtString:= Copy(ExtString,1,Length(ExtString)-1);
  //if Assigned(FA) then begin
  //  FA.Exts.Clear;
  //  FA.Exts.CommaText:= ExtString;
  //end;
end;

procedure TFormPrefs.ButtonAddFileExtClick(Sender: TObject);
var
  Ext: string;
  AListItem: TListItem;
begin
  Ext:= Lowercase(Trim(FileExtEdit.Text));
  Ext:= ExtractFileExt('.'+Ext);
  if not Assigned(ExtListView.FindCaption(0,Ext,false,true,true)) then begin
    AListItem:= ExtListView.Items.Add;
    AListItem.Caption:= Ext;
    AListItem.Checked:= true;
  end;
end;

procedure TFormPrefs.cbSnapshotTimeOutClick(Sender: TObject);
begin
  EditSnapshotTimeOut.Enabled := cbSnapshotTimeOut.Checked;
  UDSnapshotTimeOut.Enabled := cbSnapshotTimeOut.Checked;
end;

procedure TFormPrefs.Image18Click(Sender: TObject);
begin
  CBDDraw.Checked:= not(CBDDraw.Checked);
end;

procedure TFormPrefs.MemTrackEnter(Sender: TObject);
begin
  Memo2.Visible:= true;
  Shape1.Visible:= true;
end;

initialization
{$ifdef doStyles}
  TStyleManager.Engine.RegisterStyleHook(TCustomTabControl, TTabBackgroundStyleHook);
{$endif}
end.
