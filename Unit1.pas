unit Unit1;

{define Time}

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,
  LifeGen, LifeCel, StdCtrls, ExtCtrls, LifeBox, {LifeCOM,}
  LifeLoad, Menus, Buttons,Clipbrd, ActiveX, {ComObj, ComServ,} ShellAPI,
  Prefs, {$ifdef time} TickTime, {$endif}
  ComCtrls, NewExplButton, LifeUtil,
  ExtDlgs, ThdTim, Mask, Gauges, LifeConst, LifeSaveDialog,
  ScrollHint, Snapshot, FileCtrl, Spin, ToolWin, NewTabSet, DropSource,
  DropTarget, DdeMan, {Shell_DDE,} ImgList
  ,VCL.Direct2D{, ImageList};


type
  TCelCountThread = class(TThread)
  private
    FCelCount: cardinal;
    FNewCelCount: cardinal;
    FUniverse: TUniverse;
    FTerminated: boolean;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure SetTerminated(Value: boolean);
  public
    constructor Create(AUniverse: TUniverse; OnReady: TNotifyEvent);
    property CelCount: cardinal read FCelCount;
    property Terminated: boolean read FTerminated write SetTerminated;
    property Universe: TUniverse read FUniverse;
  end;

  TUpdateSelStatThread = class(TThread)
  private
    FRect: TRect;
    FLifeBox: TLifeBox;
    FTerminated: boolean;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure SetTerminated(Value: boolean);
  public
    constructor Create(ALifeBox: TLifeBox; OnReady: TNotifyEvent);
    property Rect: TRect read FRect;
    property Terminated: boolean read FTerminated write SetTerminated;
    property LifeBox: TLifeBox read FLifeBox;
  end;



  TLife32HintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  end;

  TLife32MainForm = class(TForm)
    Buttonbar: TPanel;
    HelpBar: TPanel;
    PanelTop: TPanel;
    PanelLeft: TPanel;
    MainMenu1: TMainMenu;
    Game1: TMenuItem;
    Screen1: TMenuItem;
    Help1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    Zoomin1: TMenuItem;
    ZoomOut1: TMenuItem;
    Info1: TMenuItem;
    Helpindex1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N8: TMenuItem;
    Grid1: TMenuItem;
    Rules1: TMenuItem;
    N10: TMenuItem;
    ZoomPopup: TPopupMenu;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    N91: TMenuItem;
    N101: TMenuItem;
    MeasureButton: TButton;
    SlowerButton: TNewExplButton;
    ZoomButton: TNewExplButton;
    CursorDrawButton: TNewExplButton;
    GridButton: TNewExplButton;
    PlayButton: TNewExplButton;
    PauseButton: TNewExplButton;
    StepButton: TNewExplButton;
    FasterButton: TNewExplButton;
    RandomDots1: TMenuItem;
    SpeedButton: TNewExplButton;
    SpeedPopup: TPopupMenu;
    N01: TMenuItem;
    N1001: TMenuItem;
    N1501: TMenuItem;
    N2001: TMenuItem;
    N2501: TMenuItem;
    N3001: TMenuItem;
    N3501: TMenuItem;
    N4001: TMenuItem;
    N4501: TMenuItem;
    N5001: TMenuItem;
    N5501: TMenuItem;
    N6001: TMenuItem;
    StepbackButton: TNewExplButton;
    N12: TMenuItem;
    N22: TMenuItem;
    N32: TMenuItem;
    N42: TMenuItem;
    N1: TMenuItem;
    Animation1: TMenuItem;
    StepBack1: TMenuItem;
    Play1: TMenuItem;
    Pause1: TMenuItem;
    Stepforward1: TMenuItem;
    N13: TMenuItem;
    SkipTo1: TMenuItem;
    PlayTimer: TThreadedTimer;
    N301: TMenuItem;
    N401: TMenuItem;
    N751: TMenuItem;
    N15: TMenuItem;
    N20001: TMenuItem;
    N50001: TMenuItem;
    Saveas1: TMenuItem;
    SpeedSettings1: TMenuItem;
    Faster1: TMenuItem;
    Slower1: TMenuItem;
    Bevel5: TBevel;
    SkipToButton: TNewExplButton;
    GenTimer: TThreadedTimer;
    View1: TMenuItem;
    Toolbar1: TMenuItem;
    StatusLine1: TMenuItem;
    Scrollbars1: TMenuItem;
    MoveToButton: TNewExplButton;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel10: TBevel;
    N14: TMenuItem;
    ShowAll1: TMenuItem;
    HideAll1: TMenuItem;
    ViewPopup: TPopupMenu;
    Toolbar2: TMenuItem;
    StatusLine2: TMenuItem;
    Scrollbars2: TMenuItem;
    N16: TMenuItem;
    ShowAll2: TMenuItem;
    HideAll2: TMenuItem;
    StatusBar: TPanel;
    XEdit: TEdit;
    Label5: TLabel;
    YEdit: TEdit;
    Label7: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Shortcutkeys1: TMenuItem;
    N17: TMenuItem;
    Restoreallsettingstodefault1: TMenuItem;
    Menu1: TMenuItem;
    Menu2: TMenuItem;
    MakeSnapshot1: TMenuItem;
    RewindToSnapshot1: TMenuItem;
    N18: TMenuItem;
    SaveDialog1: TLifeSaveDialog;
    OpenDialog1: TLifeOpenDialog;
    N19: TMenuItem;
    MoveTo1: TMenuItem;
    Bevel11: TBevel;
    Label11: TLabel;
    Bevel12: TBevel;
    SpeedLabel: TEdit;
    ZoomLabel: TEdit;
    Bevel13: TBevel;
    Bevel14: TBevel;
    N20: TMenuItem;
    MRU1: TMenuItem;
    MRU2: TMenuItem;
    MRU3: TMenuItem;
    MRU4: TMenuItem;
    MRU5: TMenuItem;
    MRU6: TMenuItem;
    MRU7: TMenuItem;
    MRU8: TMenuItem;
    MRU9: TMenuItem;
    MRU10: TMenuItem;
    FlipPopup: TPopupMenu;
    MirrorX: TMenuItem;
    MirrorY: TMenuItem;
    R90: TMenuItem;
    R180: TMenuItem;
    R270: TMenuItem;
    PasteOrButton: TNewExplButton;
    HintTimer: TTimer;
    GenCelPanel: TPanel;
    Bevel1: TBevel;
    GenEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    CelLabel: TLabel;
    Bevel2: TBevel;
    ProgressBar1: TGauge;
    HelpLabel: TStaticText;
    PasteModePopup: TPopupMenu;
    Opaque1: TMenuItem;
    Transparent1: TMenuItem;
    Xor1: TMenuItem;
    Dontoverwrite1: TMenuItem;
    CursorSelectButton: TNewExplButton;
    MRUPopup: TPopupMenu;
    PMRU1: TMenuItem;
    PMRU2: TMenuItem;
    PMRU3: TMenuItem;
    PMRU4: TMenuItem;
    PMRU5: TMenuItem;
    PMRU6: TMenuItem;
    PMRU7: TMenuItem;
    PMRU8: TMenuItem;
    PMRU9: TMenuItem;
    PMRU10: TMenuItem;
    New1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Clear1: TMenuItem;
    N23: TMenuItem;
    SelectAll1: TMenuItem;
    N24: TMenuItem;
    MirrorHorz1: TMenuItem;
    MirrorVert1: TMenuItem;
    Rotate901: TMenuItem;
    Rotate1801: TMenuItem;
    Rotate2701: TMenuItem;
    N25: TMenuItem;
    FillRandom1: TMenuItem;
    N26: TMenuItem;
    Insertshape1: TMenuItem;
    Saveshape1: TMenuItem;
    Fillblack1: TMenuItem;
    Invert1: TMenuItem;
    CancelSkipButton: TNewExplButton;
    Image1: TImage;
    Image2: TImage;
    CursorModePopup: TPopupMenu;
    DrawMode1: TMenuItem;
    SelectMode1: TMenuItem;
    HandMode1: TMenuItem;
    CursorHandButton: TNewExplButton;
    N27: TMenuItem;
    Moveto2: TMenuItem;
    RewindButton: TNewExplButton;
    SnapshotButton: TNewExplButton;
    RewindImageList: TImageList;
    RewindPopup: TPopupMenu;
    Rewindto1: TMenuItem;
    Delete1: TMenuItem;
    Speed1: TMenuItem;
    Patternproperties1: TMenuItem;
    Colors1: TMenuItem;
    PastePutButton: TNewExplButton;
    PasteXorButton: TNewExplButton;
    PasteErrorButton: TNewExplButton;
    RuleEdit: TEdit;
    RulePopup: TPopupMenu;
    N28: TMenuItem;
    N2341: TMenuItem;
    N1234531: TMenuItem;
    N125361: TMenuItem;
    N13583571: TMenuItem;
    N2331: TMenuItem;
    N23361: TMenuItem;
    N23567836781: TMenuItem;
    N2356783781: TMenuItem;
    N2383571: TMenuItem;
    N2453681: TMenuItem;
    N34341: TMenuItem;
    N3467836781: TMenuItem;
    N4567831: TMenuItem;
    N5678356781: TMenuItem;
    N135713571: TMenuItem;
    FasterShadowButton: TNewExplButton;
    SlowerShadowButton: TNewExplButton;
    SettingsPopup: TPopupMenu;
    GeneralSettings2: TMenuItem;
    Speed2: TMenuItem;
    Rules2: TMenuItem;
    Colors2: TMenuItem;
    Patternproperties2: TMenuItem;
    Cancel2: TMenuItem;
    N52: TMenuItem;
    Zoomtofit1: TMenuItem;
    Zoomtofit2: TMenuItem;
    N1321: TMenuItem;
    Misc1: TMenuItem;
    Bevel9: TBevel;
    CursorDrawImage: TImage;
    CursorHandImage: TImage;
    CursorSelectImage: TImage;
    CursorZoomImage: TImage;
    Bevel16: TBevel;
    PasteOrImage: TImage;
    PasteXorImage: TImage;
    PastePutImage: TImage;
    PasteErrorImage: TImage;
    Options1: TMenuItem;
    Drawmode2: TMenuItem;
    Selectmode2: TMenuItem;
    HandMode2: TMenuItem;
    N30: TMenuItem;
    Transparent2: TMenuItem;
    Opaque2: TMenuItem;
    Xor2: TMenuItem;
    DontOverwrite2: TMenuItem;
    Display1: TMenuItem;
    Opennextfile1: TMenuItem;
    Opennextfile2: TMenuItem;
    N33: TMenuItem;
    N1641: TMenuItem;
    N11281: TMenuItem;
    N34: TMenuItem;
    RunBenchmark1: TMenuItem;
    RedrawButton: TNewExplButton;
    Redraw1: TMenuItem;
    N12561: TMenuItem;
    LeftPanel: TPanel;
    LeftSplitter: TSplitter;
    SidePageControl: TPageControl;
    TSSnapshots: TTabSheet;
    TSOpen: TTabSheet;
    TSTorus: TTabSheet;
    RewindList1: TSnapshotList;
    Panel1: TPanel;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    Splitter2: TSplitter;
    FileListBox1: TFileListBox;
    ClearAll2: TMenuItem;
    Box2: TMenuItem;
    Zoomtoselection1: TMenuItem;
    TSMoveTo: TTabSheet;
    TSPattern: TTabSheet;
    TSInfo: TTabSheet;
    TabPopup: TPopupMenu;
    Newsheet1: TMenuItem;
    Delete2: TMenuItem;
    N35: TMenuItem;
    Rename1: TMenuItem;
    TorusPanel: TPanel;
    InfoMemoPopup: TPopupMenu;
    InfoMenuFont: TMenuItem;
    FontDialog1: TFontDialog;
    InfoMenuCopy: TMenuItem;
    InfoMenuCut: TMenuItem;
    InfoMenuPaste: TMenuItem;
    N36: TMenuItem;
    ListView1: TListView;
    N37: TMenuItem;
    Lexicon1: TMenuItem;
    OpenDialog2: TOpenDialog;
    InfoPageControl: TPageControl;
    TSInfoMemo: TTabSheet;
    TSLexicon: TTabSheet;
    InfoMemo: TRichEdit;
    TSStat: TTabSheet;
    LexiconMemo: TRichEdit;
    Panel8: TPanel;
    CBLexiconFind: TComboBox;
    TabImageList: TImageList;
    Panel9: TPanel;
    PatternListView: TListView;
    LifeBoxPanel: TPanel;
    LifeBox1: TLifeBox;
    PanelRight: TPanel;
    ScrollBarUD: TScrollBar;
    PatternHotkey: THotKey;
    PatternNameEdit: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    PatternImages: TImageList;
    SmallPatternImages: TImageList;
    PatternPopup: TPopupMenu;
    LargeImages1: TMenuItem;
    Delete3: TMenuItem;
    DropLife32Source1: TDropLife32Source;
    DropLife32Target1: TDropLife32Target;
    Panel10: TPanel;
    SidePanelTabs: TNewTabSet;
    Panel11: TPanel;
    Hotkeys1: TMenuItem;
    Rename2: TMenuItem;
    Arrangeicons1: TMenuItem;
    N39: TMenuItem;
    SaveToDisk1: TMenuItem;
    Open2: TMenuItem;
    Bevel6: TBevel;
    Label17: TLabel;
    Label20: TLabel;
    OpenDialogPattern: TOpenDialog;
    SaveDialogPattern: TSaveDialog;
    ZoomMode1: TMenuItem;
    CursorZoomButton: TNewExplButton;
    Zoommode2: TMenuItem;
    DropLife32Target2: TDropLife32Target;
    SidePanel1: TMenuItem;
    Copy2: TMenuItem;
    Cut2: TMenuItem;
    Paste2: TMenuItem;
    N43: TMenuItem;
    enterdummy1: TMenuItem;
    DropLife32Target3: TDropLife32Target;
    Bevel17: TBevel;
    DirectXImage: TImage;
    Panel15: TPanel;
    //SCInterface: TShellCommandInterface;
    Panel16: TPanel;
    UpDown1: TUpDown;
    PanelBottom: TPanel;
    Splitter1: TSplitter;
    PanelBottomScrollbar: TPanel;
    ScrollBarLR: TScrollBar;
    Panel12: TPanel;
    Image10: TImage;
    HideSideButton: TNewExplButton;
    ShowSideButton: TNewExplButton;
    UniverseTabsetPanel: TPanel;
    UniverseTabset: TNewTabSet;
    UniverseTabRenamer: TEdit;
    FirstTabButton: TNewExplButton;
    PrevTabButton: TNewExplButton;
    NextTabButton: TNewExplButton;
    LastTabButton: TNewExplButton;
    LR: TScrollBar;
    UD: TScrollBar;
    CBIsLimited: TCheckBox;
    Description: TLabel;
    HideSidePanelButton: TNewExplButton;
    Panel3: TPanel;
    CloseSideButton: TNewExplButton;
    Panel4: TPanel;
    Image11: TImage;
    SnapshotTimer: TThreadedTimer;
    LoadSavesettings1: TMenuItem;
    LifeBoxPopup: TPopupMenu;
    Cancel1: TMenuItem;
    N4: TMenuItem;
    ItemCut: TMenuItem;
    ItemCopy: TMenuItem;
    ItemPaste: TMenuItem;
    ItemClear: TMenuItem;
    Clearoutsideselection1: TMenuItem;
    N9: TMenuItem;
    ItemSelectAll: TMenuItem;
    N6: TMenuItem;
    ItemMirrorHorz: TMenuItem;
    ItemMirrorVert: TMenuItem;
    ItemRotate270: TMenuItem;
    ItemRotate180: TMenuItem;
    ItemRotate90: TMenuItem;
    N7: TMenuItem;
    ItemBox1: TMenuItem;
    ItemFillRandom: TMenuItem;
    ItemFillBlack: TMenuItem;
    ItemInvert: TMenuItem;
    N5: TMenuItem;
    ItemTorus1: TMenuItem;
    ItemInsertShape: TMenuItem;
    ItemSaveShape: TMenuItem;
    N40: TMenuItem;
    ZoomtoFit3: TMenuItem;
    Bevel18: TBevel;
    Label1: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    TopEdit: TEdit;
    BottomEdit: TEdit;
    LeftEdit: TEdit;
    RightEdit: TEdit;
    SBTop: TSpinButton;
    SBRight: TSpinButton;
    SBBottom: TSpinButton;
    SBLeft: TSpinButton;
    RBSnapSmall: TRadioButton;
    RBSnapLarge: TRadioButton;
    RBNoSnap: TRadioButton;
    OKButton: TButton;
    Bevel20: TBevel;
    DeadEdgesUniverseButton: TNewExplButton;
    CircularUniverseButton: TNewExplButton;
    LimitNESWButton: TNewExplButton;
    LimitEWButton: TNewExplButton;
    LimitNSButton: TNewExplButton;
    Bevel22: TBevel;
    Image8: TImage;
    Label9: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    GotoPanel: TPanel;
    NW: TSpeedButton;
    N: TSpeedButton;
    NE: TSpeedButton;
    W: TSpeedButton;
    CenterButton: TSpeedButton;
    E: TSpeedButton;
    SW: TSpeedButton;
    S: TSpeedButton;
    SE: TSpeedButton;
    Label10: TLabel;
    Label12: TLabel;
    NullButton: TSpeedButton;
    Bevel24: TBevel;
    Bevel25: TBevel;
    YLabel: TLabel;
    XLabel: TLabel;
    Label24: TLabel;
    EditX: TEdit;
    EditY: TEdit;
    Panel2: TPanel;
    FilterComboBox1: TFilterComboBox;
    Bevel26: TBevel;
    Bevel27: TBevel;
    Label15: TLabel;
    Label16: TLabel;
    CelCountLabel: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    WidthLabel: TLabel;
    GenCountLabel: TLabel;
    HeightLabel: TLabel;
    Label21: TLabel;
    AreaLabel: TLabel;
    Label27: TLabel;
    Bevel28: TBevel;
    Bevel29: TBevel;
    Label25: TLabel;
    Label26: TLabel;
    SelWidthLabel: TLabel;
    SelHeightLabel: TLabel;
    Label30: TLabel;
    SelAreaLabel: TLabel;
    Label28: TLabel;
    RandomButton: TNewExplButton;
    InfoButton: TNewExplButton;
    RotateButton: TNewExplButton;
    SettingsPopupButton: TNewExplButton;
    MRUPopupButton: TNewExplButton;
    NewButton: TNewExplButton;
    OpenButton: TNewExplButton;
    SaveButton: TNewExplButton;
    SettingsButton: TNewExplButton;
    Image9: TImage;
    SkipToEdit: TEdit;
    SpinButton1: TSpinButton;
    cbShow: TCheckBox;
    Bevel30: TBevel;
    Label29: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    SBClearPatterns: TNewExplButton;
    SBOpenPatterns: TNewExplButton;
    SBSavePatterns: TNewExplButton;
    Bevel31: TBevel;
    Clearoutsideselection2: TMenuItem;
    Torus1: TMenuItem;
    LoadSave1: TMenuItem;
    N29: TMenuItem;
    N38: TMenuItem;
    N44: TMenuItem;
    ToolbarImages: TImageList;
    procedure PlayTimerTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MeasureButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ScrollBarUDScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBarUDChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Zoomin1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure LifeBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RandomDotsClick(Sender: TObject);
    procedure ItemCopyClick(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure ItemFillRandomClick(Sender: TObject);
    procedure ClearAll1Click(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure FasterButtonClick(Sender: TObject);
    procedure SlowerButtonClick(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure StepButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure ItemCutClick(Sender: TObject);
    procedure ItemPasteClick(Sender: TObject);
    procedure ItemClearClick(Sender: TObject);
    procedure ItemInsertShapeClick(Sender: TObject);
    procedure ItemSaveShapeClick(Sender: TObject);
    procedure LifeBox1Exit(Sender: TObject);
    procedure ItemMirrorHorzClick(Sender: TObject);
    procedure ItemMirrorVertClick(Sender: TObject);
    procedure ItemRotate90Click(Sender: TObject);
    procedure ItemRotate180Click(Sender: TObject);
    procedure ItemRotate270Click(Sender: TObject);
    procedure UpdateButtons;
    procedure UpdateTabButtons(NewTab: integer);
    procedure N01Click(Sender: TObject);
    procedure SlowerButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedPopupPopup(Sender: TObject);
    procedure StepbackButtonClick(Sender: TObject);
    procedure LifeBoxPopupPopup(Sender: TObject);
    procedure ScrollBarLRKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ScrollBarUDKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MeasureButtonKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Preferences1Click(Sender: TObject);
    procedure FlipXButtonClick(Sender: TObject);
    procedure FlipYButtonClick(Sender: TObject);
    procedure R90ButtonClick(Sender: TObject);
    procedure R180ButtonClick(Sender: TObject);
    procedure R270ButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SkipToButtonClick(Sender: TObject);
    procedure GenTimerTimer(Sender: TObject);
    procedure Game1Click(Sender: TObject);
    procedure Toolbar1Click(Sender: TObject);
    procedure StatusLine1Click(Sender: TObject);
    procedure Scrollbars1Click(Sender: TObject);
    procedure MoveToButtonClick(Sender: TObject);
    procedure Cancel1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ShowAll1Click(Sender: TObject);
    procedure HideAll1Click(Sender: TObject);
    procedure ButtonbarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Menu1Click(Sender: TObject);
    procedure MakeSnapshot1Click(Sender: TObject);
    procedure RewindToSnapshot1Click(Sender: TObject);
    procedure MoveTo1Click(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure MRU1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Restoreallsettingstodefault1Click(Sender: TObject);
    procedure HintTimerTimer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Transparent1Click(Sender: TObject);
    procedure OpenButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MRUPopupPopup(Sender: TObject);
    procedure XEditKeyPress(Sender: TObject; var Key: Char);
    procedure XEditChange(Sender: TObject);
    procedure GenEditChange(Sender: TObject);
    procedure GenEditKeyPress(Sender: TObject; var Key: Char);
    procedure GenEditClick(Sender: TObject);
    procedure RuleEditChange(Sender: TObject);
    procedure RuleEditKeyPress(Sender: TObject; var Key: Char);
    procedure SpeedLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LifeBox1MustPause(Sender: TObject);
    procedure ItemSelectAllClick(Sender: TObject);
    procedure ItemFillBlackClick(Sender: TObject);
    procedure ItemInvertClick(Sender: TObject);
    procedure CancelSkipButtonClick(Sender: TObject);
    procedure DrawMode1Click(Sender: TObject);
    procedure LifeBox1EditorModeChange(Sender: TObject);
    procedure LifeBox1ZoomChange(Sender: TObject);
    procedure Helpindex1Click(Sender: TObject);
    procedure Shortcutkeys1Click(Sender: TObject);
    procedure RewindButtonClick(Sender: TObject);
    procedure SnapshotButtonClick(Sender: TObject);
    procedure RewindButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RewindList1Click(Sender: TObject);
    procedure LifeBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RewindList1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure RewindList1Deletion(Sender: TObject; Item: TListItem);
    procedure Delete1Click(Sender: TObject);
    procedure Rewindto1Click(Sender: TObject);
    procedure LifeBox1PasteModeChange(Sender: TObject);
    procedure N28Click(Sender: TObject);
    procedure FasterShadowButtonClick(Sender: TObject);
    procedure XEditDblClick(Sender: TObject);
    procedure GenEditDblClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure RuleEditDblClick(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure SettingsButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Cancel2Click(Sender: TObject);
    procedure RewindPopupPopup(Sender: TObject);
    procedure LifeBox1DropFiles(Sender: TObject; var Msg: TWMDropFiles);
    procedure Zoomtofit1Click(Sender: TObject);
    procedure LifeBox1Enter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LifeBox1ChangePattern(Sender: TObject; Change: Integer);
    procedure RewindList1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure LifeBox1RuleChange(Sender: TUniverse);
    procedure LifeBox1ShowDrag(Sender: TObject; DragOffset: TPoint);
    procedure OpenDialog1OpenToClipboard(Sender: TObject);
    procedure ItemBox1Click(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Opennextfile1Click(Sender: TObject);
    procedure LifeBox1SaveProgress(Sender: TObject; Progress: Integer;
      var Cancel: boolean);
    procedure RunBenchmark1Click(Sender: TObject);
    procedure RedrawButtonClick(Sender: TObject);
    procedure Clearoutsideselection1Click(Sender: TObject);
    procedure LeftSplitterMoved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure SidePageControlChange(Sender: TObject);
    procedure ClearAll2Click(Sender: TObject);
    procedure CursorZoomImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PasteErrorImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelBottomResize(Sender: TObject);
    procedure Zoomtoselection1Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure UniverseTabsetDrawTab(Sender: TObject; TabCanvas: TCanvas;
      R: TRect; Index: Integer; Selected: Boolean);
    procedure UniverseTabsetMeasureTab(Sender: TObject; Index: Integer;
      var TabWidth: Integer);
    procedure UniverseTabsetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure Newsheet1Click(Sender: TObject);
    procedure UniverseTabsetMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UniverseTabRenamerKeyPress(Sender: TObject; var Key: Char);
    procedure UniverseTabRenamerExit(Sender: TObject);
    procedure UniverseTabRenamerChange(Sender: TObject);
    procedure UniverseTabsetDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure UniverseTabsetDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure UniverseTabsetStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure TSTorusEnter(Sender: TObject);
    procedure TopEditChange(Sender: TObject);
    procedure SBTopDownClick(Sender: TObject);
    procedure SBTopUpClick(Sender: TObject);
    procedure TopEditKeyPress(Sender: TObject; var Key: Char);
    procedure OKButtonClick(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Image7Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure CBIsLimitedClick(Sender: TObject);
    procedure NWMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure InfoMemoChange(Sender: TObject);
    procedure LifeBox1Click(Sender: TObject);
    procedure LifeBox1AfterInfoChange(Sender: TObject);
    procedure InfoMenuFontClick(Sender: TObject);
    procedure InfoMemoPopupPopup(Sender: TObject);
    procedure InfoMenuCutClick(Sender: TObject);
    procedure InfoMenuCopyClick(Sender: TObject);
    procedure InfoMenuPasteClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1Click(Sender: TObject);
    procedure DirectoryListBox1Click(Sender: TObject);
    procedure NWClick(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure LifeBox1SelectionChange(Sender: TObject);
    procedure RBSnapSmallClick(Sender: TObject);
    procedure CircularUniverseButtonClick(Sender: TObject);
    procedure Lexicon1Click(Sender: TObject);
    procedure Panel8Resize(Sender: TObject);
    procedure CBLexiconFindClick(Sender: TObject);
    procedure TabSet1MeasureTab(Sender: TObject; Index: Integer;
      var TabWidth: Integer);
    procedure SidePanelTabsMeasureTab(Sender: TObject; Index: Integer;
      var TabWidth: Integer);
    procedure SidePanelTabsDrawTab(Sender: TObject; TabCanvas: TCanvas;
      R: TRect; Index: Integer; Selected: Boolean);
    procedure SidePanelTabsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure DropTextTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      Point: TPoint; var Effect: Integer);
    procedure DropTextTarget1DragOver(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropTextTarget1GetDropEffect(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure LargeImages1Click(Sender: TObject);
    procedure Delete3Click(Sender: TObject);
    procedure PatternListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PatternHotkeyExit(Sender: TObject);
    procedure PatternNameEditChange(Sender: TObject);
    procedure SkipToEditChange(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure SkipToEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SkipToEditKeyPress(Sender: TObject; var Key: Char);
    procedure cbShowClick(Sender: TObject);
    procedure PatternListViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CloseSideButtonClick(Sender: TObject);
    procedure Rename2Click(Sender: TObject);
    procedure Arrangeicons1Click(Sender: TObject);
    procedure SaveToDisk1Click(Sender: TObject);
    procedure Open2Click(Sender: TObject);
    procedure Delete2Click(Sender: TObject);
    procedure LifeBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure InfoButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowSideButtonClick(Sender: TObject);
    procedure HideSideButtonClick(Sender: TObject);
    procedure PatternListViewInsert(Sender: TObject; Item: TListItem);
    procedure ZoomtoFit3Click(Sender: TObject);
    procedure PatternPopupPopup(Sender: TObject);
    procedure InfoMemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabPopupPopup(Sender: TObject);
    procedure UniverseTabsetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DropLife32Target2DragOver(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure DropLife32Target2GetDropEffect(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure UniverseTabsetMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SidePanel1Click(Sender: TObject);
    procedure Cut2Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure Paste2Click(Sender: TObject);
    procedure DropLife32Target3DragOver(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure InfoPageControlChange(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure SCInterfaceCmdOpenFile(FileName, Option: String);
    procedure FirstTabButtonClick(Sender: TObject);
    procedure LastTabButtonClick(Sender: TObject);
    procedure PrevTabButtonClick(Sender: TObject);
    procedure NextTabButtonClick(Sender: TObject);
    procedure UDScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure DropLife32Target2Drop(Sender: TObject;
      ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
    procedure HideSidePanelButtonClick(Sender: TObject);
    procedure ItemTorus1Click(Sender: TObject);
    procedure LeftPanelResize(Sender: TObject);
    procedure CBIsLimitedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SnapshotTimerTimer(Sender: TObject);
    procedure Clearoutsideselection2Click(Sender: TObject);
    procedure Torus1Click(Sender: TObject);
    procedure LifeBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Lexicon: TStringList;
    CelCounter: TCelCountThread;
    SelCounter: TUpdateSelStatThread;
    IsCounting: boolean;

    FNotEdited: boolean;
    FIsPaused: boolean;
    FSkipToMode: boolean;
    FZoomToFit: boolean;
    FMaxZoomToFit: integer;
    FPlaySpeed: integer;
    FSkipToShowGens: boolean;
    SteppingBack: boolean;
    FOnlyUseDefaultToolTipColors: boolean;
    FScrollHint: TScrollHint;
    FSmallScroll: integer;
    FSnapshotEvery: integer;
    FHideScrollbars: boolean;
    FHideToolbar: boolean;
    FHideStatusbar: boolean;
    GensBeforeSnapshot: integer;
    FReshowSkip: boolean;
    FHidePanelOnPlay: boolean;
    FShowSidepanelOnStartup: boolean;
    ReshowSidePanelOnPause: TTabSheet; //use only in SetIsPaused!
    OldOnMessage: TMessageEvent;
    SkipToGen: integer;
    SkipToRedraw: boolean;
    Suspended: boolean;
    CenterPattern: boolean;
    LastSettingsPage: integer;
    RewindListList: TList;
    RewindList: TSnapshotList;
    Animating: boolean;
    FSettingsAreLoaded: boolean;

    //TorusData **************
    TorusUpdateOff: boolean;
    //End TorusData***********
    //MoveTo data ************
    EditedByUser: boolean;
    //MoveTo_X,MoveTo_Y: integer;
    MoveTo_BoundingRect: TRect;
    //End MoveTo Data ********

    procedure WMDisplayChange(var Msg: TWMDisplayChange); message WM_DisplayChange;
    procedure GetFormSettings;
    procedure SaveFormSettings;
    procedure CloseDialogs;
    //procedure ClearSnapshots;

    procedure ShowRelGens(RelGens: integer);
    procedure UpdateMRUMenu;
    procedure UpdateCelCount(Sender: TObject);
    procedure BlueStatistics;
    procedure UpdateStatistics;
    procedure UpdateSelStatistics(Sender: TObject);
    procedure GetCelCount(OnReady: TNotifyEvent);
    procedure GetSelectionStats(OnReady: TNotifyEvent);
    procedure StopCounting;
    procedure RunBenchmark(NumGens: integer;
                           ShowProgress, CheckESC, ShowDisplay: boolean);

    procedure ShowSidePanel(APage: TTabSheet; PanelWidth: integer);
    procedure HideSidePanel;
    procedure LetUserChangeTabName(ATabset: TNewTabset);
    procedure FillCBLexiconFind;
    function FindKeywordInLexicon(AKeyword: string): string;
    function NewRewindList: TSnapshotList;

    procedure AddToPatternList(AUniverse: TUniverse; Key: Char; Caption: string);
    procedure ArrangePatternList;
    procedure SavePattern(FileName: string);
    procedure OpenPattern(Filename: string);
    function GetMyDocumentsPath: string;
    property SettingsAreLoaded: Boolean read FSettingsAreLoaded;

  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SysCommand;
    procedure WMMenuSelect(var Msg: TWMMenuSelect); message WM_MenuSelect;
    function MakeSnapshot(Reason: Integer): boolean;
    procedure SetMemPerBlock(value: integer);
    function GetMemPerBlock: integer;
    procedure SetSnapshotEvery(value: integer);
    procedure SetPlaySpeed(value: integer);
    procedure SetCelZoom(value: integer);
    function GetCelZoom: integer;
    procedure SetIsPaused(Value: boolean);
    procedure SetSkipToMode(Value: boolean);
    procedure SetSmallScroll(Value: integer);
    procedure SetHideScrollbars(value: boolean);
    procedure SetHideToolbar(value: boolean);
    procedure SetHideStatusbar(value: boolean);
    procedure SetGeneration(value: integer);
    function GetGeneration: integer;
    procedure ShowLongHint(Sender: TObject);
    procedure MyAppShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: Controls.THintInfo);
    procedure MyAppMsgHook(var Msg: TMsg; var Handled: boolean);
    procedure HideScrollHint;
    function GetMinGen: integer;
    property SkipToMode: boolean read FSkipToMode write SetSkipToMode;
    property NotEdited: boolean read FNotEdited write FNotEdited;
  public
		RandomDot: boolean;
    NoSnapshotTimeOut: boolean;
    OldException: TExceptionEvent;
    OldIsPaused: boolean;
    LastOpenFile: string;
    Extensions: string;
    OpenedDialog: TForm;
    LastOffset: integer;
    AutoRewindWhat: array[cpFirst..cpLast] of boolean;
    procedure WMTimer(var Msg: TWMTimer); message WM_Timer;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    function Ask(Message: string): string;
    procedure Suspend(Sender: TObject);
    procedure Resume(Sender: TObject);
    procedure OpenFile(AFilename: string);
    procedure SaveFile(AFilename: string; FileFormat: integer; IncludeTorusData: boolean);
    procedure SetStartDir(AFilename: string);
    function GetPlaySpeeds: TSpeedsArray;
		procedure Animate;
    procedure MoveTo(x1,y1: integer);

    //file extensions
    procedure RegisterFileExtensions;

    //Update UI functions
    procedure UpdatePosition(x,y: integer);
    procedure UpdateTorus;
    property MinGen: integer read GetMinGen;
    property IsPaused: boolean read FIsPaused write SetIsPaused;
    property MyLifeBox: TLifeBox read LifeBox1;
    property MemPerBlock: integer read GetMemPerBlock write SetMemPerBlock;
    property PlaySpeed: integer read FPlaySpeed write SetPlaySpeed;
    property CelZoom: integer read GetCelZoom write SetCelZoom;
    property SmallScroll: integer read FSmallScroll write SetSmallScroll;
    property ZoomToFit: boolean read FZoomToFit write FZoomToFit;
    property MaxZoomToFit: integer read FMaxZoomToFit write FMaxZoomToFit;
    property ReshowSkip: boolean read FReshowSkip write FReshowSkip;
    property HidePanelOnPlay: boolean read FHidePanelOnPlay write FHidePanelOnPlay;
    property ShowSidepanelOnStartup: boolean read FShowSidepanelOnStartup write FShowSidepanelOnStartup;
    property SnapshotEvery: integer read FSnapshotEvery write SetSnapshotEvery;
    property OnlyUseDefaultToolTipColors: boolean read FOnlyUseDefaultToolTipColors
      write FOnlyUseDefaultToolTipColors;
    property HideScrollbars: boolean read FHideScrollbars write SetHideScrollbars;
    property HideToolbar: boolean read FHideToolbar write SetHideToolbar;
    property HideStatusbar: boolean read FHideStatusbar write SetHideStatusbar;
    property Generation: integer read GetGeneration write SetGeneration;
    property ShowGenerations: boolean write FSkipToShowGens;
    //TorusData ************
    //TorusData ************
  end;

var
	Form1: TLife32MainForm;

{$ifdef time}
var
  MyTimer: TTickTimer;
  CumDisplay: Comp;
  CumGeneration: Comp;
{$endif}

//var
  //LifeApp: TLifeApp;

implementation
{$R *.DFM}
{$R Cursors.res}

uses
  RuleConst,
  ProgCorn,
  BenchMark,
  Ask,
  FastObject,
  System.Types,
  System.UITypes,
  ShlObj;
  //VCL.Controls;


constructor TUpdateSelStatThread.Create(ALifeBox: TLifeBox; OnReady: TNotifyEvent);
begin
  inherited Create(true);
  FLifeBox:= ALifeBox;
  FRect.Left:= 0;
  FRect.Top:= 0;
  FRect.Bottom:= 0;
  FRect.Right:= 0;
  FreeOnTerminate:= true;
  OnTerminate:= OnReady;
  priority:= tpLower;
  Self.Start;
end;


procedure TUpdateSelStatThread.Execute;
begin
  repeat
    Sleep(10);
  until Form1.Animating = false;
  FRect:= FLifeBox.SelectionRect;
  if not(FLifeBox.Universe.IsSelEmpty(FRect)) then begin
    FLifeBox.Universe.ShrinkSelRect(FRect);
  end;
  if Terminated then FRect:= FLifeBox.SelectionRect;
  Synchronize(DoTerminate);
end;

procedure TUpdateSelStatThread.DoTerminate;
begin
  if Assigned(OnTerminate) then OnTerminate(Self);
  Form1.IsCounting:= false;
end;

procedure TUpdateSelStatThread.SetTerminated(Value: boolean);
begin
  FTerminated:= Value;
  FLifeBox.Universe.Terminated:= value;
end;

constructor TCelCountThread.Create(AUniverse: TUniverse; OnReady: TNotifyEvent);
begin
  inherited Create(true);
  FUniverse:= AUniverse;
  FreeOnTerminate:= true;
  OnTerminate:= OnReady;
  priority:= tpLower;
  Self.Start;
end;

procedure TCelCountThread.Execute;
begin
  FNewCelCount:= Cardinal(-1);
  repeat
    Sleep(10);
  until Form1.Animating = false;
  Sleep(300);
  FNewCelCount:= FUniverse.CountCelsNow;
  if Terminated then FCelCount:= Cardinal(-1)
  else FCelCount:= FNewCelCount;
  Synchronize(DoTerminate);
  //DoTerminate;
end;

procedure TCelCountThread.DoTerminate;
begin
  //if counting stopped then we don't want to know the count.
  if (integer(CelCount) <> -1) and Assigned(OnTerminate) then OnTerminate(Self);
  Form1.IsCounting:= false;
end;

procedure TCelCountThread.SetTerminated(Value: boolean);
begin
  FTerminated:= Value;
  FUniverse.Terminated:= value;
end;

procedure TLife32HintWindow.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  if Color = clInfoBk then Canvas.Font.Color := clInfoText
  else Canvas.Font.Color:= clBlack;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK);
end;


procedure TLife32MainForm.WMDisplayChange(var Msg: TWMDisplayChange);
var
  OldEnabled: boolean;
begin
  //Disable the play thread.
  OldEnabled:= PlayTimer.Enabled;
  PlayTimer.Enabled:= false;
  //Change the Displaysettings for the entire unit 'LifeBox'
  LifeBox.DisplayChange(Msg.BitsPerPixel, Msg.Width, Msg.Height);
  //Re-enable the play timer again (if it was enabled to begin with).
  PlayTimer.Enabled:= OldEnabled;
end;

procedure TLife32MainForm.GetFormSettings;
var
  RuleList: TRuleList;
  ws: TWindowState;
begin
  with OpenReg do try
    ws:= TWindowState(ReadInteger('Unit1.pas','WindowState',integer(WindowState)));
    if ws <> wsMinimized then begin
      Left:= ReadInteger('Unit1.pas','Left',Screen.Width);
      Top:= ReadInteger('Unit1.pas','Top',Screen.Height);
      Width:= ReadInteger('Unit1.pas','Width',Screen.Width div 2);
      Height:= ReadInteger('Unit1.pas','Height',Screen.Height div 2);
      Left:= Min(Left, Screen.Width - Width);
      Top:= Min(Top, Screen.Height - Height);
    end;
    WindowState:= ws;
    finally Free;
  end;    {try}
  RuleList:= TRuleList.Create;
  RuleList.SaveToMenu(RulePopup, N28Click);
  RuleList.Free;
end;

procedure TLife32MainForm.SaveFormSettings;
begin
  with OpenReg do try
    if WindowState <> wsMinimized then begin
      WriteInteger('Unit1.pas','Left',Left);
      WriteInteger('Unit1.pas','Top',Top);
      WriteInteger('Unit1.pas','Width',Width);
      WriteInteger('Unit1.pas','Height',Height);
    end;
    WriteInteger('Unit1.pas','WindowState',Integer(WindowState));
    finally Free;
  end; {try}
end;

procedure TLife32MainForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TLife32MainForm.WMSysCommand(var Msg: TWMSysCommand);
begin
  case (Msg.CmdType and $FFF0) of    {}
    SC_MINIMIZE:if not SkipToMode then PauseButtonClick(Self);
    SC_ScreenSave: begin
      LifeBox1.ScreenSaverActive:= true;
    end; {sc_ScreenSave}
  end; {case}
  inherited;
end;

procedure TLife32MainForm.WMTimer(var Msg: TWMTimer);
begin
  TThreadedTimer(Msg.TimerID).OnTimer(Self);
  inherited;
end;

procedure TLife32MainForm.WMMenuSelect(var Msg: TWMMenuSelect);
begin
  HideScrollHint;
  with Msg do begin
    //if (MenuFlag = $FFFF) and (Menu = 0) and (Suspended) then begin
    //  Resume(Self);
    //end;
  end; {with}
  inherited;
end;

procedure TLife32MainForm.MoveTo(x1,y1: integer);
begin
  with LifeBox1 do begin
    MoveBy(x1-XScroll,y1-YScroll);
    UpdatePosition(XScroll,YScroll);
  end; {with}
  Self.SetFocus;
end;

procedure TLife32MainForm.UpdatePosition(x,y: integer);
begin
  with LifeBox1 do begin
    XEdit.Text:= IntToStr(X);
    YEdit.Text:= IntToStr(Y);
    EditX.Text:= IntToStr(X);
    EditY.Text:= IntToStr(Y);
    ScrollBarUD.Hint:= '#Y:'+IntToStr(YScroll);
    ScrollBarLR.Hint:= '#X:'+IntToStr(XScroll);
    XLabel.Caption:= 'X: '+IntToStr(XScroll);
    YLabel.Caption:= 'Y: '+IntToStr(YScroll);
    MoveToButton.Hint:= MoveToHint + ': ('+IntToStr(XScroll)+','+
	                      IntToStr(YScroll)+')'+
	                      '|'+GetLongHint(MoveToButton.Hint);
  end;
end;

procedure TLife32MainForm.SetPlaySpeed(value: integer);
var
  i: integer;
begin
  if Value <> FPlaySpeed then begin
    if Value < 0 then Value:= 0
    else if Value >= MinSpeed then Value:= MinSpeed;
    FPlaySpeed:= value;
    PlayTimer.Interval:= FPlaySpeed;
    FasterButton.Enabled:= not(FPlaySpeed = MaxPlaySpeed);
    Faster1.Enabled:= not(FPlaySpeed = MaxPlaySpeed);

    SlowerButton.Enabled:= FPlaySpeed < MinSpeed;
    Slower1.Enabled:= FPlaySpeed < MinSpeed;
    SpeedButton.Down:= false;
    FasterButton.Hint:= 'Faster:'+IntToStr(FPlaySpeed)+
      '|'+GetLongHint(FasterButton.Hint);
    SlowerButton.Hint:= 'Slower:'+IntToStr(FPlaySpeed)+
      '|'+GetLongHint(SlowerButton.Hint);
    SpeedButton.Hint:= 'Speed:'+IntToStr(FPlaySpeed)+
      '|'+GetLongHint(SpeedButton.Hint);
    SpeedLabel.Text:= IntToStr(FPlaySpeed);
    for i:= 0 to SpeedPopup.Items.Count -1 do SpeedPopup.Items[i].Checked:= false;
    try
      i:= 0;
      while (StrToInt(SpeedPopup.Items[i].Caption) < Value)
        and (i < SpeedPopup.Items.Count-1) do Inc(i);
      SpeedPopup.Items[i].Checked:= true;
      except {do nothing} ;
    end; {try}
  end; {if}
end;

procedure TLife32MainForm.Animate;
const
  //OldGenCount: integer = 0;
  GoForward = true;
begin
  Animating:= true;
	if RandomDot then LifeBox1.RandomDot;
  if not(SkipToMode) then begin
    NotEdited:= true;
      {$ifdef time}
      MyTimer.StartNow;
      {$endif}
    LifeBox1.Generate(GoForward);
      {$ifdef time}
      MyTimer.StopNow;
      CumGeneration:= CumGeneration + MyTimer.TimePast.Totaal;
      {$endif}
    LifeBox1.UpdateAll;
    if AutoRewindWhat[cpAuto] then begin
      Dec(GensBeforeSnapshot); //this avoids expensive 'div' instruction
      if GensBeforeSnapshot = 0 then begin
        GensBeforeSnapshot:= SnapshotEvery;
        LifeBox1ChangePattern(Self,cpAuto);
      end; {if}
    end; {if}
  end {if}
  else begin  //we are in skiptomode
    if LifeBox1.Generation = SkipToGen then begin
      SkipToMode:= false;
    end {if}
    else begin
      NotEdited:= true;
      LifeBox1.Generate(GoForward);
      if FSkipToShowGens then LifeBox1.UpdateAll
      else begin
        LifeBox1.Universe.ClearDisplay; //update the displaylist.
        if SkipToRedraw then begin
          LifeBox1.Redraw;
        end;
        SkipToRedraw:= false;
      end; {else}
    end; {else}
  end; {else}
  Animating:= false;
end;

procedure TLife32MainForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if LifeBox1.EnableScreen then raise E;
end;

function TLife32MainForm.Ask(Message: string): string;
var
  AskForm: TAskForm;
begin
  Application.CreateForm(TAskForm, AskForm);
  AskForm.Question:= Message;
  if AskForm.ShowModal = mrOK then Result:= AskForm.Answer
  else Result:= '';
  AskForm.Free;
end;

procedure TLife32MainForm.PlayTimerTimer(Sender: TObject);
begin
  //To avoid multi-thread conflicts, i.e. drawing through a popup menu.
  //check if we still can go on.
	if not(IsPaused) and not(Animating) then Animate;
  Application.ProcessMessages;
end;

procedure TLife32MainForm.FormPaint(Sender: TObject);
//var
  //OldCP: TPoint;
begin
  //LifeBox1.RefreshColors;
	LifeBox1.Repaint;
  FormShow(Sender);
end;

function TLife32MainForm.NewRewindList: TSnapShotList;
begin
  Result:= TSnapShotList.Create(RewindList.Owner);
  with Result do begin
    Parent:= RewindList.Parent;
    Init; //init can only be done after the control has a parent!
    OnClick:= RewindList.OnClick;
    OnColumnClick:= RewindList.OnColumnClick;
    OnCompare:= RewindList.OnCompare;
    OnDeletion:= RewindList.OnDeletion;
    PopupMenu:= RewindList.PopupMenu;
    SmallImages:= RewindList.SmallImages;
  end; {with}
end;

procedure TLife32MainForm.FormCreate(Sender: TObject);
var
  ABitmap: TBitmap;
  AMask: TBitmap;
  i: integer;
var
  ARewindList: TSnapShotList;

begin
  HintWindowClass:= TLife32HintWindow;
  GetFormSettings;
  RewindListList:= TList.Create;
  RewindList:= RewindList1;
  RewindListList.Add(RewindList);
  //life32 starts out with 3 universes, so we should also have 3 rewindlists.
  //by default all properties are set correctly.
  for i:= 1 to 2 do begin
    ARewindList:= NewRewindList;
    RewindListList.Add(ARewindList);
  end;
  Application.OnShowHint:= MyAppShowHint;
  Application.OnHint:= ShowLongHint;
  IsPaused:= true;
  NotEdited:= true;
  OldIsPaused:= true;
  FPlaySpeed:= 7;
  FSmallScroll:= 1;
  FSkipToShowGens:= true;
  try
    PanelRight.Width:= Min(Max(GetSystemMetrics(SM_CXVSCROLL),10),40);
    PanelBottom.Height:= Min(Max(GetSystemMetrics(SM_CYHSCROLL),10),40);
  except
    PanelRight.Width:= 20;
    PanelBottom.Height:= 20;
  end; {try}
  ABitmap:= TBitmap.Create;
  AMask:= TBitmap.Create;
  try
    for i:= cpFirst to cpLast do begin
      ABitmap.LoadFromResourceID(HInstance,SnapPics[i]+20000);
      AMask.Assign(ABitmap);
      AMask.Mask(clWhite);
      RewindImageList.Add(ABitmap,AMask);
    end; {for i}
    for i:= cpFirst to cpLast do begin
      ABitmap.LoadFromResourceID(HInstance,SnapPics[i]+10000);
      AMask.Assign(ABitmap);
      AMask.Mask(clWhite);
      RewindImageList.Add(ABitmap,AMask);
    end; {for i}
  finally
    ABitmap.Free;
    AMask.Free;
  end; {try}

  Screen.Cursors[crDraw]:= LoadCursor(HInstance, StrCrDraw);
  Screen.Cursors[crColorDraw]:= LoadCursor(HInstance, StrCrColorDraw);
  Screen.Cursors[crMyArrow]:= LoadCursor(HInstance, StrCrMyArrow);
  Screen.Cursors[crCross]:= LoadCursor(HInstance, StrCrCross);
  Screen.Cursors[crColorCross]:= LoadCursor(HInstance, StrCrColorCross);
  Screen.Cursors[crTRANSPARENTARROW]:= LoadCursor(HInstance, StrCrTRANSPARENTARROW);
  Screen.Cursors[crZOOM]:= LoadCursor(HInstance,StrCrZOOM);
  Screen.Cursors[crCOLORZOOM]:= LoadCursor(HInstance,StrCrCOLORZOOM);
  Screen.Cursors[crHAND]:= LoadCursor(HInstance,StrCrHAND);
  Screen.Cursors[crCOLORHAND]:= LoadCursor(HInstance,StrCrCOLORHAND);
  Screen.Cursors[crHandGrab]:= LoadCursor(HInstance,StrCrHandGrab);
  DisableDDrawOnCrash; //Disable DirectDraw if Life32 crashed the last session.
  LifeBox1.Init;
  GetSettings;

  //AutoLoadStartFile; //replaced with ShellCommandInterface.
  RegisterFileExtensions;
  //if no commandline then open last opened file.
  //if not SCInterface.ProcessCommandLine then begin
  //  SCInterfaceCmdOpenFile('','');
  //end;
  Lexicon:= TStringList.Create;
  try
    Lexicon.LoadFromFile(ExtractFilePath(Application.ExeName)+'Lexicon.txt');
    FillCBLexiconFind;
    except {do nothing}
  end;
  DropLife32Target1.register(PatternListView);
  DropLife32Target2.register(UniverseTabset);
  DropLife32Target3.register(SidePanelTabs);
end;

procedure TLife32MainForm.MeasureButtonClick(Sender: TObject);
var
	i: integer;
	TimeBefore : Longint;
const
	q = 5206;
begin
  PauseButtonClick(Sender);
  //LifeBox1.Clear;
  //LifeBox1.ChangeCel(1,0,true);   //1234567
  //LifeBox1.ChangeCel(0,2,true);  //1 x
  //LifeBox1.ChangeCel(1,2,true);  //2   x
  //LifeBox1.ChangeCel(3,1,true);  //3xx  xxx
  //LifeBox1.ChangeCel(4,2,true);  //4
  //LifeBox1.ChangeCel(5,2,true);  //5
  //LifeBox1.ChangeCel(6,2,true);     {}

  GenEdit.Text:= 'Wait...';
  GenEdit.Refresh;

  {$ifdef time}
  CumDisplay:= 0;
  CumGeneration:= 0;
  LifeCel.CumDiff:= 0;
  LifeCel.CumDisp:= 0;
  {$endif}
  TimeBefore:= MyGetTickCount;
  for i:= 1 to q do Animate;
  {$ifdef time}
  LifeBox1.CanPaint:= cpDontPaint;
  ShowMessage('The displaytime is  : '+CompToStr(LifeCel.CumDisp /1000)+#10+#13+
              'The diff. calculation takes: '+CompToStr(LifeCel.CumDiff /1000)+#10+#13+
              'The generationtime is: '+CompToStr(CumGeneration /1000));
  LifeBox1.CanPaint:= cpCanPaint;
  {$endif}
  UpdateButtons;
  GenEdit.Text:= IntToStr(MyGetTickCount - TimeBefore);
  if ScrollBarUD.CanFocus then ScrollBarUD.SetFocus;
end;

procedure TLife32MainForm.FormActivate(Sender: TObject);
var
  //i: integer;
  SidePanelWidth: integer;
begin
  RewindList.Init; //Hack, inits the rewindlist, so it looks ok.
  RewindList.Visible:= true;

  //SnapshotButtonClick(Sender);
  if ScrollBarLR.CanFocus then ScrollBarLR.SetFocus;
  if not(SettingsAreLoaded) then begin
    GetSettings;
    FSettingsAreLoaded:= true;
  end;
  GenEdit.Text:= IntToStr(LifeBox1.Generation);
  GetCelCount(UpdateCelCount);
  RuleEdit.Text:= LifeBox1.Universe.RuleString;
  //Set the color OK the first time. (see RuleEdit's OnChange)
  RuleEdit.Font.Color:= clWindowText;
  //must be called after menu-bitmaps are set-up
  UpdateMRUMenu;
  CenterPattern:= true;
  //Resume(Sender);
  with LifeBox1 do UpdatePosition(XScroll,YScroll);
  UpdateTorus;
  //synchronize the pictured tabset with the underlying pagecontrol.
  if ShowSidepanelOnStartup then SidePanelWidth:= 0 else SidePanelWidth:= 1;
  if LifeBox1.IsLimited then ShowSidePanel(TSTorus,SidePanelWidth)
  else ShowSidePanel(TSOpen,SidePanelWidth);
  //SidePanelTabs.TabIndex:= SidePageControl.ActivePage.TabIndex;
end;

procedure TLife32MainForm.FormResize(Sender: TObject);
var
  CelPixels: extended; //convert negative values to fractions.
begin
  UD.Height:= GetSystemMetrics(SM_CYVSCROLL)*2;
  UD.Width:= ScrollbarUD.Width;
  UD.Top:= PanelRight.Height - UD.Height;

  LR.Width:= GetSystemMetrics(SM_CXHSCROLL)*2;
  LR.Height:= ScrollbarLR.Height;
  LR.Left:= PanelBottomScrollbar.Width - PanelRight.Width - LR.Width;


  PanelRight.Width:= ScrollbarUD.Width;
  ScrollBarUD.Top:= 0;
  ScrollbarUD.Height:= PanelRight.Height - (UD.Height div 2);

  PanelBottom.Height:= ScrollbarLR.Height;
  ScrollBarLR.Left:= 0;//PanelBottomScrollBar.Left;
  ScrollbarLR.Width:= (PanelBottomScrollbar.Width - PanelRight.Width - (LR.Width div 2));

  with LifeBox1 do try
    if PixelsPerCel > 0 then CelPixels:= PixelsPerCel
    else CelPixels:= 1 / (1 shl Abs(PixelsPerCel));
    //RefreshColors;
    //Paint;

    ScrollBarUD.SetParams(0,-Round(Height / (1 * CelPixels)),
                             Round(Height / (1 * CelPixels)));
    ScrollBarUD.LargeChange:= Round(Height / (3 * CelPixels));
    ScrollBarUD.SmallChange:= SmallScroll;

    ScrollBarLR.SetParams(0,-Round(Width / (1 * CelPixels)),
                             Round(Width / (1 * CelPixels)));
    ScrollBarLR.LargeChange:= Round(Width / (3 * CelPixels));
    ScrollBarLR.SmallChange:= SmallScroll;
    except {ignore}
  end; {with try}
  if CenterPattern then begin
    CenterPattern:= false;
    LifeBox1.CenterOnPattern(NoRedraw);
    if ZoomToFit then LifeBox1.ZoomToFit(NoRedraw, MaxZoomToFit);
    Resume(Sender);
  end {if}
  else LifeBox1.RedrawAll;
  if (LifeBox1.CanPaint = cpInit) then LifeBox1.CanPaint:= cpCanPaint;
end;

procedure TLife32MainForm.ScrollBarUDScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  //first OnScroll is executed, after that OnChange
  case ScrollCode of
    scTrack: TScrollBar(Sender).Tag:= -1;
    scPosition: begin
      TScrollBar(Sender).Tag:= 1;
    end;
    scEndScroll:begin
      TScrollBar(Sender).Tag:= 2;
      TScrollBar(Sender).Position:= 0;
    end;
  end; {case}
end;

procedure TLife32MainForm.ScrollBarUDChange(Sender: TObject);
const
  OldPosX: integer = 0;
  OldPosY: integer = 0;
var
  HintMsg: TWMMouse;
begin
  if Suspended then Resume(Sender);
  case  TScrollBar(Sender).Tag of
    -1:begin
      LifeBox1.Scrolling:= true;
      LifeBox1.MoveBy(ScrollBarLR.Position-OldPosX,ScrollBarUD.Position-OldPosY);
      OldPosX:= ScrollBarLR.Position;
      OldPosY:= ScrollBarUD.Position;
    end; {if}
    1:begin
      OldPosX:= 0;
      OldPosY:= 0;
    end; {1:}
    2:TScrollBar(Sender).Tag:= 0
    else begin
      LifeBox1.Scrolling:= false;
      LifeBox1.MoveBy(ScrollBarLR.Position-OldPosX,ScrollBarUD.Position-OldPosY);
      if not(LifeBox1.Grid) then LifeBox1.RedrawAll;
      OldPosX:= 0;
      OldPosY:= 0;
      TScrollBar(Sender).Position:= 0;
    end; {else}
  end; {case}
  HintMsg.Xpos:= 1;
  HintMsg.YPos:= 1;
  HintMsg.Msg:= WM_MouseMove;
  HintMsg.Keys:= 0;
  if Sender is TScrollBar then begin
    ScrollBarUD.Hint:= '#Y:'+IntToStr(LifeBox1.YScroll);
    UD.Hint:= '#Y:'+IntToStr(LifeBox1.YScroll);
    ScrollBarLR.Hint:= '#X:'+IntToStr(LifeBox1.XScroll);
    LR.Hint:= '#X:'+IntToStr(LifeBox1.XScroll);
    Application.HintMouseMessage(TControl(Sender),TMessage(HintMsg));
  end; {if}
end;

procedure TLife32MainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TLife32MainForm.Zoomin1Click(Sender: TObject);
begin
  try
    CelZoom:= CelZoom + 1;
    finally Resume(Sender);
  end;
end;

procedure TLife32MainForm.ZoomOut1Click(Sender: TObject);
begin
  try
	  if CelZoom = 1 then CelZoom:= -1
	  else CelZoom:= CelZoom -1;
    finally Resume(Sender);
  end; {try}
end;

procedure TLife32MainForm.LifeBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  LifeBox1.ClientToCel(x,y);
  UpdatePosition(x,y);
end;

procedure TLife32MainForm.RandomDotsClick(Sender: TObject);
begin
  try
	  RandomDot:= not RandomDot;
	  RandomDots1.Checked:= RandomDot;
	  RandomButton.Down:= RandomDot;
    finally Resume(Sender);
  end;
end;

procedure TLife32MainForm.ItemCopyClick(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
	  LifeBox1.CopySelection;
    finally begin
	    DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.Grid1Click(Sender: TObject);
begin
  try
	  LifeBox1.Grid:= not LifeBox1.Grid;
	  Grid1.Checked:= LifeBox1.Grid;
	  GridButton.Down:= LifeBox1.Grid;
    finally Resume(Sender);
  end;
end;

procedure TLife32MainForm.Info1Click(Sender: TObject);
var
  ProgCorner: TProgCorner;
begin
  PauseButtonClick(Sender);
  Suspend(Sender);
  LifeBox1.CanPaint:= cpDialogShowing;
  ProgCorner:= TProgCorner.Create(Self);
  OpenedDialog:= ProgCorner;
  ProgCorner.ShowModal;
  OpenedDialog:= nil;
  ProgCorner.Free;
  Resume(Sender);
end;

procedure TLife32MainForm.ItemFillRandomClick(Sender: TObject);
begin
  try
    WaitCursor;
    Suspend(Sender);
    LifeBox1.FillRandom;
    UpdateButtons;
    finally begin
      Resume(Sender);
      DefaultCursor;
    end; {finally}
  end;
end;

procedure TLife32MainForm.ClearAll1Click(Sender: TObject);
begin
  Resume(Sender);
  PauseButtonClick(Sender);
	LifeBox1.Clear(true,true,true);
  if (CelZoom < 1) then CelZoom:= 1;
  LastOpenFile:= ExtractFilePath(LastOpenFile)+'untitled.lif';
  Caption:= LifeTitle + '- ['+ExtractFileName(LastOpenFile)+']';
  InfoButton.Enabled:= false;
  SkipToMode:= false;
  UpdateButtons;
end;

procedure TLife32MainForm.SetCelZoom(value: integer);
begin
  LifeBox1.PixelsPerCel:= Value;
end;

function TLife32MainForm.GetCelZoom: integer;
begin
  Result:= LifeBox1.PixelsPerCel;
end;

procedure TLife32MainForm.SetIsPaused(value: boolean);
begin
  FIsPaused:= value;
  LifeBox1.IsPaused:= FIsPaused;
  repeat
    //wait until generation of the universe is done, before tickering with it.
  until not(LifeBox1.Universe.Locked);
  if (not(IsPaused) and HidePanelOnPlay and (LeftPanel.Width > 1)) then begin
    ReshowSidePanelOnPause:= SidePageControl.ActivePage;
    HideSidePanel;
  end
  else if IsPaused and HidePanelOnPlay and Assigned(ReshowSidePanelOnPause) then begin
    ShowSidePanel(ReshowSidePanelOnPause,0);
    ReshowSidePanelOnPause:= nil;
  end; {else}
end;

procedure TLife32MainForm.SetSkipToMode(value: boolean);
const
  OldSpeed: integer = 0;
  OldEditorMode: integer = 0;
begin
  if (value <> FSkipToMode) then begin
    FSkipToMode:= value;
    if FSkipToMode then begin
      OldEditorMode:= LifeBox1.EditorMode;
      SkipToButton.Visible:= false;
      CancelSkipButton.Visible:= true;
      if not SteppingBack then begin
        ProgressBar1.MinValue:= 0; //just to avoid exceptions.
        ProgressBar1.MaxValue:= SkipToGen;
        ProgressBar1.MinValue:= LifeBox1.Generation;
        ProgressBar1.Visible:= true;
      end; {if}
      if FSkipToShowGens then LifeBox1.Redraw;
      if not(FSkipToShowGens) then begin
	      OldSpeed:= PlaySpeed;
	      PlaySpeed:= 0;
        if (LifeBox1.EditorMode = emdraw) or (LifeBox1.Editormode = emSelect) then begin
          LifeBox1.EditorMode:= emHand;
        end;
        DrawMode1.Enabled:= false;
        DrawMode2.Enabled:= false;
        SelectMode1.Enabled:= false;
        SelectMode2.Enabled:= false;
      end; {if}
      PlayButtonClick(Self);
    end
    else begin //SkiptoMode = false
      if not(FSkipToShowGens) then PlaySpeed:= OldSpeed;
      CancelSkipButton.Visible:= false;
      SkipToButton.Visible:= true;
      PauseButtonClick(Self);
      Progressbar1.Visible:= false;
      GenCelPanel.Refresh;
      //Make sure generation count is displayed OK.
      GenEdit.Repaint;
      LifeBox1.DoPaint(Self);
      LifeBox1.EditorMode:= OldEditorMode;
      SelectMode1.Enabled:= true;
      SelectMode2.Enabled:= true;
      DrawMode1.Enabled:= true;
      DrawMode2.Enabled:= true;
      //Don't popup skip to for very small offsets.
      if (LastOffset > 1) or (LastOffset < -1) then begin
        if ReshowSkip then SkipToButtonClick(Self);
        ShowRelGens(LastOffset);
      end; {if}
    end; {if}
  end; {if}
end;

procedure TLife32MainForm.ShowRelGens(RelGens: integer);
var
  Succes: boolean;
  SkipText: string;
const
  Entered: boolean = false;
begin
  succes:= true;
  if Entered then exit;
  Entered:= true;
  repeat
    try
      if RelGens > 0 then SkipText:= '+'+IntToStr(RelGens)
      else SkipText:= IntToStr(RelGens);
      GenEdit.Text:= SkipText;
      SkipToEdit.Text:= SkipText;
      GenEdit.SelectAll;
      SkipToEdit.SelectAll;
      except Succes:= false;
    end;
  until Succes;
  entered:= false;
end;

procedure TLife32MainForm.SetSmallScroll(value: integer);
begin
  //if value <> FSmallScroll then begin
	  FSmallScroll:= value;
	  ScrollBarUD.SmallChange:= value;
	  ScrollBarLR.SmallChange:= value;
    LifeBox1.SmallScroll:= value;
  //end;
end;


procedure TLife32MainForm.SetHideScrollbars(value: boolean);
begin
  FHideScrollbars:= value;
  PanelRight.Visible:= not(value);
  PanelBottom.Visible:= not(value);
  ScrollBars1.Checked:= not(value);
  ScrollBars2.Checked:= not(value);
  FormResize(Self);
end;

procedure TLife32MainForm.SetHideStatusbar(value: boolean);
begin
  FHideStatusbar:= value;
  HelpBar.Visible:= not(value);
  StatusLine1.Checked:= not(value);
  StatusLine2.Checked:= not(value);
  FormResize(Self);
end;

procedure TLife32MainForm.SetHideToolbar(value: boolean);
begin
  FHideToolbar:= value;
  Buttonbar.Visible:= not(value);
  Toolbar1.Checked:= not(value);
  Toolbar2.Checked:= not(value);
  FormResize(Self);
end;

procedure TLife32MainForm.SetMemPerBlock(value: integer);
begin
  Prefs.SaveMemPerBlock(value);
end;

function TLife32MainForm.GetMemPerBlock: integer;
begin
  Result:= Prefs.GetMemPerBlock;
end;

procedure TLife32MainForm.SetSnapshotEvery(value: integer);
begin
  if value <> FSnapshotEvery then begin
    if value = 1 then value:= 2; //one step back is always possible.
    FSnapshotEvery:= value;
    GensBeforeSnapshot:= value;
  end; {if}
end;

procedure TLife32MainForm.UpdateCelCount(Sender: TObject);
var
  CelCount: integer;
begin
  if (Sender is TCelCountThread) then begin
    CelLabel.Enabled:= true;
    CelCount:= TCelCountThread(Sender).CelCount;
    CelLabel.Caption:= IntToStr(CelCount);
    CelCountLabel.Caption:= CelLabel.Caption;
    CelLabel.Font.Color:= clWindowText;
    CelCountLabel.Font.Color:= clWindowText;
    GenCelPanel.Refresh;
    CelLabel.Refresh;
    CelCountLabel.Refresh;
    if (OpenedDialog <> nil) and (OpenedDialog is TFormPrefs) then begin
      TFormPrefs(OpenedDialog).CelCountLabel.Caption:= CelLabel.Caption;
      CelCountLabel.Font.Color:= clBlack;
    end; {if}
    (**)
    //Double check to make sure it's really empty.
    if LifeBox1.IsEmpty then begin
      //Don't change context menu's here, or weird stuff will happen.
      SaveButton.Enabled:= false;
      Saveas1.Enabled:= false;
      SelectAll1.Enabled:= false;
      if not(IsPaused) then PauseButton.Click;
      PlayButton.Enabled:= false;
      StepButton.Enabled:= false;
      Play1.Enabled:= false;
      StepForward1.Enabled:= false;
    end; {if}
  end; {if}
  if not(Lifebox1.IsEmpty) then begin
    //Don't change context menu's here, or weird stuff will happen.
    SaveButton.Enabled:= true;
    Saveas1.Enabled:= true;
    SelectAll1.Enabled:= true;
    PlayButton.Enabled:= true;
    StepButton.Enabled:= true;
    Play1.Enabled:= true;
    StepForward1.Enabled:= true;
  end; {if}
end;

procedure TLife32MainForm.BlueStatistics;
begin
  CelCountLabel.Font.Color:= clBlue;
  CelLabel.Font.Color:= clBlue;
  GenCountLabel.Font.Color:= clBlue;
  WidthLabel.Font.Color:= clBlue;
  HeightLabel.Font.Color:= clBlue;
  AreaLabel.Font.Color:= clBlue;
end;

procedure TLife32MainForm.UpdateSelStatistics(Sender: TObject);
var
  TempFloat,a,b: extended;
  SmallSelRect: TRect;
begin
  if Assigned(Sender) and (Sender is TUpdateSelStatThread) then begin
    SmallSelRect:= TUpdateSelStatThread(Sender).Rect;
  end
  else SmallSelRect:= LifeBox1.SelectionRect;
  with SmallSelRect do begin
    A:= (Right-Left); B:= (Bottom-Top);
    TempFloat:= A * B;
    TempFloat:= ABS(TempFloat);
    if TempFloat >= 1.0 then begin
      SelAreaLabel.Caption:= Format('%.0f',[TempFloat]);
      SelAreaLabel.Hint:= Format('%.0n',[TempFloat]);
      SelWidthLabel.Caption:= Format('%d',[Right-Left]);
      SelHeightLabel.Caption:= Format('%d',[Bottom-Top]);
    end {if}
    else begin
      SelAreaLabel.Caption:= '0';
      SelAreaLabel.Hint:= '0';
      SelWidthLabel.Caption:= '0';
      SelHeightLabel.Caption:= '0';
    end; {else}
  end; {with}
end;

procedure TLife32MainForm.UpdateStatistics;
var
  TempFloat,a,b: extended;
begin
  //CelCountLabel.Caption:= CelLabel.Caption; UpdateCelCount will do this,
  //As seperate thread updates the celcount. If command is uncommented it will
  //always lag one count behind.
  GenCountLabel.Caption:= IntToStr(LifeBox1.Generation);
  with LifeBox1.Universe.GetBoundingBox do begin
    //NWLabel.Caption:= Format('(%d,%d)',[Left,Top]);
    //SELabel.Caption:= Format('(%d,%d)',[Right,Bottom]);
    WidthLabel.Caption:= Format('%d',[Right-Left]);
    HeightLabel.Caption:= Format('%d',[Bottom-top]);
    A:= (Right-Left); B:= (Bottom-Top);
    TempFloat:= A * B;
    TempFloat:= abs(TempFloat);
    AreaLabel.Caption:= Format('%.0f',[TempFloat]);
    AreaLabel.Hint:= Format('%.0n',[TempFloat]);
  end; {with}
  GetSelectionStats(UpdateSelStatistics);
  GenCountLabel.Font.Color:= clWindowText;
  WidthLabel.Font.Color:= clWindowText;
  HeightLabel.Font.Color:= clWindowText;
  AreaLabel.Font.Color:= clWindowText;
end;

procedure TLife32MainForm.GetCelCount(OnReady: TNotifyEvent);
begin
  if not(IsPaused) then PlayTimer.Enabled:= false;
  if IsCounting then begin
    StopCounting;
  end;
  IsCounting:= true;
  //CelCounter:= TCelCountThread.Create(LifeBox1.Universe,OnReady);
  CelCounter:= nil;
end;

procedure TLife32MainForm.GetSelectionStats(OnReady: TNotifyEvent);
begin
  //if selection is dead, do not start the process.
  if (LifeBox1.Universe.IsSelEmpty(LifeBox1.SelectionRect)) then begin
    OnReady(Self);
  end
  else begin
    if not(IsPaused) then PlayTimer.Enabled:= false;
    if IsCounting then begin
      StopCounting;
    end;
    IsCounting:= true;
    //SelCounter:= TUpdateSelStatThread.Create(LifeBox1,OnReady);
    SelCounter:= nil;
  end;
end;

procedure TLife32MainForm.StopCounting;
begin
  if IsCounting then begin
    try
      if Assigned(CelCounter) then begin
        CelCounter.Terminate;
        CelCounter.WaitFor;
      end;
    except {ignore}
    end;
    try
      if Assigned(SelCounter) then begin
        SelCounter.Terminate;
        SelCounter.WaitFor;
      end;
    except {ignore}
    end;
  end;
  IsCounting:= false;
end;

procedure TLife32MainForm.ShowSidePanel(APage: TTabSheet; PanelWidth: integer);
var
  NewWidth: integer;
  OldWidth: integer;
begin
  OldWidth:= LeftPanel.Width;
  if PanelWidth <= 0 then begin
    if LeftPanel.Width < 30 then LeftPanel.Width:= LeftPanel.Tag;
    if LeftPanel.Width < 40 then LeftPanel.Width:=
      Min(182,(LifeBox1.Width-100+LeftPanel.Width));
  end
  else LeftPanel.Width:= PanelWidth;
  NewWidth:= LeftPanel.Width;
  NewWidth:= NewWidth - OldWidth;
  if CelZoom < 0 then NewWidth:= NewWidth * (1 shl abs(CelZoom))
  else if CelZoom > 0 then NewWidth:= NewWidth div CelZoom;
  LifeBox1.MoveBy(NewWidth div 2,0);
  SidePageControl.ActivePage:= APage;
  SidePageControlChange(Self);
  ShowSideButton.Visible:= false;
  HideSideButton.Visible:= true;
  LeftSplitterMoved(LeftSplitter);
  Resume(Self);
end;

procedure TLife32MainForm.HideSidePanel;
var
  OldWidth: integer;
begin
  OldWidth:= LeftPanel.Width;
  LeftPanel.Tag:= OldWidth;
  if CelZoom < 0 then OldWidth:= OldWidth * (1 shl abs(CelZoom))
  else if CelZoom > 0 then OldWidth:= OldWidth div CelZoom;
  LifeBox1.MoveBy(-OldWidth div 2,0);
  LeftPanel.Width:= 1;
  HideSideButton.Visible:= false;
  ShowSideButton.Visible:= true;
  Resume(Self);
end;

procedure TLife32MainForm.Suspend(Sender: TObject);
begin
  if not(Suspended) then begin
    OldIsPaused:= IsPaused;
    suspended:= true;
  end; {if}
  if not(IsPaused) then PauseButtonClick(Sender);
  if (LifeBox1.CanPaint <> cpInit) then LifeBox1.Canpaint:= cpDontPaint;
end;

procedure TLife32MainForm.Resume(Sender: TObject);
begin
  if suspended then begin
    suspended:= false;
    if (Sender is TSpeedButton) then with TSpeedButton(Sender) do
      if GroupIndex = idDialogButton then Down:= false;
    if not(OldIsPaused) then PlayButtonClick(Sender);
  end; {if}
  if (LifeBox1.CanPaint <> cpInit) then LifeBox1.CanPaint:= cpCanPaint;
end;

procedure TLife32MainForm.N11Click(Sender: TObject);
begin
  try
    CelZoom:= TMenuItem(Sender).Tag;
    except {do nothing};
  end;
  Resume(Sender);
end;

var
  OldHint: string = '';

procedure TLife32MainForm.ShowLongHint(Sender: TObject);
var
  LongHint: string;
begin
  LongHint:= GetLongHint(Application.Hint);
  //do not reshow an old hint, so as not to disturb the disabling
  //by the timer.
  if OldHint <> LongHint then begin
    OldHint:= LongHint;
    if (LongHint <> '')
       and (Pos('@',Application.Hint)=0)
       and (Pos('#',Application.Hint)=0)
       and (Pos('&',Application.Hint)=0)
       and (Pos('^',Application.Hint)=0)
       and (Application.Hint[1] <> '!') then begin
      if LongHint[1] = '$' then begin
        Delete(LongHint,1,1);
        HelpLabel.Font.Color:= clRed;
        HelpLabel.Font.Style:= HelpLabel.Font.Style + [fsBold];
      end
      else begin
        HelpLabel.Font.Color:= clWindowText;
        HelpLabel.Font.Style:= HelpLabel.Font.Style - [fsBold];
      end;

      HelpLabel.Caption:= LongHint;
      //StatusBar.Visible:= false;
      //StatusBar.SendToBack;
      HelpLabel.BringToFront;
      //restart the timer if need be.
      HintTimer.Enabled:= not(HintTimer.enabled);
      HintTimer.Enabled:= true;
    end {if}
    else begin
      //StatusBar.Visible:= true;
      //StatusBar.BringToFront;
      HelpLabel.SendToBack;
      HintTimer.Enabled:= false;
    end; {else}
  end; {if}
end;

procedure TLife32MainForm.HintTimerTimer(Sender: TObject);
begin
  HintTimer.Enabled:= false;
  //StatusBar.Visible:= true;
  //StatusBar.BringToFront;
  HelpLabel.SendToBack;
  OldHint:= '';
end;


procedure TLife32MainForm.MyAppShowHint(var HintStr: string; var CanShow: boolean;
                        var HintInfo: Controls.THintInfo);
const
  OldHintControl: TControl = nil;
  OldHintPos: TPoint = ();
  //OldCursor: TCursor = $7FFF;
var
  APoint: TPoint;
  AnItem: integer;
  StartChar: Char;
begin
  with HintInfo do begin
    //if OldCursor <> $7FFF then begin
    //  HintControl.Cursor:= OldCursor;
    //  OldCursor:= $7FFF;
    //end;
    if HintControl = OldHintControl then begin
      HintPos:= OldHintPos;
      Application.HintPause:= 50;
    end {if}
    else begin
      if (Length(HintStr) > 0) and (HintStr[1] <> '#') then
        Dec(HintPos.Y,65);
      Application.HintPause:= 500;
    end; {else}
    StartChar:= ' ';
    if (Length(HintStr) > 0) then StartChar:= HintStr[1];
    case StartChar of
      '@':begin //min..max stats.
        Delete(HintStr,1,1);
        if not(OnlyUseDefaultToolTipColors) then HintColor:= $00ffffd0; //Light Blue
        if (Length(HintStr) > 0) and (HintStr[1] = '#') then begin
          Delete(HintStr,1,1);
          Inc(HintPos.Y,60);
          if (HintControl is TEdit) and not(TEdit(HintControl).ReadOnly) then
            Inc(HintPos.Y,33);
        end; {if}
      end; {'@'}
      '#': begin //scrollbar.
        Delete(HintStr,1,1);
        ReshowTimeout:= 5;
      end; {'#'}
      '&': begin //statbar
        Delete(HintStr,1,1);
        Inc(HintPos.Y,60);
        if (HintControl is TEdit) and not(TEdit(HintControl).ReadOnly) then
          Inc(HintPos.Y,33);
      end; {'&'}
      '!','?': begin
        if Length(HintStr) = 1 then begin
          if LifeBox1.Universe.Description.Count = 0 then HintStr:= 'No info'
          else HintStr:= LifeBox1.Universe.Description.Text;
        end
        else Delete(HintStr,1,1);
        HintMaxWidth:= Min(Self.Width, Screen.Width div 2);
        while (Length(HintStr) > 0) and
        CharInSet(HintStr[Length(HintStr)],[#1..#31]) do
        Delete(HintStr,Length(HintStr),1);
        HideTimeOut:= 100000;
        if StartChar = '!' then
          Inc(HintPos.X,32); //so the cursor does not obscure the text
          //OldCursor:= HintControl.Cursor;
          //HintControl.Cursor:= crNone;
      end; {'!'}
      '^': begin //tabs at top of sidepanel.
        if HintInfo.HintControl = SidePanelTabs then begin
          AnItem:= SidePanelTabs.ItemAtPos(HintInfo.CursorPos);
          if AnItem <> -1 then begin
            HintStr:= SidePageControl.Pages[AnItem].Caption;
          end {if}
          else HintStr:= 'Sidepanel';
        end; {if}
      end; {'^':}
    end; {if}
    if HintControl is TScrollBar then begin
      if HintControl = ScrollBarUD then begin
        APoint.y:= -30;
        APoint.x:= -60;
        APoint:= ScrollBarUD.ClientToScreen(APoint);
        HintPos.Y:= APoint.y;
        HintPos.X:= APoint.X;
        HintStr:= 'Y:'+intToStr(LifeBox1.YScroll);
      end {if}
      else if HintControl = ScrollBarLR then begin
        APoint.y:= ScrollBarLR.Height + 6;
        APoint:= ScrollBarLR.ClientToScreen(APoint);
        HintPos.y:= APoint.y;
        HintStr:= 'X:'+IntToStr(LifeBox1.XScroll);
      end; {else}

      //HintPos.Y:= TScrollBar(HintControl).DragCursor;
    end; {if}
    OldHintPos:= HintPos;
    OldHintControl:= HintControl;
  end; {with}
end;

procedure TLife32MainForm.MyAppMsgHook(var Msg: TMsg; var Handled: boolean);
var
  InfoRect: TRect;
  InfoPt: TPoint;
  MousePt: TPoint;
  Ignore: boolean;
begin
  if (Assigned(FScrollHint)) and (FScrollHint.IsHintMsg(Msg)) then begin
    Ignore:= false;
    case Msg.message of
      wm_KeyDown, wm_Keyup: if Msg.wParam = vk_I then Ignore:= true;
      wm_Char: if Upcase(Char(Msg.wParam)) = 'I' then Ignore:= true;
    end; {case}
    if not(Ignore) then begin
      InfoRect:= InfoButton.ClientRect;
      InfoPt:= InfoButton.ClientToScreen(InfoRect.TopLeft);
      OffsetRect(InfoRect,InfoPt.x,InfoPt.y);
      GetCursorPos(MousePt);
      if not(PtInRect(InfoRect,MousePt)) then begin
        HideScrollHint;
        Handled:= true;
      end; {if}
    end; {if}
  end; {if}
  if Assigned(OldOnMessage) then OldOnMessage(Msg,Handled);
end;


procedure TLife32MainForm.HideScrollHint;
begin
  FScrollHint.Free;
  FScrollHint:=nil;
  Application.OnMessage:= OldOnMessage;
  InfoButton.Down:= false;
end;


function TLife32MainForm.GetPlaySpeeds: TSpeedsArray;
var
  i: integer;
begin
  with SpeedPopup do begin
    for i:= 0 to Items.Count-1 do try
      Result[i]:= StrToInt(Items[i].Caption);
      except {ignore}
    end; {for i}
  end; {with}
end;


procedure TLife32MainForm.FasterButtonClick(Sender: TObject);
var
  i: integer;
begin
  if PlaySpeed > 0 then with SpeedPopup do try
    i:= Items.Count-1;
    while i > 0  do begin
      if Items[i].Checked then try
        PlaySpeed:= StrToInt(Items[i-1].Caption);
        i:= 0;
        except {do nothing};
      end; {try}
      Dec(i);
    end; {while}
    finally Resume(Sender);
  end; {if try}
end;

procedure TLife32MainForm.SlowerButtonClick(Sender: TObject);
var
  i: integer;
begin
  with SpeedPopup do try
    if (PlaySpeed < StrToInt(Items[Items.Count-1].Caption)) then begin
      i:= 0;
      while (i < Items.Count-1)  do begin
        if Items[i].Checked then try
          PlaySpeed:= StrToInt(Items[i+1].Caption);
          i:= MaxInt-1;
          except {do nothing};
        end; {try}
        Inc(i);
      end; {while}
    end; {if}
    except {do nothing};
  end; {try}
  Resume(Sender);
end;

procedure TLife32MainForm.PlayButtonClick(Sender: TObject);
begin
  if (Sender = PlayButton) or (LifeBox1.Generation = 0) then LifeBox1ChangePattern(Self,cpPlay);
  StopCounting;
  BlueStatistics;
  if LifeBox1.Universe.LimitChanged then LifeBox1.ClearOutsideTorus;
  Resume(Sender);
	PauseButton.Visible:= true;
  //Pause1.Enabled:= true;
	PlayButton.Visible:= false;
  //Play1.Enabled:= false;
  IsPaused:= false;
  LifeBox1.ExcludeSelection;
  LifeBox1.HideSelection;
  Animate;   //One step just to set the stepbackbutton up.
  StepBackButton.Enabled:= LifeBox1.Generation > 0;
  StepBack1.Enabled:= StepbackButton.Enabled;
  //steping one gen may already pause it, we don't want to go on anyway in that
  //case, so one more test.
  if not IsPaused then begin
    PlayTimer.Enabled:= true;
    GenTimer.Enabled:= true;
  end;
end;

procedure TLife32MainForm.PauseButtonClick(Sender: TObject);
var
  WasPaused: boolean;
begin
  WasPaused:= IsPaused;
  IsPaused:= true;
  //clear display and redraw to fix any possible display errors.
  if (not WasPaused) and ((Sender is TNewExplButton)) then LifeBox1.Redraw;
	PlayTimer.Enabled:= false;
  GenTimerTimer(Sender);
  GenTimer.Enabled:= false;
	PauseButton.Visible:= false;
  //Pause1.Enabled:= false;
	PlayButton.Visible:= true;
  LifeBox1.ShowSelection;
  LifeBox1.IncludeSelection;
  //Play1.Enabled:= true;
	StepButton.Enabled:= true;
  StepbackButton.Enabled:= LifeBox1.Generation > 0;
  Stepback1.Enabled:= StepbackButton.Enabled;
  UpdateButtons;
end;

procedure TLife32MainForm.StepButtonClick(Sender: TObject);
var
  OldHidePanelOnPlay: boolean;
begin
  OldHidePanelOnPlay:= HidePanelOnPlay;
  HidePanelOnPlay:= false;
  //IsPaused:= false;  //forces entire redraw, but causes flicker.
  Resume(Self);
  StopCounting;
  if LifeBox1.Generation = 0 then LifeBox1ChangePattern(Self,cpPlay)
  else LifeBox1ChangePattern(Self,cpStep);
  if LifeBox1.Universe.LimitChanged then LifeBox1.ClearOutsideTorus;
  LifeBox1.ExcludeSelection;
  LifeBox1.HideSelection;
	Animate;
  PauseButtonClick(Sender);
  GenEdit.Text:= IntToStr(LifeBox1.Generation);
  StepbackButton.Enabled:= LifeBox1.Generation > 0;
  Stepback1.Enabled:= LifeBox1.Universe.BackCorrect;
  LifeBox1.ShowSelection;
  HidePanelOnPlay:= OldHidePanelOnPlay;
end;

procedure TLife32MainForm.LifeBox1DropFiles(Sender: TObject; var Msg: TWMDropFiles);
var
  FileName: string;
  Buffer: array[0..255] of char;
  i: integer;
  //OldCursor: TCursor;
begin
  FillChar(Buffer,SizeOf(Buffer),#0);
  with Msg do begin
    i:= DragQueryFile(Drop,$FFFFFFFF,nil,0);
    if i > 0 then begin
      DragQueryFile(Drop,0,Buffer,SizeOf(Buffer));
      Filename:= Buffer;
      OpenFile(Filename);
      Resume(Sender);
    end; {if}
    DragFinish(Drop);
  end; {with}
end;



procedure TLife32MainForm.OpenFile(AFilename: string);
var
  OldCursor: TCursor;
begin
  if FileExists(AFilename) then begin
    Suspend(Self); //only suspend, do not resume, someone will do that.
    OldCursor:= Screen.Cursor;
    try
      try
        Screen.Cursor:= crHourGlass;
        LifeBox1.LoadFromFile(AFilename);
        LifeBox1.CenterOnPattern(NoRedraw);
        //RuleEdit.Text:= LifeBox1.Universe.RuleString;
        if ZoomToFit and Self.Visible then LifeBox1.ZoomToFit(NoRedraw, MaxZoomToFit);
        SetLastFile(AFileName);
        LastOpenFile:= AFilename;
        Caption:= LifeTitle + '- ['+Proper(ExtractFileName(AFileName))+']';
        //force caption redraw.
        UpdateMRUMenu;
        UpdateButtons;
        finally Screen.Cursor:= OldCursor;
      end; {try}
      except {ignore all errors}
        //ShowMessage('Error in TLife32MainForm.OpenFile');
    end; {try}
  end; {if}
end;

procedure TLife32MainForm.OpenButtonClick(Sender: TObject);
var
  OldRules: string;
begin
  try
    PauseButtonClick(Sender);
    Suspend(sender);
    LifeBox1.CanPaint:= cpDialogShowing;
    try
      OpenDialog1.InitialDir:= ExtractFileDir(GetLastFile);
      OpenDialog1.FileName:= ExtractFileName(GetLastFile);
      if OpenDialog1.execute then with OpenDialog1 do begin
        OldRules:= LifeBox1.Universe.Rulestring;
        OpenFile(Filename);
        DirectoryListBox1.Directory:= ExtractFileDir(Filename);
        if not(UseNewRules) then LifeBox1.Universe.RuleString:= OldRules;
        SkipToMode:= false;
      end; {if}
      finally Resume(sender);
    end; {try}
    except begin
      raise;
    end; {except}
  end; {try}
end;


procedure TLife32MainForm.SaveFile(AFilename: string; FileFormat: integer; IncludeTorusData: boolean);
var
  OldCursor: TCursor;
  SaveRect: TRect;
begin
  OldCursor:= Screen.Cursor;
  try
    Screen.Cursor:= crHourGlass;
    SaveRect:= LifeBox1.Universe.GetBoundingBox;
    ProgressBar1.MinValue:= 0; //just to avoid exceptions.
    ProgressBar1.MaxValue:= SaveRect.Bottom-SaveRect.Top;
    ProgressBar1.Visible:= true;
    LifeBox1.SaveToFile(AFileName, FileFormat, IncludeTorusData);
    ProgressBar1.Visible:= false;
    Caption:= LifeTitle + '- ['+ExtractFileName(AFileName)+']';
    SetLastFile(AFileName);
    LastOpenFile:= AFilename;
    UpdateMRUMenu;
    finally Screen.Cursor:= OldCursor;
  end;
end;

procedure TLife32MainForm.SetStartDir(AFilename: string);
var
  ADir: string;
begin
  if FileExists(AFileName) then begin
    ADir:= ExtractFileDir(AFileName);
    if ADir = '' then ADir:= GetCurrentDir;
    DirectoryListBox1.Directory:= ADir;
    DriveComboBox1.Drive:= DirectoryListBox1.Drive;
  end;
end;

procedure TLife32MainForm.SaveButtonClick(Sender: TObject);
var
  NewFilename: string;
begin
  PauseButtonClick(Sender);
  Suspend(Sender);
  LifeBox1.CanPaint:= cpDialogShowing;
  try
    SaveDialog1.InitialDir:= ExtractFileDir(GetLastFile);
    NewFileName:= ExtractFileName(LastOpenFile);
    NewFileName:=
        Copy(NewFilename,1,Length(NewFilename)-Length(ExtractFileExt(NewFileName)));
    //NewFileName:= NewFileName + cstrDefaultExt;
    SaveDialog1.FileName:= NewFileName;
    SaveDialog1.Comments.Assign(LifeBox1.Universe.Description);
    SaveDialog1.IncludeTorusInfo:= LifeBox1.IsLimited;
    if SaveDialog1.execute then with SaveDialog1 do begin
      LifeBox1.Universe.Description.Assign(SaveDialog1.Comments);
      Resume(Sender);
      SaveFile(Filename, SaveDialog1.FilterIndex, SaveDialog1.IncludeTorusInfo)
    end; {if}
    finally Resume(Sender);
  end; {try}
  UpdateButtons;
end;

procedure TLife32MainForm.SettingsButtonClick(Sender: TObject);
var
  FormSettings: TFormPrefs;
begin
  PauseButtonClick(Sender);
  Suspend(Sender);
  try
    try
      FormSettings:= TFormPrefs.Create(Self);
      LifeBox1.CanPaint:= cpDialogShowing;
      FormSettings.TabbedNotebook1.PageIndex:= LastSettingsPage;
      OpenedDialog:= FormSettings;
      FormSettings.ShowModal;
      OpenedDialog:= nil;
      LastSettingsPage:= FormSettings.TabbedNoteBook1.PageIndex;
      FormSettings.Free;
    finally
      Resume(Sender);
      InfoButton.Enabled:= (LifeBox1.Universe.Description.count > 0);
      DirectXImage.Visible:= MyDDSurface.DirectDrawEnabled = DDFast;
    end; {try}
  except {ignore}
  end;
end;

procedure TLife32MainForm.ItemCutClick(Sender: TObject);
begin
  WaitCursor;
  //Suspend(Sender);
  try
    if NotEdited then begin
      NotEdited:= false;
      LifeBox1.Revision:= LifeBox1.Revision + 1;
    end; {if}
    LifeBox1.CopySelection;
    LifeBox1.ClearSelection;
    UpdateButtons;
	  finally begin
	    DefaultCursor;
	    Resume(Sender);
	  end; {Finally}
  end; {try}
end;

procedure TLife32MainForm.ItemPasteClick(Sender: TObject);
begin
  WaitCursor;
  //Suspend(Sender);
  try
	  LifeBox1.PasteSelection;
	  UpdateButtons;
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemClearClick(Sender: TObject);
begin
  WaitCursor;
  //Suspend(Sender);
  try
	  LifeBox1.ClearSelection;
    UpdateButtons;
    finally begin
	    DefaultCursor;
	    Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemInsertShapeClick(Sender: TObject);
var
  AShape: TUniverse;
  OldRules: string;
begin
  AShape:= TUniverse.Create('',nbDefault);
  //Suspend(Self);
  LifeBox1.CanPaint:= cpDialogShowing;
  try
    with OpenDialog1 do begin
      OpenDialog1.InitialDir:= ExtractFileDir(GetLastFile);
      OpenDialog1.FileName:= ExtractFileName(GetLastFile);
      if Execute then begin
        OldRules:= LifeBox1.Universe.Rulestring;
        AShape.LoadFromFile(FileName);
        LifeBox1.InsertShape(AShape);
        if not(UseNewRules) then LifeBox1.Universe.Rulestring:= OldRules
        else LifeBox1.Universe.RuleString:= AShape.RuleString;
      end; {if}
    end; {with}
    UpdateButtons;
    finally begin
      AShape.Free;
  	  Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemSaveShapeClick(Sender: TObject);
var
  AShape: TUniverse;
begin
  Suspend(Self);
  LifeBox1.CanPaint:= cpDialogShowing;
  try
    with SaveDialog1 do begin
      if Execute then begin
        AShape:= LifeBox1.GetCutout;
        if Assigned(AShape) then try
          AShape.Description.Assign(Comments);
          LifeBox1.SaveShape(AShape, Filename, FilterIndex);
          finally AShape.Free;
        end; {try}
      end; {if}
    end; { with }
    finally Resume(Sender);
  end; {try}
end;

procedure TLife32MainForm.LifeBox1Exit(Sender: TObject);
begin
  //PauseButtonClick(Sender);
end;

procedure TLife32MainForm.ItemMirrorHorzClick(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.MirrorSelHorz;
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemMirrorVertClick(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.MirrorSelVert;
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemRotate90Click(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.RotateSel90;
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemRotate180Click(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.RotateSel180;
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemRotate270Click(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.RotateSel270;
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.UpdateButtons;
var
  IsEmpty: boolean;
  CanPlay: boolean;
begin
  CanPlay:= LifeBox1.CanPlay;
  IsEmpty:= LifeBox1.IsEmpty;
  if not CanPlay then begin
    if not IsPaused then PauseButtonClick(Self);
    PlayButton.Visible:= true;
  end;
  StepButton.Enabled:= (CanPlay) and not(IsEmpty);
  PlayButton.Enabled:= (CanPlay) and not(IsEmpty);
  Play1.Enabled:= (CanPlay) and not(IsEmpty);
  Stepforward1.Enabled:= (CanPlay) and not(IsEmpty);
  SaveButton.Enabled:= not(IsEmpty);
  Saveas1.Enabled:= not(IsEmpty);
  RunBenchmark1.Enabled:= not(IsEmpty);
  SnapShotButton.Enabled:= not(IsEmpty);
  MakeSnapshot1.Enabled:= not(IsEmpty);
  SkipTo1.Enabled:= not(IsEmpty);
  ZoomToFit2.Enabled:= not(IsEmpty);

  StepbackButton.Enabled:= LifeBox1.Generation > 0;
  Stepback1.Enabled:= StepbackButton.Enabled;
  RewindButton.Enabled:= RewindList.Items.Count > 0;
  RewindToSnapshot1.Enabled:= RewindButton.Enabled;

  PauseButton.Visible:= not IsPaused;
  Pause1.Enabled:= not IsPaused;
  if (CanPlay) and not(IsEmpty) then begin
    PlayButton.Visible:= IsPaused;
    Play1.Enabled:= IsPaused;
  end;
  CursorDrawButton.Visible:= LifeBox1.EditorMode = emDraw;
  CursorSelectButton.Visible:= LifeBox1.EditorMode = emSelect;
  //CursorZoomButton.Visible:= LifeBox1.EditorMode = emCursorZoom;
  CursorHandButton.Visible:= LifeBox1.EditorMode = emHand;
  GridButton.Down:= LifeBox1.Grid;
  RandomButton.Down:= RandomDot;
  InfoButton.Enabled:= (LifeBox1.Universe.Description.count > 0);

  GenEdit.Text:= IntToStr(LifeBox1.Generation);
  GenEdit.Refresh;
  //Re-evaluate the correctness of the value in the skiptoedit.
  SkipToEditChange(self);
  //RuleEdit.Text:= LifeBox1.Universe.Rulestring; OnRuleChange does this.
  RuleEdit.Refresh;
  ZoomLabel.Refresh;
  if IsPaused then begin
    GetCelCount(UpdateCelCount);
    UpdateStatistics;
  end;
end;

procedure TLife32MainForm.UpdateTabButtons(NewTab: integer);
begin
  FirstTabButton.Enabled:= (NewTab <> 0);
  PrevTabButton.Enabled:= FirstTabButton.Enabled;
  LastTabButton.Enabled:= (NewTab <> (UniverseTabset.Tabs.Count -1));
  NextTabButton.Enabled:= LastTabButton.Enabled;
end;

procedure TLife32MainForm.N01Click(Sender: TObject);
begin
  try
    SpeedButton.Down:= false;
    PlaySpeed:= StrToInt(TMenuItem(Sender).Caption);
    except {do nothing};
  end;
  Resume(Sender);
end;

procedure TLife32MainForm.SlowerButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Msg: TWMLButtonDown;
begin
  if Button = mbRight then with Msg do begin
    Msg:= WM_LButtonDown;
    Keys:= MK_LBUTTON	;
    xpos:= 0; ypos:= 0;
  end;
  SpeedButton.Dispatch(Msg);
end;

procedure TLife32MainForm.FasterShadowButtonClick(Sender: TObject);
begin
  SlowerButtonMouseDown(Sender, mbRight,[],0,0);
end;


procedure TLife32MainForm.SpeedPopupPopup(Sender: TObject);
var
  Msg: TWMLButtonUp;
begin
  Suspend(Sender);
  with Msg do begin
    Msg:= WM_RButtonUp;
    Keys:= MK_RButton;
    Xpos:= 0; YPos:= 0;
  end;
  SpeedButton.Dispatch(Msg);
  CloseDialogs;
end;

procedure TLife32MainForm.StepbackButtonClick(Sender: TObject);
var
  TargetGen: integer;
begin
  Resume(Sender);
  if not(SkipToMode) then begin
    if not IsPaused then PauseButtonClick(Sender);
    //LifeBox1.HideSelection;
    StopCounting;
    TargetGen:= LifeBox1.Generation - 1;
    if TargetGen >= MinGen then begin
      SteppingBack:= true;
      ShowGenerations:= false;
      Generation:= TargetGen;
      SteppingBack:= false;
    end {if}
    else begin
      StepBackButton.Enabled:= false;
      StepBack1.Enabled:= StepBackButton.Enabled;
    end;
    LifeBox1.UpdateAll;
    GenEdit.Text:= IntToStr(LifeBox1.Generation);
    //StepbackButton.Enabled:= false;
    //Stepback1.Enabled:= false;
    //LifeBox1.ShowSelection;
  end;
end;

procedure TLife32MainForm.LifeBoxPopupPopup(Sender: TObject);
var
  HasSelection: boolean;
  HasInsertion: boolean;
  HasBigRect: boolean;
begin
  //PauseButtonClick(Sender);
  Suspend(Sender);
  HasSelection:= not(IsMyRectEmpty(LifeBox1.SelectionRect));
  HasInsertion:= not(IsMyRectEmpty(LifeBox1.OrgSelectionRect));
  with Lifebox1.OrgSelectionRect do begin
    HasBigRect:= HasInsertion and not(IsMyRectEmpty(Rect(Left,Top,Right-1,Bottom-1)))
  end;
  ItemCut.Visible:= HasSelection;
  ItemCopy.Visible:= HasSelection;
  ItemPaste.Visible:= Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_BITMAP);
  ItemClear.Visible:= HasSelection;
  Clearoutsideselection1.Visible:= HasSelection;
  n9.Visible:= HasSelection or ItemPaste.Visible;

  Cut1.Enabled:= HasSelection;
  Copy1.Enabled:= HasSelection;
  Paste1.Enabled:= ItemPaste.Visible and HasInsertion;
  Clear1.Enabled:= HasSelection;
  //--
  ItemSelectAll.Visible:= not(HasSelection) and not(LifeBox1.IsEmpty);
  n6.Visible:= ItemSelectAll.Visible;
  SelectAll1.Enabled:= not(LifeBox1.IsEmpty);
  //--
  ItemMirrorHorz.Visible:= HasSelection;
  ItemMirrorVert.Visible:= HasSelection;
  ItemRotate90.Visible:= HasSelection;
  ItemRotate180.Visible:= HasSelection;
  ItemRotate270.Visible:= HasSelection;
  n7.Visible:= HasSelection;

  MirrorHorz1.Enabled:= ItemMirrorHorz.Visible;
  MirrorVert1.Enabled:= ItemMirrorVert.Visible;
  Rotate901.Enabled:= ItemRotate90.Visible;
  Rotate1801.Enabled:= ItemRotate180.Visible;
  Rotate2701.Enabled:= ItemRotate270.Visible;
  //--
  ItemBox1.Visible:= HasBigRect;
  ItemFillRandom.Visible:= HasBigRect;
  ItemFillBlack.Visible:= HasBigRect;
  ItemInvert.Visible:= HasBigRect;
  n5.Visible:= ItemFillRandom.Visible;

  Box2.Enabled:= ItemBox1.Visible;
  FillRandom1.Enabled:= ItemFillRandom.Visible;
  FillBlack1.Enabled:= ItemFillBlack.Visible;
  Invert1.Enabled:= ItemInvert.Visible;
  //--
  ItemSaveShape.Visible:= HasSelection;
  SaveShape1.Enabled:= HasSelection;
  InsertShape1.Enabled:= HasInsertion;
  //Refresh pictures
  //PictureMenu(LifeBoxPopup.Items,true);
  //PictureMenu(Edit1,true);
end;

procedure TLife32MainForm.ScrollBarLRKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vk_Next, vk_Prior: begin
       if ScrollBarUD.CanFocus then ScrollBarUD.SetFocus;
       Key:= 0;
    end;
    //handled by lifebox.
    vk_Down,vk_Up,vk_Left,vk_Right: Key:= 0;
    vk_Tab: if (ssShift in Shift) then Key:= vk_Next else Key:= vk_Prior;
  end {case}
end;

procedure TLife32MainForm.ScrollBarUDKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vk_Tab: begin
       if ScrollBarLR.CanFocus then ScrollBarLR.SetFocus;
       Key:= 0;
    end;
    //handled by lifebox.
    vk_Left,vk_Right,vk_Down,vk_Up: Key:= 0;
  end {case}
end;

procedure TLife32MainForm.MeasureButtonKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then Key:= 0;
end;

procedure TLife32MainForm.Preferences1Click(Sender: TObject);
var
  FormSettings: TFormPrefs;
begin
  PauseButtonClick(Sender);
  Suspend(Sender);
  try
    FormSettings:= TFormPrefs.Create(Self);
    LifeBox1.CanPaint:= cpDialogShowing;
	  FormSettings.TabbedNotebook1.PageIndex:= TMenuItem(Sender).GroupIndex;
	  FormSettings.ShowModal;
	  FormSettings.Free;
	  //RuleEdit.Text:= LifeBox1.Universe.RuleString;
    InfoButton.Enabled:= (LifeBox1.Universe.Description.Count > 0);
    DirectXImage.Visible:= MyDDSurface.DirectDrawEnabled = DDFast;
    finally Resume(Sender);
  end; {try}
end;

procedure TLife32MainForm.FlipXButtonClick(Sender: TObject);
var
  x,y: integer;
begin
  Resume(Sender);
  with LifeBox1 do try
    X:= XScroll;
    y:= YScroll;
    MirrorHorz;
    MoveToFast(x+(8-x)*2-1,y);
    //Refresh;
    RedrawAll;
    finally begin
      UpdateButtons;
    end; {finally}
  end; {with}
end;

procedure TLife32MainForm.FlipYButtonClick(Sender: TObject);
var
  x,y: integer;
begin
  Resume(Sender);
  with LifeBox1 do try
    x:= XScroll;
    y:= YScroll;
    MirrorVert;
    MoveToFast(x,y+(8-y)*2-1);
    //Refresh;
    RedrawAll;
    finally begin
      UpdateButtons;
    end; {finally}
  end; {with}
end;

procedure TLife32MainForm.R90ButtonClick(Sender: TObject);
var
  x,y: integer;
begin
  Resume(Sender);
  with LifeBox1 do try
    x:= XScroll;
    y:= yscroll;
    Rotate90;
    x:= x + (8-x)*2-1;
    MoveToFast(-y,-x);
    //Refresh;
    RedrawAll;
    finally begin
      UpdateButtons;
    end; {finally}
  end; {with}
end;

procedure TLife32MainForm.R180ButtonClick(Sender: TObject);
var
  x,y: integer;
begin
  with LifeBox1 do try
    x:= Xscroll;
    y:= YScroll;
    Rotate180;
    MoveByFast((8-x)*2-1,(8-y)*2-1);
    //Refresh;
    RedrawAll;
    finally begin
      Resume(Sender);
      UpdateButtons;
    end; {finally}
  end; {with}
end;

procedure TLife32MainForm.R270ButtonClick(Sender: TObject);
var
  x,y: integer;
begin
  with LifeBox1 do try
    x:= XScroll;
    y:= YScroll;
    Rotate270;
    y:= y + (8-y)*2-1;
    MoveToFast(-y,-x);
    //Refresh;
    RedrawAll;
    finally begin
      Resume(Sender);
      UpdateButtons;
    end; {finally}
  end; {with}
end;

procedure TLife32MainForm.SkipToButtonClick(Sender: TObject);
begin
  Suspend(Sender);
  ShowSidePanel(TSSnapShots,0);
  if SkipToEdit.CanFocus then SkipToEdit.SetFocus;
end;

procedure TLife32MainForm.SetGeneration(value: integer);
var
  StartGen: integer;
  i: integer;
  ASnapShot: TSnapshot;
  Found: boolean;
  SaveLastOffset: integer;
begin
  SkipToGen:= value;
  StartGen:= LifeBox1.Generation;
  LastOffset:= SkipToGen - StartGen;
  if (LastOffset <= 1) and (LastOffset >= -1) then LastOffset:= 0;
  //Skip forward
  if (SkipToGen > StartGen) then begin
    SkipToMode:= true;
  end {if}
  //No, skip backward (that is, first jump back then skipforward.
  else if SkipToGen < StartGen then begin
    //try stepping back.
    if ((SkipToGen + 1) = StartGen) and LifeBox1.Universe.BackCorrect then
    LifeBox1.Generate(false)
    //Ok, lets jump
    else begin
      Found:= false;
      ASnapShot:= nil;
      i:= 0;
      while (i < RewindList.Items.Count) and not(Found) do begin
        with RewindList do
          ASnapshot:= GetSnapShot(i);
        found:= (ASnapShot.Generation >= SkipToGen) and
          (LifeBox1.PatternID = ASnapshot.PatternID) and
          (LifeBox1.Revision = ASnapshot.Revision);
        if not(found) then Inc(i);
      end; {while}
      if (Found) then begin
        if (ASnapShot.Generation > SkipToGen) then begin
          if (i>0) then ASnapShot:= RewindList.GetSnapShot(i-1)
          else ASnapshot:= nil; //should never occur.
        end; {if}
      end; {if}
      if Assigned(ASnapshot) then begin
        if FSkipToShowGens then LifeBox1.RewindToSnapshot(ASnapshot)
        else LifeBox1.RewindToSnapshotSilent(ASnapshot);
        SaveLastOffset:= LastOffset;
        //We've jumped, now skip the rest.
        //limited recursion.
        if (LifeBox1.Generation <> SkipToGen) then SetGeneration(SkipToGen)
        else ShowRelGens(SaveLastOffset);
        LastOffset:= SaveLastOffset;
      end; {if}
    end; {else}
  end; {else if}
  //UpdateButtons;
end;

function TLife32MainForm.GetGeneration: integer;
begin
  Result:= LifeBox1.Generation;
end;

procedure TLife32MainForm.GenTimerTimer(Sender: TObject);
const
  Even: boolean = false;
begin
  //if not(IsPaused) then begin
    GenEdit.Text:= IntToStr(LifeBox1.Generation);
    GenEdit.Hint:= '@#±0..2.147.483.647'+' : '+GenEdit.Text;
    if SkipToMode and not(SteppingBack) then begin
      //only update half the time, so it doesn't slow down that much.
      Even:= not Even;
      if Even then ProgressBar1.Progress:= LifeBox1.Generation;
    end;
  //end; {if}
end;

procedure TLife32MainForm.Game1Click(Sender: TObject);
begin
  Suspend(Sender);
  CloseDialogs;
end;

procedure TLife32MainForm.Menu1Click(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to MainMenu1.Items.Count-1 do with MainMenu1 do begin
    Items[i].Visible:= not(Items[i].Visible);
  end; {for}
  Menu1.Checked:= MainMenu1.Items[0].Visible;
  Menu2.Checked:= Menu1.Checked;
  if Menu2.Checked then begin
    EnableMenuItem(GetSystemMenu(Handle,false),CmdShowMenu,
                   MF_BYCOMMAND or MF_GRAYED);
  end
  else begin
    EnableMenuItem(GetSystemMenu(Handle,false),CmdShowMenu,
                   MF_BYCOMMAND or MF_ENABLED);
  end;
  FormResize(Sender);
  Resume(Sender);
end;

procedure TLife32MainForm.Toolbar1Click(Sender: TObject);
begin
  HideToolbar:= not(HideToolbar);
  Resume(Sender);
end;

procedure TLife32MainForm.StatusLine1Click(Sender: TObject);
begin
  HideStatusbar:= not(HideStatusbar);
  Resume(Sender);
end;

procedure TLife32MainForm.Scrollbars1Click(Sender: TObject);
begin
  HideScrollbars:= not(HideScrollbars);
  Resume(sender);
end;

procedure TLife32MainForm.ShowAll1Click(Sender: TObject);
begin
  if not(Game1.Visible) then Menu1Click(Sender);
  if not(ButtonBar.Visible) then ToolBar1Click(Sender);
  if not(HelpBar.Visible) then StatusLine1Click(Sender);
  if not(PanelRight.Visible) or not(PanelBottom.Visible) then
    ScrollBars1Click(Sender);
end;

procedure TLife32MainForm.HideAll1Click(Sender: TObject);
begin
  if (PanelRight.Visible) or (PanelBottom.Visible) then
    ScrollBars1Click(Sender);
  if (HelpBar.Visible) then StatusLine1Click(Sender);
  if (ButtonBar.Visible) then ToolBar1Click(Sender);
end;

procedure TLife32MainForm.MoveToButtonClick(Sender: TObject);
begin
  Suspend(Sender);
  ShowSidePanel(TSMoveTo,0);
end;

procedure TLife32MainForm.Cancel1Click(Sender: TObject);
begin
  Resume(Sender);
end;

procedure TLife32MainForm.FormClose(Sender: TObject; var Action: TCloseAction);
//var
  //Dummy: integer;
begin
  //this just to hide the infobox asap, so as not to worry the user.
  HideScrollHint;
  try
    PlayTimer.Enabled:= false;
    GenTimer.Enabled:= false;
    if IsCounting then StopCounting;
  except {ignore}
  end; {try}
  try
    SaveFormSettings;
    SaveSettings;
    LifeClosesOK; //Store the fact that Life32 did not crash.
    except {ignore} //or app will not close in case of an error.
  end;
end;

procedure TLife32MainForm.CloseDialogs;
begin
  if Assigned(OpenedDialog) then OpenedDialog.ModalResult:= mrCancel;
end;

(*
procedure TLife32MainForm.ClearSnapshots;
var
  i: integer;
begin
  i:= RewindList.Items.Count;
  while i > 0 do begin
    Dec(i);
    RewindList.Items.Delete(i);
  end; {while}
end; (**)

procedure TLife32MainForm.ButtonbarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PopupPoint: TPoint;
begin
  if Button = mbRight then begin
    PopupPoint.x:= x;
    PopupPoint.y:= y;
    PopupPoint:= TControl(Sender).ClientToScreen(PopupPoint);
    with PopupPoint do ViewPopup.Popup(x,y);
    Resume(Sender);
  end; {if}
end;

procedure TLife32MainForm.MakeSnapshot1Click(Sender: TObject);
begin
  SnapshotButtonClick(Sender);
  Resume(Sender);
end;

procedure TLife32MainForm.RewindToSnapshot1Click(Sender: TObject);
begin
  RewindButtonClick(Sender);
  Resume(Sender);
end;

procedure TLife32MainForm.MoveTo1Click(Sender: TObject);
begin
  MoveToButton.Click;
end;

procedure TLife32MainForm.InfoButtonClick(Sender: TObject);
var
  HintRect: TRect;
begin
  if not(InfoButton.Down) then HideScrollHint
  else if (LifeBox1.Universe.Description.Count > 0) then begin
    FScrollHint:= TScrollHint.Create(Self);
    OldOnMessage:= Application.OnMessage;
    Application.OnMessage:= MyAppMsgHook;
    if Assigned(FScrollHint) then with FScrollHint do begin
      HintRect:= CalcHintRect((Screen.Width div 3)*2,
                 LifeBox1.Universe.Description.Text,nil);
      OffsetRect(HintRect, InfoButton.ClientOrigin.x,
                 InfoButton.ClientOrigin.y+InfoButton.Height);
      ActivateHint(HintRect,LifeBox1.Universe.Description.Text);
    end; {with}
  end; {if}
end;

procedure TLife32MainForm.MRU1Click(Sender: TObject);
var
  Filename: string;
  Index: integer;
begin
  try
    Index:= TMenuItem(Sender).Tag;
    Filename:= GetMRUFilename(Index);
    if FileExists(Filename) then begin
      OpenFile(Filename);
    end {if}
    else begin
      RemoveMRUFile(Index);
      UpdateMRUMenu;
    end; {else}
    Suspend(Sender);
    Resume(Sender);
    except begin
      //ShowMessage('Error in TLife32MainForm.MRU1Click');
      raise;
    end; {except}
  end; {try}
end;

procedure TLife32MainForm.UpdateMRUMenu;
var
  MRUFiles: TStringlist;
  Parent1: TMenuItem;
  FirstIndex: integer;
  Parent2: TMenuItem;
  SecondIndex: integer;
  TempStr: string;
  i,a: integer;
begin
  MRUFiles:= GetMRUList;
  i:= 0;
  while i < MRUFiles.Count do begin
    a:= Pos('&',MRUFiles[i]);
    if a <> 0 then begin
      TempStr:= MRUFiles[i];
      Insert('&',TempStr,a);
      MRUFiles[i]:= TempStr;
    end; {if}
    Inc(i);
  end;
  try
    Parent1:= MRU1.Parent;
    FirstIndex:= MRU1.MenuIndex;
    Parent2:= PMRU1.Parent;
    SecondIndex:= PMRU1.MenuIndex;
    //show/hide separator.
    N20.Visible:= (MRUFiles.count > 0);
    N33.Visible:= (MRUFiles.count > 0);
    OpenNextFile1.Enabled:= (MRUFiles.count > 0);
    OpenNextFile2.Enabled:= (MRUFiles.count > 0);
    for i:= 0 to 9 do begin
      if i < MRUFiles.count then begin
        Parent1.Items[FirstIndex+i].Visible:= true;
        Parent2.Items[SecondIndex+i].Visible:= true;
        Parent1.Items[FirstIndex+i].Caption:=
          '&'+IntToStr(i+1)[Length(IntToStr(i+1))]+' '+MRUFiles[i];
        Parent2.Items[SecondIndex+i].Caption:=
          '&'+IntToStr(i+1)[Length(IntToStr(i+1))]+' '+MRUFiles[i];
      end
      else begin
        Parent1.Items[FirstIndex+i].Visible:= false;
        Parent2.Items[SecondIndex+i].Visible:= false;
      end; {else}
    end; { for }
    finally MRUFiles.Free;
  end; {try}
  //PictureMenu(Game1,true);  //This is dead code.
end;

procedure TLife32MainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CursorPos: TPoint;
  AKey: Char;
  Offset: integer;
  i: integer;
  a: integer;
  found: boolean;
  AnItem: TListItem;
  StartDragHack: TStartDragHack;
begin
  if (SkipToMode) then SkipToRedraw:= true;
  case Key of //always handle <esc>.
    vk_escape: with LifeBox1 do begin
      IncludeSelection;
      CancelSelection;
      UpdateButtons;
      //GenEdit.Text:= IntToStr(LifeBox1.Generation);
    end; {vk_escape}
    vk_Return,10: if SkipToMode then key:= 0; //ignore enter in skiptomode.
  end; {case}
  AKey:= Upcase(Char((MapVirtualKey(Key,2)and $FF)));

  if not ((ActiveControl = UniverseTabRenamer) or
          (ActiveControl is THotKey) or
          (ActiveControl = PatternNameEdit) or
          (ActiveControl = RuleEdit) or
          (ActiveControl is TRichEdit)) then
  if ((not(ActiveControl is TCustomEdit))
      or CharInSet(AKey,HandleAlways) or ((shift * [ssCtrl,ssAlt]) <> [])) then begin
    LifeBox1.KeyDown(Key, Shift);
    GetCursorPos(CursorPos);

    //if LifeBox wants to disable the key, it should set it to #0.
    AKey:= Upcase(Char((MapVirtualKey(Key,2)and $FF)));
    if (AKey <> #0) then
      case (AKey) of
      #9: activeControl:= LifeBox1;
      #13: if IsPaused then PlayButtonClick(PlayButton) else PauseButtonClick(PauseButton);
      #32,'>','.': if StepButton.Enabled then StepButtonClick(StepButton);
      '<',',': if StepbackButton.Enabled then StepBackButtonClick(StepBackButton);
      'A'..'W':if ((Shift = []) or (Shift = [ssShift])) then begin
        i:= 0;
        found:= false;
        while (PatternListView.Items.count > i) and not(found) do begin
          a:= ShortCut(Integer(AKey),shift);
          found:= a = TShortCut(PatternListView.Items[i].OverlayIndex);
          if not found then inc(i);
        end; {while}
        if found then begin
          AnItem:= PatternListView.Items[i];
          with DropLife32Source1 do try
            Universe:= TUniverse(AnItem.Data);
            ImageIndex:= AnItem.Index;
            ImageHotSpotX:= 4;
            ImageHotSpotY:= 4;
            SpecialDrag:= true;
            //wiggle the mouse around to force Windows to show the pattern.
            StartDragHack:= TStartDragHack.Create;
            Execute;
            StartDragHack.Free;
          finally ;
          end; {with try}
        end;
      end {if}
      else if (shift = [ssCtrl]) then case (AKey) of
        'I': begin
          ShowSidePanel(TSInfo,0);
        end; {i:}
      end; {else case}
      //'L': R90ButtonClick(Self);
      //'M': if (Shift = [ssCtrl]) then MakeSnapshot1Click(Sender);
      //'O': if (Shift = [ssCtrl]) then OpenButtonClick(Sender);
      //'P': PauseButton.Click;
      //'R': if (Shift = [ssCtrl]) then RewindToSnapshot1Click(Sender)
      //     else R270ButtonClick(Self);
      //'S': if (Shift = [ssCtrl]) then SaveButtonClick(Sender);
      'X': FlipXButtonClick(Self);
      'Y': FlipYButtonClick(Self);
      'Z': begin
        if LifeBox1.SelectionVisible then LifeBox1.ZoomToSelection(MaxZoomToFit)
        else LifeBox1.ZoomToFit(DrawAgain, MaxZoomToFit);
      end; {'Z'}
      '/': CelZoom:= CelZoom-1;
      '*': CelZoom:= CelZoom+1;
      '#': Grid1Click(Self);
      '+','=': if FasterButton.Enabled then FasterButton.Click;
      '-','_': if SlowerButton.Enabled then SlowerButton.Click;
      '0'..'9': begin
        if (Shift = [ssAlt]) then begin
          case (AKey) of
            '1':if MRU1.Visible then MRU1Click(MRU1);
            '2':if MRU2.Visible then MRU1Click(MRU2);
            '3':if MRU3.Visible then MRU1Click(MRU3);
            '4':if MRU4.Visible then MRU1Click(MRU4);
            '5':if MRU5.Visible then MRU1Click(MRU5);
            '6':if MRU6.Visible then MRU1Click(MRU6);
            '7':if MRU7.Visible then MRU1Click(MRU7);
            '8':if MRU8.Visible then MRU1Click(MRU8);
            '9':if MRU9.Visible then MRU1Click(MRU9);
            '0':if MRU10.Visible then MRU1Click(MRU10);
          end; {case}
          if CharInSet(AKey,['0'..'9']) then Key:= 0;
        end {if}
        else if (Shift = [ssShift]) then case AKey of
          '1': R180ButtonClick(Self);
          '2': R270ButtonClick(Self);
          '3': Grid1Click(Self);
          '5': if RandomButton.Visible then RandomButton.Click;
          '8': CelZoom:= CelZoom + 1;
          '9': R90ButtonClick(Self);
        end; { case }
      end; {'0'..'9'}
    end; {case}
    Offset:= Max(CelZoom,1);
    //@@@@ add code, so that doing a shift cursor-key starts a box-drag.
    if (Shift = [ssShift]) then case Key of
      vk_Left: SetCursorPos(CursorPos.X - Offset,CursorPos.y);
      vk_Right: SetCursorPos(CursorPos.X + Offset,CursorPos.y);
      vk_Down: SetCursorPos(CursorPos.X,CursorPos.y + Offset);
      vk_Up: SetCursorPos(CursorPos.X,CursorPos.y - Offset);
    end; {if case}
  end {if}
  else if (ActiveControl = GenEdit) then begin
    case Key of
      vk_Left,vk_Right,vk_Down,vk_Up: {do nothing, leave key alone};
      else Key:= 0;
    end;
  end
  else Key:= 0;
end;

procedure TLife32MainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not(ActiveControl is TEdit)) and (not(ActiveControl is TMemo)) and
     (not(ActiveControl is THotKey)) then
    LifeBox1.KeyUp(Key, Shift);
end;

procedure TLife32MainForm.Restoreallsettingstodefault1Click(Sender: TObject);
var
  i: integer;
  LimitRect: TRect;
begin
  with LifeBox1 do begin
    FreezeSelection:= false;
    Universe.RuleString:= '';
    Universe.Neighborhood:= nbDefault;

    FrameDropTime:= DefaultFrameDropTime;
    Grid:= DefaultGrid;
    EditorMode:= emSelect;
    PasteMode:= lpmOr;
    FillPercentage:= DefaultFillPercentage;
    BoldGridSpacing:= DefaultBoldGridSpacing;
    //Colors
    HandScroll:= DefaultScroll;
    CelColor:= DefaultCelColor;
    BackColor:= DefaultBackColor;
    GridColor:= DefaultGridColor;
    Grid2Color:= DefaultGrid2Color;
    DragColor:= DefaultDragColor;
    TorusColor:= DefaultTorusColor;
    SelRectColor:= DefaultSelRectColor;
    //Torus stuff
    LimitRect.Left:= DefaultLimitRect.Left;
    LimitRect.Top:= DefaultLimitRect.Top;
    LimitRect.Right:= DefaultLimitRect.Right;
    LimitRect.Bottom:= DefaultLimitRect.Bottom;
    Limit:= LimitRect;
    IsLimited:= DefaultIsLimited;
    TorusKind:= DefaultTorusKind;
    DeadEdges:= DefaultDeadEdges;
  end; {with}
  //Snapshots
  for i:= cpFirst to cpLast do AutoRewindWhat[i]:= true;
  AutoRewindWhat[cpStep]:= false;
  SnapshotEvery:= 0;
  SnapshotTimer.Interval:= defaultSnapshotTimeOut;
  NoSnapshotTimeOut:= defaultNoSnapshotTimeOut;

  CelZoom:= DefaultZoom;
  PlaySpeed:= DefaultSpeed;
  MyDDSurface.DirectDrawEnabled:= DDFast;
  SmallScroll:= DefaultScroll;
  LifeLoad.MaxPictureWidth:= DefaultPictureWidth;
  SaveDialog1.InitialDir:= '';
  Extensions:= DefaultExtensions;
  ZoomToFit:= true;
  MaxZoomToFit:= MaxZoom;
  OnlyUseDefaultToolTipColors:= false;
  ShowSidePanel(TSOpen,0);
  ShowSidepanelOnStartup:= DefaultShowSidepanelOnStartup;
  HidePanelOnPlay:= DefaultHidePanelOnPlay;
  ReshowSkip:= DefaultReshowSkip;
  ZoomToFit:= DefaultZoomToFit;

  SaveSettings;
  Resume(Sender);
end;

procedure TLife32MainForm.Transparent1Click(Sender: TObject);
begin
  LifeBox1.PasteMode:= TMenuItem(Sender).Tag;
end;

procedure TLife32MainForm.OpenButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Msg: TWMLButtonDown;
begin
  if Button = mbRight then with Msg do begin
    Msg:= WM_LButtonDown;
    Keys:= MK_LBUTTON	;
    xpos:= 0; ypos:= 0;
  end;
  MRUPopupButton.Dispatch(Msg);
end;

procedure TLife32MainForm.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Msg: TWMLButtonDown;
begin
  if Button = mbRight then with Msg do begin
    Msg:= WM_LButtonDown;
    Keys:= MK_LBUTTON	;
    xpos:= 0; ypos:= 0;
  end;
  TButton(Sender).Dispatch(Msg);
end;

procedure TLife32MainForm.MRUPopupPopup(Sender: TObject);
begin
  Suspend(Sender);
  CloseDialogs;
end;

procedure TLife32MainForm.XEditKeyPress(Sender: TObject; var Key: Char);
var
  X1, Y1: integer;
begin
  if CharInSet(Key,[#13,#10]) then try
    Key:= #0;
    if (Sender = EditX) or (Sender = EditY) then begin
      XEdit.Text:= EditX.Text;
      YEdit.Text:= EditY.Text;
    end;
    X1:= StrToInt(Trim(XEdit.Text));
    Y1:= StrToInt(Trim(YEdit.Text));
    if (x1 >= MinX) and (x1 <= MaxX) and (y1 >= MinY) and (y1 <= MaxY) then
      MoveTo(x1,y1);
    except {ignore}
  end
  else if not CharInSet(Key,ValidNumbers) then Key:= #0
end;

procedure TLife32MainForm.XEditChange(Sender: TObject);
begin
  try
    with Sender as TEdit do try
      StrToInt(Text);
      Font.Color:= clWindowText;
      except Font.Color:= clRed;
    end; {try}
    except {ignore}
  end; {try}
end;

function TLife32MainForm.GetMinGen: integer;
begin
  Result:= 0;
end;

procedure TLife32MainForm.GenEditChange(Sender: TObject);
var
  i: integer;
begin
  if (ActiveControl = GenEdit) and (IsPaused) then try
    with Sender as TEdit do try
      i:= StrToInt(Text);
      if i <> LifeBox1.Generation then Font.Color:= clBlue
      else Font.Color:= clWindowText;
      if (Length(Text)>0) and CharInSet(Text[1],['-','+']) then begin
        if (i < (MinGen - LifeBox1.Generation)) then Font.Color:= clRed
      end
      else if (i < MinGen) then Font.Color:= clRed

      except Font.Color:= clRed;
    end; {try}
    except {ignore}
  end; {try}
end;

procedure TLife32MainForm.GenEditKeyPress(Sender: TObject; var Key: Char);
var
  TargetGen: integer;
  NewKey: Word;
  ShiftState: TShiftState;
const
  ShowGens = true;
  BlindSkip = false;
begin
  if not CharInSet(Key,ValidNumbers) then begin
    if not CharInSet(Key,HandleAlways) then begin
      NewKey:= VkKeyScan(Key);
      if Bool(GetKeyState(vk_Shift)) then ShiftState:= [ssShift];
      Form1.FormKeyDown(Self,NewKey,ShiftState);
    end;
    key:= #0;
  end;
  if CharInSet(Key,[#10,#13]) then begin
    GenEdit.Font.Color:= clWindowText;
    Key:= #0;
    if not(SkipToMode) then begin
      try
        TargetGen:= StrToInt(GenEdit.Text);
        except TargetGen:= -1;
      end; {try}
      if (Length(GenEdit.Text) > 0) and CharInSet(GenEdit.Text[1],['+','-']) then
        TargetGen:= LifeBox1.Generation + TargetGen;
      if (TargetGen >= MinGen) then begin
        if (TargetGen < LifeBox1.Generation) then begin
          ShowGenerations:= BlindSkip;
          Generation:= TargetGen;
        end
        else begin
          ShowGenerations:= ShowGens;
          Generation:= TargetGen;
        end;
        if GenEdit.CanFocus then GenEdit.SetFocus;
      end; {if}
    end; {if}
  end; {if}
end;

procedure TLife32MainForm.GenEditClick(Sender: TObject);
begin
  try
    with Sender as TEdit do SelectAll;
    except {ignore}
  end; {try}
end;

procedure TLife32MainForm.RuleEditChange(Sender: TObject);
begin
  with RuleEdit do begin
    if LifeBox1.Universe.IsValidRule(Text) then begin
      if lowercase(Text) = lowercase(LifeBox1.Universe.Rulestring) then
        Font.Color:= clWindowText
      else Font.Color:= clBlue;
    end
    else Font.Color:= clRed;
  end; {with}
end;

procedure TLife32MainForm.RuleEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Upcase(Key),ValidRules) then Key:= #0;
  if CharInSet(Key,[#10,#13]) then with RuleEdit do begin
    if LifeBox1.Universe.IsValidRule(Text) then begin
      LifeBox1.Universe.RuleString:= Text;
      Font.Color:= clWindowText;
    end;
    if LifeBox1.CanFocus then LifeBox1.SetFocus;
  end;
end;

procedure TLife32MainForm.SpeedLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PopupPoint: TPoint;
begin
  //if Button = mbRight then begin
    PopupPoint.x:= x;
    PopupPoint.y:= y;
    PopupPoint:= TControl(Sender).ClientToScreen(PopupPoint);
    with PopupPoint do SpeedPopup.Popup(x,y);
    Resume(Sender);
  //end;
end;

procedure TLife32MainForm.ZoomLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PopupPoint: TPoint;
begin
  //if Button = mbRight then begin
    PopupPoint.x:= x;
    PopupPoint.y:= y;
    PopupPoint:= TControl(Sender).ClientToScreen(PopupPoint);
    with PopupPoint do ZoomPopup.Popup(x,y);
    Resume(Sender);
  //end;
end;

procedure TLife32MainForm.LifeBox1MustPause(Sender: TObject);
begin
  if not IsPaused then PauseButtonClick(Sender);
end;

procedure TLife32MainForm.ItemSelectAllClick(Sender: TObject);
begin
  PauseButton.Click; //Pause after selecting.
  OldIsPaused:= true;
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.SelectAll(DrawAgain);
    finally begin
      DefaultCursor;
      Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemFillBlackClick(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.FillBlack;
    UpdateButtons;
    finally begin
      Resume(Sender);
      DefaultCursor;
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.ItemInvertClick(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
    LifeBox1.InvertSelection;
    UpdateButtons;
    finally begin
      Resume(Sender);
      DefaultCursor;
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.CancelSkipButtonClick(Sender: TObject);
begin
  SkipToMode:= false;
end;

procedure TLife32MainForm.DrawMode1Click(Sender: TObject);
begin
  with TMenuItem(Sender) do try
    LifeBox1.EditorMode:= Tag;
    Checked:= true;
    finally Resume(Sender);
  end;
end;

procedure TLife32MainForm.LifeBox1EditorModeChange(Sender: TObject);
begin
  CursorDrawButton.Visible:= false;
  CursorZoomButton.Visible:= false;
  CursorHandButton.Visible:= false;
  CursorSelectButton.Visible:= false;

  CursorDrawImage.Visible:= false;
  CursorZoomImage.Visible:= false;
  CursorHandImage.Visible:= false;
  CursorSelectImage.Visible:= false;
  //LifeBox1.CancelSelection;
  case LifeBox1.EditorMode of
    emDraw: begin
      CursorDrawButton.Visible:= true;
      CursorDrawImage.Visible:= true;
      DrawMode1.Checked:= true;
      DrawMode2.Checked:= true;
    end; {emDraw:}
    emSelect: begin
      CursorSelectButton.Visible:= true;
      CursorSelectImage.Visible:= true;
      SelectMode1.Checked:= true;
      SelectMode2.Checked:= true;
    end; {emSelect:}
    emHand: begin
      CursorHandButton.Visible:= true;
      CursorHandImage.Visible:= true;
      HandMode1.Checked:= true;
      HandMode2.Checked:= true;
    end; {emHand:}
    emCursorZoom: begin
      CursorZoomButton.Visible:= true;
      CursorZoomImage.Visible:= true;
      ZoomMode1.Checked:= true;
      ZoomMode2.Checked:= true;
    end; {emCursorZoom:}
  end; {case}
end;

procedure TLife32MainForm.LifeBox1PasteModeChange(Sender: TObject);
begin
  PasteOrButton.Visible:= false;
  PastePutButton.Visible:= false;
  PasteXorButton.Visible:= false;
  PasteErrorButton.Visible:= false;

  PasteOrImage.Visible:= false;
  PastePutImage.Visible:= false;
  PasteXorImage.Visible:= false;
  PasteErrorImage.Visible:= false;

  case LifeBox1.PasteMode of
    lpmOr: begin
      PasteOrButton.Visible:= true;
      PasteOrImage.Visible:= true;
      Transparent1.Checked:= true;
      Transparent2.Checked:= true;
    end; {pmOr:}
    lpmPut: begin
      PastePutButton.Visible:= true;
      PastePutImage.Visible:= true;
      Opaque1.Checked:= true;
      Opaque2.Checked:= true;
    end; {pmPut:}
    lpmXor: begin
      PasteXorButton.Visible:= true;
      PasteXorImage.Visible:= true;
      Xor1.Checked:= true;
      Xor2.Checked:= true;
    end; {pmXor:}
    lpmError: begin
      PasteErrorButton.Visible:= true;
      PasteErrorImage.Visible:= true;
      DontOverwrite1.Checked:= true;
      DontOverwrite2.Checked:= true;
    end; {pmError:}
  end; {case}
end;

procedure TLife32MainForm.LifeBox1ZoomChange(Sender: TObject);
var
  i: integer;
  ZoomStr: string;
begin
  with ZoomPopup do begin
   for i:= 0 to Items.Count -1 do Items[i].Checked:= false;
  end; {with}
  ZoomPopup.Items[CelZoom-MinZoom+2].Checked:= true;
  ZoomStr:= Trim(ZoomPopup.Items[CelZoom-MinZoom+2].Caption);
  ZoomButton.Hint:= 'Zoom:'+ZoomStr+'|'+GetLongHint(ZoomButton.Hint);
  ZoomLabel.Text:= ZoomStr;
  if CelZoom > 3 then begin
    GridButton.Enabled:= true;
    Grid1.Enabled:= true;
    if LifeBox1.Grid then begin
      GridButton.Down:= true;
      Grid1.checked:= true;
    end {if}
    else begin
      GridButton.Down:= false;
      Grid1.Checked:= false;
    end; {else}
  end {if}
  else begin
    GridButton.Enabled:= false;
    Grid1.Enabled:= false;
  end; {else}

  ZoomIn1.Enabled:= not(CelZoom = MaxZoom);
  ZoomOut1.Enabled:= not(CelZoom = MinZoom);
  FormResize(Sender);
end;

procedure TLife32MainForm.Helpindex1Click(Sender: TObject);
begin
  Resume(Sender);
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TLife32MainForm.Shortcutkeys1Click(Sender: TObject);
begin
  Resume(Sender);
  Application.HelpContext(9);
end;

procedure TLife32MainForm.RewindButtonClick(Sender: TObject);
var
  ASnapShot: TSnapShot;
  i: integer;
  Found: boolean;
  AnItem: TListItem;
begin
  try
    ASnapShot:= nil;
    //First make sure the list is sorted properly.
    {if (RewindList.Tag <> coPatternID) then} RewindList.CustomSort(nil,coPatternID);
    //Now start looking, just start at the end and work up from there.
    i:= RewindList.Items.count;
    Found:= false;
    while (i > 0) and not Found do begin
      Dec(i);
      ASnapShot:= RewindList.GetSnapShot(i);
      if (ASnapShot.PatternID <= LifeBox1.PatternID) then begin
        Found:= (ASnapShot.PatternID < LifeBox1.PatternID);
        if (not Found) and (ASnapShot.Revision <= LifeBox1.Revision) then begin
          Found:= (ASnapshot.Revision < LifeBox1.Revision);
          if not(Found) then Found:= (ASnapShot.Generation < LifeBox1.Generation);
        end; {if}
      end; {if}
    end; {while}

    OldIsPaused:= true;
    if Found then begin
      StopCounting;
      if Assigned(ASnapShot) then begin
        LifeBox1.RewindToSnapshot(ASnapShot);
        AnItem:= RewindList.Items[i];
        RewindList.ItemFocused:= AnItem;
      end; {if}
    end;
    Resume(Sender);

    PauseButtonClick(Sender);
    //Pause the action after menu closes.
    except begin
      //ShowMessage('Error in TLife32MainForm.RewindButtonClick');
      raise;
    end; {except}
  end; {try}
end;

//put the snapshot code in it's own routine with 1 parameter for
//the reason the snapshot was taken.

function TLife32MainForm.MakeSnapshot(Reason: Integer): boolean;
var
  Dummy: TSnapshot;
  ASnapShot: TSnapShot;
begin
  Result:= false;
  try
    Dummy:= LifeBox1.MakesnapshotDummy;
    if Assigned(Dummy) then try
      if not(RewindList.SnapshotExists(Dummy,Reason)) then begin
        //Give the program only x seconds to make a snapshot (x=2 by default).
        SnapshotTimer.Enabled:= not(NoSnapshotTimeout);
        ASnapShot:= LifeBox1.MakeSnapShotCopy;
        SnapshotTimer.Enabled:= false;
        if Assigned(ASnapshot) then begin
          if LifeBox1.Generation <> 0 then Reason:= Reason + cpLast + 1;
          RewindList.AddSnapshot(ASnapShot, Reason);
          //SkipToForm.RewindList2.AddSnapShot(ASnapshot,Reason);
          RewindButton.Enabled:= true;
          RewindToSnapshot1.Enabled:= true;
          Result:= true; //succes.
        end; {if}
      end;
      finally Dummy.Free;
    end; {try}
    except {ignore}
  end; {try}
  RewindList.Refresh;
end;

procedure TLife32MainForm.SnapshotButtonClick(Sender: TObject);
begin
  if MakeSnapShot(cpUser) then begin
    RewindButton.Enabled:= true;
    RewindToSnapshot1.Enabled:= true;
  end;
end;

procedure TLife32MainForm.RewindButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    Suspend(Sender);
    //LifeBox1.CanPaint:= cpDialogShowing;
    ShowSidePanel(TSSnapShots,0);
    //Don't resume yet, we want to do this *after* the box has closed.
  end;
end;

procedure TLife32MainForm.RewindList1Click(Sender: TObject);
var
  ASnapShot: TSnapShot;
begin
  if Assigned(RewindList.Selected) then with RewindList do begin
    ASnapShot:= GetSnapShot(Selected.Index);
    Resume(Sender);
    LifeBox1.RewindToSnapshot(ASnapShot);
    UpdateButtons;
  end; {if}
end;

procedure TLife32MainForm.LifeBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if suspended or (ActiveControl <> LifeBox1) or (not(LifeBox1.CanPlay)) then begin
    if not suspended then begin
      UpdateButtons;
    end;
    CloseDialogs;
    Resume(Sender);
  end;
end;

procedure TLife32MainForm.RewindList1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:= RewindList.Compare(Item1,Item2,Data);
end;

procedure TLife32MainForm.RewindList1Deletion(Sender: TObject; Item: TListItem);
begin
  if RewindList.Items.Count > 0 then RewindList.RemoveSnapShot(Item.Index);
end;

procedure TLife32MainForm.Delete1Click(Sender: TObject);
var
  TheList: TListView;
  TheIndex: integer;
begin
  TheList:= RewindList;
  while TheList.SelCount > 0 do begin
    TheIndex:= TheList.Selected.Index;
    RewindList.Items.Delete(TheIndex);
  end; {while}
  RewindButton.Enabled:= RewindList.Items.Count > 0;
  RewindToSnapShot1.Enabled:= RewindButton.Enabled;
  Resume(Sender);
end;

procedure TLife32MainForm.Rewindto1Click(Sender: TObject);
var
  TheList: TListView;
begin
  TheList:= RewindList;
  OnClick(TheList);
end;

procedure TLife32MainForm.N28Click(Sender: TObject);
var
  NewRule: string;
begin
  NewRule:= TMenuItem(Sender).caption;
  NewRule:= Copy(NewRule,1,Pos(#9,NewRule)-1);
  LifeBox1.Universe.RuleString:= NewRule;

  Resume(Sender);
end;

procedure TLife32MainForm.XEditDblClick(Sender: TObject);
begin
  MoveToButton.Click;
end;

procedure TLife32MainForm.GenEditDblClick(Sender: TObject);
begin
  if SkipToButton.Visible then SkipToButton.Click
  else CancelSkipButton.Click;
end;

procedure TLife32MainForm.Label2Click(Sender: TObject);
begin
  GenEditClick(GenEdit);
end;

procedure TLife32MainForm.Label5Click(Sender: TObject);
begin
  GenEditClick(XEdit);
end;

procedure TLife32MainForm.Label7Click(Sender: TObject);
begin
  GenEditClick(YEdit);
end;

procedure TLife32MainForm.RuleEditDblClick(Sender: TObject);
begin
  Rules1.Click;
end;

procedure TLife32MainForm.Label11Click(Sender: TObject);
begin
  GenEditClick(RuleEdit);
end;

procedure TLife32MainForm.SettingsButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Msg: TWMLButtonDown;
begin
  if Button = mbRight then with Msg do begin
    Msg:= WM_LButtonDown;
    Keys:= MK_LBUTTON	;
    xpos:= 0; ypos:= 0;
  end;
  SettingsPopupButton.Dispatch(Msg);
end;

procedure TLife32MainForm.Cancel2Click(Sender: TObject);
begin
  Resume(Sender);
end;

procedure TLife32MainForm.RewindPopupPopup(Sender: TObject);
var
  TheList: TListView;
begin
  //no suspend needed.
  TheList:= RewindList;
  ClearAll2.Enabled:= TheList.Items.Count > 0;
  RewindTo1.Enabled:= Assigned(TheList.Selected);
  Delete1.Enabled:= RewindTo1.Enabled;
end;

procedure TLife32MainForm.Zoomtofit1Click(Sender: TObject);
begin
  Resume(Sender);
  LifeBox1.ZoomToFit(DrawAgain, MaxZoomToFit);
end;

procedure TLife32MainForm.LifeBox1Enter(Sender: TObject);
begin
  Form1.SetFocus;
end;

procedure TLife32MainForm.FormShow(Sender: TObject);
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  CursorPos:= LifeBox1.ScreenToClient(CursorPos);
  if (LifeCel.MyDDSurface.DirectDrawEnabled = DDFast) then begin
    LifeBox1.PixelsPerCel:= LifeBox1.PixelsPerCel;
    DirectXImage.Visible:= true;
  end;
  LifeBox1.RedrawCursorArea(CursorPos.x,CursorPos.y);
  //move the selection back and force to force showing the current item.
  //SidePanelTabs.SelectNext(false);
  //SidePanelTabs.SelectNext(true);
end;

procedure TLife32MainForm.LifeBox1ChangePattern(Sender: TObject; Change: Integer);
const
  Edited: boolean = false;
  Rewinded:boolean = false;
begin
  if (LifeBox1.IsEmpty) and (LifeBox1.Generation = 0) then {do nothing}
  else begin
    //Reset the bounding rect for the moveto panel;
    MoveTo_BoundingRect:= Rect(0,0,0,0);
    case Change of
      cpDraw, cpSelect: if (not Edited) or (LifeBox1.Generation > 0) then begin
        Edited:= true;
        Rewinded:= false;
        if AutoRewindWhat[Change] then MakeSnapshot(Change);
        LifeBox1.Revision:= LifeBox1.Revision + 1;
        LifeBox1.Generation:= 0;
      end;
      cpClear, cpOpen: begin
        Edited:= false;
        Rewinded:= false;
        if AutoRewindWhat[Change] then MakeSnapshot(Change);
        LifeBox1.Revision:= 0;
        LifeBox1.Generation:= 0;
        LifeBox1.PatternID:= LifeBox1.MostRecentPatternID + 1;
        UpdateTorus;
      end;
      cpPlay, cpStep: begin
        Edited:= false;
        Rewinded:= false;
        if AutoRewindWhat[Change] then MakeSnapshot(Change)
        else if (Change = cpPlay) and (LifeBox1.Generation = 0) then MakeSnapshot(Change);
      end;
      cpRewind: begin
        if LifeBox1.Universe.PatternName <> '' then
          Caption:= LifeTitle + '- ['+LifeBox1.Universe.PatternName+']';
        if not Rewinded then begin
          Rewinded:= true;
          Edited:= false;
          if AutoRewindWhat[Change] then MakeSnapshot(Change);
        end;
      end;
      cpRotate: begin
        Edited:= true;
        Rewinded:= false;
        if AutoRewindWhat[Change] then MakeSnapshot(Change);
        LifeBox1.Revision:= LifeBox1.Revision + 1;
        LifeBox1.Generation:= 0;
      end;
      cpAuto: begin
        if AutoRewindWhat[Change] then MakeSnapshot(Change);
      end;
      cpTorus: begin
        UpdateTorus;
      end;
      else Assert(true,'TLife32MainForm.LifeBox1ChangePattern case not handled');
    end; {case}
  end;
end;

procedure TLife32MainForm.RewindList1ColumnClick(Sender: TObject;
  Column: TListColumn);
var
  SortBy: integer;
begin
  SortBy:= coPatternID;
  case Column.Index of
    0: SortBy:= coCause;
    1: SortBy:= coGeneration;
    2: SortBy:= coPatternID;
    3: SortBy:= coRevision;
    4: SortBy:= coName;
  end; {case}
  //reverse sortorder is pressed twice.
  if TListView(Sender).Tag = SortBy then SortBy:= -SortBy;
  TListView(Sender).CustomSort(nil,SortBy);
  TListView(Sender).Tag:= SortBy;
end;

procedure TLife32MainForm.LifeBox1RuleChange(Sender: TUniverse);
begin
  if (Sender is TUniverse) then begin
    RuleEdit.Text:= TUniverse(Sender).RuleString;
    RuleEdit.Font.Color:= clWindowText;
    OpenDialog1.CurrentRules:= TUniverse(Sender).RuleString;
    if (Sender <> LifeBox1.Universe) then
      LifeBox1.Universe.Neighborhood:= Sender.Neighborhood;
      LifeBox1.Universe.RuleString:= Sender.RuleString;
    //update revision, so snapshotlist works OK.
    LifeBox1.Revision:= LifeBox1.Revision + 1;
  end
  else //Raise Exception.Create('Error in LifeBox1RuleChange');
end;

procedure TLife32MainForm.LifeBox1ShowDrag(Sender: TObject; DragOffset: TPoint);
var
  XStr, YStr: string;
begin
  with DragOffset do begin
    XStr:= IntToStr(x);
    if x >= 0 then XStr:= '+'+XStr;
    XEdit.Text:= XStr;
    YStr:= IntToStr(y);
    if y >= 0 then YStr:= '+'+YStr;
    YEdit.Text:= YStr;
  end; {with}
end;

procedure TLife32MainForm.OpenDialog1OpenToClipboard(Sender: TObject);
var
  ClipboardUniverse: TUniverse;
  LifeLines: TStringList;
begin
  ClipboardUniverse:= TUniverse.Create('',nbDefault);
  try
    ClipboardUniverse.LoadFromFile(OpenDialog1.Filename);
    ClipboardUniverse.ResetBoundingBox; //Select all
    LifeLines:= ClipboardUniverse.SaveToStringList(smDefault, false);
    try
      Clipboard.SetTextBuf(LifeLines.GetText);
      finally LifeLines.Free;
    end; {try}
    finally ClipboardUniverse.Free;
  end; {try}
end;

procedure TLife32MainForm.ItemBox1Click(Sender: TObject);
begin
  try
    LifeBox1.DrawBox;
    finally Resume(Sender);
  end; {try}
  UpdateButtons;
end;

procedure TLife32MainForm.SaveDialog1TypeChange(Sender: TObject);
var
  Filename: string;
  FileExt: string;
begin
  Filename:= SaveDialog1.FileName;
  FileExt:= ExtractFileExt(Filename);
  FileName:= Copy(FileName,1,Length(FileName)-Length(FileExt));
  case SaveDialog1.FilterIndex of
    smDefault, smLife105, smLife106: FileExt:= cstrDefaultExt;
    smRLE: fileext:= '.rle';
    smDBLife: FileExt:= '.l';
    smXLife20: FileExt:= '.xli';
    smBitmap: FileExt:= cstrBitmapExt;
    smGif: FileExt:= cstrGifExt;
  end; {case}
  FileName:= FileName + FileExt;
  SaveDialog1.FileName:= FileName;
  SaveDialog1.DefaultExt:= FileExt;
end;

procedure TLife32MainForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  //if ComServer.ObjectCount > 1 then begin
  //  CanClose:= false;
  //  Visible:= false;
  //end;
  //LifeApp.ObjRelease;
  //LifeApp.Free;
end;

procedure TLife32MainForm.Opennextfile1Click(Sender: TObject);
var
  LastFile: string;
  NextFile: string;
begin
  LastFile:= GetLastFile;
  if LastFile <> '' then with FileListBox1 do begin
    ApplyFilePath(LastFile);
    NextFile:= Items[((ItemIndex+1) mod Items.count)];
    NextFile:= ExtractFilePath(LastFile) + NextFile;
    OpenFile(NextFile);
    Resume(Sender);
  end;
end;

procedure TLife32MainForm.LifeBox1SaveProgress(Sender: TObject;
  Progress: Integer; var Cancel: boolean);
begin
  ProgressBar1.Progress:= Progress;
  if GetASyncKeyState(vk_escape) <> 0 then Cancel:= true;
end;

procedure TLife32MainForm.RunBenchmark(NumGens: integer;
                          ShowProgress, CheckESC, ShowDisplay: boolean);
var
	i: integer;
  ProgressUpdate: integer;
  ProgressCount: integer;
	TimeBefore: integer;
  TimeAfter: integer;
  TimeBeforeProgress: integer;
  TimeAfterProgress: integer;
begin
  PauseButtonClick(Self);
  LifeBox1ChangePattern(Self,cpPlay); //force auto-snapshot.

  GenEdit.Text:= 'Wait...';
  GenEdit.Refresh;
  ProgressUpdate:= 0;
  ProgressCount:= 0;

  if ShowProgress then begin
    ProgressBar1.MinValue:= 0; //just to avoid exceptions.
    ProgressBar1.MaxValue:= NumGens;
    ProgressBar1.Visible:= true;
    ProgressUpdate:= NumGens div 100;
    if progressUpdate = 0 then ProgressUpdate:= 1;
    ProgressCount:= ProgressUpdate;
  end;

  TimeBeforeProgress:= MyGetTickCount;
  TimeBefore:= MyGetTickCount;
  if ShowDisplay then begin
    for i:= 0 to NumGens-1 do begin
      Animate;
      if ShowProgress then begin
        dec(ProgressCount);
        if ProgressCount = 0 then begin
          ProgressCount:= ProgressUpdate;
          TimeAfterProgress:= MyGetTickCount;
          //only refresh progress every 100 millisec.
          if (TimeAfterProgress - TimeBeforeProgress) > 200 then begin
            ProgressBar1.Progress:= i;
            ProgressBar1.Refresh;
            TimeBeforeProgress:= TimeAfterProgress;
          end; {if}
        end; {if}
      end; {if}
      if CheckESC then if GetAsyncKeyState(vk_escape) <> 0 then break
    end; {for i}
  end {if}
  else {if not(ShowDisplay} begin
    for i:= 0 to NumGens-1 do begin
      LifeBox1.Generate(GoForward);
      LifeBox1.Universe.ClearDisplay; //update the displaylist.
      if ShowProgress then begin
        dec(ProgressCount);
        if ProgressCount = 0 then begin
          ProgressCount:= ProgressUpdate;
          ProgressBar1.Progress:= i;
          ProgressBar1.Refresh;
        end; {if}
      end; {if}
      if CheckESC then if GetAsyncKeyState(vk_escape) <> 0 then break
    end; {for i}
  end; {else}
  TimeAfter:= MyGetTickCount;

  LifeBox1.CanPaint:= cpDialogShowing;
  //ShowMessage('The time in milliseconds is: '+IntToStr(TimeAfter-TimeBefore));
  //one last refresh to get it at 100%
  ProgressBar1.Progress:= i;
  ProgressBar1.Refresh;
  ProgressBar1.Visible:= false;
  GenEdit.Refresh;
  UpdateButtons;
  //show the benchmark dialog again, now with the time.
  RunBenchMark1.Tag:= (TimeAfter - TimeBefore);
  RunBenchMark1.Click;
  LifeBox1.CanPaint:= cpCanPaint;
end;

procedure TLife32MainForm.RunBenchmark1Click(Sender: TObject);
var
  BenchMarkForm: TBenchMarkForm;
  LetsGo: boolean;
  NumGens1: integer;
  ShowProgress1,CheckEsc1,ShowDisplay1: boolean;
begin
  Suspend(Sender);
  LifeBox1.CanPaint:= cpDialogShowing;
  LetsGo:= false;
  NumGens1:= 0; ShowProgress1:= false;
  CheckEsc1:= false; ShowDisplay1:= false;
  try
		BenchMarkForm:= TBenchMarkForm.Create(Self);
	  OpenedDialog:= BenchMarkForm;
    BenchmarkForm.BenchmarkMillisecs:= RunBenchmark1.Tag;
    case BenchMarkForm.ShowModal of
      mrOK: with BenchMarkForm do begin
        LetsGo:= true;
        NumGens1:= Numgens;
        ShowProgress1:= ShowProgress;
        CheckEsc1:= CheckEsc;
        ShowDisplay1:= ShowDisplay;
      end;
    end; {case}
	  BenchMarkForm.Free;
	  OpenedDialog:= nil;
    finally Resume(Sender);
    if LetsGo then RunBenchMark(NumGens1,ShowProgress1,CheckEsc1,ShowDisplay1);
  end;
  RunBenchMark1.Tag:= 0;
end;

procedure TLife32MainForm.RedrawButtonClick(Sender: TObject);
begin
  Resume(Sender);
  LifeBox1.RedrawAll;
end;

procedure TLife32MainForm.Clearoutsideselection1Click(Sender: TObject);
begin
  WaitCursor;
  Suspend(Sender);
  try
	  LifeBox1.ClearOutsideSelection;
    LifeBox1.CenterOnPattern(false);
    UpdateButtons;
    finally begin
	    DefaultCursor;
	    Resume(Sender);
    end; {finally}
  end; {try}
end;

procedure TLife32MainForm.LeftSplitterMoved(Sender: TObject);
begin
  if (LeftPanel.Width < 40) and (LeftPanel.Visible) then begin
    HideSidePanel;
  end
  else begin
    DriveComboBox1.Width:= Panel1.Width-2;
    FilterComboBox1.Width:= Panel2.Width-2;
    LeftPanel.Tag:= LeftPanel.Width;
  end;
  if LeftPanel.Width > DefaultSidePanelSize + 20 then Panel11.Width:= 40
  else Panel11.Width:= 20;
end;

procedure TLife32MainForm.Splitter2Moved(Sender: TObject);
begin
  LeftPanel.Visible:= true;
  LeftSplitter.Visible:= true;
  LeftPanel.Width:= 1;
end;

procedure TLife32MainForm.FileListBox1Click(Sender: TObject);
begin
  Suspend(Self);
  OpenFile(FileListBox1.FileName);
  LifeBox1.SetFocus;
  Resume(Self);
end;

function TLife32MainForm.GetMyDocumentsPath: string;
// Replace CSIDL_HISTORY with the constants below
var
  Allocator: IMalloc;
  SpecialDir: PItemIdList;
  FBuf: array[0..MAX_PATH] of Char;
begin
  if SHGetMalloc(Allocator) = NOERROR then begin
    SHGetSpecialFolderLocation(Self.Handle, CSIDL_PERSONAL, SpecialDir);
    SHGetPathFromIDList(SpecialDir, @FBuf[0]);
    Allocator.Free(SpecialDir);
    Result:= FBuf;
  end;
end;

procedure TLife32MainForm.SidePageControlChange(Sender: TObject);
begin
  SidePanelTabs.TabIndex:= SidePageControl.ActivePage.PageIndex;
  if SidePageControl.ActivePage = TSOpen then try
    DirectoryListbox1.Directory:= ExtractFileDir(GetLastFile);
  except {ignore}
    DirectoryListbox1.Directory:= GetMyDocumentsPath;
  end;
  if SidePageControl.ActivePage = TSPattern then begin
    if ArrangeIcons1.Checked then ArrangePatternList;
  end;
end;

procedure TLife32MainForm.ClearAll2Click(Sender: TObject);
var
  TheList: TListView;
  TheIndex: integer;
begin
  TheList:= RewindList;
  while TheList.Items.Count > 0 do begin
    TheIndex:= 0;
    RewindList.Items.Delete(TheIndex);
    //SkipToForm.RewindList2.Items.Delete(TheIndex);
  end; {while}
  RewindButton.Enabled:= RewindList.Items.Count > 0;
  RewindToSnapShot1.Enabled:= RewindButton.Enabled;
  Resume(Sender);
end;

procedure TLife32MainForm.CursorZoomImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PopupPoint: TPoint;
begin
  //if Button = mbRight then begin
    PopupPoint.x:= x;
    PopupPoint.y:= y;
    PopupPoint:= TControl(Sender).ClientToScreen(PopupPoint);
    with PopupPoint do CursorModePopup.Popup(x,y);
    Resume(Sender);
  //end;
end;

procedure TLife32MainForm.PasteErrorImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PopupPoint: TPoint;
begin
  //if Button = mbRight then begin
    PopupPoint.x:= x;
    PopupPoint.y:= y;
    PopupPoint:= TControl(Sender).ClientToScreen(PopupPoint);
    with PopupPoint do PasteModePopup.Popup(x,y);
    Resume(Sender);
  //end;
end;

procedure TLife32MainForm.PanelBottomResize(Sender: TObject);
begin
  ScrollbarLR.Width:= (PanelBottomScrollbar.Width - PanelRight.Width);
end;

procedure TLife32MainForm.Zoomtoselection1Click(Sender: TObject);
begin
  Resume(Sender);
  LifeBox1.ZoomToSelection(MaxZoomToFit);
end;

procedure TLife32MainForm.Splitter1Moved(Sender: TObject);
begin
  ScrollbarLR.Width:= PanelBottomScrollbar.Width - PanelRight.Width;
end;

procedure TLife32MainForm.UniverseTabsetDrawTab(Sender: TObject;
  TabCanvas: TCanvas; R: TRect; Index: Integer; Selected: Boolean);
begin
  if Selected then begin
    TabCanvas.Font.Style:= TabCanvas.Font.Style + [fsBold];
    UniverseTabRenamer.Left:= R.Left + UniverseTabSetPanel.Left;
  end
  else TabCanvas.Font.Style:= TabCanvas.Font.Style - [fsBold];
  TabCanvas.TextOut(R.Left,R.Top+1,UniverseTabSet.Tabs[Index]);
end;

procedure TLife32MainForm.UniverseTabsetMeasureTab(Sender: TObject;
  Index: Integer; var TabWidth: Integer);
begin
  with UniverseTabset do begin
    if Index = TabIndex then Canvas.Font.Style:= Canvas.Font.Style + [fsBold]
    else Canvas.Font.Style:= Canvas.Font.Style - [fsBold];
    TabWidth:= Canvas.TextWidth(Tabs[Index]);
  end; {with}
end;

procedure TLife32MainForm.UniverseTabsetChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
var
  NewUniverse: TUniverse;
  OldUniverse: TUniverse;
  NewRewindList: TSnapshotList;
begin
  PauseButton.Click;
  UniverseTabRenamer.Visible:= false;
  with UniverseTabset do begin
    OldUniverse:= Tabs.Objects[TabIndex] as TUniverse;
    if OldUniverse = nil then begin
      OldUniverse:= LifeBox1.Universe;
      OldUniverse.AddRef;
      Tabs.Objects[TabIndex]:= OldUniverse;
    end;
    //Tabs.Objects[TabIndex]:= LifeBox1.Universe;
    NewUniverse:= Tabs.Objects[NewTab] as TUniverse;
    if NewUniverse = nil then begin
      NewUniverse:= LifeBox1.NewUniverse;
      NewUniverse.AddRef;
      Tabs.Objects[NewTab]:= NewUniverse;
    end;
  end;  {with}
  LifeBox1.Universe:= NewUniverse;
  LifeBox1.CancelSelection;
  LifeBox1.RedrawAll;
  //The rewindlists and the universes are synchronized.
  RewindList.Visible:= false;
  NewRewindList:= TSnapshotList(RewindListList[NewTab]);
  NewRewindList.Visible:= true;
  RewindList:= NewRewindList;
  UpdateTabButtons(NewTab);
  UpdateButtons;
end;

procedure TLife32MainForm.Newsheet1Click(Sender: TObject);
var
  ARewindList: TSnapshotList;
  AIndex: integer;
begin
  with UniverseTabset do begin
    AIndex:= TabIndex;
    Tabs.Insert(AIndex,'Sheet'+IntToStr(Tabs.Count+1));
    ARewindList:= NewRewindList;
    RewindListList.Insert(AIndex,ARewindList);
    TabIndex:= TabIndex - 1;
  end;
  Resume(sender);
end;

procedure TLife32MainForm.Delete2Click(Sender: TObject);
var
  OldTab: Integer;
  OldUniverse: TUniverse;
begin
  PauseButton.Click;
  StopCounting;
  with UniverseTabset do begin
    //first change the selection.
    OldTab:= TabIndex;
    OldUniverse:= Tabs.Objects[TabIndex] as TUniverse;
    UniverseTabset.SelectNext(true);
    Tabs.Delete(OldTab);
    RewindListList.Delete(OldTab);
    repeat {do nothing}
    until not(OldUniverse.Locked);
    OldUniverse.Free;
  end;
  resume(sender);
end;

procedure TLife32MainForm.LetUserChangeTabName(ATabset: TNewTabset);
begin
  with UniverseTabRenamer do begin
    Text:= ATabset.Tabs[ATabset.TabIndex];
    Font.Style:= Font.Style + [fsBold];
    Visible:= true;
    Setfocus;
    SelectAll;
  end; {with}
end;

procedure TLife32MainForm.UniverseTabsetMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  OldClickTime: integer = 0;
var
  NewClickTime: integer;
  DoubleClickTime: integer;
begin
  UniverseTabset.Tag:= 1;
  //implement a 'double click to edit name scheme'
  NewClickTime:= GetTickCount;
  DoubleClickTime:= 300;
  if (NewClickTime - OldClickTime) < DoubleClickTime then begin
    //UniverseTabRenamer.Left:= ... already set in Ownerdraw item.
    LetUserChangeTabName(UniverseTabset);
  end; {if}
  OldClickTime:= NewClickTime;
end;

procedure TLife32MainForm.UniverseTabRenamerKeyPress(Sender: TObject;
  var Key: Char);
begin
  UniverseTabset.Tabs[UniverseTabset.TabIndex]:= UniverseTabRenamer.Text;
  if Key = #13 then begin
    UniverseTabRenamer.Visible:= false;
    LifeBox1.SetFocus;
  end; {if}
end;

procedure TLife32MainForm.UniverseTabRenamerExit(Sender: TObject);
var
  AKey: Char;
begin
  //act as if the enter key is pressed.
  AKey:= #13;
  with UniverseTabRenamer do if Assigned(OnKeyPress) then OnKeyPress(Sender,AKey);
end;

procedure TLife32MainForm.UniverseTabRenamerChange(Sender: TObject);
begin
  with UniverseTabRenamer do begin
    Width:= UniverseTabSet.Canvas.TextWidth(Text) + 22;
    ScrollBy(100,0);
  end;
end;

procedure TLife32MainForm.UniverseTabsetDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source is TNewTabset then Accept:= true;
  //if Source is
end;

procedure TLife32MainForm.UniverseTabsetDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  DragItem: integer;
  BeforeItem: integer;
begin
  if Source = UniverseTabset then with UniverseTabset do begin
    DragItem:= Tag;
    BeforeItem:= ItemAtPos(Point(x,y));
    if BeforeItem = -1 then begin
      BeforeItem:= 0;
      if x > 10 then BeforeItem:= Tabs.Count-1;
    end; {if}
    if DragItem <> BeforeItem then begin
      Tabs.Move(DragItem,BeforeItem);
    end; {if}
  end; {if with}
end;

procedure TLife32MainForm.UniverseTabsetStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  BeforeItem: integer;
  APoint: TPoint;
begin
  with UniverseTabset do begin
    GetCursorPos(APoint);
    BeforeItem:= ItemAtPos(ScreenToClient(APoint));
    if BeforeItem = -1 then begin
      BeforeItem:= 0;
      if APoint.x > 10 then BeforeItem:= Tabs.Count-1;
    end; {if}
    Tag:= BeforeItem;
  end; {with}
end;

//Torus part of the code **********************************************


procedure TLife32MainForm.TSTorusEnter(Sender: TObject);
begin
  with LifeBox1.Limit do begin
    if TopEdit.Text = '' then TopEdit.Text:= IntToStr(Top);
    if LeftEdit.Text = '' then LeftEdit.Text:= IntToStr(Left);
    if RightEdit.Text = '' then RightEdit.Text:= IntToStr(Right - Left +1);
    if BottomEdit.Text = '' then BottomEdit.Text:= IntToStr(Bottom - Top +1);
  end; {with}
  LimitNSButton.Down:= LifeBox1.TorusKind = tk_UpDown;
  LimitEWButton.Down:= LifeBox1.TorusKind = tk_LeftRight;
  LimitNESWButton.Down:= LifeBox1.TorusKind = tk_All;
  //
  DeadEdgesUniverseButton.Down:= LifeBox1.DeadEdges;
  CircularUniverseButton.Down:= not(LifeBox1.DeadEdges);
  //
  CBIsLimited.Checked:= LifeBox1.IsLimited;
  CBIsLimitedClick(Self);
end;

procedure TLife32MainForm.TopEditChange(Sender: TObject);
var
  i: integer;
begin
  inherited;
  with Sender as TEdit do try
    i:= StrToInt(Text);
    if (i > MaxX) or (i < MinX) then raise Exception.Create('Out of Range');
    Font.Color:= clBlue;
    if LifeBox1.IsLimited then OKButton.Enabled:= true;
    except Font.Color:= clRed;
  end; {try}
end;

procedure TLife32MainForm.SBTopDownClick(Sender: TObject);
var
  i: integer;
begin
  with (Sender as TSpinButton) do try
    i:= StrToInt(TEdit(FocusControl).Text);
    if Sender = SBTop then Inc(i) else Dec(i);
    if i < MinX then i:= MaxX
    else if i > MaxX then i:= MinX;
    TEdit(FocusControl).Text:= IntToStr(i);
    except ; //ignore errors
  end; {try}
end;

procedure TLife32MainForm.SBTopUpClick(Sender: TObject);
var
  i: integer;
begin
  with (Sender as TSpinButton) do try
    i:= StrToInt(TEdit(FocusControl).Text);
    if Sender = SBTop then Dec(i) else Inc(i);
    if i < MinX then i:= MaxX
    else if i > MaxX then i:= MinX;
    TEdit(FocusControl).Text:= IntToStr(i);
    except ; //ignore errors
  end; {try}
end;

procedure TLife32MainForm.TopEditKeyPress(Sender: TObject; var Key: Char);
var
  i: integer;
  accept: boolean;
  ARect: TRect;
  NewLimit: TRect;
begin
  with TEdit(Sender) do begin
    if (key = '-') then begin
      if (SelStart <> 0) then key:= #0;
      if (Sender = RightEdit) or (Sender = BottomEdit) then Key:= #0;
      if (SelLength <> Length(Text)) and (Pos('-',Text) <> 0) then key:= #0;
    end {if}
    else if not CharInSet(key,['0'..'9',#0..#31]) then key:= #0;
    if CharInSet(key,[#13,#10]) then begin
      ARect:= LifeBox1.Limit;
      with ARect do try
        i:= StrToInt(Text);
        if Sender = TopEdit then Top:= i
        else if Sender = LeftEdit then Left:= i
        else if Sender = RightEdit then Right:= i
        else if Sender = BottomEdit then Bottom:= i;
        LifeBox1.Limit:= ARect;
        NewLimit:= LifeBox1.Limit;
        accept:= CompareMem(@ARect,@NewLimit,SizeOf(ARect));
        if accept then Font.Color:= clWindowText;
      except Font.Color:= clRed;
      end; {with try}
    end;
  end; {with}
end;

procedure TLife32MainForm.OKButtonClick(Sender: TObject);
var
  ARect: TRect;
  accept: boolean;
begin
  with ARect do begin
    accept:= true;
    try
      Left:= StrToInt(LeftEdit.Text);
      LeftEdit.Font.Color:= clWindowText;
      except accept:= false; LeftEdit.Font.Color:= clRed;
    end; {try}
    try
      Top:= StrToInt(TopEdit.Text);
      TopEdit.Font.Color:= clWindowText;
      except accept:= false; TopEdit.Font.Color:= clRed;
    end; {try}
    try
      Right:= StrToInt(RightEdit.Text) + Left - 1;
      RightEdit.Font.Color:= clWindowText;
      except accept:= false; RightEdit.Font.Color:= clRed;
    end; {try}
    try
      Bottom:= StrToInt(BottomEdit.Text) + Top - 1;
      BottomEdit.Font.Color:= clWindowText;
      except accept:= false; BottomEdit.Font.Color:= clRed;
    end; {try}
  end; {with}
  if accept then begin
    PauseButton.Click;
    LifeBox1.Limit:= ARect;
    OKButton.Enabled:= false;
    CBIsLimitedClick(OKButton);
    LifeBox1.RedrawAll;
  end;
end;

procedure TLife32MainForm.Image3Click(Sender: TObject);
begin
  SBTopUpClick(SBTop);
end;

procedure TLife32MainForm.Image4Click(Sender: TObject);
begin
  SBTopUpClick(SBBottom);
end;

procedure TLife32MainForm.Image7Click(Sender: TObject);
begin
  SBTopDownClick(SBLeft);
end;

procedure TLife32MainForm.Image5Click(Sender: TObject);
begin
  SBTopUpClick(SBRight);
end;

procedure TLife32MainForm.CBIsLimitedClick(Sender: TObject);
var
  ARect: TRect;
begin
  Label9.Visible:= CBIsLimited.Checked;
  Image8.Visible:= CBIsLimited.Checked;
  with ARect do begin
    try Top:= StrToInt(TopEdit.Text) except Top:= 0; end;
    try Left:= StrToInt(LeftEdit.Text) except Left:= 0; end;
    try Right:= Left - 1 + StrToInt(RightEdit.Text) except Right:= 0; end;
    try Bottom:= Top -1 + StrToInt(BottomEdit.Text) except Bottom:= 0; end;
  end;
  TorusUpdateOff:= true;
  LifeBox1.DeadEdges:= DeadEdgesUniverseButton.Down;
  if LimitNESWButton.Down then LifeBox1.TorusKind:= tk_All
  else if LimitEWButton.Down then LifeBox1.TorusKind:= tk_LeftRight
  else LifeBox1.TorusKind:= tk_UpDown;
  TorusUpdateOff:= false;
  LifeBox1.Limit:= ARect; (**)
  PauseButton.Click;
  LifeBox1.IsLimited:= CBIsLimited.Checked;
end;

procedure TLife32MainForm.UpdateTorus;
var
  ARect: TRect;
begin
  if not TorusUpdateOff then begin
    with LifeBox1 do begin
      DeadEdgesUniverseButton.Down:= LifeBox1.DeadEdges;
      CircularUniverseButton.Down:= not(LifeBox1.DeadEdges);

      LimitNESWButton.Down:= TorusKind = tk_All;
      LimitEWButton.Down:= TorusKind = tk_LeftRight;
      LimitNSButton.Down:= TorusKind = tk_UpDown;

      if IsMyRectEmpty(Limit) then ARect:= SelectionRect else ARect:= Limit;
      TopEdit.Text:= IntToStr(ARect.Top);
      LeftEdit.Text:= IntToStr(ARect.Left);
      RightEdit.Text:= IntToStr(ARect.Right - ARect.Left + 1);
      BottomEdit.Text:= IntToStr(ARect.Bottom - ARect.Top + 1);

      CBIsLimited.Checked:= IsLimited;
    end; {with}
  end;
end;


//Torus code ends.. here ************************

//MoveTo code goes -err.- below here ************

procedure TLife32MainForm.NWMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  MiddleX, MiddleY: integer;
  x1,y1: integer;
begin
  if not EditedByUser then begin
    if IsMyRectEmpty(MoveTo_BoundingRect) then begin
      MoveTo_BoundingRect:= LifeBox1.Universe.GetBoundingBox;
    end;
    with MoveTo_BoundingRect do begin
      MiddleX:= (Right + Left) div 2;
      MiddleY:= (Top + Bottom) div 2;
      case TControl(Sender).Tag of
        idN: begin X1:= MiddleX; Y1:=Top; end;
        idNW:begin X1:= Left; Y1:=Top; end;
        idW: begin X1:= Left; Y1:=MiddleY; end;
        idSW:begin X1:= Left; Y1:=Bottom-1; end;
        idS: begin X1:= MiddleX; Y1:=Bottom-1; end;
        idSE:begin X1:= Right-1; Y1:=Bottom-1; end;
        idE: begin X1:= Right-1; Y1:=MiddleY; end;
        idNE:begin X1:= Right-1; Y1:=Top; end;
        idCenter:begin X1:= MiddleX; Y1:=MiddleY; end;
        idNulCenter: begin X1:= 0; Y1:= 0; end;
        else begin x1:= 0; y1:= 0; end;
      end; {case}
      if X1 > MaxX then x1:= x1 or $FFFF0000;
      if y1 > MaxX then y1:= y1 or $FFFF0000;
      if x1 < MinX then x1:= x1 and $0000ffff;
      if y1 < MinY then y1:= y1 and $0000ffff;
      UpdatePosition(x1,y1);
    end; {with}
  end;
end;

procedure TLife32MainForm.NWClick(Sender: TObject);
var
  MiddleX, MiddleY: integer;
begin
  MoveTo_BoundingRect:= LifeBox1.Universe.GetBoundingBox;
  with MoveTo_BoundingRect do begin
    MiddleX:= (Right + Left) div 2;
    MiddleY:= (Top + Bottom) div 2;
    case TControl(Sender).Tag of
      idN: MoveTo(MiddleX,Top);
      idNW: MoveTo(Left,Top);
      idW:MoveTo(Left,MiddleY);
      idSW: MoveTo(Left,Bottom);
      idS: MoveTo(MiddleX,Bottom);
      idSE:MoveTo(Right,Bottom);
      idE: MoveTo(Right,MiddleY);
      idNE:MoveTo(Right,Top);
      idCenter: MoveTo(MiddleX,MiddleY);
      idNulCenter: MoveTo(0,0);
    end; {case}
  end; {with}
end;

procedure TLife32MainForm.InfoMemoChange(Sender: TObject);
var
  OldOnChange: TNotifyEvent;
begin
  //temporally disable change event, so we do not get caught in an endless loop.
  if ActiveControl = Sender then begin
    OldOnChange:= LifeBox1.Universe.Description.OnChange;
    LifeBox1.Universe.Description.OnChange:= nil;
    LifeBox1.Universe.Description.Assign(InfoMemo.Lines);
    LifeBox1.Universe.Description.OnChange:= OldOnChange;
  end;
end;

procedure TLife32MainForm.LifeBox1Click(Sender: TObject);
begin
  if ScrollbarLR.CanFocus then ScrollbarLR.SetFocus
  else LifeBox1.SetFocus;
end;

procedure TLife32MainForm.LifeBox1AfterInfoChange(Sender: TObject);
var
  OldOnChange: TNotifyEvent;
begin
  //disable infomemo's OnChange, so the change does not do in a loop.
  OldOnChange:= InfoMemo.OnChange;
  InfoMemo.OnChange:= nil;
  InfoMemo.Lines.Text:= LifeBox1.Universe.Description.Text;
  InfoMemo.OnChange:= OldOnChange;
end;

procedure TLife32MainForm.InfoMemoPopupPopup(Sender: TObject);
begin
  if InfoMemoPopup.PopupComponent = nil then
    InfoMemoPopup.PopupComponent:= InfoMemo;
  with InfoMemoPopup.PopupComponent as TRichEdit do begin
    InfoMenuCut.Enabled:= SelLength > 0;
    InfoMenuCopy.Enabled:= InfoMenuCut.Enabled;
    InfoMenuPaste.Enabled:= Clipboard.HasFormat(CF_Text);
    Lexicon1.Visible:= (InfoMemoPopup.PopupComponent = InfoMemo) or
                       (InfoMemoPopup.PopupComponent = LexiconMemo);
    N37.Visible:= Lexicon1.Visible;
  end;
end;

procedure TLife32MainForm.InfoMenuCutClick(Sender: TObject);
begin
  with InfoMemoPopup.PopupComponent as TRichEdit do CutToClipboard;
end;

procedure TLife32MainForm.InfoMenuCopyClick(Sender: TObject);
begin
  with InfoMemoPopup.PopupComponent as TRichEdit do CopyToClipboard;
end;

procedure TLife32MainForm.InfoMenuPasteClick(Sender: TObject);
begin
  with InfoMemoPopup.PopupComponent as TRichEdit do PasteFromClipboard;
end;

procedure TLife32MainForm.InfoMenuFontClick(Sender: TObject);
begin
  with InfoMemoPopup.PopupComponent as TRichEdit do begin
    FontDialog1.Font.Assign(Font);
    if FontDialog1.Execute then begin
      Font.Assign(FontDialog1.Font);
    end; {if}
  end; {with}
end;


procedure TLife32MainForm.DirectoryListBox1Change(Sender: TObject);
const
  OldDir: string = '';
var
  i: integer;
  Filename: string;
begin
  if (DirectoryListBox1.Directory <> OldDir) or (Sender = FilterComboBox1) then begin
    ListView1.Items.Clear;
    i:= 0;
    while i < FileListBox1.Items.Count do begin
      Filename:= FileListBox1.Items[i];
      ListView1.Items.Add.Caption:= Proper(Filename);
      inc(i);
    end;
  end;
  OldDir:= DirectoryListBox1.Directory;
end;

procedure TLife32MainForm.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ListView1.AlphaSort;
  ListView1.Tag:= -ListView1.Tag;
end;

procedure TLife32MainForm.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  if Item1.Caption > Item2.Caption then Compare:= 1
  else if Item1.Caption < Item2.Caption then Compare:= -1
  else Compare:= 0;
  Compare:= Compare * ListView1.Tag;
end;

procedure TLife32MainForm.ListView1Click(Sender: TObject);
var
  AFileName: string;
begin
  if Assigned(ListView1.Selected) then begin
    AFileName:= DirectoryListBox1.Directory+'\'+ListView1.Selected.Caption;
    Suspend(Self);
    OpenFile(AFileName);
    LifeBox1.SetFocus;
    Resume(Self);
  end;
end;

procedure TLife32MainForm.DirectoryListBox1Click(Sender: TObject);
begin
  DirectoryListBox1.OpenCurrent;
end;

procedure TLife32MainForm.Rename1Click(Sender: TObject);
begin
  LetUserChangeTabName(UniverseTabset);
  resume(sender);
end;

procedure TLife32MainForm.LifeBox1SelectionChange(Sender: TObject);
const
  SnapLarge = 1;
  SnapSmall = 2;
  NoSnap = 0;
var
  SnapMode: integer;
  SnapRect: TRect;
begin
  if RBSnapSmall.Checked then Snapmode:= SnapSmall
  else if RBSnapLarge.Checked then Snapmode:= SnapLarge
  else Snapmode:= NoSnap;
  case SnapMode of
    SnapSmall: begin
      SnapRect:= LifeBox1.SelectionRect;
      if not(IsMyRectEmpty(SnapRect)) then begin
        Dec(SnapRect.Top); Dec(SnapRect.Left);
      end;
    end;
    SnapLarge: begin
      SnapRect:= LifeBox1.OrgSelectionRect;
      Dec(SnapRect.Right); Dec(SnapRect.Bottom);
    end;
    NoSnap: SnapRect:= Rect(0,0,0,0);
  end; {case}
  if not(IsMyRectEmpty(SnapRect)) then with LifeBox1, SnapRect do begin
    TopEdit.Text:= IntToStr(Top);
    if Top <> Limit.Top then TopEdit.Font.Color:= clBlue
    else TopEdit.Font.Color:= clWindowText;
    LeftEdit.Text:= IntToStr(Left);
    if Left <> Limit.Left then LeftEdit.Font.Color:= clBlue
    else LeftEdit.Font.Color:= clWindowText;
    RightEdit.Text:= IntToStr(Right - Left + 1);
    if Right <> Limit.Right then RightEdit.Font.Color:= clBlue
    else RightEdit.Font.Color:= clWindowText;
    BottomEdit.Text:= IntToStr(Bottom - Top + 1);
    if Bottom <> Limit.Bottom then BottomEdit.Font.Color:= clBlue
    else BottomEdit.Font.Color:= clWindowText;
  end; {if with}
end;

procedure TLife32MainForm.RBSnapSmallClick(Sender: TObject);
begin
  if Assigned(LifeBox1.OnSelectionChange) then LifeBox1.OnSelectionChange(Self);
end;

procedure TLife32MainForm.CircularUniverseButtonClick(Sender: TObject);
var
  ATorusKind: integer;
begin
  LifeBox1.DeadEdges:= DeadEdgesUniverseButton.down;
  if LimitNESWButton.Down then ATorusKind:= tk_All
  else if LimitEWButton.Down then ATorusKind:= tk_LeftRight
  else ATorusKind:= tk_UpDown;
  LifeBox1.TorusKind:= ATorusKind;
end;

procedure TLife32MainForm.FillCBLexiconFind;
var
  i: integer;
  KeyWord: string;
  ALine: string;
  ALine2: PChar;
  AChar: Char;
begin
  AChar:= ':';
  with CBLexiconFind do begin
    i:= 0;
    while i < Lexicon.Count do begin
      if Lexicon[i] <> '' then begin
        ALine:= Lexicon[i];
        ALine2:= PChar(ALine);
        KeyWord:= AnsiExtractQuotedStr(ALine2,AChar);
      end;
      if KeyWord <> '' then begin
        CBLexiconFind.Items.Add(Keyword);
        KeyWord:= '';
      end;
      Inc(i);
    end; {while}
  end;
end;

function TLife32MainForm.FindKeywordInLexicon(AKeyword: string): string;
var
  i: integer;
  FirstLine, LastLine: integer;
  Found: boolean;

function WithoutTabs(ALine: string): string;
var
  i: integer;
begin
  Result:= ALine;
  i:= Length(Result);
  while i > 0 do begin
    if CharInSet(Result[i],TabsEtc) then Result[i]:= #32;
    Dec(i);
  end;
  Result:= Trim(ALine);
end;

begin
  Result:= '';
  AKeyword:= trim(AKeyword);
  if Lexicon.Count > 0 then begin
    i:= 0;
    if AKeyWord = '' then exit;
    if AKeyword = ':' then exit;
    try if AKeyWord[1] <> ':' then AKeyWord:= ':' + AKeyWord; except Exit; end;
    repeat
      Found:= (Pos(Uppercase(AKeyWord),Uppercase(Lexicon[i])) <> 0);
      //':' must be the first symbol on the line.
      if Found then try
        Found:= trim(Lexicon[i])[1] = ':';
      except Found:= false;
      end;
      inc(i);
    until Found or (i = Lexicon.Count);
    if Found then begin
      FirstLine:= i-1;
      Found:= false;
      while (i < Lexicon.Count) and not found do begin
        if Lexicon[i] <> '' then Found:= Lexicon[i][1] = ':';
        Inc(i);
      end; {while}
      LastLine:= max(i-2,FirstLine);
      i:= FirstLine;
      repeat
        Result:= Result + WithoutTabs(Lexicon[i]) + LF;
        Inc(i);
      until i > LastLine;
    end; {if}
  end; {if}
end;

procedure TLife32MainForm.Lexicon1Click(Sender: TObject);
var
  KeyWord: string;
  Explanation: string;

function GetCurrentWord: string;
var
  found: boolean;
  TheMemo: TRichEdit;
  CurrentLine, CurrentColumn: integer;
  AllText: string;
  ALine: string;
  i: integer;
  LineStart: integer;
  WordStart, ExprStart, WordEnd, ExprEnd: integer;
begin
  WordStart:= 0; ExprStart:= 0; WordEnd:= 0; ExprEnd:= 0;
  if InfoPageControl.ActivePage = TSInfoMemo then TheMemo:= TRichEdit(InfoMemo)
  else TheMemo:= TRichEdit(LexiconMemo);
  try
    if Length(TheMemo.Lines.Text) = 0 then exit
    else begin
      //Try to find an '{' before the word.
      //Get the current line and column numbers from the selection.
      with TheMemo do begin
        CurrentLine:= Perform(EM_LINEFROMCHAR,0,SelStart);
        CurrentColumn:= SelStart - Perform(EM_LINEINDEX, CurrentLine, 0)+1;
        AllText:= TheMemo.Text;
        LineStart:= Max(CurrentColumn-40,1);
        CurrentColumn:= CurrentColumn - LineStart + 1;
        ALine:= Copy(AllText,LineStart,Min(Length(AllText),80));
        StripLFCR(ALine, CurrentColumn);
      end;

      //Find the start of the word or expression.
      Found:= false;
      i:= CurrentColumn;
      while (i > 0) and not found do begin
        if CharInSet(ALine[i],['}']) then found:= true; //stop looking.
        if CharInSet(ALine[i],['{']) then ExprStart:= i
        else if CharInSet(ALine[i],NoWord) and (WordStart = 0) then WordStart:= i;
        dec(i);
        if not found then Found:= (ExprStart <> 0)
      end; {while}
      if (ExprStart or WordStart) = 0 then WordStart:= 1;

      //Find the end of the word or expression.
      Found:= false;
      i:= CurrentColumn + Max(TheMemo.SelLength-1,1);
      while (i <= Length(ALine)) and not found do begin
        if CharInSet(Aline[i],['{']) then found:= true;
        if (ExprStart <> 0) and CharInSet(ALine[i],['}']) then ExprEnd:= i
        else if CharInSet(ALine[i],NoWord) and (WordEnd = 0) then WordEnd:= i;
        inc(i);
        if not found then Found:= (ExprEnd <> 0)
      end; {while}
      if (ExprEnd or WordEnd) = 0 then WordEnd:= Length(ALine)+1;
      if (ExprEnd = 0) then ExprEnd:= WordEnd;

      //Do we have an expression?
      if (ExprStart <> 0) then begin
        Result:= Copy(ALine,ExprStart+1,ExprEnd-ExprStart-1);
      end
      //No, it's a word.
      else begin
        Result:= Copy(ALine,WordStart+1,WordEnd-WordStart-1);
      end;
    end; {else}
  except Result:= ' ';
  end;
end;

begin
  if Lexicon.Count = 0 then begin
    Suspend(sender);
    LifeBox1.CanPaint:= cpDialogShowing;
    if OpenDialog2.execute then begin
      try
        Lexicon.LoadFromFile(OpenDialog2.FileName);
        FillCBLexiconFind;
        except {do nothing}
      end; {if}
    end; {if}
    Resume(Sender);
  end; {if}
  KeyWord:= GetCurrentWord;
  Explanation:= FindKeywordInLexicon(KeyWord);
  //nothing found, try without a trailing 's'.
  if Explanation = '' then begin
    if (Length(KeyWord) > 0) and (Upcase(Keyword[Length(Keyword)])='S') then begin
      Delete(KeyWord,Length(KeyWord),1);
      Explanation:= FindKeywordInLexicon(KeyWord);
    end;
  end;
  //Still nothing found, try replacing 'ize' with 'is'
  if Explanation = '' then begin
    if (pos('ize',lowercase(Keyword)) = length(keyword)-2) then begin
      Delete(KeyWord,Length(KeyWord)-2,3);
      Keyword:= Keyword + 'is';
      Explanation:= FindKeywordInLexicon(KeyWord);
    end;
  end;
  while Pos('{',keyword) <> 0 do Delete(keyword,Pos('{',Keyword),1);
  while Pos('}',keyword) <> 0 do Delete(keyword,Pos('}',Keyword),1);
  if Explanation <> '' then begin
    LexiconMemo.Lines.Insert(0,Explanation);
    InfoPageControl.ActivePage:= TSLexicon;
  end; {if}
end;

procedure TLife32MainForm.Panel8Resize(Sender: TObject);
begin
  CBLexiconFind.Width:= Panel8.Width - 4;
end;

procedure TLife32MainForm.CBLexiconFindClick(Sender: TObject);
var
  Info: string;
begin
  Info:= FindKeywordInLexicon(CBLexiconFind.Text);
  if Info <> '' then LexiconMemo.Lines.Add(Info);
end;

procedure TLife32MainForm.TabSet1MeasureTab(Sender: TObject;
  Index: Integer; var TabWidth: Integer);
begin
  TabWidth:= 30;
end;


procedure TLife32MainForm.SidePanelTabsMeasureTab(Sender: TObject;
  Index: Integer; var TabWidth: Integer);
begin
  TabWidth:= TabImageList.Width;
end;

procedure TLife32MainForm.SidePanelTabsDrawTab(Sender: TObject;
  TabCanvas: TCanvas; R: TRect; Index: Integer; Selected: Boolean);
var
  ARect: TRect;
begin
  ARect:= SidePanelTabs.ItemRect(Index);
  if Index < TabImageList.Count then begin
    TabImageList.Draw(TabCanvas,ARect.Left+1,ARect.Top+8,Index);
  end;
end;

procedure TLife32MainForm.SidePanelTabsChange(Sender: TObject;
  NewTab: Integer; var AllowChange: Boolean);
const
  cOpen = 0;
  cInfo = 1;
  cSnapshot = 2;
  cTorus = 3;
  cMoveTo = 4;
  cPattern = 5;
var
  OldPage: TTabSheet;
begin
  with SidePageControl do begin
    OldPage:= ActivePage;
    case NewTab of
      cOpen: ActivePage:= TSOpen;
      cInfo: ActivePage:= TSInfo;
      cSnapshot: ActivePage:= TSSnapShots;
      cTorus: ActivePage:= TSTorus;
      cMoveTo: ActivePage:= TSMoveTo;
      cPattern: ActivePage:= TSPattern;
    end; {case}
    if OldPage <> ActivePage then SidePageControl.OnChange(SidePageControl);
  end; {with}
end;

procedure TLife32MainForm.FormDestroy(Sender: TObject);
begin
  DropLife32Target1.Unregister;
end;

procedure TLife32MainForm.AddToPatternList(AUniverse: TUniverse; Key: Char; Caption: string);
var
  Image1, Image2: TBitmap;
  NewItem: TListItem;
  DropData: TUniverse;
  ShortcutKey: string;
begin
  Image1:= nil;
  try
    DropData:= AUniverse.Clone;
    Image1:= DropData.SaveToBitmap(AUniverse.ClipRect);
    //make sure image is square before resizing it.
    if Image1.Width > Image1.Height then Image1.Height:= Image1.Width
    else Image1.Width:= Image1.Height;
    Image2:= TBitmap.Create;
    try
      Image2.Height:= PatternImages.Height;
      Image2.Width:= PatternImages.Width;
      Image2.Canvas.Brush.Color:= clLtGray;
      Image2.Canvas.FillRect(Rect(0,0,Image2.Width,Image2.Height));
      Image2.Canvas.Copymode:= cmMergeCopy;
      Image2.Canvas.StretchDraw(Rect(0,0,Image2.Width,Image2.Height),Image1);
      PatternImages.Add(Image2,nil);
    finally
      Image2.Free;
    end;

    Image2:= TBitmap.Create;
    try
      Image2.Height:= SmallPatternImages.Height;
      Image2.Width:= SmallPatternImages.Width;
      Image2.Canvas.StretchDraw(Rect(0,0,Image2.Width,Image2.Height),Image1);
      SmallPatternImages.Add(Image2,nil);
    finally
      Image2.Free;
    end;

    NewItem:= PatternListView.Items.Add;
    if Caption = '' then begin
      if NewItem.Index > 22 then NewItem.Caption:= IntToStr(NewItem.Index - 23)
      else NewItem.Caption:= Char(integer('A')+NewItem.Index);
    end {if}
    else NewItem.Caption:= Caption;
    NewItem.ImageIndex:= NewItem.Index;
    if Key = #0 then begin
      //Counting starts at 0.
      if NewItem.Index > 22 then begin
        if NewItem.Index > 45 then ShortcutKey:= #0
        else ShortcutKey:= 'shift+'+char(Integer('A')+NewItem.ImageIndex-23);
      end
      else ShortcutKey:= char(Integer('A')+NewItem.ImageIndex);
    end
    else ShortcutKey:= Key;
    //we misuse the overlayindex to hold the shortcutkey.
    NewItem.OverlayIndex:= Integer(TextToShortCut(ShortCutKey));
    NewItem.Data:= DropData;
  finally
    Image1.Free;
  end; {try with}
end;


procedure TLife32MainForm.DropTextTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  DropPoint: TPoint;
  i: integer;
  Life32CutOut: ILife32CutOut;
  FromInside: boolean;
begin
  with PatternListView do begin
    DropLife32Target1.DataObject.QueryInterface(ILife32CutOut,Life32CutOut);
    if not(Assigned(Life32CutOut)) then FromInside:= false
    else FromInside:= Life32CutOut.SourceID = DropLife32Source1.ReadSourceID;
    if PtInRect(Rect(0,0,Width,Height),Point) and FromInside then
      Effect:= DROPEFFECT_MOVE
    else Effect:= DROPEFFECT_COPY;
    Tag:= 0;
  end; {with}
  if (Effect = DROPEFFECT_MOVE) then with PatternListView do begin
    DropPoint:= Point;
    Dec(DropPoint.x,DropLife32Source1.ImageHotSpotX);
    Dec(DropPoint.Y,DropLife32Source1.ImageHotSpotY);
    if Assigned(Selected) then Selected.SetPosition(DropPoint);
  end else begin
    //AddToPatternList(DropLife32Target1.Universe,#0,'');
    AddToPatternList(Life32Cutout.GetCutOut,#0,'');
    i:= PatternListView.Items.Count - 1;
    if PatternListView.Tag = 0 then if i >= 0 then PatternListView.Items[i].SetPosition(point)
    else PatternListview.Tag:= 0;
  end; {else}
  PatternListView.Refresh;
end;

procedure TLife32MainForm.DropTextTarget1DragOver(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Life32CutOut: ILife32CutOut;
  FromInside: boolean;
begin
  with PatternListView do begin
    DropLife32Target1.DataObject.QueryInterface(ILife32CutOut,Life32CutOut);
    if not(Assigned(Life32CutOut)) then FromInside:= false
    else FromInside:= Life32CutOut.SourceID = DropLife32Source1.ReadSourceID;
    if (FromInside) and PtInRect(Rect(0,0,Width,Height),Point) then
      Effect:= DROPEFFECT_MOVE
    else Effect:= DROPEFFECT_COPY;
  end; {with}
end;

procedure TLife32MainForm.DropTextTarget1GetDropEffect(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  Effect:= DROPEFFECT_COPY;
end;

procedure TLife32MainForm.LargeImages1Click(Sender: TObject);
begin
  LargeImages1.Checked:= not(LargeImages1.Checked);
  if LargeImages1.Checked then begin
    PatternListView.ViewStyle:= vsIcon;
    DropLife32Source1.Images:= PatternImages;
  end {if}
  else begin
    PatternListView.ViewStyle:= vsSmallIcon;
    DropLife32Source1.Images:= SmallPatternImages;
  end; {else}
end;

procedure TLife32MainForm.Delete3Click(Sender: TObject);
var
  i: integer;
begin
  if Assigned(PatternListView.Selected) then with PatternListView do begin
    i:= Selected.Index;
    TUniverse(Items[i].Data).Free;
    Items.Delete(i);
    PatternImages.Delete(i);
    SmallPatternImages.Delete(i);

    i:= 0;
    while i < Items.Count do begin
      Items[i].ImageIndex:= i;
      Inc(i);
    end;
  end; {if with}
end;

procedure TLife32MainForm.PatternListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if (Change = ctState) and (Assigned(PatternListView.Selected)) then begin
    PatternNameEdit.Text:= PatternListView.Selected.Caption;
    PatternHotkey.Hotkey:= TShortCut(PatternListView.Selected.OverlayIndex);
  end; {if}
  SBSavePatterns.Enabled:= PatternListView.Items.Count > 0;
  SBClearPatterns.Enabled:= Assigned(PatternListView.Selected);
end;

procedure TLife32MainForm.PatternHotkeyExit(Sender: TObject);
begin
  if assigned(PatternListView.Selected) then begin
    PatternListView.Selected.OverlayIndex:= integer(PatternHotkey.HotKey);
  end;
end;

procedure TLife32MainForm.PatternNameEditChange(Sender: TObject);
begin
  with PatternListView do begin
    if Assigned(Selected) then Selected.Caption:= PatternNameEdit.Text;
  end; {with}
end;

procedure TLife32MainForm.SkipToEditChange(Sender: TObject);
var
  TargetGen: integer;
begin
  try
    TargetGen:= StrToInt(SkipToEdit.Text);
    except TargetGen:= -1;
  end; {try}
  if (Length(SkipToEdit.Text)>0) and CharInSet(SkipToEdit.Text[1],['-','+']) then begin
    if (TargetGen < (MinGen - LifeBox1.Generation)) then
      SkipToEdit.Font.Color:= clRed
    else SkipToEdit.Font.Color:= clWindowText;
  end
  else if (TargetGen < MinGen) then SkipToEdit.Font.Color:= clRed
  else SkipToEdit.Font.Color:= clWindowText;
end;

procedure TLife32MainForm.SpinButton1DownClick(Sender: TObject);
var
  SkipValue: integer;
  SkipText: string;
  OK: boolean;
begin
  OK:= true;
  SkipText:= SkipToEdit.Text;
  SkipValue:= 0;
  try
    SkipValue:= StrToInt(SkipText);
    except Ok:= false;
  end; {try}
  if OK then begin
    if (LifeBox1.Generation + SkipValue) > MinGen then Dec(SkipValue);
    if CharInSet(SkipText[1],['+']) and (SkipValue >= 0) then
      SkipText:= SkipText[1]
    else SkipText:= '';
    SkipText:= SkipText + IntToStr(SkipValue);
    SkipToEdit.Text:= SkipText;
  end; {if}
end;

procedure TLife32MainForm.SpinButton1UpClick(Sender: TObject);
var
  SkipValue: integer;
  SkipText: string;
  OK: boolean;
begin
  OK:= true;
  SkipText:= SkipToEdit.Text;
  SkipValue:= 0;
  try
    SkipValue:= StrToInt(SkipText);
    except Ok:= false;
  end; {try}
  if OK then begin
    if SkipValue < MaxInt then Inc(SkipValue);
    if CharInSet(SkipText[1],['+','-']) then begin
      if SkipValue < 0 then SkipText:= ''
      else SkipText:= '+';
    end
    else SkipText:= '';
    SkipText:= SkipText + IntToStr(SkipValue);
    SkipToEdit.Text:= SkipText;
  end; {if}
end;

procedure TLife32MainForm.SkipToEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  SaveKey: Word;
begin
  SaveKey:= Key;
  Key:= 0;
  case SaveKey of
    vk_Up: SpinButton1UpClick(Sender);
    vk_Down: SpinButton1DownClick(Sender);
    else Key:= SaveKey;
  end; {case}
end;

procedure TLife32MainForm.SkipToEditKeyPress(Sender: TObject;
  var Key: Char);
var
  Target: string;
  TargetGen: integer;
  NewGen: integer;
begin
  if not CharInSet(Key,['+','-','0'..'9',#0..#31]) then Key:= #0;
  case Key of
    #13,#10: begin
      Target:= SkipToEdit.Text;
      Target:= Trim(Target);
      try
        NewGen:= StrToInt(Target);
        if (Target[1] = '-') or (Target[1] = '+') then begin
          TargetGen:= Max(Generation + NewGen,0);
        end {if}
        else TargetGen:= NewGen;
      except TargetGen:= Generation;
      end; {try}
      ShowGenerations:= cbShow.Checked;
      if TargetGen > MinGen then Generation:= TargetGen;
    end; {#13,#10}
    #27: SkipToMode:= false;
  end; {case}
end;

procedure TLife32MainForm.cbShowClick(Sender: TObject);
begin
  ShowGenerations:= cbShow.Checked;
  if SkipToEdit.CanFocus then SkipToEdit.SetFocus;
end;

procedure TLife32MainForm.PatternListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AnItem: TListItem;
  OffsetX, OffsetY: integer;
begin
  AnItem:= PatternListView.GetItemAt(x,y);
  if (AnItem <> nil) and (AnItem = PatternListView.Selected) then begin
    OffsetX:= x - AnItem.Left;
    OffsetY:= y - AnItem.Top;
    with DropLife32Source1 do try
      Universe:= TUniverse(AnItem.Data);
      ImageIndex:= PatternListView.Selected.Index;
      ImageHotSpotX:= OffsetX;
      ImageHotSpotY:= OffsetY;
      Execute;
    finally ;
    end; {with try}
  end;
end;

procedure TLife32MainForm.CloseSideButtonClick(Sender: TObject);
begin
  HideSidePanel;
end;

//Procedure invoked when F2 is pressed.
procedure TLife32MainForm.Rename2Click(Sender: TObject);
begin
  if (LeftPanel.Width > 1) and (SidePageControl.ActivePage = TSPattern) then begin
    PatternNameEdit.SetFocus;
    PatternNameEdit.SelectAll;
  end;
end;

procedure TLife32MainForm.Arrangeicons1Click(Sender: TObject);
begin
  ArrangeIcons1.Checked:= not(ArrangeIcons1.Checked);
  if ArrangeIcons1.Checked then ArrangePatternList;
end;

procedure TLife32MainForm.ArrangePatternList;
var
  x,y,i: integer;
  horz: integer;
  size: integer;
begin
  size:= PatternImages.Width + 10;
  horz:= PatternListView.Width div (size);
  i:= 0;
  x:= 0;
  y:= 0;
  with PatternListView do while (i < Items.Count) do begin
    Items[i].SetPosition(Point(x * size + 4,y * (size + 13)+5));
    Inc(x);
    if (x >= horz) then begin
      x:= 0;
      Inc(y);
    end; {if}
    Inc(i);
  end; {with while}
end;

procedure TLife32MainForm.SavePattern(FileName: string);
var
  i: integer;
  AnItem: TUniverse;
  Part, Whole: TStringList;
begin
  Whole:= TStringList.Create;
  Whole.Add('#Life32');
  Whole.Add('#D life pattern file');
  Whole.Add('#D filename='+filename);
  try
    with PatternListView do begin
      i:= 0;
      while i < Items.Count do begin
        Whole.Add('#Pattern='+IntToStr(i));
        Whole.Add('#Key='+IntToStr(Items[i].OverLayIndex));
        Whole.Add('#Caption='+Items[i].Caption);
        AnItem:= TUniverse(Items[i].Data);
        Part:= AnItem.SaveToStringList(smDefault, false);
        Whole.AddStrings(Part);
        Part.Free;
        Inc(i);
      end; {while}
    end; {with}
    Whole.SaveToFile(FileName);
  finally
    Whole.Free;
  end; {try}
end;

procedure TLife32MainForm.SaveToDisk1Click(Sender: TObject);
begin
  with SaveDialogPattern do begin
    if execute then SavePattern(FileName);
  end; {with}
end;

procedure TLife32MainForm.OpenPattern(Filename: string);
var
  Whole: TStringList;
  Part: TStringList;
  i,head,next,tail: integer;
  Key, Caption: string;
  AUniverse: TUniverse;
begin
  //clear all
  Whole:= TStringList.Create;
  try
    Whole.LoadFromFile(Filename);
    if Whole.IndexOf('#Life32') = -1  then Abort;
    if Whole.IndexOf('#D life pattern file') = -1 then Abort;
    next:= 0;
    while next <> -1 do begin
      head:= Whole.IndexOfName('#Pattern');
      Whole.delete(head);
      //Head is now the first line of the pattern
      next:= Whole.IndexOfName('#Pattern');
      //Next is the first line of the next pattern (or -1).
      if next = -1 then tail:= Whole.count-1 else tail:= next - 1;
      Part:= TStringList.Create;
      try
        //copy part of the pattern to it's own list.
        for i:= head to tail do begin
          Part.Add(Whole[i]);
        end; {for i}
        Key:= Part.Values['#Key'];
        if Key = '' then Key:= '0';
        Key:= Char(StrToInt(Key));
        Caption:= Part.Values['#Caption'];
        with LifeBox1.Universe do
          AUniverse:= TUniverse.Create(RuleString,Neighborhood);
        AUniverse.LoadFromStringList(Part);
        AddToPatternList(AUniverse,Key[1],Caption);
      finally
        Part.Free;
      end; {try}
    end; {while}
  finally
    Whole.Free;
  end; {try}
end;

procedure TLife32MainForm.Open2Click(Sender: TObject);
begin
  with OpenDialogPattern do begin
    if execute then OpenPattern(FileName);
  end;
end;


procedure TLife32MainForm.LifeBox1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  UpdateButtons;
end;

procedure TLife32MainForm.InfoButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    Suspend(Sender);
    //LifeBox1.CanPaint:= cpDialogShowing;
    ShowSidePanel(TSInfo,0);
    //Don't resume yet, we want to do this *after* the box has closed.
  end;
end;

procedure TLife32MainForm.ShowSideButtonClick(Sender: TObject);
begin
  ShowSidePanel(SidePageControl.ActivePage,0);
end;

procedure TLife32MainForm.HideSideButtonClick(Sender: TObject);
begin
  HideSidePanel;
end;

procedure TLife32MainForm.PatternListViewInsert(Sender: TObject;
  Item: TListItem);
begin
  //Implement auto-arrange if activated.
  if ArrangeIcons1.Checked then begin
    //notify that the droptarget should not reposition the last image.
    PatternListView.Tag:= 1;
    ArrangePatternList;
  end;
end;

procedure TLife32MainForm.ZoomtoFit3Click(Sender: TObject);
begin
  if LifeBox1.SelectionVisible then LifeBox1.ZoomToSelection(MaxZoomToFit)
  else LifeBox1.ZoomToFit(DrawAgain, MaxZoomToFit);
  Resume(Sender);
end;

procedure TLife32MainForm.PatternPopupPopup(Sender: TObject);
begin
  Delete3.Enabled:= Assigned(PatternListView.Selected);
  SaveToDisk1.Enabled:= PatternListView.Items.Count > 0;
end;

procedure TLife32MainForm.InfoMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  LastMouseDown: integer = 0;
var
  TimeNow: integer;
begin
  TimeNow:= GetTickCount;
  if TimeNow - LastMouseDown < 300 then begin
    Lexicon1.Click;
  end;
  LastMouseDown:= TimeNow;
end;

procedure TLife32MainForm.TabPopupPopup(Sender: TObject);
begin
  Suspend(sender);
  Delete2.Enabled:= UniverseTabset.Tabs.Count > 1;
end;

procedure TLife32MainForm.UniverseTabsetMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  Msg: TWMLButtonDown;
begin
  UniverseTabset.Tag:= 0;
  if Button = mbRight then begin
    with Msg do begin
      Msg:= WM_LButtonDown;
      Keys:= MK_LBUTTON	;
      xpos:= x; ypos:= y;
    end;
    UniverseTabset.Dispatch(Msg);
    with UniverseTabset do p:= UniverseTabset.ClientToScreen(Point(x,Top));
    p.y:= p.y - (GetsystemMetrics(SM_CYMENU)*4);
    TabPopup.Popup(p.x,p.y);
  end;
end;

procedure TLife32MainForm.DropLife32Target2DragOver(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Item: integer;
begin
  //figure out which tab is being hovered over.
  Item:= UniverseTabset.ItemAtPos(Point);
  if Item >= 0 then UniverseTabset.TabIndex:= Item;
end;

procedure TLife32MainForm.DropLife32Target2GetDropEffect(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
begin
  //Effect:= DROPEFFECT_COPY;
end;

procedure TLife32MainForm.UniverseTabsetMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  //if UniverseTabset.Tag = 1 then UniverseTabset.BeginDrag(false);
end;

procedure TLife32MainForm.SidePanel1Click(Sender: TObject);
begin
  if SidePanel1.Checked then HideSidePanel
  else ShowSidePanel(SidePageControl.ActivePage,0);
  SidePanel1.Checked:= not SidePanel1.Checked;
end;

procedure TLife32MainForm.Cut2Click(Sender: TObject);
begin
  RuleEdit.CutToClipboard;
end;

procedure TLife32MainForm.Copy2Click(Sender: TObject);
begin
  RuleEdit.CopyToClipboard;
end;

procedure TLife32MainForm.Paste2Click(Sender: TObject);
begin
  RuleEdit.PasteFromClipboard;
end;

procedure TLife32MainForm.DropLife32Target3DragOver(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Item: integer;
begin
  //always select the scrapbook page when a drag is offered.
  Item:= SidePanelTabs.Tabs.Count-1;
  SidePanelTabs.TabIndex:= Item;
end;


procedure TLife32MainForm.InfoPageControlChange(Sender: TObject);
begin
  if InfoPageControl.ActivePage = TSStat then Description.Caption:= 'Statistics'
  else if InfoPageControl.ActivePage = TSInfoMemo then begin
    InfoMemoPopup.PopupComponent:= InfoMemo;
    Description.Caption:= 'Description';
  end
  else if InfoPageControl.ActivePage = TSLexicon then begin
    InfoMemoPopup.PopupComponent:= LexiconMemo;
    Description.Caption:= 'Lexicon';
  end;
end;

procedure TLife32MainForm.UpDown1Click(Sender: TObject;
  Button: TUDBtnType);
begin
  InfoPageControl.SelectNextPage(Button = btNext);
end;

procedure TLife32MainForm.ListView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AFileName: string;
  ValidLifeFile: boolean;
  LifeLines: TStringList;
  DescLines: TStringList;
begin
  if Button = mbRight then begin
    if Assigned(ListView1.Selected) then begin
      AFileName:= DirectoryListBox1.Directory+'\'+ListView1.Selected.Caption;
      ValidLifeFile:= FileExists(AFilename);
      if ValidLifeFile then begin
        DescLines:= nil;
        LifeLines:= TStringList.Create;
        try
          LifeLines.LoadFromFile(AFileName);
          DescLines:= GetDescription(LifeLines);
          LifeLines.Free;
          ListView1.Hint:= '?'+DescLines.Text+'|';
          if ListView1.Hint = '?' then ListView1.Hint:= 'No info|';
        except ListView1.Hint:= 'Right-click for description|Right-click for description';
        end; {try}
        DescLines.Free;
      end;
    end; {if}
  end; {if}
end;

procedure TLife32MainForm.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctState then begin
    ListView1.Hint:= 'Right-click for description|Right-click for description';
    Application.HideHint;
  end;
end;

procedure TLife32MainForm.RegisterFileExtensions;
begin
  with TStringList.Create do try
    Add('Name='+'Life32.LifeFile.2');
    Add('Exts='+Extensions);
    Add('desc='+'Conway''s life file');
    Add('cmds=open');
    //SCInterface.FileAssociations.Add(CommaText);
  finally
    Free;
  end;
end;

procedure TLife32MainForm.SCInterfaceCmdOpenFile(FileName,
  Option: String);
var
  i: integer;
  ARewindList: TSnapshotList;
  AnIndex,a: integer;
  TabName: string;
begin
  Suspend(self);
  //if not empty then insert a new sheet.
  i:= 1;
  while i <= Length(Filename) do begin
    if Filename[i] = '"' then Delete(Filename,i,1)
    else Inc(i);
  end; {while}
  if (FileName = '') and (LastOpenFile = '') then Filename:= GetMRUFilename(1);
  if Filename <> '' then begin
    TabName:= Trim(ExtractFileName(Filename));
    a:= Pos(ExtractFileExt(FileName),TabName);
    Delete(TabName,a,Length(TabName)-a+1);
    TabName:= Proper(TabName);
    if not(LifeBox1.IsEmpty) then with UniverseTabset do begin
      AnIndex:= TabIndex;
      Tabs.Insert(AnIndex,TabName);
      ARewindList:= NewRewindList;
      RewindListList.Insert(AnIndex,ARewindList);
      TabIndex:= TabIndex-1;
    end;
    //else with UniverseTabSet do Tabs[TabIndex]:= TabName;
    OpenFile(Filename);
    SetStartDir(Filename);
  end;
  Resume(self);
end;


procedure TLife32MainForm.FirstTabButtonClick(Sender: TObject);
begin
  UniverseTabset.TabIndex:= 0;
end;

procedure TLife32MainForm.LastTabButtonClick(Sender: TObject);
begin
  UniverseTabset.TabIndex:= UniverseTabset.Tabs.Count-1;
end;

procedure TLife32MainForm.PrevTabButtonClick(Sender: TObject);
begin
  if (UniverseTabset.TabIndex > 0) then UniverseTabset.SelectNext(false);
end;

procedure TLife32MainForm.NextTabButtonClick(Sender: TObject);
begin
  if (UniverseTabset.TabIndex < UniverseTabset.Tabs.Count -1) then UniverseTabset.SelectNext(true);
end;

procedure TLife32MainForm.UDScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  Scrollbar: TScrollbar;
begin
  if Sender = UD then Scrollbar:= ScrollbarUD
  else if Sender = LR then Scrollbar:= ScrollbarLR
  else exit;
  with Scrollbar do begin
    if ScrollCode = scLineUp then Position:= Position - SmallChange
    else if ScrollCode = scLineDown then Position:= Position + SmallChange;
  end;
end;

procedure TLife32MainForm.DropLife32Target2Drop(Sender: TObject;
  ShiftState: TShiftState; Point: TPoint; var Effect: Integer);
var
  Item: integer;
  PastePoint: TPoint;
  DragRect: TRect;
begin
  //When dropping from an outside source, pause the action, so no strange
  //behavior will occur.
  PauseButton.Click;
  //figure out which tab is being hovered over.
  Item:= UniverseTabset.ItemAtPos(Point);
  if Item >= 0 then UniverseTabset.TabIndex:= Item;
  DragRect:= DropLife32Target2.Universe.ClipRect;
  PastePoint.x:= -ABS((DragRect.Right - DragRect.Left) div 2);
  PastePoint.y:= -ABS((DragRect.Bottom - DragRect.Top) div 2);
  //The above action will also change the current universe.
  //Drop the dragged part in the now active universe.
  Effect:= DROPEFFECT_COPY;
  LifeBox1.Universe.InsertShape(DropLife32Target2.Universe,PastePoint,LifeBox1.PasteMode);
  LifeBox1.RedrawAll;
end;

procedure TLife32MainForm.HideSidePanelButtonClick(Sender: TObject);
begin
  HideSidePanel;
  ShowSidePanel(SidePageControl.ActivePage,DefaultSidePanelSize);
end;

procedure TLife32MainForm.ItemTorus1Click(Sender: TObject);
begin
  ShowSidePanel(TSTorus,0);
end;

procedure TLife32MainForm.LeftPanelResize(Sender: TObject);
begin
  if ArrangeIcons1.Checked then ArrangePatternList;
end;

procedure TLife32MainForm.CBIsLimitedKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  Key:= 0;
  LifeBox1.SetFocus;
end;

procedure TLife32MainForm.SnapshotTimerTimer(Sender: TObject);
begin
  LifeGen.StopSnapshot:= true;
  SnapShotTimer.Enabled:= false;
end;

procedure TLife32MainForm.Clearoutsideselection2Click(Sender: TObject);
begin
  if not(IsMyRectEmpty(LifeBox1.SelectionRect)) then begin
    Clearoutsideselection1Click(sender);
  end; {if}
end;

procedure TLife32MainForm.Torus1Click(Sender: TObject);
begin
  ItemTorus1Click(sender);
end;

procedure TLife32MainForm.LifeBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetSelectionStats(UpdateSelStatistics);
end;

initialization
  {$ifdef time}
  MyTimer:= TTickTimer.Create;
  CumDisplay:= 0;
  CumGeneration:= 0;
  {$endif}
finalization
  {$ifdef time}
  MyTimer.Free;
  {$endif}
end.
