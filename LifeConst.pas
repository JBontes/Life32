unit LifeConst;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Graphics, messages, Windows;

type
  TLineDrawState = byte;
  TTorusKind = byte;
  TRuleArray = array[0..512] of boolean;

const
  bit0 = $0001; bit8 = $0100;
  bit1 = $0002; bit9 = $0200;
  bit2 = $0004; bita = $0400;
  bit3 = $0008; bitb = $0800;
  bit4 = $0010; bitc = $1000;
  bit5 = $0020; bitd = $2000;
  bit6 = $0040; bite = $4000;
  bit7 = $0080; bitf = $8000;
  DDFast = 1;
  DDTempDisabled = 2;
  DDExitOK = 4;
  DDScreenSaver = 8;

const
  CFSTR_LIFE32 = 'Life32v2 Snapshotstream';
  CFSTR_LIFE32PATTERNLIST = 'Life32v2 InternalPatternList';

  tFree = 0;
  tBusy = 1;

  lds_On = byte(true);
  lds_Off = byte(false);
  lds_Xor = abs(byte(true))+1; {will avail to 2 even without ABS}

  tk_LeftRight = 5;
  tk_UpDown = 10;
  tk_All = 15;

  LifeTitle = 'Life32';

  MemsArray : array [0..4] of integer = (4*1024,8*1024,16*1024,32*1024,64*1024);
  DefaultMemPerBlock = 64*1024;

  Numbers = ['0'..'9'];
  NormalRules = ['0'..'9','/','\','s','b','S','B',':',#1..#32];
  SpecialRules = ['a','c'..'f','i','k','j','q','r','y','t','w','z'];
  AlphaNums = ['!'..'~'];
  ValidNumbers = ['0'..'9',#1..#31,'-','+'];
  HandleAlways = [#32,'!','@','#','(',',','.','<','>','X'..'Z','x'..'z','*','/'];
  ValidRules = ['0'..'9','A'..'F','I','K','J','Q','R','Y','T','W','Z',
                'S','M','O','P','X','H','V',':','/','\','-',#1..#31];
  NoWord = [#1..#32,',','.','!','(',')','*','''','"','<','>','?','/','\','|','{','}'];
  TabsEtc = [#1..#31];
  NewLine = [#10,#13];
  LFCR = #10#13;
  CR = #13;
  LF = #10;
  strtemp = 'temp'+'\';
  ScrapbookKeys = 'ABCDEFGHIJKLMNOPQRSTUVW';

  //DefaultExtensions: array [0..7] of string = ('.lif','.l','.xlife','.xli','.life','.rle','.gif','.bmp');
  DefaultExtensionState: array [0..7] of boolean = (true,true,true,true,true,true,false,false);
  DefaultExtensions = 'lif,l,xli,rle,xlife,life';

  cXLabel = 'X: %d';
  cYLabel = 'Y: %d';
  CelWidth = 16;
  CelHeight = 16;

  //Neighbourhoods.
  //Defined as a bitmap like so.
  //8 7 6
  //5 4 3
  //2 1 0
  //thus $1FF has all bits set, where the cell itself counts as a neighbour.

  nbUnchanged = $0;
  nbMin = $001;
  nbMoore = $1EF;
  nb9Cell = nbMoore;
  nbDefault = nbMoore;
  nbVonNeuman = $0AA;
  nb5Cell = nbVonNeuman;
  nbHex = $1AB;
  nb7Cell = nbHex;
  nbX = $145;
  nbMax = $1FF;
  nbJustFriends = $ffef; //Moore neighbourhood, but specialcased.
  DefaultMaxNeighbors = 8;


  //Saving in Life 1.05 format stuff
  cstrLife105Header = '#Life 1.05';
  cstrLife106Header = '#Life 1.06';
  cstrMCellHeader = '#MCell';
  MaxDescLines = 22;
  DefaultPictureWidth = 70;

  cstrProLifeExt = '.PLF';
  cstrDefaultExt = '.lif';
  cstrBitmapExt =  '.bmp';
  cstrMCellExt = '.mcl';
  cstrGifExt = '.gif';


  asDesigning = 0;
  asRunning = 1;
  FixedItemsInRuleMenu = 5;

  DontCreate = false;
  MakeIt = true;
  GoForward = true;
  GoBack = false;

  MaxX = $7FFFF;
  MaxY = MaxX;
  MinX = -(MaxX+1);
  MinY = MinX;

  MaxPlaySpeed = 0;
  MaxSpeed = 0;
  MinSpeed = 5000;
  DefaultSpeed = 50;
  MaxZoom = 10;
  MinZoom = -8;
  DefaultZoom = 2;
  DefaultScroll = 2;
  DefaultFramedropTime = 0;
  DefaultFillPercentage = 33;
  DefaultZoomToFit = true;
  DefaultBoldGridSpacing = 5;
  DefaultGrid = true;
  DefaultRules = '23/3';
  DefaultNoSnapshotTimeOut = false;
  MinSnapshotTimeOut = 300;
  DefaultSnapshotTimeOut = 2000;
  DefaultReshowSkip = true;

  NoRedraw = false;
  DrawAgain = true;
  DefaultDirectDrawEnabled = DDFast;

  DefaultBackColor = clWhite;
  DefaultCelColor = clBlack;
  DefaultGridColor = clSilver;
  DefaultGrid2Color = clGray;
  DefaultSelRectColor = clBlue;
  DefaultZoomRectColor = clYellow;
  DefaultDragColor = clRed;
  DefaultTorusColor = clOlive; {Dirty Yellow}

  DefaultLimitRect: TRect = (Left:-300;Top:-200;Right:300;Bottom:200);
  DefaultIsLimited = false;
  DefaultTorusKind = tk_All;
  DefaultDeadEdges = false;

  //Sidepanel
  DefaultSidePanelSize = 182;
  DefaultShowSidepanelOnStartup = true;
  DefaultHidePanelOnPlay = false;

  //Help topics
  HelpOnSettings = 1001;

  //SaveModes
  smDefault = 1;
  smLife105 = 2;
  smRLE = 3;
  smDBLife = 4;
  smXLife20 = 5;
  smLife106 = 6;
  smBitmap = 7;
  smGif = 8;
  smMCell = 9;


  crDraw = 5;
  crCross = 6;
  crZOOM = 7;
  crHAND = 8;
  crTransparentArrow = 9;
  crHANDGRAB = 10;
  crMonochrome = 15;
  crColorDraw = 16;
  crColorCross = 17;
  crCOLORZOOM = 18;
  crCOLORHAND = 19;
  crMyArrow = 26;
  strCrDraw = 'DRAWCURSOR';
  strCrColorDraw = 'COLORDRAW';
  strCrMyArrow = 'ARROW';
  strCrCross = 'CROSS';
  strCrColorCross = 'COLORCROSS';
  strCrTRANSPARENTARROW = 'TRANSPARENTARROW';
  strCrZOOM = 'ZOOM';
  strCrCOLORZOOM = 'COLORZOOM';
  strCrHAND = 'HAND';
  strCrCOLORHAND = 'COLORHAND';
  strCrHANDGRAB = 'HANDGRAB';

  WM_MyDropFiles = WM_User + 400;

  VK_0 = 48;
  VK_1 = 49;
  VK_2 = 50;
  VK_3 = 51;
  VK_4 = 52;
  VK_5 = 53;
  VK_6 = 54;
  VK_7 = 55;
  VK_8 = 56;
  VK_9 = 57;

  VK_A = 65;
  VK_B = 66;
  VK_C = 67;
  VK_D = 68;
  VK_E = 69;
  VK_F = 70;
  VK_G = 71;
  VK_H = 72;
  VK_I = 73;
  VK_J = 74;
  VK_K = 75;
  VK_L = 76;
  VK_M = 77;
  VK_N = 78;
  VK_O = 79;
  VK_P = 80;
  VK_Q = 81;
  VK_R = 82;
  VK_S = 83;
  VK_T = 84;
  VK_U = 85;
  VK_V = 86;
  VK_W = 87;
  VK_X = 88;
  VK_Y = 89;
  VK_Z = 90;

  ProgCornFile = 'Life32.RTF';

  LifeFilter = 'Life files|*.lif;*.xli;*.l;*.plf;*.rle;*.life;*.xlife|All files (*.*)|*.*';

  MoveToHint = 'Move to';

  cstrLastOpenedFiles = 'LastOpenFiles';
  cstrMRUList = 'MRUList';


  NumSpeeds = 19;

  idNW = 777;
  idN = 888;
  idNE = 999;
  idW = 444;
  idCenter = 555;
  idE = 666;
  idSW = 111;
  idS = 222;
  idSE = 333;
  idNulCenter = 1000;

  idDialogButton = 10;

  menuZoomCheck = 2200;

  menuPlay = 1301;
  menuStep = 1303;
  menuSnapshot = 1298;
  menuDraw = 2010;
  menuSelect = 2011;
  menuOpen = 1100;
  menuNew = 1220;
  menuRewind = 1299;
  menuAuto = 1199;
  menuRotate = 1013;

  cDontCreate = false;
  cMakeIt = true;
  cNoClipboard = false;
  cUseClipboard = true;

  cpUser = 0;
  cpDraw = 1;
  cpClear = 2;
  cpSelect = 3;
  cpPlay = 4;
  cpOpen = 5;
  cpRewind = 6;
  cpStep = 7;
  cpRotate = 8;
  cpAuto = 9;
  cpTorus = 10;
  cpFirst = cpUser;
  cpLast = cpAuto;

  RewindName: array[cpFirst..cpLast] of string =
    ('RewindUser','RewindDraw','RewindClear','RewindSelect',
     'RewindPlay','RewindOpen','RewindRewind','RewindStep',
     'RewindRotate','RewindAuto');

  coPatternID = 1;
  coRevision = 2;
  coGeneration = 3;
  coCause = 4;
  coName = 5;

  SnapPics: array[cpFirst..cpLast] of integer = (menuSnapShot, menuDraw,
            menuNew, menuSelect, menuPlay, menuOpen, menuRewind, menuStep,
            menuRotate, menuAuto);

  ColorError = $FFFEFDFC;

  cClearSel = true;
  cKeepSel = false;

  CmdShowMenu = $F200;
  CmdSysSeparator = $F300;

  emDraw = 2010;
  emSelect = 2011;
  emCursorZoom = 2012;
  emHand = 2013;

  lpmOr = 2001;
  lpmPut = 2002;
  lpmXor = 2003;
  lpmError = 2004;

type
  TSpeedsArray = array [0..NumSpeeds-1] of integer;

  TPasteMode = integer;
  TFillAction = (faClear, faFill, faInvert);


  TDDStates = integer;

const
  PasteHints: array [lpmOr..lpmError] of string =
  ('Paste mode:Transparent',
   'Paste mode:Opaque',
   'Paste mode:Invert',
   'Paste mode:Don''t overwrite');

var
  CF_LIFE32: UINT; //clipboard format

implementation

initialization
  CF_LIFE32:= RegisterClipboardFormat(CFSTR_LIFE32);
end.
