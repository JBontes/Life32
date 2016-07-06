unit u_InOut;


// LIFE
//  #N - Normal rules (Conway)
//  #R - Rules
//  #S - Speed
//  #D - Description, comment
//  #C - Description, comment
//  #P - The block position
//
// MCL
//  #GAME     - The game type
//  #RULE     - Rules as a text string
//  #N        - Game of Life, Conway's rules
//  #BOARD    - Board size
//  #WRAP     - 'Wrap at edges' status
//  #SPEED    - animation speed
//  #CCOLORS  - Count of colors
//  #PALETTE  - color palette
//  #COLORING - coloring method, 1/2
//  #DIV      - diversities
//  #D        - Description, comment
//  #L        - Data line

interface

uses
  Windows, Classes, u_CADefs;

const
  BORDER_EXTRAMARGIN = 600;

type
  TSaveAs = (IO_OPTIMAL, IO_MCL, IO_RLE, IO_LIF105, IO_LIF106, IO_MLF);

type
  TOpenParams = class
  private { Private declarations }

  published { Published declarations }

  public { Public declarations }
    fOpen: Boolean;         // Open flag (False - insert/append/ to the open file)
    fWarnNotFound: Boolean; // Issue a warning when the file could not befound
    fStringsReady: Boolean; // Don't load the disk file, strings are already prepared
    fOnlyPattern: Boolean;  // Load only the pattern, ignore rules and settings
    fApplyRules: Boolean;   // Apply family and rules?
    constructor Create;
    procedure Reset;
  end;

  TInOut = class
  private { Private declarations }
    m_IOCells: array of TCells; // loaded cells
    m_IOCellsCount: Integer; // count of loaded cells
    m_Family: TFamily; // the game
    m_sRules: String; // rules
    m_Description: TStringList; // the CA file description
    m_Speed: Integer; // game speed
    m_BSizeX,m_BSizeY: Integer; // board size
    m_Wrap: Integer; // wrapping state
    m_CColors: Integer; // count of colors
    m_Coloring: Integer; // coloring method
    m_sPalette: String; // colors palette file name
    m_dMclVsn: Double; // MCL file version

    m_LineStrings: TStringList; // list of strings representing the file

    m_SaveSpeed: Boolean;    // save speed with pattern?
    m_SaveBSize: Boolean;    // save board size with pattern?
    m_SaveWrap: Boolean;     // save wrapping state with pattern?
    m_SaveCColors: Boolean;  // save count of colors with pattern?
    m_SavePalNam: Boolean;   // save color palette name with pattern?
    m_SaveDiv: Boolean;      // save diversities with pattern?
    m_OnlySaveAs: Boolean;   // when saving always use the Save as... dialog
    m_OnlyTopRow1D: Boolean; // save only the top row of 1-D CA

    // OPEN interface
    function  ReadFileToStrings(sFilNam: String; fWarnNotFound: Boolean): Boolean;
    function  DoOpenLifeFile: Boolean;

    // Open MLF
    function  ReadFileMLF: Boolean;

    // Open MCL
    function  ProcessOneMCLLine(bff: String; var iCol, iRow, iNum: Integer): Boolean;
    function  ReadFileMCL: Boolean;

    // Open LIF105
    function  ProcessOneLIF105Line(bff: String; var iBlkX, iBlkY: Integer; var iRow: Integer): Boolean;
    function  ReadFileLIF105: Boolean;

    // Open LIF106
    function  ProcessOneLIF106Line(bff: String): Boolean;
    function  ReadFileLIF106: Boolean;

    // Open RLE
    function  ProcessOneRLELine(bff: String; var iCol, iRow, iNum: Integer; var fEndFlg, fXYFound: Boolean): Boolean;
    function  ReadFileRLE: Boolean;

    // Save RLE
    function  FormatRLEItem(cnt: Integer; typ: String): String;
    procedure UpdateRLEString(var sBff: String; sPfx: String; var cnt: Integer; typ: String);

    // Open PLF
    function  ReadFilePLF(sFilNam: String): Boolean;

    // Open SG
    function  GetSGVal(ch: Char): Byte;
    function  ReadFileSG: Boolean;

    // Save
    procedure MakeRLEStrings(rect: TRect; fFromBoard: Boolean);
    procedure MakeLIF105Strings(rect: TRect; fFromBoard: Boolean);
    procedure MakeLIF106Strings(rect: TRect; fFromBoard: Boolean);
    procedure MakeMLFStrings(rect: TRect; fFromBoard: Boolean);
    procedure MakeMCLStrings(rect: TRect; fFromBoard: Boolean);

  published { Published declarations }
    property SaveSpeed: Boolean    read m_SaveSpeed    write m_SaveSpeed;
    property SaveBSize: Boolean    read m_SaveBSize    write m_SaveBSize; // save board size with pattern?
    property SaveWrap:  Boolean    read m_SaveWrap     write m_SaveWrap; // save wrapping state with pattern?
    property SaveCColors: Boolean  read m_SaveCColors  write m_SaveCColors; // save count of colors with pattern?
    property SavePalNam: Boolean   read m_SavePalNam   write m_SavePalNam; // save color palette name with pattern?
    property SaveDiv: Boolean      read m_SaveDiv      write m_SaveDiv; // save diversities with pattern?
    property OnlyTopRow1D: Boolean read m_OnlyTopRow1D write m_OnlyTopRow1D;
    property OnlySaveAs: Boolean   read m_OnlySaveAs   write m_OnlySaveAs; // when saving always use the Save as... dialog


    // OPEN interface
    function OpenLifeFile(sFilNam: String; oprm: TOpenParams): Boolean;
    function ReadFileRule(sFilNam: String; var iFamIdx: TFamily): String;

    // SAVE interface
    procedure SaveFile(var sFilNam: String; filTyp: TSaveAs);

    // Clipboard interface
    function PutRectOnClipboard(rect: TRect): Boolean;
    function GetFromClipboard: Boolean;
    function OpenFromClipboard: Boolean;

    // Misc
    procedure AddCell(iCol, iRow: Integer; bVal: Byte);
    function  MinRectangle: TRect;
    function  GetAllFilter: String;

  public { Public declarations }
    constructor Create;
  end;

var
  InOut: TInOut;
  OpenParams: TOpenParams;

implementation

uses
  SysUtils, Dialogs, Math, u_Main, u_Board, u_Undo, u_Rules, u_Description,
  u_Selection, u_ColorPalette, u_ColorsBar, u_Diversities, uMWTools, uMWStrings,
  Controls, Forms, Clipbrd;


//------------------------------------------------------------------------------
// Construction / destruction
constructor TOpenParams.Create;
begin
  Reset;
end;
procedure TOpenParams.Reset;
begin
  fOpen         := True;  // Open flag (False - insert/append/ to the open file)
  fWarnNotFound := True;  // Issue a warning when the file could not befound
  fStringsReady := False; // Don't load the disk file, strings are already prepared
  fOnlyPattern  := False; // Load only the pattern, ignore rules and settings
  fApplyRules   := True;  // Apply game and rules?
end;
constructor TInOut.Create;
begin
  SetLength(m_IOCells, 0); // nothing loaded
  m_IOCellsCount := 0;
  m_Description := TStringList.Create; // the CA file description
  m_LineStrings := TStringList.Create; // list of strings representing the file
  OpenParams    := TOpenParams.Create; // Open parameters
end;
//------------------------------------------------------------------------------
// Published methods
procedure TInOut.AddCell(iCol, iRow: Integer; bVal: Byte);
begin
  Inc(m_IOCellsCount);
  if m_IOCellsCount > High(m_IOCells) then SetLength(m_IOCells, m_IOCellsCount+500);
  with m_IOCells[m_IOCellsCount] do
  begin
    col := iCol;
    row := iRow;
    val := bVal;
  end;
end;
//------------------------------------------------------------------------------
// Calculate the selection minimal rectangle
function TInOut.MinRectangle: TRect;
var
  rect: TRect;
  i: Integer;
begin
  rect.Left   := Board.MaxCell.x;
  rect.Right  := Board.MinCell.x;
  rect.Top    := Board.MaxCell.y;
  rect.Bottom := Board.MinCell.y;

  for i := 1 to m_IOCellsCount do
    with m_IOCells[i] do
    begin
      if rect.Left   > col then rect.Left   := col;
      if rect.Right  < col then rect.Right  := col;
      if rect.Bottom < row then rect.Bottom := row;
      if rect.Top    > row then rect.Top    := row;
    end;
  MinRectangle := rect;
end;
//------------------------------------------------------------------------------
// Get the Run &n/to n cycle(s)... files filter
function TInOut.GetAllFilter: String;
begin
  GetAllFilter := 'All CA files formats|*.mcl;*.life;*.lif;*.l;*.rle;*.plf;*.mlf' +
                  '|Life 1.05,1.06 files (*.LIF, *.LIFE)|*.life;*.lif' +
                  '|RLE files (*.RLE)|*.rle' +
                  '|dbLife files (*.L)|*.l' +
                  '|ProLife (*.PLF)|*.plf' +
                  '|MCell files (*.MCL, *.MLF)|*.mcl;*.mlf' +
                  '|All files (*.*)|*.*'
end;

///////////////////////////////////
// Opening
//

//------------------------------------------------------------------------------
// Interface function opening the given file
function TInOut.OpenLifeFile(sFilNam: String; oprm: TOpenParams): Boolean;
var
  i: Integer;
  ix, iy: Integer;
  rect: TRect;
  midPnt: TPoint;  // pattern center point
  dx, dy: Integer; // centering vector
  Save_Cursor: TCursor;
  fOpenOK: Boolean;
  fStringsOK: Boolean;
  fFileLoaded: Boolean; // file successfully processed?
begin
  fOpenOK := False;
  fFileLoaded := False;
  SetLength(m_IOCells, 0); // nothing loaded
  m_IOCellsCount := 0;
  m_Description.Clear;
  m_Family  := FAMI_LIFE; // Life
  m_sRules  := '23/3'; // standard Conway's rules
  m_Speed   := -1; // game speed
  m_BSizeX  := -1; // board size
  m_BSizeY  := m_BSizeX;
  m_Wrap    :=  0; // wrapping state
  m_CColors := -1; // count of colors
  m_Coloring := -1; // default
  m_sPalette:= ''; // colors palette file name
  m_dMclVsn :=  0; // MCL file version

  Save_Cursor := Screen.Cursor;

  try
    Screen.Cursor := crHourglass; // show hourglass cursor
    sFilNam := Trim(sFilNam);

    if oprm.fStringsReady then // strings were prepared outside
      fStringsOK := True
    else
      fStringsOK := ReadFileToStrings(sFilNam, oprm.fWarnNotFound);

    if fStringsOK then
    begin
      Diversities.m_Enabled := False; // they must be activated separately for each file
      fFileLoaded := DoOpenLifeFile;  // put found cells to the array of cells
    end;

  finally
    if fFileLoaded or (m_IOCellsCount > 0) then // anything loaded?
    begin
      Undo.AddItem(UDOEVT_OPEN); // new file about to be opened, add universe to the Undo list

      rect := MinRectangle; // get the bounding rectangle
      if oprm.fOpen then // open a file in a new universe
      begin
        if not oprm.fOnlyPattern then // rules/parameters should be applied
        begin
          Board.PrepareNew(False, True); // prepare the board

          if m_Wrap >= 0 then // wrapping at edges
            Board.Wrap := IntToBool(m_Wrap);

          if oprm.fApplyRules then // no rules - no count of states
          begin
            if m_CColors >= 2 then // count of colors
              Board.StatesCount := m_CColors;
          end;

          if m_Coloring in [1,2] then // coloring method
            Board.ColoringMethod := m_Coloring;

          if m_Speed >= 0 then     // game speed
            frmMain.SetInterval(m_Speed);

          if (m_BSizeX > 0) and (m_BSizeY > 0) then // board size specified
            Board.BoardSize := Point(m_BSizeX, m_BSizeY)
          else // pattern does not specify the size - check, if current fits
          begin
            ix := rect.Right-rect.Left + 1 + BORDER_EXTRAMARGIN;  // add some margin
            iy := rect.Bottom-rect.Top + 1 + BORDER_EXTRAMARGIN;

            if Board.BoardSize.x > ix then // if board was bigger, leave it
              ix := Board.BoardSize.x;
            if Board.BoardSize.y > iy then
              iy := Board.BoardSize.y;

            Board.BoardSize := Point(ix, iy);
          end;

          if oprm.fApplyRules then
          begin
            Pgm.Family := m_Family;     // remember: first the game, next rules!
            Board.Rules := Trim(m_sRules);
          end;

          if Length(m_sPalette) > 0 then // colors palette file name
          begin
            CrrClo.LoadFromFile(m_sPalette);
            frmColorBar.UpdateColors; // update the colors bar
            frmMain.UpdatePaletteUI;  // mark the active palette in menus
          end;

          // in case of MCell 1.0 increase the count of states
          if m_dMclVsn = 1 then // QQ can one compare doubles in Pascal?
          begin
            if Pgm.Family <> FAMI_CYCL then // here it was proper...
              Board.StatesCount := Board.StatesCount + 1;

            if Pgm.Family = FAMI_WLIF then
              RulesWLife.iClo := Board.StatesCount;
          end;

          frmMain.SetFileName(sFilNam, True); // file name
          frmMain.LastDir := ExtractFilePath(sFilNam); // store the folder for next time

          // add to history of opened files
          frmMain.MRUFileList.AddItem(sFilNam);
          frmMain.MRUFavFolders.AddItem(ExtractFilePath(sFilNam));

          // add description
          Description.Clear;
          for i := 0 to m_Description.Count - 1 do
          begin
            Description.Add(m_Description[i]);
          end;
        end;

        // center the pattern being loaded
        midPnt.x := (rect.Left + rect.Right + 1) div 2;
        midPnt.y := (rect.Top + rect.Bottom + 1) div 2;
        dx := -midPnt.x;
        if (Pgm.FamiType = FAMITYP_1D) then // 1-D
          dy := Board.MinCell.y - rect.Top // always to the top row
        else
          dy := -midPnt.y;

        for i := 1 to m_IOCellsCount do // append all cells
          with m_IOCells[i] do
            Board.SetCell(col+dx, row+dy, val); // set

        if not oprm.fOnlyPattern then // rules/parameters should be applied
        begin
          // any fitting after loading?
          case iOpnAct of
            OPNACT_CTR: Board.PanCenterPattern(False); // center shape
            OPNACT_FIT: Board.ZoomBestFit(False);      // Best fit
          end;

          if Undo.ResetOnNew then Undo.ClearAll; // reinitialize Undo
          fModified := False; // nothing modified so far
        end;
        frmMain.ShowFormCaption;
      end
      else // insert (append) without creating a new universe
      begin
        Selection.SelClear; // delete current selection
        Selection.SelFrame := rect;
        for i := 1 to m_IOCellsCount do // append all cells
          with m_IOCells[i] do
            Selection.AddCellAbs(col, row, val, False); // set

        // place the file in the upper left corner
        Selection.MoveFrameTo(Point(Board.OrgX + 5, Board.OrgY + 5), False);
        frmMain.cmdModeMarkClick(Nil); // go to mark mode
      end;

      // we are ready to show what was loaded
      Board.Redraw([DRAW_GRID, DRAW_CELLS]); // redraw the board
      fOpenOK := True;
    end
    else // no cells loaded
    begin
      MessageDlg('File ' + Chr(10) + '[' + sFilNam + ']' + Chr(10) + ' could not be loaded :(', mtWarning, [mbOK], 0);
    end;

    SetLength(m_IOCells, 0); // clean up
    m_IOCellsCount := 0;
    Screen.Cursor := Save_Cursor  // always restore to normal
  end;

  if fOpenOK then
  begin
    if oprm.fOpen then
      frmMain.PlayResSound('SND_OPEN')
    else
      frmMain.PlayResSound('SND_DROP');
  end;

  frmMain.UpdatePttInfoHint;
  OpenLifeFile := fOpenOK;
end;
//------------------------------------------------------------------------------
// Read the file to the list of strings
function TInOut.ReadFileToStrings(sFilNam: String; fWarnNotFound: Boolean): Boolean;
var
  sExt: String; // file extension (type)
  fOk: Boolean;
  vFile: System.TextFile;
  bff, tok: String;
  iPos: Integer;
begin
  fOk := False; // Let's be pesimistic (realistic?)
  m_LineStrings.Clear; // prepare the list of strings
  if FileExists(sFilNam) then
  begin
    sExt := AnsiUpperCase(ExtractFileExt(sFilNam));
    if sExt = '.PLF' then // ProLife - binary file
    begin
      ReadFilePLF(SFilNam);
      fOk := False; // a trick - in order no to perform DoOpenLifeFile()
    end
    else
    begin
      System.AssignFile(vFile, sFilNam);
      Reset(vFile); // open the original file
      ReadLn(vFile, bff); // get the first line
      fOk := True; // ok
      if Pos(Chr(10), bff) = 0 then // DOS file, read all lines
      begin
        m_LineStrings.Add(bff); // add the first line
        while not Eof(vFile) do // traverse the whole file
        begin
          ReadLn(vFile, bff); // get the line and parse it
          m_LineStrings.Add(bff);
        end;
      end
      else // UNIX file, bff stores the whole file, break it to lines
      begin
        iPos := Pos(Chr(10), bff); // up to a newline
        while iPos > 0 do
        begin
          tok := Copy(bff, 1, iPos - 1);
          m_LineStrings.Add(tok);
          bff := Copy(bff, iPos + 1, Length(bff) - iPos);
          iPos := Pos(Chr(10), bff);
        end;
      end;
      CloseFile(vFile);
    end;
  end
  else
    if fWarnNotFound then
      MessageDlg('File ' + Chr(10) + '[' + sFilNam + ']' + Chr(10) + ' not found', mtWarning, [mbOK], 0);

  ReadFileToStrings := fOk;
end;
//------------------------------------------------------------------------------
// Low-level function opening the file
function TInOut.DoOpenLifeFile: Boolean;
var
  fOk: Boolean;
  sBff, sFstLin: String;
  i: Integer;
begin
  fOk := False; // Let's be pesimistic (realistic?)
  SetLength(m_IOCells, 0); // nothing loaded
  m_IOCellsCount := 0;

  // remove leading empty lines first
  while (m_LineStrings.Count > 0) and (Length(m_LineStrings.Strings[0]) = 0) do
    m_LineStrings.Delete(0);

  if m_LineStrings.Count > 0 then // still any lines?
  begin
    // find the first non-comment line
    i := 0;
    sFstLin := '';
    while i < m_LineStrings.Count do
    begin
      sBff := Trim(m_LineStrings[i]);
      if (Length(sBff) = 0) or  StrStartsWith(sBff, '#C') or StrStartsWith(sBff, '#D') then
      begin
        Inc(i); // comment found, go on searching
      end
      else
      begin
        sFstLin := sBff;
        i := m_LineStrings.Count;
      end;
    end;

    if StrStartsWith(sFstLin, '#MCLife ') then // '#MCLife 1.0'
      fOk := ReadFileMCL       // *.MCL
    else if StrStartsWith(sFstLin, '#MCell ') then // '#MCell 2.0'
      fOk := ReadFileMCL       // *.MCL
    else if StrStartsWith(sFstLin, '#Life 1.05') then
      fOk := ReadFileLIF105    // *.LIF, *.LIFE
    else if StrStartsWith(sFstLin, '#P ') then
      fOk := ReadFileLIF105    // *.LIF, *.LIFE
    else if StrStartsWith(sFstLin, '#Life 1.06') then
      fOk := ReadFileLIF106    // *.LIF, *.LIFE
    else if StrStartsWith(sFstLin, 'x =') then
      fOk := ReadFileRLE       // RLE: *.L
    else if StrStartsWith(sFstLin, 'x=') then
      fOk := ReadFileRLE       // RLE: *.L
    else if StrStartsWith(sFstLin, '//') then
      fOk := ReadFileMLF       // *.MLF
    else if StrStartsWith(sFstLin, '#SG') then
      fOk := ReadFileSG;       // Semigraphics .*oO

    if not fOk then // nothing read, RLE without the 'x =' at the beginning?
    begin
      for i := 0 to m_LineStrings.Count - 1 do
        if StrStartsWith(m_LineStrings[i], 'x =') then
        begin
          fOk := ReadFileRLE; // RLE: *.L
          break;
        end;
    end;

    if not fOk then // nothing read, try LIF, it's most popular
      fOk := ReadFileLIF105; // *.LIF, *.LIFE

    if not fOk then // nothing read, try DOS *.lif
      fOk := ReadFileLIF106; // *.LIF, *.LIFE

    if not fOk then // nothing read, RLE?
      fOk := ReadFileRLE;    // RLE: *.L

    if not fOk then // The last chance: incomplete MCL
      fOk := ReadFileMCL;    // *.MCL
  end;

  DoOpenLifeFile := fOk;
end;
//------------------------------------------------------------------------------
// Read the rule rom the pattern file
function TInOut.ReadFileRule(sFilNam: String; var iFamIdx: TFamily): String;
var
  vFile: System.TextFile;
  bff, tok, sRules: String;
  i, iPos: Integer;
  tmpStrings: TStringList; // list of strings representing the file
begin
  sRules := '';
  iFamIdx := FAMI_LIFE;
  tmpStrings := TStringList.Create; // list of strings representing the file
  tmpStrings.Clear; // prepare the list of strings
  if FileExists(sFilNam) then
  begin
    System.AssignFile(vFile, sFilNam);
    Reset(vFile); // open the original file
    ReadLn(vFile, bff); // get the first line
    if Pos(Chr(10), bff) = 0 then // DOS file, read all lines
    begin
      tmpStrings.Add(bff); // add the first line
      while not Eof(vFile) do // traverse the whole file
      begin
        ReadLn(vFile, bff); // get the line
        tmpStrings.Add(bff);
      end;
    end
    else // UNIX file, bff stores the whole file, break it to lines
    begin
      iPos := Pos(Chr(10), bff); // up to a newline
      while iPos > 0 do
      begin
        tok := Copy(bff, 1, iPos - 1);
        tmpStrings.Add(tok);
        bff := Copy(bff, iPos + 1, Length(bff) - iPos);
        iPos := Pos(Chr(10), bff);
      end;
    end;
    CloseFile(vFile);
    for i := 0 to tmpStrings.Count - 1 do
    begin
      // #GAME - game type
      // #N    - Normal rules (Conway)
      // #R    - Rules string
      // #RULE - Rules string
      bff := tmpStrings[i];
      if StrStartsWith(bff, '#GAME') then // game
      begin
        iFamIdx := Pgm.CheckStringFamily(bff);
      end
      else if StrStartsWith(bff, '#RULE') then // specific rules
      begin
        tok := Copy(bff, 6, Length(bff));
        // long rule can be splitted into several lines
        sRules := sRules + Trim(tok); // set rules
      end
      else if StrStartsWith(bff, '#N') then // standard rules
      begin
        sRules := '23/3'; // standard Conway's rules
      end
      else if StrStartsWith(bff, '#R') then // specific rules
      begin
        tok := Copy(bff, 3, Length(bff));
        sRules := sRules + Trim(tok); // set rules
      end
      else if StrStartsWith(bff, 'x =') then // RLE rules
      begin
        iPos := Pos('rules', bff);  // rules
        if iPos > 0 then
        begin
          iPos := iPos + 5;
          sRules := GetNextToken(bff, iPos, ' =');
        end
        else
        begin
          iPos := Pos('rule', bff);
          if iPos > 0 then
          begin
            iPos := iPos + 4;
            sRules := GetNextToken(bff, iPos, ' =');
          end
        end;
      end
    end;
  end
  else
  begin
    MessageDlg('File ' + Chr(10) + '[' + sFilNam + ']' + Chr(10) + ' not found', mtWarning, [mbOK], 0);
  end;

  ReadFileRule := Trim(sRules);
end;
//------------------------------------------------------------------------------
// MLF - my own old temporary format
function TInOut.ReadFileMLF: Boolean;
var
  bff, tok: String;
  i, iCol, iRow: Integer;
  iPos: Integer;
begin
  ReadFileMLF := False;

  // analyze all lines
  for i := 0 to m_LineStrings.Count - 1 do
  begin
    bff := m_LineStrings[i];
    bff := Trim(bff);
    if Length(bff) > 0 then
      if Copy(bff, 1, 2) <> '//' then // it's not a comment
      begin
        iPos := Pos(',', bff);
        if iPos > 1 then
        begin
          tok := Copy(bff, 1, iPos - 1);   iRow := CvtStr2Int(tok);
          tok := Copy(bff, iPos + 1, 100); iCol := CvtStr2Int(tok);
          AddCell(iCol, iRow, 1);
          ReadFileMLF := True; // any cell added
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------
// Convert one Semigraphics character to the cell value
function TInOut.GetSGVal(ch: Char): Byte;
var
  bVal: Byte;
begin
  case ch of
    ' ': bVal := 0;
    '.': bVal := 0;
    '*': bVal := 1;
    'o': bVal := 2;
    'O': bVal := 3;
    'Q': bVal := 4;
    '@': bVal := 5;
    'W': bVal := 6;
    '#': bVal := 7
  else   bVal := 8;
  end;
  GetSGVal := bVal;
end;
//------------------------------------------------------------------------------
// #SG - Semigraphics .*oO
function TInOut.ReadFileSG: Boolean;
var
  i, j, iRow: Integer;
  sBff: String;
  bVal: Byte;
begin
  ReadFileSG := False;
  // analyze all lines
  iRow := 0;
  for i := 0 to m_LineStrings.Count - 1 do // for all rows
  begin
    sBff := Trim(m_LineStrings[i]);
    if StrStartsWith(sBff, '#SG') then
    begin
      ReadFileSG := True;
      sBff := Trim(Copy(sBff, 4, Length(sBff)));
      for j := 1 to Length(sBff) do
      begin
        bVal := GetSGVal(sBff[j]);
        if bVal > 0 then
          AddCell(j-1, iRow, bVal);
      end;
      Inc(iRow); // next line
    end;
  end;
end;
//------------------------------------------------------------------------------
// *.lif, *.life files line parser
// Interprets also mixed LIFE/RLE files
// Return True if at least one cell was added
function TInOut.ProcessOneLIF105Line(bff: String; var iBlkX, iBlkY: Integer; var iRow: Integer): Boolean;
var
  iPos: Integer;
  tok: String;
  iCol: Integer;
  i, j, iNum: Integer;
begin
  bff := Trim(bff);
  ProcessOneLIF105Line := False;
  if Length(bff) > 0 then
  begin
    if bff[1] in ['#', '!', '/'] then // special characters
    begin
      if StrStartsWith(bff, '#P') then // the block position
      begin
        iRow := 0;
        tok := Trim(Copy(bff, 3, Length(bff)));
        iPos := Pos(' ', tok); // blank separates x and y
        if iPos > 0 then
        begin
          iBlkX := CvtStr2Int(Copy(tok, 1, iPos - 1));
          iBlkY := CvtStr2Int(Copy(tok, iPos + 1, Length(tok) - iPos));
        end;
      end
      else if StrStartsWith(bff, '#N') then // standard rules
      begin
        m_sRules := '23/3'; // standard Conway's rules
      end
      else if StrStartsWith(bff, '#R') then // specific rules
      begin
        tok := Copy(bff, 3, Length(bff));
        m_sRules := tok; // set rules
      end
      else if StrStartsWith(bff, '#S') then // speed
      begin
        tok := Copy(bff, 3, Length(bff));
        m_Speed := CvtStr2Int(tok);
      end
      else if StrStartsWith(bff, '#D') or
              StrStartsWith(bff, '#C') or
              StrStartsWith(bff, '!') then // description
      begin
        if StrStartsWith(bff, '#D') or StrStartsWith(bff, '#C') then
        begin
          tok := Copy(bff, 3, Length(bff));
          if Length(tok) > 0 then // remove one leading blank
            if tok[1] = ' ' then
              tok := Copy(tok, 2, Length(tok)-1);
        end
        else // '!'
          tok := Copy(bff, 2, Length(bff));
        m_Description.Add(tok); // add the line
      end;
    end
    else // cells data line
    begin
      iCol := 0;
      iNum := 0;
      bff := Trim(bff); // in Life105 blanks are insignificant
      for i := 1 to Length(bff) do
      begin
        if bff[i] in ['0'..'9'] then
          iNum := iNum * 10 + CvtStr2Int(bff[i])
        else
        begin
          if iNum = 0 then iNum := 1;
          if bff[i] in  ['*', 'o', 'O'] then // alive cell
          begin
            for j := 0 to iNum-1 do
            begin
              AddCell(iCol + j + iBlkX, iRow + iBlkY, 1);
            end;
            ProcessOneLIF105Line := True;
            iCol := iCol + iNum;
          end
          else
          begin
            iCol := iCol + iNum;  // blank
          end;
          iNum := 0;
        end;
      end;
      Inc(iRow);
    end;
  end;
end;
//------------------------------------------------------------------------------
// Life 1.05
function TInOut.ReadFileLIF105: Boolean;
var
  bff: String;
  i, iRow: Integer;
  iBlkX, iBlkY: Integer; // Block left-top corner
begin
  ReadFileLIF105 := False;
  iBlkX := 0;
  iBlkY := 0;
  iRow := 0;

  // analyze all lines
  for i := 0 to m_LineStrings.Count - 1 do
  begin
    bff := m_LineStrings[i];
    if ProcessOneLIF105Line(bff, iBlkX, iBlkY, iRow) then
      ReadFileLIF105 := True;
  end;
end;
//------------------------------------------------------------------------------
// DOS *.lif files line parser
// Return True if at least one cell was added
function TInOut.ProcessOneLIF106Line(bff: String): Boolean;
var
  iCol, iRow: Integer;
  iPos: Integer;
  tok: String;
begin
  ProcessOneLIF106Line := False;
  bff := Trim(bff);
  if Length(bff) > 0 then
    if (bff[1] in ['#', '/', '.', '*']) then // it's a comment
    begin
      if StrStartsWith(bff, '#N') then // standard rules
      begin
        m_sRules := '23/3'; // standard Conway's rules
      end
      else if StrStartsWith(bff, '#R') then // specific rules
      begin
        tok := Copy(bff, 3, Length(bff));
        m_sRules := tok; // set rules
      end
      else if StrStartsWith(bff, '#S') then // speed
      begin
        tok := Copy(bff, 3, Length(bff));
        m_Speed := CvtStr2Int(tok);
      end
      else if StrStartsWith(bff, '#D') or
              StrStartsWith(bff, '#C') or
              StrStartsWith(bff, '!') then // description
      begin
        if StrStartsWith(bff, '#D') or StrStartsWith(bff, '#C') then
        begin
          tok := Copy(bff, 3, Length(bff));
          if Length(tok) > 0 then // remove one leading blank
            if tok[1] = ' ' then
              tok := Copy(tok, 2, Length(tok)-1);
        end
        else // '!'
          tok := Copy(bff, 2, Length(bff));
        m_Description.Add(tok); // add the line
      end;
    end
    else
    begin
      iPos := Pos(' ', bff);
      if iPos > 1 then
      begin
        tok := Copy(bff, 1, iPos - 1);   iCol := CvtStr2Int(tok);
        tok := Copy(bff, iPos + 1, 100); iRow := CvtStr2Int(tok);
        if Board.IsCellValid(iCol, iRow) then
        begin
          AddCell(iCol, iRow, 1);
          ProcessOneLIF106Line := True;
        end;
      end;
    end;
end;
//------------------------------------------------------------------------------
// Life 1.06 - DOS format, cell coordinates (x y)
// Return True if at least one cell was added
function TInOut.ReadFileLIF106: Boolean;
var
  bff: String;
  i: Integer;
begin
  ReadFileLIF106 := False;

  // analyze all lines
  for i := 0 to m_LineStrings.Count - 1 do
  begin
    bff := m_LineStrings[i];
    if ProcessOneLIF106Line(bff) then
      ReadFileLIF106 := True;
  end;
end;
//------------------------------------------------------------------------------
// *.l, *.rle files line parser
// Return True if at least one cell was added
function TInOut.ProcessOneRLELine(bff: String; var iCol, iRow, iNum: Integer; var fEndFlg, fXYFound: Boolean): Boolean;
const
  iniCol: Integer = 0;
var
  i, j, iTmp: Integer;
  sTmp: String;
begin
  bff := Trim(bff);
  ProcessOneRLELine := False;
  if StrStartsWith(bff, '#D') or StrStartsWith(bff, '#C') then // strange description
  begin
    bff := Copy(bff, 3, Length(bff));
    if Length(bff) > 0 then // remove one leading blank
      if bff[1] = ' ' then
        bff := Copy(bff, 2, Length(bff)-1);
    m_Description.Add(bff); // add the line
  end
  else
  begin
    if fEndFlg then // past the end - the description
    begin
      m_Description.Add(bff); // add the line
    end
    else
    begin
      if Length(bff) > 0 then
      begin
        if (not fXYFound) and (Pos('x', bff) = 1) then // the first line
        begin
          fXYFound := True;
          ProcessOneRLELine := True; // any line processed
          i := Pos('x', bff); // X size
          if i > 0 then
          begin
            i := i + 1;
            sTmp := GetNextToken(bff, i, ' =,');
            iCol := -(abs(CvtStr2Int(sTmp)) div 2);
            iniCol := iCol;
          end;

          i := Pos('y', bff); // Y size
          if i > 0 then
          begin
            i := i + 1;
            sTmp := GetNextToken(bff, i, ' =,');
            iRow := -(abs(CvtStr2Int(sTmp)) div 2);
          end;

          i := Pos('rules', bff);  // rules
          if i > 0 then
          begin
            i := i + 5;
            m_sRules := GetNextToken(bff, i, ' =');
          end
          else
          begin
            i := Pos('rule', bff);
            if i > 0 then
            begin
              i := i + 4;
              m_sRules := GetNextToken(bff, i, ' =');
            end
          end;

          i := Pos('fps', bff); // speed
          if i > 0 then
          begin
            sTmp := Copy(bff, 4, Length(bff));
            m_Speed := CvtStr2Int(sTmp);
          end;

          i := Pos('skip', bff); // ?
          if i > 0 then
          begin
            // QQtodo
          end;
        end
        else // the normal line
        begin
          for i := 1 to Length(bff) do
          begin
            if bff[i] in ['0'..'9'] then
              iNum := iNum * 10 + CvtStr2Int(bff[i])
            else
            begin
              if iNum = 0 then iNum := 1;
              case bff[i] of
                '$': begin
                       iRow := iRow + iNum;
                       iCol := iniCol;
                     end;
                'b','B','.':
                     iCol := iCol + iNum;  // blank
                '!': begin // end flag, the rest is the description
                       fEndFlg := True;
                       break;
                     end;
                 else
                     begin // probably a cell
                       if bff[i] in ['a'..'z','A'..'Z'] then
                       begin
                         case bff[i] of
                           'x', 'X': iTmp := 2;
                           'y', 'Y': iTmp := 3;
                           'z', 'Z': iTmp := 4;
                         else        iTmp := 1;
                         end;
                         for j := 0 to iNum-1 do
                           AddCell(iCol+j, iRow, iTmp);
                         iCol := iCol + iNum;
                         ProcessOneRLELine := True; // any cell added
                       end;
                     end;
              end;
              iNum := 0;
            end;
          end;
        end;
      end; // Length(bff)
    end; // !endFlg
  end;
end;
//------------------------------------------------------------------------------
// Read the file in RLE format
function TInOut.ReadFileRLE: Boolean;
var
  bff: String;
  i, iCol, iRow: Integer;
  iNum: Integer; // number of repetitions
  fEndFlg: Boolean; // '!' found, start of the comment
  fXYFound: Boolean;  // x= y= line found
begin
  ReadFileRLE := False;
  iCol := 0; iRow := 0; iNum := 0;
  fEndFlg := False;
  fXYFound := False;

  // analyze all lines
  for i := 0 to m_LineStrings.Count - 1 do
  begin
    bff := m_LineStrings[i];
      if ProcessOneRLELine(bff, iCol, iRow, iNum, fEndFlg, fXYFound) then
        ReadFileRLE := True; // any cell added
  end;
end;
//------------------------------------------------------------------------------
// *.MCL files line parser
// Return True if at least one cell was added
function TInOut.ProcessOneMCLLine(bff: String; var iCol, iRow, iNum: Integer): Boolean;
const
  iniCol: Integer = 0;
var
  i, j: Integer;
  tok: String;
  iAdd: Integer; // used for states > 24
begin
  bff := Trim(bff);
  ProcessOneMCLLine := False;
  iAdd := 0;

  if Length(bff) > 0 then
  begin
    if StrStartsWith(bff, '#RULE') then // specific rules
    begin
      tok := Copy(bff, 6, Length(bff));
      // long rule can be splitted into several lines
      m_sRules := m_sRules + Trim(tok); // set rules
      ProcessOneMCLLine := True; // rule defined
    end
    else if StrStartsWith(bff, '#GAME') then // game
    begin
      m_Family := Pgm.CheckStringFamily(bff);
      ProcessOneMCLLine := True; // game defined
    end
    else if StrStartsWith(bff, '#BOARD') then // board size
    begin
      tok := Copy(bff, 7, Length(bff));
      i := Pos('x', tok);
      if i > 0 then
      begin
        m_BSizeX := CvtStr2Int(Copy(tok, 1, i-1));
        m_BSizeY := CvtStr2Int(Copy(tok, i+1, 10));
      end;
    end
    else if StrStartsWith(bff, '#SPEED') then // speed
    begin
      tok := Copy(bff, 7, Length(bff));
      m_Speed := CvtStr2Int(tok);
    end
    else if StrStartsWith(bff, '#WRAP') then // wrap at edges
    begin
      tok := Copy(bff, 6, Length(bff));
      m_Wrap := CvtStr2Int(tok);
    end
    else if StrStartsWith(bff, '#CCOLORS') then // count of colors
    begin
      tok := Copy(bff, 9, Length(bff));
      m_CColors := CvtStr2Int(tok);
    end
    else if StrStartsWith(bff, '#COLORING') then // coloring method
    begin
      tok := Copy(bff, 10, Length(bff));
      m_Coloring := CvtStr2Int(tok);
    end
    else if StrStartsWith(bff, '#PALETTE') then // colors palette file name
    begin
      tok := Trim(Copy(bff, 9, Length(bff)));
      m_sPalette:= tok;
    end
    else if StrStartsWith(bff, '#DIV') then // diversities
    begin
      tok := Trim(Copy(bff, 5, Length(bff)));
      Diversities.ItemFromString(tok);
    end
    else if StrStartsWith(bff, '#D') then // description
    begin
      tok := Copy(bff, 3, Length(bff));
      if Length(tok) > 0 then // remove one leading blank
        if tok[1] = ' ' then
          tok := Copy(tok, 2, Length(tok)-1);
      m_Description.Add(tok); // add the line
    end
    else if StrStartsWith(bff, '#L') then // data line
    begin
      bff := Copy(bff, 3, Length(bff));
      for i := 1 to Length(bff) do
      begin
        if bff[i] in ['0'..'9'] then
          iNum := iNum * 10 + CvtStr2Int(bff[i])
        else
        begin
          if iNum = 0 then iNum := 1;
          case bff[i] of
            '$': begin
                   iRow := iRow + iNum;
                   iCol := iniCol;
                   iNum := 0;
                 end;

            'a'..'j': // a: 25..48, b: 49..72, c: 73..96, ..., j: 241..255
                 begin
                   iAdd := (Ord(bff[i]) - Ord('a') + 1) * 24;
                 end;

            'A'..'X': // cell in state 1..24
                 begin
                   for j := 0 to iNum-1 do
                     AddCell(iCol+j, iRow, Ord(bff[i]) - Ord('A') + 1 + iAdd);
                   iCol := iCol + iNum;
                   ProcessOneMCLLine := True; // any cell added
                   iAdd := 0;
                   iNum := 0;
                 end;

            '.': // blank
                 begin
                   iCol := iCol + iNum;
                   iNum := 0;
                 end;
            else
                 iNum := 0;
          end;
        end;
      end;
    end // data line
    else if StrStartsWith(bff, '#N') then // standard rules
    begin
      m_Family := FAMI_LIFE;
      m_sRules := '23/3'; // standard Conway's rules
    end;
  end; // Length(bff)
end;
//------------------------------------------------------------------------------
// Read the file in MCL format
function TInOut.ReadFileMCL: Boolean;
var
  bff: String;
  i, iCol, iRow: Integer;
  iNum: Integer; // number of repetitions
begin
  ReadFileMCL := False;
  iCol := 0; iRow := 0; iNum := 0;

  // determine the file version
  bff := m_LineStrings[0];
  if StrStartsWith(bff, '#MCLife ') then // '#MCLife 1.0'
  begin
    bff := Copy(bff, 9, 20);
    m_dMclVsn := CvtStr2Dbl(bff);
  end;
  if StrStartsWith(bff, '#MCell ') then // '#MCell 2.0'
  begin
    bff := Copy(bff, 8, 20);
    m_dMclVsn := CvtStr2Dbl(bff);
  end;

  // analyze all lines
  m_sRules  := ''; // no default rule
  for i := 0 to m_LineStrings.Count - 1 do
  begin
    bff := m_LineStrings[i];
      if ProcessOneMCLLine(bff, iCol, iRow, iNum) then
        ReadFileMCL := True; // any cell added
  end;
end;
//------------------------------------------------------------------------------
// PLF - ProLife binary file
function TInOut.ReadFilePLF(sFilNam: String): Boolean;
var
  fStream: TFileStream;
  siz: Integer;
  bVal: Byte;
  bytAry: array[0..511] of Byte;
  xMax, yMax, bytes: Integer;
  x, y: Integer;
begin
  ReadFilePLF := False;
  fStream := TFileStream.Create(sFilNam, fmOpenRead or fmShareDenyWrite);

  try
    siz := fStream.Size;
    if siz >= 6 then
    begin
      // first 2 bytes are insignificant
      fStream.Read(bVal, 1); fStream.Read(bVal, 1); // 2 dummies

      fStream.Read(bVal, 1); xMax := bVal;
      fStream.Read(bVal, 1); xMax := xMax + bVal * 256;
      fStream.Read(bVal, 1); yMax := bVal;
      fStream.Read(bVal, 1); yMax := yMax + bVal * 256;
      bytes := 2*(xMax+1);

      if 2*(xMax+yMax+xMax*yMax+4) = siz then
      begin
        for y := 0 to yMax do
        begin
          fStream.Read(bytAry, bytes);
          for x := 0 to bytes - 1 do
          begin
            bVal := bytAry[x xor 1];
            if bVal <> 0 then
            begin
              if ((bVal and   1) <> 0) then AddCell(x * 8 + 7, y, 1);
              if ((bVal and   2) <> 0) then AddCell(x * 8 + 6, y, 1);
              if ((bVal and   4) <> 0) then AddCell(x * 8 + 5, y, 1);
              if ((bVal and   8) <> 0) then AddCell(x * 8 + 4, y, 1);
              if ((bVal and  16) <> 0) then AddCell(x * 8 + 3, y, 1);
              if ((bVal and  32) <> 0) then AddCell(x * 8 + 2, y, 1);
              if ((bVal and  64) <> 0) then AddCell(x * 8 + 1, y, 1);
              if ((bVal and 128) <> 0) then AddCell(x * 8 + 0, y, 1);
            end;
          end;
        end;
      end
      else
      begin
        Application.MessageBox('Improper ProLife file!', 'Error', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
      end;
    end
    else
    begin
      Application.MessageBox('Improper ProLife file!', 'Error', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
    end;

  finally
    fStream.Free;
  end;

end;
//------------------------------------------------------------------------------

///////////////////////////////////
// Saving
//

//------------------------------------------------------------------------------
// Save the board in MLF format
procedure TInOut.MakeMLFStrings(rect: TRect; fFromBoard: Boolean);
var
  iCol, iRow: Integer;
  sBff: String;
begin
  m_LineStrings.Clear; // prepare the list of strings

  // first save the header
  m_LineStrings.Add('// Mirek''s Cellebration data file');
  m_LineStrings.Add('// MCell v.' + frmMain.lblVsn.GetLabelText);

  // next save the board
  for iCol := rect.Left to rect.Right do
    for iRow := rect.Top to rect.Bottom do
      if Board.GetCell(iCol, iRow) <> 0 then // clear marked cells
      begin
        sBff := Format('%d,%d', [iRow, iCol]); // swap order
        m_LineStrings.Add(sBff);
      end;
end;
//------------------------------------------------------------------------------
//
// Handy MCL conversions
//
//------------------------------------------------------------------------------
// Convert the cell's string representation to the value
function GetMCLCellVal(sStr: String): Byte;
var
  iAdd: Integer;
begin
  if sStr = '.' then
    GetMCLCellVal := 0
  else
  begin
    if Length(sStr) > 1 then
    begin
      iAdd := (Ord(sStr[1]) - Ord('a') + 1) * 24;
      GetMCLCellVal := Ord(sStr[2]) - Ord('A') + 1 + iAdd
    end
    else
      GetMCLCellVal := Ord(sStr[1]) - Ord('A') + 1;
  end;
end;
//------------------------------------------------------------------------------
// Return the string corresponding to the given value of a cell
function GetMCLCellChar(bVal: Byte): String;
var
  iRng: Integer;
  sRet: String;
begin
  sRet := '';
  if bVal = 0 then
  begin
    sRet := '.'
  end
  else
  begin
    iRng := (bVal - 1) div 24;
    if iRng > 0 then
      sRet := Chr(Ord('a') + iRng - 1);      // a..j
    sRet := sRet + Chr(Ord('A') + (bVal - iRng * 24 - 1)); // A..X
  end;
  GetMCLCellChar := sRet;
end;
function IsMCLCellChar(sStr: String): Boolean;
begin
  IsMCLCellChar := (sStr[1] in ['A'..'X', 'a'..'j']);
end;
//------------------------------------------------------------------------------
// Save the board in MCL format
procedure TInOut.MakeMCLStrings(rect: TRect; fFromBoard: Boolean);
var
  i, iCol, iRow: Integer;
  sBff: String;
  lastTyp, thisTyp: String; // $ - new line, b - blank, A..X - cells
  bCnt, nCnt: Integer; // blanks and new lines
  oCnt: Integer; // count of so far collected cells of one type
  bVal: Byte;
begin
  m_LineStrings.Clear; // prepare the list of strings

  // first save the header
  m_LineStrings.Add('#MCell 4.20');

  // next the game type
  sBff := Pgm.GetFamilyName(Pgm.Family);

  if Length(sBff) > 0 then
    m_LineStrings.Add('#GAME ' + sBff);

  // now rules
  sBff := Board.Rules;
  while Length(sBff) > 0 do
  begin
    m_LineStrings.Add('#RULE ' + LeftStr(sBff, 64));
    sBff := Copy(sBff, 65, Length(sBff) - 64);
  end;

  // board size
  if InOut.SaveBSize then
    m_LineStrings.Add('#BOARD ' + IntToStr(Board.BoardSize.x)
      + 'x' + IntToStr(Board.BoardSize.y));

  // speed
  if InOut.SaveSpeed then
    m_LineStrings.Add('#SPEED ' + IntToStr(frmMain.GetInterval()));

  // wrapping
  if InOut.SaveWrap then
    m_LineStrings.Add('#WRAP ' + IntToStr(BoolToInt(Board.Wrap)));

  // count of colors
  if InOut.SaveCColors then
    m_LineStrings.Add('#CCOLORS ' + IntToStr(Board.StatesCount));

  // color palette name
  if InOut.SavePalNam then
    m_LineStrings.Add('#PALETTE ' + CrrClo.name);

  // diversities
  if InOut.SaveDiv then
  begin
    m_LineStrings.Add('#DIV ' + Diversities.ItemAsString(DIV_SYSTEM));
    if Diversities.ItemActive(DIV_NOISE) then
      m_LineStrings.Add('#DIV ' + Diversities.ItemAsString(DIV_NOISE));
    if Diversities.ItemActive(DIV_BHOLE) then
      m_LineStrings.Add('#DIV ' + Diversities.ItemAsString(DIV_BHOLE));
    if Diversities.ItemActive(DIV_SNOVA) then
      m_LineStrings.Add('#DIV ' + Diversities.ItemAsString(DIV_SNOVA));
    if Diversities.ItemActive(DIV_STRIN) then
      m_LineStrings.Add('#DIV ' + Diversities.ItemAsString(DIV_STRIN));
  end;

  // save the description
  for i := 0 to Description.Count - 1 do
    m_LineStrings.Add('#D ' + Description[i]);

  // next save the board
  bCnt := 0; nCnt := 0; oCnt := 0;
  sBff := '';
  lastTyp := '$';
  for iRow := rect.Top to rect.Bottom do // for all rows
  begin
    frmMain.sttFldBoard.Caption := IntToStr(((iRow - rect.Top) * 100) div (rect.Bottom - rect.Top + 1)) + '%';
    Application.ProcessMessages;

    if (Pgm.FamiType <> FAMITYP_1D) or (not fFromBoard) or (not InOut.OnlyTopRow1D) or (iRow = rect.Top) then
    begin
      for iCol := rect.Left to rect.Right do // for all columns
      begin
        if fFromBoard then // from board
          bVal := Board.GetCell(iCol, iRow) // alive cell
        else // from selection
          bVal := Selection.GetCellVal(Point(iCol-rect.Left, iRow-rect.Top));

        thisTyp := GetMCLCellChar(bVal);

        if bVal > 0 then // alive cell
        begin
          if lastTyp <> thisTyp then // a change, output what was collected
          begin
            UpdateRLEString(sBff, '#L ', nCnt, '$');
            UpdateRLEString(sBff, '#L ', bCnt, '.');
            UpdateRLEString(sBff, '#L ', oCnt, lastTyp);
            oCnt := 1; // the first cell of this type
          end
          else
          begin
            Inc(oCnt); // the next cell of last type
          end;
          lastTyp := thisTyp;
        end
        else // a blank
        begin
          if IsMCLCellChar(lastTyp) then
            UpdateRLEString(sBff, '#L ', oCnt, lastTyp);
          Inc(bCnt);
          lastTyp := '.';
        end;
      end; // for columns
      // end of the row
      if IsMCLCellChar(lastTyp) then
        UpdateRLEString(sBff, '#L ', oCnt, lastTyp);
      bCnt := 0;
      oCnt := 0;
      Inc(nCnt);
      lastTyp := '$';
    end; // if all rows or this row
  end; // for rows
  m_LineStrings.Add('#L ' + sBff);
end;
//------------------------------------------------------------------------------
// Make strings representing the LIFE105 file
procedure TInOut.MakeLIF105Strings(rect: TRect; fFromBoard: Boolean);
var
  iCol, iRow: Integer;
  sBff: String;
  bVal: Byte;
begin
  m_LineStrings.Clear; // prepare the list of strings

  // first save the header
  m_LineStrings.Add('#Life 1.05');

  // now rules
  if Board.Rules = '23/3' then sBff := '#N'
  else                         sBff := '#R ' + Board.Rules;
  m_LineStrings.Add(sBff);

  // save the description
  for iRow := 0 to Description.Count - 1 do
    m_LineStrings.Add('#D ' + Description[iRow]);

  // next save the board
  sBff := Format('#P %d %d', [rect.Right - rect.Left + 1, rect.Bottom - rect.Top + 1]);
  for iRow := rect.Top to rect.Bottom do // for all rows
  begin
    sBff := '';
    for iCol := rect.Left to rect.Right do // for all columns
    begin
      if fFromBoard then // from board
        bVal := Board.GetCell(iCol, iRow) // alive cell
      else // from selection
        bVal := Selection.GetCellVal(Point(iCol-rect.Left, iRow-rect.Top));

      if bVal = 0 then sBff := sBff + '.'
      else             sBff := sBff + '*';
    end;

    // delete trailing '.'s
    while (Length(sBff) > 0) and (sBff[Length(sBff)] = '.') do
      sBff := Copy(sBff, 1, Length(sBff) - 1);

    // must be at least one '.'
    if Length(sBff) = 0 then
      sBff := '.';

    m_LineStrings.Add(sBff);
  end;
end;
//
// Make strings representing the LIFE106 file
procedure TInOut.MakeLIF106Strings(rect: TRect; fFromBoard: Boolean);
var
  iCol, iRow: Integer;
  sBff: String;
begin
  m_LineStrings.Clear; // prepare the list of strings

  // first save the header
  m_LineStrings.Add('#Life 1.06');

  // now rules
  if Board.Rules = '23/3' then sBff := '#N'
  else                         sBff := '#R ' + Board.Rules;
  m_LineStrings.Add(sBff);

  // next save the board
  for iCol := rect.Left to rect.Right do
    for iRow := rect.Top to rect.Bottom do
      if Board.GetCell(iCol, iRow) <> 0 then // clear marked cells
      begin
        sBff := Format('%d %d', [iCol, iRow]);
        m_LineStrings.Add(sBff);
      end;
end;
//------------------------------------------------------------------------------
// Format one RLE token
function TInOut.FormatRLEItem(cnt: Integer; typ: String): String;
var
  sRet: String;
begin
  if cnt > 0 then
  begin
    if      cnt = 1 then sRet := typ
    else if cnt = 2 then sRet := typ + typ
    else                 sRet := Format('%d%s', [cnt, typ]);
  end;
  FormatRLEItem := sRet;
end;
//------------------------------------------------------------------------------
// A new item must be appended to the RLE output,
// handle all that stuff
procedure TInOut.UpdateRLEString(var sBff: String; sPfx: String; var cnt: Integer; typ: String);
var
  sNewStr: String;
begin
  if cnt > 0 then
  begin
    sNewStr := FormatRLEItem(cnt, typ);
    if Length(sBff + sNewStr) > 70 then // time to write?
    begin
      m_LineStrings.Add(sPfx + sBff); // next line
      sBff := '';
    end;
    sBff := sBff + sNewStr;
    cnt := 0;
  end;
end;
//------------------------------------------------------------------------------
// Make strings representing the RLE file
procedure TInOut.MakeRLEStrings(rect: TRect; fFromBoard: Boolean);
var
  iCol, iRow: Integer;
  sBff: String;
  lastTyp: Char; // $ - new line, b - blank, o - cell
  bCnt, oCnt, nCnt: Integer; // blanks, cells and new lines
  bVal: Byte;
begin
  m_LineStrings.Clear; // prepare the list of strings

  // first save the header
  sBff := Format('x = %d, y = %d', [rect.Right - rect.Left + 1, rect.Bottom - rect.Top + 1]);
  if Board.Rules <> '23/3' then
    sBff := sBff + ', rule = ' + Board.Rules;
  m_LineStrings.Add(sBff);

  // next save the board
  bCnt := 0; oCnt := 0; nCnt := 0;
  sBff := '';
  lastTyp := '$';
  for iRow := rect.Top to rect.Bottom do // for all rows
  begin
    for iCol := rect.Left to rect.Right do // for all columns
    begin
      if fFromBoard then // from board
        bVal := Board.GetCell(iCol, iRow) // alive cell
      else // from selection
        bVal := Selection.GetCellVal(Point(iCol-rect.Left, iRow-rect.Top));

      if bVal <> 0 then // alive cell
      begin
        if lastTyp <> 'o' then // a change, output what was collected
        begin
          UpdateRLEString(sBff, '', nCnt, '$');
          UpdateRLEString(sBff, '', bCnt, 'b');
        end;
        Inc(oCnt);
        lastTyp := 'o';
      end
      else // a blank
      begin
        if lastTyp = 'o' then UpdateRLEString(sBff, '', oCnt, 'o');
        Inc(bCnt);
        lastTyp := 'b';
      end;
    end;
    // end of the row
    if lastTyp = 'o' then UpdateRLEString(sBff, '', oCnt, 'o');
    bCnt := 0;
    oCnt := 0;
    Inc(nCnt);
    lastTyp := '$';
  end;

  sBff := sBff + '!';
  m_LineStrings.Add(sBff);

  // save the description
  for iRow := 0 to Description.Count - 1 do
    m_LineStrings.Add(Description[iRow]);
end;
//------------------------------------------------------------------------------
// Save the board in the specified format
procedure TInOut.SaveFile(var sFilNam: String; filTyp: TSaveAs);
var
  vFile: System.TextFile;
  i: Integer;
  rect: TRect;
begin
  rect := MinRectangle; // get the bounding rectangle

  if filTyp = IO_OPTIMAL then // determine the optimal format
  begin
    if Pgm.Family <> FAMI_LIFE then // not standard Life
    begin
      filTyp := IO_MCL; // the only format that supports other games
    end
    else
    begin
      if rect.Right - rect.Left + 1 <= 80 then
        filTyp := IO_LIF105
      else
        filTyp := IO_RLE;
    end;
  end;

  // first put the board to strings
  case filTyp of
    IO_RLE:    MakeRLEStrings(Board.CalcPatternRect(), True);
    IO_MCL:    MakeMCLStrings(Board.CalcPatternRect(), True);
    IO_MLF:    MakeMLFStrings(Board.CalcPatternRect(), True);
    IO_LIF105: MakeLIF105Strings(Board.CalcPatternRect(), True);
    IO_LIF106: MakeLIF106Strings(Board.CalcPatternRect(), True);
  end;

  // my file formats have fixed extensions
  case filTyp of
    IO_MCL: sFilNam := ChangeFileExt(sFilNam, '.mcl');
    IO_MLF: sFilNam := ChangeFileExt(sFilNam, '.mlf');
  end;

  if fMakeBAK then // make .BAK copies
  begin
    DeleteFile(sFilNam + '.BAK'); // remove last .BAK
    RenameFile(sFilNam, sFilNam + '.BAK'); // create new
  end;

  // now put all strings to the disk file
  System.AssignFile(vFile, sFilNam);
  Rewrite(vFile); // open the original file

  // save strings to the file
  for i := 0 to m_LineStrings.Count - 1 do
    WriteLn(vFile, m_LineStrings[i]);

  CloseFile(vFile); // Done

  fModified := False; // nothing modified so far
  frmMain.ShowFormCaption;

  // add to history of opened files
  frmMain.MRUFileList.AddItem(sFilNam);
  frmMain.MRUFavFolders.AddItem(ExtractFilePath(sFilNam));
end;
//------------------------------------------------------------------------------

//////////////////////////////////////////////////
// Clipboard interface
//

//------------------------------------------------------------------------------
// Put the selection on the clipboard
function TInOut.PutRectOnClipboard(rect: TRect): Boolean;
begin
  PutRectOnClipboard := False;

  // first put the rectangle to strings
  if Pgm.Family <> FAMI_LIFE then
  begin
    MakeMCLStrings(rect, False);
  end
  else
  begin
    if rect.Right - rect.Left + 1 <= 80 then
      MakeLIF105Strings(rect, False)
    else
      MakeRLEStrings(rect, False);
  end;

  if m_LineStrings.Count > 0 then
  begin
    Clipboard.SetTextBuf(m_LineStrings.GetText);
    PutRectOnClipboard := True;
  end;
end;
//------------------------------------------------------------------------------
// Get the clipboard contents, put it to the selection
function TInOut.GetFromClipboard: Boolean;
var
  bff: String;
  i: Integer;
begin
  GetFromClipboard := False;
  SetString(bff, nil, 65535 + 1);

  // make strings out of the clipboard contents
  Clipboard.GetTextBuf(PChar(bff), Length(bff));
  if Length(bff) > 0 then
  begin
    m_LineStrings.SetText(PChar(bff));
    if DoOpenLifeFile then // put found cells to the array of cells
    begin
      GetFromClipboard := True;
      Selection.SelClear; // delete current selection
      Selection.SelFrame := MinRectangle; // get the bounding rectangle
      for i := 1 to m_IOCellsCount do // append all cells
        with m_IOCells[i] do
          Selection.AddCellAbs(col, row, val, False); // set

      // place the file in the upper left corner
      Selection.MoveFrameTo(Point(Board.OrgX + 15, Board.OrgY + 15), False);
      Selection.ShowFrame; // eventual marking frame
      Selection.DrawCellsDyn(True); // show cells dynamically
      frmMain.cmdModeMarkClick(Nil); // go to mark mode
    end
    else
    begin
      // QQ Beep
    end;
  end
  else
  begin
    // QQ Beep
  end;
end;
//------------------------------------------------------------------------------
// Get the clipboard contents, open it as a file
function TInOut.OpenFromClipboard: Boolean;
var
  bff: String;
begin
  OpenFromClipboard := False;
  SetString(bff, nil, 65535 + 1);

  // make strings out of the clipboard contents
  Clipboard.GetTextBuf(PChar(bff), Length(bff));
  if Length(bff) > 0 then
  begin
    m_LineStrings.SetText(PChar(bff));
    OpenParams.fOpen         := True;
    OpenParams.fWarnNotFound := False;
    OpenParams.fStringsReady := True;
    OpenParams.fOnlyPattern  := False;
    OpenParams.fApplyRules   := True;
    OpenLifeFile(SCLPBRD, OpenParams) // Load the file given as a parameter
  end
  else
  begin
    // QQ Beep
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

