unit Lifegen;

{$undef LifeDebug}

interface

uses
  Classes, SysUtils, WinTypes, WinProcs, Controls,Graphics,
  RotatBit, LifeCel, LifeHash, LifeRules,
  StdCtrls, Dialogs, ClipBrd, Snapshot, LifeUtil, LifeConst, GifImage;

type
  TUniverse = class;

  TRuleChangeEvent = procedure(Sender: TUniverse) of object;
  TSaveProgressEvent = procedure(Sender: TObject; Progress: integer; var Cancel: Boolean) of object;

  TUniverse = class(TObject)
  private
    living: TLifeCel;     // start of live list.
    hibernating: TLifeCel;// p1 or p2 blocks
    morgue: TLifeCel;     // empty blocks, not quite ready for cleanup.
    caretaker: TLifeCel;  // startingpoint in morgue for deletion.

    FHashTable: TLifeHash;  // lookuptable on coordinate.
    FNeighborhood: integer; //defaults to nbMoore.
    //FRuleString: string;
    FRules: TLifeRules;         // rules of the game.

    FCounter: Integer;
    FIsCounting: boolean;
    FUseCount: integer;
    FWriteLock: Boolean;
    FDescription: TStringList;
    FOnRuleChange: TRuleChangeEvent;
    FOnSaveProgress: TSaveProgressEvent;

    FClipRect: TRect;
    FLimit: TRect;
    FIsLimited: boolean;
    FTorusKind: TTorusKind;
    FDeadEdges: Boolean;
    FLimitChanged: Boolean;
    FCheckPeriod: boolean;
    FTerminated: boolean;
    FPatternName: string;
    FChecksumList: TList;
    FCenterPoint: TPoint;


    procedure Generate_p;
    procedure Generate_q;
    //function convertRules(ARule: string): string;
    procedure ImplementRules;
    procedure KillCage(c: TLifeCel);
    procedure TranquilizeCage(c: TLifeCel);
    function RattleCage(c: TLifeCel): boolean;
    procedure InsertCage(c: TLifeCel);
    function AllocateCage(x,y: Integer): TLifeCel;
    function GetBlockBitmask(x,y: integer): integer;
    function GetLifeCelSlowly(x,y: integer; force:boolean): TLifeCel;
    function GetLifeCel(x,y: integer; force:boolean): TLifeCel;
    function GetBlockIndex(x,y: integer):integer;
    procedure IncinerateCages(beNice: boolean);
    procedure SetRules(ARuleString: string);
    procedure SetNeighborhood(Value: integer);
    procedure SetPatternName(Value: string);
    function GetRules: string;
    procedure ShrinkSelRectFast(var ARect: TRect);
    procedure CorrectUniverse;
    procedure LimitTorus(TorusKind: TTorusKind; DeadEdges: boolean);
    function GetChecksum: integer;
    procedure DoChecksum;

    procedure ClearSmallRect(ARect: TRect);
    procedure ClearBigRect(ARect: TRect);
    procedure FillSmallRect(ARect: TRect);
    procedure FillBigRect(ARect: TRect);
    procedure InvertSmallRect(ARect: TRect);
    procedure InvertBigRect(ARect: TRect);

    procedure SetWriteLock(Value: Boolean);
    procedure SetLimit(Value: TRect);
    function GetLimitChanged: boolean;

    property WriteLock: Boolean read FWriteLock write SetWriteLock;
  public
    Display:TLifeCel;   // lijst met blokken in het zichtbare gebied.
    Qcycle: boolean;  // false = p kant nu, true = q kant nu.
    BackCorrect: boolean;

    constructor Create(MyRules: string; ANeighborhood: integer);
    destructor Destroy; override;
    procedure Release; virtual;
    procedure AddRef; virtual;

    procedure RemoveFromDisplay(c: TLifeCel);
    procedure ClearDisplay;
    procedure Generate(Direction: Boolean);
    procedure AddToDisplay(c: TLifeCel);
    procedure ChangeCelFast(x,y: integer; state: boolean);
    procedure ChangeCel(x, y:integer; state: boolean);
    function CelState(x, y: integer): boolean;
    procedure DrawLine(x1,y1,x2,y2: integer; DragState: TLineDrawState);

    function GetBoundingBoxFast: TRect;
    function GetBoundingBox: TRect;
    procedure ShrinkSelRect(var ARect: TRect);
    function IsSelEmpty(ARect: TRect): boolean;
    procedure ResetBoundingBox;

    procedure LoadFromStringList(AList: TStringList);
    function SaveToStringList(FileFormat: integer; IncludeTorusData: boolean): TStringList;
    procedure LoadFromBitmap(ABitmap: TBitmap);
    function SaveToBitmap: TBitmap; overload;
    function SaveToBitmap(const ClipRect: TRect): TBitmap; overload;
    function SaveToGifbitmap: TGifImage;

    procedure LoadFromFile(AFile: string);
    procedure SaveToFile(AFile: string; FileFormat: integer; IncludeTorusData: boolean);

    function MakeSnapshotCopy: TSnapshot;
    procedure RewindToSnapshot(AStream: TStream);

    function IsUniverseEqual(Universe2: TUniverse): boolean;

    procedure FillRandom(ARect: TRect; APercentage, PasteMode: integer);
    //function SnapShotRect(ARect: TRect): TLifeShape;
    function CutRect(ARect: TRect; UseClipboard: boolean): TUniverse;
    function CopyRect(ARect: TRect; UseClipboard: boolean): TUniverse;
    function InsertShape(AUniverse: TUniverse; APos: TPoint; PasteMode: TPasteMode): boolean;
    procedure PasteRect(AUniverse: TUniverse; APos: TPoint; PasteMode: TPasteMode);
    function Clone: TUniverse;
    procedure DrawBox(ARect: TRect);
    procedure FillRect(ARect: TRect; DoWhat: TFillAction);
    procedure Clear(ClearDesc: boolean);
    procedure RattleAllCages;
    procedure FreshenView;
    procedure StepBack;
    procedure FlipX;
    procedure FlipY;
    //procedure BackFlip;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;
    function CanPlay: boolean;
    function IsEmpty: boolean;
    function IsValidRule(ARule: string): boolean;
    function CountCelsNow: cardinal;

    function SaveProgress(Progress: Integer): Boolean;

    //function GetCelCount(OnReady: TNotifyEvent): integer;
    property OnRuleChange: TRuleChangeEvent read FOnRuleChange write FOnRuleChange;
    property OnSaveProgress: TSaveProgressEvent read FOnSaveProgress write FOnSaveProgress;
    property Locked: Boolean read FWriteLock;
    property RuleString: string read GetRules write SetRules;
    property Counter: integer read FCounter write FCounter;
    property ClipRect: TRect read FClipRect write FClipRect;
    property Description: TStringlist read FDescription write FDescription;
    property Limit: TRect read FLimit write SetLimit;
    property IsLimited: boolean read FIsLimited write FIsLimited;
    property LimitChanged: boolean read GetLimitChanged;
    property TorusKind: TTorusKind read FTorusKind write FTorusKind;
    property DeadEdges: boolean read FDeadEdges write FDeadEdges;
    property Neighborhood: integer read FNeighborhood write SetNeighborhood;
    property PatternName: string read FPatternName write SetPatternName;
    property CheckPeriod: boolean read FCheckPeriod write FCheckPeriod;
    property ChecksumList: TList read FChecksumList;
    property CenterPoint: TPoint read FCenterPoint write FCenterPoint;
    property IsCounting: boolean read FIsCounting;
    property Terminated: boolean read FTerminated write FTerminated;
  end;

  TChecksum = class(TObject)
  private
    FChecksum: integer;
    FGeneration: integer;
  public
    constructor Create(AChecksum, AGen: integer);
    property Checksum: integer read FChecksum;
    property Generation: integer read FGeneration;
  end;

var
  StopSnapshot: boolean = false;

implementation


uses
  LifeLoad, System.Types;

 {   The Crunch en Munch tables process a 4x4 block to get the
        inner 2x2 block.  Four results are stored in eacg entry.

        This requires some bit-masking in order to get results
        but the alternative is four seperate tables    }
type
  PLifeCel = ^TLifeCel;

var
  Crunch: array[0..65535] of smallint;  // p --> q lookup rules
  Munch: array[0..65535] of smallint;  // p <-- q lookup rules
  Rules: TRuleArray; // lookuptable for 1 cell.


constructor TChecksum.Create(AChecksum, AGen: integer);
begin
  inherited Create;
  FChecksum:= AChecksum;
  FGeneration:= AGen;
end;

//The underlying commented out code does not work, why ???
(*
procedure TUniverse.CorrectUniverse;

  procedure CorrectCelNeighbors(c: TLifeCel);
  var
    x2,y2: integer;
    x,y: integer;
  begin

    RattleCage(c);
    x:= (c.Coor.x * 16);  //pin
    y:= (c.coor.y * 16);  //pin
    if(Qcycle) then begin
      x2:= (x+16);  //pin
      y2:= (y+16);  //pin

      if not Assigned(c.E) then allocateCage(x2,y)
      else rattleCage(c.E);

      if not Assigned(c.SE) then allocateCage(x2,y2)
      else rattleCage(c.SE);

      if not Assigned(c.S) then allocateCage(x,y2)
      else rattleCage(c.S);
    end {if}
    else begin  //pcycle
      x2:=(x-16);  //pin
      y2:=(y-16);  //pin

      if not Assigned(c.W) then allocateCage(x2,y)
      else rattleCage(c.W);

      if not Assigned(c.NW) then allocateCage(x2,y2)
      else rattleCage(c.NW);

      if not Assigned(c.N) then allocateCage(x,y2)
      else rattleCage(c.N);
    end; {else}
  end;

var
  c: TLifeCel;
begin
  c:= living.next;
  while c <> living do begin
    CorrectCelNeighbors(c);
    c:= c.Next;
  end; {while}
end;   (**)

//this one does, although I don't know why.
procedure TUniverse.CorrectUniverse;
var
  c: TLifeCel;
  x1,y1: integer;
begin
  c:= living.next;
  while c <> living do begin
    x1:= (c.coor.x * 16);// + integer(QCycle);
    y1:= (c.coor.y * 16);// + integer(QCycle);
    changecel(x1+15,y1,CelState(x1+15,y1));
    changecel(x1+15,y1+15,CelState(x1+15,y1+15));
    changecel(x1,y1,CelState(x1,y1));
    changecel(x1,y1+15,CelState(x1,y1+15));
    c:= c.Next;
  end; {while}
end;


procedure TUniverse.FlipX;
var
  c: TLifeCel;
  Temp: integer;
  a: integer;
begin
  WriteLock:= true;
  a:= Integer(QCycle)*2;
  //First get everyone in the living list.
  RattleAllCages;
  //Next dismantle the hashtable
  FHashTable.Clear;

  //Now visit every cel and mirror it around it's X axis.
  c:= living.next;
  while c <> living do begin
    c.FlipX(QCycle);
    FHashTable.Store(c);
    c:= c.Next;
  end; {while}
  CorrectUniverse;
  with FClipRect do begin
    //in p cycle the rotation axes is 7.5 in q cycle it is 8.5
    Temp:= -Left+16+a;
    Left:= -Right+16+a;
    Right:= Temp;
  end; { with }
  WriteLock:= false;
end;

procedure TUniverse.FlipY;
var
  c: TLifeCel;
  Temp: integer;
  a: integer;
begin
  WriteLock:= true;
  a:= Integer(QCycle)*2;
  //First get everyone in the living list.
  RattleAllCages;
  //Next dismantle the hashtable
  FHashTable.Clear;

  //Now visit every cel and mirror it around it's X axis.
  c:= living.next;
  while c <> living do begin
    c.FlipY(QCycle);
    FHashTable.Store(c);
    c:= c.Next;
  end;
  CorrectUniverse;
  //in p cycle the rotation axes is 7.5 in q cycle it is 8.5
  with FClipRect do begin
    Temp:= -Top+16+a;
    Top:= -Bottom+16+a;
    Bottom:= Temp;
  end; { with }
  WriteLock:= false;
end;

(*procedure TUniverse.BackFlip;
var
  c: TLifeCel;
  x1,y1: integer;
  Temp: integer;
begin
  //First get everyone in the living list.
  RattleAllCages;
  //Next dismantle the hashtable
  FHashTable.Clear;

  //Now visit every cel and mirror it around it's X axis.
  c:= living.next;
  while c <> living do begin
    c.BackFlip(QCycle);
    FHashTable.Store(c);
    c:= c.Next;
  end;
  c:= living.next;
  CorrectUniverse;
  with FClipRect do begin
    Temp:= -Top+15;
    Top:= -Bottom+15;
    Bottom:= Temp;
    Temp:= -Left+15;
    Left:= -Right+15;
    Right:= Temp;
  end; { with }
end;(**)

procedure TUniverse.Rotate90;
var
  c: TLifeCel;
  Temp: integer;
  a: integer;
begin
  WriteLock:= true;
  a:= integer(QCycle)*2;
  RattleAllCages;
  FHashTable.Clear;
  c:= living.next;
  while c <> living do begin
    c.FlipX(QCycle);
    c.BackFlip(QCycle);
    FHashTable.Store(c);
    c:= c.Next;
  end;
  CorrectUniverse;
  //in p cycle the rotation axes is 7.5 in q cycle it is 8.5
  with FClipRect do begin
    Temp:= Left;
    Left:= -Bottom+16+a;
    Bottom:= Right;
    Right:= -Top+16+a;
    Top:= Temp;
  end; { with }
  with FLimit do begin
    Temp:= Left;
    Left:= -Bottom+16+a;
    Bottom:= Right;
    Right:= -Top+16+a;
    Top:= Temp;
  end; { with }
  WriteLock:= false;
end;

procedure TUniverse.Rotate180;
var
  c: TLifeCel;
  Temp: integer;
  a: integer;
begin
  WriteLock:= true;
  a:= integer(QCycle)*2;
  RattleAllCages;
  FHashTable.Clear;
  c:= living.next;
  while c <> living do begin
    c.FlipY(QCycle);
    c.FlipX(QCycle);
    FHashTable.Store(c);
    c:= c.Next;
  end;
  CorrectUniverse;
  //in p cycle the rotation axes is 7.5 in q cycle it is 8.5
  with FClipRect do begin
    Temp:= Left;
    Left:= -Right +16+a;
    Right:= -Temp + 16+a;
    Temp:= Top;
    Top:= -Bottom + 16+a;
    Bottom:= -Temp +16+a
  end; { with }
  with FLimit do begin
    Temp:= Left;
    Left:= -Right +16+a;
    Right:= -Temp + 16+a;
    Temp:= Top;
    Top:= -Bottom + 16+a;
    Bottom:= -Temp +16+a
  end; { with }
  WriteLock:= false;
end;

procedure TUniverse.Rotate270;
var
  c: TLifeCel;
  Temp: integer;
  a,b: integer;
  TempRect: TRect;
begin
  WriteLock:= true;
  a:= integer(QCycle);
  b:= a*2;
  RattleAllCages;
  FHashTable.Clear;
  c:= living.next;
  while c <> living do begin
    c.FlipY(QCycle);
    c.BackFlip(QCycle);
    FHashTable.Store(c);
    c:= c.Next;
  end;
  CorrectUniverse;
  //in p cycle the rotation axes is 7.5 in q cycle it is 8.5
  with FClipRect do begin
    Temp:= Left;
    Left:= Top;
    Top:= -Right + 16+b;
    Right:= Bottom;
    Bottom:= -Temp+16+b;
  end; { with }
  with Limit do begin
    Temp:= Left;
    TempRect.Left:= Top;
    TempRect.Top:= -Right + 16+b;
    TempRect.Right:= Bottom;
    TempRect.Bottom:= -Temp+16+b;
  end; { with }
  Limit:= TempRect;
  WriteLock:= false;
end;

constructor TUniverse.Create(MyRules: string; ANeighborhood: integer);
begin
  inherited Create;
  Qcycle:= false;
  BackCorrect:=false;
  FUseCount:= 1;
  FPatternName:= '';

  living:= TLifeCel.Create;
  living.next:= living;
  living.prev:= living;
  living.displaynext:= living;
  living.displayprev:= living;

  morgue:= TLifeCel.Create;
  morgue.next:= morgue;
  morgue.prev:= morgue;
  morgue.displaynext:= morgue;
  morgue.displayprev:= morgue;

  hibernating:= TLifeCel.Create;
  hibernating.next:= hibernating;
  hibernating.prev:= hibernating;
  hibernating.displaynext:= hibernating;
  hibernating.displayprev:= hibernating;


  display:= TLifeCel.Create;
  display.next:= display;
  display.prev:= display;
  display.DisplayNext:= Display;
  display.DisplayPrev:= display;
  caretaker:=morgue;

  FHashTable:= TLifeHash.Create;
  FDescription:= TStringList.Create;
  FChecksumList:= TList.Create;
  FTorusKind:= DefaultTorusKind;
  FDeadEdges:= DefaultDeadEdges;
  FNeighborhood:= ANeighborhood;
  FRules:= TLifeRules.Create;
  SetRules(MyRules);
  ImplementRules;
end;

destructor TUniverse.Destroy;
begin
  Clear(true);
  FRules.Free;
  FHashTable.Free;
  FDescription.Free;
  morgue.FreeSlow;
  living.FreeSlow;
  hibernating.FreeSlow;
  display.FreeSlow;
  inherited Destroy;
end;

procedure TUniverse.Release;
begin
  Dec(FUseCount);
  if FUseCount = 0 then Destroy;
end;

procedure TUniverse.AddRef;
begin
  Inc(FUseCount);
end;

procedure TUniverse.SetWriteLock(Value: Boolean);
begin
  try
    //if we try to get a writelock while one is still on, raise an exception
    if (FWriteLock and Value) then begin
      FWriteLock:= false; //don't stop all action.
      LifeCel.MyDDSurface.DirectDrawEnabled:=
        LifeCel.MyDDSurface.DirectDrawEnabled or ddTempDisabled;
      //raise Exception.Create('Universe is locked for writing');
    end
    else FWriteLock:= Value;
    finally
      LifeCel.MyDDSurface.DirectDrawEnabled:=
        LifeCel.MyDDSurface.DirectDrawEnabled and not ddTempDisabled;
  end;
end;

procedure TUniverse.SetLimit(Value: TRect);
begin
  if not(CompareMem(@FLimit, @Value, SizeOf(Value))) then begin
    FLimit:= Value;
    FLimitChanged:= true;
  end;
end;

function TUniverse.GetLimitChanged: boolean;
begin
  Result:= FLimitChanged;
  FLimitChanged:= false;
end;


//The Hexlife Rule
//--------
//
//With that in mind, I started playing around with rules based on
//different-shaped neighborhoods, but insisting on symmetry.  Taking symmetries
//into account, there are three ways each of getting 2, 3, and 4 neighbors,
//for a total of 13 different neighborhoods.  I tried a lot of variations, but
//the one I settled on only distinguishes between different ways of getting two
//neighbors.  For these, one can use the organic chemists' naming convention
//for benzene-ring disubstitutions: ortho, meta, and para.  Rich Schroeppel and
//Achim Flammenkamp suggested this, and it may be a useful mnemonic for some
//people.  It has the disadvantage that there is no standard extension to
//subsets of 3 or 4 neighbors.  Fortunately, we don't need this for the rule
//used here.  So let's call these neighborhoods 2o, 2m, and 2p
//as follows:
//
//       O O            O .            O .
// 2o:  . x .     2m:  . x O     2p:  . x .
//       . .            . .            . O
//
//Extending the birth/survival convention used above, the rule is B2o/S2m34.
//
//That is, we always have survival but not birth on 3 or 4 neighbors.  On
//two neighbors we have birth but not survival if the neighbors are adjacent (2o),
//and survival but not birth if there is one empty neighbor cell in between (2m).
//If they are opposite each other (2p), there is no birth or survival (which
//is also the case for 0, 1, 5, or 6 neighbors).
//
//I settled on these rules in order to get a glider. It uses the only really
//small mechanism I'm aware of.  I consider it crucial that gliders show
//up spontaneously from random starting states.



//About the Just-friends rule:
//The most interesting rule I found was this one:
//
//A cell stays alive only if it has 1 or 2 live neighbors (in any position).
//A cell is born only if it has exactly two live neighbors which are not
//adjacent vertically or horizontally.
//
//So in the following figure, the central cell is born if there are live
//cells at any two cells marked ac, ad, ae, af, ag, bd, be, bf, bg, bh,
//ce, cf, cg, ch, df, dg, dh, eg, eh, or fh.
//	abc  876
//	hid  543
//	gfe  210


(*function TUniverse.convertRules(ARule: string): string;
const
  none = ' ';
var
  NewRule: string;
  s: array [0..9] of boolean;
  b: array [0..9] of boolean;
  ch, chold: char;
  HexS,HexB: char; //handle special HexLife additions, see hexrule above
  unknown: boolean;
  s_first: boolean;
  slash: boolean;
  i: integer;
  neighbors: integer;
  MaxNeighbors: char;
  Prefix: string;
  NewNeighborhood: integer;


begin
  HexS:= none; HexB:= none; //assume no special hexlife handling required.
  if ARule = '' then ARule:= DefaultRules;
  NewRule:= '';
  unknown:= true;   // unknown whether S comes first, or B
  s_first:= true;   // S comes first, by default
  slash:= false;    // past the slash yet?

  //strip off prefix
  i:= Pos(':',ARule);
  if i = 0 then Prefix:= '' //default is Moore neighborhood.
  else begin
    Prefix:= Copy(ARule,1,i);
    Delete(Arule,1,i);
  end; {else}

  NewNeighborhood:= StrToNeighborhood(Prefix);
  if NewNeighborhood = nbUnchanged then NewNeighborhood:= Neighborhood;
  MaxNeighbors:= IntToStr(MaxNeighborsInNeighborhood(NewNeighborhood))[1];

  for i:= 0 to 9 do begin
    s[i]:= false;
    b[i]:= false;
  end; {for i}

  chold:= ' ';
  for i:= 1 to Length(ARule) do begin
    ch:= ARule[i];
    if (ch ='b') or (ch ='B') then begin
      if (unknown) then begin
        unknown:=false;
        if (slash) then s_first:= true
        else s_first:= false;
      end {if}
      else begin
        if ((slash and not(s_first)) or (not(slash) and s_first)) then begin
          Result:= newrule;  // conflicting data
          Exit;
        end; {if}
      end {else}
    end {if}
    else if (ch ='s') or (ch ='S') then begin
      if (unknown) then begin
        unknown:= false;
        if(slash) then s_first:=false
        else s_first:= true;
      end {if}
      else begin
        if ((slash and s_first) or (not(slash) and  not(s_first))) then begin
          Result:= newrule; // conflicting data
          exit;
        end; {if}
      end; {else}
    end {else if}
    else if(ch in ['/','\']) then begin
      if(slash) then begin
        Result:= newrule;          // more than 1 slash
        exit
      end;
      slash:= true;
    end {else if}
    else if (ch =' ') then break  // ignore the rest
    //if rule is HexLife then characters m,o,p are also allowed, see hexrule above
    else if (NewNeighborhood = nbHex) and (chold = '2') and (Upcase(ch) in ['M','O','P']) then
    begin
      if s_first and not (slash) then HexS:= UpCase(ch)
      else if s_first and (slash) then HexB:= Upcase(ch)
      else if not(s_first) and not (slash) then HexB:= Upcase(ch)
      else HexS:= Upcase(ch);
    end {else}
    else if (ch <'0') or (ch >=Succ(MaxNeighbors)) then begin
      Result:= newrule;  // invalid character
      exit;
    end; {else if}
    chold:= ch; //remember previous char
  end; {for i}

  for i:= 1 to Length(ARule) do begin
    ch:=ARule[i];

    if (ch >='0') and  (ch<= MaxNeighbors) then begin
      if (s_first) then s[integer(ch)-$30]:= true
      else b[integer(ch)-$30]:= true;
    end {if}
    else if(ch in ['/','\']) then s_first:= not(s_first);
  end; {for i}

  if (b[0]) then begin
    Result:= newrule;   // no 0 allowed in born list
    exit;
  end; {if}

  for i:= 0 to 9 do if s[i] then begin
    NewRule:= NewRule + Chr(i+$30);
    if (i = 2) and (HexS <> ' ') then NewRule:= NewRule + HexS;
  end; {for i}
  newrule:= NewRule + '/';
  for i:= 1 to 9 do if b[i] then begin
    NewRule:= NewRule + Chr(i+$30);
    if (i = 2) and (HexB <> ' ') then NewRule:= NewRule + HexB;
  end; {for i}


  for i:= 0 to 511 do begin
    neighbors:= CountNeighbors(i,NewNeighborhood);
    if NewNeighborhood = nbJustFriends then begin
      Rules[i]:= ((Neighbors = 1) or (Neighbors = 2)) and JustFriendsAlive(i,NewNeighborhood);
    end {if}
    else begin
      if (Bool(i and bit4)) then begin
        Rules[i]:= s[neighbors]; {-1}
        if (Neighbors = 2) and (HexS <> ' ') and
           (HexRule2mop(i,NewNeighborhood) <> HexS) then Rules[i]:= false;
      end
      else begin
        Rules[i]:= b[neighbors];
        if (Neighbors = 2) and (HexB <> ' ') and
           (HexRule2mop(i,NewNeighborhood) <> HexB) then Rules[i]:= false;
      end; {else}
    end; {else}
  end; {for i}

  Result:= lowercase(prefix+newrule);
end; (**)

(*function TUniverse.AreRulesOK: boolean;
var
  i: integer;
  Neighbors: integer;
begin
  Result:= true;
  for i:= 0 to 511 do begin
    Neighbors:= CountNeighbors(i);
    if Rules[i] then begin
      if (Neighbors < 2) then Result:= false;
      if (Neighbors > 3) then Result:= false;
      if (not((i and $0010)>0) and (Neighbors = 2)) then Result:= false;
    end
    else begin
      if (Neighbors = 3) then Result:= false;
      if ((i and $0010)>0) and (Neighbors = 2) then Result:= false;
    end; {else}
  end; {for i}
end;   (**)

function TUniverse.IsValidRule(ARule: string): boolean;
begin
  Result:= FRules.IsValidRule(ARule);
end;


procedure TUniverse.generate_p;
var
  c, cnext, cS, cSE, cE: TLifeCel;
  cqstate: integer;
  cSpstate, cSEpstate, cEpstate: integer;
  x,y,xp, yp: Integer;
  xor1,xor2,xor3: integer;
  ix00, ix02, ix10, ix12,              // 4x4 neighborhoods
  ix01, ix03, ix11, ix13,              // extracted from a
  ix20, ix22, ix30, ix32,              // full 8x8 block,
  ix21, ix23, ix31, ix33: integer;     // for table lookups.
  n0, n1, n2, n3: integer;      // table lookup results
begin
  //This should never be called directly, so no check for writelock here.
  {$ifdef lifeDebug}
  DebugMemo.Lines.Add('*** P->Q  generate_p');
  {$endif}

  { For each cage: }

  c:= Living.next;
  while c <> living do begin
    cnext:= c.next;
    cS:=c.S;
    cE:=c.E;
    cSE:=c.SE;
    cqstate:= c.qstate;

    if Assigned(cS) then cSpstate:=cS.pstate
    else cSpstate:= -1;

    if Assigned(cE) then cEpstate:=cE.pstate
    else cEpstate:= -1;

    if Assigned(cSE) then cSEpstate:=cSE.pstate
    else cSEpstate:= -1;

    //if c.pstate hibernating and Spstate toprow hibernating and.
    //Epstate rightedge hibernating and SEpstate corner hibernating than,
    //have a closer look.
    if (((c.pstate and  $08080808)= $08080808) and ((cSpstate and $02000200)= $02000200)
      and ((cEpstate and $04040000)= $04040000) and ((cSEpstate and $01000000)=$01000000))
    then begin
      //if c and neighbors are dead than
      if(((c.pstate and  $80808080)=$80808080) and ((cSpstate and $20002000)=$20002000)
        and ((cEpstate and $40400000)=$40400000) and ((cSEpstate and $10000000)=$10000000))
      then begin
        //p and q in morgue
        c.flags:= c.flags or $c000;
        //if p and q in morgue and rattling bit is off, than destroy c.
        if((c.flags and $f800)=$f000) then killCage(c);
        //all dead.
        cqstate:= $ffffffff;
      end {if}
      //if hiberanting, but not empty, then put to sleep next time.
      else begin
        c.flags:= c.flags or $4000;
        if((c.flags and $5800)=$5000) then tranquilizeCage(c);
        //all sleeping.
        cqstate:= cqstate or $0f0f0f0f;
      end; {else}
      c.flags:= c.flags and $f7ff;  // Reset the Rattling bit
    end {if}
    else begin
      c.flags:= c.flags and $07ff;
      x:= c.coor.x * 16;  {maal 16}
      y:= c.coor.y * 16;  {maal 16}
      xp:= (x+16);  //pin
      yp:= (y+16);  //pin

      //  if  ##|
      //      --+ not hibernating then.
      if((c.pstate and $08020401) <> $08020401) then begin // first 8x8 active

        ix00:= (c.p[0]) and $ffff;   // darn signed arithmetic!  // 00 11
        ix10:= (c.p[1]) and $ffff;                               // 00 11
        ix20:= (c.p[2]) and $ffff;                               // 22 33
        ix30:= (c.p[3]) and $ffff;                               // 22 33

        ix02:= (ix00 and $00ff) or (ix10 and $ff00);      // 01 18
        ix12:= (ix10 and $00ff) or (c.p[8] and $ff00);    // 01 18
        ix22:= (ix20 and $00ff) or (ix30 and $ff00);      // 23 3A
        ix32:= (ix30 and $00ff) or (c.p[10] and $ff00);   // 23 3A

        ix01:= (ix00 and $0f0f) or (ix20 and $f0f0);      // 00 11
        ix11:= (ix10 and $0f0f) or (ix30 and $f0f0);      // 22 33
        ix21:= (ix20 and $0f0f) or (c.p[4] and $f0f0);    // 22 33
        ix31:= (ix30 and $0f0f) or (c.p[5] and $f0f0);    // 44 55

        ix03:= (ix01 and $00ff) or (ix11 and $ff00);                         // 01 18
        ix13:= (ix12 and $0f0f) or (ix32 and $f0f0);                         // 23 3A
        ix23:= (ix21 and $00ff) or (ix31 and $ff00);                         // 23 3A
        ix33:= (ix32 and $0f0f) or (ix31 and $00f0) or (c.p[12] and $f000);  // 45 5C

        n0:=   (($f000 and Crunch[ix00])
             or ($0f00 and Crunch[ix01])
             or ($00f0 and Crunch[ix02])
             or ($000f and Crunch[ix03]));

        n1:=   (($f000 and Crunch[ix10])
             or ($0f00 and Crunch[ix11])
             or ($00f0 and Crunch[ix12])
             or ($000f and Crunch[ix13]));

        n2:=   (($f000 and Crunch[ix20])
             or ($0f00 and Crunch[ix21])
             or ($00f0 and Crunch[ix22])
             or ($000f and Crunch[ix23]));

        n3:=   (($f000 and Crunch[ix30])
             or ($0f00 and Crunch[ix31])
             or ($00f0 and Crunch[ix32])
             or ($000f and Crunch[ix33]));

              // qstate bitmap
              // My 8x8 or Ver.2x8 or Hor.8x2 or Corner

        xor1:= c.q[1] xor n1;
        xor2:= c.q[2] xor n2;
        xor3:= c.q[3] xor n3;

        if ((xor3 and $000f) = 0) then begin
          if ((n3 and $000f) = 0) then         // SE 2x2 corner
            cqstate:= cqstate or $11000000    // morgue and hiber
          else cqstate:= cqstate or $01000000;    // just hibernation

          if(((xor2 or xor3) and $0f0f) = 0) then begin  // S 8x2 Horizontal border
            if(((n2 or n3) and $0f0f) = 0) then cqstate:= cqstate or $22000000
            else cqstate:= cqstate or $02000000;
          end {if}
          else cqstate:= cqstate and $55ffffff;

          if(((xor1 or xor3) and $00ff) = 0) then begin  // E 2x8 Vertical border
            if(((n1 or n3) and $00ff) = 0) then cqstate:= cqstate or $44000000
            else cqstate:= cqstate or $04000000;

            if((xor1 or xor2 or xor3 or (c.q[0] xor n0)) = 0) then begin  // whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then cqstate:= cqstate or $88000000
              else cqstate:= cqstate or $08000000;
            end {if}
            else cqstate:= cqstate and $77ffffff;
          end {if}
          else cqstate:= cqstate and $33ffffff;
        end {if}
        else cqstate:= cqstate and $00ffffff;

        c.q[0]:=n0; c.q[2]:=n2;
        c.q[1]:=n1; c.q[3]:=n3;
      end {if}
      else begin
        cqstate:= cqstate or $0f000000;

        if((c.q[3] and $000f) = 0) then        // SE 2x2 corner
          cqstate:= cqstate or $11000000;    // morgue and hiber

        if(((c.q[2] or c.q[3]) and $0f0f) = 0) then
          cqstate:= cqstate or $22000000;

        if(((c.q[1] or c.q[3]) and $00ff) = 0) then
          cqstate:= cqstate or $44000000;

        if((c.q[0] or c.q[1] or c.q[2] or c.q[3]) = 0) then
          cqstate:= cqstate or $88000000;
      end; {else}

      if(((c.pstate and $00080004) <> $00080004) or
        ((cSpstate and $02000100) <> $02000100)) then begin // second 8x8 (lower left):
        ix00:= (c.p[4]) and $ffff;
        ix10:= (c.p[5]) and $ffff;
        ix20:= (c.p[6]) and $ffff;
        ix30:= (c.p[7]) and $ffff;

        ix02:= (ix00 and $00ff) or (ix10 and $ff00);
        ix12:= (ix10 and $00ff) or (c.p[12] and $ff00);
        ix22:= (ix20 and $00ff) or (ix30 and $ff00);
        ix32:= (ix30 and $00ff) or (c.p[14] and $ff00);

        ix01:= (ix00 and $0f0f) or (ix20 and $f0f0);
        ix11:= (ix10 and $0f0f) or (ix30 and $f0f0);

        if Assigned(cS) then begin // there's a Southern neighbor (hi y'all!)
          ix21:= (ix20 and $0f0f) or (cS.p[0] and $f0f0);
          ix31:= (ix30 and $0f0f) or (cS.p[1] and $f0f0);
          ix33:= (ix32 and $0f0f) or (ix31 and $00f0) or (cS.p[8] and $f000);
        end {if}
        else begin
          ix21:= (ix20 and $0f0f);
          ix31:= (ix30 and $0f0f);
          ix33:= (ix32 and $0f0f) or (ix31 and $00f0);
        end; {else}

        ix03:= (ix01 and $00ff) or (ix11 and $ff00);
        ix13:= (ix12 and $0f0f) or (ix32 and $f0f0);
        ix23:= (ix21 and $00ff) or (ix31 and $ff00);

        n0:=   (($f000 and Crunch[ix00])
             or ($0f00 and Crunch[ix01])
             or ($00f0 and Crunch[ix02])
             or ($000f and Crunch[ix03]));

        n1:=   (($f000 and Crunch[ix10])
             or ($0f00 and Crunch[ix11])
             or ($00f0 and Crunch[ix12])
             or ($000f and Crunch[ix13]));

        n2:=   (($f000 and Crunch[ix20])
             or ($0f00 and Crunch[ix21])
             or ($00f0 and Crunch[ix22])
             or ($000f and Crunch[ix23]));

        n3:=   (($f000 and Crunch[ix30])
             or ($0f00 and Crunch[ix31])
             or ($00f0 and Crunch[ix32])
             or ($000f and Crunch[ix33]));


        xor1:= c.q[5] xor n1;
        xor2:= c.q[6] xor n2;
        xor3:= c.q[7] xor n3;

        if((xor3 and $000f) = 0) then begin
          if((n3 and $000f) = 0) then        // SE 2x2 corner
            cqstate:= cqstate or $00110000    // morgue and hiber
          else cqstate:= cqstate or $00010000;    // just hibernation

          if(((xor2 or xor3) and $0f0f) = 0) then begin  // S 8x2 Horizontal border
            if(((n2 or n3) and $0f0f) = 0) then cqstate:= cqstate or $00220000
            else cqstate:= cqstate or $00020000;
          end {if}
          else begin
            cqstate:= cqstate and $ff55ffff;
            if Assigned(cS) then rattleCage(cS)
            else begin
              allocateCage(x,yp);
              cS:=c.S;
            end; {else}
          end; {else}

          if(((xor1 or xor3) and $00ff) = 0) then begin  // E 2x8 Vertical border
            if(((n1 or n3) and $00ff) = 0) then cqstate:= cqstate or $00440000
            else cqstate:= cqstate or $00040000;

            if((xor1 or xor2 or xor3 or (c.q[4] xor n0)) = 0) then begin  // whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then cqstate:= cqstate or $00880000
              else cqstate:= cqstate or $00080000;
            end {if}
            else cqstate:= cqstate and $ff77ffff;
          end {if}
          else cqstate:= cqstate and $ff33ffff;
        end {if}
        else begin
          cqstate:= cqstate and $ff00ffff;

          if Assigned(cS) then rattleCage(cS)
          else begin
            allocateCage(x,yp);
            cS:=c.S;
          end; {else}
        end; {else}

        c.q[4]:=n0; c.q[6]:=n2;
        c.q[5]:=n1; c.q[7]:=n3;
      end {if}
      else begin
        cqstate:= cqstate or $000f0000;

        if((c.q[7] and $000f) = 0) then cqstate:= cqstate or $00110000;

        if(((c.q[6] or c.q[7]) and $0f0f) = 0) then cqstate:= cqstate or $00220000;

        if(((c.q[5] or c.q[7]) and $00ff) = 0) then cqstate:= cqstate or $00440000;

        if((c.q[4] or c.q[5] or c.q[6] or c.q[7]) = 0) then
          cqstate:= cqstate or $00880000;
      end; {else}

      if(((c.pstate and $00000802) <> $00000802)
        or((cEpstate and $04010000) <> $04010000)) then begin  // third 8x8 (upper right): */
        ix00:= (c.p[8]) and $ffff;
        ix10:= (c.p[9]) and $ffff;
        ix20:= (c.p[10])and $ffff;
        ix30:= (c.p[11])and $ffff;

        ix02:= (ix00 and $00ff) or (ix10 and $ff00);
        ix22:= (ix20 and $00ff) or (ix30 and $ff00);

        ix01:= (ix00 and $0f0f) or (ix20 and $f0f0);
        ix11:= (ix10 and $0f0f) or (ix30 and $f0f0);
        ix21:= (ix20 and $0f0f) or (c.p[12] and $f0f0);
        ix31:= (ix30 and $0f0f) or (c.p[13] and $f0f0);

        if Assigned(cE) then begin
          ix12:= (ix10 and $00ff) or (cE.p[0] and $ff00);
          ix32:= (ix30 and $00ff) or (cE.p[2] and $ff00);
          ix33:= (ix32 and $0f0f) or (ix31 and $00f0) or (cE.p[4] and $f000);
        end
        else begin
          ix12:= (ix10 and $00ff);
          ix32:= (ix30 and $00ff);
          ix33:= (ix32 and $0f0f) or (ix31 and $00f0);
        end;

        ix03:= (ix01 and $00ff) or (ix11 and $ff00);
        ix13:= (ix12 and $0f0f) or (ix32 and $f0f0);
        ix23:= (ix21 and $00ff) or (ix31 and $ff00);

        n0:=   (($f000 and Crunch[ix00])
             or ($0f00 and Crunch[ix01])
             or ($00f0 and Crunch[ix02])
             or ($000f and Crunch[ix03]));

        n1:=   (($f000 and Crunch[ix10])
             or ($0f00 and Crunch[ix11])
             or ($00f0 and Crunch[ix12])
             or ($000f and Crunch[ix13]));

        n2:=   (($f000 and Crunch[ix20])
             or ($0f00 and Crunch[ix21])
             or ($00f0 and Crunch[ix22])
             or ($000f and Crunch[ix23]));

        n3:=   (($f000 and Crunch[ix30])
             or ($0f00 and Crunch[ix31])
             or ($00f0 and Crunch[ix32])
             or ($000f and Crunch[ix33]));


        xor1:= c.q[9] xor n1;
        xor2:= c.q[10] xor n2;
        xor3:= c.q[11] xor n3;

        if((xor3 and $000f) = 0) then begin
          if((n3 and $000f) = 0) then        // SE 2x2 corner
            cqstate:= cqstate or $00001100    // morgue and hiber
          else cqstate:= cqstate or $00000100;    // just hibernation

          if(((xor2 or xor3) and $0f0f) = 0) then begin  // S 8x2 Horizontal border
            if(((n2 or n3) and $0f0f) = 0) then cqstate:= cqstate or $00002200
            else cqstate:= cqstate or $00000200;
          end
          else cqstate:= cqstate and $ffff55ff;

          if(((xor1 or xor3) and $00ff) = 0) then begin  // E 2x8 Vertical border
            if(((n1 or n3) and $00ff) = 0) then cqstate:= cqstate or $00004400
            else cqstate:= cqstate or $00000400;

            if((xor1 or xor2 or xor3 or (c.q[8] xor n0)) = 0) then begin  // whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then cqstate:= cqstate or $00008800
              else cqstate:= cqstate or $00000800;

            end
            else cqstate:= cqstate and $ffff77ff;
          end
          else begin
            cqstate:= cqstate and $ffff33ff;

            if Assigned(cE) then rattleCage(cE)
            else begin
              allocateCage(xp,y);
              cE:=c.E;
            end;
          end;
        end
        else begin
          cqstate:= cqstate and $ffff00ff;

          if Assigned(cE) then rattleCage(cE)
          else begin
            allocateCage(xp,y);
            cE:=c.E;
          end;
        end;

        c.q[8]:=n0;  c.q[10]:=n2;
        c.q[9]:=n1;  c.q[11]:=n3;
      end
      else begin
        cqstate:= cqstate or $00000f00;

        if((c.q[11] and $000f) = 0) then cqstate:= cqstate or $00001100;

        if(((c.q[10] or c.q[11]) and $0f0f) = 0) then cqstate:= cqstate or $00002200;

        if(((c.q[9] or c.q[11]) and $00ff) = 0) then cqstate:= cqstate or $00004400;

        if((c.q[8] or c.q[9] or c.q[10] or c.q[11]) = 0) then
          cqstate:= cqstate or $00008800;
      end;

      if(((c.pstate  and $00000008) <> $00000008)
       or((cSpstate  and $00000200) <> $00000200)
       or((cEpstate  and $00040000) <> $00040000)
       or((cSEpstate and $01000000) <> $01000000)) then begin  // fourth 8x8 (lower right): */
        ix00:= (c.p[12]) and $ffff;
        ix10:= (c.p[13]) and $ffff;
        ix20:= (c.p[14]) and $ffff;
        ix30:= (c.p[15]) and $ffff;

        ix02:= (ix00 and $00ff) or (ix10 and $ff00);
        ix22:= (ix20 and $00ff) or (ix30 and $ff00);

        ix01:= (ix00 and $0f0f) or (ix20 and $f0f0);
        ix11:= (ix10 and $0f0f) or (ix30 and $f0f0);

        if Assigned(cS) then begin
          ix21:= (ix20 and $0f0f) or (cS.p[8] and $f0f0);
          ix31:= (ix30 and $0f0f) or (cS.p[9] and $f0f0);
        end
        else begin
          ix21:= (ix20 and $0f0f);
          ix31:= (ix30 and $0f0f);
        end;

        if Assigned(c.E) then begin
          ix12:= (ix10 and $00ff) or (cE.p[4] and $ff00);
          ix32:= (ix30 and $00ff) or (cE.p[6] and $ff00);
        end
        else begin
          ix12:= (ix10 and $00ff);
          ix32:= (ix30 and $00ff);
        end;

        ix03:= (ix01 and $00ff) or (ix11 and $ff00);
        ix13:= (ix12 and $0f0f) or (ix32 and $f0f0);
        ix23:= (ix21 and $00ff) or (ix31 and $ff00);

        if Assigned(cSE) then begin
          ix33:= (ix32 and $0f0f) or (ix31 and $00f0) or (cSE.p[0] and $f000);
        end
        else begin
          ix33:= (ix32 and $0f0f) or (ix31 and $00f0);
        end;

        n0:=  (($f000 and Crunch[ix00])
            or ($0f00 and Crunch[ix01])
            or ($00f0 and Crunch[ix02])
            or ($000f and Crunch[ix03]));

        n1:=  (($f000 and Crunch[ix10])
            or ($0f00 and Crunch[ix11])
            or ($00f0 and Crunch[ix12])
            or ($000f and Crunch[ix13]));

        n2:=  (($f000 and Crunch[ix20])
            or ($0f00 and Crunch[ix21])
            or ($00f0 and Crunch[ix22])
            or ($000f and Crunch[ix23]));

        n3:=  (($f000 and Crunch[ix30])
            or ($0f00 and Crunch[ix31])
            or ($00f0 and Crunch[ix32])
            or ($000f and Crunch[ix33]));


        xor1:= c.q[13] xor n1;
        xor2:= c.q[14] xor n2;
        xor3:= c.q[15] xor n3;

        if((xor3 and $000f) = 0) then begin
          if((n3 and $000f) = 0) then         // SE 2x2 corner
            cqstate:= cqstate or $00000011    // morgue and hiber
          else cqstate:= cqstate or $00000001;    // just hibernation

          if(((xor2 or xor3) and $0f0f) = 0) then begin  // S 8x2 Horizontal border
            if(((n2 or n3) and $0f0f) = 0) then cqstate:= cqstate or $00000022
            else cqstate:= cqstate or $00000002;
          end
          else begin
            cqstate:= cqstate and $ffffff55;

            if Assigned(cS) then rattleCage(cS)
            else begin
              allocateCage(x,yp);
              //cS:=c.S;
            end;

          end;

          if(((xor1 or xor3) and $00ff) = 0) then begin  // E 2x8 Vertical border
            if(((n1 or n3) and $00ff) = 0) then cqstate:= cqstate or $00000044
            else cqstate:= cqstate or $00000004;

            if((xor1 or xor2 or xor3 or (c.q[12] xor n0)) = 0) then begin// whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then cqstate:= cqstate or $00000088
              else cqstate:= cqstate or $00000008;
            end
            else cqstate:= cqstate and $ffffff77;
          end
          else begin
            cqstate:= cqstate and $ffffff33;

            if Assigned(cE) then rattleCage(cE)
            else begin
              allocateCage(xp,y);
              //cE:=c.E;
            end;
          end;
        end
        else begin
          cqstate:= cqstate and $ffffff00;

          if Assigned(cS) then rattleCage(cS)
          else begin
            allocateCage(x,yp);
            //cS:=c.S;
          end;

          if Assigned(cE) then rattleCage(cE)
          else begin
            allocateCage(xp,y);
            //cE:=c.E;
          end;

          if Assigned(cSE) then rattleCage(cSE)
          else begin
            allocateCage(xp,yp);
            //cSE:=c.SE;
          end;
        end;

        c.q[12]:=n0; c.q[14]:=n2;
        c.q[13]:=n1; c.q[15]:=n3;
      end
      else begin
        cqstate:= cqstate or $0000000f;

        if((c.q[15] and $000f) = 0) then cqstate:= cqstate or $00000011;

        if(((c.q[14] or c.q[15]) and $0f0f) = 0) then cqstate:= cqstate or $00000022;

        if(((c.q[13] or c.q[15]) and $00ff) = 0) then cqstate:= cqstate or $00000044;

        if((c.q[12] or c.q[13] or c.q[14] or c.q[15]) =0) then
          cqstate:= cqstate or $00000088;
      end;
    end;
    if(not(BackCorrect)) then cqstate:=0;
    c.qstate:= cqstate;
    c:= cnext;
  end; {while}
  BackCorrect:=true;
end;

procedure TUniverse.generate_q;
var
  c, cnext: TLifeCel;
  cN, cNW, cW: TLifeCel;
  cpstate: integer;
  cNqstate, cNWqstate, cWqstate: integer;
  x, y, xm, ym: integer;
  xor0, xor1, xor2: integer;
  ix00, ix02, ix10, ix12,   // 4x4 neighborhoods
  ix01, ix03, ix11, ix13,   // extracted from a
  ix20, ix22, ix30, ix32,   // full 8x8 block,
  ix21, ix23, ix31, ix33: integer;   // for table lookups.
  n0, n1, n2, n3: integer;         // table lookup results
begin
  {$ifdef LifeDebug}
  DebugMemo.Lines.Add('*** Q -> P generate_q');
  {$endif}

  // For each cage: */
  c:= living.next;
  while (c <> living) do begin
    cnext:= c.next;
    cN:=c.N;
    cW:=c.W;
    cNW:=c.NW;
    cpstate:= c.pstate;

    if Assigned(cN) then cNqstate:=cN.qstate
    else cNqstate:= -1;

    if Assigned(cW) then cWqstate:=cW.qstate
    else cWqstate:= -1;

    if Assigned(cNW) then cNWqstate:=cNW.qstate
    else cNWqstate:= -1;

    if(((c.qstate and $08080808)=$08080808) and ((cNqstate and $00020002)=$00020002)
      and((cWqstate and $00000404)=$00000404) and ((cNWqstate and $00000001)=$00000001))
    then begin
      if(((c.qstate and $80808080)=$80808080) and ((cNqstate and $00200020)=$00200020)
        and((cWqstate and $00004040)=$00004040) and ((cNWqstate and $00000010)=$00000010))
      then begin
        c.flags:= c.flags or $3000;
        if((c.flags and $f800)=$f000) then killCage(c);
        cpstate := $ffffffff;
      end {if}
      else begin
        c.flags:= c.flags or $1000;
        if((c.flags and $5800)=$5000) then tranquilizeCage(c);
        cpstate:= cpstate or $0f0f0f0f;
      end; {else}
      c.flags:= c.flags and $f7ff;  // Reset the Rattling bit
    end {if}
    else begin
      c.flags:= c.flags and $07ff;
      x:= c.coor.x * 16;  y:= c.coor.y * 16;
      xm:= (x-16);  //pin
      ym:= (y-16);  //pin

      if(((c.qstate and $08000000) <> $08000000)
       or((cNqstate and $00020000) <> $00020000)
       or((cWqstate and $00000400) <> $00000400)
       or((cNWqstate and $00000001) <> $00000001))  // first 8x8 not hibernating/morgue */
      then begin
        ix00:= (c.q[0]) and $ffff;
        ix10:= (c.q[1]) and $ffff;
        ix20:= (c.q[2]) and $ffff;
        ix30:= (c.q[3]) and $ffff;

        ix12:= (ix00 and $00ff) or (ix10 and $ff00);
        ix32:= (ix20 and $00ff) or (ix30 and $ff00);

        ix21:= (ix00 and $0f0f) or (ix20 and $f0f0);
        ix31:= (ix10 and $0f0f) or (ix30 and $f0f0);

        if Assigned(cN) then begin
          ix01:= (ix00 and $f0f0) or (cN.q[6] and $0f0f);
          ix11:= (ix10 and $f0f0) or (cN.q[7] and $0f0f);
        end {if}
        else begin
          ix01:= (ix00 and $f0f0);
          ix11:= (ix10 and $f0f0);
        end; {else}

        if Assigned(cW) then begin
          ix02:= (ix00 and $ff00) or (cW.q[9] and $00ff);
          ix22:= (ix20 and $ff00) or (cW.q[11] and $00ff);
        end {if}
        else begin
          ix02:= (ix00 and $ff00);
          ix22:= (ix20 and $ff00);
        end; {else}

        ix13:= (ix01 and $00ff) or (ix11 and $ff00);
        ix23:= (ix02 and $0f0f) or (ix22 and $f0f0);
        ix33:= (ix21 and $00ff) or (ix31 and $ff00);

        if Assigned(cNW) then begin
          ix03:= (ix02 and $f0f0) or (ix01 and $0f00) or (cNW.q[15] and $000f);
        end {if}
        else begin
          ix03:= (ix02 and $f0f0) or (ix01 and $0f00);
        end;

        n0:= (($000f and Munch[ix00])
           or ($00f0 and Munch[ix01])
           or ($0f00 and Munch[ix02])
           or ($f000 and Munch[ix03]));

        n1:= (($000f and Munch[ix10])
           or ($00f0 and Munch[ix11])
           or ($0f00 and Munch[ix12])
           or ($f000 and Munch[ix13]));

        n2:= (($000f and Munch[ix20])
           or ($00f0 and Munch[ix21])
           or ($0f00 and Munch[ix22])
           or ($f000 and Munch[ix23]));

        n3:= (($000f and Munch[ix30])
           or ($00f0 and Munch[ix31])
           or ($0f00 and Munch[ix32])
           or ($f000 and Munch[ix33]));


        xor0:= c.p[0] xor n0;
        xor1:= c.p[1] xor n1;
        xor2:= c.p[2] xor n2;

        if((xor0 and $f000) = 0) then begin
          if((n0 and $f000) = 0) then     // NW 2x2 corner
            cpstate:= cpstate or $11000000    // morgue and hiber
          else cpstate:= cpstate or $01000000;    // just hibernation

          if(((xor1 or xor0) and $f0f0) = 0) then begin  // N 8x2 Horizontal border
            if(((n1 or n0) and $f0f0) = 0) then
              cpstate:= cpstate or $22000000
            else cpstate:= cpstate or $02000000;
          end {if}
          else begin
            cpstate:= cpstate and $55ffffff;

            if Assigned(cN) then rattleCage(cN)
            else begin
              allocateCage(x,ym);
              cN:=c.N;
            end; {else}
          end; {else}

          if(((xor2 or xor0) and $ff00) = 0) then begin// W 2x8 Vertical border
            if(((n2 or n0) and $ff00) = 0) then
              cpstate:= cpstate or $44000000
            else cpstate:= cpstate or $04000000;

            if((xor2 or xor1 or xor0 or (c.p[3] xor n3)) = 0) then begin  // whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then
                cpstate:= cpstate or $88000000
              else cpstate:= cpstate or $08000000;

            end {if}
            else cpstate:= cpstate and $77ffffff;
          end {if}
          else begin
            cpstate:= cpstate and $33ffffff;

            if Assigned(cW) then rattleCage(cW)
            else begin
              allocateCage(xm,y);
              cW:=c.W;
            end; {else}
          end; {else}
        end {if}
        else begin
          cpstate:= cpstate and $00ffffff;

          if Assigned(cN) then rattleCage(cN)
          else begin
            allocateCage(x,ym);
            cN:=c.N;
          end; {else}

          if Assigned(cW) then rattleCage(cW)
          else begin
            allocateCage(xm,y);
            cW:=c.W;
          end; {else}

          if Assigned(cNW) then rattleCage(cNW)
          else begin
            allocateCage(xm,ym);
            //cNW:=c.NW;
          end; {else}
        end; {else}

        c.p[0]:=n0; c.p[2]:=n2;
        c.p[1]:=n1; c.p[3]:=n3;
      end {if}
      else begin
        cpstate:= cpstate or $0f000000;
        if((c.p[0] and $f000) = 0) then
          cpstate:= cpstate or $11000000;

        if(((c.p[1] or c.p[0]) and $f0f0) = 0) then
          cpstate:= cpstate or $22000000;

        if(((c.p[2] or c.p[0]) and $ff00) = 0) then
          cpstate:= cpstate or $44000000;

        if((c.p[3] or c.p[2] or c.p[1] or c.p[0]) = 0) then
          cpstate:= cpstate or $88000000;
      end; {else}

      if(((c.qstate and $02080000) <> $02080000)
       or((cWqstate and $00000104) <> $00000104)) then begin // second 8x8 (lower left)
        ix00:= (c.q[4]) and $ffff;
        ix10:= (c.q[5]) and $ffff;
        ix20:= (c.q[6]) and $ffff;
        ix30:= (c.q[7]) and $ffff;

        ix12:= (ix00 and $00ff) or (ix10 and $ff00);
        ix32:= (ix20 and $00ff) or (ix30 and $ff00);

        ix01:= (ix00 and $f0f0) or (c.q[2] and $0f0f);
        ix11:= (ix10 and $f0f0) or (c.q[3] and $0f0f);
        ix21:= (ix20 and $f0f0) or (ix00 and $0f0f);
        ix31:= (ix30 and $f0f0) or (ix10 and $0f0f);

        if Assigned(cW) then begin
          ix02:= (ix00 and $ff00) or (cW.q[13] and $00ff);
          ix22:= (ix20 and $ff00) or (cW.q[15] and $00ff);
          ix03:= (ix02 and $f0f0) or (ix01 and $0f00) or (cW.q[11] and $000f);
        end {if}
        else begin
          ix02:= (ix00 and $ff00);
          ix22:= (ix20 and $ff00);
          ix03:= (ix02 and $f0f0) or (ix01 and $0f00);
        end; {else}

        ix13:= (ix01 and $00ff) or (ix11 and $ff00);
        ix23:= (ix02 and $0f0f) or (ix22 and $f0f0);
        ix33:= (ix21 and $00ff) or (ix31 and $ff00);

        n0:= (($000f and Munch[ix00])
           or ($00f0 and Munch[ix01])
           or ($0f00 and Munch[ix02])
           or ($f000 and Munch[ix03]));

        n1:= (($000f and Munch[ix10])
           or ($00f0 and Munch[ix11])
           or ($0f00 and Munch[ix12])
           or ($f000 and Munch[ix13]));

        n2:= (($000f and Munch[ix20])
           or ($00f0 and Munch[ix21])
           or ($0f00 and Munch[ix22])
           or ($f000 and Munch[ix23]));

        n3:= (($000f and Munch[ix30])
           or ($00f0 and Munch[ix31])
           or ($0f00 and Munch[ix32])
           or ($f000 and Munch[ix33]));

        xor0:= c.p[4] xor n0;
        xor1:= c.p[5] xor n1;
        xor2:= c.p[6] xor n2;

        if((xor0 and $f000) = 0) then begin
          if((n0 and $f000) = 0) then     // NW 2x2 corner
            cpstate:= cpstate or $00110000    // morgue and hiber
          else cpstate:= cpstate or $00010000;    // just hibernation

          if(((xor1 or xor0) and $f0f0) = 0) then begin  // N 8x2 Horizontal border
            if(((n1 or n0) and $f0f0) = 0) then
              cpstate:= cpstate or $00220000
            else cpstate:= cpstate or $00020000;
          end {if}
          else cpstate:= cpstate and $ff55ffff;

          if(((xor2 or xor0) and $ff00) = 0) then begin   // W 2x8 Vertical border
            if(((n2 or n0) and $ff00) = 0) then
              cpstate:= cpstate or $00440000
            else cpstate:= cpstate or $00040000;

            if((xor2 or xor1 or xor0 or (c.p[7] xor n3)) = 0) then begin  // whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then
                cpstate:= cpstate or $00880000
              else cpstate:= cpstate or $00080000;
            end {if}
            else cpstate:= cpstate and $ff77ffff;
          end {if}
          else begin
            cpstate:= cpstate and $ff33ffff;

            if Assigned(cW) then rattleCage(cW)
            else begin
              allocateCage(xm,y);
              //cW:=c.W;
            end; {else}
          end; {else}
        end {if}
        else begin
          cpstate:= cpstate and $ff00ffff;

          if Assigned(cW) then rattleCage(cW)
          else begin
            allocateCage(xm,y);
            //cW:=c.W;
          end; {else}
        end; {else}

        c.p[4]:=n0; c.p[6]:=n2;
        c.p[5]:=n1; c.p[7]:=n3;
      end {if}
      else begin
        cpstate:= cpstate or $000f0000;

        if((c.p[4] and $f000) = 0) then
          cpstate:= cpstate or $00110000;

        if(((c.p[5] or c.p[4]) and $f0f0) = 0) then
          cpstate:= cpstate or $00220000;

        if(((c.p[6] or c.p[4]) and $ff00) = 0) then
          cpstate:= cpstate or $00440000;

        if((c.p[7] or c.p[6] or c.p[5] or c.p[4]) = 0) then
          cpstate:= cpstate or $00880000;
      end; {else}

      if(((c.qstate and $04000800) <> $04000800)
       or((cNqstate and $00010002) <> $00010002)) then begin//third 8x8 (upper right)

        ix00:= (c.q[8]) and $ffff;
        ix10:= (c.q[9]) and $ffff;
        ix20:= (c.q[10])and $ffff;
        ix30:= (c.q[11])and $ffff;

        ix02:= (ix00 and $ff00) or (c.q[1] and $00ff);
        ix12:= (ix00 and $00ff) or (ix10 and $ff00);
        ix22:= (ix20 and $ff00) or (c.q[3] and $00ff);
        ix32:= (ix20 and $00ff) or (ix30 and $ff00);

        ix21:= (ix20 and $f0f0) or (ix00 and $0f0f);
        ix31:= (ix30 and $f0f0) or (ix10 and $0f0f);

        if Assigned(cN) then begin
          ix01:= (ix00 and $f0f0) or (cN.q[14] and $0f0f);
          ix11:= (ix10 and $f0f0) or (cN.q[15] and $0f0f);
          ix03:= (ix02 and $f0f0) or (ix01 and $0f00) or (cN.q[7] and $000f);
        end {if}
        else begin
          ix01:= (ix00 and $f0f0);
          ix11:= (ix10 and $f0f0);
          ix03:= (ix02 and $f0f0) or (ix01 and $0f00);
        end; {else}

        ix13:= (ix01 and $00ff) or (ix11 and $ff00);
        ix23:= (ix02 and $0f0f) or (ix22 and $f0f0);
        ix33:= (ix21 and $00ff) or (ix31 and $ff00);

        n0:= (($000f and Munch[ix00])
           or ($00f0 and Munch[ix01])
           or ($0f00 and Munch[ix02])
           or ($f000 and Munch[ix03]));

        n1:= (($000f and Munch[ix10])
           or ($00f0 and Munch[ix11])
           or ($0f00 and Munch[ix12])
           or ($f000 and Munch[ix13]));

        n2:= (($000f and Munch[ix20])
           or ($00f0 and Munch[ix21])
           or ($0f00 and Munch[ix22])
           or ($f000 and Munch[ix23]));

        n3:= (($000f and Munch[ix30])
           or ($00f0 and Munch[ix31])
           or ($0f00 and Munch[ix32])
           or ($f000 and Munch[ix33]));

        xor0:= c.p[8] xor n0;
        xor1:= c.p[9] xor n1;
        xor2:= c.p[10] xor n2;

        if((xor0 and $f000) = 0) then begin
          if((n0 and $f000) = 0) then         // NW 2x2 corner
            cpstate:= cpstate or $00001100    // morgue and hiber
          else cpstate:= cpstate or $00000100;    // just hibernation

          if(((xor1 or xor0) and $f0f0) = 0) then begin // N 8x2 Horizontal border
            if(((n1 or n0) and $f0f0) = 0) then
              cpstate:= cpstate or $00002200
            else cpstate:= cpstate or $00000200;
          end {if}
          else begin
            cpstate:= cpstate and $ffff55ff;

            if Assigned(cN) then rattleCage(cN)
            else begin
              allocateCage(x,ym);
              //cN:=c.N;
            end; {else}
          end; {else}

          if(((xor2 or xor0) and $ff00) = 0) then begin// W 2x8 Vertical border
            if(((n2 or n0) and $ff00) = 0) then
              cpstate:= cpstate or $00004400
            else cpstate:= cpstate or $00000400;

            if((xor2 or xor1 or xor0 or (c.p[11] xor n3)) = 0) then begin// whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then
                cpstate:= cpstate or $00008800
              else cpstate:= cpstate or $00000800;
            end {if}
            else cpstate:= cpstate and $ffff77ff;
          end {if}
          else cpstate:= cpstate and $ffff33ff;
        end {if}
        else begin
          cpstate:= cpstate and $ffff00ff;

          if Assigned(cN) then rattleCage(cN)
          else begin
            allocateCage(x,ym);
            //cN:=c.N;
          end; {else}
        end; {else}

        c.p[8]:=n0;  c.p[10]:=n2;
        c.p[9]:=n1;  c.p[11]:=n3;
      end {if}
      else begin
        cpstate:= cpstate or $00000f00;

        if((c.p[8] and $f000) = 0) then
          cpstate:= cpstate or $00001100;

        if(((c.p[9] or c.p[8]) and $f0f0) = 0) then
          cpstate:= cpstate or $00002200;

        if(((c.p[10] or c.p[8]) and $ff00) = 0) then
          cpstate:= cpstate or $00004400;

        if((c.p[11] or c.p[10] or c.p[9] or c.p[8]) = 0) then
          cpstate:= cpstate or $00008800;
      end; {else}

      if((c.qstate and $01040208) <> $01040208) then begin// fourth 8x8 (lower right)                begin
        ix00:= (c.q[12]) and $ffff;
        ix10:= (c.q[13]) and $ffff;
        ix20:= (c.q[14]) and $ffff;
        ix30:= (c.q[15]) and $ffff;

        ix02:= (ix00 and $ff00) or (c.q[5] and $00ff);
        ix12:= (ix10 and $ff00) or (ix00 and $00ff);
        ix22:= (ix20 and $ff00) or (c.q[7] and $00ff);
        ix32:= (ix30 and $ff00) or (ix20 and $00ff);

        ix01:= (ix00 and $f0f0) or (c.q[10] and $0f0f);
        ix11:= (ix10 and $f0f0) or (c.q[11] and $0f0f);
        ix21:= (ix20 and $f0f0) or (ix00 and $0f0f);
        ix31:= (ix30 and $f0f0) or (ix10 and $0f0f);

        ix03:= (ix01 and $0f00) or (ix02 and $f0f0) or (c.q[3] and $000f);
        ix13:= (ix01 and $00ff) or (ix11 and $ff00);
        ix23:= (ix02 and $0f0f) or (ix22 and $f0f0);
        ix33:= (ix12 and $0f0f) or (ix32 and $f0f0);

        n0:= (($000f and Munch[ix00])
           or ($00f0 and Munch[ix01])
           or ($0f00 and Munch[ix02])
           or ($f000 and Munch[ix03]));

        n1:= (($000f and Munch[ix10])
           or ($00f0 and Munch[ix11])
           or ($0f00 and Munch[ix12])
           or ($f000 and Munch[ix13]));

        n2:= (($000f and Munch[ix20])
           or ($00f0 and Munch[ix21])
           or ($0f00 and Munch[ix22])
           or ($f000 and Munch[ix23]));

        n3:= (($000f and Munch[ix30])
           or ($00f0 and Munch[ix31])
           or ($0f00 and Munch[ix32])
           or ($f000 and Munch[ix33]));


        xor0:= c.p[12] xor n0;
        xor1:= c.p[13] xor n1;
        xor2:= c.p[14] xor n2;

        if((xor0 and $f000) = 0) then begin
          if((n0 and $f000) = 0) then        // NW 2x2 corner
            cpstate:= cpstate or $00000011 // morgue and hiber
          else cpstate:= cpstate or $00000001;    // just hibernation

          if(((xor1 or xor0) and $f0f0) = 0) then begin  // N 8x2 Horizontal border
            if(((n1 or n0) and $f0f0) = 0) then
              cpstate:= cpstate or $00000022
            else cpstate:= cpstate or $00000002;
          end {if}
          else cpstate:= cpstate and $ffffff55;

          if(((xor2 or xor0) and $ff00) = 0) then begin  // W 2x8 Vertical border
            if(((n2 or n0) and $ff00) = 0) then
              cpstate:= cpstate or $00000044
            else cpstate:= cpstate or $00000004;

            if((xor2 or xor1 or xor0 or (c.p[15] xor n3)) = 0) then begin  // whole 8x8 block
              if((n0 or n1 or n2 or n3) = 0) then
                cpstate:= cpstate or $00000088
              else cpstate:= cpstate or $00000008;
            end {if}
            else cpstate:= cpstate and $ffffff77;
          end {if}
          else cpstate:= cpstate and $ffffff33;
        end {if}
        else cpstate:= cpstate and $ffffff00;

        c.p[12]:=n0; c.p[14]:=n2;
        c.p[13]:=n1; c.p[15]:=n3;
      end {if}
      else begin
        cpstate:= cpstate or $0000000f;

        if((c.p[12] and $f000) = 0) then
          cpstate:= cpstate or $00000011;

        if(((c.p[13] or c.p[12]) and $f0f0) = 0) then
          cpstate:= cpstate or $00000022;

        if(((c.p[14] or c.p[12]) and $ff00) = 0) then
          cpstate:= cpstate or $00000044;

        if((c.p[15] or c.p[14] or c.p[13] or c.p[12]) = 0) then
          cpstate:= cpstate or $00000088;
      end; {else}
    end; {else}
    if(not BackCorrect) then cpstate:=0;
    c.pstate:= cpstate;
    c:= cnext;
  end; {while}
  BackCorrect:=true;
end;

procedure TUniverse.IncinerateCages(beNice: boolean);
var
  victim: TLifeCel;
  KillMe: TLifeCel;
begin
  if benice then victim:= caretaker.next
  else victim:= morgue.next;            // whole morgue

  while(victim <> morgue) do begin
    // not being displayed.
    if((victim.flags and 1) = 0) then FHashTable.delete(victim);
    Killme:= Victim;
    victim:= Victim.Next;

    // no more references to the victim: now we can kill it (bang!)
    //Pascal has no garbage-collection (Thank you Lord !).
    Killme.Free;   //Free calls Destroy, Destroy updates neighbor-references.
  end; {while}
  caretaker:= morgue.next;              // start at top again
end;

procedure TUniverse.InsertCage(c: TLifeCel);
begin
  FHashTable.Store(c);

  c.prev:=living;
  c.next:= living.next;
  living.next:= c;
  c.next.prev:= c;

  AddToDisplay(c);
end;

function TUniverse.AllocateCage(x,y: Integer):TLifeCel;
var
  c: TLifeCel;
begin
  c:= TLifeCel.Create;
  c.coor.x:= x shr 4; c.coor.y:= y shr 4;

  //Store stuff in the hashlist, this also updates neighbors.
  FHashTable.Store(c);

  c.prev:=living;
  c.next:= living.next;
  living.next:= c;
  c.next.prev:= c;

  AddToDisplay(c);

  Result:= c;
end;

// Put Cage in the morgue state.  There are only 2 possible fates from here:
// rattleCage() and incinerateCages(): return to the living list, or be deallocated.
procedure TUniverse.killCage(c: TLifeCel);
begin
  c.flags:= c.flags or $02;

  // Remove from living list
  c.next.prev:= c.prev;
  c.prev.next:= c.next;

  // Put in morgue
  c.Prev:= morgue;
  c.next:= morgue.next;
  c.next.prev:= c;
  morgue.next:= c;
end;

// Put Cage in the hibernation state.  There is only 1 possible fate from here:
// rattleCage() to return to the living list.  But you can hibernate forever.
procedure TUniverse.tranquilizeCage(c: TLifeCel);
begin
  c.flags:= c.flags or $04;

  // Remove from living list
  c.next.prev:= c.prev;
  c.prev.next:= c.next;

  // Put in hibernation
  c.prev:= hibernating;
  c.next:= hibernating.next;
  hibernating.next:= c;
  c.next.prev:= c;
end;

// This cage will have to be calculated next generation.  Take out of either
// morgue or hibernation state, whichever the case may be:
function TUniverse.RattleCage(c: TLifeCel): Boolean;
begin
  c.flags:= c.flags or $0800;  // set the Rattling bit
  if((c.flags and $06) <> 0) then begin
    if((c.flags and $02) <> 0) then begin // resurrect from morgue
      if((c.flags and 1) = 0) then addToDisplay(c);

      // Remove from morgue
      if (caretaker=c) then caretaker:= c.Next;
    end; {if}
    // wake up from hibernation c.q. morgue
    c.flags:= c.flags and $0ff1;
    c.pstate:=0;
    c.qstate:=0;

    // Remove from hibernation c.q. morgue
    c.prev.next:= c.next;
    c.next.prev:= c.prev;

    // Put in living list
    c.next:= living.next;
    c.next.prev:= c;
    c.prev:= living;
    living.next:= c;

    Result:= true;
  end {if}
  else Result:= false; (**)
end;


function TUniverse.GetLifeCelSlowly(x,y: integer; force:boolean): TLifeCel;
var
  x2,y2: integer;
  c: TLifeCel;
begin
  Dec(x,integer(QCycle));             //this code assumes 1=true, 0=false.
  Dec(y,integer(QCycle)); //pin
  c:= FHashTable.RetrieveCached(x,y);

  if (force) then begin
    if not Assigned(c) then c:= AllocateCage(x,y)
    else RattleCage(c);

    if(Qcycle) then begin
      x2:= (x+16);  //pin
      y2:= (y+16);  //pin

      if not Assigned(c.E) then allocateCage(x2,y)
      else rattleCage(c.E);

      if not Assigned(c.SE) then allocateCage(x2,y2)
      else rattleCage(c.SE);

      if not Assigned(c.S) then allocateCage(x,y2)
      else rattleCage(c.S);
    end {if}
    else begin  //pcycle
      x2:=(x-16);  //pin
      y2:=(y-16);  //pin

      if not Assigned(c.W) then allocateCage(x2,y)
      else rattleCage(c.W);

      if not Assigned(c.NW) then allocateCage(x2,y2)
      else rattleCage(c.NW);

      if not Assigned(c.N) then allocateCage(x,y2)
      else rattleCage(c.N);
    end; {else}
  end; {if}
  Result:= c;
end;


function TUniverse.GetLifeCel(x,y: integer; force:boolean): TLifeCel;
var
  x2,y2: integer;
  c: TLifeCel;
  hEdge: boolean;
  vEdge: boolean;
begin
  Dec(x,integer(QCycle));             //this code assumes 1=true, 0=false.
  Dec(y,integer(QCycle)); //pin
  c:= FHashTable.RetrieveCached(x,y);

  if (force) then begin
    if not Assigned(c) then c:= AllocateCage(x,y)
    else RattleCage(c);

    if(Qcycle) then begin
      vEdge:= ((x and $0e) = $0e);
      hEdge:= ((y and $0e) = $0e);

      x2:= (x+16);  //pin
      y2:= (y+16);  //pin

      if(vEdge) then begin
        if not Assigned(c.E) then allocateCage(x2,y)
        else rattleCage(c.E);

        if(hEdge) then begin
          if not Assigned(c.SE) then allocateCage(x2,y2)
          else rattleCage(c.SE);
        end; {if}
      end; {if}
      if(hEdge) then begin
        if not Assigned(c.S) then allocateCage(x,y2)
        else rattleCage(c.S);
      end; {if}
    end {if}
    else begin  //pcycle
      vEdge:= ((x and $0e) = 0);
      hEdge:= ((y and $0e) = 0);

      x2:=(x-16);  //pin
      y2:=(y-16);  //pin

      if(vEdge) then begin
        if not Assigned(c.W) then allocateCage(x2,y)
        else rattleCage(c.W);

        if(hEdge) then begin
          if not Assigned(c.NW) then allocateCage(x2,y2)
          else rattleCage(c.NW);
        end; {if}
      end; {if}
      if(hEdge) then begin
        if not Assigned(c.N) then allocateCage(x,y2)
        else rattleCage(c.N);
      end; {if}
    end; {else}
  end; {if}
  Result:= c;
end;

function TUniverse.GetBlockIndex(x,y: integer): integer; assembler;
asm
  //  Dec(x,integer(QCycle));
  //  Dec(y,integer(QCycle));
  //  Result:= (x and $8) or ((y and $c) shr 1) or ((x and $4) shr 2);

  //@@@@ only works when data is aligned !!!!!!!
  mov  eax,dword ptr [eax+TUniverse.QCycle] //u
  sub  edx,eax  //u
  sub  ecx,eax  //v
  mov  eax,edx  //u
  and  ecx,$0c  //v
  shr  ecx,1    //u
  and  eax,$08  //v
  or   eax,ecx  //u
  and  edx,$04  //v
  shr  edx,2    //u
  or   eax,edx  //u
end; {asm}


function TUniverse.getBlockBitmask(x,y: integer): integer; assembler
asm
  //Dec(x,integer(QCycle));
  //Dec(y,integer(QCycle));
  //a:= (4*(x and 2));         //if Bool(x and 2) then Inc(a,8);
  (*Inc(a,(2*(y and 2)));   //if Bool(y and 2) then Inc(a,4);
  (*Inc(a,(2*(y and 1)));   //if Bool(y and 1) then Inc(a,2); (**)
  //Inc(a,(2*(y and 3)));
  //Inc(a,x and 1);           //if Bool(x and 1) then Inc(a,1);
  //Result:= $8000 shr a;

  //xor  ebx,ebx  //-
  //@@@only works when data is aligned !!!
  mov eax,dword ptr [eax+TUniverse.QCycle] //* //u
  sub edx,eax          //u
  sub ecx,eax          //v
  mov eax,edx          //u
  and ecx,3            //v
  and eax,2            //u
  lea eax,[eax*4]      //v
  and edx,1            //u
  lea eax,[ecx*2+eax]  //v
  lea ecx,[eax+edx]    //u
  mov eax,$8000        //v
  shr eax,cl           //u
end;

    // implementation of the public interface: */

procedure TUniverse.ChangeCel(x,y: integer; state: boolean);
var
  ix, bitset: integer;
  c: TLifeCel;
begin
  BackCorrect:=false;

  c:=GetLifeCel(x,y,true);
  ix:=getBlockIndex(x,y);
  bitset:=getBlockBitmask(x,y);

  if(Qcycle) then begin
    if(((c.q[ix] and bitset) <> 0) xor state) then begin
      c.q[ix]:= c.q[ix] xor bitset;

      c.qstate:= 0;                       //alive = true
      if Assigned(c.N) then c.N.qstate:= c.N.qstate and $ff00ff00;
      if Assigned(c.W) then c.W.qstate:= c.W.qstate and $ffff0000;
      if Assigned(c.NW) then c.NW.qstate:= c.NW.qstate and $ffffff00;
    end; {if}
  end  {if}
  else begin
    if(((c.p[ix] and bitset) <> 0) xor state) then begin
      c.p[ix]:= c.p[ix] xor bitset;

      c.pstate:= 0;
      if Assigned(c.S) then c.S.pstate:= c.S.pstate and $00ff00ff;
      if Assigned(c.E) then c.E.pstate:= c.E.pstate and $0000ffff;
      if Assigned(c.SE) then c.SE.pstate:= c.SE.pstate and $00ffffff;
    end; {if}
  end; {else}
end;

procedure TUniverse.ChangeCelFast(x,y: integer; state: boolean);
var
  ix, bitset: integer;
  c: TLifeCel;
begin
  c:=GetLifeCel(x,y,state); //only force creation if set to 'on'.
  if Assigned(c) then begin
    if (c.flags and 6) <> 0 then RattleCage(c);
    BackCorrect:=false;
    ix:=getBlockIndex(x,y);
    bitset:=getBlockBitmask(x,y);

    if(Qcycle) then begin
      if(((c.q[ix] and bitset) <> 0) xor state) then begin
        c.q[ix]:= c.q[ix] xor bitset;

        c.qstate:= 0;                       //alive = waar
        if Assigned(c.N) then c.N.qstate:= c.N.qstate and $ff00ff00;
        if Assigned(c.W) then c.W.qstate:= c.W.qstate and $ffff0000;
        if Assigned(c.NW) then c.NW.qstate:= c.NW.qstate and $ffffff00;
      end; {if}
    end  {if}
    else begin
      if(((c.p[ix] and bitset) <> 0) xor state) then begin
        c.p[ix]:= c.p[ix] xor bitset;

        c.pstate:= 0;
        if Assigned(c.S) then c.S.pstate:= c.S.pstate and $00ff00ff;
        if Assigned(c.E) then c.E.pstate:= c.E.pstate and $0000ffff;
        if Assigned(c.SE) then c.SE.pstate:= c.SE.pstate and $00ffffff;
      end; {if}
    end; {else}
  end; {if}
end;


function TUniverse.CelState(x,y: integer):boolean;
var
  ix, bitset: integer;
  c: TLifeCel;
begin
  c:= GetLifeCel(x,y,false);
  if not Assigned(c) then Result:= false  // no block, no Cel
  else begin
    ix:=getBlockIndex(x,y);
    bitset:=getBlockBitmask(x,y);

    if(Qcycle) then Result:= ((c.q[ix] and bitset) <> 0)
    else Result:= ((c.p[ix] and bitset) <> 0)
  end; {else}
end;


function TUniverse.GetBoundingBoxFast: TRect;
const
  Kill_mAll = false;
var
  c: TLifeCel;
begin
  with Result do begin
    RattleAllCages; //get everyone else in the living list.
    IncinerateCages(Kill_mAll); //Remove the dead wood.
    c:= living.next;
    Left:= c.coor.x; Top:= c.coor.y; Right:= c.coor.x; Bottom:= c.coor.y;
    while c <> living do begin
      if (c.coor.x < Left) and not(c.IsEmpty(QCycle)) then Left:= c.coor.x
      else if (c.coor.x > Right) and not(c.IsEmpty(QCycle)) then Right:= c.coor.x;
      if (c.coor.y < Top) and not(c.IsEmpty(QCycle)) then Top:= c.coor.y
      else if (c.coor.y > Bottom) and not(c.IsEmpty(QCycle)) then Bottom:= c.coor.y;
      c:= c.next;
    end; {while}
  end; {with}
  if (living.next <> living) then with Result do begin //not empty
    Left:= (Left * 16) + integer(QCycle);
    Top:= (Top * 16) + integer(QCycle);
    Right:= ((Right + 1)*16) + integer(QCycle);  //was ...-1
    Bottom:= ((Bottom + 1)*16) + integer(QCycle);  //was ...-1
  end; {if}
end;

function TUniverse.GetBoundingBox: TRect;
begin
  Result:= GetBoundingBoxFast;
  if not IsMyRectEmpty(Result) then ShrinkSelRect(Result);
end;

function TUniverse.IsSelEmpty(ARect: TRect): boolean;
begin
  ShrinkSelRect(ARect);
  Result:= IsMyRectEmpty(ARect);
end;

procedure TUniverse.ShrinkSelRect(var ARect: TRect);
var
  x,y: integer;
  //c: TLifeCel;
  Ready: boolean;
begin
  //Convert selection rect to the minimumRect.
  with ARect do begin
    Dec(Bottom);
    Dec(Right);
  end; {with}
  ShrinkSelRectFast(ARect); //do the grunt work fast.
  //the fast one fails to shrink if there is stuff outside of the original
  //selection rect, then a pixel or two white inside and then black.
  //All in one TLifeCel object. So we do a slow routine to finish up after
  //the fast one.
  //With a large selection, this can make the difference between waiting 2
  //secs and instant responce.
  with ARect do begin
    if Left > Right then Right:= Left;
    if Top > Bottom then Bottom:= Top;
    Ready:= false;
    while (not Ready) and (not Terminated) and (Bottom > Top) do begin
      for x:= Left to Right do begin
        Ready:= Ready or CelState(x,Bottom);
      end; {for}
      if not Ready then Dec(Bottom);
    end; {while}

    Ready:= false;
    while (not Ready) and (not Terminated) and (Right > Left) do begin
      for y:= Top to Bottom do begin
        Ready:= Ready or CelState(Right,y);
      end; {for}
      if not Ready then Dec(Right);
    end; {while}


    Ready:= false;
    while (not Ready) and (not Terminated) and (Bottom > Top) do begin
      for x:= Left to Right do begin
        Ready:= Ready or CelState(x,Top);
      end; {for}
      if not Ready then Inc(Top);
    end; {while}

    Ready:= false;
    while (not Ready) and (not Terminated) and (Right > Left) do begin
      for y:= Top to Bottom do begin
        Ready:= Ready or CelState(Left,y);
      end; {for}
      if not Ready then Inc(Left);
    end; {while}
    if CelState(Left,Top) or not(IsMyRectEmpty(ARect)) then begin
      Inc(Right);
      Inc(Bottom);
    end;
  end; {with}
end;(**)

procedure TUniverse.ShrinkSelRectFast(var ARect: TRect);
var
  MinLeft, MinTop, MaxRight, MaxBottom: integer;
  i: integer;
  a: integer;
  c: TLifeCel;
  Ready: boolean;
  SaveRect: TRect;
begin
  a:= Integer(QCycle);

  SaveRect:= ARect;
  //translate rect to universe coordinates if need be.
  if QCycle then OffsetRect(ARect,-1,-1);

  with ARect do begin

    //First Bottom-Right, so Top-Left stays the same in EmptyRect.
    Ready:= false;
    while (not Ready) and (Bottom > Top) do begin
      MaxBottom:= -1;
	    i:= Left;
	    while i <= Right do begin
	      c:= GetLifeCel(i+a,Bottom+a,false);
	      MaxBottom:= Max(c.BottomRow(QCycle),MaxBottom);
	      i:= (i+16) and $ffffFFF0
	    end; {while}
      Bottom:= (Bottom and $FFFFFFF0) + MaxBottom;
      Ready:= (MaxBottom > -1)
    end; {while}

    Ready:= false;
    while (not Ready) and (Right > Left) do begin
      MaxRight:= -1;
	    i:= Top;
	    while i <= Bottom do begin
	      c:= GetLifeCel(Right+a,i+a,false);
	      MaxRight:= Max(c.RightCol(QCycle),MaxRight);
	      i:= (i+16) and $ffffFFF0;
	    end; {while}
      Right:= (Right and $FFFFFFF0) + MaxRight;
      Ready:= (MaxRight <> -1);
    end; {while}

    //then top and bottom
    Ready:= false;
    while (not Ready) and (Right > Left) do begin
      MinLeft:= 16;
	    i:= Top;
	    while i <= Bottom do begin
	      c:= GetLifeCel(Left+a,i+a,false);
	      MinLeft:= Min(c.LeftCol(QCycle),MinLeft);
	      i:= (i+16) and $ffffFFF0;
	    end; {while}
      Left:= (Left and $FFFFFFF0) + MinLeft;
      Ready:= (MinLeft <> 16);
    end;

    Ready:= false;
    while (not Ready) and (Bottom > Top) do begin
      MinTop:= 16;
      i:= Left;
	    while i <= Right do begin
	      c:= GetLifeCel(i+a,Top+a,false);
	      MinTop:= Min(c.TopRow(QCycle),MinTop);
	      i:= (i+16) and $ffffFFF0
	    end;
      Top:= (Top and $FFFFFFF0) + MinTop;
      Ready:= (MinTop <> 16);
    end; { while }

  end; {with}
  //translate Rect back to UI coordinates.
  if QCycle then OffsetRect(ARect,1,1);
  //Make sure the rect is shrunken and not expanded.
  //IntersectRect(NewRect,ARect,SaveRect);
  if ARect.Top < SaveRect.Top then ARect.Top:= SaveRect.top;
  if ARect.Bottom > SaveRect.Bottom then ARect.Bottom:= SaveRect.Bottom;
  if ARect.Left < SaveRect.Left then ARect.Left:= SaveRect.Left;
  if ARect.Right > SaveRect.Right then ARect.Right:= SaveRect.Right;
  //Correct any strange swaps of top and bottom or left and right,
  //due to the above tests.
  if ARect.Top > ARect.Bottom then ARect.Top:= ARect.Bottom;
  if ARect.Left > ARect.Right then ARect.Left:= ARect.Right;


  //Make sure that if the rect is empty the top-left corner stays
  //as it was set by the user, so everything stays aligned.
  //No expansion of the SelRect, to differenciate between empty
  //and 1 pixel rects, this is done by ShrinkSelRect;
end;(**)

procedure TUniverse.ResetBoundingBox;
var
  AClipRect: TRect;
begin
  AClipRect:= GetBoundingBox;
  FClipRect:= AClipRect;
end;

procedure TUniverse.LoadFromStringList(AList: TStringList);
var
  TempDesc: TStringList;
begin
  WriteLock:= true;
  ReadLife(AList,Self);
  ResetBoundingBox;
  TempDesc:= GetDescription(AList);
  Description.Text:= trim(TempDesc.Text);
  TempDesc.Free;
  WriteLock:= false;
end;

procedure TUniverse.LoadFromFile(AFile: string);
var
  LifeLines: TStringList;
  LifeBitmap: TBitmap;
  LifeGifBitmap: TGifImage;
  FileIsText: boolean;

  function WaitForTempFile: boolean;
  var
    FileHandle: THandle;
    WaitTime: integer;
    OkNow: boolean;
  begin
    OkNow:= false;
    if pos(strtemp,lowercase(AFile)) <> 0 then begin
      //wait a little while for the tempfile to mature.
      WaitTime:= 0;
      repeat
        Sleep(100);
        Inc(WaitTime,100);
        FileHandle:= CreateFile(PChar(AFile),
                     GENERIC_READ,
                     FILE_SHARE_DELETE or FILE_SHARE_READ	or FILE_SHARE_WRITE,
                     nil,
                     OPEN_EXISTING,
                     FILE_ATTRIBUTE_NORMAL,0);
        if FileHandle <> INVALID_HANDLE_VALUE then begin
          OKNow:= GetFileSize(FileHandle,nil) > 0;
        end;
        CloseHandle(FileHandle);
      until OkNow or (WaitTime > 1000);
    end; {if}
    Result:= OKNow;
  end;

begin
  FileIsText:= true;
  PatternName:= ExtractFileName(AFile);
  if Uppercase(ExtractFileExt(AFile)) = Uppercase(cstrProLifeExt) then begin
    FileIsText:= not(ReadProLife(AFile,Self));
  end {if}
  else if Uppercase(ExtractFileExt(AFile)) = Uppercase(cstrBitmapExt) then begin
    LifeBitmap:= TBitmap.Create;
    FileIsText:= false;
    try
      LifeBitmap.LoadFromFile(AFile);
      ReadBitmap(LifeBitmap,Self);
      except FileIsText:= true;
    end; {try}
    LifeBitmap.Free;
  end {else if}
  else if Uppercase(ExtractFileExt(AFile)) = Uppercase(cstrGifExt) then begin
    LifeBitmap:= TBitmap.Create;
    LifeGifBitmap:= TGifImage.Create;
    FileIsText:= false;
    try
      LifeGifBitmap.LoadFromFile(AFile);
      LifeBitmap.Assign(LifeGifBitmap.Bitmap);
      //Clipboard.Assign(LifeBitmap); //************ debug code
      ReadBitmap(LifeBitmap,Self);
    except
      FileIsText:= true;
    end; {try}
    LifeBitmap.Free;
    LifeGifBitmap.Free;
  end; {else if}
  if not FileIsText then RuleString:= DefaultRules
  else begin
    LifeLines:= TStringList.Create;
    try
      try
        LifeLines.LoadFromFile(AFile);
      except
        LifeLines.LoadFromFile(AFile+' ');
        //if WaitForTempFile then LifeLines.LoadFromFile(AFile);
      end; {try}
      LoadFromStringList(LifeLines);
      RuleString:= LifeLoad.GetRules(LifeLines);
    finally
      LifeLines.Free;
    end; {try}
  end; {if}
end;

function TUniverse.SaveToStringList(FileFormat: integer; IncludeTorusData: boolean): TStringList;
begin
  Result:= TStringList.Create;
  WriteLife(Result,Self,FileFormat,IncludeTorusData);
end;

procedure TUniverse.LoadFromBitmap(ABitmap: TBitmap);
begin
  FDescription.Clear;
  ReadBitmap(ABitmap,Self);
  ResetBoundingBox;
end;

function TUniverse.SaveToBitmap: TBitmap;
begin
  Result:= TBitmap.Create;
  WriteBitmap(Result,Self);
end;

function TUniverse.SaveToBitmap(const ClipRect: TRect): TBitmap;
var
  x,y: integer;
begin
  Result:= TBitmap.Create;
  Result.Monochrome:= true;
  with ClipRect, Result do begin
    Height:= Bottom - Top;
    Width:= Right - Left;
    for x:= 0 to Width -1 do for y:= 0 to Height -1 do begin
      case Self.CelState(x+Left, y+Top) of
        true: Canvas.Pixels[x,y]:= clBlack;
        false: Canvas.Pixels[x,y]:= clWhite;
      end; {case}
    end; {for x,y}
  end; {with}
end;

function TUniverse.SaveToGifBitmap: TGifImage;
var
  ABitmap: TBitmap;
begin
  ABitmap:= SaveToBitmap;
  Result:= TGifImage.Create;
  Result.Assign(ABitmap);
  ABitmap.Free;
end;


{$ifopt I-}
{$I+}   //enable IO-exceptions
{$define cancelI}
{$endif}

procedure TUniverse.SaveToFile(AFile: string; FileFormat: integer; IncludeTorusData: boolean);
var
  LifeBitmap: TBitmap;
  LifeGifBitmap: TGifImage;
  LifeLines: TStringList;
begin
  LifeLines:= nil;
  LifeBitmap:= nil;
  try
    if FileFormat = smBitmap then begin
      LifeBitmap:= SaveToBitmap;
      try
        LifeBitmap.SaveToFile(AFile);
        except on E:exception do
          ShowMessage('File "'+AFile+' could not be saved.'+#10+#13+
                      'The following error has occured:'+
                      E.Message);
      end; {try}
    end {if}
    else if FileFormat = smGif then begin
      LifeGifBitmap:= SaveToGifBitmap;
      try
        LifeGifBitmap.SaveToFile(AFile);
        except on E:exception do
          ShowMessage('File "'+AFile+' could not be saved.'+#10+#13+
                      'The following error has occured:'+
                      E.Message);
      end; {try}
    end {else if}
    else begin
      LifeLines:= SaveToStringList(FileFormat, IncludeTorusData);
      try
        LifeLines.SaveToFile(AFile);
        except on E:exception do
          ShowMessage('File "'+AFile+' could not be saved.'+#10+#13+
                      'The following error has occured:'+
                      E.Message);
      end; {try}
    end; {try}
    finally begin
      LifeLines.Free;
      LifeBitmap.Free;
    end;
  end; {try}
end;

{$ifdef cancelI}
{$i-}
{$endif}


function TUniverse.MakeSnapshotCopy: TSnapshot;
var
  c: TLifeCel;
  ALen: integer;
  StreamSize: integer;
  Reserved: integer;
  RuleCopy: string;
  NowValue: TDateTime;
begin
  Result:= TSnapshot.Create;
  StopSnapshot:= false;
  //RattleAllCages;
  RuleCopy:= RuleString;
  with Result do begin
    //First write universe stuff.
    Write(Counter,SizeOf(Counter));
    Write(Reserved,SizeOf(Reserved));
    Write(Reserved,SizeOf(Reserved));
    NowValue:= Now;
    Write(NowValue,SizeOf(System.TDateTime));//timestamp.
    Write(QCycle,SizeOf(QCycle));
    ALen:= Length(FPatternName);
    Write(ALen,SizeOf(ALen));
    Write(PChar(@FPatternName[1])^,ALen);
    if RuleCopy = '' then RuleCopy:= '23/3';
    ALen:= Length(RuleCopy);
    Write(ALen,SizeOf(ALen));
    Write(PChar(@RuleCopy[1])^,ALen);
    with ClipRect do begin
      Write(Left,SizeOf(Left));
      Write(Top,SizeOf(Top));
      Write(Right,SizeOf(Right));
      Write(Bottom,SizeOf(Bottom));
    end; {with}
    StreamSize:= Result.Position;
    //reserve space for the stream's size tag.
    Write(StreamSize,SizeOf(StreamSize));
    c:= living.next;
    while (c <> living) and not (StopSnapshot) do begin
      c.Write(Result, QCycle);
      c:= c.next;
    end; {while}
    c:= hibernating.next;
    while (c <> hibernating) and not (StopSnapshot) do begin
      c.write(Result, QCycle);
      c:= c.Next;
    end; {while}
    //now that we know the size, write it down.
    Result.Position:= StreamSize;
    StreamSize:= Result.Size;
    Write(StreamSize,SizeOf(StreamSize));
  end; {with}
  if StopSnapshot then begin
    Result.Free;
    Result:= nil;
  end;
end;

procedure TUniverse.RewindToSnapshot(AStream: TStream);
var
  ALen: integer;
  Reserved: integer;
  Reserved2: TDateTime;
  c: TLifeCel;
  DummyRect: TRect;
  RuleStorage: {Short}string;
  StreamSize: integer;
begin
  Clear(false); //Clear has also checks for a writelock.
  WriteLock:= true;
  with AStream do begin
    //First Read universe stuff.
    Read(FCounter,SizeOf(FCounter));
    Read(Reserved,SizeOf(Reserved));
    Read(Reserved,SizeOf(Reserved));
    Read(Reserved2,SizeOf(Reserved2));
    Read(QCycle,SizeOf(QCycle));
    Read(ALen,SizeOf(ALen));
    SetLength(FPatternName,ALen);
    Read(PChar(@FPatternName[1])^,ALen);
    Read(ALen,SizeOf(ALen));
    SetLength(RuleStorage,ALen);
    Read(PChar(@RuleStorage[1])^,ALen);
    RuleString:= RuleStorage;
    with {ClipRect} DummyRect do begin  //********************
      Read(Left,SizeOf(Left));
      Read(Top,SizeOf(Top));
      Read(Right,SizeOf(Right));
      Read(Bottom,SizeOf(Bottom));
    end; {with}
    Read(StreamSize,SizeOf(StreamSize));
    while not(AStream.Position >= StreamSize) do begin
      c:= TLifeCel.Create;
      c.Read(AStream, QCycle);
      InsertCage(c);
    end; {while}  (**)
  end; {with}
  CorrectUniverse;
  writeLock:= false;
end;

function TUniverse.IsUniverseEqual(Universe2: TUniverse): boolean;
var
  c,c2: TLifeCel;
  OutofPhase: boolean;
  Diff: integer;
begin
  OutOfPhase:= QCycle xor Universe2.QCycle;
  Diff:= 0;
  c:= Living.Next;
  while (c <> living) and (Diff = 0) do begin
    c2:= Universe2.GetLifeCel(c.coor.x*16,c.coor.y*16,OutOfPhase);
    if c2 = nil then Diff:= c.GetCelSum(QCycle)
    else Diff:= c.GetCelDiff(c2,QCycle,Universe2.QCycle);
    c:= c.Next;
  end; {while}

  c:= hibernating.next;
  while (c <> hibernating) and (Diff = 0) do begin
    c2:= Universe2.GetLifeCel(c.coor.x*16,c.coor.y*16,OutOfPhase);
    if c2 = nil then Diff:= c.GetCelSum(QCycle)
    else Diff:= c.GetCelDiff(c2,QCycle,Universe2.QCycle);
    c:= c.Next;
  end; {while}
  Result:= Diff = 0;
end;

procedure TUniverse.FillRandom(ARect: TRect; APercentage, PasteMode: integer);
var
  x,y: integer;
begin
  WriteLock:= true;
  with ARect do try
    case Pastemode of
      lpmOr, lpmXor: {do nothing};
      lpmPut: FillRect(ARect,faClear);
      lpmError: if not(IsSelEmpty(ARect)) then Abort;
    end; {case}
    case PasteMode of
      lpmOr, lpmPut, lpmError: begin
        for x:= Left to Right-1 do for y:= Top to Bottom-1 do begin
          if Random(100) < APercentage then ChangeCel(x,y,true);
        end; {for xy}
      end; {lpmOr,,}
      lpmXor: begin
        for x:= Left to Right-1 do for y:= Top to Bottom-1 do begin
          if Random(100) < APercentage then ChangeCel(x,y,not(CelState(x,y)));
        end; {for xy}
      end; {lpmXor}
    end; {case}
    //Rect done
    finally WriteLock:= false;
  end; {with try}
end;

function TUniverse.CutRect(ARect: TRect; UseClipboard: boolean): TUniverse;
begin
  Result:= CopyRect(ARect, UseClipboard);
  FillRect(ARect, faClear); //has it's own writelock.
end;


//before entering this function, ARect must already be shrunken to fit.
function TUniverse.CopyRect(ARect: TRect; UseClipboard: boolean): TUniverse;
var
  c: TLifeCel;
  Source: TLifeCel;
  x,y: integer;
  LifeLines: TStringList;
  a: integer;
begin
  Result:= TUniverse.Create(RuleString, Neighborhood);
  Result.OnRuleChange:= Self.OnRuleChange;
  Result.QCycle:= QCycle;
  a:= integer(QCycle);
  with ARect do begin
    if not(IsMyRectEmpty(ARect)) then begin
      //always start at cell boundary, otherwise we would get duplicates.
      y:= (Top - 1) and $FFFFfff0;
        //without the border q-cycles seem to go wrong.
        while (y <= Bottom+16) do begin
        x:= (Left -1) and $FFFFfff0;
        while (x <= Right+16) do begin
          Source:= GetLifeCel(x+a,y+a,false);
          if Assigned(Source) then begin
            c:= Result.AllocateCage((Source.coor.x*16),(Source.coor.y*16));
            c.Assign(Source);
          end;
          inc(x,16);
          //X:= (x+16) and $ffffFFF0;
        end; {while}
        Inc(y,16);
        //Y:= (y+16) and $ffffFFF0;
      end; {while}
      Result.FClipRect:= ARect;
    end; {if}
  end; {with}
  //Save universe as text
  //no conditions for this yet.
  if UseClipboard then begin

    LifeLines:= TStringList.Create;
    if WriteLife(LifeLines,Result,smDefault, false) then begin
      //use the 1st y line to store ID of cut-out. (=Hex pointer value).
      LifeLines.Insert(0,'#D'+IntToHex(integer(Result),8));
      Clipboard.SetTextBuf(LifeLines.GetText);
    end; {if}
  end; {if}
  //Rect ok.
end;

function TUniverse.Clone: TUniverse;
var
  c: TLifeCel;
  Source: TLifeCel;
begin
  Result:= TUniverse.Create(RuleString, Neighborhood);
  Result.OnRuleChange:= Self.OnRuleChange;
  Result.QCycle:= QCycle;
  Source:= living.next;
  while Source <> living do begin
    c:= Result.AllocateCage((Source.coor.x*16),(Source.coor.y*16));
    c.Assign(Source);
    Source:= Source.next;
  end; {while}
  Source:= hibernating.next;
  while Source <> hibernating do begin
    c:= Result.AllocateCage((Source.coor.x*16),(Source.coor.y*16));
    c.Assign(Source);
    Source:= Source.next;
  end; {while}
  Result.CorrectUniverse;
  Result.FClipRect:= FClipRect;
  Result.FLimit:= FLimit;
end;



//Draw a line using Bresenham's algorithm
procedure TUniverse.DrawLine(x1,y1,x2,y2: integer; DragState: TLineDrawState);

  procedure DrawDot(Celx,Cely: integer);
  begin
    if (DragState = lds_Xor) then ChangeCel(CelX,CelY,not(CelState(CelX,CelY)))
    else ChangeCel(Celx,Cely,Boolean(DragState));
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

  procedure DrawVertLine(x0,y0,DeltaY: integer);
  var
    Direction: integer;
    c: TLifeCel;
    cx: integer;
    IsForced: boolean;
  begin
    writelock:= true;
    if DeltaY > 0 then Direction:= 1 else Direction:= -1;
    //can we work with whole cells at once?
    if ABS(DeltaY) < 32 then begin
      DrawDot(x0,y0);
      repeat
        Inc(y0,Direction);
        Dec(DeltaY,Direction);
        DrawDot(x0,y0);
      until (DeltaY = 0);
    end
    else begin
      //Length > 32, so we will span at least one full cell.
      IsForced:= boolean(DragState);
      if QCycle then begin Dec(x0); Dec(y0); end;
      cx:= x0 and 15;
      case Direction of
        1: begin
          c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
          if c <> nil then c.DrawVertLineDown(cx, y0 and 15,DragState,Qcycle);
          Dec(DeltaY,16-(y0 and 15));
          repeat
            Inc(y0,16);
            Dec(DeltaY,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawVertLineDown(cx,0,DragState,Qcycle);
          until (DeltaY < 16);
          if DeltaY <> 0 then begin
            Inc(y0,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawVertLineUp(cx,DeltaY,DragState,Qcycle);
          end; {if}
        end; {1:}
        -1: begin
          c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
          if c <> nil then c.DrawVertLineUp(cx,y0 and 15,DragState,Qcycle);
          Inc(DeltaY,(y0 and 15));
          repeat
            Dec(y0,16);
            Inc(DeltaY,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawVertLineUp(cx,15,DragState,Qcycle);
          until (DeltaY > -16);
          if DeltaY <> 0 then begin
            Dec(y0,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawVertLineDown(cx,-DeltaY,DragState,Qcycle);
          end; {if}
        end; {-1}
      end; {case}
    end; {else}
    writelock:= false;
  end; {DrawVertLine}

  procedure DrawHorzLine(x0,y0,DeltaX: integer);
  var
    Direction: integer;
    c: TLifeCel;
    cy: integer;
    IsForced: boolean;
  begin
    writelock:= true;
    if DeltaX > 0 then Direction:= 1 else Direction:= -1;
    //can we work with whole cells at once?
    if ABS(DeltaX) < 32 then begin
      DrawDot(x0,y0);
      repeat
        Inc(x0,Direction);
        Dec(DeltaX,Direction);
        DrawDot(x0,y0);
      until (DeltaX = 0);
    end
    else begin
      //Length > 32, so we will span at least one full cell.
      IsForced:= boolean(DragState);
      if QCycle then begin Dec(x0); Dec(y0); end;
      cy:= y0 and 15;
      case Direction of
        1: begin
          c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
          if c <> nil then c.DrawHorzLineRight(x0 and 15,cy,DragState,Qcycle);
          Dec(DeltaX,16-(x0 and 15));
          repeat
            Inc(x0,16);
            Dec(DeltaX,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawHorzLineRight(0,cy,DragState,Qcycle);
          until (DeltaX < 16);
          if DeltaX <> 0 then begin
            Inc(x0,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawHorzLineLeft(DeltaX,cy,DragState,Qcycle);
          end; {if}
        end; {1:}
        -1: begin
          c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
          if c <> nil then c.DrawHorzLineLeft(x0 and 15,cy,DragState,Qcycle);
          Inc(DeltaX,(x0 and 15));
          repeat
            Dec(x0,16);
            Inc(DeltaX,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawHorzLineLeft(15,cy,DragState,Qcycle);
          until (DeltaX > -16);
          if DeltaX <> 0 then begin
            Dec(x0,16);
            c:= GetLifeCelSlowly(x0+integer(Qcycle),y0+integer(Qcycle),IsForced);
            if c <> nil then c.DrawHorzLineRight(16+DeltaX,cy,DragState,Qcycle);
          end; {if}
        end; {-1}
      end; {case}
    end; {else}
    writelock:= false;
  end; {DrawHorzLine}

var
  DeltaX, DeltaY: integer;
  Temp: integer;
begin
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
  if (DeltaX or DeltaY) = 0 then Exit;
  if (DeltaX = 0) then DrawVertLine(x1,y1,DeltaY)
  else if (DeltaY = 0) then DrawHorzLine(x1,y1,DeltaX)
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

function TUniverse.InsertShape(AUniverse: TUniverse; APos: TPoint; PasteMode: TPasteMode): Boolean;
var
  x,y: integer;
  x1,y1,x2,y2: integer;
  PasteRect: TRect;
  CanPaste: boolean;
  ACel: TLifeCel;
begin
  WriteLock:= true;
  CanPaste:= true;
  if Assigned(AUniverse) then with AUniverse.ClipRect do begin
    PasteRect:= AUniverse.ClipRect;
    OffsetRect(PasteRect,(APos.X-Left), (APos.y-Top));
    case PasteMode of
      lpmPut: begin
        WriteLock:= false;
        FillRect(PasteRect, faClear);
        WriteLock:= true;
      end;
      lpmError: begin
        ShrinkSelRect(PasteRect);
        CanPaste:= IsMyRectEmpty(PasteRect);
      end; {lpmError}
    end; {case}
    case PasteMode of
      lpmXor:begin
        x:= Left;
        while x < Right do begin
          //for y:=Top to Bottom-1 do begin
          y:= Top;
          while y < Bottom do begin
            if AUniverse.FHashTable.RetrieveCached(x,y) <> nil then begin
              if AUniverse.CelState(x,y) then try
                x2:= APos.x+(x-Left);
                y2:= APos.y+(y-Top);
                ChangeCel(x2,y2,CelState(x2,y2) xor AUniverse.CelState(x,y));
              except
                {ignore errors}
              end; {if try}
            end;
            inc(y);
          end; {while y}
          inc(x);
        end; {while x}  (**)

        (**ACel:= AUniverse.FHashTable.First;
        while ACel <> nil do begin
          x1:= ACel.Coor.x*16;
          y1:= ACel.Coor.y*16;
          for x:= x1-1 to x1+17 do begin
            for y:= y1-1 to y1+17 do begin
              if PtInRect(AUniverse.ClipRect,Point(x,y)) and AUniverse.CelState(x,y) then begin
                x2:= APos.x+(x-Left);
                y2:= APos.y+(y-Top);
                //ChangeCel(x2,y2,AUniverse.CelState(x,y) xor CelState(x2,y2));
                ChangeCel(x2,y2,true);
              end; {if}
            end; {for y}
          end; {for x}
          ACel:= AUniverse.FHashTable.Next;
        end;  {while} (**)
      end; {lpmXor}
      else if CanPaste then begin
        //Browse though the hashtable of the paste universe.
        ACel:= AUniverse.FHashTable.First;
        while ACel <> nil do begin
          x1:= ACel.Coor.x*16;
          y1:= ACel.Coor.y*16;
          for x:= x1-1 to x1+17 do begin
            for y:= y1-1 to y1+17 do begin
              if PtInRect(AUniverse.ClipRect,Point(x,y)) and AUniverse.CelState(x,y) then try
                ChangeCel(APos.x+(x-Left),APos.y+(y-Top),true);
              except
                {ignore errors}
              end; {if try}
            end; {for y}
          end; {for x}
          ACel:= AUniverse.FHashTable.Next;
        end; (**)

        (*x:= Left;
        while x < Right do begin
          //for y:=Top to Bottom-1 do begin
          y:= Top;
          while y < Bottom do begin
            if AUniverse.FHashTable.RetrieveCached(x,y) <> nil then begin
              if AUniverse.CelState(x,y) then begin
                ChangeCel(APos.x+(x-Left),APos.y+(y-Top),true);
              end; {if}
            end;
            inc(y);
          end; {while y}
          inc(x);
        end; {while x}  (**)
      end; {else}
    end;  {case}
  end; {with}
  Result:= CanPaste;
  WriteLock:= false;
  //Rect ok.
end;

procedure TUniverse.PasteRect(AUniverse: TUniverse; APos: TPoint;
                             PasteMode: TPasteMode);
var
  MyHandle: THandle;
  TextPtr: PChar;
  PatternList: TStringList;
  TempDesc: TStringList;
  UniverseID: string;
  A,B: string;
  UseClipboard: boolean;
  ABitmap: TBitmap;
begin
  UseClipboard:= false;
  if (Clipboard.HasFormat(CF_TEXT)) then begin
    if (AUniverse <> nil) then begin
      UniverseID:= Clipboard.AsText;
      if Copy(UniverseID,1,2) = '#D' then begin
        A:= IntToHex(Integer(AUniverse),8);
        B:= Copy(UniverseID,3,Min(8,Length(UniverseID)-2));
        UseClipboard:= (A <> B);
      end {if}
      else UseClipboard:= true;
    end {if}
    else if AUniverse = nil then begin
      AUniverse:= TUniverse.Create(RuleString, Neighborhood);
      AUniverse.OnRuleChange:= Self.OnRuleChange;
      UseClipboard:= true;
    end; {else}

    //@@@@@ more tests here.
    if UseClipBoard then begin
      PatternList:= TStringList.Create;
      try
        Clipboard.Open;
        try
          MyHandle:= Clipboard.GetAsHandle(CF_TEXT);
          TextPtr:= GlobalLock(MyHandle);
          PatternList.SetText(TextPtr);
          GlobalUnlock(MyHandle);
          finally Clipboard.Close;
        end; {try}
        AUniverse.Clear(true);
        ReadLife(PatternList,AUniverse);
        AUniverse.FClipRect:= AUniverse.GetBoundingBox;
        if Self.IsEmpty then begin
          TempDesc:= GetDescription(PatternList);
          Description.Text:= TempDesc.Text;
          TempDesc.Free;
        end;
        finally PatternList.Free;
      end; {try}
    end; {if}
  end {if CF_TEXT}
  else if (Clipboard.HasFormat(CF_BITMAP)) then begin
    Clipboard.Open;
    try
      ABitmap:= TBitmap.Create;
      if (AUniverse = nil) then AUniverse:= TUniverse.Create(RuleString, Neighborhood)
      else AUniverse.Clear(true);
      AUniverse.OnRuleChange:= Self.OnRuleChange;
      try
        ABitmap.LoadFromClipBoardFormat(cf_BITMAP,ClipBoard.GetAsHandle(cf_BITMAP),0);
        AUniverse.LoadFromBitmap(ABitmap);
        AUniverse.FClipRect:= AUniverse.GetBoundingBox;
      finally
        ABitmap.free;
      end; {try}
    finally
      Clipboard.Close;
    end; {try}
  end; {else if CF_BITMAP}
  InsertShape(AUniverse, APos, PasteMode); //has it's own writelock.
  if AUniverse.RuleString <> RuleString then RuleString:= AUniverse.RuleString;
  //Rect ok.
end;

procedure TUniverse.ClearSmallRect(ARect: TRect); far;
var
  x,y: integer;
begin
  if not(IsMyRectEmpty(ARect)) then with ARect do begin
    for x:= Left to Right-1 do for y:= Top to Bottom-1 do ChangeCel(x,y,false);
  end; {with}
end; {ClearSmallRect}

procedure TUniverse.ClearBigRect(ARect: TRect);
var
  x,y: integer;
  c: TLifeCel;
begin
  //Rect width and Height always are a multiple of 16.
  if not(IsMyRectEmpty(ARect)) then with ARect do begin
    x:= Left;
    while x < (Right) do begin
      y:= Top;
      while y < (Bottom) do begin
        c:= GetLifeCel(x,y,false);
        if Assigned(c) then c.Clear(QCycle);
        Inc(y,16);
      end;{while}
      Inc(x,16);
    end;{while}
  end; {with}
end; {ClearBigRect}


procedure TUniverse.FillSmallRect(ARect: TRect);
var
  x,y: integer;
begin
  if not(IsMyRectEmpty(ARect)) then with ARect do begin
    for x:= Left to Right-1 do for y:= Top to Bottom-1 do ChangeCel(x,y,true);
  end; {with}
end; {FillSmallRect}

procedure TUniverse.FillBigRect(ARect: TRect);
var
  x,y: integer;
  c: TLifeCel;
begin
  //Rect width and Height always are a multiple of 16.
  if not(IsMyRectEmpty(ARect)) then with ARect do begin
    x:= Left;
    while x < (Right) do begin
      y:= Top;
      while y < (Bottom) do begin
        c:= GetLifeCel(x,y,true);
        if Assigned(c) then c.FillBlack(QCycle);
        Inc(y,16);
      end;{while}
      Inc(x,16);
    end;{while}
  end; {with}
end; {FillBigRect}

procedure TUniverse.InvertSmallRect(ARect: TRect);
var
  x,y: integer;
begin
  if not(IsMyRectEmpty(ARect)) then with ARect do begin
    for x:= Left to Right-1 do for y:= Top to Bottom-1 do
      ChangeCel(x,y,not(CelState(x,y)));
  end; {with}
end; {InvertSmallRect}

procedure TUniverse.InvertBigRect(ARect: TRect);
var
  x,y: integer;
  c: TLifeCel;
begin
  //Rect width and Height always are a multiple of 16.
  if not(IsMyRectEmpty(ARect)) then with ARect do begin
    x:= Left;
    while x < (Right) do begin
      y:= Top;
      while y < (Bottom) do begin
        c:= GetLifeCel(x,y,true);
        if Assigned(c) then c.Invert(QCycle);
        Inc(y,16);
      end;{while}
      Inc(x,16);
    end;{while}
  end; {with}
end; {InvertBigRect}


procedure TUniverse.DrawBox(ARect: TRect);
begin
  with ARect do begin
    Dec(Right); Dec(Bottom);
    DrawLine(Left,Top,Right,Top,lds_on); //Top
    DrawLine(Left,Bottom,Right,Bottom,lds_on); //bottom
    DrawLine(Left,Top,Left,Bottom,lds_on); //left
    DrawLine(Right,Top,Right,Bottom,lds_on); //Right
  end; {with}
end;

procedure TUniverse.FillRect(ARect: TRect; DoWhat: TFillAction);
type
  TFillProc = procedure(ARect: TRect) of object;
var
  RSides, RMiddle: TRect;
  a: integer;
  DoSmallRect: TFillProc;
  DoBigRect: TFillProc;

begin
  WriteLock:= true;
  case DoWhat of
    faClear:begin
      DoSmallRect:= ClearSmallRect;
      DoBigRect:= ClearBigRect;
    end; {dwClear}
    faFill:begin
      DoSmallRect:= FillSmallRect;
      DoBigRect:= FillBigRect;
    end; {dwFill}
    else begin  //case faInvert
      DoSmallRect:= InvertSmallRect;
      DoBigRect:= InvertBigRect;
    end; {dwInvert}
  end; {case}

  RattleAllCages;
  a:= integer(QCycle);
  //First we get a Rect with whole cells in it, this can be cleared.
  //a whole cell at a time.
  //The other rects are deduced from that.
  with ARect do begin
    RMiddle.Top:=   ((Top + 16) and $FFFFfff0)+a;
    RMiddle.Left:=  ((Left + 16) and $FFFFfff0)+a;
    RMiddle.Bottom:=  ((Bottom-16) and $FFFFfff0)+a;
    RMiddle.Right:=  ((Right-16) and $FFFFfff0)+a;
    if IsRectEmpty(RMiddle) then DoSmallRect(ARect)
    else begin
      DoBigRect(RMiddle);
      //Top
      RSides:= ARect;
      RSides.Bottom:= RMiddle.Top;
      DoSmallRect(RSides);
      //Bottom
      RSides:= ARect;
      RSides.Top:= RMiddle.Bottom;
      DoSmallRect(RSides);
      //Left
      RSides.Left:= Left;
      RSides.Top:= RMiddle.Top;
      RSides.Bottom:= RMiddle.Bottom;
      RSides.Right:= RMiddle.Left;
      DoSmallRect(RSides);
      //Right;
      RSides.Left:= RMiddle.Right;
      RSides.Right:= Right;
      DoSmallRect(RSides);
    end; {else}
  end; {with}
  WriteLock:= false;
  //Rect OK.
end;



    {*
     * ImplementRules()
     *
     * Expand Crunch[], Munch[] tables from Rule[] table.
     *
     * This is done so that we can work from a 4x4 -> 2x2
     * conversion instead of the old 3x3 -> 1x1.  This makes the
     * algorithm at least 4x faster, at the price of a lookup table
     * 128 times larger, and some extra initialization time.
     *}
procedure TUniverse.ImplementRules;
var
  ic000, i3000, i0c00, i0300, i00c0, i0030,
  i8000, i4000, i2000, i1000, i0800, i0400, i0200, i0100,
  i0080, i0040, i0020, i0010: integer;
  r8000, r4000, r2000, r1000, r0800, r0400,
  r0080, r0020, r0008, r: integer;
  r8000a, r4000a, r2000a, r2000b: integer;
  r0800a, r0800b, r0200a, r0200b,
  r0100a, r0100b, r0100c, r0100d: integer;
  r0080a, r0040a, r0040b,
  r0010a, r0010b, r0010c, r0010d: integer;
  r0004a, r0004b, r0004c, r0004d,
  r0002a, r0002b, r0002c, r0002d,
  r0001a, r0001b: integer;
  m1, m2: integer;

  tempi,i: integer;
begin
  //WriteLock:= true;
  for tempi:= 0 to ($FFF0 div $10) do begin
    i:= tempi * $10;

    ic000:=i and $c000; i3000:=i and $3000;
    i0c00:=i and $0c00; i0300:=i and $0300;
    i00c0:=i and $00c0; i0030:=i and $0030;
    i8000:=i and $8000; i4000:=i and $4000; i2000:=i and $2000; i1000:=i and $1000;
    i0800:=i and $0800; i0400:=i and $0400; i0200:=i and $0200; i0100:=i and $0100;
    i0080:=i and $0080; i0040:=i and $0040; i0020:=i and $0020; i0010:=i and $0010;

    r8000:= (ic000 shr 7) or (i3000 shr 8) or (i0c00 shr 9) or (i0080 shr 1) or (i0020 shr 2);
    r4000:=  i00c0        or (i0030 shr 1) or (i4000 shr 6) or (i1000 shr 7) or (i0400 shr 8);
    r2000:= (i3000 shr 5) or (i0c00 shr 6) or (i0300 shr 7) or (Rol32(i0020, 1));
    r1000:= (Rol32(i0030, 2)) or (i1000 shr 4) or (i0400 shr 5) or (i0100 shr 6);

    r0800:= (i0c00 shr 3) or (i0300 shr 4) or (ic000 shr 13) or (i0080 shr 7);
    r0400:= (i00c0 shr 6) or (i0400 shr 2) or (i0100 shr 3) or (i4000 shr 12);
    r    := (i0300 shr 1) or (ic000 shr 10) or (i3000 shr 11) or (i0080 shr 4) or (i0020 shr 5);
    r0200a:= Byte(Rules[r]) * $0220;
    r0200b:= Byte(Rules[r or 64]) * $0220;
    r    := (i00c0 shr 3) or (i0030 shr 4) or  i0100 or (i4000 shr 9) or (i1000 shr 10);
    r0100a:= Byte(Rules[r]) * $0110;
    r0100b:= Byte(Rules[r or 64]) * $0110;
    r0100c:= Byte(Rules[r or 128]) * $0110;
    r0100d:= Byte(Rules[r or 192]) * $0110;

    r0080:= (Rol32(i00c0,1)) or i0030 or (i8000 shr 9) or (i2000 shr 10) or (i0800 shr 11);
    r    := (ic000 shr 8) or (i3000 shr 9) or (i0c00 shr 10) or (Rol32(i0040,2)) or (Rol32(i0010,1));
    r0040a:= Byte(Rules[r]) * $0440;
    r0040b:= Byte(Rules[r or 4]) * $0440;
    r0020:= (Rol32(i0030,3)) or (i2000 shr 7) or (i0800 shr 8) or (i0200 shr 9);
    r    := (i3000 shr 6) or (i0c00 shr 7) or (i0300 shr 8) or (Rol32(i0010,4));
    r0010a:= Byte(Rules[r]) * $0110;
    r0010b:= Byte(Rules[r or 4]) * $0110;
    r0010c:= Byte(Rules[r or 32]) * $0110;
    r0010d:= Byte(Rules[r or 36]) * $0110;

    r0008:= (i00c0 shr 5) or (i0800 shr 5) or (i0200 shr 6) or (i8000 shr 15);
    r    := (i0c00 shr 4) or (i0300 shr 5) or (ic000 shr 14) or (i0040 shr 4);
    r0004a:= Byte(Rules[r]) * $4004;
    r0004b:= Byte(Rules[r or 32]) * $4004;
    r0004c:= Byte(Rules[r or 256]) * $4004;
    r0004d:= Byte(Rules[r or 288]) * $4004;

    r    := (i00c0 shr 2) or (i0030 shr 3) or (i0200 shr 3) or (i8000 shr 12) or (i2000 shr 13);
    r0002a:= (Byte(Rules[r]) * $2002);
    r0002b:= (Byte(Rules[r or 128]) * $2002);
    r0002c:= (Byte(Rules[r or 256]) * $2002);
    r0002d:= (Byte(Rules[r or 384]) * $2002);

    r    := (i0300 shr 2) or (ic000 shr 11) or (i3000 shr 12) or (i0040 shr 1) or (i0010 shr 2);
    r0001a:= (Byte(Rules[r]) * $1001);
    r0001b:= (Byte(Rules[r or 256]) * $1001);

    r8000a:= (Byte(Rules[r8000]) * $8008);
    r4000a:= (Byte(Rules[r4000]) * $4004);
    r2000a:= (Byte(Rules[r2000]) * $2002);
    r2000b:= (Byte(Rules[r2000 or 1]) * $2002);
    r0800a:= (Byte(Rules[r0800]) * $0880);
    r0800b:= (Byte(Rules[r0800 or 8]) * $0880);
    r0080a:= (Byte(Rules[r0080]) * $0880);

    (* 0 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000]) * $1001)
         or r0800a or (Byte(Rules[r0400]) * $0440) or r0200a or r0100a;
    m2:= r0080a or r0040a or (Byte(Rules[r0020]) * $0220) or r0010a
         or (Byte(Rules[r0008]) * $8008) or r0004a or r0002a or r0001a;
    Crunch[i]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 1:  i0001 = 1,  i0003 = 1 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 1]) * $1001)
         or r0800a or (Byte(Rules[r0400 or 8]) * $0440) or r0200a or r0100b;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 2]) * $0220) or r0010b
         or (Byte(Rules[r0008 or 16]) * $8008) or r0004b or r0002b or r0001b;
    Crunch[i+1]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+1] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 2:  i0002 = 2,  i0003 = 2 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 2]) * $1001)
         or r0800b or (Byte(Rules[r0400 or 16]) * $0440) or r0200b or r0100c;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 4]) * $0220) or r0010a
         or (Byte(Rules[r0008 or 32]) * $8008) or r0004a or r0002c or r0001a;
    Crunch[i+2]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+2] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 3:  i0001 = 1,  i0002 = 2,  i0003 = 3 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 3]) * $1001)
         or r0800b or (Byte(Rules[r0400 or 24]) * $0440) or r0200b or r0100d;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 6]) * $0220) or r0010b
         or (Byte(Rules[r0008 or 48]) * $8008) or r0004b or r0002d or r0001b;
    Crunch[i+3]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+3] := smallint((m1 and $00ff) or (m2 and $ff00));

    r4000a:= (Byte(Rules[r4000 or 1]) * $4004);
    r0080a:= (Byte(Rules[r0080 or 2]) * $0880);

    (* 4:  i0004 = 4,  i000c = 4 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 8]) * $1001)
         or r0800a or (Byte(Rules[r0400 or 64]) * $0440) or r0200a or r0100a;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 16]) * $0220) or r0010c
         or (Byte(Rules[r0008 or 128]) * $8008) or r0004c or r0002a or r0001a;
    Crunch[i+4]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+4] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 5:  i0001 = 1,  i0003 = 1,  i0004 = 4, i000c = 4 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 9]) * $1001)
         or r0800a or (Byte(Rules[r0400 or 72]) * $0440) or r0200a or r0100b;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 18]) * $0220) or r0010d
         or (Byte(Rules[r0008 or 144]) * $8008) or r0004d or r0002b or r0001b;
    Crunch[i+5]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+5] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 6:  i0002 = 2,  i0003 = 2,  i0004 = 4,  i000c = 4 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 10]) * $1001)
         or r0800b or (Byte(Rules[r0400 or 80]) * $0440) or r0200b or r0100c;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 20]) * $0220) or r0010c
         or (Byte(Rules[r0008 or 160]) * $8008) or r0004c or r0002c or r0001a;
    Crunch[i+6]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+6] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 7:  i0001 = 1,  i0002 = 2, i0003 = 3,  i0004 = 4. i000c = 4 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 11]) * $1001)
               or r0800b or (Byte(Rules[r0400 or 88]) * $0440) or r0200b or r0100d;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 22]) * $0220) or r0010d
               or (Byte(Rules[r0008 or 176]) * $8008) or r0004d or r0002d or r0001b;
    Crunch[i+7]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+7] := smallint((m1 and $00ff) or (m2 and $ff00));

    r8000a:= (Byte(Rules[r8000 or 1]) * $8008);
    r4000a:= (Byte(Rules[r4000 or 2]) * $4004);
    r2000a:= (Byte(Rules[r2000 or 8]) * $2002);
    r2000b:= (Byte(Rules[r2000 or 9]) * $2002);
    r0800a:= (Byte(Rules[r0800 or 64]) * $0880);
    r0800b:= (Byte(Rules[r0800 or 72]) * $0880);
    r0080a:= (Byte(Rules[r0080 or 4]) * $0880);

    (* 8:  i0008 = 8,  i000c = 8 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 16]) * $1001)
               or r0800a or (Byte(Rules[r0400 or 128]) * $0440) or r0200a or r0100a;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 32]) * $0220) or r0010a
               or (Byte(Rules[r0008 or 256]) * $8008) or r0004a or r0002a or r0001a;
    Crunch[i+8]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+8] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 9:  i0001 = 1,  i0003 = 1,  i0008 = 8,  i000c = 8 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 17]) * $1001)
               or r0800a or (Byte(Rules[r0400 or 136]) * $0440) or r0200a or r0100b;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 34]) * $0220) or r0010b
               or (Byte(Rules[r0008 or 272]) * $8008) or r0004b or r0002b or r0001b;
    Crunch[i+9]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+9] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 10:  i0002 = 2,  i0003 = 2,  i0008 = 8,  i000c = 8 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 18]) * $1001)
               or r0800b or (Byte(Rules[r0400 or 144]) * $0440) or r0200b or r0100c;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 36]) * $0220) or r0010a
               or (Byte(Rules[r0008 or 288]) * $8008) or r0004a or r0002c or r0001a;
    Crunch[i+10]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+10] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 11:  i0001 = 1,  i0003 = 1,  i0008 = 8,  i000c = 8 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 19]) * $1001)
               or r0800b or (Byte(Rules[r0400 or 152]) * $0440) or r0200b or r0100d;
    m2:= r0080a or r0040a or (Byte(Rules[r0020 or 38]) * $0220) or r0010b
               or (Byte(Rules[r0008 or 304]) * $8008) or r0004b or r0002d or r0001b;
    Crunch[i+11]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+11] := smallint((m1 and $00ff) or (m2 and $ff00));

    r4000a:= (Byte(Rules[r4000 or 3]) * $4004);
    r0080a:= (Byte(Rules[r0080 or 6]) * $0880);

    (* 12:  i0004 = 4,  i0008 = 8,  i000c = 12 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 24]) * $1001)
               or r0800a or (Byte(Rules[r0400 or 192]) * $0440) or r0200a or r0100a;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 48]) * $0220) or r0010c
               or (Byte(Rules[r0008 or 384]) * $8008) or r0004c or r0002a or r0001a;
    Crunch[i+12]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+12] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 13:  i0001 = 1,  i0003 = 1,  i0004 = 4,  i0008 = 8,  i000c = 12 *)
    m1:= r8000a or r4000a or r2000a or (Byte(Rules[r1000 or 25]) * $1001)
               or r0800a or (Byte(Rules[r0400 or 200]) * $0440) or r0200a or r0100b;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 50]) * $0220) or r0010d
               or (Byte(Rules[r0008 or 400]) * $8008) or r0004d or r0002b or r0001b;
    Crunch[i+13]:= smallint((m1 and $ff00) or (m2 and $00ff));
    Munch[i+13] := smallint((m1 and $00ff) or (m2 and $ff00));

    (* 14:  i0002 = 2,  i0003 = 2,  i0004 = 4,  i0008 = 8,  i000c = 12 *)
    m1:= r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 26]) * $1001)
               or r0800b or (Byte(Rules[r0400 or 208]) * $0440) or r0200b or r0100c;
    m2:= r0080a or r0040b or (Byte(Rules[r0020 or 52]) * $0220) or r0010c
               or (Byte(Rules[r0008 or 416]) * $8008) or r0004c or r0002c or r0001a;
    Crunch[i+14]:= smallint(((m1 and $ff00) or (m2 and $00ff)));
    Munch[i+14] := smallint(((m1 and $00ff) or (m2 and $ff00)));

    (* 15:  i0001 = 1,  i0002 = 2,  i0003 = 3, i0004 = 4,  i0008 = 8,  i000c = 12 *)
    m1 := r8000a or r4000a or r2000b or (Byte(Rules[r1000 or 27]) * $1001)
               or r0800b or (Byte(Rules[r0400 or 216]) * $0440) or r0200b or r0100d;
    m2 := r0080a or r0040b or (Byte(Rules[r0020 or 54]) * $0220) or r0010d
               or (Byte(Rules[r0008 or 432]) * $8008) or r0004d or r0002d or r0001b;
    Crunch[i+15] := smallint(((m1 and $ff00) or (m2 and $00ff)));
    Munch[i+15]  := smallint(((m1 and $00ff) or (m2 and $ff00)));

  end; {for tempi}
  rattleAllCages;
  //WriteLock:= false;
end;

procedure TUniverse.SetPatternName(Value: string);
begin
  FPatternName:= Proper(Value);
end;

procedure TUniverse.SetNeighborhood(Value: integer);
begin
  if ((Value >= nbMin) and (Value <= nbMax)) then begin
    FRules.Neighborhood:= value;

    ImplementRules; //recalc rule lookup tables.
    if Assigned(FOnRuleChange) then FOnRuleChange(Self);
  end;
end;

procedure TUniverse.SetRules(ARuleString: string);
var
  OldRules: string;
begin
  OldRules:= FRules.Rulestring;
  FRules.RuleString:= ARuleString;
  Rules:= FRules.RuleArray;
  ImplementRules;
  if (OldRules <> FRules.RuleString) and (Assigned(FOnRuleChange)) then FOnRuleChange(Self);
end;

function TUniverse.GetRules: string;
begin
  Result:= FRules.RuleString;
end;


procedure TUniverse.RemoveFromDisplay(c: TLifeCel);
begin
  if((c.flags and 1) <> 0) then begin //not yet removed from displaylist.

    c.DisplayPrev.DisplayNext:= c.DisplayNext;
    c.DisplayNext.DisplayPrev:= c.DisplayPrev;
    c.DisplayNext:= c;
    c.DisplayPrev:= c;

    c.flags:= c.flags and $fffe;
  end; {if}
end;

procedure TUniverse.ClearDisplay;
var
  c: TLifeCel;
  cn: TLifeCel;
begin
  c:= Display.DisplayNext;
  while c <> Display do begin
    cn:= c.DisplayNext;
    RemoveFromDisplay(c);
    c:= cn;
  end; {while}
end;

procedure TUniverse.AddToDisplay(c: TLifeCel);
begin
  if((c.flags and 1) = 0) then begin  //not inserted yet.
    c.DisplayPrev:= Display;
    C.DisplayNext:= Display.DisplayNext;
    Display.DisplayNext:= c;
    c.DisplayNext.DisplayPrev:= c;

    c.flags:= c.flags or 1;
  end; {if}
end;


procedure TUniverse.FreshenView;
var
  c: TLifeCel;
begin
  c:= living.next;
  while (c <> living) do begin
    AddToDisplay(c);
    c:= c.Next;
  end;
  c:= hibernating.next;
  while (c <> hibernating) do begin
    AddToDisplay(c);
    c:= c.Next;
  end;
end;

//Clear the universe
procedure TUniverse.clear(ClearDesc: boolean);
var
  Cel: TLifeCel;
  KillMe: TLifeCel;
begin
  PatternName:= '';
  WriteLock:= true;
  //cleanup the whole lot.

  //first the morgue.
  Cel:= Morgue.next;
  while Cel <> morgue do begin
    KillMe:= Cel;
    Cel:= Cel.Next;
    KillMe.FreeSlow; //FreeFast allowed, but never mind here.
  end;
  Morgue.Next:= Morgue;
  Morgue.Prev:= Morgue;

  //then the zombies
  Cel:= hibernating.next;
  while Cel <> hibernating do begin
    KillMe:= Cel;
    Cel:= Cel.Next;
    KillMe.FreeSlow;
  end;
  Hibernating.next:= hibernating;
  Hibernating.prev:= hibernating;

  //then the living (it's getting better all the time gna gna)
  Cel:= living.next;
  while Cel <> living do begin
    KillMe:= Cel;
    Cel:= Cel.Next;
    KillMe.FreeSlow;
  end;
  living.next:= living;
  living.prev:= living;

  //now the hashtable.
  FHashTable.Free;
  Counter:= 0;
  FHashTable:= TLifeHash.Create;
  Qcycle:=false;

  //last the description
  if ClearDesc then FDescription.Clear;
  WriteLock:= false;
end;

procedure TUniverse.RattleAllCages;
var
  c: TLifeCel;
const
  Kill_mAll = false;
begin
  c:= living.next;
  while c <> living do begin
    c.pstate:= 0;
    c.qstate:= 0;
    c:= c.Next;
  end; {while}

  //RattleCage removes these cells from the morgue cq hiber list.
  IncinerateCages(Kill_mAll);
  while(Morgue.next <> morgue) do RattleCage(morgue.next);
  while(hibernating.next <> hibernating) do RattleCage(hibernating.next);
end;

function TUniverse.GetChecksum: integer;
var
  c: TLifeCel;
  a1,a3: integer;
const
  Mask0 = $00010000;  //ok
  Mask1 = $03020303;  //ok
  Mask2 = $00540000;  //ok
  Mask3 = $fca8fcfc;  //ok
begin
  Result:= 0;
  c:= living.next;
  while c <> living do begin
    Result:= Result xor c.GetChecksum(QCycle);
    c:= c.next;
  end; {while}
  c:= hibernating.next;
  while c <> hibernating do begin
    Result:= Result xor c.GetChecksum(QCycle);
    c:= c.next;
  end; {while}
  //if QCycle, convert the Qcyclish checksum to it's P-cycle version.
  if QCycle then begin  //code copied from the 'ConvertQtoP function in LifeCel.
    a3:= Result and Mask3;
    a3:= a3 or (Result and Mask2);
    a1:= Result and Mask1;
    a1:= a1 or (Result and Mask0);
    a1:= ((a1 and $02020202) shl 5) or //-E-- ---- -6-- ----|-e-- ---- -6-- ----
         ((a1 and $01010100) shr 1) or //---- ---- 7--- ----|f--- ---- 7--- ----
         ((a1 and $00000001) shl 31);  //F--- ---- ---- ----|---- ---- ---- ----
    a3:= ((a3 and $a8a8a8a8) shr 3) or //---c -a-8 ---4 -2-0|---c -a-8 ---4 -2-0
         ((a3 and $54545400) shr 9) or //---- ---- --5- 3-1-|--d- b-9- --5- 3-1-
         ((a3 and $00000054) shl 23);  //--D- B-9- ---- ----|---- ---- ---- ----
    Result:= (a1 or a3);
  end; {if}
end;

procedure TUniverse.DoChecksum;
var
  AChecksum: TChecksum;
begin
  AChecksum:= TChecksum.Create(GetChecksum, Counter);
  ChecksumList.Add(AChecksum);
end;

procedure TUniverse.stepBack;
begin
  if(BackCorrect) then begin
    Qcycle:= not(Qcycle);

    RattleAllCages;
  end; {if}
  BackCorrect:= false;
end;

function TUniverse.CanPlay: boolean;
begin
  Result:= living.Next <> living;
  if not Result then Result:= not(living.IsStable)
end;

function TUniverse.IsEmpty: boolean;
var
  CelCount: Cardinal;
  c: TLifeCel;
begin
  CelCount:= 0;
  try
    c:= living.next;
    while (CelCount = 0) and (c <> living) do begin
      Inc(CelCount,c.NumberOfCels(QCycle));
      c:= c.next;
    end; {while}

    c:= hibernating.next;
    while (CelCount = 0) and (c <> hibernating) do begin
      Inc(CelCount,c.NumberOfCels(QCycle));
      c:= c.next;
    end;
  except CelCount:= Cardinal(-1);
  end; {try}
  Result:= CelCount = 0;
end;

(*function TUniverse.GetCelCount(OnReady: TNotifyEvent): integer;
var
  c: TLifeCel;
  CelCount: integer;
begin
  if Assigned(OnReady) then begin
    CelCounter.Free;
    CelCounter:= TCelCountThread.Create(Self,OnReady);
    CelCount:= -1;
  end
  else begin //Just see if universe is empty or not.
    CelCount:= 0;
    try
      c:= living.next;
      while (CelCount = 0) and (c <> living) do begin
        Inc(CelCount,c.NumberOfCels(QCycle));
        c:= c.next;
      end; {while}

      c:= hibernating.next;
      while (CelCount = 0) and (c <> hibernating) do begin
        Inc(CelCount,c.NumberOfCels(QCycle));
        c:= c.next;
      end;
    except CelCount:= 0;
    end; {try}
  end;
  Result:= CelCount;
end;  (**)

function TUniverse.SaveProgress(Progress: Integer): Boolean;
var
  Cancel: Boolean;
begin
  Cancel:= false;
  if Assigned(FOnSaveProgress) then OnSaveProgress(Self,Progress,Cancel);
  Result:= Cancel;
end;

procedure TUniverse.LimitTorus(TorusKind: TTorusKind; DeadEdges: boolean);
var
  x,y: integer;
  DeadRect: TRect;
  WorkingRect: TRect;
begin
  case TorusKind of
    tk_All: WorkingRect:= Limit;
    tk_UpDown: begin
      WorkingRect:= GetBoundingBox;
      WorkingRect.Top:= Limit.Top;
      WorkingRect.Bottom:= Limit.Bottom;
      Limit:= WorkingRect;
    end; {tk_UpDown}
    tk_LeftRight: begin
      WorkingRect:= GetBoundingBox;
      WorkingRect.Left:= Limit.Left;
      WorkingRect.Right:= Limit.Right;
      Limit:= WorkingRect;
    end; {tk_LeftRight}
  end; {case}
  Dec(WorkingRect.Top); Dec(WorkingRect.Left);
  Inc(WorkingRect.Bottom); Inc(WorkingRect.Right);

  if DeadEdges then DeadRect:= WorkingRect
  else with WorkingRect do begin
    DeadRect.Left := Left-1;
    DeadRect.Top := Top-1;
    DeadRect.Right := Right+1;
    DeadRect.Bottom := Bottom+1;
    with WorkingRect do begin
      if (TorusKind and tk_UpDown) <> 0 then for x:= Left to Right do begin
        ChangeCelFast(x,Top,CelState(x,Bottom-1));
        ChangeCelFast(x,Bottom,CelState(x,Top+1));
      end; {if for}
      if (TorusKind and tk_LeftRight) <> 0 then for y:= Top to Bottom do begin
        ChangeCelFast(Left,y,CelState(Right-1,y));
        ChangeCelFast(Right,y,CelState(Left+1,y));
      end; {if}
    end; {if with}
  end; {else with}

  //Now lay a zero border around it, so nothing gets outside it.
  //Without this an object passing through the border would go out
  //both ways on the other end.
  with DeadRect do begin
    if (TorusKind and tk_UpDown) <> 0 then begin
      DrawLine(Left,Top,Right,Top,lds_off);
      DrawLine(Left,Bottom,Right,Bottom,lds_off);
    end; {if}
    if (TorusKind and tk_LeftRight) <> 0 then begin
      DrawLine(Left,Top,Left,Bottom,lds_off);
      DrawLine(Right,Top,Right,Bottom,lds_off);
    end; {if}
  end; {with}
end;

function TUniverse.CountCelsNow: cardinal;
var
  c: TLifeCel;
  state: boolean;
begin
  //wait until the universe is done generating.
  repeat
    Sleep(10);
  until WriteLock = false;
  FIsCounting:= true;
  Result:= 0;
  state:= QCycle;
  try
    c:= living.next;
    while (c <> living) and not(Terminated) do begin
      Inc(Result,c.NumberOfCels(state));
      c:= c.next;
    end; {while}

    c:= hibernating.next;
    while (c <> hibernating) and not(Terminated) do begin
      Inc(Result,c.NumberOfCels(state));
      c:= c.next;
    end;
    except Result:= Cardinal(-1);
    if Terminated then Result:= Cardinal(-1);
  end; {try}
  FIsCounting:= false;
end;


procedure TUniverse.generate(Direction: Boolean);
begin
  FWriteLock:= true;
  if (Direction and CanPlay) then begin
    //if we have a limited universe, than do the limiting stuff now.
    if IsLimited then LimitTorus(TorusKind, DeadEdges);
    if CheckPeriod then DoChecksum;
    Inc(FCounter);
    if (Counter and $00ff) = 0 then IncinerateCages(true); //cleanup every 256 ticks.

    if (Qcycle) then begin
      generate_q();
      Qcycle:= false;              // going into p cycle
    end {if}
    else begin
      generate_p();
      Qcycle:= true;
    end; {else}
  end {if}
  else if not(Direction) then begin
    stepBack;  //not(forward)
    Dec(FCounter);
  end;
  FWriteLock:= false;
end;

initialization
end.
