unit Snapshot;

interface

uses
  classes, SysUtils, Forms, ComCtrls, Dialogs, Controls,
  LifeConst;

type
  TSnapshot = class(TMemoryStream)
  protected
    function GetGeneration: integer;
    procedure SetGeneration(value: integer);
    function GetPatternID: integer;
    procedure SetPatternID(value: integer);
    function GetRevision: integer;
    procedure SetRevision(value: integer);
    function GetTimeStamp: TDateTime;
    procedure SetTimeStamp(value: TDateTime);
    function GetName: string;
  public
    property Generation: integer read GetGeneration write SetGeneration;
    property PatternID: integer read GetPatternID write SetPatternID;
    property Revision: integer read GetRevision write SetRevision;
    property TimeStamp: TDateTime read GetTimeStamp write SetTimeStamp;
    property Name: string read GetName;
  end;

  TSnapshotList = class(TListView)
  private
    UniverseList: TList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddSnapShot(ASnapshot: TSnapshot; Reason: integer);
    procedure RemoveSnapShot(Index: integer);
    function SnapshotExists(ASnapShot: TSnapshot; Reason: integer): boolean;
    function Compare(Item1,Item2: TListItem; Data: Integer): integer;
    function GetSnapShot(Index: integer): TSnapShot;
    procedure Init;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Johan',[TSnapShotList]);
end;

const
  ColCaptions: array[1..5] of string =
    ('C','  Gen  ',' Pat ','Rev','Name                                            ');
  NameCol = 5;


constructor TSnapShotList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UniverseList:= TList.Create;
  UniverseList.Add(Items);
  Align:= alClient;
  BorderStyle:= bsSingle;
  ColumnClick:= true;
  MultiSelect:= true;
  Visible:= false;
end;

procedure TSnapShotList.Init;
var
  AColumn: TListColumn;
  i: integer;
begin
  try
    if not (csDesigning in ComponentState) then begin
      Columns.Clear;
      i:= (High(ColCaptions) - Low(ColCaptions)) + 1;
      while i > Columns.count do Columns.Add;
      for i:= Low(ColCaptions) to High(ColCaptions) do begin
        AColumn:= Columns[i-1];
        with AColumn do begin
          if i <> NameCol then Alignment:= taRightJustify
          else Alignment:= taLeftJustify;
          Caption:= ColCaptions[i];
          Width:= ColumnHeaderWidth;
        end; {with}
      end; {for i}   (**)
    end;
    HotTrack:= true;
    ReadOnly:= true;
    RowSelect:= true;
    ShowColumnHeaders:= true;
    ViewStyle:= vsReport; (**)
    except ShowMessage('Error in TSnapShotList.Init, contact Johan Bontes'+#10+#13+
                       'at JBontes@Compuserve.com');
  end; {try}
end;

procedure TSnapshotList.AddSnapshot(ASnapshot: TSnapshot; Reason: integer);
var
  AListItem: TListItem;
begin
  AListItem:= Items.Add;
  with AListItem do begin
    Caption:= '';
    SubItems.Add(IntToStr(ASnapShot.Generation));
    SubItems.Add(IntToStr(ASnapShot.PatternID));
    SubItems.Add(IntToStr(ASnapShot.Revision));
    SubItems.Add(ASnapShot.Name);
    //SubItems.Add(TimeToStr(ASnapshot.TimeStamp));
    ImageIndex:= Reason;
    Data:= pointer(ASnapShot);
  end; {with}
  //Shots.Add(ASnapShot);
end;

procedure TSnapShotList.RemoveSnapshot(Index: integer);
var
  DeadSnapshot: TSnapShot;
begin
  DeadSnapShot:= GetSnapShot(Index);
  DeadSnapShot.Free;
  //Shots.Delete(Integer(Items[Index].Data));
end;


function TSnapShotList.SnapshotExists(ASnapshot: TSnapshot; Reason: integer):boolean;
var
  i: integer;
  max: integer;
begin
  Result:= false;
  i:= 0;
  if (ASnapshot.Generation <> 0) then Reason:= Reason + cpLast + 1;
  Max:= Items.Count;
  while i < Items.Count do begin
    with Items[i] do begin
      if (ImageIndex = Reason) and
        (StrToInt(SubItems[0]) = (ASnapshot.Generation)) and
        (StrToInt(SubItems[1]) = (ASnapshot.PatternID)) and
        (StrToInt(SubItems[2]) = (ASnapshot.Revision)) then begin
        i:= Max;
        Result:= true;
      end; {if}
    end; {with}
    Inc(i);
  end; {while}
end;

function TSnapshotList.Compare(Item1, Item2: TListItem; Data: integer): integer;
const
  AGrB = 1;
  AisB = 0;
  ASmB = -1;
var
  ItemA, ItemB: TSnapShot;
  ReverseOrder: boolean;
begin
  ItemA:= GetSnapshot(Item1.Index);
  ItemB:= GetSnapShot(Item2.Index);
  Result:= AisB;
  if Data < 0 then begin
    Data:= -Data;
    ReverseOrder:= true;
  end
  else ReverseOrder:= false;
  //Order is PatternID, Revision, Generation, Cause.
  case Data of
    coName: begin
      if ItemA.Name > ItemB.Name then Result:= AGrB
      else if ItemA.Name < ItemB.Name then Result:= ASmB
      else begin
        if ItemA.PatternID > ItemB.PatternID then Result:= AGrB
        else if ItemA.PatternID < ItemB.PatternID then Result:= ASmB
        else begin
          if ItemA.Revision > ItemB.Revision then Result:= AGrB
          else if ItemA.Revision < ItemB.Revision then Result:= ASmB
          else begin
            if ItemA.Generation > ItemB.Generation then Result:= AGrB
            else if ItemA.Generation < ItemB.Generation then Result:= ASmB
            else Result:= AIsB;
          end; {else}
        end; {else}
      end; {else}
    end; {coName}
    coPatternID:begin //order by PatternID etc.
      if ItemA.PatternID > ItemB.PatternID then Result:= AGrB
      else if ItemA.PatternID < ItemB.PatternID then Result:= ASmB
      else begin
        if ItemA.Revision > ItemB.Revision then Result:= AGrB
        else if ItemA.Revision < ItemB.Revision then Result:= ASmB
        else begin
          if ItemA.Generation > ItemB.Generation then Result:= AGrB
          else if ItemA.Generation < ItemB.Generation then Result:= ASmB
          else begin
            if Item1.ImageIndex > Item2.ImageIndex then Result:= AGrB
            else if Item1.ImageIndex < Item2.ImageIndex then Result:= ASmB
            else Result:= AIsB;
          end; {else}
        end; {else}
      end; {else}
    end; {coPatternID:}
    coRevision:begin //order by Revision etc.
      if ItemA.Revision > ItemB.Revision then Result:= AGrB
      else if ItemA.Revision < ItemB.Revision then Result:= ASmB
      else begin
        if ItemA.PatternID > ItemB.PatternID then Result:= AGrB
        else if ItemA.PatternID < ItemB.PatternID then Result:= ASmB
        else begin
          if ItemA.Generation > ItemB.Generation then Result:= AGrB
          else if ItemA.Generation < ItemB.Generation then Result:= ASmB
          else begin
            if Item1.ImageIndex > Item2.ImageIndex then Result:= AGrB
            else if Item1.ImageIndex < Item2.ImageIndex then Result:= ASmB
            else Result:= AIsB;
          end; {else}
        end; {else}
      end; {else}
    end; {coRevision:}
    coGeneration:begin //order by Generation etc.
      if ItemA.Generation > ItemB.Generation then Result:= AGrB
      else if ItemA.Generation < ItemB.Generation then Result:= ASmB
      else begin
        if ItemA.PatternID > ItemB.PatternID then Result:= AGrB
        else if ItemA.PatternID < ItemB.PatternID then Result:= ASmB
        else begin
          if ItemA.Revision > ItemB.Revision then Result:= AGrB
          else if ItemA.Revision < ItemB.Revision then Result:= ASmB
          else begin
            if Item1.ImageIndex > Item2.ImageIndex then Result:= AGrB
            else if Item1.ImageIndex < Item2.ImageIndex then Result:= ASmB
            else Result:= AIsB;
          end; {else}
        end; {else}
      end; {else}
    end; {coGeneration:}
    coCause:begin //order by Cause etc.
      if Item1.ImageIndex > Item2.ImageIndex then Result:= AGrB
      else if Item1.ImageIndex < Item2.ImageIndex then Result:= ASmB
      else begin
        if ItemA.PatternID > ItemB.PatternID then Result:= AGrB
        else if ItemA.PatternID < ItemB.PatternID then Result:= ASmB
        else begin
          if ItemA.Revision > ItemB.Revision then Result:= AGrB
          else if ItemA.Revision < ItemB.Revision then Result:= ASmB
          else begin
            if ItemA.Generation > ItemB.Generation then Result:= AGrB
            else if ItemA.Generation < ItemB.Generation then Result:= ASmB
            else Result:= AIsB;
          end; {else}
        end; {else}
      end; {else}
    end; {coCause:}
  end; {case}
  if ReverseOrder then Result:= -Result;
end;

function TSnapShotList.GetSnapshot(Index: integer): TSnapShot;
var
  AnItem: TListItem;
begin
  AnItem:= Items[Index];
  if Assigned(AnItem) then Result:= TSnapShot(AnItem.Data)
  else Result:= nil;
end;


const
  GenPos = 0;
  PatPos = GenPos + SizeOf(integer);
  RevPos = PatPos + SizeOf(integer);
  TimePos = RevPos + SizeOf(integer);
  NamePos = TimePos + SizeOf(TDateTime) + SizeOf(Boolean);

function TSnapshot.GetGeneration: integer;
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(GenPos,soFromBeginning);
  Read(Result,SizeOf(Result));
  Seek(OldPos,soFromBeginning);
end;

procedure TSnapShot.SetGeneration(value: integer);
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(GenPos,soFromBeginning);
  Write(value,SizeOf(value));
  Seek(OldPos,soFromBeginning);
end;

function TSnapshot.GetPatternID: integer;
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(PatPos,soFromBeginning);
  Read(Result,SizeOf(Result));
  Seek(OldPos,soFromBeginning);
end;

procedure TSnapShot.SetPatternID(value: integer);
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(PatPos,soFromBeginning);
  Write(value,SizeOf(value));
  Seek(OldPos,soFromBeginning);
end;

function TSnapshot.GetRevision: integer;
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(RevPos,soFromBeginning);
  Read(Result,SizeOf(Result));
  Seek(OldPos,soFromBeginning);
end;

procedure TSnapShot.SetRevision(value: integer);
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(RevPos,soFromBeginning);
  Write(value,SizeOf(value));
  Seek(OldPos,soFromBeginning);
end;

function TSnapshot.GetTimeStamp: TDateTime;
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(TimePos,soFromBeginning);
  Read(Result,SizeOf(Result));
  Seek(OldPos,soFromBeginning);
end;

procedure TSnapShot.SetTimeStamp(value: TDateTime);
var
  OldPos: integer;
begin
  OldPos:= Position;
  Seek(TimePos,soFromBeginning);
  Write(value,SizeOf(value));
  Seek(OldPos,soFromBeginning);
end;


function TSnapShot.GetName: string;
var
  OldPos: integer;
  ALen: integer;
begin
  OldPos:= Position;
  Seek(NamePos,soFromBeginning);
  Read(ALen,SizeOf(ALen));
  SetLength(Result,ALen);
  Read(PChar(@Result[1])^,ALen);
  Seek(OldPos,soFromBeginning);
end;

end.
