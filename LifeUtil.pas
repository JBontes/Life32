unit LifeUtil;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, Forms, Menus, LifeConst,
  registry, inifiles;

function OpenReg: TCustomIniFile;
function Max(a,b: integer): integer;
function Min(a,b: integer): integer;
function FastRandom(MaxVal: integer): integer;
procedure SwapInt(var a,b: integer);
function IntToOct(value:longint; digits:integer):string;
function OctToInt(value:string):longint;


function ProcentToInt(S: string): integer;

procedure WaitCursor;
procedure DefaultCursor;

function OffsetRect(var ARect: TRect; dx,dy: integer): Bool;
function UnionRect(var Dest: TRect; Rect1,Rect2: TRect): bool;
function IsMyRectEmpty(ARect: TRect): boolean;
function CorrectRect(ARect: TRect): TRect;

function Proper(Astr: string): string;
procedure StripLFCR(var AStr: string; var Column: integer);

//procedure PictureMenu(AMenu: TMenuItem; Refresh: boolean);

procedure DebugBeep;
function MyGetTickCount: integer;
function GetPrefix(ARule: string): string;
function StripPrefix(ARule: string): string;
function StrToNeighborhood(S: string): integer;
function NeighborhoodToStr(N: integer): string;
function NeighborhoodToStrLong(N: integer): string;
function CountNeighbors(Lookup, Neighborhood: integer): integer;
function JustFriendsAlive(Lookup, Neighborhood: integer): boolean;
function HexRule2mop(Lookup, Neighborhood: integer): char;
function InsertPrefix(ARule, APrefix:string): string;
function MaxNeighborsInNeighborhood(N: integer): integer;



type
  TStartDragHack = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

implementation

uses
  TickTime;

var
  BitmapList: TList = nil;
  TagList: TList = nil;
  IntelInstalled: boolean = false;
  TickTimer: TTickTimer;

constructor TStartDragHack.Create;
begin
  //Start executing right away.
  inherited Create(false);
end;

//move the mouse a little back and forth to enter into the
//Drag and drop loop.
procedure TStartDragHack.Execute;
var
  APos: TPoint;
begin
  //First wait awhile, so we don't move the mouse too soon.
  Sleep(100);
  //Now get the position of the mouse.
  GetCursorPos(APos);
  //move it a little forth
  SetCursorPos(APos.x+1,APos.y);
  Sleep(50);
  //and back.
  SetCursorPos(APos.x,APos.y);
end;

function OpenReg: TCustomInifile;
const
  DontCreate = false;
  MakeIt = true;
begin
  try
    //Result:= TIniFile.Create('Life32.ini');
    Result:= TRegistryIniFile.Create('Software\Life32');
    with Result do begin
      //Rootkey:= HKEY_LOCAL_MACHINE;
      //if not(OpenKey('SOFTWARE',DontCreate)) then
      //  raise Exception.Create('Registry may be damaged');
      //if not(OpenKey('JBontes\Life32\Settings',MakeIt)) then
      //  raise Exception.Create('Can''t save settings to registry');
    end; {with}
  except
    Result:= nil;
  end;
end;

function Max(a,b: Integer): integer; assembler;
asm
  cmp EAX,EDX
  jg  @@aMax
  mov EAX,EDX
@@aMax:
end;

function Min(a,b: Integer): integer; assembler;
asm
  cmp EAX,EDX
  jl  @@aMin
  mov EAX,EDX
@@aMin:
end;

function FastRandom(MaxVal: integer): integer;
const
  X: integer = 0;
  m= 65535;
  a= 37;
  c= 13;
begin
  Result:= (a*X+c) mod M;
  X:= Result;
  Result:= Result mod MaxVal;
end;

procedure SwapInt(var a,b: integer);
{$ifdef win32}
//EAX = a; EDX=b
asm
  push EBX
  mov ECX,[EAX]
  mov EBX,[EDX]
  mov [EDX],ECX
  mov [EAX],EBX
  pop EBX
end;
{$else}
var
  Temp: integer;
begin
  temp:= a;
  a:= b;
  b:= temp;
end;
{$endif}

//convert an Integer to an octal number
function IntToOct(value:longint; digits:integer):string;
const
  Z_Car = ord('0');
var
  i: integer;
begin
  Result:= '';
  repeat
    Result:= char(Z_Car + (Value and $07)) + Result;
    Value:= Value shr 3;
  until Value = 0;
  if Length(Result) < digits then for i:= Length(Result)+1 to Digits do
    Result:= '0' + Result;
end;

//convert an octal number to integer ?
function OctToInt(value:string):longint;
var
  i : integer;
  a : integer;
  int: integer;
begin
  Int:=0;
  for i:=1 to length(value) do begin
    a:= Ord(value[i]) - Ord('0');
    if (a > 7) or (a < 0) then EconvertError.Create('Not a valid octal value');
    Int:=Int * 8 + a;
  end;
  Result:=Int;
end;

function ProcentToInt(S: string): integer;
var
  i: integer;
begin
  i:= Pos('%',S);
  if i <> 0 then Delete(S,i,1);
  Result:= StrToInt(s);
end;


function UnionRect(var Dest: TRect; Rect1,Rect2: TRect): bool;
begin
  if IsMyRectEmpty(Rect1) then Dest:= Rect2
  else if IsMyRectEmpty(Rect2) then Dest:= Rect1
  else begin
    Dest.Left:= Min(Rect1.Left,Rect2.Left);
    Dest.Top:= Min(Rect1.Top,Rect2.Top);
    Dest.Right:= Max(Rect1.Right,Rect2.Right);
    Dest.Bottom:= Max(Rect1.Bottom,Rect2.Bottom);
  end;
  Result:= not(IsMyRectEmpty(Dest));
end;


function OffsetRect(var ARect: TRect; dx,dy: integer): Bool;
begin
  Result:= true;
  with ARect do begin
    Inc(Left,dx);
    Inc(Right,dx);
    Inc(Top,dy);
    Inc(Bottom,dy);
  end; {with}
end;

function IsMyRectEmpty(ARect: TRect): boolean;
begin
  Result:= (ARect.Left = ARect.Right) and (ARect.Top = ARect.Bottom);
end;

function CorrectRect(ARect: TRect): TRect;
begin
  with ARect do begin
    if Left > Right then SwapInt(Left,Right);
    if Top > Bottom then SwapInt(Top,Bottom);
  end;
  Result:= ARect;
end;

function Proper(Astr: string): string;
begin
  if AStr = Uppercase(AStr) then Astr:= lowercase(AStr);
  if length(Astr) >=1 then Astr[1]:= Upcase(Astr[1]);
  Result:= Astr;
end;

procedure StripLFCR(var AStr: string; var Column: integer);
var
  i: integer;
  removed: boolean;
begin
  try
    i:= 1;
    AStr:= AStr + ' '; //add a space so we can look ahead.
    while i < Length(AStr) do begin
      //replace whitespace with ' '.
      if CharInSet(Astr[i],[#1..#31,'"','''']) then Astr[i]:= ' ';
      inc(i);
    end; {while}
    i:= 1;
    while i < Length(AStr) do begin
      //now remove double spaces.
      removed:= false;
      if (AStr[i] = ' ') and (AStr[i+1] = ' ') then begin
        Delete(AStr,i,1);
        if (Column >= i) then Dec(Column);
        removed:= true;
      end;
      if not removed then Inc(i);
    end;
    //remove the trailing space.
    Delete(AStr,Length(AStr),1)
  except
    AStr:= ' ';
  end;
end;

procedure WaitCursor;
begin
  Screen.Cursor:= crHourGlass;
end;

procedure DefaultCursor;
begin
  //Screen.Cursor:= crDefault;
  Screen.Cursor:= crTransparentArrow;
end;

procedure DebugBeep;
//var
  //i: integer;
begin
  //for i:= 0 to 5 do MessageBeep(0);
end;

function MyGetTickCount: integer;
begin
  if IntelInstalled then Result:= TickTimer.MilliTimePast
  else Result:= Windows.GetTickCount;
end;

function GetPrefix(ARule: string): string;
var
  i: integer;
begin
  Result:= '';
  i:= Pos(':',ARule);
  if i <> 0 then Result:= Copy(ARule,1,i);
  Result:= uppercase(Result);
end;

function StripPrefix(ARule: string): string;
var
  MOP: integer;
  Prefix: string;
begin
  Result:= ARule;
  Prefix:= GetPrefix(ARule);
  if Prefix = 'M:' then Delete(Result,Pos(':',ARule)-1,Length(Prefix));
  if Prefix = 'H:' then begin
    mop:= Pos('2m',lowercase(result)) + Pos('2o',lowercase(result)) +
          Pos('2p',lowercase(result));
    if mop > 0 then Delete(Result,Pos(':',ARule)-1,Length(Prefix));
  end; {if}
end;

function InsertPrefix(ARule, APrefix:string): string;
var
  i: integer;
begin
  Result:= ARule;
  i:= Pos(':',ARule);
  if i <> 0 then Delete(ARule,1,i);
  i:= Pos(':',APrefix);
  if i <> 0 then Delete(APrefix,i,1);
  if APrefix = '' then APrefix:= NeighBorHoodToStr(nbDefault)
  else APrefix:= APrefix + ':';
  Result:= APrefix + ARule;
end;

function CountNeighbors(Lookup, Neighborhood: integer): integer;
begin
  Result:= 0;
  Lookup:= (Lookup and Neighborhood);
  if Bool(Lookup and bit0) then Inc(Result); //%000000001
  if Bool(Lookup and bit1) then Inc(Result); //%000000010
  if Bool(Lookup and bit2) then Inc(Result); //%000000100
  if Bool(Lookup and bit3) then Inc(Result); //%000001000
  if Bool(Lookup and bit4) then Inc(Result); //%000010000
  if Bool(Lookup and bit5) then Inc(Result); //%000100000
  if Bool(Lookup and bit6) then Inc(Result); //%001000000
  if Bool(Lookup and bit7) then Inc(Result); //%010000000
  if Bool(Lookup and bit8) then Inc(Result); //%100000000
end;

const
  JustFriendsLookup: array [0..19] of integer = ($140,$108,$101,$102,$104,$88,
                                                 $81,$82,$84,$a0,$41,$42,$44,
                                                 $60,$28,$21,$22,$0a,$0c,$05);

function JustFriendsAlive(Lookup, Neighborhood: integer): boolean;
var
  i: integer;
begin
  i:= Low(JustFriendsLookup);
  Result:= Bool(Lookup and bit4);
  while (i <= High(JustFriendsLookup)) and not Result do begin
    Result:= (Lookup = JustFriendsLookup[i]);
    Inc(i);
  end; {else for i}
end;

function HexRule2mop(Lookup, Neighborhood: integer): char;
const
  none = ' ';
  m: array [0..5] of integer = ($21,$102,$a0,$108,$81,$0a);
  o: array [0..5] of integer = ($03,$22,$120,$180,$88,$09);
  //first 3 items in p are repeated.
  p: array [0..5] of integer = ($101,$82,$28,$101,$82,$28);
var
  i: integer;
begin
  if Neighborhood <> nbHex then result:= none
  else begin
    Result:= none;
    //leave out non-relevant bits for the hex rule.
    lookup:= lookup and not bit4 and not bit2 and not bit6;
    i:= low(m);
    while (i <= high(m)) and (result = none) do begin
      if lookup = m[i] then Result:= 'm'
      else if Lookup = o[i] then Result:= 'o'
      else if Lookup = p[i] then Result:= 'p';
      inc(i);
    end; {while}
  end; {else}
end;

function MaxNeighborsInNeighborhood(N: integer): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to 8 do begin
    if Odd(N) then Inc(Result);
    N:= N shr 1;
  end; {for i}
end;


function StrToNeighborhood(S: string): integer;
var
  i: integer;
begin
  Result:= nbUnchanged; //special case: don't alter neighborhood;
  i:= Pos(':',S);
  if i <> 0 then begin
    S:= copy(S,1,i-1);
    if (S <> '') then case Upcase(S[1]) of
      'M': result:= nbMoore;
      'H': result:= nbHex;
      'V': result:= nbVonNeuman;
      'X': result:= nbX;
      '0'..'7': try
        Result:= OctToInt(S); //S must be in Oct notation
        except Result:= nbMoore;
      end; {try}
    end; {if case}
  end; {if}
end;

function NeighborhoodToStr(N: integer): string;
begin
  case N of
    nbMoore: Result:= 'M:';
    nbX: Result:= 'X:';
    nbVonNeuman: Result:= 'V:';
    nbHex: Result:= 'H:';
    nbJustFriends: Result:= 'J:';
    else Result:= IntToOct(N,0)+':';
  end; {case}
end;

function NeighborhoodToStrLong(N: integer): string;
begin
  case N of
    nbUnchanged: Result:= '';
    nbMoore: Result:= 'M:Moore';
    nbX: Result:= 'X:';
    nbVonNeuman: Result:= 'V:Von Neuman';
    nbHex: Result:= 'H:HexLife';
    else Result:= IntToOct(N,0)+':';
  end; {case}
end;




procedure PictureMenu(AMenu: TMenuItem; Refresh: boolean);
//var
  //i: integer;
begin
  //i:= 1;
end; (**)

(*
procedure PictureMenu(AMenu: TMenuItem; Refresh: boolean);
var
  ABitmap: TBitmap;
  BBitmap: TBitmap;
  i,j: integer;
  ID: integer;
  OldTag: integer;
begin
  if not Assigned(BitmapList) then BitmapList:= TList.Create;
  if not Assigned(TagList) then TagList:= TList.Create;
  ABitmap:= nil;  //kill warnings.
  BBitmap:= nil;

  i:= 0;
  while i < AMenu.Count do with AMenu.Items[i] do begin
    ID:= Tag;
    if (ID <= MaxZoom) then ID:= menuZoomCheck;
    if (ID >= 1000) then try
      BBitmap:= nil;
      if not(Refresh) then begin
        if FindResource(HInstance,PChar('#'+IntToStr(ID)),RT_BITMAP) <> 0 then begin
          ABitmap:= TBitmap.Create;
          ABitmap.LoadFromResourceID(HInstance,ID);
        end {if}
        else ABitmap:= nil;
        if ID >= 2000 then begin
          if FindResource(HInstance,PChar('#'+IntToStr(ID+1000)),RT_BITMAP) <> 0 then begin
            BBitmap:= TBitmap.Create;
            BBitmap.LoadFromResourceID(HInstance,ID+1000);
          end; {if}
        end; {if}
      end {if}
      else begin  //Refresh
        j:= 0;
        while (j < TagList.Count) do begin
          OldTag:= integer(TagList[j]);
          if OldTag = ID then begin
            ABitmap:= TBitmap(BitmapList[j]);
            if ID >= 2000 then BBitmap:= TBitmap(BitmapList[j+1]);
            j:= TagList.Count;
          end; {if}
          Inc(j);
        end; { while }
      end; {else}
      if Assigned(ABitmap) then begin
        if Visible then begin
          if not(Assigned(BBitmap)) then BBitmap:= ABitmap;
          if not(SetMenuItemBitmaps(AMenu.Handle,Command,mf_bycommand,
                                    ABitmap.Handle,BBitmap.Handle)) then
            raise Exception.Create('');
        end; {if}
        if not(Refresh) then begin
          BitmapList.Add(ABitmap);
          TagList.Add(pointer(ID));
          if ID >= 2000 then begin
            BitmapList.Add(BBitmap);
            TagList.Add(pointer(ID+1000));
          end; {if}
        end; {if}
      end; {if}
      except begin
        if BBitmap <> ABitmap then BBitmap.Free;
        ABitmap.Free;
      end;
    end; {if try}
    Inc(i);
  end; {while}
end; (**)


initialization
  IntelInstalled:= TickTime.IsIntelInstalled;
  //IntelInstalled:= false;
  if IntelInstalled then begin
    TickTimer:= TTickTimer.Create;
    TickTimer.StartNow;
  end;
finalization
  if IntelInstalled then TickTimer.Free;
end.
