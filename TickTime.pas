unit TickTime;

interface

uses SysUtils;

type
  TTijd = record
    case integer of
      1: (Totaal: Comp);
      2: (Lo, Hi: Longint);
      3: (Lom, Dummy, Him, Dummy2: word);
  end;


  TTickTimer = class(TObject)
  protected
    function GetStartTime: TTijd;
    function GetEndTime: TTijd;
    function GetTimePast: TTijd;
    function GetStartMilliTime: integer;
    function GetEndMilliTime: integer;
    function GetMilliTimePast: integer;
  public
    procedure ClearTime;
    procedure StartNow;
    procedure StopNow;
    property StartTime: TTijd read GetStartTime;
    property EndTime: TTijd read GetEndTime;
    property TimePast: TTijd read GetTimePast;
    property StartMilliTime: integer read GetStartMilliTime;
    property EndMilliTime: integer read GetEndMilliTime;
    property MilliTimePast: integer read GetMilliTimePast;
  end;

function CompToStr(const A: Comp): string;
function IsIntelInstalled: boolean;

implementation

uses
  Windows;

var
  Start, Eind: TTijd;
  TicksPerMilliSec: integer;


function CompToStr(const A: Comp): string;
begin
  //Str(A:20:0,Result);
  //Result:= Trim(Result);
  Result:= IntToStr(Int64((@A)^));
end;

function IsIntelInstalled: boolean;
begin
  {$ifdef win32}
  result:= true;
  try
    asm
      db 00FH
      db 031H
    end; {asm}
    except result:= false
  end; {try}
  {$else}
  result:= false;
  {$endif}
end;

function GetTicksPerMilli: integer;
var
  TickTimer: TTickTimer;
  MilliStart: integer;
  MilliEnd: integer;
  TicksPast: TTijd;
begin
  TickTimer:= TTickTimer.Create;
  MilliStart:= GetTickCount;
  TickTimer.StartNow;
  repeat
    MilliEnd:= GetTickCount
  until MilliEnd - MilliStart > 20; //time 20 milliseconds or more.
  TicksPast:= TickTimer.TimePast;
  MilliEnd:= MilliEnd - MilliStart;
  Result:= Round(TicksPast.Totaal / MilliEnd);
end;


procedure TTickTimer.StartNow;
  asm
    DB 00FH
    DB 031H
    {$ifdef win32}
    MOV Start.Lo,EAX
    MOV Start.Hi,EDX
    {$else}
    DB 066H; MOV Start.LOM,AX
    DB 066H; MOV Start.Him,DX
    {$endif}
  end; {asm}

procedure TTickTimer.StopNow;
  asm
    DB 00FH
    DB 031H
    {$ifdef win32}
    MOV Eind.Lo,EAX
    MOV Eind.Hi,EDX
    {$else}
    DB 066H; MOV Eind.LOM,AX
    DB 066H; MOV Eind.Him,DX
    {$endif}
  end; {asm}

function TTickTimer.GetStartTime: TTijd;
begin
  StartNow;
  Result:= Start;
end;

function TTickTimer.GetEndTime: TTijd;
begin
  StopNow;
  Result:= Eind;
end;

function TTickTimer.GetTimePast: TTijd;
begin
  StopNow;
  Result.Totaal:= Eind.Totaal - Start.Totaal;
end;

function TTickTimer.GetStartMilliTime: integer;
begin
  Result:= Round(StartTime.Totaal / TicksPerMilliSec);
end;

function TTickTimer.GetEndMilliTime: integer;
begin
  Result:= Round(Eind.Totaal / TicksPerMilliSec);
end;

function TTickTimer.GetMilliTimePast: integer;
begin
  Result:= Round(TimePast.Totaal / TicksPerMilliSec);
end;


procedure ClearIt;
begin
  Start.Hi:= 0;
  Start.Lo:= 0;
  Eind:= Start;
end;

procedure TTickTimer.ClearTime;
begin
  ClearIt;
end;

initialization
  ClearIt;
  if IsIntelInstalled then TicksPerMilliSec:= GetTicksPerMilli;
end.
