unit ThdTim;

interface

uses
  Windows, ExtCtrls, Messages, SysUtils, Classes;

{$WARN SYMBOL_PLATFORM OFF}
{$T+}

type
  TThreadedTimer = class;

  TTimerThread = class(TThread)
  private
    FOnFastTerminate: TNotifyEvent;
    FTimer: pointer;
    FMainForm: HWnd;
  public
    OwnerTimer: TThreadedTimer;
    constructor Create(CreateSuspended: Boolean; FastTerminateEvent: TNotifyEvent; MainForm: HWnd; Timer: pointer);
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoTimer;
    property OnFastTerminate: TNotifyEvent read FOnFastTerminate write FOnFastTerminate;
  end;

//  TTimerFields = class(TComponent)
//  private
//    FInterval: Cardinal;
//    FWindowHandle: HWND;
//    FOnTimer: TNotifyEvent;
//    FEnabled: Boolean;
//  end;

  TThreadedTimer = class(TComponent)
  private class var
    FLastTimerId: integer;
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FNewInterval: Integer;
    FTimerThread: TTimerThread;
    FThreadPriority: TThreadPriority;
    FSynchronized: boolean;
    FLowResTimer: TTimer;
    function GetOnTimer: TNotifyEvent;
  protected
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Integer);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure ThreadDone(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Wait;
    procedure Proceed;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Integer read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property Synchronized: boolean read FSynchronized write FSynchronized;
  end;

procedure Register;

implementation

uses Forms;

constructor TTimerThread.Create(CreateSuspended: Boolean;FastTerminateEvent: TNotifyEvent; MainForm: HWnd; Timer: pointer);
begin
  inherited Create(true);
  FOnFastTerminate:= FastTerminateEvent;
  FTimer:= Timer;
  FMainForm:= MainForm;
  if not(CreateSuspended) then Start;
end;

destructor TTimerThread.Destroy;
begin
  inherited Destroy;
end;

procedure TTimerThread.Execute;
begin
  while not(Terminated) do begin
    if (OwnerTimer.FInterval > 0) then begin
      Sleep(OwnerTimer.FInterval);
      if not(Terminated) then DoTimer;
    end else DoTimer;
  end;
  if Assigned(FOnFastTerminate) then FOnFastTerminate(Self);
end;

procedure TTimerThread.DoTimer;
begin
  SendMessage(FMainForm, WM_Timer, NativeInt(FTimer), 0);
end;

constructor TThreadedTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Inc(FLastTimerId);
  FEnabled := false;
  FInterval := 1000;
  FThreadPriority := tpNormal;
  FTimerThread := nil;
  FLowResTimer:= TTimer.Create(AOwner);
  FLowResTimer.Enabled:= false;
end;

destructor TThreadedTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  FTimerThread.Free;
  FLowResTimer.Free;
  inherited Destroy;
end;

function TThreadedTimer.GetOnTimer: TNotifyEvent;
begin
  Result:= FLowResTimer.OnTimer;
end;


procedure TThreadedTimer.Proceed;
begin
  Enabled:= true;
end;

procedure TThreadedTimer.UpdateTimer;
begin
  if (FNewInterval > 100) then begin
    if Assigned(FTimerThread) then FTimerThread.Terminate;
    FLowResTimer.Interval:= FNewInterval;
    FLowResTimer.Enabled:= Self.Enabled;
    FInterval:= FNewInterval;
  end else begin
    FLowResTimer.Enabled:= false;
    FInterval:= FNewInterval;
    if FTimerThread = nil then begin
      Assert(Owner is TForm);
      FTimerThread:= TTimerThread.Create(true, ThreadDone, TForm(Owner).Handle, Self);
    end;
    FTimerThread.OwnerTimer:= self;
    FTimerThread.Priority:= FThreadPriority;
    if (FInterval >= 0) and FEnabled and Assigned(FLowResTimer.OnTimer) then begin
      if FTimerThread.Suspended then FTimerThread.Start;
    end;
  end;
end;

procedure TThreadedTimer.Wait;
begin
  Enabled:= false;
end;

procedure TThreadedTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TThreadedTimer.SetInterval(Value: Integer);
begin
  if Value <> FInterval then begin
    FNewInterval := Value;
    UpdateTimer;
  end;
end;

procedure TThreadedTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FLowResTimer.OnTimer:= Value;
  UpdateTimer;
end;

procedure TThreadedTimer.SetThreadPriority(Value: TThreadPriority);
begin
  if Value <> FThreadPriority then begin
    FThreadPriority := Value;
    UpdateTimer;
  end;
end;



procedure TThreadedTimer.ThreadDone(Sender: TObject);
begin
  if (Sender = FTimerThread) then FTimerThread:= nil;
end;

procedure Register;
begin
  RegisterComponents('Johan', [TThreadedTimer]);
end;

end.
