unit LifeSaveDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls, extctrls;

type
  TLifeOpenDialog = class(TOpenDialog)
  private
    FLifeLabel: TLabel;
    FLifeMemo: TMemo;
    FLifePanel: TPanel;
    FOpenToClipboard: TButton;
    FComments: TStringList;
    FUseRules: TCheckBox;
    FCurrentRules: string;
    FOnOpenToClipboard: TNotifyEvent;
  protected
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    procedure LifeMemoMouseDown(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    function GetUseNewRules: Boolean;
    procedure OpenToClipboardClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property Comments: TStringList read FComments;
    property CurrentRules: string read FCurrentRules write FCurrentRules;
    property UseNewRules: boolean read GetUseNewRules;
  published
    property OnOpenToClipboard: TNotifyEvent
      read FOnOpenToClipboard write FOnOpenToClipboard;
  end;

  TLifeSaveDialog = class(TLifeOpenDialog)
  protected
    procedure DoShow; override;
    procedure LifeMemoKeyPress(Sender: TObject; var Key: Char);
    procedure SetIncludeTorusInfo(Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean; override;
    property IncludeTorusInfo: boolean read GetUseNewRules write SetIncludeTorusInfo;
  end;

procedure Register;

implementation

uses
  CommDlg, LifeConst, LifeLoad, LifeRules;

{$R LifeSaveDlg.Res}

procedure Register;
begin
  RegisterComponents('Johan', [TLifeOpenDialog]);
  RegisterComponents('Johan', [TLifeSaveDialog]);
end;

function ConvertRules(ARule: string): string;
var
  LifeRules: TLifeRules;
begin
  Result:= '';
  LifeRules:= TLifeRules.Create;
  try
    LifeRules.Rulestring:= ARule;
    Result:= LifeRules.Rulestring;
  finally
    LifeRules.Free;
  end; {try}
end;

const
  RuleCaption = 'Use these rules: %s';

constructor TLifeOpenDialog.Create(AOwner: TComponent);
var
  ACaption: string;
begin
  inherited Create(AOwner);
  CurrentRules:= DefaultRules;
  Filter:= LifeFilter;
  FLifePanel:= TPanel.Create(Self);
  FComments:= TStringList.Create;
  with FLifePanel do begin
    Name:= 'EditPanel';
    Caption:= '';
    SetBounds(6,160,200,300);
    BevelOuter:= bvNone;
    BorderWidth:= 10;
    TabOrder:= 1;
    FUseRules:= TCheckBox.Create(Self);
    with FUseRules do begin
      Name:= 'UseRules';
      FmtStr(ACaption,RuleCaption,[DefaultRules]);
      Caption:= ACaption;
      SetBounds(10,10,66,10+ABS(Font.Height));
      Checked:= true;
      Enabled:= false;
      Align:= alTop;
      Parent:= FLifePanel;
      Visible:= not(Self is TLifeSaveDialog);
    end;
    FOpenToClipboard:= TButton.Create(Self);
    with FOpenToClipboard do begin
      Name:= 'OpenToClipboard';
      Caption:= 'Open to clipboard';
      SetBounds(300,13,120,32);
      Parent:= FLifePanel;
      OnClick:= OpenToClipboardClick;
      Visible:= not(Self is TLifeSaveDialog);
    end;

    FLifeLabel:= TLabel.Create(Self);
    with FLifeLabel do begin
      Name:= 'LifeLabel';
      Caption:= ' Description of the pattern';
      SetBounds(10,26,157,10+ABS(Font.Height));
      Align:= alTop;
      AutoSize:= false;
      Parent:= FLifePanel;
    end; {with}
    FLifeMemo:= TMemo.Create(Self);
    if not (Self is TLifeSaveDialog) then begin
      FLifeMemo.TabStop:= false;
      FLifeMemo.WantTabs:= false;
    end;
    with FLifeMemo do begin
      Ctl3D:= true;
      ScrollBars:= ssVertical;
      Name:= 'LifeEdit';
      SetBounds(8,49,520,90);
      ReadOnly:= true;
      Color:= clBtnFace;
      Align:= alClient;
      TabOrder:= 0;
      //ShowHint:= true;
      WordWrap:= true;
      Font.Name:= 'Arial';
      Font.Size:= 9;
      Parent:= FLifePanel;
      OnMouseDown:= LifeMemoMouseDown;
    end; {with}
  end; {with}
end;

constructor TLifeSaveDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLifeLabel.Caption:= ' Enter a description of the pattern';
  with FLifeMemo do begin
    ReadOnly:= false;
    Color:= clWindow;
    WantReturns:= true;
    OnKeyPress:= LifeMemoKeyPress;
  end; {with}
  //UseRules doubles as "save torus info Y/N".
  with FUseRules do begin
    Caption:= 'Save torus data';
    Enabled:= true;
    Checked:= false;
  end;
end; {Create}

destructor TLifeOpenDialog.Destroy;
begin
  try
    FComments.Free;
    FLifeMemo.Free;
    FLifePanel.Free;
    except {ignore}
  end; {try}
  inherited Destroy;
end;

procedure TLifeOpenDialog.OpenToClipboardClick(Sender: TObject);
begin
  if FileExists(FileName) then begin
    DoClose;
    if Assigned(FOnOpenToClipboard) then FOnOpenToClipboard(Sender);
    PostMessage(GetParent(Handle),wm_Close, 0,0);
  end;
end;

procedure TLifeOpenDialog.DoSelectionChange;
var
  FullName: string;
  ValidLifeFile: boolean;
  LifeLines: TStringList;
  DescLines: TStringList;
  Rules: string;

  function ValidFile(const FileName: string): boolean;
  begin
    Result:= GetFileAttributes(PChar(FileName)) <> $FFFFFFFF;
  end;

begin
  //this is a bit of a hack, but who cares.
  if not(Self is TLifeSaveDialog) then begin
    FullName:= FileName;
    ValidLifeFile:= FileExists(FullName) and (ValidFile(FullName));
    if ValidLifeFile then begin
      LifeLines:= TStringList.Create;
      try
        LifeLines.LoadFromFile(FullName);
        DescLines:= GetDescription(LifeLines);
        Rules:= Trim(ConvertRules(GetRules(LifeLines)));
        if Rules <> CurrentRules then begin
          FUseRules.Font.Color:= clRed;
          FUseRules.Enabled:= true;
        end
        else begin
          FUseRules.Font.Color:= clWindowText;
          FUseRules.Enabled:= false;
        end;
        FmtStr(Rules, RuleCaption, [Rules]);
        FUseRules.Caption:= Rules;

        if Assigned(DescLines) then begin
          FLifeMemo.Lines.Assign(DescLines);
          DescLines.Free;
        end
        else FLifeMemo.Lines.Clear;
        except FLifeMemo.Lines.Clear;
      end; {try}
      LifeLines.Free;
    end {if}
    else FLifeMemo.Lines.Clear;
  end; {if}
  inherited DoSelectionChange;
end;

procedure TLifeOpenDialog.DoClose;
begin
  FComments.Assign(FLifeMemo.Lines);
  inherited DoClose;
  Application.HideHint;
end;

function TLifeOpenDialog.GetUseNewRules: Boolean;
begin
  Result:= FUseRules.Checked;
end;

procedure TLifeOpenDialog.DoShow;
var
  PreviewRect: TRect;
begin
  GetClientRect(Handle, PreviewRect);
  PreviewRect.Top:= GetStaticRect.Bottom;
  Dec(PreviewRect.Top,16);
  FLifePanel.BoundsRect:= PreviewRect;
  FLifePanel.ParentWindow:= Handle;
  inherited DoShow;
  FLifeMemo.Lines.Clear;
end;

procedure TLifeSaveDialog.DoShow;
begin
  inherited DoShow;
  try
    FLifeMemo.Lines.Assign(FComments)
    except FLifeMemo.Lines.Clear;
  end; {try}
end;

procedure TLifeOpenDialog.LifeMemoMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
begin
  Application.HideHint;
end;

function TLifeOpenDialog.Execute: boolean;
begin
  if NewStyleControls and not(ofOldStyleDialog in Options) then
    Template:= 'MYTEMPLATE'
  else Template:= nil;
  //For some #$@! reason the "wm_close" message I post to the
  //dialog clears the caption of the button, so we re-set it.
  FOpenToClipboard.Caption:= '';
  FOpenToClipboard.Caption:= 'Open to clipboard';
  Result:= inherited Execute;
end;

function TLifeSaveDialog.Execute: boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template:= 'MYTEMPLATE'
  else Template:= nil;
  Result:= DoExecute(@GetSaveFileName);
end;

procedure TLifeSaveDialog.LifeMemoKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key:= #10;
end;

procedure TLifeSaveDialog.SetIncludeTorusInfo(Value: boolean);
begin
  FUseRules.Visible:= Value;
  FUseRules.Checked:= true;
end;

end.
