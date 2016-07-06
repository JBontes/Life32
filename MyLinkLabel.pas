

unit MyLinkLabel;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Shellapi;



Type
  TMyLinkLabel = class(TStaticText)
  private
    FLinkDestination: string;
    FLinkColor: TColor;
    FLinkColorMouseMove: TColor;
  protected
    procedure SetLinkDestination(const Value: string);
    procedure Click; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure SetLinkColor(Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LinkDestination: string read FLinkDestination write SetLinkDestination;
    property LinkColor: TColor read FLinkColor write SetLinkColor;
    property LinkColorMouseMove: TColor read FLinkColorMouseMove write FLinkColorMouseMove;
  end;

{.$IF declared(TLinkLabel)}
{.$ELSE}
procedure Register;
{.$ENDIF}

implementation

uses
  System.UITypes;
//******************* TLinkLabel.Create *************************

constructor TMyLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LinkDestination:= 'http://';
  Cursor:= crHandPoint;

  Font.Color:= clBlue;
  Font.Style:= Font.Style + [fsUnderline];
  FLinkColor:= clBlue;
  FLinkColorMouseMove:= clRed;
end;

//******************* TLinkLabel.SetLinkDestination *************************

procedure TMyLinkLabel.SetLinkDestination(const Value: string);
begin
  if Value <> FLinkDestination then begin
    FLinkDestination:= AnsiLowerCase(Value);
  end;
end;

//******************* TLinkLabel.Click *************************

procedure TMyLinkLabel.Click;
begin
  if FLinkDestination <> '' then begin
    ShellExecute(TWinControl(Owner).Handle, 'open', PChar(LinkDestination), nil, nil, SW_SHOW);
  end;
  inherited Click;
end;
//******************* TLinkLabel.CMMouseEnter *************************

procedure TMyLinkLabel.CMMouseEnter(var Msg: TMessage);
begin
  Font.Color:= FLinkColorMouseMove;
  Refresh;
end;
//******************* TLinkLabel.CMMouseLeave *************************

procedure TMyLinkLabel.CMMouseLeave(var Msg: TMessage);
begin
  Font.Color:= FLinkColor;
  Refresh;
end;
//******************* TLinkLabel.SetLinkColor *************************

procedure TMyLinkLabel.SetLinkColor(Value: TColor);
begin
  if Value <> FLinkColor then begin
    FLinkColor:= Value;
    Font.Color:= FLinkColor;
    Refresh;
  end;
end;
//******************* TLinkLabel.CMFontChanged *************************

procedure TMyLinkLabel.CMFontChanged(var Msg: TMessage);
begin
  if csDesigning In ComponentState then begin
    FLinkColor:= Font.Color;
    Refresh;
  end;
end;

{.$IF declared(TLinkLabel)}
{.$ELSE}
procedure Register;
begin
  RegisterComponents('Additional', [TMyLinkLabel]);
end;
{.$ENDIF}


end.
