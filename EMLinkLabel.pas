{***************************************************************
 *
 * Unit Name: EMLinkLabel
 * Purpose: Link Label
 * Author: Eugen Mihailescu
 * Company: EM Quicksoft Romania SRL
 * Copyright: 1998,2002 © All rights reserved.
 * Web Site: http://www.em-quicksoft.com
 * Support: support@em-quicksoft.com
 * History: 03/19/2000
 *
 * Disclaimer: The objectives of this program is only educational.
 * You MUST NOT use it without getting any material benefit on it
 * You have the right to share this code with others without removing
 * the copyright and/or disclaimer notice.
 * You are allowed to include its code in your application
 * only if it was designed for non commercial purpose,
 * otherwise we claim $10 for each copy of your
 * application which contains this code inside.
 ****************************************************************}

unit EMLinkLabel;

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
  Shellapi,
  MMSystem;



Type
  TLinkLabel = class(TLabel)
  private
    FLinkDestination: string;
    FLinkColor: TColor;
    FLinkColorMouseMove: TColor;
    FLinkSound: Boolean;
    FTabOrder: TTabOrder;
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
    property TabOrder: TTabOrder read FTabOrder write FTabOrder default -1;
  end;

{$IF declared(TLinkLabel)}
{$ELSE}

procedure Register;
{$ENDIF}

implementation

uses
  System.UITypes;
//******************* TLinkLabel.Create *************************

constructor TLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LinkDestination:= 'http://';
  Cursor:= crHandPoint;

  Font.Color:= clBlue;
  Font.Style:= Font.Style + [fsUnderline];
  FLinkColor:= clBlue;
  FLinkColorMouseMove:= clRed;
  FTabOrder:= -1;
end;




//******************* TLinkLabel.SetLinkDestination *************************

procedure TLinkLabel.SetLinkDestination(const Value: string);
var
  p: Integer;
begin
  if Value <> FLinkDestination then begin
    FLinkDestination:= AnsiLowerCase(Value);
  end;
end;
//******************* TLinkLabel.Click *************************

procedure TLinkLabel.Click;
begin
  if FLinkDestination <> '' then begin
    ShellExecute(TWinControl(Owner).Handle, 'open', PChar(LinkDestination), nil, nil, SW_SHOW);
  end;
  inherited Click;
end;
//******************* TLinkLabel.CMMouseEnter *************************

procedure TLinkLabel.CMMouseEnter(var Msg: TMessage);
begin
  Font.Color:= FLinkColorMouseMove;
  Refresh;
end;
//******************* TLinkLabel.CMMouseLeave *************************

procedure TLinkLabel.CMMouseLeave(var Msg: TMessage);
begin
  Font.Color:= FLinkColor;
  Refresh;
end;
//******************* TLinkLabel.SetLinkColor *************************

procedure TLinkLabel.SetLinkColor(Value: TColor);
begin
  if Value <> FLinkColor then begin
    FLinkColor:= Value;
    Font.Color:= FLinkColor;
    Refresh;
  end;
end;
//******************* TLinkLabel.CMFontChanged *************************

procedure TLinkLabel.CMFontChanged(var Msg: TMessage);
begin
  if csDesigning In ComponentState then begin
    FLinkColor:= Font.Color;
    Refresh;
  end;
end;

{$IF declared(TLinkLabel)}
{$ELSE}

procedure Register;
begin
  RegisterComponents('EM-Quicksoft', [TLinkLabel]);
end;
{$ENDIF}





procedure TLinkLabel.ChangeColors;
begin
  if FAutoColors then
    if FVisited then Font.Color:=clVisited else Font.Color:=clUnvisited
  else
    if FVisited then Font.Color:=FColorVisited else Font.Color:=FColorUnvisited
end;

procedure TLinkLabel.SetAutoColors(AutoCol : Boolean);
begin
  if AutoCol<>FAutoColors then begin
    FAutoColors:=AutoCol;
    ChangeColors;
  end;
end;

procedure TLinkLabel.SetColorUnvisited(Col : TColor);
begin
  if FColorUnvisited<>Col then begin
    FColorUnvisited:=Col;
    if not FAutoColors then ChangeColors;
  end;
end;

procedure TLinkLabel.SetColorVisited(Col : TColor);
begin
  if FColorVisited<>Col then begin
    FColorVisited:=Col;
    if not FAutoColors then ChangeColors;
  end;
end;

procedure TLinkLabel.SetVisited(Vis : Boolean);
begin
  if FVisited<>Vis then begin
    FVisited:=Vis;
    ChangeColors;
  end;
end;

procedure TLinkLabel.WMClick(var Msg : TMessage);
begin
  inherited;
  ShellExecute(0, 'open', PChar(FHREF), '', '', SW_SHOW);
end;

procedure Register;
begin
  RegisterComponents('Additional', [TLinkLabel]);
end;

end.
