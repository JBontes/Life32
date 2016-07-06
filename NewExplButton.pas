unit NewExplButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, Menus;

type
  TNewExplButton = class(TSpeedButton)
  private
    FDropDown: TPopupMenu;
  protected
    procedure SetDropDown(value: TPopupMenu);
    procedure WMLButtonDown(var msg: TWMLButtonDown); message WM_LBUTTONDOWN;
		procedure WMLButtonUp(var msg: TWMLButtonUp); message WM_LBUTTONUP;
  public
    constructor Create(aOwner: TComponent); override;

  published
    property DropDownMenu: TPopupMenu read FDropDown write SetDropDown stored True;
  end;

procedure Register;

implementation

uses
  System.Types;

procedure Register;
begin
  RegisterComponents('Johan', [TNewExplButton]);
end;

constructor TNewExplButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


procedure TNewExplButton.SetDropDown(value: TPopupMenu);
begin
	FDropDown := value;
	Repaint;
end;

procedure TNewExplButton.WMLButtonDown(var Msg: TWMLButtonDown);
var
  theMsg: TMsg;
  p: TPoint;
  lpPoint: TPoint;
begin
  if Visible and Enabled and (FDropDown <> nil) then begin
    p := ClientToScreen(Point(0, Height));
		FDropDown.Popup(p.x, p.y);
		while PeekMessage(theMsg, HWND(0), WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
			;
		if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
		GetCursorPos(lpPoint);
		lpPoint := Parent.ScreenToClient(lpPoint);
		
		Repaint;
  end
  else inherited;
end;

procedure TNewExplButton.WMLButtonUp(var Msg: TWMLButtonUp);
begin
  if (FDropDown = nil) then inherited;
end;

end.
