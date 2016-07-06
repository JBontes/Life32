unit ScrollHint;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Windows, Classes, Controls, Messages, Forms, Graphics,
  LifeUtil, LifeConst;

type
  TScrollHint = class(THintWindow)
  private
    FScrollPos: integer;
    FMaxLines: integer;
    FMaxPos: integer;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    HintText: TStringList;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsHintMsg(var Msg: TMsg): boolean; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
  end;

implementation

constructor TScrollHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Brush.Style:= bsSolid;
  Brush.Color:= clInfoBk;
  HintText:= TStringList.Create;
  Cursor:= crTransparentArrow;
end;

destructor TScrollHint.Destroy;
begin
  HintText.Free;
  inherited Destroy;
end;

procedure TScrollHint.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style:= Style or WS_VSCROLL;
    WindowClass.Style := WindowClass.Style and not CS_SAVEBITS;
  end;
end;

procedure TScrollHint.WMNCPaint(var Msg: TMessage);
var
  R: TRect;
  MyDC: HDC;
begin
  MyDC:= GetWindowDC(Handle);
  try
    R := Rect(0, 0, Width, Height);
    DrawEdge(MyDC, R, BDR_RAISEDOUTER, BF_RECT);
    finally ReleaseDC(Handle,MyDC);
  end; {try}
  DefaultHandler(Msg);
end;

procedure TScrollHint.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
end;

procedure TScrollHint.CMTextChanged(var Msg: TMessage);
begin
  DefaultHandler(Msg);
end;


procedure TScrollHint.WMVScroll(var Msg: TWMVScroll);
var
  NewHint: string;
  i: integer;
  Large: integer;
begin
  Large:= FMaxLines div 2;
  case Msg.ScrollCode of
    SB_LineUp: if FScrollPos > 0 then Dec(FScrollPos);
    SB_LineDown: if FScrollPos <= FMaxPos then Inc(FScrollPos);
    SB_PageUp: if FScrollPos >= Large then Dec(FScrollPos,Large) else FScrollPos:= 0;
    SB_PageDown: Inc(FScrollPos,Large);
    SB_ThumbPosition,SB_THUMBTRACK: FScrollPos:= Msg.Pos;
  end; {case}
  if (FScrollPos <> GetScrollPos(Handle,SB_VERT)) then begin
    SetScrollPos(Handle,SB_VERT,FScrollPos,true);
    NewHint:= '';
    for i:=FScrollPos to Min(FScrollPos+FMaxLines,HintText.Count-1) do begin
      NewHint:= NewHint + HintText[i]+#13#10;
    end; {for}
    Caption:= NewHint;
    InvalidateRect(Handle,nil,true);
  end {if}
  else ValidateRect(Handle,nil)
end;

function TScrollHint.IsHintMsg(var Msg: TMsg): boolean;
var
  MyRect: TRect;
begin
  Result:= inherited IsHintMsg(Msg);
  if Result then with Msg do begin
    if (message = WM_MouseMove) or (message = WM_NCMouseMove) then Result:= false;
  end; {with}
  if Result then with Msg do begin
    MyRect:= ClientRect;
    OffsetRect(MyRect,ClientOrigin.x,ClientOrigin.y);
    Result:= not(PtInRect(MyRect,Pt));
  end; {if}
end;

procedure TScrollHint.ActivateHint(Rect: TRect; const AHint: string);
var
  i: integer;
  NewHint: string;
begin
  with Rect do begin
    Right:= Right + GetSystemMetrics(SM_CYHSCROLL);
  end; {with}
  HintText.Text:= AHint;
  FMaxLines:= (Screen.Height - Rect.Top) div ABS(Canvas.Font.Height);
  FMaxPos:= Max(0,HintText.Count-FMaxLines+10);
  Rect.Bottom:= Min(Rect.Bottom, Screen.Height);

  if HintText.Count > FMaxLines then begin
    NewHint:= '';
    for i:=0 to FMaxLines-1 do begin
      NewHint:= NewHint + HintText[i]+#13#10;
    end; {for}
  end
  else NewHint:= AHint;
  inherited ActivateHint(Rect,NewHint);

  //Set up the scrollbar
  SetScrollRange(Handle,SB_VERT,0,FMaxPos,true);
end;

procedure TScrollHint.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font.Color := clInfoText;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DT_EXPANDTABS);
end;

end.
