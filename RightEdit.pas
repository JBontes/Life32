unit RightEdit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Menus, DsgnIntF;

type
  TMyCustomEdit = class (TCustomEdit)
  private
    FAlignment: TAlignment;
    FOldAlignment : TAlignment;
    FTextMargin : integer;
    function CalcTextMargin : integer;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetAlignment(Value: TAlignment);
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
  public

    constructor Create(AOwner: TComponent); override;
  end;

  TRightEdit = class (TMyCustomEdit)
  published
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

constructor TMyCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextMargin:= CalcTextMargin;
end;

function TMyCustomEdit.CalcTextMargin : integer;
{
This was borrowed from TDBEdit.  It calculates a pixel
offset from the edge of the control to the text
(a margin) used in the paint routine.
}
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;

begin
  DC:= GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont:= SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I:= SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I:= Metrics.tmHeight;
  Result:= I div 4;
end;

procedure TMyCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment:= Value;
    Invalidate;
  end;
end;

procedure TMyCustomEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FOldAlignment:= FAlignment;
  Alignment:= taLeftJustify;

end;

procedure TMyCustomEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  Alignment:= FOldAlignment;
end;

procedure TMyCustomEdit.WMPaint(var Message: TWMPaint);
{borrowed from TDBEdit}
{paints the text in the appropriate position}
var
  Width, Indent, Left: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  Canvas: TControlCanvas;
begin
  {let the existing code handle left justify}
  if (FAlignment = taLeftJustify) then begin
    inherited;
    Exit;
  end;

  Canvas:= TControlCanvas.Create;
  try
    Canvas.Control:= Self;
    DC:= Message.DC;
    if DC = 0 then DC:= BeginPaint(Handle, PS);

    Canvas.Handle:= DC;

    Canvas.Font:= Font;
    with Canvas do begin
      R:= ClientRect;
      if (BorderStyle = bsSingle) then begin
        Brush.Color:= clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color:= Color;
      Pen.Color:= Color;
      S:= Text;
      Width:= TextWidth(S);
      if BorderStyle = bsNone then Indent:= 0
      else Indent:= FTextMargin;
      if FAlignment = taRightJustify then Left:= R.Right - Width - Indent
      else Left:= (R.Left + R.Right - Width) div 2;

      //TextRect(R, Left, Indent, S);
      TextOut(Left,Indent,S);
      R.Right:= Left;
      FillRect(R);
    end;
  finally
    Canvas.Handle:= 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end; {try}
end;

procedure Register;
begin
  RegisterComponents('Johan', [TRightEdit]);
end;

end.

{ This code came from Lloyd's help file! }
