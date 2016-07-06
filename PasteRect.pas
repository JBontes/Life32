unit PasteRect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TPasteRect = class(TCustomControl)
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPaint;
    procedure Paint; override;
  public
    { Public declarations }
  published
    property DragMode;
    property DragCursor;
    property Cursor;
    property ShowHint;
    property ParentShowHint;
    property OnEndDrag;
    property OnStartDrag;
    property OnDragDrop;
    property OnDragOver;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Johan', [TPasteRect]);
end;


procedure TPasteRect.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    ExStyle:= ExStyle or WS_EX_TRANSPARENT or WS_EX_TOPMOST;
    Style:= Style or WS_THICKFRAME or WS_CHILD or WS_VISIBLE;
    WindowClass.Style:= WindowClass.Style or CS_HREDRAW or CS_VREDRAW;
  end; {with}
  Brush.Color:= clInfoBk;
end;

procedure TPasteRect.WMNCPaint(var Msg: TWMNCPaint);
var
  MyDC: HDC;
  MyPen: HPen;
  OldPen: HPen;
  //Dummy: TPoint;
begin
  (*MyDC:= GetWindowDC(Handle);
  try
    MyPen:= CreatePen(PS_SOLID,2,clBlack);
    try
      OldPen:= SelectObject(MyDC,MyPen);
	    MoveToEx(MyDC,1,1,nil);
	    LineTo(MyDC,1,Height-1); LineTo(MyDC,Width-1,Height-1);
	    LineTo(MyDC,Width-1,1); LineTo(MyDC,1,1);
	    finally begin
	      SelectObject(MyDC,OldPen);
	      DeleteObject(MyPen);
	    end; {finally}
	  end; {try}
	  finally ReleaseDC(Handle,MyDC);
  end; *){try}
end;

procedure TPasteRect.Paint;
var
  MyDC: HDC;
  MyPen: HPen;
  OldPen: HPen;
begin
  MyDC:= GetWindowDC(Handle);
  try
    MyPen:= CreatePen(PS_SOLID,2,clBlack);
    try
      OldPen:= SelectObject(MyDC,MyPen);
	    MoveToEx(MyDC,1,1,nil);
	    LineTo(MyDC,1,Height-1); LineTo(MyDC,Width-1,Height-1);
	    LineTo(MyDC,Width-1,1); LineTo(MyDC,1,1);
	    finally begin
	      SelectObject(MyDC,OldPen);
	      DeleteObject(MyPen);
	    end; {finally}
	  end; {try}
	  finally ReleaseDC(Handle,MyDC);
  end; {try}
  //ValidateRect(Handle,nil);
end;

end.
