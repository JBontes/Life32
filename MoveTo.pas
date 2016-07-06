unit MoveTo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, LifeConst, ExtCtrls;

type
  TMoveToForm = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    NW: TSpeedButton;
    N: TSpeedButton;
    NE: TSpeedButton;
    W: TSpeedButton;
    CenterButton: TSpeedButton;
    E: TSpeedButton;
    SW: TSpeedButton;
    S: TSpeedButton;
    SE: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    NullButton: TSpeedButton;
    EditX: TEdit;
    EditY: TEdit;
    OKMoveButton: TButton;
    CancelMoveButton: TButton;
    GroupBox1: TGroupBox;
    YLabel: TLabel;
    XLabel: TLabel;
    procedure NWClick(Sender: TObject);
    procedure EditXKeyPress(Sender: TObject; var Key: Char);
    procedure OKMoveButtonClick(Sender: TObject);
    procedure NWMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditXChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    EditedByUser: boolean;
    OldCursorPos: TPoint;

    procedure GetDlgSettings;
    procedure SaveDlgSettings;
  public
    X,Y: integer;
    BoundingRect: TRect;
  end;

var
  MoveToForm: TMoveToForm;

implementation

{$R *.DFM}

uses
  Prefs, LifeUtil, Unit1;

procedure TMoveToForm.NWClick(Sender: TObject);
begin
  SetCursorPos(OldCursorPos.x,OldCursorPos.y);
  ModalResult:= TControl(Sender).Tag;
end;



procedure TMoveToForm.EditXKeyPress(Sender: TObject; var Key: Char);
var
  OK: boolean;
begin
  if not(key in ['0'..'9',#1..#31,'-']) then key:= #0
  else EditedByUser:= true;

  OK:= true;
  try X:= StrToInt(EditX.Text) except OK:= false end;
  try Y:= StrToInt(EditY.Text) except OK:= false end;
  OkMoveButton.Enabled:= OK;
  if key = #27 then ModalResult:= mrCancel
  else if (key = #13) and OK then ModalResult:= mrOK;
end;

procedure TMoveToForm.OKMoveButtonClick(Sender: TObject);
var
  OK: boolean;
begin
  OK:= true;
  try X:= StrToInt(EditX.Text) except OK:= false end;
  try Y:= StrToInt(EditY.Text) except OK:= false end;
  if X > (1024*1024) then X:= (1024*1024)
  else if X < -((1024*1024)-1) then x:= -((1024*1024)-1);
  if Y > (1024*1024) then Y:= (1024*1024)
  else if Y < -((1024*1024)-1) then Y:= -((1024*1024)-1);

  if not(Ok) then ModalResult:= mrCancel;
end;

procedure TMoveToForm.NWMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  MiddleX, MiddleY: integer;
begin
  if not EditedByUser then begin
    with BoundingRect do begin
      MiddleX:= (Right + Left) div 2;
      MiddleY:= (Top + Bottom) div 2;
      case TControl(Sender).Tag of
        idN: begin X:= MiddleX; Y:=Top; end;
        idNW:begin X:= Left; Y:=Top; end;
        idW: begin X:= Left; Y:=MiddleY; end;
        idSW:begin X:= Left; Y:=Bottom-1; end;
        idS: begin X:= MiddleX; Y:=Bottom-1; end;
        idSE:begin X:= Right-1; Y:=Bottom-1; end;
        idE: begin X:= Right-1; Y:=MiddleY; end;
        idNE:begin X:= Right-1; Y:=Top; end;
        idCenter:begin X:= MiddleX; Y:=MiddleY; end;
        idNulCenter: begin X:= 0; Y:= 0; end;
      end; {case}
      if X > MaxX then x:= x or $FFFF0000;
      if y > MaxX then y:= y or $FFFF0000;
      if x < MinX then x:= x and $0000ffff;
      if y < MinY then y:= y and $0000ffff;
      EditX.Text:= IntToStr(x);
      EditY.Text:= IntToStr(y);
    end; {with}
  end;
end;

procedure TMoveToForm.EditXChange(Sender: TObject);
var
  okX, okY: boolean;
begin
  if TEdit(Sender).Modified and (EditX.Text = '') and (EditY.Text = '') then
    EditedByUser:= false;
  okX:= true;
  try
    X:= StrToInt(EditX.Text);
    if (X < MinX) or (X > MaxX) then okX:= false
    else EditX.Font.Color:= clWindowText;
    except okX:= false;
  end; {try}
  if not okX then EditX.Font.Color:= clRed;
  okY:= true;
  try
    Y:= StrToInt(EditY.Text);
    if (Y < MinY) or (Y > MaxY) then okY:= false
    else EditY.Font.Color:= clWindowText;
    except okY:= false;
  end; {try}
  if not okY then EditY.Font.Color:= clRed;
  OKMoveButton.Enabled:= okX and okY;
end;

procedure TMoveToForm.FormActivate(Sender: TObject);
var
  CenterPoint: TPoint;
begin
  GetCursorPos(OldCursorPos);
  with CenterButton do begin
    CenterPoint.x:= Left + (Width div 2);
    CenterPoint.y:= Top + (Height div 2);
    CenterPoint:= Self.ClientToScreen(CenterPoint);
    SetCursorPos(CenterPoint.x,CenterPoint.y);
  end; {with}
  XLabel.Caption:= Format(cXLabel,[Form1.MyLifeBox.XScroll]);
  YLabel.Caption:= Format(cYLabel,[Form1.MyLifeBox.YScroll]);
  DRaw(true);
end;

procedure TMoveToForm.FormCreate(Sender: TObject);
begin
  GetDlgSettings;
end;

procedure TMoveToForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveDlgSettings;
end;

procedure TMoveToForm.GetDlgSettings;
begin
  with OpenReg do try
    if OpenKey('MoveTo.pas',DontCreate) then try
      Left:= ReadInteger('Left');
      Top:= ReadInteger('Top');
      except begin
        Top:= Screen.Width;
        Left:= Screen.Height;
      end; {except}
    end; {if}
    finally Free;
    Left:= Min(Left,Screen.Width - Width);
    Top:= Min(Top, Screen.Height - Height);
  end; {with}
end;

procedure TMoveToForm.SaveDlgSettings;
begin
  with OpenReg do try
    OpenKey('MoveTo.pas',MakeIt);
    WriteInteger('Left',Left);
    WriteInteger('Top',Top);
    finally Free;
  end; {with}
end;

end.
