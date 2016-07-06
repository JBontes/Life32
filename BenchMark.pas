unit BenchMark;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Menus;

type
  TBenchMarkForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RichEdit1: TRichEdit;
    GroupBox1: TGroupBox;
    UpDown1: TUpDown;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    cbShowDisplay: TCheckBox;
    cbCheckEsc: TCheckBox;
    cbShowProgress: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    RewindButton: TBitBtn;
    Edit2: TEdit;
    MainMenu1: TMainMenu;
    Rewind1: TMenuItem;
    Rewinddummy1: TMenuItem;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure RewindButtonClick(Sender: TObject);
    procedure Rewinddummy1Click(Sender: TObject);
  private
    procedure GetDlgSettings;
    procedure SaveDlgSettings;
  public
    BenchmarkMillisecs: integer;
    NumGens: integer;
    ShowProgress, CheckEsc, ShowDisplay: boolean;
  end;

var
  BenchMarkForm: TBenchMarkForm;

implementation

{$R *.DFM}

uses
  Prefs, LifeUtil, LifeConst, Unit1;

procedure TBenchMarkForm.Edit1Change(Sender: TObject);
begin
  with Sender as TEdit do try
    StrToInt(Text);
    Font.Color:= clWindowText;
    if (Length(Text)>0) and CharInSet(Text[1],['-']) then Abort;
    except Font.Color:= clRed;
  end; {try}
end;

procedure TBenchMarkForm.Edit1Click(Sender: TObject);
begin
  try
    with Sender as TEdit do SelectAll;
    except {ignore}
  end; {try}
end;

procedure TBenchMarkForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key,ValidNumbers) then key:= #0;
  if Key = '-' then Key:= #0;
end;

procedure TBenchMarkForm.Button1Click(Sender: TObject);
begin
  try
    NumGens:= StrToInt(Edit1.Text);
    except NumGens:= 0;
  end;
  ShowProgress:= cbShowProgress.Checked;
  CheckEsc:= cbCheckEsc.Checked;
  ShowDisplay:= cbShowDisplay.Checked;
  SaveDlgSettings;
end;

procedure TBenchmarkForm.GetDlgSettings;
begin
  with OpenReg do try
    Left:= ReadInteger('Benchmark.pas','Left',Screen.Width);
    Top:= ReadInteger('Benchmark.pas','Top',Screen.Height);
    Edit1.Text:= IntToStr(ReadInteger('Benchmark.pas','Offset',1000));
    cbShowDisplay.Checked:= ReadBool('Benchmark.pas','ShowDisplay',true);
    cbShowProgress.Checked:= ReadBool('Benchmark.pas','ShowProgress',true);
    cbCheckESC.Checked:= ReadBool('Benchmark.pas','CheckESC',true);
    finally Free;
    Left:= Min(Left,Screen.Width - Width);
    Top:= Min(Top, Screen.Height - Height);
  end; {with}
end;

procedure TBenchmarkForm.SaveDlgSettings;
begin
  with OpenReg do try
    WriteInteger('Benchmark.pas','Left',Left);
    WriteInteger('Benchmark.pas','Top',Top);
    try
      WriteInteger('Benchmark.pas','Offset',StrToInt(Edit1.Text));
      except {do nothing}
    end; {try}
    WriteBool('Benchmark.pas','ShowDisplay',cbShowDisplay.Checked);
    WriteBool('Benchmark.pas','ShowProgress',cbShowProgress.Checked);
    writeBool('Benchmark.pas','CheckESC',cbCheckESC.Checked);
    finally Free;
  end; {with}
end;

procedure TBenchMarkForm.FormActivate(Sender: TObject);
begin
  GetDlgSettings;
  Edit2.Text:= IntToStr(BenchmarkMillisecs);
  RewindButton.Enabled:= Unit1.Form1.Generation <> 0;
end;

procedure TBenchMarkForm.RewindButtonClick(Sender: TObject);
begin
  Unit1.Form1.Generation:= 0;
  RewindButton.Enabled:= false;
end;

procedure TBenchMarkForm.Rewinddummy1Click(Sender: TObject);
begin
  if RewindButton.Enabled then RewindButton.Click;
end;

end.
