unit SkipTo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, LifeConst, LifeUtil, Prefs, Spin, Snapshot,
  Menus;

type
  TSkipToForm = class(TForm)
    Panel1: TPanel;
    RewindPanel: TPanel;
    RewindList2: TSnapshotList;
    Panel2: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    GroupBox5: TGroupBox;
    Image9: TImage;
    Edit1: TEdit;
    SpinButton1: TSpinButton;
    cbShow: TCheckBox;
    cbReshowSkip: TCheckBox;
    procedure FormActivate(Sender: TObject);
  private
    CurrentGen: integer;
  protected
  public
    SkipToGen: integer;
    ShowGens: boolean;
    NewSnapShot: TSnapShot;
  end;

var
  SkipToForm: TSkipToForm;

implementation

{$R *.DFM}

uses Unit1;

var
  MinGen: integer;

const
  CreateTime = 1;
  ActivateTime = 2;
  CloseTime = 3;
  OKTime = 4;

procedure TSkipToForm.FormActivate(Sender: TObject);
begin
  MinGen:= Form1.MinGen;
  CurrentGen:= Form1.MyLifeBox.Generation;
  cbReshowSkip.Checked:= Form1.ReshowSkip;
  if SkipToGen < MinGen then SkipToGen:= CurrentGen;
  Edit1.text:= IntToStr(SkipToGen-CurrentGen);
  if SkipToGen-CurrentGen >= 0 then Edit1.Text:= '+'+Edit1.Text;
  Edit1.SelectAll;
  Edit1.Hint:= '@'+ IntToStr(MinGen) +'..'+IntToStr(MaxInt);
end;


end.
