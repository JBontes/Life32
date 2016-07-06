unit Ask;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAskForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
  private
    { Private declarations }
  protected
    procedure SetQuestion(value: string);
    function GetAnswer: string;
  public
    property Question: string write SetQuestion;
    property Answer: string read GetAnswer;
  end;

var
  AskForm: TAskForm;

implementation

{$R *.DFM}

procedure TAskForm.SetQuestion(value: string);
begin
  Label1.Caption:= value;
end;

function TAskForm.GetAnswer: string;
begin
  Result:= Edit1.Text;
end;

end.
