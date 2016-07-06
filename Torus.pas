unit Torus;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, NewExplButton,
  LifeConst;

type
  TTorusForm = class(TForm)
    TorusForm: TPanel;
  private
    procedure GetDlgSettings;
    procedure SaveDlgSettings;
  public

  end;



var
  TorusForm: TTorusForm;

implementation

{$R *.DFM}

uses
  LifeUtil, unit1;

procedure TTorusForm.GetDlgSettings;
begin

end;

procedure TTorusForm.SaveDlgSettings;
begin

end;

end.
