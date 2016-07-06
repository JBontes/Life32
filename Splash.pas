unit Splash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TSplashForm = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.DFM}

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  SetWindowLong(Handle,GWL_STYLE, GetWindowLong(Handle,GWL_STYLE) AND NOT WS_CAPTION);
  ClientHeight:= Height;
end;


end.
