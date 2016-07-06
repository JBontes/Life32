unit ScreenSaveInit;

{this unit does the necessary init-stuff.
 1) Checking parameters
 2) Filling some Variables with values
 2.1) ParamHandle: The value of the handle passed on command line
 2.2) ConfigParent: The handle of the screen-properties-window when the config
      dialog should display modal to it
 3) sets IsLibrary = True to ensure the Application-Window does not show up

 Version 1.0
 Author: Meik Weber

 IMPORTANT: See Copyright and disclaimer in README.TXT and Project Source}

interface

uses Windows; {do not use any VCL here}

{these are some globally needed values}
const
  savModeRunNormal    = 1; {right-click in explorer on configure}
  savModeConfigure    = 2; {config-dialog from screen-properties dialog}
  savModeExecute      = 3; {run full screen}
  savModePreview      = 4; {run in preview window}
  savModeSetPwd       = 5; {show change password dialog}

  MouseTicksToIgnore = 6;

var
  SaverMode   : Integer;  {one of the consts above}
  ParamHandle : THandle;  {the Handle passed on command line as parameter}
  ConfigParent: hWnd;     {the parent window for config-dialog}

implementation

uses SysUtils; {this is not VCL, so it's safe to use}

procedure SetParamHandle;
begin
  try
    ParamHandle := StrToInt (ParamStr (2));
  except
    ParamHandle := 0;
  end;
end;

function GetSaverMode: boolean;
var
  Param1 : string;
  T : TextFile;
begin
  ConfigParent := 0; {no modal dialog if not called via /c}
  if ParamCount = 0 then
    SaverMode := savModeRunNormal
  else begin
    Param1 := UpperCase (ParamStr (1));
    if not(Param1[1] in ['-','/']) then SaverMode:= savModeRunNormal
    else begin
      Delete (Param1, 1, 1); {remove the first character, usually a "/" or a "-"}
      case Param1[1] of
        'C': begin
          SaverMode := savModeConfigure;
          ConfigParent := GetForegroundWindow;
        end;
        'A': if (ParamCount > 1) then begin {there must be a window handle}
          SaverMode := savModeSetPwd;
          SetParamHandle;
        end;
        'P': if (ParamCount > 1) then begin {there must be a window handle}
          SaverMode := savModePreview;
          SetParamHandle;
        end;
        else begin
          SaverMode := savModeExecute;
          ConfigParent := GetForegroundWindow;
          {I need to give back the focus the currently active window after terminating the
           full screen saver}
        end; {else}
      end; {case}
    end; {else}
  end; {else}
  Result:= SaverMode <> SavModeRunNormal;
end;

initialization
  {get cmdline parameters and fill variables, VCL is not yet available}
  {if we want to use it as a screen saver we need to disable the windows.
   this only works if this unit is to initialize before the VCL, so put it
   as the first unit in your project source and make sure not to use another
   VCL unit here}
  IsLibrary := GetSaverMode;
end.
