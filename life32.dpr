program life32;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

uses
  Windows,
  Messages,
  Dialogs,
  SysUtils,
  Forms,
  ActiveX,
  Unit1 in 'Unit1.pas' {Form1},
  LifeLoad in 'LifeLoad.pas',
  Prefs in 'Prefs.pas' {FormPrefs},
  ProgCorn in 'ProgCorn.pas' {ProgCorner},
  DDSurface in 'DDSurface.pas',
  LifeCel in 'LifeCel.pas',
  LifeUtil in 'LifeUtil.pas',
  LifeConst in 'LifeConst.pas',
  Lifegen in 'Lifegen.pas',
  LifeBox in 'LifeBox.pas',
  ScrollHint in 'ScrollHint.pas',
  Snapshot in 'Snapshot.pas',
  LifeHash in 'LifeHash.pas',
  RuleConst in 'RuleConst.pas',
  BenchMark in 'BenchMark.pas',
  Life32_TLB in 'Life32_TLB.pas',
  Unit2 in 'Unit2.pas' {LifeApplication: CoClass},
  Ask in 'Ask.pas' {AskForm},
  Splash in 'Splash.pas' {SplashForm},
  gifimage in 'gifimage.pas',
  LifeRules in 'LifeRules.pas',
  WinHelpViewer;

{BenchMarkForm}

{$R *.TLB}
{$R *.RES}

var
  SplashScreen: TSplashForm;

begin
  Application.Initialize;
  Application.HelpFile := ExtractFilePath(Application.ExeName)+'Life32.hlp';
  SplashScreen:= TSplashForm.Create(Application);
  try
    //SplashScreen.Show;
    //SplashScreen.update; {To paint the splash screen}
    //if the following line says TForm1 change this to
    //TLife32MainForm.
    //Application.CreateForm(TLife32MainForm, Form1);
    Application.CreateForm(TLife32MainForm, Form1);
  //SplashScreen.Close;
  finally
    SplashScreen.Free;
  end; {try}
  Application.Run;
  //sometimes the application hung here, due to OLE issues
  //exitprocess prevents that.
  ExitProcess(0);
end.
