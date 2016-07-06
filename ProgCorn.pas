unit ProgCorn;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, Clipbrd, LifeConst, Tabnotbk, LifeUtil,
  MyLinkLabel, RichEdit;

type
  TRichEdit = class(ComCtrls.TRichEdit)
  protected
    procedure CNNotify(var Message: TWMNotifyRE); message CN_NOTIFY;
  end;

  TProgCorner = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    TabbedNotebook1: TTabbedNotebook;
    RichEdit2: TRichEdit;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    Panel3: TPanel;
    Button1: TButton;
    RichEdit1: TRichEdit;
    RichEdit3: TRichEdit;
    RichEdit4: TRichEdit;
    LinkLabel2: TMyLinkLabel;
    LinkLabel3: TMyLinkLabel;
    LinkLabel6: TMyLinkLabel;
    LinkLabel1: TMyLinkLabel;
    LinkLabel7: TMyLinkLabel;
    LinkLabel8: TMyLinkLabel;
    LinkLabel9: TMyLinkLabel;
    LinkLabel10: TMyLinkLabel;
    Bevel1: TBevel;
    LinkLabel11: TMyLinkLabel;
    LinkLabel12: TMyLinkLabel;
    Button2: TButton;
    MyLinkLabel1: TMyLinkLabel;
    MyLinkLabel2: TMyLinkLabel;
    procedure Copy1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
  private
    RichEdits: array[1..4] of TRichEdit;
    procedure GetDlgSettings;
    procedure SaveDlgSettings;
    //procedure InitRichEditURLDetection(RE: TRichEdit);
    //function RichEditByHandle(Handle: HWnd): TRichEdit;
    procedure InitRichEditURLDetection(RE: TRichEdit);
  public
    { Public declarations }
  end;

var
  ProgCorner: TProgCorner;

implementation

{$R *.DFM}

uses
  ShellAPI{, RichEdit};

const
  AURL_ENABLEEAURLS = 8;
  AURL_ENABLEURL = 1;

procedure TProgCorner.InitRichEditURLDetection(RE: TRichEdit);
var
  mask: NativeInt;
begin
  mask := SendMessage(RE.Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(RE.Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
  SendMessage(RE.Handle, EM_AUTOURLDETECT, {AURL_ENABLEEAURLS} AURL_ENABLEURL, 0);
end;

procedure TProgCorner.FormCreate(Sender: TObject);
begin
  ProgCorner:= Self;
  InitRichEditURLDetection(RichEdit1);
  InitRichEditURLDetection(RichEdit2);
  InitRichEditURLDetection(RichEdit3);
  InitRichEditURLDetection(RichEdit4);
  RichEdits[1]:= RichEdit1;
  RichEdits[2]:= RichEdit2;
  RichEdits[3]:= RichEdit3;
  RichEdits[4]:= RichEdit4;

  //WordWarp should be set during runtime only, because
  //otherwise the text will not warp, but rather be cut off
  //before run time.
  RichEdit1.Text:= RichEdit1.Text + ' ';
  RichEdit2.Text:= RichEdit2.Text + ' ';
  RichEdit3.Text:= RichEdit3.Text + ' ';
  RichEdit4.Text:= RichEdit4.Text + ' ';
  RichEdit1.WordWrap:= true;
  RichEdit2.WordWrap:= true;
  RichEdit3.WordWrap:= true;
  RichEdit4.WordWrap:= true;
  GetDlgSettings;
end;

procedure TProgCorner.Copy1Click(Sender: TObject);
var
  ActiveRichEdit: TRichEdit;
begin
  ActiveRichEdit:= TRichEdit(Self.FindComponent('RichEdit'+
    IntToStr(TabbedNotebook1.PageIndex+1)));
  with ActiveRichEdit do begin
    if SelText <> '' then Clipboard.AsText:= SelText
    else ClipBoard.AsText:= Lines.Text;
  end; {with}
end;

procedure TProgCorner.PopupMenu1Popup(Sender: TObject);
begin
  Copy1.Enabled:= true;
end;

procedure TProgCorner.GetDlgSettings;
begin
  with OpenReg do try
    Left:= ReadInteger('ProgCorn.pas','Left',Screen.Height);
    Top:= ReadInteger('ProgCorn.pas','Top',Screen.Width);
    finally Free;
    Left:= Min(Left,Screen.Width - Width);
    Top:= Min(Top, Screen.Height - Height);
  end; {with}
end;

procedure TProgCorner.SaveDlgSettings;
begin
  with OpenReg do try
    WriteInteger('ProgCorn.pas','Left',Left);
    WriteInteger('ProgCorn.pas','Top',Top);
    finally Free;
  end; {with}
end;

procedure TProgCorner.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveDlgSettings;
end;

procedure TProgCorner.Button2Click(Sender: TObject);
begin
  Application.HelpContext(4);
end;

{ TRichEdit }

//function TProgCorner.RichEditByHandle(Handle: HWnd): TRichEdit;
//var
//  i: integer;
//begin
//  for i:= Low(RichEdits) to High(RichEdits) do begin
//    if RichEdits[i].Handle = Handle then exit(RichEdits[i]);
//  end;
//  Result:= nil;
//end;

procedure TRichEdit.CNNotify(var Message: TWMNotifyRE);
var
  p: TENLink;
  sURL: string;
  CE: TRichEdit;
begin
  //if (Message.Msg = WM_NOTIFY) then begin
    if (Message.NMHdr.code = EN_LINK) then begin
      p:= TENLink(Pointer(TWMNotify(Message).NMHdr)^);
      if (p.Msg = WM_LBUTTONDOWN) then begin
        try
          //CE:= TRichEdit(ProgCorner.ActiveControl);
          //SendMessage(CE.Handle, EM_EXSETSEL, 0, Longint(@(p.chrg)));
          SendMessage(p.nmhdr.hwndFrom, EM_EXSETSEL, 0, Longint(@(p.chrg)));
          CE:= Self;
          if assigned(CE) then begin
            sURL:= CE.SelText;
            ShellExecute(Handle, 'open', PChar(sURL), 0, 0, SW_SHOWNORMAL);
          end;
        except
          {ignore}
        end;
      end;
    end;
  //end;
  inherited;
end;



end.
