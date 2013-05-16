unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLIntf, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
  Main;

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
  OpenURL('http://narodmon.ru/');
end;

procedure TfrmAbout.Label6Click(Sender: TObject);
begin
  OpenURL('http://habrahabr.ru/users/mear/');
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblVersion.Caption := frmMain.FVersion;
end;

end.

