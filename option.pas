unit Option;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Buttons;

type

  { TfrmOption }

  TfrmOption = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cbProxyAuth: TCheckBox;
    cbAutostart: TCheckBox;
    cbWindValue: TComboBox;
    chkHideClose: TCheckBox;
    chkShortValue: TCheckBox;
    chkStayOnTop: TCheckBox;
    chkShowHidden: TCheckBox;
    cbTempValue: TComboBox;
    edtLocation: TEdit;
    edtProxyHost: TEdit;
    edtProxyLogin: TEdit;
    edtProxyPassword: TEdit;
    gbProxy: TGroupBox;
    gbProxyAuth: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblCurLocation: TLabel;
    pcConfig: TPageControl;
    rbProxyNo: TRadioButton;
    rbProxyHTTP: TRadioButton;
    edtProxyPort: TSpinEdit;
    edtUpdateInfo: TSpinEdit;
    sbRadius: TScrollBar;
    sbWidgetTransparent: TScrollBar;
    spRadius: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure cbProxyAuthChange(Sender: TObject);
    procedure rbProxyHTTPChange(Sender: TObject);
    procedure rbProxyNoChange(Sender: TObject);
    procedure sbRadiusChange(Sender: TObject);
    procedure sbWidgetTransparentChange(Sender: TObject);
    procedure spRadiusChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmOption: TfrmOption;

implementation

uses
  Widget;

{$R *.lfm}

{ TfrmOption }

procedure TfrmOption.rbProxyHTTPChange(Sender: TObject);
begin
  gbProxy.Enabled          := rbProxyHTTP.Checked;
  edtProxyHost.Enabled     := rbProxyHTTP.Checked;
  edtProxyPort.Enabled     := rbProxyHTTP.Checked;
  cbProxyAuth.Enabled      := rbProxyHTTP.Checked;

  gbProxyAuth.Enabled      := cbProxyAuth.Checked;
  edtProxyLogin.Enabled    := cbProxyAuth.Checked;
  edtProxyPassword.Enabled := cbProxyAuth.Checked;
end;

procedure TfrmOption.rbProxyNoChange(Sender: TObject);
begin
  gbProxy.Enabled          := rbProxyHTTP.Checked;
  edtProxyHost.Enabled     := rbProxyHTTP.Checked;
  edtProxyPort.Enabled     := rbProxyHTTP.Checked;
  cbProxyAuth.Enabled      := rbProxyHTTP.Checked;

  gbProxyAuth.Enabled      := cbProxyAuth.Checked;
  edtProxyLogin.Enabled    := cbProxyAuth.Checked;
  edtProxyPassword.Enabled := cbProxyAuth.Checked;
end;

procedure TfrmOption.sbRadiusChange(Sender: TObject);
begin
  spRadius.Value := sbRadius.Position;
end;

procedure TfrmOption.sbWidgetTransparentChange(Sender: TObject);
begin
  if Assigned(frmWidget) then begin
    frmWidget.AlphaBlendValue := sbWidgetTransparent.Position;
  end;
end;

procedure TfrmOption.spRadiusChange(Sender: TObject);
begin
  sbRadius.Position := spRadius.Value;
end;

procedure TfrmOption.cbProxyAuthChange(Sender: TObject);
begin
  gbProxyAuth.Enabled      := cbProxyAuth.Checked;
  edtProxyLogin.Enabled    := cbProxyAuth.Checked;
  edtProxyPassword.Enabled := cbProxyAuth.Checked;
end;

end.

