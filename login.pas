unit Login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnLogout: TBitBtn;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    cbShowPassword: TCheckBox;
    edtLogin: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure cbShowPasswordChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLogin: TfrmLogin;

implementation

{$R *.lfm}

{ TfrmLogin }

procedure TfrmLogin.cbShowPasswordChange(Sender: TObject);
begin
  if cbShowPassword.Checked then begin
    edtPassword.PasswordChar := #0;
  end else begin
    edtPassword.PasswordChar := '*';
  end;

  edtPassword.Repaint;
end;

end.

