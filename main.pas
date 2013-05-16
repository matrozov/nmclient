unit Main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef MSWINDOWS}
    registry,
  {$endif}
  {$ifdef UNIX}
    BaseUnix,
  {$endif}
  Process,
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  DateUtils,
  ExtCtrls,
  Menus,
  StdCtrls,
  Device,
  Math,
  LCLIntf,
  ActnList, ComCtrls,
  mJSON,
  MD5hash,
  VInfo,
  VersionTypes, types;
type
  { TfrmMain }

  TSelectMode = (smLocation, smTray, smHide);

  TfrmMain = class(TForm)
    actAccount: TAction;
    actConfig: TAction;
    actHide: TAction;
    actExit: TAction;
    actAbout: TAction;
    actFilterAll: TAction;
    actFilterMy: TAction;
    actFilterPublic: TAction;
    actFilterFavorit: TAction;
    actFilterHide: TAction;
    actFilterUnknown: TAction;
    actFilterWet: TAction;
    actFilterTemp: TAction;
    actFilterPressure: TAction;
    actFilterWind: TAction;
    actFilterElectricity: TAction;
    actFilterAzimut: TAction;
    actSiteTwitter: TAction;
    actSiteVK: TAction;
    actSortTime: TAction;
    actUpdateInfo: TAction;
    actUpdateList: TAction;
    actUpdateProgram: TAction;
    actSortDistance: TAction;
    actProgrammSite: TAction;
    actSite: TAction;
    actShow: TAction;
    ActionList: TActionList;
    App: TApplicationProperties;
    Image: TImageList;
    MainMenu: TMainMenu;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    miShow: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    miAccount: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    miHide: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pbMain: TPaintBox;
    sbScroll: TScrollBar;
    sbStatus: TStatusBar;
    tmrUpdate: TTimer;
    TrayPopup: TPopupMenu;
    Tray: TTrayIcon;
    procedure actAboutExecute(Sender: TObject);
    procedure actAccountExecute(Sender: TObject);
    procedure actConfigExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFilterAllExecute(Sender: TObject);
    procedure actFilterAzimutExecute(Sender: TObject);
    procedure actFilterElectricityExecute(Sender: TObject);
    procedure actFilterFavoritExecute(Sender: TObject);
    procedure actFilterHideExecute(Sender: TObject);
    procedure actFilterMyExecute(Sender: TObject);
    procedure actFilterPressureExecute(Sender: TObject);
    procedure actFilterPublicExecute(Sender: TObject);
    procedure actFilterTempExecute(Sender: TObject);
    procedure actFilterUnknownExecute(Sender: TObject);
    procedure actFilterWetExecute(Sender: TObject);
    procedure actFilterWindExecute(Sender: TObject);
    procedure actHideExecute(Sender: TObject);
    procedure actProgrammSiteExecute(Sender: TObject);
    procedure actShowExecute(Sender: TObject);
    procedure actSiteExecute(Sender: TObject);
    procedure actSiteTwitterExecute(Sender: TObject);
    procedure actSiteVKExecute(Sender: TObject);
    procedure actSortDistanceExecute(Sender: TObject);
    procedure actSortTimeExecute(Sender: TObject);
    procedure actUpdateInfoExecute(Sender: TObject);
    procedure actUpdateListExecute(Sender: TObject);
    procedure actUpdateProgramExecute(Sender: TObject);
    procedure AppMinimize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure pbMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbMainMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbMainPaint(Sender: TObject);
    procedure sbScrollChange(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
  private
    FDeviceControl: TDeviceControl;

    FDeviceList:    TDeviceList;

    FTrayList:      TList;

    FSelectPublic:  TSelect;
    FSelectMy:      TSelect;
    FSelectTray:    TSelect;
    FSelectHide:    TSelect;

    FSensorFilter:  TSensorFilter;

    FSort:          TDeviceListSort;

    FCanClose:   Boolean;
    FDrawH, FmX, FmY: Integer;
    FBuffer:     TBitmap;
    FSelect:     Integer;
    FSelectMode: TSelectMode;

    FLastUpdate: TDateTime;
    FLastUpdateInfo: TDateTime;
    FLastVerUpdate: TDateTime;

    FOption:     TmJSON;
  public
    FMD5:        String;
    FVersion:    String;
    FLogin:      String;
    FLocation:   String;
  public
    procedure onStart;

    procedure TrayNotify(ACaption, AMessage: String; ATimeOut: Integer = 5000);
  private
    procedure MenuUpdate;
    procedure DLUpdate;
    procedure Login(AClear: Boolean);
    procedure ConnectError(ABallon: Boolean);
    procedure CheckUpdate(AShowInTray, AShowNoUpdate: Boolean);
    procedure Config(APageIndex: Integer = 0);
    function  AutostartCheck: Boolean;
    procedure AutostartSet(AEnabled: Boolean);
    procedure Calc;
    procedure Draw;
    procedure TrayPrepare;
    procedure TrayUpdate;

    function  CelToFar(ACel: Single): Single;
    function  MSToKmH(AMS: Single): Single;
    function  GrToNapr(AGr: Single; AMode: Byte): String;
    function  ValueToStr(AValue: Single): String;
    function  DateToStr(AValue: TDateTime): String;
  end;

const
  DeviceHeight = 30;
  DeviceSeparate = 10;
  SensorHeight = 50;
  SensorSeparate = 5;

var
  frmMain: TfrmMain;

function MessageDlgEx(ACaption, AText: String; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;

implementation

uses
  About, Option, Login, Widget;

{$R *.lfm}

function MessageDlgEx(ACaption, AText: String; ADlgType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
var
  MsgDlg: TForm;
  I: Integer;
  Button: TCustomButton;
begin
  if (Assigned(frmMain)) then begin
    frmMain.Show;
  end;

  MsgDlg := CreateMessageDialog(AText, ADlgType, AButtons);

  MsgDlg.FormStyle := fsSystemStayOnTop;
  MsgDlg.Position  := poMainFormCenter;
  MsgDlg.Caption   := ACaption;

  for I := 0 to MsgDlg.ComponentCount - 1 do begin
    if not (MsgDlg.Components[I] is TCustomButton) then begin
      Continue;
    end;

    Button := TButton(MsgDlg.Components[I]);

    if Button.Caption = '&Yes' then begin
      Button.Caption := 'Да';
    end else if Button.Caption = '&No' then begin
      Button.Caption := 'Нет';
    end else if Button.Caption = '&Close' then begin
      Button.Caption := 'Закрыть';
    end;

    Button.Cursor := crHandPoint;
  end;

  Result := MsgDlg.ShowModal;

  MsgDlg.Free;
end;

function AngleColor(FromColor, ToColor: TColor; MaxAngle, Angle: word): TColor;
var
  FR, FG, FB, TR, TG, TB, RR, RG, RB: byte;
begin
  FB := byte(FromColor shr 16);
  FG := byte(FromColor shr 8);
  FR := byte(FromColor);

  TB := byte(ToColor shr 16);
  TG := byte(ToColor shr 8);
  TR := byte(ToColor);

  RR := (TR - FR) * Angle div MaxAngle + FR;
  RG := (TG - FG) * Angle div MaxAngle + FG;
  RB := (TB - FB) * Angle div MaxAngle + FB;

  Result := (RR or (RG shl 8) or (RB shl 16));
end;

{ TfrmMain }

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FOption.Get('hideClose', True).AsBoolean then begin
    CanClose := FCanClose;

    if not CanClose then begin
      Hide;
    end;
  end else begin
    CanClose := True;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frmWidget := TfrmWidget.Create(Application);
  frmWidget.Show;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  FCanClose := True;
  Close;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Application);

  if FOption.Get('stayOnTop', False).AsBoolean then begin
    frmAbout.FormStyle  := fsSystemStayOnTop;
  end else begin
    frmAbout.FormStyle  := fsNormal;
  end;

  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TfrmMain.actAccountExecute(Sender: TObject);
begin
  Login(True);
end;

procedure TfrmMain.actConfigExecute(Sender: TObject);
begin
  Config;
end;

procedure TfrmMain.actFilterAllExecute(Sender: TObject);
begin
  FSelectPublic := tAny;
  FSelectMy     := tAny;
  FSelectTray   := tAny;
  FSelectHide   := tFalse;

  DLUpdate;

  Calc;

  actFilterAll.Checked := True;

  FOption.Get('filter', 'all').AsString := 'all';
end;

procedure TfrmMain.actFilterAzimutExecute(Sender: TObject);
begin
  if actFilterAzimut.Checked then begin
    FSensorFilter := FSensorFilter + [smAzimut]
  end else begin
    FSensorFilter := FSensorFilter - [smAzimut];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actFilterElectricityExecute(Sender: TObject);
begin
  if actFilterElectricity.Checked then begin
    FSensorFilter := FSensorFilter + [smElectricity]
  end else begin
    FSensorFilter := FSensorFilter - [smElectricity];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actFilterFavoritExecute(Sender: TObject);
begin
  FSelectPublic := tAny;
  FSelectMy     := tAny;
  FSelectTray   := tTrue;
  FSelectHide   := tFalse;

  DLUpdate;

  Calc;

  actFilterFavorit.Checked := True;

  FOption.Get('filter', 'all').AsString := 'favorit';
end;

procedure TfrmMain.actFilterHideExecute(Sender: TObject);
begin
  FSelectPublic := tAny;
  FSelectMy     := tAny;
  FSelectTray   := tAny;
  FSelectHide   := tTrue;

  DLUpdate;

  Calc;

  actFilterHide.Checked := True;

  FOption.Get('filter', 'all').AsString := 'hide';
end;

procedure TfrmMain.actFilterMyExecute(Sender: TObject);
begin
  FSelectPublic := tAny;
  FSelectMy     := tTrue;
  FSelectTray   := tAny;
  FSelectHide   := tFalse;

  DLUpdate;

  Calc;

  actFilterMy.Checked := True;

  FOption.Get('filter', 'all').AsString := 'my';
end;

procedure TfrmMain.actFilterPublicExecute(Sender: TObject);
begin
  FSelectPublic := tTrue;
  FSelectMy     := tAny;
  FSelectTray   := tAny;
  FSelectHide   := tFalse;

  DLUpdate;

  Calc;

  actFilterPublic.Checked := True;

  FOption.Get('filter', 'all').AsString := 'public';
end;

procedure TfrmMain.actFilterUnknownExecute(Sender: TObject);
begin
  if actFilterUnknown.Checked then begin
    FSensorFilter := FSensorFilter + [smUnknown]
  end else begin
    FSensorFilter := FSensorFilter - [smUnknown];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actFilterTempExecute(Sender: TObject);
begin
  if actFilterTemp.Checked then begin
    FSensorFilter := FSensorFilter + [smTemp]
  end else begin
    FSensorFilter := FSensorFilter - [smTemp];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actFilterPressureExecute(Sender: TObject);
begin
  if actFilterPressure.Checked then begin
    FSensorFilter := FSensorFilter + [smPressure]
  end else begin
    FSensorFilter := FSensorFilter - [smPressure];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actFilterWetExecute(Sender: TObject);
begin
  if actFilterWet.Checked then begin
    FSensorFilter := FSensorFilter + [smWet]
  end else begin
    FSensorFilter := FSensorFilter - [smWet];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actFilterWindExecute(Sender: TObject);
begin
  if actFilterWind.Checked then begin
    FSensorFilter := FSensorFilter + [smWind]
  end else begin
    FSensorFilter := FSensorFilter - [smWind];
  end;

  DLUpdate;

  Calc;

  FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger := Integer(FSensorFilter);
end;

procedure TfrmMain.actHideExecute(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMain.actProgrammSiteExecute(Sender: TObject);
begin
  OpenURL('http://vk.com/page-41162221_44149589');
end;

procedure TfrmMain.actShowExecute(Sender: TObject);
begin
  if Showing then begin
    Hide;
  end else begin
    Draw;
    Show;
    BringToFront;
  end;
end;

procedure TfrmMain.actSiteExecute(Sender: TObject);
begin
  OpenURL('http://narodmon.ru/');
end;

procedure TfrmMain.actSiteTwitterExecute(Sender: TObject);
begin
  OpenURL('https://twitter.com/narodmon');
end;

procedure TfrmMain.actSiteVKExecute(Sender: TObject);
begin
  OpenURL('http://vk.com/narodmon');
end;

procedure TfrmMain.actSortDistanceExecute(Sender: TObject);
begin
  FSort := dsDistance;

  DLUpdate;

  Draw;

  actSortDistance.Checked := True;

  FOption.Get('sort', 'distance').AsString := 'distance';
end;

procedure TfrmMain.actSortTimeExecute(Sender: TObject);
begin
  FSort := dsTime;

  DLUpdate;

  Draw;

  actSortTime.Checked := True;

  FOption.Get('sort', 'distance').AsString := 'time';
end;

procedure TfrmMain.actUpdateInfoExecute(Sender: TObject);
begin
  if FDeviceControl.UpdateInfoAll then begin
    DLUpdate;

    Calc;

    TrayPrepare;
  end else begin
    ConnectError(True);
  end;
end;

procedure TfrmMain.actUpdateListExecute(Sender: TObject);
begin
  if FDeviceControl.UpdateList then begin
    DLUpdate;

    Calc;

    TrayPrepare;
  end else begin
    ConnectError(True);
  end;
end;

procedure TfrmMain.actUpdateProgramExecute(Sender: TObject);
begin
  CheckUpdate(False, True);
end;

procedure TfrmMain.AppMinimize(Sender: TObject);
begin
  Application.Restore;

  if Showing then begin
    Hide;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  tmrUpdate.Enabled := False;

  FDeviceList.Free;
  FDeviceControl.Free;

  FOption.Get('width',  Width).AsInteger  := Width;
  FOption.Get('height', Height).AsInteger := Height;
  FOption.Get('left',   Left).AsInteger   := Left;
  FOption.Get('top',    Top).AsInteger    := Top;

  FOption.SaveToFile(ExtractFilePath(Application.ExeName) + 'nmclient.conf', True);
  FOption.Free;

  FBuffer.Free;

  while FTrayList.Count > 0 do begin
    TTrayIcon(FTrayList.Items[0]).Free;

    FTrayList.Delete(0);
  end;

  FTrayList.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  Draw;
end;

procedure TfrmMain.MenuItem10Click(Sender: TObject);
begin
  if FLogin <> '' then begin
    actAccount.Caption := 'Учетная запись (' + FLogin + ')';
  end else begin
    actAccount.Caption := 'Учетная запись';
  end;
end;

procedure TfrmMain.pbMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Sensor: TSensor;
  Device: TDevice;
  SensorTag: TSensorTag;
begin
  FmX := X;
  FmY := Y;

  if (ssLeft in Shift) and (FSelect <> -1) then begin
    Draw;

    if FSelectMode in [smTray, smHide] then begin
      Sensor := FDeviceList.FindSensor(FSelect);

      if Sensor <> nil then begin
        case FSelectMode of
          smTray: SensorTag := stTray;
          smHide: SensorTag := stHide;
        end;

        if SensorTag in Sensor.Tags then begin
          Sensor.Tags := Sensor.Tags - [SensorTag];
        end else begin
          Sensor.Tags := Sensor.Tags + [SensorTag];
        end;

        DLUpdate;

        Calc;

        TrayPrepare;

        Draw;
      end;
    end else if FSelectMode = smLocation then begin
      Device := FDeviceList.Find(FSelect);

      if Device <> nil then begin
        OpenURL('http://narodmon.ru?id=' + IntToStr(Device.ID));
      end;
    end;
  end;
end;

procedure TfrmMain.pbMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) or (ssRight in Shift) then begin
    sbScroll.Position := sbScroll.Position + FmY - Y;
  end;

  FmX := X;
  FmY := Y;

  Draw;
end;

procedure TfrmMain.pbMainMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  sbScroll.Position := sbScroll.Position - WheelDelta div 3;

  Draw;
end;

procedure TfrmMain.pbMainPaint(Sender: TObject);
begin
  pbMain.Canvas.Draw(0, 0, FBuffer);
end;

procedure TfrmMain.sbScrollChange(Sender: TObject);
begin
  Draw;
end;

procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
begin
  tmrUpdate.Enabled := False;

  if MinutesBetween(Now, FLastVerUpdate) >= 30 then begin
    CheckUpdate(True, False);
  end;

  if MinutesBetween(Now, FLastUpdate) >= 60 then begin
    FDeviceControl.UpdateList;

    DLUpdate;

    Calc;

    TrayUpdate;

    FLastUpdate := Now;
  end else if MinutesBetween(Now, FLastUpdateInfo) >= FOption.Get('updateInfo', 5).AsInteger then begin
    FDeviceControl.UpdateInfoAll;

    DLUpdate;

    Calc;

    TrayUpdate;

    FLastUpdateInfo := Now;
  end;

  tmrUpdate.Enabled := True;
end;

procedure TfrmMain.TrayPopupPopup(Sender: TObject);
begin
  miShow.Visible := not Showing;
  miHide.Visible := Showing;

  if FLogin <> '' then begin
    actAccount.Caption := 'Учетная запись (' + FLogin + ')';
  end else begin
    actAccount.Caption := 'Учетная запись';
  end;
end;

procedure TfrmMain.DLUpdate;
begin
  if FDeviceList <> nil then begin
    FDeviceList.Free;
  end;

  FDeviceList := FDeviceControl.Select(FSelectPublic, FSelectMy, FSelectTray, FSelectHide, FSort, FSensorFilter);

  MenuUpdate;

  Calc;
end;

procedure TfrmMain.Login(AClear: Boolean);
var
  Result: Integer;
begin
  frmLogin := TfrmLogin.Create(Application);

  if FOption.Get('stayOnTop', False).AsBoolean then begin
    frmLogin.FormStyle  := fsSystemStayOnTop;
  end else begin
    frmLogin.FormStyle  := fsNormal;
  end;

  if AClear then begin
    frmLogin.edtLogin.Text          := FLogin;
    frmLogin.edtPassword.Text       := '';
    frmLogin.cbShowPassword.Checked := False;
  end;

  if FLogin <> '' then begin
    frmLogin.edtLogin.Enabled       := False;
    frmLogin.edtPassword.Enabled    := False;
    frmLogin.cbShowPassword.Enabled := False;
    frmLogin.btnOk.Enabled          := False;
    frmLogin.btnLogout.Enabled      := True;
  end else begin
    frmLogin.edtLogin.Enabled       := True;
    frmLogin.edtPassword.Enabled    := True;
    frmLogin.cbShowPassword.Enabled := True;
    frmLogin.btnOk.Enabled          := True;
    frmLogin.btnLogout.Enabled      := False;
  end;

  Result := frmLogin.ShowModal;

  if Result = mrOk then begin
    FLogin := FDeviceControl.Login(frmLogin.edtLogin.Text, frmLogin.edtPassword.Text);

    if FLogin <> '' then begin
      MessageDlgEx('Авторизация', 'Вход выполнен успешно!', mtInformation, [mbOK]);

      FDeviceControl.UpdateList;

      DLUpdate;

      Calc;

      TrayPrepare;
    end else begin
      MessageDlgEx('Авторизация', 'Логин или пароль были введены не верно!', mtError, [mbOK]);

      Login(False);
    end;
  end else if Result = mrClose then begin
    FLogin := FDeviceControl.Logout;

    if FLogin = '' then begin
      MessageDlgEx('Авторизация', 'Выход выполнен успешно!', mtInformation, [mbOK]);

      FDeviceControl.UpdateList;

      DLUpdate;

      Calc;

      TrayPrepare;
    end;
  end;

  frmLogin.Free;
end;

procedure TfrmMain.ConnectError(ABallon: Boolean);
begin
  if ABallon then begin
    TrayNotify('Ошибка', 'Ошибка соединения');
  end else if MessageDlgEx('Ошибка соединения', 'Не удалось связаться с сервером.'#13#10'Возможно у вас отсутствует доступ в интернет или он осуществляется через прокси-сервер.'#13#10#13#10'Хотите перейти в настройки сети?', mtError, mbYesNo) = mrYes then begin
    Config(2);
  end;
end;

procedure TfrmMain.CheckUpdate(AShowInTray, AShowNoUpdate: Boolean);
var
  FilePath, MD5, MD5new: String;

  procedure NotifyUpdate(AInTray: Boolean);
  var
    Prc: TProcess;
  begin
    if AInTray then begin
      TrayNotify('Обновление программы', 'Доступно новое обновление программы. Перезапустите программу для обновления', 10000);
    end else begin
      if MessageDlgEx('Обновление программы', 'На сайте проекта доступна новая версия программы,'#13#10'хотите загрузить и запустить новую версию?', mtConfirmation, mbYesNo) = mrYes then begin
        CopyFile(Application.ExeName, FilePath + {$ifdef WINDOWS}'update.exe'{$endif}{$ifdef UNIX}'update'{$endif});
        {$ifdef UNIX}FpChmod(FilePath + 'update', &777);{$endif}

        Prc := TProcess.Create(nil);
        Prc.CurrentDirectory := FilePath;
        Prc.Executable := {$ifdef WINDOWS}'update.exe'{$endif}{$ifdef UNIX}'update'{$endif};
        Prc.Parameters.Add('/u');
        Prc.Parameters.Add(ExtractFileName(Application.ExeName));
        Prc.Options := Prc.Options - [poWaitOnExit];
        Prc.Execute;
        Prc.Free;

        Application.Terminate;
        Exit;
      end;
    end;
  end;
begin
  FLastVerUpdate := Now;

  MD5 := FDeviceControl.CheckUpdate(FVersion);

  if MD5 = '' then begin
    ConnectError(True);
  end else if MD5 <> FMD5 then begin
    FilePath := ExtractFilePath(Application.ExeName);

    if FileExists(FilePath + 'nmclient.upd') then begin
      MD5new := GetMD5File(FilePath + 'nmclient.upd');

      if MD5new = MD5 then begin
        NotifyUpdate(AShowInTray);

        Exit;
      end;
    end;

    if FDeviceControl.DownloadUpdate(MD5) then begin
      NotifyUpdate(AShowInTray);
    end;
  end else if AShowNoUpdate then begin
    if AShowInTray then begin
      TrayNotify('Обновление программы', 'Новых версий не обнаружено');
    end else begin
      MessageDlgEx('Обновление программы', 'Новых версий не обнаружено', mtInformation, [mbOK]);
    end;
  end;
end;

procedure TfrmMain.onStart;
var
  Info: TVersionInfo;
  Str: String;

  function ProductVersionToString(PV: TFileProductVersion): String;
  begin
    Result := Format('%d.%d.%d', [PV[0], PV[1], PV[2]]);
  end;
begin
  FMD5 := GetMD5File(Application.ExeName);

  FCanClose := False;

  DoubleBuffered := True;

  FTrayList := TList.Create;

  FBuffer := TBitmap.Create;
  FBuffer.Canvas.AntialiasingMode := amOn;

  actFilterAll.Checked := True;
  actSortDistance.Checked := True;

  FOption := TmJSON.Create;

  if FileExists(ExtractFilePath(Application.ExeName) + 'nmclient.conf') then begin
    FOption.LoadFromFile(ExtractFilePath(Application.ExeName) + 'nmclient.conf');

    App.ShowMainForm := not FOption.Get('showHidden', True).AsBoolean;
  end;

  if not FOption.Exists('uuid') then begin
    FOption.Add('uuid', MD5(IntToStr(Random(MaxInt)) + '.' + IntToStr(Random(MaxInt)) + '.' + IntToStr(Random(MaxInt)) + '.' + IntToStr(Random(MaxInt))));
  end;

  Info := TVersionInfo.Create;
  Info.Load(HInstance);

  FVersion := ProductVersionToString(Info.FixedInfo.FileVersion);

  Info.Free;

  FLastUpdate := Now;
  FLastUpdateInfo := Now;
  FLastVerUpdate := Now;

  FDeviceControl := TDeviceControl.Create(FOption);

  if not FDeviceControl.UpdateList then begin
    ConnectError(False);
  end;

  FLogin := FDeviceControl.CheckLogin;

  Str := FOption.Get('filter', 'all').AsString;

  if Str = 'my' then begin
    actFilterMy.Execute;
  end else if Str = 'public' then begin
    actFilterPublic.Execute;
  end else if Str = 'favorit' then begin
    actFilterFavorit.Execute;
  end else if Str = 'hide' then begin
    actFilterHide.Execute;
  end else begin
    actFilterAll.Execute;
  end;

  Str := FOption.Get('sort', 'distance').AsString;

  if Str = 'time' then begin
    actSortTime.Execute;
  end else begin
    actSortDistance.Execute;
  end;

  FSensorFilter := TSensorFilter(FOption.Get('sensorFilter', Integer(CSensorFilterAll)).AsInteger);

  actFilterUnknown.Checked     := smUnknown     in FSensorFilter;
  actFilterTemp.Checked        := smTemp        in FSensorFilter;
  actFilterWet.Checked         := smWet         in FSensorFilter;
  actFilterPressure.Checked    := smPressure    in FSensorFilter;
  actFilterWind.Checked        := smWind        in FSensorFilter;
  actFilterAzimut.Checked      := smAzimut      in FSensorFilter;
  actFilterElectricity.Checked := smElectricity in FSensorFilter;

  if FOption.Get('stayOnTop', False).AsBoolean then begin
    FormStyle := fsSystemStayOnTop;
  end else begin
    FormStyle := fsNormal;
  end;

  FLocation := FDeviceControl.Location(FOption.Get('location', '').AsString);

  if FLocation = '' then begin
    ConnectError(True);
  end else begin
    Caption   := 'Народный мониторинг - Клиент (' + FLocation + ')';
    Application.Title := 'Народный мониторинг - Клиент (' + FLocation + ')';
    Tray.Hint := 'Народный мониторинг - Клиент (' + FLocation + ')';

    if not FOption.Get('locationSet', False).AsBoolean then begin
      if MessageDlgEx('Ваше текущее расположение', 'Программа определила ваше текущее расположение как: "' + FLocation + '" - это точное расположение?'#13#10#13#10'Нажмите "Нет" для перехода в настройки расположения.', mtConfirmation, mbYesNo) = mrNo then begin
        Config(1);
      end;

      FOption.Get('locationSet', True).AsBoolean := True;
    end;
  end;

  Width  := FOption.Get('width',  400).AsInteger;
  Height := FOption.Get('height', 500).AsInteger;
  Left   := FOption.Get('left',   Integer((Screen.Monitors[0].Width - Width) div 2)).AsInteger;
  Top    := FOption.Get('top',    Integer((Screen.Monitors[0].Height - Height) div 2)).AsInteger;

  DLUpdate;

  TrayPrepare;

  CheckUpdate(False, False);

  tmrUpdate.Enabled := True;
end;

procedure TfrmMain.MenuUpdate;
var
  D, S: Integer;
  Device: TDevice;
  Sensor: TSensor;
  CntAll, CntMy, CntPublic, CntFavorit, CntHide, CntUnknown, CntTemp, CntWet, CntPressure, CntWind, CntAzimut, CntElectricity: Integer;
begin
  CntAll         := 0;
  CntMy          := 0;
  CntPublic      := 0;
  CntFavorit     := 0;
  CntHide        := 0;
  CntUnknown     := 0;
  CntTemp        := 0;
  CntWet         := 0;
  CntPressure    := 0;
  CntWind        := 0;
  CntAzimut      := 0;
  CntElectricity := 0;

  for D := 0 to FDeviceControl.Count - 1 do begin
    Device := FDeviceControl.Get(D);

    for S := 0 to Device.Count - 1 do begin
      Sensor := Device.Get(S);

      if stHide in Sensor.Tags then begin
        Inc(CntHide);
      end else begin
        if Sensor.Pub then begin
          Inc(CntPublic);
        end;

        if stTray in Sensor.Tags then begin
          Inc(CntFavorit);
        end;

        if Device.My then begin
          Inc(CntMy);
        end;

        Inc(CntAll);
      end;

      case Sensor.Mode of
        smTemp:        Inc(CntTemp);
        smWet:         Inc(CntWet);
        smPressure:    Inc(CntPressure);
        smWind:        Inc(CntWind);
        smAzimut:      Inc(CntAzimut);
        smElectricity: Inc(CntElectricity);
        else           Inc(CntUnknown);
      end;
    end;
  end;

  actFilterAll.Caption         := Format('Все (%d)', [CntAll]);

  actFilterMy.Caption          := Format('Мои (%d)', [CntMy]);
  actFilterMy.Enabled          := CntMy > 0;

  actFilterPublic.Caption      := Format('Публичные (%d)', [CntPublic]);
  actFilterPublic.Enabled      := CntPublic > 0;

  actFilterFavorit.Caption     := Format('Избранные (%d)', [CntFavorit]);
  actFilterFavorit.Enabled     := CntFavorit > 0;

  actFilterHide.Caption        := Format('Скрытые (%d)', [CntHide]);
  actFilterHide.Enabled        := CntHide > 0;

  actFilterUnknown.Caption     := Format('Неизвестные (%d)', [CntUnknown]);
  actFilterUnknown.Enabled     := CntUnknown > 0;

  actFilterTemp.Caption        := Format('Температура (%d)', [CntTemp]);
  actFilterTemp.Enabled        := CntTemp > 0;

  actFilterWet.Caption         := Format('Влажность (%d)', [CntWet]);
  actFilterWet.Enabled         := CntWet > 0;

  actFilterPressure.Caption    := Format('Давление (%d)', [CntPressure]);
  actFilterPressure.Enabled    := CntPressure > 0;

  actFilterWind.Caption        := Format('Ветер (%d)', [CntWind]);
  actFilterWind.Enabled        := CntWind > 0;

  actFilterAzimut.Caption      := Format('Азимут (%d)', [CntAzimut]);
  actFilterAzimut.Enabled      := CntAzimut > 0;

  actFilterElectricity.Caption := Format('Электричество (%d)', [CntElectricity]);
  actFilterElectricity.Enabled := CntElectricity > 0;
end;

procedure TfrmMain.Config(APageIndex: Integer);
var
  jsonProxy: TmJSONList;
begin
  frmOption := TfrmOption.Create(Application);

  if FOption.Get('stayOnTop', False).AsBoolean then begin
    frmOption.FormStyle  := fsSystemStayOnTop;
  end else begin
    frmOption.FormStyle  := fsNormal;
  end;

  frmOption.cbAutostart.Checked := AutostartCheck;
  {$IFNDEF WINDOWS}
    frmOption.cbAutostart.Enabled := False;
  {$endif}

  frmOption.chkShowHidden.Checked  := FOption.Get('showHidden', True).AsBoolean;
  frmOption.chkHideClose.Checked   := FOption.Get('hideClose',  True).AsBoolean;
  frmOption.chkStayOnTop.Checked   := FOption.Get('stayOnTop',  False).AsBoolean;

  frmOption.edtLocation.Text       := FOption.Get('location', '').AsString;
  frmOption.lblCurLocation.Caption := FLocation;

  frmOption.sbRadius.Position      := FOption.Get('radius', 1000).AsInteger;
  frmOption.spRadius.Value         := FOption.Get('radius', 1000).AsInteger;

  jsonProxy := TmJSONList(FOption.Get('proxy'));

  frmOption.rbProxyNo.Checked     := True;
  frmOption.edtProxyHost.Text     := '';
  frmOption.edtProxyPort.Value    := 3128;
  frmOption.cbProxyAuth.Checked   := False;
  frmOption.edtProxyLogin.Text    := '';
  frmOption.edtProxyPassword.Text := '';

  if jsonProxy <> nil then begin
    frmOption.rbProxyHTTP.Checked := jsonProxy.Get('enabled', False).AsBoolean;

    frmOption.edtProxyHost.Text   := jsonProxy.Get('host', '').AsString;
    frmOption.edtProxyPort.Value  := jsonProxy.Get('port', 3128).AsInteger;
    frmOption.cbProxyAuth.Checked := jsonProxy.Get('auth', False).AsBoolean;

    frmOption.edtProxyLogin.Text    := jsonProxy.Get('login', '').AsString;
    frmOption.edtProxyPassword.Text := jsonProxy.Get('password', '').AsString;
  end;

  frmOption.edtUpdateInfo.Value     := FOption.Get('updateInfo', 5).AsInteger;
  frmOption.cbTempValue.ItemIndex   := FOption.Get('tempValue', 0).AsInteger;
  frmOption.cbWindValue.ItemIndex   := FOption.Get('windValue', 0).AsInteger;
  frmOption.chkShortValue.Checked   := FOption.Get('shortValue', True).AsBoolean;

  frmOption.pcConfig.ActivePageIndex := APageIndex;

  if frmOption.ShowModal = mrOk then begin
    AutostartSet(frmOption.cbAutostart.Checked);

    FOption.Get('showHidden', True).AsBoolean  := frmOption.chkShowHidden.Checked;
    FOption.Get('hideClose',  True).AsBoolean  := frmOption.chkHideClose.Checked;
    FOption.Get('stayOnTop',  False).AsBoolean := frmOption.chkStayOnTop.Checked;

    if FOption.Get('stayOnTop', False).AsBoolean then begin
      FormStyle := fsSystemStayOnTop;
    end else begin
      FormStyle := fsNormal;
    end;

    if jsonProxy = nil then begin
      jsonProxy := FOption.AddObject('proxy');
    end;

    jsonProxy.Get('enabled',  False).AsBoolean  := frmOption.rbProxyHTTP.Checked;
    jsonProxy.Get('host',     '').AsString      := frmOption.edtProxyHost.Text;
    jsonProxy.Get('port',     3128).AsInteger   := frmOption.edtProxyPort.Value;
    jsonProxy.Get('auth',     False).AsBoolean  := frmOption.cbProxyAuth.Checked;
    jsonProxy.Get('login',    '').AsString      := frmOption.edtProxyLogin.Text;
    jsonProxy.Get('password', '').AsString      := frmOption.edtProxyPassword.Text;

    if FOption.Get('location', '').AsString <> Trim(frmOption.edtLocation.Text) then begin
      FOption.Get('location', '').AsString := Trim(frmOption.edtLocation.Text);
      FOption.Get('locationSet', True).AsBoolean := True;

      FLocation := FDeviceControl.Location(FOption.Get('location', '').AsString);
      Caption   := 'Народный мониторинг - Клиент (' + FLocation + ')';
      Application.Title := 'Народный мониторинг - Клиент (' + FLocation + ')';
    end;

    FOption.Get('radius', 1000).AsInteger := frmOption.spRadius.Value;

    FOption.Get('updateInfo', 5).AsInteger    := frmOption.edtUpdateInfo.Value;
    FOption.Get('tempValue', 0).AsInteger     := frmOption.cbTempValue.ItemIndex;
    FOption.Get('windValue', 0).AsInteger     := frmOption.cbWindValue.ItemIndex;
    FOption.Get('shortValue', True).AsBoolean := frmOption.chkShortValue.Checked;

    FOption.SaveToFile(ExtractFilePath(Application.ExeName) + 'nmclient.conf');

    FDeviceControl.UpdateList;

    DLUpdate;

    Calc;

    TrayUpdate;
  end;

  frmOption.Free;
end;

function TfrmMain.AutostartCheck: Boolean;
{$ifdef MSWINDOWS}
var
  Reg: TRegistry;
{$endif}
begin
  Result := False;

  {$ifdef MSWINDOWS}
  Reg := TRegistry.Create(KEY_READ);

  Reg.RootKey := HKEY_CURRENT_USER;

  if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Run') then begin
    Result := LowerCase(Reg.ReadString('nmclient')) = LowerCase(Application.ExeName);
  end;
  {$endif}
end;

procedure TfrmMain.AutostartSet(AEnabled: Boolean);
{$ifdef MSWINDOWS}
var
  Reg: TRegistry;
{$endif}
begin
  {$ifdef MSWINDOWS}
  Reg := TRegistry.Create(KEY_WRITE);

  Reg.RootKey := HKEY_CURRENT_USER;

  if Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run', False) then begin
    if AEnabled then begin
      Reg.WriteString('nmclient', Application.ExeName);
    end else begin
      Reg.DeleteValue('nmclient');
    end;
  end;
  {$endif}
end;

procedure TfrmMain.Calc;
var
  D, S: Integer;
  Device: TDevice;
begin
  if FDeviceList = nil then begin
    Exit;
  end;

  FDrawH := DeviceSeparate;

  for D := 0 to FDeviceList.Count - 1 do begin
    Device := FDeviceList.Get(D);

    FDrawH := FDrawH + DeviceHeight + DeviceSeparate;

    for S := 0 to Device.Count - 1 do begin
      FDrawH := FDrawH + SensorHeight + SensorSeparate;
    end;
  end;

  FDrawH := FDrawH;

  Draw;
end;

procedure TfrmMain.Draw;
var
  D, S, DrawY, DeviceH: Integer;
  DeviceDraw: Boolean;
  Device: TDevice;
  Sensor: TSensor;
  Cur: TCursor;
  Str: String;
  Style: TTextStyle;
begin
  if FDeviceList = nil then begin
    Exit;
  end;

  FSelect := -1;

  FBuffer.SetSize(pbMain.Width, pbMain.Height);

  sbScroll.Max := FDrawH;
  sbScroll.PageSize := pbMain.Height;
  sbScroll.Position := Max(sbScroll.Position, 0);
  sbScroll.Position := Min(sbScroll.Position, FDrawH - pbMain.Height);

  Cur := crDefault;

  FBuffer.Canvas.Brush.Style := bsSolid;
  FBuffer.Canvas.Brush.Color := clBtnFace;
  FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);

  DrawY := DeviceSeparate;

  for D := 0 to FDeviceList.Count - 1 do begin
    Device := FDeviceList.Get(D);

    DeviceH := DeviceHeight + Device.Count * (SensorHeight + SensorSeparate);

    DeviceDraw := (DrawY < sbScroll.Position + pbMain.Height) or (DrawY + DeviceH > sbScroll.Position) or ((DrawY < sbScroll.Position) and (DrawY + DeviceH > sbScroll.Position + pbMain.Height));

    if DeviceDraw then begin
      FBuffer.Canvas.Brush.Style := bsSolid;
      FBuffer.Canvas.Brush.Color := RGBToColor(250, 250, 250);

      if Device.My then begin
        FBuffer.Canvas.Pen.Color := RGBToColor(0, 180, 0);
      end else begin
        FBuffer.Canvas.Pen.Color := clSilver;
      end;

      FBuffer.Canvas.Rectangle(10, DrawY - sbScroll.Position, pbMain.Width - 10, DrawY - sbScroll.Position + DeviceH);

      FBuffer.Canvas.Brush.Style := bsClear;

      FBuffer.Canvas.Font.Height := 18;

      Str := DateToStr(Device.Time);

      if (FmX > 15) and (FmX < pbMain.Width - 15 - FBuffer.Canvas.TextWidth(Str) - 5) and (FmY > DrawY - sbScroll.Position) and (FmY < DrawY - sbScroll.Position + DeviceHeight) then begin
        Cur         := crHandPoint;

        FSelect     := Device.ID;
        FSelectMode := smLocation;
      end;

      if (FSelectMode = smLocation) and (FSelect = Device.ID) then begin
        FBuffer.Canvas.Font.Color := RGBToColor(0, 100, 255);
      end else begin
        FBuffer.Canvas.Font.Color := clGray;
      end;

      FBuffer.Canvas.TextRect(Rect(15, DrawY - sbScroll.Position, pbMain.Width - 15 - FBuffer.Canvas.TextWidth(Str) - 5, DrawY - sbScroll.Position + DeviceHeight), 15, DrawY - sbScroll.Position + 1, Device.Location);

      FBuffer.Canvas.Font.Height := 12;

      FBuffer.Canvas.TextRect(Rect(15, DrawY - sbScroll.Position, pbMain.Width - 15, DrawY - sbScroll.Position + DeviceHeight), 15, DrawY - sbScroll.Position + 15, Format('%.0f км', [Device.Distance]));

      FBuffer.Canvas.Font.Color := clGray;
      FBuffer.Canvas.Font.Height := 18;

      FillChar(Style, SizeOf(Style), 0);
      Style.Alignment := taRightJustify;

      FBuffer.Canvas.TextRect(Rect(15, DrawY - sbScroll.Position, pbMain.Width - 15, DrawY - sbScroll.Position + DeviceHeight), 15, DrawY - sbScroll.Position + 6, Str, Style);
    end;

    DrawY := DrawY + DeviceHeight;

    for S := 0 to Device.Count - 1 do begin
      Sensor := Device.Get(S);

      if DeviceDraw then begin
        FBuffer.Canvas.Brush.Style := bsSolid;
        FBuffer.Canvas.Brush.Color := clWhite;
        FBuffer.Canvas.Pen.Color := clSilver;

        FBuffer.Canvas.Rectangle(15, DrawY - sbScroll.Position, pbMain.Width - 15, DrawY - sbScroll.Position + SensorHeight);

        FBuffer.Canvas.Brush.Style := bsClear;

        FBuffer.Canvas.Font.Height := 18;
        FBuffer.Canvas.Font.Color := clBlack;

        FBuffer.Canvas.TextRect(Rect(20, DrawY - sbScroll.Position + 5, pbMain.Width - 20, DrawY - sbScroll.Position + SensorHeight - 5), 20, DrawY - sbScroll.Position + 5, Sensor.Name);

        FBuffer.Canvas.Brush.Style := bsSolid;
        FBuffer.Canvas.Font.Height := 14;

        FillChar(Style, SizeOf(Style), 0);
        Style.Alignment := taCenter;

        if (FmX > 20) and (FmX < 60) and (FmY > DrawY - sbScroll.Position + 28) and (FmY < DrawY - sbScroll.Position + 45) then begin
          Cur         := crHandPoint;

          FSelect     := Sensor.ID;
          FSelectMode := smTray;
        end;

        if stTray in Sensor.Tags then begin
          FBuffer.Canvas.Pen.Color   := RGBToColor(0, 100, 255);
          FBuffer.Canvas.Brush.Color := RGBToColor(0, 100, 255);
          FBuffer.Canvas.Font.Color  := clWhite;
        end else if (FSelectMode = smTray) and (FSelect = Sensor.ID) then begin
          FBuffer.Canvas.Pen.Color   := RGBToColor(0, 100, 255);
          FBuffer.Canvas.Brush.Color := RGBToColor(204, 224, 255);
          FBuffer.Canvas.Font.Color  := RGBToColor(0, 100, 255);
        end else begin
          FBuffer.Canvas.Pen.Color   := RGBToColor(120, 120, 120);
          FBuffer.Canvas.Brush.Color := clWhite;
          FBuffer.Canvas.Font.Color  := RGBToColor(0, 100, 255);
        end;

        FBuffer.Canvas.Rectangle(Rect(20, DrawY - sbScroll.Position + 28, 20 + 30 + 10, DrawY - sbScroll.Position + 45));
        FBuffer.Canvas.TextRect(Rect(20, DrawY - sbScroll.Position + 28, 20 + 30 + 10, DrawY - sbScroll.Position + 45), 20, DrawY - sbScroll.Position + 30, 'Трей', Style);

        if (FmX > 65) and (FmX < 125) and (FmY > DrawY - sbScroll.Position + 28) and (FmY < DrawY - sbScroll.Position + 45) then begin
          Cur         := crHandPoint;

          FSelect     := Sensor.ID;
          FSelectMode := smHide;
        end;

        if stHide in Sensor.Tags then begin
          FBuffer.Canvas.Pen.Color   := RGBToColor(220, 50, 0);
          FBuffer.Canvas.Brush.Color := RGBToColor(220, 50, 0);
          FBuffer.Canvas.Font.Color  := clWhite;
        end else if (FSelectMode = smHide) and (FSelect = Sensor.ID) then begin
          FBuffer.Canvas.Pen.Color   := RGBToColor(220, 50, 0);
          FBuffer.Canvas.Brush.Color := RGBToColor(255, 220, 206);
          FBuffer.Canvas.Font.Color  := RGBToColor(220, 50, 0);
        end else begin
          FBuffer.Canvas.Pen.Color   := RGBToColor(120, 120, 120);
          FBuffer.Canvas.Brush.Color := clWhite;
          FBuffer.Canvas.Font.Color  := RGBToColor(220, 50, 0);
        end;

        FBuffer.Canvas.Rectangle(Rect(65, DrawY - sbScroll.Position + 28, 125, DrawY - sbScroll.Position + 45));
        FBuffer.Canvas.TextRect(Rect(65, DrawY - sbScroll.Position + 28, 125, DrawY - sbScroll.Position + 45), 65, DrawY - sbScroll.Position + 30, 'Скрыть', Style);

        FBuffer.Canvas.Font.Height := 44;

        case Sensor.Mode of
          smTemp: begin
            if Sensor.Value > 40 then begin
              FBuffer.Canvas.Font.Color := RGBToColor(255, 50, 0);
            end else if Sensor.Value < -25 then begin
              FBuffer.Canvas.Font.Color := RGBToColor(0, 50, 255);
            end else begin
              FBuffer.Canvas.Font.Color := AngleColor(RGBToColor(0, 50, 255), RGBToColor(255, 50, 0), 65, Round(Sensor.Value + 25));
            end;

            if FOption.Get('tempValue', 0).AsInteger = 0 then begin
              Str := ValueToStr(Sensor.Value) + ' °C';
            end else begin
              Str := ValueToStr(CelToFar(Sensor.Value)) + ' °F';
            end;
          end;
          smWet: begin
            FBuffer.Canvas.Font.Color := RGBToColor(0, 50, 150);

            Str := ValueToStr(Sensor.Value) + ' %';
          end;
          smPressure: begin
            FBuffer.Canvas.Font.Color := clBlack;

            Str := ValueToStr(Sensor.Value) + ' мм';
          end;
          smWind: begin
            FBuffer.Canvas.Font.Color := RGBToColor(50, 150, 200);

            if FOption.Get('windValue', 0).AsInteger = 0 then begin
              Str := ValueToStr(Sensor.Value) + ' м/с';
            end else begin
              Str := ValueToStr(MSToKmH(Sensor.Value)) + ' км/ч';
            end;
          end;
          smAzimut: begin
            FBuffer.Canvas.Font.Color := RGBToColor(0, 100, 0);

            Str := ValueToStr(Sensor.Value) + '° ' + GrToNapr(Sensor.Value, 1);
          end;
          smElectricity: begin
            FBuffer.Canvas.Font.Color := RGBToColor(150, 100, 0);

            Str := ValueToStr(Sensor.Value) + ' Вт';
          end;
          smUnknown: begin
            FBuffer.Canvas.Font.Color := clGray;

            Str := ValueToStr(Sensor.Value) + ' ?';
          end;
        end;

        FillChar(Style, SizeOf(Style), 0);
        Style.Alignment := taRightJustify;

        FBuffer.Canvas.TextRect(Rect(20, DrawY - sbScroll.Position + 5, pbMain.Width - 20, DrawY - sbScroll.Position + SensorHeight - 5), 20, DrawY - sbScroll.Position + 5, Str, Style);
      end;

      DrawY := DrawY + SensorHeight + SensorSeparate;
    end;

    DrawY := DrawY + DeviceSeparate;
  end;

  pbMain.Cursor := Cur;

  pbMain.Repaint;
end;

procedure TfrmMain.TrayPrepare;
var
  S, D: Integer;
  Device: TDevice;
  Sensor: TSensor;
  TrayList: TDeviceList;
  TrayIcon: TTrayIcon;
begin
  while FTrayList.Count > 0 do begin
    TTrayIcon(FTrayList.Items[0]).Free;

    FTrayList.Delete(0);
  end;

  TrayList := FDeviceControl.Select(tAny, tAny, tTrue, tAny, dsDistance, CSensorFilterAll);

  for D := 0 to TrayList.Count - 1 do begin
    Device := TrayList.Get(D);

    for S := 0 to Device.Count - 1 do begin
      Sensor := Device.Get(S);

      TrayIcon := TTrayIcon.Create(nil);
      TrayIcon.Tag       := Sensor.ID;
      TrayIcon.PopUpMenu := TrayPopup;
      TrayIcon.OnClick   := @actShowExecute;
      TrayIcon.Show;

      FTrayList.Add(TrayIcon);
    end;
  end;

  TrayList.Free;

  Tray.Visible := FTrayList.Count = 0;

  TrayUpdate;
end;

procedure TfrmMain.TrayUpdate;
var
  I: Integer;
  Sensor: TSensor;
  HintText, ValueText: String;
  TrayIcon: TTrayIcon;
  Bmp: TBitmap;
  Style: TTextStyle;
begin
  Bmp := TBitmap.Create;
  Bmp.Canvas.AntialiasingMode := amOn;
  Bmp.Width := 16;
  Bmp.Height := 16;

  for I := 0 to FTrayList.Count - 1 do begin
    TrayIcon := TTrayIcon(FTrayList.Items[I]);

    Sensor := FDeviceControl.FindSensor(TrayIcon.Tag);

    if Sensor = nil then begin
      Continue;
    end;

    HintText := Sensor.Device.Location + #13#10;

    if Length(Sensor.Name) > 0 then begin
      HintText := HintText + Sensor.Name + #13#10;
    end;

    case Sensor.Mode of
      smTemp: begin
        if Sensor.Value > 40 then begin
          Bmp.Canvas.Brush.Color := RGBToColor(255, 50, 0);
        end else if Sensor.Value < -25 then begin
          Bmp.Canvas.Brush.Color := RGBToColor(0, 50, 255);
        end else begin
          Bmp.Canvas.Brush.Color := AngleColor(RGBToColor(0, 50, 255), RGBToColor(255, 50, 0), 65, Round(Sensor.Value + 25));
        end;

        if FOption.Get('tempValue', 0).AsInteger = 0 then begin
          ValueText := Format('%.0f', [Sensor.Value]);
          HintText  := HintText + ValueToStr(Sensor.Value) + ' °C';
        end else begin
          ValueText := Format('%.0f', [CelToFar(Sensor.Value)]);
          HintText  := HintText + ValueToStr(CelToFar(Sensor.Value)) + ' °F';
        end;
      end;
      smWet: begin
        Bmp.Canvas.Brush.Color := RGBToColor(0, 0, 80);

        ValueText := Format('%.0f', [Sensor.Value]);
        HintText  := HintText + ValueToStr(Sensor.Value) + ' %';
      end;
      smPressure: begin
        Bmp.Canvas.Brush.Color := RGBToColor(0, 80, 0);

        ValueText := Format('%.0f', [Sensor.Value]);
        HintText  := HintText + ValueToStr(Sensor.Value) + ' мм';
      end;
      smWind: begin
        Bmp.Canvas.Brush.Color := RGBToColor(50, 150, 200);

        if FOption.Get('windValue', 0).AsInteger = 0 then begin
          ValueText := Format('%.0f', [Sensor.Value]);
          HintText  := HintText + ValueToStr(Sensor.Value) + ' м/с';
        end else begin
          ValueText := Format('%.0f', [MSToKmH(Sensor.Value)]);
          HintText  := HintText + ValueToStr(MSToKmH(Sensor.Value)) + ' км/ч';
        end;
      end;
      smAzimut: begin
        Bmp.Canvas.Brush.Color := RGBToColor(0, 100, 0);

        ValueText := GrToNapr(Sensor.Value, 0);
        HintText  := HintText + ValueToStr(Sensor.Value) + '° "' + GrToNapr(Sensor.Value, 2) + '"';
      end;
      smElectricity: begin
        Bmp.Canvas.Brush.Color := RGBToColor(150, 100, 0);

        ValueText := Format('%.0f', [Sensor.Value]);
        HintText  := HintText + ValueToStr(Sensor.Value) + ' Вт';
      end;
      smUnknown: begin
        Bmp.Canvas.Brush.Color := clBlack;

        ValueText := Format('%.0f', [Sensor.Value]);
        HintText  := HintText + ValueToStr(Sensor.Value) + ' ?';
      end;
    end;

    HintText := HintText + ' в ' + DateToStr(Sensor.Time);

    FillChar(Style, SizeOf(TTextStyle), 0);
    Style.Alignment := taCenter;

    Bmp.Canvas.FillRect(0, 0, 16, 16);

    Bmp.Canvas.Font.Name  := 'Trebuchet MS';
    Bmp.Canvas.Font.Color := clWhite;

    if Length(ValueText) = 1 then begin
      Bmp.Canvas.Font.Size  := 10;
      Bmp.Canvas.TextRect(Rect(0, 0, 16, 16), 0, -2, ValueText, Style);
    end else if Length(ValueText) = 2 then begin
      Bmp.Canvas.Font.Size  := 8;
      Bmp.Canvas.TextRect(Rect(0, 0, 16, 16), 0, 0, ValueText, Style);
    end else begin
      Bmp.Canvas.Font.Size  := 7;
      Bmp.Canvas.TextRect(Rect(0, 0, 16, 16), 0, 0, ValueText, Style);
    end;

    TrayIcon.Icon.Assign(Bmp);
    TrayIcon.Hint := HintText;
  end;

  Bmp.Free;
end;

procedure TfrmMain.TrayNotify(ACaption, AMessage: String; ATimeOut: Integer = 5000);
begin
  if Tray.Visible then begin
    Tray.BalloonHint    := AMessage;
    Tray.BalloonTimeout := ATimeOut;
    Tray.BalloonTitle   := ACaption;
    Tray.ShowBalloonHint;
  end else if FTrayList.Count > 0 then begin
    TTrayIcon(FTrayList.Items[0]).BalloonHint    := AMessage;
    TTrayIcon(FTrayList.Items[0]).BalloonTimeout := ATimeOut;
    TTrayIcon(FTrayList.Items[0]).BalloonTitle   := ACaption;
    TTrayIcon(FTrayList.Items[0]).ShowBalloonHint;
  end;
end;

function TfrmMain.CelToFar(ACel: Single): Single;
begin
  Result := ACel * 9 / 5 + 32;
end;

function TfrmMain.MSToKmH(AMS: Single): Single;
begin
  Result := AMS * 3.6;
end;

function TfrmMain.GrToNapr(AGr: Single; AMode: Byte): String;
const
  ModeRange: array [0..16] of array [0..2] of Single =
    (
      (0,      11.25,  0),
      (11.25,  33.75,  1),
      (33.75,  56.25,  2),
      (56.25,  78.75,  3),
      (78.75,  101.25, 4),
      (101.25, 123.75, 5),
      (123.75, 146.25, 6),
      (146.25, 168.75, 7),
      (168.75, 191.25, 8),
      (191.25, 213.75, 9),
      (213.75, 236.25, 10),
      (236.25, 258.75, 11),
      (258.75, 281.25, 12),
      (281.25, 303.75, 13),
      (303.75, 326.25, 14),
      (326.25, 348.75, 15),
      (348.75, 360,    0)
    );
  ModeText: array [0..2] of array [0..15] of String =
    (
      ('с', 'ссв', 'св', 'всв', 'в', 'вюв', 'юв', 'ююв', 'ю', 'ююз', 'юз', 'зюз', 'з', 'зсз', 'сз', 'ссз'),
      ('Север', 'С-С-В', 'С-В', 'В-С-В', 'Восток', 'В-Ю-В', 'Ю-В', 'Ю-Ю-В', 'Юг', 'Ю-Ю-З', 'Ю-З', 'З-Ю-З', 'Запад', 'З-С-З', 'С-З', 'С-С-З'),
      ('Север', 'Северо-Северо-Восток', 'Северо-Восток', 'Восток-Северо-Восток', 'Восток', 'Восток-Юго-Восток', 'Юго-Восток', 'Юго-Юго-Восток', 'Юг', 'Юго-Юго-Запад', 'Юго-Запад', 'Запад-Юго-Запад', 'Запад', 'Запад-Северо-Запад', 'Северо-Запад', 'Северо-Северо-Запад')
    );
var
  I: Integer;
begin
  AGr := Round(AGr) mod 360;

  Result := '?';

  for I := 0 to 16 do begin
    if (AGr >= ModeRange[I][0]) and (AGr < ModeRange[I][1]) then begin
      Result := ModeText[AMode][Round(ModeRange[I][2])];

      Break;
    end;
  end;
end;

function TfrmMain.ValueToStr(AValue: Single): String;
var
  Ch: String;
begin
  if FOption.Get('shortValue', True).AsBoolean then begin
    if (AValue < 1) and (AValue > -1) then begin
      Result := Format('%.2f', [AValue]);
    end else if (AValue < 10) and (AValue > -10) then begin
      Result := Format('%.1f', [AValue]);
    end else begin
      Result := Format('%.0f', [AValue]);
    end;
  end else begin
    Result := Format('%.2f', [AValue]);
  end;

  if Pos(',', Result) > 0 then begin
    while True do begin
      Ch := Copy(Result, Length(Result), 1);

      if (Ch <> '0') and (Ch <> ',') then begin
        Break;
      end;

      Result := Copy(Result, 1, Length(Result) - 1);

      if Ch = ',' then begin
        Break;
      end;
    end;
  end;
end;

function TfrmMain.DateToStr(AValue: TDateTime): String;
begin
  if FormatDateTime('DDMMYYYY', AValue) <> FormatDateTime('DDMMYYYY', Now) then begin
    Result := FormatDateTime('DD.MM.YYYY hh:mm', AValue);
  end else begin
    Result := FormatDateTime('hh:mm', AValue);
  end;
end;

end.
