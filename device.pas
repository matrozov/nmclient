unit Device;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  mJSON,
  MD5hash,
  Request,
  LCLIntf,
  Time;

type
  TDeviceControl = class;
  TDeviceList = class;
  TDevice = class;
  TSensor = class;

  { TDeviceList }

  TDeviceListSort = (dsDistance, dsTime);

  TDeviceList = class
  private
    FDeviceControl: TDeviceControl;
    FDevices: TList;
  private
    procedure Sort(ASort: TDeviceListSort);

    function  Add: TDevice; overload;
    function  Add(ADevice: TDevice): TDevice; overload;
    function  Add(AjsonDevice: TmJSONList): TDevice; overload;

    procedure Delete(AID: Integer); overload;
    procedure Delete(ADevice: TDevice); overload;

    procedure Clear;
  public
    constructor Create(ADeviceControl: TDeviceControl);
    destructor Destroy; override;

    function  Get(AIndex: Integer): TDevice;

    function  Find(AID: Integer): TDevice;

    function  Count: Integer;

    function  FindSensor(AID: Integer): TSensor;
  end;

  { TDevice }

  TDevice = class
  private
    FDeviceList: TDeviceList;

    FSensors:  TList;

    FID:       Integer;
    FLocation: String;
    FDistance: Single;
    FMy:       Boolean;
    FTime:     TDateTime;
  private
    function  Clone(ADeviceList: TDeviceList): TDevice;

    function  Add: TSensor; overload;
    function  Add(ASensor: TSensor): TSensor; overload;
    function  Add(AjsonSensor: TmJSONList): TSensor; overload;

    procedure Delete(AID: Integer); overload;
    procedure Delete(ASensor: TSensor); overload;

    procedure Clear;
  public
    constructor Create(ADeviceList: TDeviceList);
    destructor Destroy; override;

    function  Get(AIndex: Integer): TSensor;

    function  Find(AID: Integer): TSensor;

    function  Count: Integer;
  public
    property  DeviceList: TDeviceList read FDeviceList;

    property  ID:         Integer     read FID;
    property  Location:   String      read FLocation;
    property  Distance:   Single      read FDistance;
    property  My:         Boolean     read FMy;
    property  Time:       TDateTime   read FTime;
  end;

  TSensorMode   = (smUnknown, smTemp, smWet, smPressure, smWind, smAzimut, smElectricity);
  TSensorFilter = set of TSensorMode;

  { TSensor }

  TSensorTag = (stTray, stHide);
  TSensorTags = set of TSensorTag;

  TSensor = class
  private
    FDevice: TDevice;

    FID:    Integer;
    FMode:  TSensorMode;
    FPub:   Boolean;
    FName:  String;
    FValue: Single;
    FTime:  TDateTime;
  private
    function  Clone(ADevice: TDevice): TSensor;

    function  GetTags: TSensorTags;
    procedure SetTags(AValue: TSensorTags);

    procedure Update(AjsonSensor: TmJSONList);
  public
    constructor Create(ADevice: TDevice);
    destructor Destroy; override;
  public
    property  Device: TDevice     read FDevice;

    property  ID:     Integer     read FID;
    property  Mode:   TSensorMode read FMode;
    property  Pub:    Boolean     read FPub;
    property  Name:   String      read FName;
    property  Value:  Single      read FValue;
    property  Time:   TDateTime   read FTime;

    property  Tags:   TSensorTags read GetTags write SetTags;
  end;

  { TDeviceControl }

  TSelect = (tAny, tTrue, tFalse);

  TDeviceControl = class(TDeviceList)
  private
    FOption:  TmJSON;
  private
    function  Request(AJson: TmJSON): Boolean;

    procedure AddDevices(AjsonDevices: TmJSONArray);
  public
    constructor Create(AOption: TmJSON);
    destructor Destroy; override;

    function  Select(APublic: TSelect; AMy: TSelect; ATray: TSelect; AHide: TSelect; ASort: TDeviceListSort; ASensorFilter: TSensorFilter): TDeviceList;

    function  UpdateList: Boolean;
    function  UpdateInfoAll: Boolean;
    function  UpdateInfoSubscribe: Boolean;

    function  CheckUpdate(AVersion: String): String;
    function  DownloadUpdate(AMD5: String): Boolean;

    function  Location(ALocation: String): String;

    function  CheckLogin: String;
    function  Login(ALogin, APassword: String): String;
    function  Logout: String;
  end;

const
  {$ifdef WINDOWS}API_KEY: String = '60JKxYw0fCj2g';{$endif}
     {$ifdef UNIX}API_KEY: String = '34G7Fqj3lzwrU';{$endif}
  CSensorFilterAll: TSensorFilter = [smUnknown, smTemp, smWet, smPressure, smWind, smAzimut, smElectricity];

implementation

uses
  Main;

{ TDeviceList }

function DeviceListSort_Distance(Item1, Item2: Pointer): Integer;
var
  Dev1: TDevice absolute Item1;
  Dev2: TDevice absolute Item2;
begin
  if Dev1.Distance > Dev2.Distance then begin
    Result := 1;
  end else if Dev1.Distance = Dev2.Distance then begin
    Result := 0;
  end else begin
    Result := -1;
  end;
end;

function DeviceListSort_Time(Item1, Item2: Pointer): Integer;
var
  Dev1: TDevice absolute Item1;
  Dev2: TDevice absolute Item2;
begin
  if Dev1.Time < Dev2.Time then begin
    Result := 1;
  end else if Dev1.Time = Dev2.Time then begin
    Result := 0;
  end else begin
    Result := -1;
  end;
end;

constructor TDeviceList.Create(ADeviceControl: TDeviceControl);
begin
  FDeviceControl := ADeviceControl;

  FDevices := TList.Create;
end;

destructor TDeviceList.Destroy;
begin
  Clear;

  FDevices.Free;

  inherited Destroy;
end;

procedure TDeviceList.Sort(ASort: TDeviceListSort);
begin
  case ASort of
    dsDistance: FDevices.Sort(@DeviceListSort_Distance);
    dsTime:     FDevices.Sort(@DeviceListSort_Time);
  end;
end;

function TDeviceList.Add: TDevice;
begin
  Result := Add(TDevice.Create(Self));
end;

function TDeviceList.Add(ADevice: TDevice): TDevice;
begin
  Result := ADevice;

  FDevices.Add(Result);
end;

function TDeviceList.Add(AjsonDevice: TmJSONList): TDevice;
var
  jsonSensors: TmJSONArray;
  I: Integer;
begin
  Result := Add();

  Result.FID       := AjsonDevice.Get('id', -1).AsInteger;
  Result.FLocation := AjsonDevice.Get('location', '').AsString;
  Result.FDistance := AjsonDevice.Get('distance', 0).AsSingle;
  Result.FMy       := AjsonDevice.Get('my', False).AsBoolean;
  Result.FTime     := UnixToLocalTime(AjsonDevice.Get('time', 0).AsInteger);

  jsonSensors := TmJSONArray(AjsonDevice.Get('sensors'));

  if jsonSensors <> nil then begin
    for I := 0 to jsonSensors.Count - 1 do begin
      Result.Add(TmJSONList(jsonSensors.Get(I)));
    end;
  end;
end;

function TDeviceList.Find(AID: Integer): TDevice;
var
  D: Integer;
begin
  Result := nil;

  for D := 0 to Count - 1 do begin
    if Get(D).ID = AID then begin
      Result := Get(D);

      Break;
    end;
  end;
end;

function TDeviceList.Get(AIndex: Integer): TDevice;
begin
  Result := nil;

  if (AIndex >= 0) and (AIndex < FDevices.Count) then begin
    Result := TDevice(FDevices.Items[AIndex]);
  end;
end;

procedure TDeviceList.Delete(AID: Integer);
begin
  Delete(Get(AID));
end;

procedure TDeviceList.Delete(ADevice: TDevice);
var
  D: Integer;
begin
  D := FDevices.IndexOf(ADevice);

  if D <> -1 then begin
    FDevices.Delete(D);
  end;
end;

function TDeviceList.Count: Integer;
begin
  Result := FDevices.Count;
end;

procedure TDeviceList.Clear;
var
  Device: TDevice;
begin
  while FDevices.Count > 0 do begin
    Device := TDevice(FDevices.Items[0]);
    Device.Free;
  end;
end;

function TDeviceList.FindSensor(AID: Integer): TSensor;
var
  D: Integer;
  Device: TDevice;
begin
  Result := nil;

  for D := 0 to FDevices.Count - 1 do begin
    Device := TDevice(FDevices.Items[D]);

    Result := Device.Find(AID);

    if Result <> nil then begin
      Break;
    end;
  end;
end;

{ TDevice }

function TDevice.Clone(ADeviceList: TDeviceList): TDevice;
begin
  Result := TDevice.Create(ADeviceList);
  Result.FID       := FID;
  Result.FLocation := FLocation;
  Result.FDistance := FDistance;
  Result.FMy       := FMy;
  Result.FTime     := FTime;
end;

constructor TDevice.Create(ADeviceList: TDeviceList);
begin
  FDeviceList := ADeviceList;

  FSensors := TList.Create;
end;

destructor TDevice.Destroy;
begin
  FDeviceList.Delete(Self);

  Clear;

  FSensors.Free;

  inherited Destroy;
end;

function TDevice.Add: TSensor;
begin
  Result := Add(TSensor.Create(Self));
end;

function TDevice.Add(ASensor: TSensor): TSensor;
begin
  Result := ASensor;

  FSensors.Add(Result);
end;

function TDevice.Add(AjsonSensor: TmJSONList): TSensor;
begin
  Result := Add();

  Result.FID := AjsonSensor.Get('id', -1).AsInteger;

  case AjsonSensor.Get('type', -1).AsInteger of
    1:   Result.FMode := smTemp;
    2:   Result.FMode := smWet;
    3:   Result.FMode := smPressure;
    4:   Result.FMode := smWind;
    5:   Result.FMode := smAzimut;
    11:  Result.FMode := smElectricity;
    else Result.FMode := smUnknown;
  end;

  Result.FName  := AjsonSensor.Get('name', '').AsString;
  Result.FPub   := AjsonSensor.Get('pub', False).AsBoolean;
  Result.FValue := AjsonSensor.Get('value', 0).AsSingle;
  Result.FTime  := UnixToLocalTime(AjsonSensor.Get('time', 0).AsInteger);
end;

function TDevice.Find(AID: Integer): TSensor;
var
  S: Integer;
begin
  Result := nil;

  for S := 0 to FSensors.Count - 1 do begin
    if TSensor(FSensors.Items[S]).ID = AID then begin
      Result := TSensor(FSensors.Items[S]);

      Break;
    end;
  end;
end;

function TDevice.Get(AIndex: Integer): TSensor;
begin
  Result := nil;

  if (AIndex >= 0) and (AIndex < FSensors.Count) then begin
    Result := TSensor(FSensors.Items[AIndex]);
  end;
end;

procedure TDevice.Delete(AID: Integer);
begin
  Delete(Get(AID));
end;

procedure TDevice.Delete(ASensor: TSensor);
var
  S: Integer;
begin
  S := FSensors.IndexOf(ASensor);

  if S <> -1 then begin
    FSensors.Delete(S);
  end;
end;

function TDevice.Count: Integer;
begin
  Result := FSensors.Count;
end;

procedure TDevice.Clear;
var
  Sensor: TSensor;
begin
  while FSensors.Count > 0 do begin
    Sensor := TSensor(FSensors.Items[0]);
    Sensor.Free;
  end;
end;

{ TSensor }

function TSensor.Clone(ADevice: TDevice): TSensor;
begin
  Result := TSensor.Create(ADevice);
  Result.FID    := FID;
  Result.FMode  := FMode;
  Result.FName  := FName;
  Result.FPub   := FPub;
  Result.FValue := FValue;
  Result.FTime  := FTime;
end;

function TSensor.GetTags: TSensorTags;
var
  JsonTags: TmJSONList;
  JsonSensorTags: TmJSONList;
  DeviceControl: TDeviceControl;
begin
  Result := [];

  if FDevice.FDeviceList.FDeviceControl <> nil then begin
    DeviceControl := FDevice.FDeviceList.FDeviceControl;
  end else begin
    DeviceControl := TDeviceControl(FDevice.FDeviceList);
  end;

  JsonTags := TmJSONList(DeviceControl.FOption.Get('tags'));

  if JsonTags = nil then begin
    Exit;
  end;

  JsonSensorTags := TmJSONList(JsonTags.Get(IntToStr(FID)));

  if JsonSensorTags = nil then begin
    Exit;
  end;

  if JsonSensorTags.Get('tray', False).AsBoolean then begin
    Result := Result + [stTray];
  end;

  if JsonSensorTags.Get('hide', False).AsBoolean then begin
    Result := Result + [stHide];
  end;
end;

procedure TSensor.SetTags(AValue: TSensorTags);
var
  JsonTags: TmJSONList;
  JsonSensorTags: TmJSONList;
  DeviceControl: TDeviceControl;
begin
  if FDevice.FDeviceList.FDeviceControl <> nil then begin
    DeviceControl := FDevice.FDeviceList.FDeviceControl;
  end else begin
    DeviceControl := TDeviceControl(FDevice.FDeviceList);
  end;

  JsonTags := TmJSONList(DeviceControl.FOption.Get('tags'));

  if JsonTags = nil then begin
    JsonTags := DeviceControl.FOption.AddObject('tags');
  end;

  JsonSensorTags := TmJSONList(JsonTags.Get(IntToStr(FID)));

  if JsonSensorTags = nil then begin
    JsonSensorTags := JsonTags.AddObject(IntToStr(FID));
  end;

  if stTray in AValue then begin
    JsonSensorTags.Get('tray', True).AsBoolean := True;
  end else begin
    JsonSensorTags.Delete('tray');
  end;

  if stHide in AValue then begin
    JsonSensorTags.Get('hide', True).AsBoolean := True;
  end else begin
    JsonSensorTags.Delete('hide');
  end;
end;

constructor TSensor.Create(ADevice: TDevice);
begin
  FDevice := ADevice;
end;

destructor TSensor.Destroy;
begin
  FDevice.Delete(Self);

  inherited Destroy;
end;

procedure TSensor.Update(AjsonSensor: TmJSONList);
begin
  FValue := AjsonSensor.Get('value', '').AsSingle;
  FTime  := UnixToLocalTime(AjsonSensor.Get('time', 0).AsInteger);

  if FTime > FDevice.FTime then begin
    FDevice.FTime := FTime;
  end;
end;

{ TDeviceControl }

constructor TDeviceControl.Create(AOption: TmJSON);
begin
  inherited Create(nil);

  FOption := AOption;
end;

destructor TDeviceControl.Destroy;
begin
  inherited Destroy;
end;

function TDeviceControl.Select(APublic: TSelect; AMy: TSelect; ATray: TSelect; AHide: TSelect; ASort: TDeviceListSort; ASensorFilter: TSensorFilter): TDeviceList;
var
  D, S: Integer;
  Device, newDevice: TDevice;
  Sensor: TSensor;
  CanSelect: Boolean;
begin
  Result := TDeviceList.Create(Self);

  for D := 0 to Count - 1 do begin
    Device := Get(D);

    newDevice := Device.Clone(Result);

    for S := 0 to Device.Count - 1 do begin
      Sensor := Device.Get(S);

      if not (Sensor.Mode in ASensorFilter) then begin
        Continue;
      end;

      if (APublic <> tAny) or (AMy <> tAny) or (ATray <> tAny) or (AHide <> tAny) then begin
        CanSelect := True;

        if APublic = tTrue then begin
          CanSelect := CanSelect and (Sensor.Pub = True);
        end else if APublic = tFalse then begin
          CanSelect := CanSelect and (Sensor.Pub = False);
        end;

        if AMy = tTrue then begin
          CanSelect := CanSelect and (Device.My = True);
        end else if AMy = tFalse then begin
          CanSelect := CanSelect and (Device.My = False);
        end;

        if ATray = tTrue then begin
          CanSelect := CanSelect and (stTray in Sensor.Tags);
        end else if ATray = tFalse then begin
          CanSelect := CanSelect and (not (stTray in Sensor.Tags));
        end;

        if AHide = tTrue then begin
          CanSelect := CanSelect and (stHide in Sensor.Tags);
        end else if AHide = tFalse then begin
          CanSelect := CanSelect and (not (stHide in Sensor.Tags));
        end;

        if CanSelect then begin
          newDevice.Add(Sensor.Clone(newDevice));
        end;
      end else begin
        newDevice.Add(Sensor.Clone(newDevice));
      end;
    end;

    if newDevice.Count > 0 then begin
      Result.Add(newDevice);
    end;
  end;

  Result.Sort(ASort);
end;

function TDeviceControl.Request(AJson: TmJSON): Boolean;
var
  Req: TRequest;
  JsonProxy: TmJSONList;
begin
  Req := TRequest.Create;

  Req.Method  := rmPost;
  Req.Host    := 'narodmon.ru';
  Req.URI     := 'client.php';
  Req.Request := AJson.SaveToString;

  JsonProxy    := TmJSONList(FOption.Get('proxy'));

  if JsonProxy <> nil then begin
    Req.Proxy := JsonProxy.Get('enabled', False).AsBoolean;

    if Req.Proxy then begin
      Req.ProxyHost := JsonProxy.Get('host', '').AsString;
      Req.ProxyPort := JsonProxy.Get('port', 8080).AsInteger;

      Req.ProxyAuth := JsonProxy.Get('auth', False).AsBoolean;

      if Req.ProxyAuth then begin
        Req.ProxyLogin    := JsonProxy.Get('login', '').AsString;
        Req.ProxyPassword := JsonProxy.Get('password', '').AsString;
      end;
    end;
  end else begin
    Req.Proxy := False;
  end;

  Result := Req.Execute;

  AJson.Clear;

  if Result then begin
    Result := AJson.LoadFromString(Req.Response);

    if not Result then begin
      frmMain.TrayNotify('Error', 'Parse error');
    end;
  end;

  Req.Free;
end;

procedure TDeviceControl.AddDevices(AjsonDevices: TmJSONArray);
var
  I: Integer;
begin
  if AjsonDevices <> nil then begin
    for I := 0 to AjsonDevices.Count - 1 do begin
      Add(TmJSONList(AjsonDevices.Get(I)));
    end;
  end;
end;

function TDeviceControl.UpdateList: Boolean;
var
  Json: TmJSON;
begin
  frmMain.sbStatus.SimpleText := 'Обновление списка датчиков...';

  Result := False;

  Json := TmJSON.Create;
  Json.Add('cmd',     'sensorList');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);
  Json.Add('radius',  FOption.Get('radius', 1000).AsInteger);

  if Request(Json) then begin
    Clear;

    AddDevices(TmJSONArray(Json.Get('devices')));

    Result := True;
  end;

  Json.Free;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.UpdateInfoAll: Boolean;
var
  D, S: Integer;
  Device: TDevice;
  Sensor: TSensor;
  Json: TmJSON;
  JsonSensors: TmJSONArray;
  JsonSensor: TmJSONList;
begin
  Result := True;

  if Count = 0 then begin
    Exit;
  end;

  frmMain.sbStatus.SimpleText := 'Обновление показаний датчиков...';

  Result := False;

  Json := TmJSON.Create;
  Json.Add('cmd',     'sensorInfo');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);

  JsonSensors := Json.AddArray('sensor');

  for D := 0 to Count - 1 do begin
    Device := Get(D);

    if Device = nil then begin
      Continue;
    end;

    for S := 0 to Device.Count - 1 do begin
      Sensor := Device.Get(S);

      if Sensor = nil then begin
        Continue;
      end;

      JsonSensors.Add('', Sensor.ID);
    end;
  end;

  if Request(Json) then begin
    AddDevices(TmJSONArray(Json.Get('devices')));

    JsonSensors := TmJSONArray(Json.Get('sensors'));

    if JsonSensors <> nil then begin
      for S := 0 to JsonSensors.Count - 1 do begin
        JsonSensor := TmJSONList(JsonSensors.Get(S));

        Sensor := FindSensor(JsonSensor.Get('id', '').AsInteger);

        if Sensor = nil then begin
          Continue;
        end;

        Sensor.Update(JsonSensor);
      end;
    end;

    Result := True;
  end;

  Json.Free;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.UpdateInfoSubscribe: Boolean;
var
  D, S: Integer;
  Device: TDevice;
  Sensor: TSensor;
  SubList: TDeviceList;
  Json: TmJSON;
  JsonSensors: TmJSONArray;
  JsonSensor: TmJSONList;
begin
  Result := True;

  SubList := Select(tAny, tAny, tTrue, tFalse, dsDistance, []);

  if SubList.Count = 0 then begin
    Exit;
  end;

  frmMain.sbStatus.SimpleText := 'Обновление показаний подписанных датчиков...';

  Result := False;

  Json := TmJSON.Create;
  Json.Add('cmd',     'sensorInfo');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);

  JsonSensors := Json.AddArray('sensor');

  for D := 0 to SubList.Count - 1 do begin
    Device := SubList.Get(D);

    for S := 0 to Device.Count - 1 do begin
      Sensor := Device.Get(S);

      JsonSensors.Add('', Sensor.ID);
    end;
  end;

  if Request(Json) then begin
    AddDevices(TmJSONArray(Json.Get('devices')));

    JsonSensors := TmJSONArray(Json.Get('sensors'));

    if JsonSensors <> nil then begin
      for S := 0 to JsonSensors.Count - 1 do begin
        JsonSensor := TmJSONList(JsonSensors.Get(S));

        Sensor := FindSensor(JsonSensor.Get('id', '').AsInteger);

        if Sensor = nil then begin
          Continue;
        end;

        Sensor.Update(JsonSensor);
      end;
    end;

    Result := True;
  end;

  Json.Free;

  SubList.Free;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.CheckUpdate(AVersion: String): String;
var
  Json: TmJSON;
begin
  frmMain.sbStatus.SimpleText := 'Проверка обновлений...';

  Result := '';

  Json := TmJSON.Create;
  Json.Add('cmd',     'version');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);
  Json.Add('version', AVersion);
  Json.Add('fname',   {$ifdef WINDOWS}'nmclient.exe'{$endif}{$ifdef UNIX}'nmclient_linux'{$endif});

  if Request(Json) then begin
    Result := LowerCase(Json.Get('md5', '').AsString);
  end;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.DownloadUpdate(AMD5: String): Boolean;
var
  Req: TRequest;
  JsonProxy: TmJSONList;
  Fle: TFileStream;
  MD5: String;
begin
  frmMain.sbStatus.SimpleText := 'Загрузка обновлений...';

  Req := TRequest.Create;

  Req.Method  := rmPost;
  Req.Host    := 'narodmon.ru';
  Req.URI     := 'client.php?fname=' + {$ifdef WINDOWS}'nmclient.exe'{$endif}{$ifdef UNIX}'nmclient_linux'{$endif};

  JsonProxy    := TmJSONList(FOption.Get('proxy'));

  if JsonProxy <> nil then begin
    Req.Proxy := JsonProxy.Get('enabled', False).AsBoolean;

    if Req.Proxy then begin
      Req.ProxyHost := JsonProxy.Get('host', '').AsString;
      Req.ProxyPort := JsonProxy.Get('port', 8080).AsInteger;

      Req.ProxyAuth := JsonProxy.Get('auth', False).AsBoolean;

      if Req.ProxyAuth then begin
        Req.ProxyLogin    := JsonProxy.Get('login', '').AsString;
        Req.ProxyPassword := JsonProxy.Get('password', '').AsString;
      end;
    end;
  end else begin
    Req.Proxy := False;
  end;

  Result := Req.Execute;

  if Result then begin
    MD5 := MD5hash.MD5(Req.Response);

    Result := MD5 = AMD5;

    if Result then begin
      Fle := TFileStream.Create(ExtractFilePath(Application.ExeName) + 'nmclient.upd', fmCreate);
      Fle.Write(Req.Response[1], Length(Req.Response));
      Fle.Free;
    end;
  end;

  Req.Free;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.Location(ALocation: String): String;
var
  Json: TmJSON;
begin
  frmMain.sbStatus.SimpleText := 'Определение местоположения...';

  Result := '';

  Json := TmJSON.Create;
  Json.Add('cmd',     'location');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);

  if ALocation <> '' then begin
    Json.Add('addr', ALocation);
  end;

  if Request(Json) then begin
    Result := Json.Get('addr', 0).AsString;
  end;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.CheckLogin: String;
var
  Json: TmJSON;
begin
  frmMain.sbStatus.SimpleText := 'Проверка входа...';

  Result := '';

  Json := TmJSON.Create;
  Json.Add('cmd',     'login');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);

  if Request(Json) then begin
    Result := Json.Get('login', '').AsString;
  end;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.Login(ALogin, APassword: String): String;
var
  Json: TmJSON;
begin
  frmMain.sbStatus.SimpleText := 'Вход...';

  Result := '';

  APassword := MD5(LowerCase(FOption.Get('uuid', '').AsString) + MD5(APassword));

  Json := TmJSON.Create;
  Json.Add('cmd',     'login');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);
  Json.Add('login',   ALogin);
  Json.Add('hash',    APassword);

  if Request(Json) then begin
    Result := Json.Get('login', '').AsString;
  end;

  frmMain.sbStatus.SimpleText := '';
end;

function TDeviceControl.Logout: String;
var
  Json: TmJSON;
begin
  frmMain.sbStatus.SimpleText := 'Выход...';

  Result := '';

  Json := TmJSON.Create;
  Json.Add('cmd',     'logout');
  Json.Add('uuid',    LowerCase(FOption.Get('uuid', '').AsString));
  Json.Add('api_key', API_KEY);

  if Request(Json) then begin
    Result := Json.Get('login', '').AsString;
  end;

  frmMain.sbStatus.SimpleText := '';
end;

end.
