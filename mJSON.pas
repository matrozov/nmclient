unit mJSON;

interface

//{$define ZLIB}

uses
  Classes,
  {$ifdef ZLIB}
  ZLibEx,
  {$endif}
  SysUtils;

type
  TmJSONValue = class;

  TmJSONString = class;
  TmJSONInteger = class;
  TmJSONSingle = class;
  TmJSONBoolean = class;

  TmJSONList = class;

  TmJSONArray = class;
  TmJSONObject = class;

  TmJSON = class;

  TmJSONValue = class
  private
    FParent: TmJSONList;

    FKey: String;
  protected
    function  GetAsString: String; virtual;
    function  GetAsInteger: Integer; virtual;
    function  GetAsSingle: Single; virtual;
    function  GetAsBoolean: Boolean; virtual;
    function  GetAsGUID: TGUID;

    procedure SetAsString(AValue: String); virtual;
    procedure SetAsInteger(AValue: Integer); virtual;
    procedure SetAsSingle(AValue: Single); virtual;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsGUID(AValue: TGUID);
  public
    constructor Create(AParent: TmJSONList; AKey: String);

    function  Clone: TmJSONValue; virtual;
  public
    property  Key: String read FKey;

    property  AsString: String read GetAsString write SetAsString;
    property  AsInteger: Integer read GetAsInteger write SetAsInteger;
    property  AsSingle: Single read GetAsSingle write SetAsSingle;
    property  AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property  AsGUID: TGUID read GetAsGUID write SetAsGUID;
  end;

  TmJSONString = class(TmJSONValue)
  private
    FValue: String;
  protected
    function  GetAsString: String; override;
    function  GetAsInteger: Integer; override;
    function  GetAsSingle: Single; override;
    function  GetAsBoolean: Boolean; override;

    procedure SetAsString(AValue: String); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsSingle(AValue: Single); override;
    procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(AParent: TmJSONList; AKey: String; AValue: String);

    function  Clone: TmJSONValue; override;
  end;

  TmJSONInteger = class(TmJSONValue)
  private
    FValue: Integer;
  protected
    function  GetAsString: String; override;
    function  GetAsInteger: Integer; override;
    function  GetAsSingle: Single; override;
    function  GetAsBoolean: Boolean; override;

    procedure SetAsString(AValue: String); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsSingle(AValue: Single); override;
    procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(AParent: TmJSONList; AKey: String; AValue: Integer);

    function  Clone: TmJSONValue; override;
  end;

  TmJSONSingle = class(TmJSONValue)
  private
    FValue: Single;
  protected
    function  GetAsString: String; override;
    function  GetAsInteger: Integer; override;
    function  GetAsSingle: Single; override;
    function  GetAsBoolean: Boolean; override;

    procedure SetAsString(AValue: String); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsSingle(AValue: Single); override;
    procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(AParent: TmJSONList; AKey: String; AValue: Single);

    function Clone: TmJSONValue; override;
  end;

  TmJSONBoolean = class(TmJSONValue)
  private
    FValue: Boolean;
  protected
    function  GetAsString: String; override;
    function  GetAsInteger: Integer; override;
    function  GetAsSingle: Single; override;
    function  GetAsBoolean: Boolean; override;

    procedure SetAsString(AValue: String); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsSingle(AValue: Single); override;
    procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(AParent: TmJSONList; AKey: String; AValue: Boolean);

    function  Clone: TmJSONValue; override;
  end;

  TmJSONList = class(TmJSONValue)
  private
    FList: TList;
  private
    function  FindIndex(AKey: String): Integer;
  protected
    function  GetAsString: String; override;

    procedure SetAsString(AValue: String); override;
  public
    constructor Create(AParent: TmJSONList; AKey: String);
    destructor Destroy; override;

    procedure Add(AKey: String; AValue: String); overload;
    procedure Add(AKey: String; AValue: Integer); overload;
    procedure Add(AKey: String; AValue: Single); overload;
    procedure Add(AKey: String; AValue: Boolean); overload;
    procedure Add(AKey: String; AValue: TGUID); overload;

    procedure Add(ANode: TmJSONValue); overload;

    function  AddArray(AKey: String): TmJSONArray;
    function  AddObject(AKey: String): TmJSONObject;

    procedure Replace(AKey: String; AValue: String); overload;
    procedure Replace(AKey: String; AValue: Integer); overload;
    procedure Replace(AKey: String; AValue: Single); overload;
    procedure Replace(AKey: String; AValue: Boolean); overload;
    procedure Replace(AKey: String; AValue: TGUID); overload;

    function  Get(AIndex: Integer): TmJSONValue; overload;
    function  Get(AKey: String): TmJSONValue; overload;
    function  Get(AKey: String; ADefault: String):  TmJSONValue; overload;
    function  Get(AKey: String; ADefault: Integer): TmJSONValue; overload;
    function  Get(AKey: String; ADefault: Single):  TmJSONValue; overload;
    function  Get(AKey: String; ADefault: Boolean): TmJSONValue; overload;
    function  Get(AKey: String; ADefault: TGUID):   TmJSONValue; overload;

    function  GetList(AIndex: Integer): TmJSONList; overload;
    function  GetList(AKey: String): TmJSONList; overload;

    function  Exists(AKey: String): Boolean;

    procedure Delete(AKey: String);

    function  Count: Integer;

    procedure Clear;
  public
    property  Value[AIndex: Integer]: TmJSONValue read Get;
  end;

  TmJSONArray = class(TmJSONList)
  public
    function  Clone: TmJSONValue; override;
  end;

  TmJSONObject = class(TmJSONList)
  public
    function  Clone: TmJSONValue; override;
  end;

  { TmJSON }

  TmJSON = class(TmJSONObject)
  public
    constructor Create;

    function  SaveToString(ACompress: Boolean = False): AnsiString;
    procedure SaveToStream(AStream: TStream; ACompress: Boolean = False);
    procedure SaveToFile(AFileName: String; ACompress: Boolean = False);

    function  LoadFromString(AString: AnsiString; ACompress: Boolean = False): Boolean;
    function  LoadFromStream(AStream: TStream; ACompress: Boolean = False): Boolean;
    function  LoadFromFile(AFileName: String; ACompress: Boolean = False): Boolean;
  end;

implementation

function Backsplash(AString: String): String;
var
  I: Integer;
begin
  Result := '';

  for I := 1 to Length(AString) do begin
    case AString[I] of
      '"','\','/': Result := Result + '\' + AString[I];
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else begin
        Result := Result + AString[I];
      end;
    end;
  end;
end;

{ TJSONValue }

function TmJSONValue.Clone: TmJSONValue;
begin
  Result := nil;
end;

constructor TmJSONValue.Create(AParent: TmJSONList; AKey: String);
begin
  FParent := AParent;

  FKey := AKey;
end;

function TmJSONValue.GetAsBoolean: Boolean;
begin
  Result := False;
end;

function TmJSONValue.GetAsGUID: TGUID;
begin
  Result := StringToGUID(AsString);
end;

function TmJSONValue.GetAsSingle: Single;
begin
  Result := 0;
end;

function TmJSONValue.GetAsInteger: Integer;
begin
  Result := 0;
end;

function TmJSONValue.GetAsString: String;
begin
  Result := '';
end;

procedure TmJSONValue.SetAsBoolean(AValue: Boolean);
begin

end;

procedure TmJSONValue.SetAsGUID(AValue: TGUID);
begin
  AsString := GUIDToString(AValue);
end;

procedure TmJSONValue.SetAsSingle(AValue: Single);
begin

end;

procedure TmJSONValue.SetAsInteger(AValue: Integer);
begin

end;

procedure TmJSONValue.SetAsString(AValue: String);
begin

end;

{ TJSONString }

function TmJSONString.Clone: TmJSONValue;
begin
  Result := TmJSONString.Create(nil, FKey, FValue);
end;

constructor TmJSONString.Create(AParent: TmJSONList; AKey, AValue: String);
begin
  inherited Create(AParent, AKey);

  FValue := AValue;
end;

function TmJSONString.GetAsBoolean: Boolean;
begin
  Result := FValue <> '';
end;

function TmJSONString.GetAsSingle: Single;
var
  Code: Integer;
begin
  Val(FValue, Result, Code);
  if Code <> 0 then Result := 0;
end;

function TmJSONString.GetAsInteger: Integer;
var
  Code: Integer;
begin
  Val(FValue, Result, Code);
  if Code <> 0 then Result := 0;
end;

function TmJSONString.GetAsString: String;
begin
  Result := FValue;
end;

procedure TmJSONString.SetAsBoolean(AValue: Boolean);
begin
  if AValue then begin
    FValue := 'True';
  end else begin
    FValue := 'False';
  end;
end;

procedure TmJSONString.SetAsSingle(AValue: Single);
begin
  FValue := FloatToStr(AValue);
end;

procedure TmJSONString.SetAsInteger(AValue: Integer);
begin
  FValue := IntToStr(AValue);
end;

procedure TmJSONString.SetAsString(AValue: String);
begin
  FValue := AValue;
end;

{ TJSONInteger }

function TmJSONInteger.Clone: TmJSONValue;
begin
  Result := TmJSONInteger.Create(nil, FKey, FValue);
end;

constructor TmJSONInteger.Create(AParent: TmJSONList; AKey: String; AValue: Integer);
begin
  inherited Create(AParent, AKey);

  FValue := AValue;
end;

function TmJSONInteger.GetAsBoolean: Boolean;
begin
  Result := FValue <> 0;
end;

function TmJSONInteger.GetAsSingle: Single;
begin
  Result := FValue;
end;

function TmJSONInteger.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TmJSONInteger.GetAsString: String;
begin
  Result := IntToStr(FValue);
end;

procedure TmJSONInteger.SetAsBoolean(AValue: Boolean);
begin
  FValue := Integer(AValue);
end;

procedure TmJSONInteger.SetAsSingle(AValue: Single);
begin
  FValue := Round(AValue);
end;

procedure TmJSONInteger.SetAsInteger(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TmJSONInteger.SetAsString(AValue: String);
var
  Code: Integer;
begin
  Val(AValue, FValue, Code);
  if Code <> 0 then FValue := 0;
end;

{ TJSONSingle }

function TmJSONSingle.Clone: TmJSONValue;
begin
  Result := TmJSONSingle.Create(nil, FKey, FValue);
end;

constructor TmJSONSingle.Create(AParent: TmJSONList; AKey: String; AValue: Single);
begin
  inherited Create(AParent, AKey);

  FValue := AValue;
end;

function TmJSONSingle.GetAsBoolean: Boolean;
begin
  Result := FValue <> 0;
end;

function TmJSONSingle.GetAsSingle: Single;
begin
  Result := FValue;
end;

function TmJSONSingle.GetAsInteger: Integer;
begin
  Result := Round(FValue);
end;

function TmJSONSingle.GetAsString: String;
var
  FS: TFormatSettings;
begin
  //FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := #0;

  Result := FloatToStr(FValue, FS)
end;

procedure TmJSONSingle.SetAsBoolean(AValue: Boolean);
begin
  FValue := Integer(AValue);
end;

procedure TmJSONSingle.SetAsSingle(AValue: Single);
begin
  FValue := AValue;
end;

procedure TmJSONSingle.SetAsInteger(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TmJSONSingle.SetAsString(AValue: String);
var
  FS: TFormatSettings;
begin
  //FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := #0;

  FValue := StrToFloat(AValue, FS);
end;

{ TJSONBoolean }

function TmJSONBoolean.Clone: TmJSONValue;
begin
  Result := TmJSONBoolean.Create(nil, FKey, FValue);
end;

constructor TmJSONBoolean.Create(AParent: TmJSONList; AKey: String; AValue: Boolean);
begin
  inherited Create(AParent, AKey);

  FValue := AValue;
end;

function TmJSONBoolean.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TmJSONBoolean.GetAsSingle: Single;
begin
  Result := Integer(FValue);
end;

function TmJSONBoolean.GetAsInteger: Integer;
begin
  Result := Integer(FValue);
end;

function TmJSONBoolean.GetAsString: String;
begin
  if FValue then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TmJSONBoolean.SetAsBoolean(AValue: Boolean);
begin
  FValue := AValue;
end;

procedure TmJSONBoolean.SetAsSingle(AValue: Single);
begin
  FValue := AValue <> 0;
end;

procedure TmJSONBoolean.SetAsInteger(AValue: Integer);
begin
  FValue := AValue <> 0;
end;

procedure TmJSONBoolean.SetAsString(AValue: String);
begin
  FValue := AValue <> '';
end;

{ TJSONList }

procedure TmJSONList.Add(AKey: String; AValue: Integer);
begin
  if Self is TmJSONArray then AKey := '';

  FList.Add(TmJSONInteger.Create(Self, AKey, AValue));
end;

procedure TmJSONList.Add(AKey, AValue: String);
begin
  if Self is TmJSONArray then AKey := '';

  FList.Add(TmJSONString.Create(Self, AKey, AValue));
end;

procedure TmJSONList.Add(AKey: String; AValue: Boolean);
begin
  if Self is TmJSONArray then AKey := '';

  FList.Add(TmJSONBoolean.Create(Self, AKey, AValue));
end;

function TmJSONList.AddArray(AKey: String): TmJSONArray;
begin
  Result := TmJSONArray.Create(Self, AKey);

  FList.Add(Result);
end;

procedure TmJSONList.Add(ANode: TmJSONValue);
begin
  FList.Add(ANode.Clone);

  ANode.FParent := Self;
end;

procedure TmJSONList.Add(AKey: String; AValue: TGUID);
begin
  Add(AKey, GUIDToString(AValue));
end;

function TmJSONList.AddObject(AKey: String): TmJSONObject;
begin
  Result := TmJSONObject.Create(Self, AKey);

  FList.Add(Result);
end;

procedure TmJSONList.Clear;
begin
  while FList.Count > 0 do begin
    TmJSONValue(FList.Items[0]).Free;
    FList.Delete(0);
  end;
end;

function TmJSONList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TmJSONList.Create(AParent: TmJSONList; AKey: String);
begin
  inherited Create(AParent, AKey);

  FList := TList.Create;
end;

procedure TmJSONList.Delete(AKey: String);
var
  Index: Integer;
begin
  Index := FindIndex(AKey);

  if Index > -1 then begin
    TmJSONValue(FList.Items[Index]).Free;

    FList.Delete(Index);
  end;
end;

destructor TmJSONList.Destroy;
begin
  Clear;

  FList.Free;

  inherited Destroy;
end;

function TmJSONList.Exists(AKey: String): Boolean;
begin
  Result := FindIndex(AKey) > -1;
end;

function TmJSONList.FindIndex(AKey: String): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FList.Count - 1 do begin
    if LowerCase(TmJSONValue(FList.Items[I]).Key) = LowerCase(AKey) then begin
      Result := I;

      Break;
    end;
  end;
end;

function TmJSONList.Get(AIndex: Integer): TmJSONValue;
begin
  Result := nil;

  if (AIndex < 0) or (AIndex >= FList.Count) then Exit;

  Result := TmJSONValue(FList.Items[AIndex]);
end;

function TmJSONList.Get(AKey: String; ADefault: Integer): TmJSONValue;
var
  Index: Integer;
begin
  Index := FindIndex(AKey);

  if Index > -1 then begin
    Result := TmJSONValue(FList.Items[Index]);
  end else begin
    Result := TmJSONInteger.Create(Self, AKey, ADefault);

    FList.Add(Result);
  end;
end;

function TmJSONList.Get(AKey: String; ADefault: String): TmJSONValue;
var
  Index: Integer;
begin
  Index := FindIndex(AKey);

  if Index > -1 then begin
    Result := TmJSONValue(FList.Items[Index]);
  end else begin
    Result := TmJSONString.Create(Self, AKey, ADefault);

    FList.Add(Result);
  end;
end;

function TmJSONList.Get(AKey: String; ADefault: Boolean): TmJSONValue;
var
  Index: Integer;
begin
  Index := FindIndex(AKey);

  if Index > -1 then begin
    Result := TmJSONValue(FList.Items[Index]);
  end else begin
    Result := TmJSONBoolean.Create(Self, AKey, ADefault);

    FList.Add(Result);
  end;
end;

function TmJSONList.Get(AKey: String): TmJSONValue;
var
  Index: Integer;
begin
  Result := nil;

  Index := FindIndex(AKey);

  if Index = -1 then Exit;

  Result := TmJSONValue(FList.Items[Index]);
end;

function TmJSONList.Get(AKey: String; ADefault: Single): TmJSONValue;
var
  Index: Integer;
begin
  Index := FindIndex(AKey);

  if Index > -1 then begin
    Result := TmJSONValue(FList.Items[Index]);
  end else begin
    Result := TmJSONSingle.Create(Self, AKey, ADefault);

    FList.Add(Result);
  end;
end;

function TmJSONList.GetAsString: String;
var
  I: Integer;
  V: TmJSONValue;
begin
  if Self is TmJSONArray then begin
    Result := '[';
  end else begin
    Result := '{';
  end;

  for I := 0 to FList.Count - 1 do begin
    V := TmJSONValue(FList.Items[I]);

    if (I > 0) then Result := Result + ',';

    if Self is TmJSONObject then begin
      Result := Result + '"' + V.Key + '":';
    end;

    if V is TmJSONString then begin
      Result := Result + '"' + Backsplash(V.AsString) + '"';
    end else begin
      Result := Result + V.AsString;
    end;
  end;

  if Self is TmJSONArray then begin
    Result := Result + ']';
  end else begin
    Result := Result + '}';
  end;
end;

function TmJSONList.GetList(AIndex: Integer): TmJSONList;
begin
  Result := TmJSONList(Get(AIndex));
end;

function TmJSONList.GetList(AKey: String): TmJSONList;
begin
  Result := TmJSONList(Get(AKey));
end;

procedure TmJSONList.SetAsString(AValue: String);
type
  TValueType = (vtObject, vtArray);
  TReadAs = (raUnknown, raString, raInteger, raSingle, raBoolean);
var
  Str, K, V: String;
  Cur: TmJSONList;
  ReadAs: TReadAs;

  procedure ParseError;
  begin
    Clear;

    raise Exception.Create('Parse error');
  end;

  function AsInteger(AStr: String): Integer;
  var
    Code: Integer;
  begin
    Val(AStr, Result, Code);

    if Code <> 0 then Result := 0;
  end;

  function AsSingle(AStr: String): Single;
  var
    Code: Integer;
  begin
    Val(AStr, Result, Code);

    if Code <> 0 then Result := 0;
  end;

  function AsBoolean(AStr: String): Boolean;
  begin
    Result := (LowerCase(AStr) = 'true');
  end;

  function CodeToUTF8(AString: AnsiString): UTF8String;
  var
    C: Integer;
    A: AnsiString;
  begin
    C := StrToInt('$' + AString);

    SetLength(A, 2);

    A[2] := Char((C and 63) + 128);
    A[1] := Char(C div 64 + 192);

    Result := A;
  end;

  function ReadDelim: Char;
  begin
    Result := #0;

    while (Result = #0) or (Result = ' ') do begin
      if Length(Str) = 0 then Break;

      if Str[1] in [' ', #13, #10, #08] then System.Delete(Str, 1, 1);

      Result := Str[1];
    end;
  end;

  function ReadString: String;
  var
    Ch, StrCh: Char;

    Slash: Boolean;
  begin
    Result := '';

    StrCh  := #0;
    Slash  := False;

    ReadAs := raUnknown;

    while True do begin
      if Length(Str) = 0 then begin
        ParseError;
        Break;
      end;

      Ch := Str[1];

      case ReadAs of
        raUnknown: begin
          case Ch of
            ' ', #13, #10, #08: begin
              System.Delete(Str, 1, 1);
              // Nothing todo
            end;
            '"','''': begin
              StrCh  := Ch;
              ReadAs := raString;
              System.Delete(Str, 1, 1);
            end;
            '-','0'..'9': begin
              ReadAs := raInteger;
              Result := Result + Ch;
              System.Delete(Str, 1, 1);
            end;
            't','T','f','F': begin
              ReadAs := raBoolean;
              Result := Result + Ch;
              System.Delete(Str, 1, 1);
            end
            else begin
              ParseError;
              Break;
            end;
          end;
        end;
        raString: begin
          case Ch of
            'b': begin
              if Slash then begin
                Result := Result + #8;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            't': begin
              if Slash then begin
                Result := Result + #9;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            'n': begin
              if Slash then begin
                Result := Result + #10;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            'f': begin
              if Slash then begin
                Result := Result + #12;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            'r': begin
              if Slash then begin
                Result := Result + #13;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            'u': begin
              if Slash then begin
                Result := Result + CodeToUTF8(Copy(Str, 2, 4));
                System.Delete(Str, 1, 5);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            '/': begin
              if Slash then begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            '"', '''': begin
              if StrCh = Ch then begin
                if Slash then begin
                  Result := Result + Ch;
                  System.Delete(Str, 1, 1);
                  Slash := False;
                end else begin
                  System.Delete(Str, 1, 1);
                  Break;
                end;
              end else begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end;
            end;
            '\': begin
              if Slash then begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
                Slash := False;
              end else begin
                System.Delete(Str, 1, 1);
                Slash := True;
              end;
            end;
            else begin
              Result := Result + Ch;
              System.Delete(Str, 1, 1);
              Slash := False;
            end;
          end;
        end;
        raInteger: begin
          case Ch of
            '.': begin
              Result := Result + Ch;
              System.Delete(Str, 1, 1);
              ReadAs := raSingle;
            end;
            '0'..'9': begin
              Result := Result + Ch;
              System.Delete(Str, 1, 1);
            end;
            '-': begin
              if Length(Result) = 0 then begin
                Result := Result + Ch;
                System.Delete(Str, 1, 1);
              end else begin
                ParseError;
                Break;
              end;
            end;
            ' ', ',', ':', '}', ']': Break;
            else begin
              ParseError;
              Break;
            end;
          end;
        end;
        raSingle: begin
          case Ch of
            '0'..'9': begin
              Result := Result + Ch;
              System.Delete(Str, 1, 1);
            end;
            ' ', ',', ':', '}', ']': Break;
            else begin
              ParseError;
              Break;
            end;
          end;
        end;
        raBoolean: begin
          case Ch of
            ' ', ',', ':', '}', ']': Break;
            else begin
              Result := Result + Ch;

              if (Copy('true', 1, Length(Result)) = LowerCase(Result)) or (Copy('false', 1, Length(Result)) = LowerCase(Result)) then begin
                System.Delete(Str, 1, 1);
              end else begin
                ParseError;
                Break;
              end;
            end;
          end;
        end;
      end;
    end;

    Result := Trim(Result);
  end;

  procedure NewValue(AValueType: TValueType);
  var
    Ch: Char;
  begin
    while True do begin
      K := '';
      V := '';

      if AValueType = vtObject then begin
        if Length(Str) = 0 then begin
          ParseError;
          Break;
        end;

        if Str[1] = '}' then begin
          Break;
        end;

        K := ReadString;

        if ReadAs <> raString then begin
          ParseError;
          Break;
        end;

        Ch := ReadDelim;

        System.Delete(Str, 1, 1);

        if Ch <> ':' then begin
          ParseError;
          Break;
        end;
      end;

      if Length(Str) = 0 then begin
        ParseError;
        Break;
      end;

      Ch := ReadDelim;

      case Ch of
        '{': begin
          System.Delete(Str, 1, 1);

          Cur := Cur.AddObject(K);

          NewValue(vtObject);
        end;
        '[': begin
          System.Delete(Str, 1, 1);

          Cur := Cur.AddArray(K);

          NewValue(vtArray);
        end;
        '}',']': Break;
        else begin
          V := ReadString;

          case ReadAs of
            raUnknown: begin
              ParseError;
              Break;
            end;
            raString: Cur.Add(K, V);
            raInteger: Cur.Add(K, AsInteger(V));
            raSingle: Cur.Add(K, AsSingle(V));
            raBoolean: Cur.Add(K, AsBoolean(V));
          end;
        end;
      end;

      Ch := ReadDelim;

      if ((AValueType = vtObject) and (Ch = '}'))
      or ((AValueType = vtArray) and (Ch = ']')) then Break;

      if Ch <> ',' then begin
        ParseError;
        Break;
      end;

      System.Delete(Str, 1, 1);
    end;

    System.Delete(Str, 1, 1);

    Cur := Cur.FParent;
  end;
begin
  Clear;

  Str := AValue;

  K := '';
  V := '';

  Cur := Self;

  if Length(Str) = 0 then Exit;

  if Str[1] = '{' then begin
    System.Delete(Str, 1, 1);

    NewValue(vtObject);
  end else if Str[1] = '[' then begin
    System.Delete(Str, 1, 1);

    NewValue(vtArray);
  end else begin
    Exit;
  end;
end;

procedure TmJSONList.Replace(AKey: String; AValue: Integer);
var
  V: TmJSONValue;
begin
  if Self is TmJSONArray then AKey := '';

  V := Get(AKey);

  if V = nil then begin
    Add(AKey, AValue);
  end else if V is TmJSONInteger then begin
    V.AsInteger := AValue;
  end else begin
    Delete(AKey);

    Add(AKey, AValue);
  end;
end;

procedure TmJSONList.Replace(AKey: string; AValue: String);
var
  V: TmJSONValue;
begin
  if Self is TmJSONArray then AKey := '';

  V := Get(AKey);

  if V = nil then begin
    Add(AKey, AValue);
  end else if V is TmJSONString then begin
    V.AsString := AValue;
  end else begin
    Delete(AKey);

    Add(AKey, AValue);
  end;
end;

procedure TmJSONList.Replace(AKey: String; AValue: Single);
var
  V: TmJSONValue;
begin
  if Self is TmJSONArray then AKey := '';

  V := Get(AKey);

  if V = nil then begin
    Add(AKey, AValue);
  end else if V is TmJSONSingle then begin
    V.AsSingle := AValue;
  end else begin
    Delete(AKey);

    Add(AKey, AValue);
  end;
end;

procedure TmJSONList.Replace(AKey: String; AValue: TGUID);
begin
  Replace(AKey, GUIDToString(AValue));
end;

procedure TmJSONList.Replace(AKey: String; AValue: Boolean);
var
  V: TmJSONValue;
begin
  if Self is TmJSONArray then AKey := '';

  V := Get(AKey);

  if V = nil then begin
    Add(AKey, AValue);
  end else if V is TmJSONBoolean then begin
    V.AsBoolean := AValue;
  end else begin
    Delete(AKey);

    Add(AKey, AValue);
  end;
end;

procedure TmJSONList.Add(AKey: String; AValue: Single);
begin
  if Self is TmJSONArray then AKey := '';

  FList.Add(TmJSONSingle.Create(Self, AKey, AValue));
end;

function TmJSONList.Get(AKey: String; ADefault: TGUID): TmJSONValue;
begin
  Result := Get(AKey, GUIDToString(ADefault));
end;

{ TJSON }

constructor TmJSON.Create;
begin
  inherited Create(nil, '');
end;

function TmJSON.LoadFromFile(AFileName: String; ACompress: Boolean): Boolean;
var
  Fle: TFileStream;
begin
  Fle := TFileStream.Create(AFileName, fmOpenRead);

  Result := LoadFromStream(Fle, ACompress);

  Fle.Free;
end;

function TmJSON.LoadFromStream(AStream: TStream; ACompress: Boolean): Boolean;
var
  Str: AnsiString;
begin
  SetLength(Str, AStream.Size - AStream.Position);

  AStream.Read(Str[1], AStream.Size - AStream.Position);

  Result := LoadFromString(Str, ACompress);
end;

function TmJSON.LoadFromString(AString: AnsiString; ACompress: Boolean): Boolean;
begin
  try
    {$ifdef ZLIB}
      if ACompress then begin
        AString := ZDecompressStr(AString);
      end;
    {$endif}

    AsString := UTF8Decode(AString);

    Result := True;
  except
    Result := False;
  end;
end;

procedure TmJSON.SaveToFile(AFileName: String; ACompress: Boolean);
var
  Fle: TFileStream;
begin
  Fle := TFileStream.Create(AFileName, fmCreate);

  SaveToStream(Fle, ACompress);

  Fle.Free;
end;

procedure TmJSON.SaveToStream(AStream: TStream; ACompress: Boolean);
var
  Str: AnsiString;
begin
  Str := SaveToString(ACompress);

  AStream.Write(Str[1], Length(Str));
end;

function TmJSON.SaveToString(ACompress: Boolean): AnsiString;
begin
  Result := UTF8Encode(AsString);

  if ACompress then begin
    {$ifdef ZLIB}Result := ZCompressStr(Result);{$endif}
  end;
end;

{ TJSONArray }

function TmJSONArray.Clone: TmJSONValue;
var
  I: Integer;
begin
  Result := TmJSONArray.Create(nil, FKey);

  for I := 0 to Count - 1 do begin
    TmJSONList(Result).Add(Get(I));
  end;
end;

{ TJSONObject }

function TmJSONObject.Clone: TmJSONValue;
var
  I: Integer;
begin
  Result := TmJSONObject.Create(nil, FKey);

  for I := 0 to Count - 1 do begin
    TmJSONList(Result).Add(Get(I));
  end;
end;

end.
