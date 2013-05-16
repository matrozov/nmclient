unit Request;

{$mode objfpc}{$H+}

interface

//{$define DEBUG}

uses
  {$ifdef WINDOWS}
    ZLibEx,
  {$endif}
  SysUtils,
  Classes,
  lNet,
  Forms,
  Dialogs,
  Base64,
  DateUtils;

type
  { TRequest }

  TRequestMethod = (rmGet, rmPost);

  TRequest = class
  private
    FTCP:             TLTCP;

    FReadTime:        TDateTime;
    FFirstHeader:     Boolean;
    FHeader:          Boolean;

    FProxy:           Boolean;
    FProxyHost:       AnsiString;
    FProxyPort:       Word;
    FProxyAuth:       Boolean;
    FProxyLogin:      AnsiString;
    FProxyPassword:   AnsiString;

    FMethod:          TRequestMethod;
    FHost:            String;
    FPort:            Word;
    FURI:             String;

    FRequest:         AnsiString;

    FResponseBuffer:  AnsiString;
    FResponseCode:    Word;
    FResponse:        AnsiString;

    FContentLength:   Integer;
    FContentEncoding: AnsiString;
  private
    procedure tcpReceive(aSocket: TLSocket);
    procedure tcpError(const aMsg: string; aSocket: TLSocket);
  public
    constructor Create;
    destructor  Destroy; override;

    function Execute: Boolean;
  public
    property Proxy:         Boolean    read FProxy         write FProxy;
    property ProxyHost:     AnsiString read FProxyHost     write FProxyHost;
    property ProxyPort:     Word       read FProxyPort     write FProxyPort;
    property ProxyAuth:     Boolean    read FProxyAuth     write FProxyAuth;
    property ProxyLogin:    AnsiString read FProxyLogin    write FProxyLogin;
    property ProxyPassword: AnsiString read FProxyPassword write FProxyPassword;

    property Method:        TRequestMethod read FMethod    write FMethod;
    property Host:          String     read FHost          write FHost;
    property Port:          Word       read FPort          write FPort;
    property URI:           String     read FURI           write FURI;

    property Request:       AnsiString read FRequest       write FRequest;
    property ResponseCode:  Word       read FResponseCode;
    property Response:      AnsiString read FResponse;
  end;

implementation

uses
  Main;

{ TPostRequest }

procedure TRequest.tcpReceive(aSocket: TLSocket);
var
  Str, Hdr, HdrName, HdrVal: AnsiString;
  Size, P: Integer;
begin
  FReadTime := Now;

  Size := aSocket.GetMessage(Str);

  if Size = 0 then begin
    Exit;
  end;

  FResponseBuffer := FResponseBuffer + Str;

  while Length(FResponseBuffer) > 0 do begin
    if FHeader then begin
      P := Pos(#10, FResponseBuffer);

      if P = 0 then begin
        Break;
      end;

      Hdr := Copy(FResponseBuffer, 1, P);
      Delete(FResponseBuffer, 1, P);

      if FFirstHeader then begin
        P := Pos(' ', Hdr);
        Delete(Hdr, 1, P);
        P := Pos(' ', Hdr);
        Delete(Hdr, P, Length(Hdr) - P + 1);

        FResponseCode := StrToInt(Hdr);

        FFirstHeader := False;
      end else begin
        P := Pos(':', Hdr);
        HdrName := LowerCase(Copy(Hdr, 1, P - 1));
        HdrVal  := Trim(Copy(Hdr, P + 1, 1024));

        if HdrName = 'content-length' then begin
          FContentLength := StrToInt(HdrVal);
        end else if HdrName = 'content-encoding' then begin
          FContentEncoding := LowerCase(HdrVal);
        end;
      end;

      if Trim(Hdr) = '' then begin
        FHeader := False;
      end;
    end else begin
      FResponse := FResponse + FResponseBuffer;
      FResponseBuffer := '';
    end;
  end;
end;

procedure TRequest.tcpError(const aMsg: string; aSocket: TLSocket);
begin

end;

constructor TRequest.Create;
begin
  FTCP := TLTCP.Create(nil);

  FTCP.OnReceive := @tcpReceive;
  FTCP.OnError   := @tcpError;

  FMethod := rmGet;
  FPort   := 80;
end;

destructor TRequest.Destroy;
begin
  if FTCP.Connecting or FTCP.Connected then begin
    FTCP.Disconnect;
  end;

  FTCP.Destroy;

  inherited Destroy;
end;

function TRequest.Execute: Boolean;
var
  MethodStr: String;

  procedure Wait;
  begin
    FTCP.CallAction;

    Application.ProcessMessages;
    Sleep(1);
  end;

  procedure Write(AString: AnsiString);
  begin
    FTCP.SendMessage(AString);
  end;

  procedure WriteLn(AString: AnsiString = '');
  begin
    Write(AString + #13#10);
  end;

begin
  Result := False;

  if FProxy then begin
    FTCP.Host  := FProxyHost;
    FTCP.Port  := FProxyPort;
  end else begin
    FTCP.Host  := FHost;
    FTCP.Port  := FPort;
  end;

  FFirstHeader     := True;
  FHeader          := True;

  FResponseBuffer  := '';
  FResponseCode    := 0;
  FResponse        := '';

  FContentLength   := -1;
  FContentEncoding := '';

  FTCP.Timeout     := 1000;

  if FTCP.Connect then begin
    FReadTime := Now;

    repeat
      Wait;
    until (MilliSecondsBetween(Now, FReadTime) > 2000) or FTCP.Connected;

    if FTCP.Connected then begin
      case FMethod of
        rmGet:  MethodStr := 'GET';
        rmPost: MethodStr := 'POST';
      end;

      if FProxy then begin
        WriteLn(MethodStr + ' http://' + FHost + '/' + FURI + ' HTTP/1.0');
      end else begin
        WriteLn(MethodStr + ' /' + FURI + ' HTTP/1.0');
      end;

      WriteLn('Host: ' + FHost);
      WriteLn('User-Agent: nmclient (v' + frmMain.FVersion + ', ' {$ifdef WINDOWS}+ 'windows'{$endif}{$ifdef UNIX}+ 'linux'{$endif}+ ')');
      WriteLn('Pragma: no-cache');
      WriteLn('Cache-Control: no-cache');
      WriteLn('Content-Length: ' + IntToStr(Length(FRequest)));
      {$ifdef WINDOWS}WriteLn('Accept-Encoding: gzip, x-gzip');{$endif}

      if FProxy and FProxyAuth then begin
        WriteLn('Proxy-Authorization: Basic ' + EncodeStringBase64(FProxyLogin + ':' + FProxyPassword));
      end;

      WriteLn('');

      if FRequest <> '' then begin
        Write(FRequest);
      end;

      FReadTime := Now;

      repeat
        Wait;
      until (MilliSecondsBetween(Now, FReadTime) > 2000) or (not FTCP.Connected) or ((FContentLength > -1) and (Length(FResponse) = FContentLength));
    end else begin
      FTCP.Disconnect(True);
      {$ifdef DEBUG}ShowMessage('Connection timeout');{$endif}
      Exit;
    end;
  end else begin
    {$ifdef DEBUG}ShowMessage('Can''t connect');{$endif}
    Exit;
  end;

  FTCP.Disconnect(True);

  if FResponseCode <> 200 then begin
    {$ifdef DEBUG}ShowMessage('Invalid response code: ' + IntToStr(FResponseCode));{$endif}
    Exit;
  end;

  if (FContentLength > -1) and (Length(FResponse) <> FContentLength) then begin
    {$ifdef DEBUG}ShowMessage('Invalid response length: ' + IntToStr(FContentLength) + ' need and ' + IntToStr(Length(FResponse)) + ' get');{$endif}
    Exit;
  end;

  if FContentEncoding <> '' then begin
    if (FContentEncoding = 'gzip') or (FContentEncoding = 'x-gzip') then begin
      try
        {$ifdef WINDOWS}
          ZLibEx.ZDecompressString2(FResponse, FResponse, 16 + 15);
        {$else}
          {$ifdef DEBUG}ShowMessage('ZLib not support'){$endif};
          Exit;
        {$endif}
      except
        {$ifdef DEBUG}ShowMessage('Exception in ZLib decompressonr'){$endif};
        Exit;
      end;
    end else begin
      {$ifdef DEBUG}ShowMessage('Unsupported compress method: ' + FContentEncoding){$endif};
      Exit;
    end;
  end;

  Result := True;
end;

end.

