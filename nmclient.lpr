program nmclient;

{$mode objfpc}{$H+}

uses
  {$ifdef WINDOWS}
    Windows,
  {$endif}
  {$ifdef UNIX}
    {$ifdef UseCThreads}
      cthreads,
    {$endif}
    BaseUnix,
  {$endif}
  Dialogs,
  Process,
  FileUtil,
  LCLIntf,
  SysUtils,
  Classes,
  Interfaces,
  Forms, Main, lnetvisual, request, device, about, option, time, Login,
  mJSON, Widget;

{$R *.res}

var
  Temp:     String;
  Fle:      TFileStream;
  PID:      String;
  FilePath: String;
  FileName: String;
  Prc:      TProcess;

function ProcessExists(APID: String): Boolean;
{$ifdef WINDOWS}
  var
    ProcessHandle: THandle;
{$endif}
begin
  {$ifdef WINDOWS}
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, StrToInt(APID));

    if ProcessHandle = 0 then begin
      Result := False;
    end else begin
      CloseHandle(ProcessHandle);

      Result := True;
    end;
  {$endif}
  {$ifdef UNIX}
    Result := DirectoryExists(Format('/proc/%s', [APID]));
  {$endif}
end;

begin
  Randomize;

  Application.Title := 'Народный мониторинг - Клиент';
  Application.Initialize;

  RequireDerivedFormResource := True;

  FilePath := ExtractFilePath(Application.ExeName);

  if ParamStr(1) = '/u' then begin
    Sleep(1000);

    FileName := ParamStr(2);

    if FileExists(FilePath + FileName) then begin
      DeleteFile(FilePath + FileName);
      RenameFile(FilePath + 'nmclient.upd', FilePath + FileName);
      {$ifdef UNIX}FpChmod(FilePath + FileName, &777);{$endif}

      Prc := TProcess.Create(nil);
      Prc.CurrentDirectory := FilePath;
      Prc.Executable := FileName;
      Prc.Options := Prc.Options - [poWaitOnExit];
      Prc.Execute;
      Prc.Free;

      Application.Terminate;
      Exit;
    end;
  end;

  if (ParamStr(1) <> '/noupdate') and FileExists('nmclient.upd') then begin
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

  DeleteFile(FilePath + {$ifdef WINDOWS}'update.exe'{$endif}{$ifdef UNIX}'update'{$endif});
  DeleteFile(FilePath + 'nmclient.upd');

  Temp := GetTempDir;

  if FileExists(Temp + '/nmclient.pid') then begin
    Fle := TFileStream.Create(Temp + '/nmclient.pid', fmOpenRead);
    SetLength(PID, Fle.Size);
    Fle.Read(PID[1], Fle.Size);
    Fle.Free;

    if ProcessExists(PID) then begin
      MessageDlgEx(Application.Title, 'Еще один экземпляр программы "' + Application.Title + '" уже запущен.'#13#10'Запуск второго экземляра программы не возможен!', mtError, [mbClose]);
      Halt;
    end;
  end;

  PID := IntToStr(GetProcessID);

  Fle := TFileStream.Create(Temp + '/nmclient.pid', fmCreate);
  Fle.Write(PID[1], Length(PID));
  Fle.Free;
Application.CreateForm(TfrmMain, frmMain);

  frmMain.onStart;
  Application.Run;

  DeleteFile(Temp + '/nmclient.pid');
end.

