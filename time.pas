unit Time;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  DateUtils,
  {$ifdef MSWINDOWS}
    windows,
  {$endif}
  {$ifdef UNIX}
    unixutil,
  {$endif}
  SysUtils;

function GetBias: Integer;
function UniversalTimeToLocal(UT: TDateTime): TDateTime;
function LocalToUniversalTime(LT: TDateTime): TDateTime;
function LocalTimeToUnix(LocalTime: TDateTime): LongInt;
function UnixToLocalTime(UnixTime: LongInt): TDateTime;

implementation

function GetBias: Integer;
{$ifdef MSWINDOWS}
var
  BiasType: Byte;
  TZInfo: TTimeZoneInformation;
{$endif}
begin
  {$ifdef MSWINDOWS}
  BiasType := GetTimeZoneInformation(TZInfo);

    case BiasType of
      TIME_ZONE_ID_UNKNOWN:  Result := TZInfo.Bias;
      TIME_ZONE_ID_STANDARD: Result := TZInfo.Bias + TZInfo.StandardBias;
      TIME_ZONE_ID_DAYLIGHT: Result := TZInfo.Bias + TZInfo.DaylightBias;
    end;
  {$endif}
  {$ifdef UNIX}
    Result := -Tzseconds div 60;
  {$endif}
end;

function UniversalTimeToLocal(UT: TDateTime): TDateTime;
begin
  Result := IncMinute(UT, -GetBias());
end;

function LocalToUniversalTime(LT: TDateTime): TDateTime;
begin
  Result := IncMinute(LT, GetBias());
end;

function LocalTimeToUnix(LocalTime: TDateTime): LongInt;
begin
  Result := DateTimeToUnix(LocalToUniversalTime(LocalTime));
end;

function UnixToLocalTime(UnixTime: LongInt): TDateTime;
begin
  Result := UniversalTimeToLocal(UnixToDateTime(UnixTime));
end;

end.

