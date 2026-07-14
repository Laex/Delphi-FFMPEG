unit uFFPlatformTime;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Monotonic wall clock in milliseconds (playback timing). }

interface

function FFGetTickCount64: UInt64;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows;
  {$ELSE}
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  System.Diagnostics,
  {$ENDIF}
  {$ENDIF}

function FFGetTickCount64: UInt64;
{$IFNDEF MSWINDOWS}
{$IFNDEF FPC}
var
  Stamp: Int64;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := GetTickCount64;
  {$ELSE}
  {$IFDEF FPC}
  Result := UInt64(GetTickCount64);
  {$ELSE}
  Stamp := TStopwatch.GetTimeStamp;
  Result := UInt64(Stamp div 10000);
  {$ENDIF}
  {$ENDIF}
end;

end.
