unit uFFDesignTime;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Detect RAD Studio form designer (safe to skip FFmpeg / threads teardown). }

interface

uses
  System.Classes;

function FFIsDesignTime(AComponent: TComponent = nil): Boolean;

implementation

uses
  {$IFDEF FPC}
  SysUtils;
  {$ELSE}
  System.SysUtils;
  {$ENDIF}

function FFHostIsIde: Boolean;
var
  ExeName: string;
begin
  ExeName := LowerCase(ExtractFileName(ParamStr(0)));
  Result := (ExeName = 'bds.exe') or (ExeName = 'bdscmd.exe');
end;

function FFIsDesignTime(AComponent: TComponent): Boolean;
begin
  if (AComponent <> nil) and (csDesigning in AComponent.ComponentState) then
    Exit(True);
  Result := FFHostIsIde;
end;

end.
