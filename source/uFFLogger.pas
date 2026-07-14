unit uFFLogger;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ FFmpeg av_log callback bridge for Delphi applications and design-time debugging. }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  System.SysUtils,
  {$ENDIF}
  ffmpeg_types,
  libavutil,
  uFFLoader;

type
  TFFLogLevel = (
    llQuiet = -8,
    llPanic = 0,
    llFatal = 8,
    llError = 16,
    llWarning = 24,
    llInfo = 32,
    llVerbose = 40,
    llDebug = 48,
    llTrace = 56
  );

  TFFLogEvent = procedure(Sender: TObject; ALevel: TFFLogLevel; const AMessage: string) of object;

  TFFLogger = class(TComponent)
  private
    FActive: Boolean;
    FLevel: TFFLogLevel;
    FForwardToDefault: Boolean;
    FOnLog: TFFLogEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetLevel(const Value: TFFLogLevel);
    procedure InstallCallback;
    procedure RemoveCallback;
  protected
    procedure DoLog(ALevel: TFFLogLevel; const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function LevelToAvLevel(ALevel: TFFLogLevel): Integer;
    class function AvLevelToLevel(AValue: Integer): TFFLogLevel;
    class function Default: TFFLogger;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Level: TFFLogLevel read FLevel write SetLevel default llInfo;
    property ForwardToDefault: Boolean read FForwardToDefault write FForwardToDefault default False;
    property OnLog: TFFLogEvent read FOnLog write FOnLog;
  end;

implementation

{$IFDEF FPC}
uses SysUtils;
{$ENDIF}

var
  GActiveLogger: TFFLogger;
  GDefaultLogger: TFFLogger;
  GLogLinePrefix: Integer = 1;

procedure FFmpegLogCallback(avcl: Pointer; level: Integer; fmt: PAnsiChar; vl: PVA_LIST); cdecl;
var
  Line: array [0 .. 2047] of AnsiChar;
  Len: Integer;
  Msg: string;
  LogLevel: TFFLogLevel;
begin
  if Assigned(GActiveLogger) and GActiveLogger.Active then
  begin
    LogLevel := TFFLogger.AvLevelToLevel(level);
    if Integer(LogLevel) <= Integer(GActiveLogger.Level) then
    begin
      Len := av_log_format_line2(avcl, level, fmt, vl, @Line[0], SizeOf(Line), GLogLinePrefix);
      if Len > 0 then
      begin
        Msg := string(AnsiString(@Line[0]));
        GActiveLogger.DoLog(LogLevel, Msg);
      end;
    end;
  end;

  if (not Assigned(GActiveLogger)) or (not GActiveLogger.Active) or GActiveLogger.ForwardToDefault then
    av_log_default_callback(avcl, level, fmt, vl);
end;

class function TFFLogger.LevelToAvLevel(ALevel: TFFLogLevel): Integer;
begin
  Result := Integer(ALevel);
end;

class function TFFLogger.AvLevelToLevel(AValue: Integer): TFFLogLevel;
begin
  if AValue <= Integer(llPanic) then
    Result := llPanic
  else if AValue <= Integer(llFatal) then
    Result := llFatal
  else if AValue <= Integer(llError) then
    Result := llError
  else if AValue <= Integer(llWarning) then
    Result := llWarning
  else if AValue <= Integer(llInfo) then
    Result := llInfo
  else if AValue <= Integer(llVerbose) then
    Result := llVerbose
  else if AValue <= Integer(llDebug) then
    Result := llDebug
  else
    Result := llTrace;
end;

class function TFFLogger.Default: TFFLogger;
begin
  if GDefaultLogger = nil then
    GDefaultLogger := TFFLogger.Create(nil);
  Result := GDefaultLogger;
end;

constructor TFFLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevel := llInfo;
  if GDefaultLogger = nil then
    GDefaultLogger := Self;
end;

destructor TFFLogger.Destroy;
begin
  if GDefaultLogger = Self then
    GDefaultLogger := nil;
  Active := False;
  inherited Destroy;
end;

procedure TFFLogger.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      InstallCallback
    else
      RemoveCallback;
    FActive := Value;
  end;
end;

procedure TFFLogger.SetLevel(const Value: TFFLogLevel);
begin
  FLevel := Value;
  if FActive and not (csDesigning in ComponentState) then
    av_log_set_level(LevelToAvLevel(FLevel));
end;

procedure TFFLogger.InstallCallback;
begin
  if csDesigning in ComponentState then
    Exit;
  TFFLoader.EnsureLoaded;
  GActiveLogger := Self;
  GLogLinePrefix := 1;
  av_log_set_level(LevelToAvLevel(FLevel));
  av_log_set_callback(@FFmpegLogCallback);
end;

procedure TFFLogger.RemoveCallback;
begin
  if GActiveLogger = Self then
  begin
    av_log_set_callback(@av_log_default_callback);
    GActiveLogger := nil;
  end;
end;

procedure TFFLogger.DoLog(ALevel: TFFLogLevel; const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, ALevel, AMessage);
end;

end.
