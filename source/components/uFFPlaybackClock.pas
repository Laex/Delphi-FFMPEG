unit uFFPlaybackClock;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Master playback clock for A/V sync (audio PTS or wall clock). }

interface

uses
  uFFPlatformTime,
  {$IFDEF FPC}
  Classes,
  SyncObjs;
  {$ELSE}
  System.Classes,
  System.SyncObjs;
  {$ENDIF}

const
  FFPlaybackAheadMs  = 10;
  FFPlaybackLateMs   = 100;

type
  TFFPlaybackClock = class
  private
    FLock: TCriticalSection;
    FBaseMs: Int64;
    FStartTick: UInt64;
    FPauseStartTick: UInt64;
    FPauseAccumTick: UInt64;
    FPaused: Boolean;
    FUseAudioMaster: Boolean;
    FAudioMasterMs: Int64;
    FAudioMasterActive: Boolean;
    FAudioAnchorMs: Int64;
    FAudioAnchorTick: UInt64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(ABaseMs: Int64; AUseAudioMaster: Boolean);
    procedure Pause;
    procedure Resume;
    function GetTimeMs: Int64;
    procedure SetAudioTimeMs(AValue: Int64);
    procedure WaitUntil(AFrameMs: Int64; const AStopRequested: Boolean; const APaused: Boolean);
    function IsLate(AFrameMs: Int64; ALateThresholdMs: Integer = FFPlaybackLateMs): Boolean;

    property UseAudioMaster: Boolean read FUseAudioMaster;
    property AudioMasterActive: Boolean read FAudioMasterActive;
    property Paused: Boolean read FPaused;
  end;

implementation

procedure FFSleepMs(MS: Cardinal);
begin
  {$IFDEF FPC}
  Sleep(MS);
  {$ELSE}
  TThread.Sleep(MS);
  {$ENDIF}
end;

constructor TFFPlaybackClock.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TFFPlaybackClock.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TFFPlaybackClock.GetTimeMs: Int64;
begin
  FLock.Enter;
  try
    if FUseAudioMaster and FAudioMasterActive then
    begin
      if FPaused and (FPauseStartTick > 0) then
        Result := FAudioAnchorMs + Int64(FPauseStartTick - FAudioAnchorTick - FPauseAccumTick)
      else
        Result := FAudioAnchorMs + Int64(FFGetTickCount64 - FAudioAnchorTick - FPauseAccumTick);
      Exit;
    end;

    if FPaused and (FPauseStartTick > 0) then
      Result := FBaseMs + Int64(FPauseStartTick - FStartTick - FPauseAccumTick)
    else
      Result := FBaseMs + Int64(FFGetTickCount64 - FStartTick - FPauseAccumTick);
  finally
    FLock.Leave;
  end;
end;

function TFFPlaybackClock.IsLate(AFrameMs: Int64; ALateThresholdMs: Integer): Boolean;
begin
  if AFrameMs < 0 then
    Exit(False);
  Result := GetTimeMs - AFrameMs > ALateThresholdMs;
end;

procedure TFFPlaybackClock.Pause;
begin
  FLock.Enter;
  try
    if FPaused then
      Exit;
    FPaused := True;
    FPauseStartTick := FFGetTickCount64;
  finally
    FLock.Leave;
  end;
end;

procedure TFFPlaybackClock.Reset(ABaseMs: Int64; AUseAudioMaster: Boolean);
begin
  FLock.Enter;
  try
    FBaseMs := ABaseMs;
    FUseAudioMaster := AUseAudioMaster;
    FAudioMasterMs := ABaseMs;
    FAudioAnchorMs := ABaseMs;
    FAudioMasterActive := not AUseAudioMaster;
    FStartTick := FFGetTickCount64;
    FAudioAnchorTick := FStartTick;
    FPauseStartTick := 0;
    FPauseAccumTick := 0;
    FPaused := False;
  finally
    FLock.Leave;
  end;
end;

procedure TFFPlaybackClock.Resume;
var
  NowTick: UInt64;
begin
  FLock.Enter;
  try
    if not FPaused then
      Exit;
    NowTick := FFGetTickCount64;
    if FPauseStartTick > 0 then
      Inc(FPauseAccumTick, NowTick - FPauseStartTick);
    FPauseStartTick := 0;
    FPaused := False;
  finally
    FLock.Leave;
  end;
end;

procedure TFFPlaybackClock.SetAudioTimeMs(AValue: Int64);
var
  NowMs: Int64;
begin
  if AValue < 0 then
    Exit;
  FLock.Enter;
  try
    if not FUseAudioMaster then
      Exit;

    if FPaused and (FPauseStartTick > 0) then
      NowMs := FAudioAnchorMs + Int64(FPauseStartTick - FAudioAnchorTick - FPauseAccumTick)
    else
      NowMs := FAudioAnchorMs + Int64(FFGetTickCount64 - FAudioAnchorTick - FPauseAccumTick);

    if AValue >= NowMs then
    begin
      FAudioMasterMs := AValue;
      FAudioAnchorMs := AValue;
      FAudioAnchorTick := FFGetTickCount64;
      FAudioMasterActive := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TFFPlaybackClock.WaitUntil(AFrameMs: Int64; const AStopRequested: Boolean; const APaused: Boolean);
var
  ClockMs: Int64;
  Delta: Int64;
begin
  if AFrameMs < 0 then
    Exit;

  while not AStopRequested do
  begin
    if APaused then
    begin
      FFSleepMs(10);
      Continue;
    end;

    ClockMs := GetTimeMs;
    if AFrameMs <= ClockMs + FFPlaybackAheadMs then
      Break;

    Delta := AFrameMs - ClockMs;
    if Delta > 200 then
      FFSleepMs(10)
    else if Delta > 20 then
      FFSleepMs(5)
    else
      FFSleepMs(1);
  end;
end;

end.
