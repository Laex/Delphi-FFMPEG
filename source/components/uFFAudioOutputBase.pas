unit uFFAudioOutputBase;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$POINTERMATH ON}

{ Shared PCM S16 volume helper for audio backends. }

interface

type
  TFFAudioOutputBase = class
  private
    FVolume: Single;
    FPaused: Boolean;
    FDraining: Boolean;
  protected
    procedure ApplyVolume(ASamples: PSmallInt; ASampleCount: Integer);
    procedure BeginDrain;
    procedure EndDrain;
  public
    constructor Create;
    procedure SetVolume(AValue: Single);
    property Volume: Single read FVolume write SetVolume;
    property Paused: Boolean read FPaused write FPaused;
    property Draining: Boolean read FDraining;
  end;

implementation

constructor TFFAudioOutputBase.Create;
begin
  inherited Create;
  FVolume := 1.0;
end;

procedure TFFAudioOutputBase.ApplyVolume(ASamples: PSmallInt; ASampleCount: Integer);
var
  I: Integer;
  V: Single;
  S: Integer;
begin
  if FVolume >= 0.999 then
    Exit;
  V := FVolume;
  if V < 0 then
    V := 0;
  for I := 0 to ASampleCount - 1 do
  begin
    S := Round(ASamples[I] * V);
    if S > 32767 then
      S := 32767
    else if S < -32768 then
      S := -32768;
    ASamples[I] := SmallInt(S);
  end;
end;

procedure TFFAudioOutputBase.SetVolume(AValue: Single);
begin
  if AValue < 0 then
    AValue := 0
  else if AValue > 1 then
    AValue := 1;
  FVolume := AValue;
end;

procedure TFFAudioOutputBase.BeginDrain;
begin
  FDraining := True;
end;

procedure TFFAudioOutputBase.EndDrain;
begin
  FDraining := False;
end;

end.
