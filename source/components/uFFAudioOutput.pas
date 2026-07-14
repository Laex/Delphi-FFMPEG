unit uFFAudioOutput;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Cross-platform PCM S16 audio output (WaveOut / ALSA / SDL2). }

interface

uses
  uFFAudioResampler,
  uFFException;

type
  TFFAudioOutput = class
  private
    FImpl: TObject;
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
  public
    constructor Create(ASampleRate: Integer = FFAudioDefaultSampleRate;
      AChannels: Integer = FFAudioDefaultChannels);
    destructor Destroy; override;

    procedure Write(const ABuffer: Pointer; AByteCount: Integer);
    procedure Pause;
    procedure Resume;
    procedure Flush;

    property Volume: Single read GetVolume write SetVolume;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  uFFAudioOutputWin,
  {$ENDIF}
  {$IFDEF LINUX}
  uFFAudioOutputALSA,
  uFFAudioOutputSDL,
  {$ENDIF}
  uFFAudioOutputBase;

{$IFDEF MSWINDOWS}

constructor TFFAudioOutput.Create(ASampleRate: Integer; AChannels: Integer);
begin
  inherited Create;
  FImpl := TFFAudioOutputWin.Create(ASampleRate, AChannels);
end;

{$ENDIF}

{$IFDEF LINUX}

constructor TFFAudioOutput.Create(ASampleRate: Integer; AChannels: Integer);
begin
  inherited Create;
  try
    FImpl := TFFAudioOutputALSA.Create(ASampleRate, AChannels);
  except
    on E: EFFException do
    begin
      if TFFAudioOutputSDL.IsAvailable then
        FImpl := TFFAudioOutputSDL.Create(ASampleRate, AChannels)
      else
        raise;
    end;
  end;
end;

{$ENDIF}

{$IFNDEF MSWINDOWS}
{$IFNDEF LINUX}

constructor TFFAudioOutput.Create(ASampleRate: Integer; AChannels: Integer);
begin
  inherited Create;
  raise EFFException.Create('TFFAudioOutput is not supported on this platform');
end;

{$ENDIF}
{$ENDIF}

destructor TFFAudioOutput.Destroy;
begin
  FImpl.Free;
  inherited;
end;

procedure TFFAudioOutput.Flush;
begin
  if FImpl is TFFAudioOutputBase then
  begin
    {$IFDEF MSWINDOWS}
    TFFAudioOutputWin(FImpl).Flush;
    {$ENDIF}
    {$IFDEF LINUX}
    if FImpl is TFFAudioOutputSDL then
      TFFAudioOutputSDL(FImpl).Flush
    else if FImpl is TFFAudioOutputALSA then
      TFFAudioOutputALSA(FImpl).Flush;
    {$ENDIF}
  end;
end;

function TFFAudioOutput.GetVolume: Single;
begin
  if FImpl is TFFAudioOutputBase then
    Result := TFFAudioOutputBase(FImpl).Volume
  else
    Result := 1.0;
end;

procedure TFFAudioOutput.Pause;
begin
  {$IFDEF MSWINDOWS}
  TFFAudioOutputWin(FImpl).Pause;
  {$ENDIF}
  {$IFDEF LINUX}
  if FImpl is TFFAudioOutputSDL then
    TFFAudioOutputSDL(FImpl).Pause
  else
    TFFAudioOutputALSA(FImpl).Pause;
  {$ENDIF}
end;

procedure TFFAudioOutput.Resume;
begin
  {$IFDEF MSWINDOWS}
  TFFAudioOutputWin(FImpl).Resume;
  {$ENDIF}
  {$IFDEF LINUX}
  if FImpl is TFFAudioOutputSDL then
    TFFAudioOutputSDL(FImpl).Resume
  else
    TFFAudioOutputALSA(FImpl).Resume;
  {$ENDIF}
end;

procedure TFFAudioOutput.SetVolume(const Value: Single);
begin
  if FImpl is TFFAudioOutputBase then
    TFFAudioOutputBase(FImpl).Volume := Value;
end;

procedure TFFAudioOutput.Write(const ABuffer: Pointer; AByteCount: Integer);
begin
  {$IFDEF MSWINDOWS}
  TFFAudioOutputWin(FImpl).Write(ABuffer, AByteCount);
  {$ENDIF}
  {$IFDEF LINUX}
  if FImpl is TFFAudioOutputSDL then
    TFFAudioOutputSDL(FImpl).Write(ABuffer, AByteCount)
  else
    TFFAudioOutputALSA(FImpl).Write(ABuffer, AByteCount);
  {$ENDIF}
end;

end.
