unit uFFAudioOutputALSA;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$POINTERMATH ON}

{ Linux ALSA PCM playback (S16 interleaved). }

interface

{$IFDEF LINUX}

uses
  System.Classes,
  System.SysUtils,
  uFFException,
  uFFAudioResampler,
  uFFAudioOutputBase;

type
  TFFAudioOutputALSA = class(TFFAudioOutputBase)
  private
    FPcm: Pointer;
    FSampleRate: Integer;
    FChannels: Integer;
    FBuffer: TBytes;
    procedure CheckAlsa(Ret: Integer; const AMsg: string);
  public
    constructor Create(ASampleRate: Integer = FFAudioDefaultSampleRate;
      AChannels: Integer = FFAudioDefaultChannels);
    destructor Destroy; override;

    procedure Write(const ABuffer: Pointer; AByteCount: Integer);
    procedure Pause;
    procedure Resume;
    procedure Flush;
  end;

{$ENDIF}

implementation

{$IFDEF LINUX}

const
  SND_PCM_STREAM_PLAYBACK = 0;
  SND_PCM_FORMAT_S16_LE   = 2;

type
  Psnd_pcm_t = Pointer;

function snd_pcm_open(var pcm: Psnd_pcm_t; const name: PAnsiChar; stream: Integer; mode: Integer): Integer; cdecl;
  external 'libasound.so.2' name 'snd_pcm_open';
function snd_pcm_close(pcm: Psnd_pcm_t): Integer; cdecl;
  external 'libasound.so.2' name 'snd_pcm_close';
function snd_pcm_set_params(pcm: Psnd_pcm_t; format: Integer; access: Integer; channels: Cardinal;
  rate: Cardinal; soft_resample: Integer; latency: Cardinal): Integer; cdecl;
  external 'libasound.so.2' name 'snd_pcm_set_params';
function snd_pcm_writei(pcm: Psnd_pcm_t; buffer: Pointer; size: NativeInt): NativeInt; cdecl;
  external 'libasound.so.2' name 'snd_pcm_writei';
function snd_pcm_prepare(pcm: Psnd_pcm_t): Integer; cdecl;
  external 'libasound.so.2' name 'snd_pcm_prepare';
function snd_pcm_drop(pcm: Psnd_pcm_t): Integer; cdecl;
  external 'libasound.so.2' name 'snd_pcm_drop';
function snd_pcm_pause(pcm: Psnd_pcm_t; enable: Integer): Integer; cdecl;
  external 'libasound.so.2' name 'snd_pcm_pause';

constructor TFFAudioOutputALSA.Create(ASampleRate: Integer; AChannels: Integer);
var
  Ret: Integer;
  Pcm: Psnd_pcm_t;
begin
  inherited Create;
  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FPcm := nil;
  Pcm := nil;
  Ret := snd_pcm_open(Pcm, 'default', SND_PCM_STREAM_PLAYBACK, 0);
  FPcm := Pcm;
  CheckAlsa(Ret, 'snd_pcm_open');
  Ret := snd_pcm_set_params(FPcm, SND_PCM_FORMAT_S16_LE, 3, FChannels, FSampleRate, 1, 500000);
  CheckAlsa(Ret, 'snd_pcm_set_params');
  SetLength(FBuffer, 16384);
end;

destructor TFFAudioOutputALSA.Destroy;
begin
  Flush;
  if FPcm <> nil then
    snd_pcm_close(FPcm);
  FPcm := nil;
  inherited;
end;

procedure TFFAudioOutputALSA.CheckAlsa(Ret: Integer; const AMsg: string);
begin
  if Ret < 0 then
    raise EFFException.CreateFmt('%s failed (%d)', [AMsg, Ret]);
end;

procedure TFFAudioOutputALSA.Flush;
begin
  if FPcm = nil then
    Exit;
  snd_pcm_drop(FPcm);
  snd_pcm_prepare(FPcm);
end;

procedure TFFAudioOutputALSA.Pause;
begin
  if (FPcm <> nil) and not Paused then
  begin
    snd_pcm_pause(FPcm, 1);
    Paused := True;
  end;
end;

procedure TFFAudioOutputALSA.Resume;
begin
  if (FPcm <> nil) and Paused then
  begin
    snd_pcm_pause(FPcm, 0);
    Paused := False;
  end;
end;

procedure TFFAudioOutputALSA.Write(const ABuffer: Pointer; AByteCount: Integer);
var
  SampleCount: Integer;
  Frames: NativeInt;
  Ret: NativeInt;
begin
  if (FPcm = nil) or (ABuffer = nil) or (AByteCount <= 0) then
    Exit;
  if AByteCount > Length(FBuffer) then
    raise EFFException.Create('TFFAudioOutputALSA.Write: buffer too large');

  Move(ABuffer^, FBuffer[0], AByteCount);
  SampleCount := AByteCount div SizeOf(SmallInt);
  ApplyVolume(PSmallInt(@FBuffer[0]), SampleCount);
  Frames := SampleCount div FChannels;
  Ret := snd_pcm_writei(FPcm, @FBuffer[0], Frames);
  if Ret < 0 then
    CheckAlsa(snd_pcm_prepare(FPcm), 'snd_pcm_prepare after write error');
end;

{$ENDIF}

end.
