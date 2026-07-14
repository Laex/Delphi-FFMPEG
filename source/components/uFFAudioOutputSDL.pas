unit uFFAudioOutputSDL;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$POINTERMATH ON}

{ SDL2 audio playback fallback (S16 interleaved). }

interface

{$IFDEF LINUX}

uses
  System.Classes,
  System.SysUtils,
  {$IFDEF FPC}
  dynlibs,
  {$ELSE}
  Winapi.Windows,
  {$ENDIF}
  uFFException,
  uFFAudioResampler,
  uFFAudioOutputBase;

type
  TFFAudioOutputSDL = class(TFFAudioOutputBase)
  private
    FDevice: UInt32;
    FSampleRate: Integer;
    FChannels: Integer;
    FBuffer: TBytes;
    FLoaded: Boolean;
    procedure LoadSdl;
    procedure CheckSdl(Ret: Integer; const AMsg: string);
  public
    constructor Create(ASampleRate: Integer = FFAudioDefaultSampleRate;
      AChannels: Integer = FFAudioDefaultChannels);
    destructor Destroy; override;

    class function IsAvailable: Boolean;

    procedure Write(const ABuffer: Pointer; AByteCount: Integer);
    procedure Pause;
    procedure Resume;
    procedure Flush;
  end;

{$ENDIF}

implementation

{$IFDEF LINUX}

const
  SDL_INIT_AUDIO          = $00000010;
  SDL_AUDIO_ALLOW_ANY_CHANGE = 1 shl 16;
  SDL_AUDIO_S16           = $8010;

type
  TSDL_AudioSpec = record
    freq: Integer;
    format: UInt16;
    channels: UInt8;
    silence: UInt8;
    samples: UInt16;
    padding: UInt16;
    size: UInt32;
    callback: Pointer;
    userdata: Pointer;
  end;

function SDL_Init(flags: UInt32): Integer; cdecl; external 'libSDL2-2.0.so.0';
function SDL_Quit: void; cdecl; external 'libSDL2-2.0.so.0';
function SDL_GetError: PAnsiChar; cdecl; external 'libSDL2-2.0.so.0';
function SDL_OpenAudioDevice(const device: PAnsiChar; iscapture: Integer; desired: PSDL_AudioSpec;
  obtained: PSDL_AudioSpec; allowed_changes: Integer): UInt32; cdecl; external 'libSDL2-2.0.so.0';
procedure SDL_CloseAudioDevice(dev: UInt32); cdecl; external 'libSDL2-2.0.so.0';
function SDL_QueueAudio(dev: UInt32; const data: Pointer; len: UInt32): Integer; cdecl; external 'libSDL2-2.0.so.0';
procedure SDL_ClearQueuedAudio(dev: UInt32); cdecl; external 'libSDL2-2.0.so.0';
procedure SDL_PauseAudioDevice(dev: UInt32; pause_on: Integer); cdecl; external 'libSDL2-2.0.so.0';

var
  GSdlRefCount: Integer = 0;

class function TFFAudioOutputSDL.IsAvailable: Boolean;
{$IFDEF FPC}
var
  H: TLibHandle;
{$ELSE}
var
  H: THandle;
{$ENDIF}
begin
  Result := False;
  {$IFDEF FPC}
  H := LoadLibrary('libSDL2-2.0.so.0');
  if H <> NilHandle then
  begin
    UnloadLibrary(H);
    Result := True;
  end;
  {$ELSE}
  H := LoadLibrary('libSDL2-2.0.so.0');
  if H <> 0 then
  begin
    FreeLibrary(H);
    Result := True;
  end;
  {$ENDIF}
end;

constructor TFFAudioOutputSDL.Create(ASampleRate: Integer; AChannels: Integer);
var
  Desired: TSDL_AudioSpec;
begin
  inherited Create;
  if not IsAvailable then
    raise EFFException.Create('SDL2 library not found (libSDL2-2.0.so.0)');

  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FDevice := 0;
  SetLength(FBuffer, 16384);

  LoadSdl;
  FillChar(Desired, SizeOf(Desired), 0);
  Desired.freq := FSampleRate;
  Desired.format := SDL_AUDIO_S16;
  Desired.channels := FChannels;
  Desired.samples := 1024;
  FDevice := SDL_OpenAudioDevice(nil, 0, @Desired, nil, SDL_AUDIO_ALLOW_ANY_CHANGE);
  if FDevice = 0 then
    raise EFFException.CreateFmt('SDL_OpenAudioDevice failed: %s', [string(SDL_GetError())]);
  SDL_PauseAudioDevice(FDevice, 0);
  FLoaded := True;
end;

destructor TFFAudioOutputSDL.Destroy;
begin
  if FLoaded then
  begin
    Flush;
    if FDevice <> 0 then
      SDL_CloseAudioDevice(FDevice);
    FDevice := 0;
    Dec(GSdlRefCount);
    if GSdlRefCount <= 0 then
    begin
      GSdlRefCount := 0;
      SDL_Quit;
    end;
    FLoaded := False;
  end;
  inherited;
end;

procedure TFFAudioOutputSDL.CheckSdl(Ret: Integer; const AMsg: string);
begin
  if Ret <> 0 then
    raise EFFException.CreateFmt('%s failed: %s', [AMsg, string(SDL_GetError())]);
end;

procedure TFFAudioOutputSDL.Flush;
begin
  if FDevice <> 0 then
    SDL_ClearQueuedAudio(FDevice);
end;

procedure TFFAudioOutputSDL.LoadSdl;
begin
  if GSdlRefCount = 0 then
    CheckSdl(SDL_Init(SDL_INIT_AUDIO), 'SDL_Init');
  Inc(GSdlRefCount);
end;

procedure TFFAudioOutputSDL.Pause;
begin
  if (FDevice <> 0) and not Paused then
  begin
    SDL_PauseAudioDevice(FDevice, 1);
    Paused := True;
  end;
end;

procedure TFFAudioOutputSDL.Resume;
begin
  if (FDevice <> 0) and Paused then
  begin
    SDL_PauseAudioDevice(FDevice, 0);
    Paused := False;
  end;
end;

procedure TFFAudioOutputSDL.Write(const ABuffer: Pointer; AByteCount: Integer);
var
  SampleCount: Integer;
begin
  if (FDevice = 0) or (ABuffer = nil) or (AByteCount <= 0) then
    Exit;
  if AByteCount > Length(FBuffer) then
    raise EFFException.Create('TFFAudioOutputSDL.Write: buffer too large');

  Move(ABuffer^, FBuffer[0], AByteCount);
  SampleCount := AByteCount div SizeOf(SmallInt);
  ApplyVolume(PSmallInt(@FBuffer[0]), SampleCount);
  CheckSdl(SDL_QueueAudio(FDevice, @FBuffer[0], AByteCount), 'SDL_QueueAudio');
end;

{$ENDIF}

end.
