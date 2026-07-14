unit uFFAudioOutputWin;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$POINTERMATH ON}

{ Windows WaveOut PCM playback. }

interface

{$IFDEF MSWINDOWS}

uses
  Winapi.Windows,
  Winapi.MMSystem,
  System.SysUtils,
  System.Classes,
  uFFException,
  uFFAudioResampler,
  uFFAudioOutputBase;

type
  TFFAudioOutputWin = class(TFFAudioOutputBase)
  private
    FWaveOut: HWAVEOUT;
    FWaveFormat: TWAVEFORMATEX;
    FHdr: array [0 .. 7] of TWAVEHDR;
    FBuffers: array [0 .. 7] of TBytes;
    FWriteIndex: Integer;
    procedure WaitForSlot(ASlot: Integer);
  public
    constructor Create(ASampleRate: Integer = FFAudioDefaultSampleRate;
      AChannels: Word = FFAudioDefaultChannels);
    destructor Destroy; override;

    procedure Write(const ABuffer: Pointer; AByteCount: Integer);
    procedure Pause;
    procedure Resume;
    procedure Flush;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

const
  BufferBytes = 16384;

constructor TFFAudioOutputWin.Create(ASampleRate: Integer; AChannels: Word);
var
  I: Integer;
  Ret: MMRESULT;
begin
  inherited Create;
  FWriteIndex := 0;
  FWaveOut := 0;

  FillChar(FWaveFormat, SizeOf(FWaveFormat), 0);
  FWaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  FWaveFormat.nChannels := AChannels;
  FWaveFormat.nSamplesPerSec := ASampleRate;
  FWaveFormat.wBitsPerSample := 16;
  FWaveFormat.nBlockAlign := FWaveFormat.nChannels * (FWaveFormat.wBitsPerSample div 8);
  FWaveFormat.nAvgBytesPerSec := FWaveFormat.nSamplesPerSec * FWaveFormat.nBlockAlign;

  Ret := waveOutOpen(@FWaveOut, WAVE_MAPPER, @FWaveFormat, 0, 0, CALLBACK_NULL);
  if Ret <> MMSYSERR_NOERROR then
    raise EFFException.CreateFmt('waveOutOpen failed (%d)', [Ret]);

  for I := Low(FHdr) to High(FHdr) do
  begin
    SetLength(FBuffers[I], BufferBytes);
    FillChar(FHdr[I], SizeOf(TWAVEHDR), 0);
    FHdr[I].dwFlags := WHDR_DONE;
  end;
end;

destructor TFFAudioOutputWin.Destroy;
begin
  Flush;
  if FWaveOut <> 0 then
  begin
    waveOutClose(FWaveOut);
    FWaveOut := 0;
  end;
  inherited;
end;

procedure TFFAudioOutputWin.Flush;
var
  I: Integer;
begin
  if FWaveOut = 0 then
    Exit;
  BeginDrain;
  try
    waveOutReset(FWaveOut);
    for I := Low(FHdr) to High(FHdr) do
    begin
      if (FHdr[I].dwFlags and WHDR_PREPARED) <> 0 then
        waveOutUnprepareHeader(FWaveOut, @FHdr[I], SizeOf(TWAVEHDR));
      FHdr[I].dwFlags := WHDR_DONE;
    end;
  finally
    EndDrain;
  end;
end;

procedure TFFAudioOutputWin.Pause;
begin
  if (FWaveOut <> 0) and not Paused then
  begin
    waveOutPause(FWaveOut);
    Paused := True;
  end;
end;

procedure TFFAudioOutputWin.Resume;
begin
  if (FWaveOut <> 0) and Paused then
  begin
    waveOutRestart(FWaveOut);
    Paused := False;
  end;
end;

procedure TFFAudioOutputWin.WaitForSlot(ASlot: Integer);
var
  Spins: Integer;
begin
  Spins := 0;
  while (FHdr[ASlot].dwFlags and WHDR_DONE) = 0 do
  begin
    if Draining then
      Exit;
    Inc(Spins);
    if Spins > 30000 then
    begin
      waveOutReset(FWaveOut);
      if (FHdr[ASlot].dwFlags and WHDR_PREPARED) <> 0 then
        waveOutUnprepareHeader(FWaveOut, @FHdr[ASlot], SizeOf(TWAVEHDR));
      FHdr[ASlot].dwFlags := WHDR_DONE;
      Exit;
    end;
    Sleep(1);
  end;
  if (FHdr[ASlot].dwFlags and WHDR_PREPARED) <> 0 then
    waveOutUnprepareHeader(FWaveOut, @FHdr[ASlot], SizeOf(TWAVEHDR));
end;

procedure TFFAudioOutputWin.Write(const ABuffer: Pointer; AByteCount: Integer);
var
  Slot: Integer;
  Ret: MMRESULT;
  SampleCount: Integer;
begin
  if (FWaveOut = 0) or (ABuffer = nil) or (AByteCount <= 0) then
    Exit;
  if AByteCount > BufferBytes then
    raise EFFException.Create('TFFAudioOutputWin.Write: buffer too large');

  Slot := FWriteIndex mod Length(FHdr);
  WaitForSlot(Slot);

  Move(ABuffer^, FBuffers[Slot][0], AByteCount);
  SampleCount := AByteCount div SizeOf(SmallInt);
  ApplyVolume(PSmallInt(@FBuffers[Slot][0]), SampleCount);

  FHdr[Slot].lpData := @FBuffers[Slot][0];
  FHdr[Slot].dwBufferLength := AByteCount;
  FHdr[Slot].dwFlags := 0;

  Ret := waveOutPrepareHeader(FWaveOut, @FHdr[Slot], SizeOf(TWAVEHDR));
  if Ret <> MMSYSERR_NOERROR then
    raise EFFException.CreateFmt('waveOutPrepareHeader failed (%d)', [Ret]);

  Ret := waveOutWrite(FWaveOut, @FHdr[Slot], SizeOf(TWAVEHDR));
  if Ret <> MMSYSERR_NOERROR then
    raise EFFException.CreateFmt('waveOutWrite failed (%d)', [Ret]);

  Inc(FWriteIndex);
end;

{$ENDIF}

end.
