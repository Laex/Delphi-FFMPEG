unit uFFPlaybackEngine;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Cross-platform A/V playback engine (demux queues + playback clock). }

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  ffmpeg_types,
  libavutil,
  libavformat,
  uFFException,
  uFFReader,
  uFFDecoder,
  uFFPacket,
  uFFPacketQueue,
  uFFPlaybackClock,
  uFFFrame,
  uFFFrameConverter,
  uFFAudioResampler,
  uFFAudioOutput,
  uFFHooks,
  uFFHardwareDecode,
  uFFDesignTime;

type
  TFFPlayerState = (psStopped, psPlaying, psPaused);

  TFFPresentFrameEvent = procedure(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer) of object;
  TFFPositionChangeEvent = procedure(Sender: TObject; APositionMs: Int64) of object;

  TFFPlaybackEngine = class
  private
    FFileName: string;
    FState: TFFPlayerState;
    FPosition: Int64;
    FDuration: Int64;
    FVolume: Single;
    FVideoStreamIndex: Integer;
    FAudioStreamIndex: Integer;
    FAudioOutput: TFFAudioOutput;
    FPlayThread: TThread;
    FVideoQueue: TFFPacketQueue;
    FAudioQueue: TFFPacketQueue;
    FClock: TFFPlaybackClock;
    FStopRequested: Boolean;
    FPaused: Boolean;
    FSeekStartMs: Int64;
    FPlayStartMs: Int64;
    FPlaybackCleanedUp: Boolean;
    FDestroying: Boolean;
    FOnPresentFrame: TFFPresentFrameEvent;
    FOnPositionChange: TFFPositionChangeEvent;
    FOnStateChange: TNotifyEvent;
    FOnFrameHook: TFFFrameHookEvent;
    FOnVideoHook: TFFVideoHookEvent;
    FOnAudioHook: TFFAudioHookEvent;
    FHardwareDevice: TFFHardwareDevice;
    procedure SetFileName(const Value: string);
    procedure SetHardwareDevice(const Value: TFFHardwareDevice);
    procedure SetState(const Value: TFFPlayerState);
    procedure SetVolume(const Value: Single);
    procedure DoStateChange;
    procedure QueuePlaybackEnded;
    procedure CleanupPlayback;
    procedure PresentFrame(ABgra: PByte; AWidth, AHeight, AStride: Integer);
    procedure NotifyPositionChange(APositionMs: Int64);
    function InvokeFrameHook(AFrame: TFFFrame): Boolean;
    procedure InvokeVideoHook(ABgra: PByte; AWidth, AHeight, AStride: Integer);
    procedure InvokeAudioHook(ABuffer: PByte; var AByteCount: Integer);
    procedure ProbeDuration;
    function GetDuration: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure SeekTo(APositionMs: Int64);
    property FileName: string read FFileName write SetFileName;
    property State: TFFPlayerState read FState;
    property Position: Int64 read FPosition;
    property Duration: Int64 read GetDuration;
    property Volume: Single read FVolume write SetVolume;
    property HardwareDevice: TFFHardwareDevice read FHardwareDevice write SetHardwareDevice default ffhdNone;
    property VideoStreamIndex: Integer read FVideoStreamIndex;
    property AudioStreamIndex: Integer read FAudioStreamIndex;
    property StopRequested: Boolean read FStopRequested;
    property OnPresentFrame: TFFPresentFrameEvent read FOnPresentFrame write FOnPresentFrame;
    property OnPositionChange: TFFPositionChangeEvent read FOnPositionChange write FOnPositionChange;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnFrameHook: TFFFrameHookEvent read FOnFrameHook write FOnFrameHook;
    property OnVideoHook: TFFVideoHookEvent read FOnVideoHook write FOnVideoHook;
    property OnAudioHook: TFFAudioHookEvent read FOnAudioHook write FOnAudioHook;
  end;

implementation

const
  QueueMaxPackets = 64;

type
  TFFPlayCoordinator = class(TThread)
  private
    FEngine: TFFPlaybackEngine;
    FDemuxThread: TThread;
    FVideoThread: TThread;
    FAudioThread: TThread;
    FHasAudio: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TFFPlaybackEngine);
    destructor Destroy; override;
  end;

  TFFDemuxThread = class(TThread)
  private
    FEngine: TFFPlaybackEngine;
    FHasAudio: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TFFPlaybackEngine; AHasAudio: Boolean);
  end;

  TFFVideoDecodeThread = class(TThread)
  private
    FEngine: TFFPlaybackEngine;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TFFPlaybackEngine);
  end;

  TFFAudioDecodeThread = class(TThread)
  private
    FEngine: TFFPlaybackEngine;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TFFPlaybackEngine);
  end;

function PtsToMs(APts: Int64; const ATimeBase: AVRational): Int64;
var
  MsBase: AVRational;
begin
  if APts = AV_NOPTS_VALUE then
    Exit(-1);
  MsBase.num := 1;
  MsBase.den := 1000;
  Result := av_rescale_q(APts, ATimeBase, MsBase);
end;

function FindStreamIndex(const Reader: TFFReader; AType: AVMediaType): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Reader.Streams.Count - 1 do
    if Reader.Streams.GetInfo(I).MediaType = AType then
      Exit(I);
end;

function FramePtsMs(AFrame: PAVFrame; AStream: PAVStream): Int64;
begin
  Result := PtsToMs(AFrame^.best_effort_timestamp, AStream^.time_base);
  if Result < 0 then
    Result := PtsToMs(AFrame^.pts, AStream^.time_base);
end;

{ TFFDemuxThread }

constructor TFFDemuxThread.Create(AEngine: TFFPlaybackEngine; AHasAudio: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEngine := AEngine;
  FHasAudio := AHasAudio;
end;

procedure TFFDemuxThread.Execute;
var
  Reader: TFFReader;
  Packet: TFFPacket;
  VideoStream: PAVStream;
  MsBase: AVRational;
  SeekTs: Int64;
begin
  Reader := TFFReader.Create(nil);
  Packet := TFFPacket.Create;
  try
    Reader.FileName := FEngine.FFileName;
    Reader.Open;

    if FEngine.FPlayStartMs > 0 then
    begin
      VideoStream := Reader.GetStream(FEngine.FVideoStreamIndex);
      MsBase.num := 1;
      MsBase.den := 1000;
      SeekTs := av_rescale_q(FEngine.FPlayStartMs, MsBase, VideoStream^.time_base);
      Reader.Seek(SeekTs, FEngine.FVideoStreamIndex);
    end;

    while not Terminated and not FEngine.FStopRequested do
    begin
      if FEngine.FPaused then
      begin
        Sleep(10);
        Continue;
      end;

      while (FEngine.FVideoQueue <> nil) and (FEngine.FVideoQueue.Count >= QueueMaxPackets) do
      begin
        if Terminated or FEngine.FStopRequested then
          Break;
        Sleep(5);
      end;

      if Terminated or FEngine.FStopRequested then
        Break;

      if not Reader.ReadPacket(Packet) then
        Break;

      if Packet.Raw^.stream_index = FEngine.FVideoStreamIndex then
      begin
        if FEngine.FVideoQueue.Count < QueueMaxPackets then
          FEngine.FVideoQueue.Push(Packet);
      end
      else if FHasAudio and (Packet.Raw^.stream_index = FEngine.FAudioStreamIndex) and
        (FEngine.FAudioQueue <> nil) and (FEngine.FAudioQueue.Count < QueueMaxPackets) then
        FEngine.FAudioQueue.Push(Packet);
    end;
  finally
    Packet.Free;
    Reader.Free;
    if FEngine.FVideoQueue <> nil then
      FEngine.FVideoQueue.Close;
    if FEngine.FAudioQueue <> nil then
      FEngine.FAudioQueue.Close;
  end;
end;

{ TFFVideoDecodeThread }

constructor TFFVideoDecodeThread.Create(AEngine: TFFPlaybackEngine);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEngine := AEngine;
end;

procedure TFFVideoDecodeThread.Execute;
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Packet: TFFPacket;
  Frame: TFFFrame;
  Converter: TFFFrameConverter;
  VideoStream: PAVStream;
  Ret: Integer;
  FrameMs: Int64;
  Converted: PAVFrame;
begin
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Frame := TFFFrame.Create;
  Converter := TFFFrameConverter.Create;
  Packet := nil;
  try
    Reader.FileName := FEngine.FFileName;
    Reader.Open;
    VideoStream := Reader.GetStream(FEngine.FVideoStreamIndex);

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FEngine.FVideoStreamIndex;
    Decoder.HardwareDevice := FEngine.FHardwareDevice;
    Decoder.Initialize;

    while not Terminated and not FEngine.FStopRequested do
    begin
      if FEngine.FVideoQueue = nil then
        Break;

      Packet := FEngine.FVideoQueue.Pop(100);
      if Packet = nil then
      begin
        if (FEngine.FVideoQueue = nil) or FEngine.FVideoQueue.Closed then
          Break;
        Continue;
      end;

      try
        Ret := Decoder.SendPacket(Packet);
        if Ret < 0 then
          Continue;

        while not Terminated and not FEngine.FStopRequested do
        begin
          Ret := Decoder.ReceiveFrame(Frame);
          if Ret = AVERROR_EAGAIN then
            Break;
          if Ret = AVERROR_EOF then
            Exit;
          if Ret < 0 then
            raise EFFException.CreateFmt('avcodec_receive_frame failed (%d)', [Ret]);

          if FEngine.InvokeFrameHook(Frame) then
            Continue;

          FrameMs := FramePtsMs(Frame.Raw, VideoStream);
          if (FEngine.FClock <> nil) and (FrameMs >= 0) and not FEngine.FClock.IsLate(FrameMs) then
            FEngine.FClock.WaitUntil(FrameMs, FEngine.FStopRequested, FEngine.FPaused);
          if FEngine.FStopRequested then
            Exit;

          Converted := Converter.Convert(Frame);
          FEngine.InvokeVideoHook(Converted^.data[0], Converter.DstWidth, Converter.DstHeight,
            Converted^.linesize[0]);
          FEngine.PresentFrame(Converted^.data[0], Converter.DstWidth, Converter.DstHeight,
            Converted^.linesize[0]);

          if FrameMs >= 0 then
            FEngine.NotifyPositionChange(FrameMs);
        end;
      finally
        Packet.Free;
        Packet := nil;
      end;
    end;
  finally
    if Packet <> nil then
      Packet.Free;
    Converter.Free;
    Frame.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

{ TFFAudioDecodeThread }

constructor TFFAudioDecodeThread.Create(AEngine: TFFPlaybackEngine);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEngine := AEngine;
end;

procedure TFFAudioDecodeThread.Execute;
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Packet: TFFPacket;
  Frame: TFFFrame;
  Resampler: TFFAudioResampler;
  AudioOutput: TFFAudioOutput;
  AudioStream: PAVStream;
  Ret: Integer;
  Pcm: PByte;
  PcmBytes: Integer;
  FrameMs: Int64;
  ResamplerReady: Boolean;
begin
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Frame := TFFFrame.Create;
  Resampler := TFFAudioResampler.Create;
  AudioOutput := nil;
  Packet := nil;
  ResamplerReady := False;
  try
    Reader.FileName := FEngine.FFileName;
    Reader.Open;
    AudioStream := Reader.GetStream(FEngine.FAudioStreamIndex);

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FEngine.FAudioStreamIndex;
    Decoder.Initialize;

    AudioOutput := TFFAudioOutput.Create;
    FEngine.FAudioOutput := AudioOutput;

    while not Terminated and not FEngine.FStopRequested do
    begin
      Packet := FEngine.FAudioQueue.Pop(100);
      if Packet = nil then
      begin
        if FEngine.FAudioQueue.Closed then
          Break;
        Continue;
      end;

      try
        Ret := Decoder.SendPacket(Packet);
        if Ret < 0 then
          Continue;

        while not Terminated and not FEngine.FStopRequested do
        begin
          while FEngine.FPaused and not FEngine.FStopRequested do
            Sleep(10);

          Ret := Decoder.ReceiveFrame(Frame);
          if Ret = AVERROR_EAGAIN then
            Break;
          if Ret = AVERROR_EOF then
            Exit;
          if Ret < 0 then
            raise EFFException.CreateFmt('avcodec_receive_frame (audio) failed (%d)', [Ret]);

          if FEngine.InvokeFrameHook(Frame) then
            Continue;

          if not ResamplerReady then
          begin
            Resampler.Configure(@Frame.Raw^.ch_layout, Frame.Raw^.sample_rate, AVSampleFormat(Frame.Raw^.format));
            ResamplerReady := True;
          end;

          Resampler.Convert(Frame.Raw, Pcm, PcmBytes);
          if PcmBytes > 0 then
          begin
            FEngine.InvokeAudioHook(Pcm, PcmBytes);
            if PcmBytes > 0 then
            begin
              AudioOutput.Volume := FEngine.FVolume;
              AudioOutput.Write(Pcm, PcmBytes);
            end;
          end;

          FrameMs := FramePtsMs(Frame.Raw, AudioStream);
          if FrameMs >= 0 then
          begin
            FEngine.FClock.SetAudioTimeMs(FrameMs);
            FEngine.NotifyPositionChange(FrameMs);
          end;
        end;
      finally
        Packet.Free;
        Packet := nil;
      end;
    end;

    Decoder.SendPacket(nil);
    while Decoder.ReceiveFrame(Frame) = 0 do
    begin
      if not ResamplerReady then
        Break;
      Resampler.Convert(Frame.Raw, Pcm, PcmBytes);
      if PcmBytes > 0 then
      begin
        FEngine.InvokeAudioHook(Pcm, PcmBytes);
        if PcmBytes > 0 then
          AudioOutput.Write(Pcm, PcmBytes);
      end;
    end;
  finally
    FEngine.FAudioOutput := nil;
    if AudioOutput <> nil then
      AudioOutput.Free;
    if Packet <> nil then
      Packet.Free;
    Resampler.Free;
    Frame.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

{ TFFPlayCoordinator }

constructor TFFPlayCoordinator.Create(AEngine: TFFPlaybackEngine);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEngine := AEngine;
  FHasAudio := False;
end;

destructor TFFPlayCoordinator.Destroy;
begin
  if FDemuxThread <> nil then
    FDemuxThread.Free;
  if FVideoThread <> nil then
    FVideoThread.Free;
  if FAudioThread <> nil then
    FAudioThread.Free;
  inherited;
end;

procedure TFFPlayCoordinator.Execute;
var
  Reader: TFFReader;
begin
  Reader := TFFReader.Create(nil);
  try
    Reader.FileName := FEngine.FFileName;
    Reader.Open;

    FEngine.FVideoStreamIndex := FindStreamIndex(Reader, AVMEDIA_TYPE_VIDEO);
    FEngine.FAudioStreamIndex := FindStreamIndex(Reader, AVMEDIA_TYPE_AUDIO);
    if FEngine.FVideoStreamIndex < 0 then
      raise EFFException.Create('TFFPlaybackEngine: no video stream');

    FHasAudio := FEngine.FAudioStreamIndex >= 0;
    if Reader.Duration > 0 then
      FEngine.FDuration := Reader.Duration div 1000;
  finally
    Reader.Free;
  end;

  FEngine.FClock.Reset(FEngine.FPlayStartMs, FHasAudio);

  FDemuxThread := TFFDemuxThread.Create(FEngine, FHasAudio);
  FVideoThread := TFFVideoDecodeThread.Create(FEngine);
  if FHasAudio then
    FAudioThread := TFFAudioDecodeThread.Create(FEngine);

  FDemuxThread.Start;
  FVideoThread.Start;
  if FHasAudio then
    FAudioThread.Start;

  FDemuxThread.WaitFor;
  FVideoThread.WaitFor;
  if FAudioThread <> nil then
    FAudioThread.WaitFor;

  FreeAndNil(FDemuxThread);
  FreeAndNil(FVideoThread);
  FreeAndNil(FAudioThread);

  TThread.Queue(nil, FEngine.QueuePlaybackEnded);
end;

{ TFFPlaybackEngine }

constructor TFFPlaybackEngine.Create;
begin
  inherited Create;
  FState := psStopped;
  FVolume := 1.0;
  FHardwareDevice := ffhdNone;
  FVideoStreamIndex := -1;
  FAudioStreamIndex := -1;
  FAudioOutput := nil;
end;

destructor TFFPlaybackEngine.Destroy;
var
  I: Integer;
begin
  FDestroying := True;
  if FFIsDesignTime(nil) then
  begin
    inherited;
    Exit;
  end;
  Stop;
  for I := 1 to 50 do
    Sleep(10);
  inherited;
end;

procedure TFFPlaybackEngine.DoStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TFFPlaybackEngine.PresentFrame(ABgra: PByte; AWidth, AHeight, AStride: Integer);
begin
  if FDestroying then
    Exit;
  if Assigned(FOnPresentFrame) then
    FOnPresentFrame(Self, ABgra, AWidth, AHeight, AStride);
end;

function TFFPlaybackEngine.InvokeFrameHook(AFrame: TFFFrame): Boolean;
begin
  Result := False;
  if Assigned(FOnFrameHook) then
    FOnFrameHook(Self, AFrame, Result);
end;

procedure TFFPlaybackEngine.InvokeVideoHook(ABgra: PByte; AWidth, AHeight, AStride: Integer);
begin
  if Assigned(FOnVideoHook) then
    FOnVideoHook(Self, ABgra, AWidth, AHeight, AStride);
end;

procedure TFFPlaybackEngine.InvokeAudioHook(ABuffer: PByte; var AByteCount: Integer);
begin
  if Assigned(FOnAudioHook) then
    FOnAudioHook(Self, ABuffer, AByteCount);
end;

procedure TFFPlaybackEngine.NotifyPositionChange(APositionMs: Int64);
var
  PosMs: Int64;
begin
  FPosition := APositionMs;
  PosMs := APositionMs;
  TThread.Queue(nil,
    procedure
    begin
      if FDestroying then
        Exit;
      if Assigned(FOnPositionChange) then
        FOnPositionChange(Self, PosMs);
    end);
end;

procedure TFFPlaybackEngine.ProbeDuration;
var
  Reader: TFFReader;
begin
  if (FDuration > 0) or (FFileName = '') then
    Exit;
  Reader := TFFReader.Create(nil);
  try
    Reader.FileName := FFileName;
    Reader.Open;
    if Reader.Duration > 0 then
      FDuration := Reader.Duration div 1000;
  finally
    Reader.Free;
  end;
end;

function TFFPlaybackEngine.GetDuration: Int64;
begin
  if FDuration <= 0 then
    ProbeDuration;
  Result := FDuration;
end;

procedure TFFPlaybackEngine.Pause;
begin
  if FState <> psPlaying then
    Exit;
  FPaused := True;
  if FClock <> nil then
    FClock.Pause;
  if FAudioOutput <> nil then
    FAudioOutput.Pause;
  SetState(psPaused);
end;

procedure TFFPlaybackEngine.Play;
begin
  if FFileName = '' then
    raise EFFException.Create('TFFPlaybackEngine.FileName is empty');

  if FState = psPaused then
  begin
    FPaused := False;
    if FClock <> nil then
      FClock.Resume;
    if FAudioOutput <> nil then
      FAudioOutput.Resume;
    SetState(psPlaying);
    Exit;
  end;

  if FState = psPlaying then
    Exit;

  Stop;
  FStopRequested := False;
  FPaused := False;
  FPlaybackCleanedUp := False;
  if FSeekStartMs > 0 then
    FPlayStartMs := FSeekStartMs
  else
    FPlayStartMs := FPosition;
  FSeekStartMs := 0;
  ProbeDuration;
  SetState(psPlaying);

  FVideoQueue := TFFPacketQueue.Create(QueueMaxPackets);
  FAudioQueue := TFFPacketQueue.Create(QueueMaxPackets);
  FClock := TFFPlaybackClock.Create;

  FPlayThread := TFFPlayCoordinator.Create(Self);
  FPlayThread.Start;
end;

procedure TFFPlaybackEngine.SeekTo(APositionMs: Int64);
begin
  if FState <> psStopped then
    Stop;
  FSeekStartMs := APositionMs;
  FPlayStartMs := APositionMs;
  FPosition := APositionMs;
end;

procedure TFFPlaybackEngine.SetFileName(const Value: string);
begin
  if FFileName = Value then
    Exit;
  Stop;
  FFileName := Value;
end;

procedure TFFPlaybackEngine.SetHardwareDevice(const Value: TFFHardwareDevice);
begin
  if FHardwareDevice = Value then
    Exit;
  if FState <> psStopped then
    raise EFFException.Create('Change HardwareDevice only while playback is stopped');
  FHardwareDevice := Value;
end;

procedure TFFPlaybackEngine.SetVolume(const Value: Single);
begin
  FVolume := Value;
  if FVolume < 0 then
    FVolume := 0
  else if FVolume > 1 then
    FVolume := 1;
  if FAudioOutput <> nil then
    FAudioOutput.Volume := FVolume;
end;

procedure TFFPlaybackEngine.SetState(const Value: TFFPlayerState);
begin
  if FState = Value then
    Exit;
  FState := Value;
  DoStateChange;
end;

procedure TFFPlaybackEngine.QueuePlaybackEnded;
begin
  TThread.Queue(nil,
    procedure
    begin
      if FDestroying then
        Exit;
      CleanupPlayback;
      if FState = psPlaying then
        SetState(psStopped);
    end);
end;

procedure TFFPlaybackEngine.CleanupPlayback;
var
  Spins: Cardinal;
begin
  if FPlaybackCleanedUp then
    Exit;
  FPlaybackCleanedUp := True;

  if FFIsDesignTime(nil) then
  begin
    if FPlayThread <> nil then
    begin
      FPlayThread.FreeOnTerminate := True;
      FPlayThread.Terminate;
      FPlayThread := nil;
    end;
    FreeAndNil(FVideoQueue);
    FreeAndNil(FAudioQueue);
    FreeAndNil(FClock);
    FAudioOutput := nil;
    Exit;
  end;

  FStopRequested := True;
  if FAudioOutput <> nil then
    FAudioOutput.Flush;
  if FVideoQueue <> nil then
    FVideoQueue.Close;
  if FAudioQueue <> nil then
    FAudioQueue.Close;
  if FPlayThread <> nil then
  begin
    FPlayThread.Terminate;
    Spins := 0;
    while not FPlayThread.Finished do
    begin
      if TThread.CurrentThread.ThreadID = MainThreadID then
        CheckSynchronize(10)
      else
        Sleep(1);
      Inc(Spins);
      if Spins > 30000 then
        Break;
    end;
    FreeAndNil(FPlayThread);
  end;
  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
  FreeAndNil(FClock);
  FAudioOutput := nil;
end;

procedure TFFPlaybackEngine.Stop;
begin
  FPaused := False;
  CleanupPlayback;
  if not FDestroying then
    SetState(psStopped);
end;

end.
