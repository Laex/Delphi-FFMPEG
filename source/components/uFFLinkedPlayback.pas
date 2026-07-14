unit uFFLinkedPlayback;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Shared linked-graph playback helper (Reader -> Decoder[s] -> sinks). }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavformat,
  uFFReader,
  uFFDecoder,
  uFFFrame,
  uFFPacket,
  uFFPacketQueue,
  uFFComponentBase,
  uFFPlaybackEngine,
  uFFPlaybackClock,
  uFFAudioOutput,
  uFFAudioResampler,
  uFFHooks,
  uFFDesignTime;

type
  TFFLinkedPlayback = class;
  TFFLinkedPositionEvent = procedure(Sender: TObject; APositionMs: Int64) of object;

  TFFLinkedAudioFrameSink = class(TInterfacedObject, IFFFrameSink)
  private
    FOwner: TObject;
    FHandler: TFFLinkedPlayback;
  public
    constructor Create(AOwner: TObject; AHandler: TFFLinkedPlayback);
    procedure TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
  end;

  TFFLinkedPlayback = class
  private
    FVideoDecoder: TFFDecoder;
    FAudioDecoder: TFFDecoder;
    FAudioSink: IFFFrameSink;
    FState: TFFPlayerState;
    FPositionMs: Int64;
    FDurationMs: Int64;
    FVolume: Single;
    FAudioOutput: TFFAudioOutput;
    FAudioResampler: TFFAudioResampler;
    FClock: TFFPlaybackClock;
    FStopRequested: Boolean;
    FHasAudioMaster: Boolean;
    FThreadedPlayback: Boolean;
    FThreadedCleanedUp: Boolean;
    FDestroying: Boolean;
    FPlaybackPaused: Boolean;
    FPlayFileName: string;
    FPlayStartMs: Int64;
    FCoordinator: TThread;
    FVideoQueue: TFFPacketQueue;
    FAudioQueue: TFFPacketQueue;
    FResamplerReady: Boolean;
    FOnStateChange: TNotifyEvent;
    FOnPositionChange: TFFLinkedPositionEvent;
    FOnAudioHook: TFFAudioHookEvent;
    procedure SetState(const Value: TFFPlayerState);
    procedure EnsureAudioOutput;
    procedure EnsureClock;
    procedure ConfigureResampler(AFrame: TFFFrame);
    function FramePtsToMs(AFrame: TFFFrame; ADecoder: TFFDecoder): Int64;
    function MsToStreamTimestamp(AReader: TFFReader; AStreamIndex, APositionMs: Int64): Int64;
    procedure NotifyPosition(APositionMs: Int64);
    procedure SubscribeAudioDecoder;
    procedure UnsubscribeAudioDecoder;
    procedure StopThreadedPlayback;
    procedure StartThreadedPlayback(const AFileName: string);
    procedure QueuePlaybackEnded;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetVideoDecoder(const Value: TFFDecoder);
    procedure SetAudioDecoder(const Value: TFFDecoder);

    procedure Play(const AFallbackFileName: string);
    procedure Pause;
    procedure Stop;
    procedure SeekTo(APositionMs: Int64);

    function ShouldPresentVideoFrame(AFrame: TFFFrame): Boolean;
    procedure HandleVideoFrame(AFrame: TFFFrame);
    procedure HandleAudioFrame(AFrame: TFFFrame);

    function GetState: TFFPlayerState;
    function GetPosition: Int64;
    function GetDuration: Int64;
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);

    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnPositionChange: TFFLinkedPositionEvent read FOnPositionChange write FOnPositionChange;
    property OnAudioHook: TFFAudioHookEvent read FOnAudioHook write FOnAudioHook;
  end;

implementation

const
  LinkedQueueMaxPackets = 64;

type
  TFFLinkedDemuxThread = class(TThread)
  private
    FOwner: TFFLinkedPlayback;
    FFileName: string;
    FHasAudio: Boolean;
    FVideoStreamIndex: Integer;
    FAudioStreamIndex: Integer;
    FStartMs: Int64;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFFLinkedPlayback; const AFileName: string; AHasAudio: Boolean;
      AVideoStreamIndex, AAudioStreamIndex: Integer; AStartMs: Int64);
  end;

  TFFLinkedVideoThread = class(TThread)
  private
    FOwner: TFFLinkedPlayback;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFFLinkedPlayback);
  end;

  TFFLinkedAudioThread = class(TThread)
  private
    FOwner: TFFLinkedPlayback;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFFLinkedPlayback);
  end;

  TFFLinkedCoordinator = class(TThread)
  private
    FOwner: TFFLinkedPlayback;
    FDemuxThread: TFFLinkedDemuxThread;
    FVideoThread: TFFLinkedVideoThread;
    FAudioThread: TFFLinkedAudioThread;
    FHasAudio: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFFLinkedPlayback; const AFileName: string; AHasAudio: Boolean;
      AVideoStreamIndex, AAudioStreamIndex: Integer; AStartMs: Int64);
    destructor Destroy; override;
    procedure TerminateWorkers;
  end;

{ TFFLinkedDemuxThread }

constructor TFFLinkedDemuxThread.Create(AOwner: TFFLinkedPlayback; const AFileName: string;
  AHasAudio: Boolean; AVideoStreamIndex, AAudioStreamIndex: Integer; AStartMs: Int64);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FFileName := AFileName;
  FHasAudio := AHasAudio;
  FVideoStreamIndex := AVideoStreamIndex;
  FAudioStreamIndex := AAudioStreamIndex;
  FStartMs := AStartMs;
end;

procedure TFFLinkedDemuxThread.Execute;
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
    Reader.FileName := FFileName;
    Reader.Open;

    if FStartMs > 0 then
    begin
      VideoStream := Reader.GetStream(FVideoStreamIndex);
      MsBase := av_make_q(1, 1000);
      SeekTs := av_rescale_q(FStartMs, MsBase, VideoStream^.time_base);
      Reader.Seek(SeekTs, FVideoStreamIndex);
    end;

    while not Terminated and not FOwner.FStopRequested do
    begin
      if FOwner.FPlaybackPaused then
      begin
        Sleep(10);
        Continue;
      end;

      while (FOwner.FVideoQueue <> nil) and (FOwner.FVideoQueue.Count >= LinkedQueueMaxPackets) do
      begin
        if Terminated or FOwner.FStopRequested then
          Break;
        Sleep(5);
      end;

      if Terminated or FOwner.FStopRequested then
        Break;

      if not Reader.ReadPacket(Packet) then
        Break;

      if Packet.Raw^.stream_index = FVideoStreamIndex then
      begin
        if FOwner.FVideoQueue.Count < LinkedQueueMaxPackets then
          FOwner.FVideoQueue.Push(Packet);
      end
      else if FHasAudio and (Packet.Raw^.stream_index = FAudioStreamIndex) and
        (FOwner.FAudioQueue <> nil) and (FOwner.FAudioQueue.Count < LinkedQueueMaxPackets) then
        FOwner.FAudioQueue.Push(Packet);
    end;
  finally
    Packet.Free;
    Reader.Free;
    if FOwner.FVideoQueue <> nil then
      FOwner.FVideoQueue.Close;
    if FOwner.FAudioQueue <> nil then
      FOwner.FAudioQueue.Close;
  end;
end;

{ TFFLinkedVideoThread }

constructor TFFLinkedVideoThread.Create(AOwner: TFFLinkedPlayback);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

procedure TFFLinkedVideoThread.Execute;
var
  Packet: TFFPacket;
begin
  Packet := nil;
  try
    while not Terminated and not FOwner.FStopRequested do
    begin
      if FOwner.FVideoQueue = nil then
        Break;

      Packet := FOwner.FVideoQueue.Pop(100);
      if Packet = nil then
      begin
        if FOwner.FVideoQueue.Closed then
          Break;
        Continue;
      end;

      try
        if FOwner.FVideoDecoder <> nil then
          FOwner.FVideoDecoder.TakePacket(Self, Packet, Packet.Raw^.stream_index);
      finally
        Packet.Free;
        Packet := nil;
      end;
    end;
  finally
    if Packet <> nil then
      Packet.Free;
  end;
end;

{ TFFLinkedAudioThread }

constructor TFFLinkedAudioThread.Create(AOwner: TFFLinkedPlayback);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

procedure TFFLinkedAudioThread.Execute;
var
  Packet: TFFPacket;
begin
  Packet := nil;
  try
    while not Terminated and not FOwner.FStopRequested do
    begin
      if FOwner.FAudioQueue = nil then
        Break;

      Packet := FOwner.FAudioQueue.Pop(100);
      if Packet = nil then
      begin
        if FOwner.FAudioQueue.Closed then
          Break;
        Continue;
      end;

      try
        if FOwner.FAudioDecoder <> nil then
          FOwner.FAudioDecoder.TakePacket(Self, Packet, Packet.Raw^.stream_index);
      finally
        Packet.Free;
        Packet := nil;
      end;
    end;
  finally
    if Packet <> nil then
      Packet.Free;
  end;
end;

{ TFFLinkedCoordinator }

constructor TFFLinkedCoordinator.Create(AOwner: TFFLinkedPlayback; const AFileName: string;
  AHasAudio: Boolean; AVideoStreamIndex, AAudioStreamIndex: Integer; AStartMs: Int64);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FHasAudio := AHasAudio;
  FDemuxThread := TFFLinkedDemuxThread.Create(AOwner, AFileName, AHasAudio, AVideoStreamIndex,
    AAudioStreamIndex, AStartMs);
  FVideoThread := TFFLinkedVideoThread.Create(AOwner);
  if FHasAudio then
    FAudioThread := TFFLinkedAudioThread.Create(AOwner);
end;

destructor TFFLinkedCoordinator.Destroy;
begin
  if FDemuxThread <> nil then
    FDemuxThread.Free;
  if FVideoThread <> nil then
    FVideoThread.Free;
  if FAudioThread <> nil then
    FAudioThread.Free;
  inherited;
end;

procedure TFFLinkedCoordinator.TerminateWorkers;
begin
  if FDemuxThread <> nil then
    FDemuxThread.Terminate;
  if FVideoThread <> nil then
    FVideoThread.Terminate;
  if FAudioThread <> nil then
    FAudioThread.Terminate;
end;

procedure TFFLinkedCoordinator.Execute;
begin
  FDemuxThread.Start;
  FVideoThread.Start;
  if FAudioThread <> nil then
    FAudioThread.Start;

  FDemuxThread.WaitFor;
  FVideoThread.WaitFor;
  if FAudioThread <> nil then
    FAudioThread.WaitFor;

  FreeAndNil(FDemuxThread);
  FreeAndNil(FVideoThread);
  FreeAndNil(FAudioThread);

  TThread.Queue(nil,
    procedure
    begin
      if (FOwner <> nil) and not FOwner.FDestroying then
        FOwner.QueuePlaybackEnded;
    end);
end;

{ TFFLinkedAudioFrameSink }

constructor TFFLinkedAudioFrameSink.Create(AOwner: TObject; AHandler: TFFLinkedPlayback);
begin
  inherited Create;
  FOwner := AOwner;
  FHandler := AHandler;
end;

procedure TFFLinkedAudioFrameSink.TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
begin
  if FHandler <> nil then
    FHandler.HandleAudioFrame(AFrame);
end;

{ TFFLinkedPlayback }

constructor TFFLinkedPlayback.Create;
begin
  inherited Create;
  FState := psStopped;
  FVolume := 1.0;
  FResamplerReady := False;
end;

destructor TFFLinkedPlayback.Destroy;
begin
  FDestroying := True;
  if FFIsDesignTime(nil) then
  begin
    UnsubscribeAudioDecoder;
    FAudioResampler.Free;
    FAudioOutput.Free;
    FClock.Free;
    FAudioSink := nil;
    inherited;
    Exit;
  end;
  Stop;
  UnsubscribeAudioDecoder;
  FAudioResampler.Free;
  FAudioOutput.Free;
  FClock.Free;
  FAudioSink := nil;
  inherited;
end;

procedure TFFLinkedPlayback.StopThreadedPlayback;
var
  Coordinator: TFFLinkedCoordinator;
begin
  if FThreadedCleanedUp then
    Exit;
  FThreadedCleanedUp := True;
  FThreadedPlayback := False;
  FStopRequested := True;

  if FFIsDesignTime(nil) then
  begin
    if FCoordinator <> nil then
    begin
      TFFLinkedCoordinator(FCoordinator).FreeOnTerminate := True;
      TFFLinkedCoordinator(FCoordinator).Terminate;
      FCoordinator := nil;
    end;
    FreeAndNil(FVideoQueue);
    FreeAndNil(FAudioQueue);
    Exit;
  end;

  if FVideoQueue <> nil then
    FVideoQueue.Close;
  if FAudioQueue <> nil then
    FAudioQueue.Close;

  if FCoordinator <> nil then
  begin
    Coordinator := TFFLinkedCoordinator(FCoordinator);
    FCoordinator := nil;
    Coordinator.TerminateWorkers;
    Coordinator.Terminate;
    Coordinator.WaitFor;
    Coordinator.Free;
  end;

  FreeAndNil(FVideoQueue);
  FreeAndNil(FAudioQueue);
end;

procedure TFFLinkedPlayback.StartThreadedPlayback(const AFileName: string);
var
  VideoIdx: Integer;
  AudioIdx: Integer;
begin
  StopThreadedPlayback;
  FStopRequested := False;
  FThreadedCleanedUp := False;
  FThreadedPlayback := True;
  FPlayFileName := AFileName;
  FPlayStartMs := FPositionMs;

  FVideoQueue := TFFPacketQueue.Create(LinkedQueueMaxPackets);
  if FHasAudioMaster then
    FAudioQueue := TFFPacketQueue.Create(LinkedQueueMaxPackets);

  VideoIdx := FVideoDecoder.StreamIndex;
  if FHasAudioMaster then
    AudioIdx := FAudioDecoder.StreamIndex
  else
    AudioIdx := -1;

  FCoordinator := TFFLinkedCoordinator.Create(Self, AFileName, FHasAudioMaster, VideoIdx, AudioIdx, FPlayStartMs);
  TFFLinkedCoordinator(FCoordinator).Start;
end;

procedure TFFLinkedPlayback.QueuePlaybackEnded;
begin
  if FDestroying or FThreadedCleanedUp then
    Exit;
  StopThreadedPlayback;
  if FState = psPlaying then
    SetState(psStopped);
end;

procedure TFFLinkedPlayback.SetState(const Value: TFFPlayerState);
begin
  if FState = Value then
    Exit;
  FState := Value;
  if Assigned(FOnStateChange) then
    FOnStateChange(nil);
end;

procedure TFFLinkedPlayback.SetVolume(const Value: Single);
begin
  if Value < 0 then
    FVolume := 0
  else if Value > 1 then
    FVolume := 1
  else
    FVolume := Value;
  if FAudioOutput <> nil then
    FAudioOutput.Volume := FVolume;
end;

function TFFLinkedPlayback.GetVolume: Single;
begin
  Result := FVolume;
end;

function TFFLinkedPlayback.GetState: TFFPlayerState;
begin
  Result := FState;
end;

function TFFLinkedPlayback.GetPosition: Int64;
begin
  Result := FPositionMs;
end;

function TFFLinkedPlayback.GetDuration: Int64;
begin
  if (FVideoDecoder <> nil) and (FDurationMs <= 0) then
  begin
    FDurationMs := FVideoDecoder.DurationMs;
    if (FDurationMs <= 0) and (FVideoDecoder.Reader <> nil) and (FVideoDecoder.Reader.Active) and
       (FVideoDecoder.Reader.Duration > 0) then
      FDurationMs := FVideoDecoder.Reader.Duration div 1000;
  end;
  Result := FDurationMs;
end;

procedure TFFLinkedPlayback.SetVideoDecoder(const Value: TFFDecoder);
begin
  FVideoDecoder := Value;
  FDurationMs := 0;
end;

procedure TFFLinkedPlayback.SubscribeAudioDecoder;
begin
  if (FAudioDecoder = nil) or (FAudioSink <> nil) then
    Exit;
  FAudioSink := TFFLinkedAudioFrameSink.Create(Self, Self);
  FAudioDecoder.SubscribeFrameSink(FAudioSink);
end;

procedure TFFLinkedPlayback.UnsubscribeAudioDecoder;
begin
  if (FAudioDecoder = nil) or (FAudioSink = nil) then
    Exit;
  FAudioDecoder.UnsubscribeFrameSink(FAudioSink);
  FAudioSink := nil;
end;

procedure TFFLinkedPlayback.SetAudioDecoder(const Value: TFFDecoder);
begin
  if FAudioDecoder = Value then
    Exit;
  UnsubscribeAudioDecoder;
  FResamplerReady := False;
  FAudioDecoder := Value;
  if (FAudioDecoder <> nil) and (FVideoDecoder <> nil) and (FVideoDecoder.Reader <> nil) and (FAudioDecoder.Reader = nil) then
    FAudioDecoder.Reader := FVideoDecoder.Reader;
  if FAudioDecoder <> nil then
    SubscribeAudioDecoder;
end;

function TFFLinkedPlayback.MsToStreamTimestamp(AReader: TFFReader; AStreamIndex, APositionMs: Int64): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
begin
  St := AReader.GetStream(AStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(APositionMs, MsBase, St^.time_base);
end;

function TFFLinkedPlayback.FramePtsToMs(AFrame: TFFFrame; ADecoder: TFFDecoder): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
  Pts: Int64;
begin
  Result := -1;
  if (AFrame = nil) or (ADecoder = nil) or (ADecoder.Reader = nil) then
    Exit;
  Pts := AFrame.Raw^.best_effort_timestamp;
  if Pts = AV_NOPTS_VALUE then
    Pts := AFrame.Raw^.pts;
  if Pts = AV_NOPTS_VALUE then
    Exit;
  St := ADecoder.Reader.GetStream(ADecoder.StreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(Pts, St^.time_base, MsBase);
end;

procedure TFFLinkedPlayback.NotifyPosition(APositionMs: Int64);
begin
  if APositionMs < 0 then
    Exit;
  FPositionMs := APositionMs;
  if Assigned(FOnPositionChange) then
    FOnPositionChange(nil, APositionMs);
end;

procedure TFFLinkedPlayback.EnsureAudioOutput;
begin
  if FAudioOutput = nil then
  begin
    FAudioOutput := TFFAudioOutput.Create;
    FAudioOutput.Volume := FVolume;
  end;
end;

procedure TFFLinkedPlayback.EnsureClock;
begin
  if FClock = nil then
    FClock := TFFPlaybackClock.Create;
end;

procedure TFFLinkedPlayback.ConfigureResampler(AFrame: TFFFrame);
begin
  if FAudioResampler = nil then
    FAudioResampler := TFFAudioResampler.Create;
  FAudioResampler.Configure(@AFrame.Raw^.ch_layout, AFrame.Raw^.sample_rate, AVSampleFormat(AFrame.Raw^.format));
  FResamplerReady := True;
end;

function TFFLinkedPlayback.ShouldPresentVideoFrame(AFrame: TFFFrame): Boolean;
var
  PtsMs: Int64;
begin
  Result := False;
  if (AFrame = nil) or (FState <> psPlaying) or FStopRequested then
    Exit;

  EnsureClock;
  PtsMs := FramePtsToMs(AFrame, FVideoDecoder);
  if PtsMs < 0 then
    Exit(True);

  { Clock uses wall time until audio master is set, then follows audio PTS. }
  if not FClock.IsLate(PtsMs) then
    FClock.WaitUntil(PtsMs, FStopRequested, FPlaybackPaused);

  if FStopRequested then
    Exit;

  if not FHasAudioMaster then
    NotifyPosition(PtsMs);

  Result := True;
end;

procedure TFFLinkedPlayback.HandleVideoFrame(AFrame: TFFFrame);
begin
  ShouldPresentVideoFrame(AFrame);
end;

procedure TFFLinkedPlayback.HandleAudioFrame(AFrame: TFFFrame);
var
  Buf: PByte;
  ByteCount: Integer;
  HandledCount: Integer;
  PtsMs: Int64;
begin
  if (AFrame = nil) or (FState <> psPlaying) then
    Exit;
  if not FResamplerReady then
    ConfigureResampler(AFrame);
  if FAudioResampler.Convert(AFrame.Raw, Buf, ByteCount) < 0 then
    Exit;
  if ByteCount <= 0 then
    Exit;

  HandledCount := ByteCount;
  if Assigned(FOnAudioHook) then
    FOnAudioHook(nil, Buf, HandledCount);
  if HandledCount <= 0 then
    Exit;

  EnsureAudioOutput;
  FAudioOutput.Write(Buf, HandledCount);

  PtsMs := FramePtsToMs(AFrame, FAudioDecoder);
  if PtsMs >= 0 then
  begin
    EnsureClock;
    FClock.SetAudioTimeMs(PtsMs);
    if FHasAudioMaster then
      NotifyPosition(PtsMs);
  end;
end;

procedure TFFLinkedPlayback.Play(const AFallbackFileName: string);
var
  Reader: TFFReader;
begin
  if FVideoDecoder = nil then
    Exit;

  Reader := FVideoDecoder.Reader;
  if Reader = nil then
    Exit;

  if FState = psPaused then
  begin
    FPlaybackPaused := False;
    if FClock <> nil then
      FClock.Resume;
    if FAudioOutput <> nil then
      FAudioOutput.Resume;
    SetState(psPlaying);
    Exit;
  end;

  if Reader.FileName = '' then
    Reader.FileName := AFallbackFileName;

  if not Reader.Active then
    Reader.Open;

  if not FVideoDecoder.Initialized then
    FVideoDecoder.Initialize;

  if (FAudioDecoder <> nil) and not FAudioDecoder.Initialized then
    FAudioDecoder.Initialize;

  FDurationMs := FVideoDecoder.DurationMs;
  FStopRequested := False;
  FPlaybackPaused := False;
  FHasAudioMaster := FAudioDecoder <> nil;
  EnsureClock;
  FClock.Reset(FPositionMs, FHasAudioMaster);
  EnsureAudioOutput;
  FAudioOutput.Flush;
  FAudioOutput.Resume;

  if Reader.AutoPump then
    Reader.AutoPump := False;

  StartThreadedPlayback(Reader.FileName);

  SetState(psPlaying);
end;

procedure TFFLinkedPlayback.Pause;
begin
  if FVideoDecoder = nil then
    Exit;
  FPlaybackPaused := True;
  if FAudioOutput <> nil then
    FAudioOutput.Pause;
  if FClock <> nil then
    FClock.Pause;
  if FState = psPlaying then
    SetState(psPaused);
end;

procedure TFFLinkedPlayback.Stop;
begin
  FStopRequested := True;
  if FAudioOutput <> nil then
    FAudioOutput.Flush;
  StopThreadedPlayback;
  if FVideoDecoder = nil then
  begin
    SetState(psStopped);
    Exit;
  end;

  if FVideoDecoder.Initialized then
    FVideoDecoder.Flush;
  if (FAudioDecoder <> nil) and FAudioDecoder.Initialized then
    FAudioDecoder.Flush;

  if FAudioOutput <> nil then
  begin
    FAudioOutput.Flush;
    FAudioOutput.Pause;
  end;

  FResamplerReady := False;
  FPlaybackPaused := False;
  FPositionMs := 0;
  SetState(psStopped);
end;

procedure TFFLinkedPlayback.SeekTo(APositionMs: Int64);
var
  Reader: TFFReader;
  WasPlaying: Boolean;
begin
  if (FVideoDecoder = nil) or (FVideoDecoder.Reader = nil) then
    Exit;

  Reader := FVideoDecoder.Reader;
  if not Reader.Active then
    Reader.Open;

  WasPlaying := FState = psPlaying;
  if WasPlaying then
    StopThreadedPlayback;

  Reader.Seek(MsToStreamTimestamp(Reader, FVideoDecoder.StreamIndex, APositionMs), FVideoDecoder.StreamIndex);
  if FVideoDecoder.Initialized then
    FVideoDecoder.Flush;
  if (FAudioDecoder <> nil) and FAudioDecoder.Initialized then
    FAudioDecoder.Flush;
  if FAudioOutput <> nil then
    FAudioOutput.Flush;
  if FClock <> nil then
    FClock.Reset(APositionMs, FHasAudioMaster);
  FResamplerReady := False;
  FPositionMs := APositionMs;

  if WasPlaying then
  begin
    FStopRequested := False;
    FThreadedCleanedUp := False;
    StartThreadedPlayback(Reader.FileName);
    SetState(psPlaying);
  end
  else
    NotifyPosition(APositionMs);
end;

end.
