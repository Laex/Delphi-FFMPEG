unit uFFRemuxJob;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Stream-copy remux job (Reader -> Writer, no re-encode). }

interface

uses
  {$IFDEF FPC}
  Classes,
  SyncObjs,
  {$ELSE}
  System.Classes,
  System.SyncObjs,
  System.Math,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavformat,
  uFFException,
  uFFReader,
  uFFWriter,
  uFFPacket,
  uFFComponentLink,
  uFFDesignTime;

type
  TFFRemuxState = (rsStopped, rsRunning, rsPaused, rsStopping);

  TFFRemuxProgressEvent = procedure(Sender: TObject; APositionMs, ADurationMs: Int64) of object;

  TFFRemuxJob = class(TComponent)
  private
    FReader: TFFReader;
    FWriter: TFFWriter;
    FStartMs: Int64;
    FEndMs: Int64;
    FCopySubtitles: Boolean;
    FState: TFFRemuxState;
    FRemuxThread: TThread;
    FStopRequested: Boolean;
    FPaused: Boolean;
    FActiveRunner: TObject;
    FOnProgress: TFFRemuxProgressEvent;
    FOnStateChange: TNotifyEvent;
    procedure SetReader(const Value: TFFReader);
    procedure SetWriter(const Value: TFFWriter);
    procedure SetState(const Value: TFFRemuxState);
    procedure DoStateChange;
    procedure RunRemux;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;

    property State: TFFRemuxState read FState;
  published
    property Reader: TFFReader read FReader write SetReader;
    property Writer: TFFWriter read FWriter write SetWriter;
    property StartMs: Int64 read FStartMs write FStartMs default 0;
    property EndMs: Int64 read FEndMs write FEndMs default 0;
    property CopySubtitles: Boolean read FCopySubtitles write FCopySubtitles default True;
    property OnProgress: TFFRemuxProgressEvent read FOnProgress write FOnProgress;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

implementation

type
  TFFRemuxRunner = class
  private
    FJob: TFFRemuxJob;
    FProgressLock: TCriticalSection;
    FLastProgressMs: Int64;
    procedure NotifyProgress(APositionMs, ADurationMs: Int64);
    function MsToStreamTimestamp(AReader: TFFReader; AStreamIndex, APositionMs: Int64): Int64;
    function PacketPtsToMs(APacket: TFFPacket; AStreamIndex: Integer): Int64;
    function FindProgressStreamIndex: Integer;
  public
    constructor Create(AJob: TFFRemuxJob);
    destructor Destroy; override;
    procedure Execute;
    procedure RequestStop;
    procedure SetPaused(const Value: Boolean);
  end;

  TFFRemuxThread = class(TThread)
  private
    FJob: TFFRemuxJob;
  protected
    procedure Execute; override;
  public
    constructor Create(AJob: TFFRemuxJob);
  end;

{ TFFRemuxRunner }

constructor TFFRemuxRunner.Create(AJob: TFFRemuxJob);
begin
  inherited Create;
  FJob := AJob;
  FProgressLock := TCriticalSection.Create;
  FLastProgressMs := -1;
end;

destructor TFFRemuxRunner.Destroy;
begin
  FProgressLock.Free;
  inherited;
end;

procedure TFFRemuxRunner.RequestStop;
begin
  FJob.FStopRequested := True;
  FJob.FPaused := False;
end;

procedure TFFRemuxRunner.SetPaused(const Value: Boolean);
begin
  FJob.FPaused := Value;
end;

function TFFRemuxRunner.MsToStreamTimestamp(AReader: TFFReader; AStreamIndex, APositionMs: Int64): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
begin
  St := AReader.GetStream(AStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(APositionMs, MsBase, St^.time_base);
end;

function TFFRemuxRunner.PacketPtsToMs(APacket: TFFPacket; AStreamIndex: Integer): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
begin
  Result := -1;
  if (APacket = nil) or (APacket.Raw = nil) or (FJob.FReader = nil) then
    Exit;
  if APacket.Raw^.pts = AV_NOPTS_VALUE then
    Exit;
  St := FJob.FReader.GetStream(AStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(APacket.Raw^.pts, St^.time_base, MsBase);
end;

function TFFRemuxRunner.FindProgressStreamIndex: Integer;
var
  I: Integer;
  Info: TFFStreamInfo;
begin
  Result := -1;
  if FJob.FReader = nil then
    Exit;
  for I := 0 to FJob.FReader.Streams.Count - 1 do
  begin
    Info := FJob.FReader.Streams.GetInfo(I);
    if Info.MediaType = AVMEDIA_TYPE_VIDEO then
      Exit(I);
  end;
  for I := 0 to FJob.FReader.Streams.Count - 1 do
  begin
    Info := FJob.FReader.Streams.GetInfo(I);
    if Info.MediaType = AVMEDIA_TYPE_AUDIO then
      Exit(I);
  end;
end;

procedure TFFRemuxRunner.NotifyProgress(APositionMs, ADurationMs: Int64);
begin
  FProgressLock.Enter;
  try
    if (APositionMs >= 0) and (Abs(APositionMs - FLastProgressMs) < 200) then
      Exit;
    FLastProgressMs := APositionMs;
  finally
    FProgressLock.Leave;
  end;
  if Assigned(FJob.FOnProgress) then
    FJob.FOnProgress(FJob, APositionMs, ADurationMs);
end;

procedure TFFRemuxRunner.Execute;
var
  Reader: TFFReader;
  Writer: TFFWriter;
  Packet: TFFPacket;
  I: Integer;
  Info: TFFStreamInfo;
  Ret: Integer;
  ProgressIdx: Integer;
  PositionMs: Int64;
  DurationMs: Int64;
  ClipStartMs: Int64;
  ClipEndMs: Int64;
  WasAutoPump: Boolean;
begin
  Reader := FJob.FReader;
  Writer := FJob.FWriter;
  if (Reader = nil) or (Writer = nil) then
    raise EFFException.Create('TFFRemuxJob: Reader and Writer are required');
  if not Writer.HasOutputTarget then
    raise EFFException.Create('TFFRemuxJob: Writer needs FileName or OutputAdapter');

  FJob.FStopRequested := False;
  FJob.FPaused := False;
  FLastProgressMs := -1;

  if not Reader.Active then
    Reader.Open;

  WasAutoPump := Reader.AutoPump;
  if WasAutoPump then
    Reader.AutoPump := False;
  try
    if not Writer.Active then
      Writer.Open;

    for I := 0 to Reader.Streams.Count - 1 do
    begin
      Info := Reader.Streams.GetInfo(I);
      if Info.MediaType = AVMEDIA_TYPE_VIDEO then
        Writer.AddStreamCopy(Reader, I)
      else if Info.MediaType = AVMEDIA_TYPE_AUDIO then
        Writer.AddStreamCopy(Reader, I)
      else if FJob.FCopySubtitles and (Info.MediaType = AVMEDIA_TYPE_SUBTITLE) then
        Writer.AddStreamCopy(Reader, I);
    end;

    Writer.WriteHeader;

    ProgressIdx := FindProgressStreamIndex;
    ClipStartMs := FJob.FStartMs;
    if FJob.FEndMs > ClipStartMs then
      ClipEndMs := FJob.FEndMs
    else
      ClipEndMs := 0;

    if ProgressIdx >= 0 then
    begin
      if Reader.Duration > 0 then
        DurationMs := Reader.Duration div 1000
      else
        DurationMs := 0;
      if ClipEndMs > ClipStartMs then
        DurationMs := ClipEndMs - ClipStartMs
      else if ClipStartMs > 0 then
        DurationMs := Max(0, DurationMs - ClipStartMs);
    end
    else
      DurationMs := 0;

    if (ClipStartMs > 0) and (ProgressIdx >= 0) then
      Reader.Seek(MsToStreamTimestamp(Reader, ProgressIdx, ClipStartMs), ProgressIdx);

    Packet := TFFPacket.Create;
    try
      while not FJob.FStopRequested and Reader.ReadPacket(Packet) do
      begin
        while FJob.FPaused and not FJob.FStopRequested do
          Sleep(10);

        if ProgressIdx >= 0 then
        begin
          PositionMs := PacketPtsToMs(Packet, ProgressIdx);
          if (PositionMs >= 0) and (ClipStartMs > 0) and (PositionMs < ClipStartMs) then
            Continue;
          if (ClipEndMs > 0) and (PositionMs >= 0) and (PositionMs >= ClipEndMs) then
          begin
            FJob.FStopRequested := True;
            Break;
          end;
          if PositionMs >= 0 then
            NotifyProgress(Max(0, PositionMs - ClipStartMs), DurationMs);
        end;

        Ret := Writer.WritePacketFromReader(Packet, Reader);
        if Ret < 0 then
          raise EFFException.CreateFmt('TFFRemuxJob: WritePacketFromReader failed (%d)', [Ret]);
      end;

      if not FJob.FStopRequested then
      begin
        NotifyProgress(DurationMs, DurationMs);
        Writer.WriteTrailer;
      end;
    finally
      Packet.Free;
    end;
  finally
    if Writer.Active then
      Writer.Close;
    if WasAutoPump then
      Reader.AutoPump := True;
  end;
end;

{ TFFRemuxThread }

constructor TFFRemuxThread.Create(AJob: TFFRemuxJob);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FJob := AJob;
end;

procedure TFFRemuxThread.Execute;
begin
  try
    FJob.RunRemux;
  finally
    FJob.FActiveRunner := nil;
    if FJob.FState in [rsRunning, rsPaused, rsStopping] then
      FJob.SetState(rsStopped);
  end;
end;

{ TFFRemuxJob }

constructor TFFRemuxJob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := rsStopped;
  FCopySubtitles := True;
end;

destructor TFFRemuxJob.Destroy;
begin
  Stop;
  inherited;
end;

procedure TFFRemuxJob.SetReader(const Value: TFFReader);
var
  Link: TComponent;
begin
  Link := FReader;
  FFSetLinkedComponent(Self, Link, Value);
  FReader := TFFReader(Link);
end;

procedure TFFRemuxJob.SetWriter(const Value: TFFWriter);
var
  Link: TComponent;
begin
  Link := FWriter;
  FFSetLinkedComponent(Self, Link, Value);
  FWriter := TFFWriter(Link);
end;

procedure TFFRemuxJob.SetState(const Value: TFFRemuxState);
begin
  if FState = Value then
    Exit;
  FState := Value;
  DoStateChange;
end;

procedure TFFRemuxJob.DoStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TFFRemuxJob.RunRemux;
var
  Runner: TFFRemuxRunner;
begin
  Runner := TFFRemuxRunner.Create(Self);
  FActiveRunner := Runner;
  try
    Runner.Execute;
  finally
    Runner.Free;
    FActiveRunner := nil;
  end;
end;

procedure TFFRemuxJob.Start;
begin
  if FState = rsRunning then
    Exit;
  if (FReader = nil) or (FWriter = nil) then
    raise EFFException.Create('TFFRemuxJob: Reader and Writer must be assigned');
  if not FWriter.HasOutputTarget then
    raise EFFException.Create('TFFRemuxJob: Writer needs FileName or OutputAdapter');

  Stop;
  SetState(rsRunning);
  FRemuxThread := TFFRemuxThread.Create(Self);
  FRemuxThread.Start;
end;

procedure TFFRemuxJob.Stop;
begin
  if FActiveRunner <> nil then
    TFFRemuxRunner(FActiveRunner).RequestStop;
  if FRemuxThread = nil then
  begin
    SetState(rsStopped);
    Exit;
  end;

  SetState(rsStopping);
  FRemuxThread.Terminate;
  if FFIsDesignTime(Self) then
  begin
    FRemuxThread.FreeOnTerminate := True;
    FRemuxThread := nil;
    SetState(rsStopped);
    Exit;
  end;
  FRemuxThread.WaitFor;
  FreeAndNil(FRemuxThread);
  SetState(rsStopped);
end;

procedure TFFRemuxJob.Pause;
begin
  if FState <> rsRunning then
    Exit;
  if FActiveRunner <> nil then
    TFFRemuxRunner(FActiveRunner).SetPaused(True);
  SetState(rsPaused);
end;

procedure TFFRemuxJob.Resume;
begin
  if FState <> rsPaused then
    Exit;
  if FActiveRunner <> nil then
    TFFRemuxRunner(FActiveRunner).SetPaused(False);
  SetState(rsRunning);
end;

end.
