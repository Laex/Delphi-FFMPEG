unit uFFReader;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Demuxer wrapper around AVFormatContext. Optional AutoPump pushes packets to linked sinks. }

interface

uses
  {$IFDEF FPC}
  Classes,
  SyncObjs,
  {$ELSE}
  System.Classes,
  System.SyncObjs,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavcodec,
  libavformat,
  uFFException,
  uFFPacket,
  uFFMemoryAccessAdapter,
  uFFComponentBase,
  uFFDesignTime;

type
  TFFReader = class;

  TFFStreamInfo = record
    Index: Integer;
    MediaType: AVMediaType;
    CodecName: string;
    Width: Integer;
    Height: Integer;
    SampleRate: Integer;
    Channels: Integer;
  end;

  TFFStreamList = class
  private
    FReader: TFFReader;
    function GetCount: Integer;
  public
    constructor Create(AReader: TFFReader);
    function GetInfo(AIndex: Integer): TFFStreamInfo;
    property Count: Integer read GetCount;
  end;

  TFFReader = class(TComponent, IFFPacketSource)
  private
    FFileName: string;
    FInputAdapter: TFFMemoryAccessAdapter;
    FActive: Boolean;
    FAutoPump: Boolean;
    FFormatCtx: PAVFormatContext;
    FStreams: TFFStreamList;
    FPacketSinks: TFFPacketSinkList;
    FPumpThread: TThread;
    FStopPump: Boolean;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    function GetDuration: Int64;
    function GetStreamCount: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetAutoPump(const Value: Boolean);
    procedure SetFileName(const Value: string);
    procedure SetInputAdapter(const Value: TFFMemoryAccessAdapter);
    procedure StartPump;
    procedure StopPump;
  protected
    procedure OpenInput;
    procedure CloseInput;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Open;
    procedure Close;
    function ReadPacket(APacket: TFFPacket): Boolean;
    procedure Seek(ATimestamp: Int64; AStreamIndex: Integer = -1);
    procedure NotifyPacketSinks(APacket: TFFPacket);

    function GetStream(AIndex: Integer): PAVStream;

    procedure SubscribePacketSink(const ASink: IFFPacketSink);
    procedure UnsubscribePacketSink(const ASink: IFFPacketSink);

    property FormatContext: PAVFormatContext read FFormatCtx;
    property StreamCount: Integer read GetStreamCount;
    property Streams: TFFStreamList read FStreams;
  published
    property FileName: string read FFileName write SetFileName;
    property InputAdapter: TFFMemoryAccessAdapter read FInputAdapter write SetInputAdapter;
    property Active: Boolean read FActive write SetActive default False;
    property AutoPump: Boolean read FAutoPump write SetAutoPump default False;
    property Duration: Int64 read GetDuration;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  uFFmpegPath;

type
  TFFReaderPumpThread = class(TThread)
  private
    FReader: TFFReader;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TFFReader);
  end;

{ TFFReaderPumpThread }

constructor TFFReaderPumpThread.Create(AReader: TFFReader);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FReader := AReader;
end;

procedure TFFReaderPumpThread.Execute;
var
  Packet: TFFPacket;
begin
  Packet := TFFPacket.Create;
  try
    while not Terminated and not FReader.FStopPump and FReader.FActive do
    begin
      if not FReader.ReadPacket(Packet) then
        Break;
      FReader.NotifyPacketSinks(Packet);
    end;
  finally
    Packet.Free;
  end;
end;

{ TFFStreamList }

function TFFStreamList.GetCount: Integer;
begin
  Result := FReader.StreamCount;
end;

function TFFStreamList.GetInfo(AIndex: Integer): TFFStreamInfo;
var
  St: PAVStream;
  Par: PAVCodecParameters;
begin
  St := FReader.GetStream(AIndex);
  Par := St^.codecpar;
  Result.Index := AIndex;
  Result.MediaType := Par^.codec_type;
  Result.CodecName := string(avcodec_get_name(Par^.codec_id));
  Result.Width := Par^.width;
  Result.Height := Par^.height;
  Result.SampleRate := Par^.sample_rate;
  Result.Channels := Par^.ch_layout.nb_channels;
end;

constructor TFFStreamList.Create(AReader: TFFReader);
begin
  inherited Create;
  FReader := AReader;
end;

procedure TFFReader.OpenInput;
var
  Path: UTF8String;
  Ret: Integer;
  Opts: ppAVDictionary;
  Ctx: PAVFormatContext;
begin
  if (FInputAdapter = nil) and (FFileName = '') then
    raise EFFException.Create('TFFReader.FileName is empty');
  if FFormatCtx <> nil then
    CloseInput;

  FFormatCtx := nil;
  Opts := nil;
  if FInputAdapter <> nil then
  begin
    FInputAdapter.EnsureAttached;
    Ctx := avformat_alloc_context();
    if Ctx = nil then
      raise EFFException.Create('avformat_alloc_context failed');
    Ctx^.pb := FInputAdapter.IOContext;
    Ctx^.flags := Ctx^.flags or AVFMT_FLAG_CUSTOM_IO;
    FFormatCtx := Ctx;
    Ret := avformat_open_input(FFormatCtx, PAnsiChar(UTF8String('memory:')), nil, Opts);
  end
  else
  begin
    Path := FFmpegUtf8Path(FFileName);
    Ret := avformat_open_input(FFormatCtx, PAnsiChar(Path), nil, Opts);
  end;
  if Ret < 0 then
    raise EFFException.CreateFmt('avformat_open_input failed (%d) for %s', [Ret, FFileName]);

  Ret := avformat_find_stream_info(FFormatCtx, nil);
  if Ret < 0 then
  begin
    CloseInput;
    raise EFFException.CreateFmt('avformat_find_stream_info failed (%d)', [Ret]);
  end;

  FActive := True;
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

procedure TFFReader.CloseInput;
begin
  StopPump;
  if FFIsDesignTime(Self) then
  begin
    FFormatCtx := nil;
    FActive := False;
    Exit;
  end;
  if FFormatCtx <> nil then
    avformat_close_input(FFormatCtx);
  FFormatCtx := nil;
  FActive := False;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TFFReader.SetInputAdapter(const Value: TFFMemoryAccessAdapter);
begin
  if FInputAdapter = Value then
    Exit;
  if FActive then
    Close;
  if Assigned(FInputAdapter) then
    FInputAdapter.RemoveFreeNotification(Self);
  FInputAdapter := Value;
  if Assigned(FInputAdapter) then
    FInputAdapter.FreeNotification(Self);
end;

constructor TFFReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStreams := TFFStreamList.Create(Self);
  FPacketSinks := TFFPacketSinkList.Create;
end;

destructor TFFReader.Destroy;
begin
  CloseInput;
  FPacketSinks.Free;
  FStreams.Free;
  inherited;
end;

procedure TFFReader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FInputAdapter) then
    FInputAdapter := nil;
end;

procedure TFFReader.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and FAutoPump and FActive then
    StartPump;
end;

procedure TFFReader.Open;
begin
  if not FActive then
    OpenInput;
  if FAutoPump and not (csDesigning in ComponentState) then
    StartPump;
end;

procedure TFFReader.Close;
begin
  CloseInput;
end;

function TFFReader.GetDuration: Int64;
var
  I: Integer;
  St: PAVStream;
  StreamUs: Int64;
begin
  if FFormatCtx = nil then
    Exit(0);
  if FFormatCtx^.duration <> AV_NOPTS_VALUE then
    Exit(FFormatCtx^.duration);

  Result := 0;
  for I := 0 to StreamCount - 1 do
  begin
    St := GetStream(I);
    if St^.duration <> AV_NOPTS_VALUE then
    begin
      StreamUs := av_rescale_q(St^.duration, St^.time_base, AV_TIME_BASE_Q);
      if StreamUs > Result then
        Result := StreamUs;
    end;
  end;
end;

function TFFReader.GetStreamCount: Integer;
begin
  if FFormatCtx = nil then
    Exit(0);
  Result := FFormatCtx^.nb_streams;
end;

function TFFReader.GetStream(AIndex: Integer): PAVStream;
begin
  if (FFormatCtx = nil) or (AIndex < 0) or (AIndex >= StreamCount) then
    raise EFFException.CreateFmt('Invalid stream index %d', [AIndex]);
  Result := PAVStream(FFormatCtx^.streams[AIndex]);
end;

function TFFReader.ReadPacket(APacket: TFFPacket): Boolean;
var
  Ret: Integer;
begin
  if not FActive then
    raise EFFException.Create('TFFReader is not active');
  if APacket = nil then
    raise EFFException.Create('ReadPacket: packet is nil');

  APacket.Clear;
  Ret := av_read_frame(FFormatCtx, APacket.Raw);
  Result := Ret >= 0;
end;

procedure TFFReader.NotifyPacketSinks(APacket: TFFPacket);
begin
  if (APacket = nil) or (csDestroying in ComponentState) then
    Exit;
  FPacketSinks.Notify(Self, APacket, APacket.Raw^.stream_index);
end;

procedure TFFReader.Seek(ATimestamp: Int64; AStreamIndex: Integer);
var
  Ret: Integer;
  St: PAVStream;
  TsUs: Int64;
begin
  if not FActive then
    raise EFFException.Create('TFFReader is not active');

  if ATimestamp <= 0 then
  begin
    Ret := avformat_seek_file(FFormatCtx, -1, 0, 0, 0, AVSEEK_FLAG_BACKWARD);
    if Ret < 0 then
      Ret := av_seek_frame(FFormatCtx, -1, 0, AVSEEK_FLAG_BACKWARD);
    if Ret < 0 then
      raise EFFException.CreateFmt('avformat_seek_file failed (%d)', [Ret]);
    Exit;
  end;

  Ret := avformat_seek_file(FFormatCtx, AStreamIndex, Low(Int64), ATimestamp, High(Int64), AVSEEK_FLAG_BACKWARD);
  if Ret < 0 then
  begin
    if (AStreamIndex >= 0) and (AStreamIndex < StreamCount) then
    begin
      St := GetStream(AStreamIndex);
      TsUs := av_rescale_q(ATimestamp, St^.time_base, AV_TIME_BASE_Q);
      Ret := avformat_seek_file(FFormatCtx, -1, Low(Int64), TsUs, High(Int64), AVSEEK_FLAG_BACKWARD);
    end;
  end;
  if Ret < 0 then
    Ret := av_seek_frame(FFormatCtx, AStreamIndex, ATimestamp, AVSEEK_FLAG_BACKWARD);
  if (Ret < 0) and (AStreamIndex >= 0) and (AStreamIndex < StreamCount) then
  begin
    St := GetStream(AStreamIndex);
    TsUs := av_rescale_q(ATimestamp, St^.time_base, AV_TIME_BASE_Q);
    Ret := av_seek_frame(FFormatCtx, -1, TsUs, AVSEEK_FLAG_BACKWARD);
  end;
  if Ret < 0 then
    raise EFFException.CreateFmt('avformat_seek_file failed (%d)', [Ret]);
end;

procedure TFFReader.SetActive(const Value: Boolean);
begin
  if Value = FActive then
    Exit;
  if Value then
    Open
  else
    Close;
end;

procedure TFFReader.SetAutoPump(const Value: Boolean);
begin
  if FAutoPump = Value then
    Exit;
  FAutoPump := Value;
  if FAutoPump and FActive and not (csDesigning in ComponentState) then
    StartPump
  else if not FAutoPump then
    StopPump;
end;

procedure TFFReader.SetFileName(const Value: string);
begin
  if FFileName = Value then
    Exit;
  if FActive then
    Close;
  FFileName := Value;
end;

procedure TFFReader.StartPump;
begin
  if (csDesigning in ComponentState) or not FActive or not FAutoPump then
    Exit;
  if FPumpThread <> nil then
    Exit;
  FStopPump := False;
  FPumpThread := TFFReaderPumpThread.Create(Self);
  FPumpThread.Start;
end;

procedure TFFReader.StopPump;
begin
  FStopPump := True;
  if FPumpThread = nil then
    Exit;
  FPumpThread.Terminate;
  if FFIsDesignTime(Self) then
  begin
    FPumpThread.FreeOnTerminate := True;
    FPumpThread := nil;
    Exit;
  end;
  FPumpThread.WaitFor;
  FreeAndNil(FPumpThread);
end;

procedure TFFReader.SubscribePacketSink(const ASink: IFFPacketSink);
begin
  if (ASink = nil) or (csDesigning in ComponentState) then
    Exit;
  FPacketSinks.Add(ASink);
  if FAutoPump and FActive then
    StartPump;
end;

procedure TFFReader.UnsubscribePacketSink(const ASink: IFFPacketSink);
begin
  if ASink = nil then
    Exit;
  FPacketSinks.Remove(ASink);
end;

end.
