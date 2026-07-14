unit uFFDecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Decoder for one stream from TFFReader (send_packet / receive_frame).
  Implements packet sink + frame source for component graph linking. }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavcodec,
  libavformat,
  libavutil,
  uFFException,
  uFFReader,
  uFFPacket,
  uFFFrame,
  uFFComponentBase,
  uFFComponentLink,
  uFFHooks,
  uFFHardwareDecode,
  uFFDesignTime
  {$IFDEF MSWINDOWS}
  , Vcl.Graphics
  {$ENDIF}
  ;

type
  TFFDecoderDetails = record
    Index: Integer;
    MediaType: AVMediaType;
    CodecName: string;
    ProfileName: string;
    Width: Integer;
    Height: Integer;
    SampleRate: Integer;
    Channels: Integer;
    BitRate: Int64;
    FrameRateNum: Integer;
    FrameRateDen: Integer;
    DurationMs: Int64;
    TimeBaseNum: Integer;
    TimeBaseDen: Integer;
  end;

  TFFDecoderFrameEvent = procedure(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64) of object;

  TFFDecoder = class(TComponent, IFFPacketSink, IFFFrameSource)
  private
    FReader: TFFReader;
    FStreamIndex: Integer;
    FAutoInitialize: Boolean;
    FCodecCtx: PAVCodecContext;
    FCodec: PAVCodec;
    FInitialized: Boolean;
    FFrameSinks: TFFFrameSinkList;
    FOnFrameHook: TFFFrameHookEvent;
    FOnFrameDecoded: TFFDecoderFrameEvent;
    FPreviewMaxPackets: Integer;
    FHardwareDevice: TFFHardwareDevice;
    FHwDecode: TFFHardwareDecodeContext;
    FHwScratch: TFFFrame;
    function GetCodecName: string;
    function GetDurationMs: Int64;
    procedure SetPreviewMaxPackets(const Value: Integer);
    procedure SubscribeReader;
    procedure UnsubscribeReader;
    procedure EnsureInitialized;
    procedure DispatchFrame(AFrame: TFFFrame);
    procedure SetReader(const Value: TFFReader);
    procedure SetAutoInitialize(const Value: Boolean);
    function DecodeFromReader(AFrame: TFFFrame; AMaxPackets: Integer): Integer;
    function MsToStreamTimestamp(APositionMs: Int64): Int64;
    function ReceiveSoftwareFrame(AFrame: TFFFrame): Integer;
    procedure SetHardwareDevice(const Value: TFFHardwareDevice);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;

    procedure Initialize;
    procedure CloseCodec;
    function SendPacket(APacket: TFFPacket): Integer;
    function ReceiveFrame(AFrame: TFFFrame): Integer;
    procedure Flush;

    procedure TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
    procedure SubscribeFrameSink(const ASink: IFFFrameSink);
    procedure UnsubscribeFrameSink(const ASink: IFFFrameSink);

    function GetStreamDetails: TFFDecoderDetails;
    function DecodeFrameAt(APositionMs: Int64; AFrame: TFFFrame): Integer;
    function DecodeNextFrame(AFrame: TFFFrame; AMaxPackets: Integer = 0): Integer;

    property CodecContext: PAVCodecContext read FCodecCtx;
    property Initialized: Boolean read FInitialized;
    property DurationMs: Int64 read GetDurationMs;
  published
    property Reader: TFFReader read FReader write SetReader;
    property StreamIndex: Integer read FStreamIndex write FStreamIndex default -1;
    property AutoInitialize: Boolean read FAutoInitialize write SetAutoInitialize default True;
    property PreviewMaxPackets: Integer read FPreviewMaxPackets write SetPreviewMaxPackets default 500;
    property CodecName: string read GetCodecName;
    property HardwareDevice: TFFHardwareDevice read FHardwareDevice write SetHardwareDevice default ffhdNone;
    property OnFrameHook: TFFFrameHookEvent read FOnFrameHook write FOnFrameHook;
    property OnFrameDecoded: TFFDecoderFrameEvent read FOnFrameDecoded write FOnFrameDecoded;
  end;

{$IFDEF MSWINDOWS}
procedure FFDecoderFrameToBitmap(ADecoder: TFFDecoder; AFrame: TFFFrame; ABitmap: TBitmap); overload;
procedure FFDecoderPreviewToBitmap(ADecoder: TFFDecoder; APositionMs: Int64; ABitmap: TBitmap); overload;
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS}
  uFFFrameConverter,
  uFFFrameBitmap;
  {$ENDIF}

{$IFDEF MSWINDOWS}
procedure FFDecoderFrameToBitmap(ADecoder: TFFDecoder; AFrame: TFFFrame; ABitmap: TBitmap);
var
  Converter: TFFFrameConverter;
begin
  if (ADecoder = nil) or (AFrame = nil) or (ABitmap = nil) then
    raise EFFException.Create('FFDecoderFrameToBitmap: invalid argument');
  Converter := TFFFrameConverter.Create;
  try
    TFFFrameBitmap.AssignFromConverter(Converter, AFrame, ABitmap);
  finally
    Converter.Free;
  end;
end;

procedure FFDecoderPreviewToBitmap(ADecoder: TFFDecoder; APositionMs: Int64; ABitmap: TBitmap);
var
  Frame: TFFFrame;
begin
  if (ADecoder = nil) or (ABitmap = nil) then
    raise EFFException.Create('FFDecoderPreviewToBitmap: invalid argument');
  Frame := TFFFrame.Create;
  try
    if ADecoder.DecodeFrameAt(APositionMs, Frame) < 0 then
      raise EFFException.CreateFmt('DecodeFrameAt failed at %d ms', [APositionMs]);
    FFDecoderFrameToBitmap(ADecoder, Frame, ABitmap);
  finally
    Frame.Free;
  end;
end;
{$ENDIF}

constructor TFFDecoder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStreamIndex := -1;
  FAutoInitialize := True;
  FPreviewMaxPackets := 500;
  FHardwareDevice := ffhdNone;
  FFrameSinks := TFFFrameSinkList.Create;
end;

destructor TFFDecoder.Destroy;
begin
  UnsubscribeReader;
  if Assigned(FReader) then
    FReader.RemoveFreeNotification(Self);
  FFrameSinks.Free;
  FHwScratch.Free;
  FHwDecode.Free;
  CloseCodec;
  inherited;
end;

procedure TFFDecoder.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FReader;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    UnsubscribeReader;
    if FInitialized then
      CloseCodec;
    FReader := TFFReader(Link);
  end;
end;

procedure TFFDecoder.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SubscribeReader;
end;

procedure TFFDecoder.SubscribeReader;
begin
  if (FReader = nil) or (csDesigning in ComponentState) then
    Exit;
  FReader.SubscribePacketSink(Self);
end;

procedure TFFDecoder.UnsubscribeReader;
begin
  if FReader = nil then
    Exit;
  FReader.UnsubscribePacketSink(Self);
end;

procedure TFFDecoder.SetReader(const Value: TFFReader);
var
  Link: TComponent;
begin
  if FReader = Value then
    Exit;
  UnsubscribeReader;
  if FInitialized then
    CloseCodec;
  Link := FReader;
  FFSetLinkedComponent(Self, Link, Value);
  FReader := TFFReader(Link);
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    SubscribeReader;
end;

procedure TFFDecoder.SetAutoInitialize(const Value: Boolean);
begin
  FAutoInitialize := Value;
end;

procedure TFFDecoder.SetPreviewMaxPackets(const Value: Integer);
begin
  if Value <= 0 then
    FPreviewMaxPackets := 500
  else
    FPreviewMaxPackets := Value;
end;

function TFFDecoder.GetDurationMs: Int64;
var
  Details: TFFDecoderDetails;
begin
  Details := GetStreamDetails;
  Result := Details.DurationMs;
end;

function TFFDecoder.GetStreamDetails: TFFDecoderDetails;
var
  St: PAVStream;
  Par: PAVCodecParameters;
  MsBase: AVRational;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Index := FStreamIndex;
  if (FReader = nil) or not FReader.Active or (FStreamIndex < 0) then
    Exit;

  St := FReader.GetStream(FStreamIndex);
  Par := St^.codecpar;
  Result.MediaType := Par^.codec_type;
  Result.CodecName := string(avcodec_get_name(Par^.codec_id));
  Result.Width := Par^.width;
  Result.Height := Par^.height;
  Result.SampleRate := Par^.sample_rate;
  Result.Channels := Par^.ch_layout.nb_channels;
  if Par^.bit_rate > 0 then
    Result.BitRate := Par^.bit_rate;

  if (St^.avg_frame_rate.num > 0) and (St^.avg_frame_rate.den > 0) then
  begin
    Result.FrameRateNum := St^.avg_frame_rate.num;
    Result.FrameRateDen := St^.avg_frame_rate.den;
  end
  else if (St^.r_frame_rate.num > 0) and (St^.r_frame_rate.den > 0) then
  begin
    Result.FrameRateNum := St^.r_frame_rate.num;
    Result.FrameRateDen := St^.r_frame_rate.den;
  end;

  Result.TimeBaseNum := St^.time_base.num;
  Result.TimeBaseDen := St^.time_base.den;

  MsBase := av_make_q(1, 1000);
  if St^.duration <> AV_NOPTS_VALUE then
    Result.DurationMs := av_rescale_q(St^.duration, St^.time_base, MsBase)
  else if FReader.Duration > 0 then
    Result.DurationMs := FReader.Duration div 1000
  else if (St^.nb_frames > 0) and (Result.FrameRateNum > 0) and (Result.FrameRateDen > 0) then
    Result.DurationMs := Int64(St^.nb_frames) * 1000 * Result.FrameRateDen div Result.FrameRateNum;
end;

function TFFDecoder.MsToStreamTimestamp(APositionMs: Int64): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
begin
  St := FReader.GetStream(FStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(APositionMs, MsBase, St^.time_base);
end;

function TFFDecoder.DecodeFromReader(AFrame: TFFFrame; AMaxPackets: Integer): Integer;
var
  Packet: TFFPacket;
  Ret: Integer;
  I: Integer;
  Limit: Integer;
begin
  if not FInitialized then
    Exit(-1);
  if (FReader = nil) or not FReader.Active then
    Exit(-1);
  if AFrame = nil then
    raise EFFException.Create('DecodeFromReader: frame is nil');

  if AMaxPackets <= 0 then
    Limit := FPreviewMaxPackets
  else
    Limit := AMaxPackets;

  Packet := TFFPacket.Create;
  try
    for I := 1 to Limit do
    begin
      if not FReader.ReadPacket(Packet) then
        Exit(AVERROR_EOF);

      if Packet.Raw^.stream_index <> FStreamIndex then
        Continue;

      Ret := SendPacket(Packet);
      if Ret = AVERROR_EAGAIN then
        Continue;
      if Ret < 0 then
        Exit(Ret);

      Ret := ReceiveFrame(AFrame);
      if Ret = 0 then
        Exit(0);
      if Ret = AVERROR_EAGAIN then
        Continue;
      Exit(Ret);
    end;
    Result := AVERROR_EAGAIN;
  finally
    Packet.Free;
  end;
end;

function TFFDecoder.DecodeNextFrame(AFrame: TFFFrame; AMaxPackets: Integer): Integer;
begin
  EnsureInitialized;
  if not FInitialized then
    raise EFFException.Create('TFFDecoder is not initialized');
  Result := DecodeFromReader(AFrame, AMaxPackets);
  if Result = 0 then
  begin
    if Assigned(FOnFrameDecoded) then
      FOnFrameDecoded(Self, AFrame, -1);
    DispatchFrame(AFrame);
  end;
end;

function TFFDecoder.DecodeFrameAt(APositionMs: Int64; AFrame: TFFFrame): Integer;
var
  WasAutoPump: Boolean;
begin
  if FReader = nil then
    raise EFFException.Create('TFFDecoder.Reader is not assigned');
  if not FReader.Active then
    raise EFFException.Create('TFFDecoder.Reader is not active');
  if FStreamIndex < 0 then
    raise EFFException.Create('TFFDecoder.StreamIndex is not set');
  if AFrame = nil then
    raise EFFException.Create('DecodeFrameAt: frame is nil');

  EnsureInitialized;
  if not FInitialized then
    raise EFFException.Create('TFFDecoder is not initialized');

  WasAutoPump := FReader.AutoPump;
  if WasAutoPump then
    FReader.AutoPump := False;
  try
    if APositionMs > 0 then
      FReader.Seek(MsToStreamTimestamp(APositionMs), FStreamIndex);
    Flush;
    AFrame.Clear;
    Result := DecodeFromReader(AFrame, FPreviewMaxPackets);
    if Result = 0 then
    begin
      if Assigned(FOnFrameDecoded) then
        FOnFrameDecoded(Self, AFrame, APositionMs);
    end;
  finally
    if WasAutoPump then
      FReader.AutoPump := True;
  end;
end;

procedure TFFDecoder.SetHardwareDevice(const Value: TFFHardwareDevice);
begin
  if FHardwareDevice = Value then
    Exit;
  if FInitialized then
    raise EFFException.Create('Change HardwareDevice only while decoder is closed');
  FHardwareDevice := Value;
end;

procedure TFFDecoder.CloseCodec;
begin
  if FFIsDesignTime(Self) then
  begin
    FCodecCtx := nil;
    FCodec := nil;
    FInitialized := False;
    FreeAndNil(FHwDecode);
    Exit;
  end;
  if FCodecCtx <> nil then
    avcodec_free_context(FCodecCtx);
  FCodecCtx := nil;
  FCodec := nil;
  FInitialized := False;
  FreeAndNil(FHwDecode);
end;

function TFFDecoder.ReceiveSoftwareFrame(AFrame: TFFFrame): Integer;
var
  Ret: Integer;
begin
  if not FInitialized then
    raise EFFException.Create('TFFDecoder is not initialized');
  if AFrame = nil then
    raise EFFException.Create('ReceiveSoftwareFrame: frame is nil');

  Ret := avcodec_receive_frame(FCodecCtx, AFrame.Raw);
  if Ret <> 0 then
    Exit(Ret);

  if (FHwDecode <> nil) and FHwDecode.Active and
     FFIsHardwarePixelFormat(AVPixelFormat(AFrame.Raw^.format)) then
  begin
    if FHwScratch = nil then
      FHwScratch := TFFFrame.Create;
    Ret := FHwDecode.TransferToSoftware(AFrame, FHwScratch);
    if Ret < 0 then
      Exit(Ret);
    AFrame.Clear;
    av_frame_ref(AFrame.Raw, FHwScratch.Raw);
  end;

  Result := 0;
end;

procedure TFFDecoder.Flush;
begin
  if not FInitialized then
    raise EFFException.Create('TFFDecoder is not initialized');
  avcodec_flush_buffers(FCodecCtx);
end;

function TFFDecoder.GetCodecName: string;
begin
  if FCodecCtx = nil then
    Exit('');
  Result := string(avcodec_get_name(FCodecCtx^.codec_id));
end;

procedure TFFDecoder.EnsureInitialized;
begin
  if not FAutoInitialize or FInitialized then
    Exit;
  if (FReader <> nil) and FReader.Active and (FStreamIndex >= 0) then
    Initialize;
end;

procedure TFFDecoder.Initialize;
var
  St: PAVStream;
  Ret: Integer;
begin
  CloseCodec;
  if FReader = nil then
    raise EFFException.Create('TFFDecoder.Reader is not assigned');
  if not FReader.Active then
    raise EFFException.Create('TFFDecoder.Reader is not active');
  if FStreamIndex < 0 then
    raise EFFException.Create('TFFDecoder.StreamIndex is not set');

  St := FReader.GetStream(FStreamIndex);
  FCodec := avcodec_find_decoder(St^.codecpar^.codec_id);
  if FCodec = nil then
    raise EFFException.Create('avcodec_find_decoder failed');

  FCodecCtx := avcodec_alloc_context3(FCodec);
  if FCodecCtx = nil then
    raise EFFException.Create('avcodec_alloc_context3 failed');

  Ret := avcodec_parameters_to_context(FCodecCtx, St^.codecpar);
  if Ret < 0 then
  begin
    CloseCodec;
    raise EFFException.CreateFmt('avcodec_parameters_to_context failed (%d)', [Ret]);
  end;

  if FHardwareDevice <> ffhdNone then
  begin
    FHwDecode := TFFHardwareDecodeContext.Create;
    if not FHwDecode.TrySetup(FCodecCtx, FCodec, FHardwareDevice) then
      FreeAndNil(FHwDecode);
  end;

  Ret := avcodec_open2(FCodecCtx, FCodec, nil);
  if Ret < 0 then
  begin
    CloseCodec;
    raise EFFException.CreateFmt('avcodec_open2 failed (%d)', [Ret]);
  end;

  FInitialized := True;
end;

function TFFDecoder.ReceiveFrame(AFrame: TFFFrame): Integer;
begin
  Result := ReceiveSoftwareFrame(AFrame);
end;

function TFFDecoder.SendPacket(APacket: TFFPacket): Integer;
begin
  if not FInitialized then
    raise EFFException.Create('TFFDecoder is not initialized');
  if APacket = nil then
    Result := avcodec_send_packet(FCodecCtx, nil)
  else
    Result := avcodec_send_packet(FCodecCtx, APacket.Raw);
end;

procedure TFFDecoder.DispatchFrame(AFrame: TFFFrame);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnFrameHook) then
    FOnFrameHook(Self, AFrame, Handled);
  if Handled then
    Exit;
  FFrameSinks.Notify(Self, AFrame, FStreamIndex);
end;

procedure TFFDecoder.TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
var
  Frame: TFFFrame;
  Ret: Integer;
begin
  if (csDestroying in ComponentState) or (csDesigning in ComponentState) then
    Exit;
  if (APkt = nil) or (AStreamIndex <> FStreamIndex) then
    Exit;

  EnsureInitialized;
  if not FInitialized then
    Exit;

  Ret := SendPacket(APkt);
  if Ret < 0 then
    Exit;

  Frame := TFFFrame.Create;
  try
    while not (csDestroying in ComponentState) do
    begin
      Ret := ReceiveFrame(Frame);
      if Ret = 0 then
        DispatchFrame(Frame)
      else if Ret = AVERROR_EAGAIN then
        Break
      else if Ret = AVERROR_EOF then
        Break
      else
        Break;
    end;
  finally
    Frame.Free;
  end;
end;

procedure TFFDecoder.SubscribeFrameSink(const ASink: IFFFrameSink);
begin
  if (ASink = nil) or (csDesigning in ComponentState) then
    Exit;
  FFrameSinks.Add(ASink);
end;

procedure TFFDecoder.UnsubscribeFrameSink(const ASink: IFFFrameSink);
begin
  if ASink = nil then
    Exit;
  FFrameSinks.Remove(ASink);
end;

end.
