unit uFFEncoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Encoder for one audio or video stream (send_frame / receive_packet).
  Optional transcode pipeline via Reader + InputDecoder + OutputWriter links. }

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
  libavcodec,
  libavutil,
  uFFException,
  uFFReader,
  uFFDecoder,
  uFFFrame,
  uFFPacket,
  uFFHooks,
  uFFComponentLink,
  uFFFrameFilter,
  uFFDesignTime;

type
  TFFEncodeState = (esStopped, esRunning, esPaused, esStopping);

  TFFEncodeProgressEvent = procedure(Sender: TObject; APositionMs, ADurationMs: Int64) of object;

  TFFEncoder = class(TComponent)
  private
    FCodecCtx: PAVCodecContext;
    FCodec: PAVCodec;
    FInitialized: Boolean;
    FMediaType: AVMediaType;
    FCodecId: AVCodecID;
    FCodecName: string;
    FBitRate: Int64;
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: AVPixelFormat;
    FSampleRate: Integer;
    FChannels: Integer;
    FSampleFormat: AVSampleFormat;
    FTimeBaseNum: Integer;
    FTimeBaseDen: Integer;
    FFrameRateNum: Integer;
    FFrameRateDen: Integer;
    FGopSize: Integer;
    FMaxBFrames: Integer;
    FOnFrameHook: TFFFrameHookEvent;
    FOptions: TStrings;
    FCopyAudio: Boolean;
    FAudioStreamIndex: Integer;
    FTranscodeAudio: Boolean;
    FAudioCodecName: string;
    FAudioBitRate: Int64;
    FAudioSampleRate: Integer;
    FAudioChannels: Integer;
    FAudioSampleFormat: AVSampleFormat;
    FAudioOptions: TStrings;
    FReader: TFFReader;
    FInputDecoder: TFFDecoder;
    FOutputWriter: TComponent;
    FState: TFFEncodeState;
    FEncodeThread: TThread;
    FActiveEngine: TObject;
    FOnProgress: TFFEncodeProgressEvent;
    FOnPreviewFrame: TFFPreviewFrameEvent;
    FOnStateChange: TNotifyEvent;
    FFrameFilter: TFFFrameFilter;
    FStartMs: Int64;
    FEndMs: Int64;
    FSourceCodecPar: PAVCodecParameters;
    function GetCodecName: string;
    procedure SetOptions(const Value: TStrings);
    procedure SetAudioOptions(const Value: TStrings);
    procedure SetReader(const Value: TFFReader);
    procedure SetInputDecoder(const Value: TFFDecoder);
    procedure SetOutputWriter(const Value: TComponent);
    procedure SetFrameFilter(const Value: TFFFrameFilter);
    function GetOutputWriter: TComponent;
    procedure SetState(const Value: TFFEncodeState);
    procedure DoStateChange;
    procedure RunTranscode;
    procedure HandleTranscodeProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
    procedure HandleTranscodePreview(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ApplySourceCodecPar(AParams: PAVCodecParameters);
    procedure Initialize;
    procedure CloseCodec;
    function SendFrame(AFrame: TFFFrame): Integer;
    function ReceivePacket(APacket: TFFPacket): Integer;
    procedure Flush;

    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure EncodeFinished;

    property CodecContext: PAVCodecContext read FCodecCtx;
    property Initialized: Boolean read FInitialized;
    property State: TFFEncodeState read FState;
  published
    property MediaType: AVMediaType read FMediaType write FMediaType default AVMEDIA_TYPE_VIDEO;
    property CodecId: AVCodecID read FCodecId write FCodecId default AV_CODEC_ID_NONE;
    property CodecName: string read FCodecName write FCodecName;
    property BitRate: Int64 read FBitRate write FBitRate;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property PixelFormat: AVPixelFormat read FPixelFormat write FPixelFormat default AV_PIX_FMT_YUV420P;
    property SampleRate: Integer read FSampleRate write FSampleRate default 44100;
    property Channels: Integer read FChannels write FChannels default 2;
    property SampleFormat: AVSampleFormat read FSampleFormat write FSampleFormat default AV_SAMPLE_FMT_FLTP;
    property TimeBaseNum: Integer read FTimeBaseNum write FTimeBaseNum default 1;
    property TimeBaseDen: Integer read FTimeBaseDen write FTimeBaseDen default 25;
    property FrameRateNum: Integer read FFrameRateNum write FFrameRateNum default 25;
    property FrameRateDen: Integer read FFrameRateDen write FFrameRateDen default 1;
    property GopSize: Integer read FGopSize write FGopSize default 12;
    property MaxBFrames: Integer read FMaxBFrames write FMaxBFrames default 2;
    property EncoderCodecName: string read GetCodecName;
    property Options: TStrings read FOptions write SetOptions;
    property CopyAudio: Boolean read FCopyAudio write FCopyAudio default False;
    property AudioStreamIndex: Integer read FAudioStreamIndex write FAudioStreamIndex default -1;
    property TranscodeAudio: Boolean read FTranscodeAudio write FTranscodeAudio default False;
    property AudioCodecName: string read FAudioCodecName write FAudioCodecName;
    property AudioBitRate: Int64 read FAudioBitRate write FAudioBitRate;
    property AudioSampleRate: Integer read FAudioSampleRate write FAudioSampleRate default 44100;
    property AudioChannels: Integer read FAudioChannels write FAudioChannels default 2;
    property AudioSampleFormat: AVSampleFormat read FAudioSampleFormat write FAudioSampleFormat default AV_SAMPLE_FMT_FLTP;
    property AudioOptions: TStrings read FAudioOptions write SetAudioOptions;
    property Reader: TFFReader read FReader write SetReader;
    property InputDecoder: TFFDecoder read FInputDecoder write SetInputDecoder;
    property OutputWriter: TComponent read GetOutputWriter write SetOutputWriter;
    property FrameFilter: TFFFrameFilter read FFrameFilter write SetFrameFilter;
    property StartMs: Int64 read FStartMs write FStartMs default 0;
    property EndMs: Int64 read FEndMs write FEndMs default 0;
    property OnFrameHook: TFFFrameHookEvent read FOnFrameHook write FOnFrameHook;
    property OnProgress: TFFEncodeProgressEvent read FOnProgress write FOnProgress;
    property OnPreviewFrame: TFFPreviewFrameEvent read FOnPreviewFrame write FOnPreviewFrame;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

implementation

uses
  uFFWriter,
  uFFTranscodeEngine;

type
  TFFEncodeThread = class(TThread)
  private
    FEncoder: TFFEncoder;
  protected
    procedure Execute; override;
  public
    constructor Create(AEncoder: TFFEncoder);
  end;

constructor TFFEncodeThread.Create(AEncoder: TFFEncoder);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FEncoder := AEncoder;
end;

procedure TFFEncodeThread.Execute;
begin
  try
    FEncoder.RunTranscode;
  finally
    FEncoder.EncodeFinished;
  end;
end;

procedure TFFEncoder.CloseCodec;
begin
  if FFIsDesignTime(Self) then
  begin
    FCodecCtx := nil;
    FCodec := nil;
    FInitialized := False;
    Exit;
  end;
  if FCodecCtx <> nil then
    avcodec_free_context(FCodecCtx);
  FCodecCtx := nil;
  FCodec := nil;
  FInitialized := False;
end;

constructor TFFEncoder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMediaType := AVMEDIA_TYPE_VIDEO;
  FCodecId := AV_CODEC_ID_NONE;
  FBitRate := 400000;
  FOptions := TStringList.Create;
  FCopyAudio := False;
  FAudioStreamIndex := -1;
  FTranscodeAudio := False;
  FAudioCodecName := '';
  FAudioBitRate := 128000;
  FAudioSampleRate := 44100;
  FAudioChannels := 2;
  FAudioSampleFormat := AV_SAMPLE_FMT_FLTP;
  FAudioOptions := TStringList.Create;
  FPixelFormat := AV_PIX_FMT_YUV420P;
  FSampleRate := 44100;
  FChannels := 2;
  FSampleFormat := AV_SAMPLE_FMT_FLTP;
  FTimeBaseNum := 1;
  FTimeBaseDen := 25;
  FFrameRateNum := 25;
  FFrameRateDen := 1;
  FGopSize := 12;
  FMaxBFrames := 2;
  FState := esStopped;
end;

destructor TFFEncoder.Destroy;
begin
  Stop;
  if Assigned(FReader) then
    FReader.RemoveFreeNotification(Self);
  if Assigned(FInputDecoder) then
    FInputDecoder.RemoveFreeNotification(Self);
  if Assigned(FOutputWriter) then
    FOutputWriter.RemoveFreeNotification(Self);
  if Assigned(FFrameFilter) then
    FFrameFilter.RemoveFreeNotification(Self);
  FOptions.Free;
  FAudioOptions.Free;
  CloseCodec;
  inherited;
end;

procedure TFFEncoder.SetOptions(const Value: TStrings);
begin
  if (FOptions = nil) or (Value = nil) then
    Exit;
  FOptions.Assign(Value);
end;

procedure TFFEncoder.SetAudioOptions(const Value: TStrings);
begin
  if (FAudioOptions = nil) or (Value = nil) then
    Exit;
  FAudioOptions.Assign(Value);
end;

procedure TFFEncoder.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FReader;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FReader := TFFReader(Link);
  Link := FInputDecoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FInputDecoder := TFFDecoder(Link);
  Link := FOutputWriter;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FOutputWriter := Link;
  Link := FFrameFilter;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FFrameFilter := TFFFrameFilter(Link);
end;

function TFFEncoder.GetOutputWriter: TComponent;
begin
  Result := FOutputWriter;
end;

procedure TFFEncoder.SetOutputWriter(const Value: TComponent);
var
  Link: TComponent;
begin
  if FOutputWriter = Value then
    Exit;
  if (Value <> nil) and not (Value is TFFWriter) then
    raise EFFException.Create('TFFEncoder.OutputWriter must be a TFFWriter');
  Link := FOutputWriter;
  FFSetLinkedComponent(Self, Link, Value);
  FOutputWriter := Link;
end;

procedure TFFEncoder.SetFrameFilter(const Value: TFFFrameFilter);
var
  Link: TComponent;
begin
  if FFrameFilter = Value then
    Exit;
  Link := FFrameFilter;
  FFSetLinkedComponent(Self, Link, Value);
  FFrameFilter := TFFFrameFilter(Link);
end;

procedure TFFEncoder.SetReader(const Value: TFFReader);
var
  Link: TComponent;
begin
  if FReader = Value then
    Exit;
  Link := FReader;
  FFSetLinkedComponent(Self, Link, Value);
  FReader := TFFReader(Link);
end;

procedure TFFEncoder.SetInputDecoder(const Value: TFFDecoder);
var
  Link: TComponent;
begin
  if FInputDecoder = Value then
    Exit;
  Link := FInputDecoder;
  FFSetLinkedComponent(Self, Link, Value);
  FInputDecoder := TFFDecoder(Link);
end;

procedure TFFEncoder.SetState(const Value: TFFEncodeState);
begin
  if FState = Value then
    Exit;
  FState := Value;
  DoStateChange;
end;

procedure TFFEncoder.DoStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TFFEncoder.HandleTranscodeProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, APositionMs, ADurationMs);
end;

procedure TFFEncoder.HandleTranscodePreview(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64);
begin
  if Assigned(FOnPreviewFrame) then
    FOnPreviewFrame(Self, AFrame, APositionMs);
end;

procedure TFFEncoder.RunTranscode;
var
  Engine: TFFTranscodeEngine;
begin
  Engine := TFFTranscodeEngine.Create;
  FActiveEngine := Engine;
  try
    Engine.Reader := FReader;
    Engine.InputDecoder := FInputDecoder;
    Engine.Encoder := Self;
    Engine.Writer := TFFWriter(FOutputWriter);
    Engine.OnProgress := HandleTranscodeProgress;
    Engine.OnPreviewFrame := HandleTranscodePreview;
    Engine.CopyAudio := FCopyAudio;
    Engine.AudioStreamIndex := FAudioStreamIndex;
    Engine.TranscodeAudio := FTranscodeAudio;
    Engine.AudioCodecName := FAudioCodecName;
    Engine.AudioBitRate := FAudioBitRate;
    Engine.AudioSampleRate := FAudioSampleRate;
    Engine.AudioChannels := FAudioChannels;
    Engine.AudioSampleFormat := FAudioSampleFormat;
    if Engine.AudioOptions <> nil then
      Engine.AudioOptions.Assign(FAudioOptions);
    Engine.StartMs := FStartMs;
    Engine.EndMs := FEndMs;
    Engine.FrameFilter := FFrameFilter;
    Engine.Execute;
  finally
    Engine.Free;
    FActiveEngine := nil;
  end;
end;

procedure TFFEncoder.Start;
begin
  if FState = esRunning then
    Exit;
  if (FReader = nil) or (FInputDecoder = nil) or (FOutputWriter = nil) then
    raise EFFException.Create('TFFEncoder.Start: Reader, InputDecoder and OutputWriter must be assigned');
  if not TFFWriter(FOutputWriter).HasOutputTarget then
    raise EFFException.Create('TFFEncoder.Start: OutputWriter needs FileName or OutputAdapter');

  Stop;
  SetState(esRunning);
  FEncodeThread := TFFEncodeThread.Create(Self);
  FEncodeThread.Start;
end;

procedure TFFEncoder.Pause;
begin
  if FState <> esRunning then
    Exit;
  if FActiveEngine <> nil then
    TFFTranscodeEngine(FActiveEngine).SetPaused(True);
  SetState(esPaused);
end;

procedure TFFEncoder.Resume;
begin
  if FState <> esPaused then
    Exit;
  if FActiveEngine <> nil then
    TFFTranscodeEngine(FActiveEngine).SetPaused(False);
  SetState(esRunning);
end;

procedure TFFEncoder.EncodeFinished;
begin
  if FState in [esRunning, esPaused, esStopping] then
    SetState(esStopped);
end;

procedure TFFEncoder.Stop;
begin
  if FActiveEngine <> nil then
    TFFTranscodeEngine(FActiveEngine).RequestStop;
  if FEncodeThread = nil then
  begin
    SetState(esStopped);
    Exit;
  end;

  SetState(esStopping);
  FEncodeThread.Terminate;
  if FFIsDesignTime(Self) then
  begin
    FEncodeThread.FreeOnTerminate := True;
    FEncodeThread := nil;
    SetState(esStopped);
    Exit;
  end;
  FEncodeThread.WaitFor;
  FreeAndNil(FEncodeThread);
  SetState(esStopped);
end;

procedure TFFEncoder.Flush;
begin
  if not FInitialized then
    raise EFFException.Create('TFFEncoder is not initialized');
  avcodec_send_frame(FCodecCtx, nil);
end;

function TFFEncoder.GetCodecName: string;
begin
  if FCodecCtx = nil then
    Exit('');
  Result := string(avcodec_get_name(FCodecCtx^.codec_id));
end;

procedure TFFEncoder.ApplySourceCodecPar(AParams: PAVCodecParameters);
begin
  FSourceCodecPar := AParams;
end;

procedure TFFEncoder.Initialize;
var
  Ret: Integer;
  Layout: AVChannelLayout;
  Opts: pAVDictionary;
  I: Integer;
  S, K, V: string;
  Eq: Integer;
begin
  CloseCodec;

  if FCodecName <> '' then
    FCodec := avcodec_find_encoder_by_name(PAnsiChar(UTF8String(FCodecName)))
  else if FCodecId <> AV_CODEC_ID_NONE then
    FCodec := avcodec_find_encoder(FCodecId)
  else if FMediaType = AVMEDIA_TYPE_AUDIO then
    FCodec := avcodec_find_encoder(AV_CODEC_ID_AAC)
  else
    FCodec := avcodec_find_encoder(AV_CODEC_ID_MPEG4);

  if FCodec = nil then
    raise EFFException.Create('avcodec_find_encoder failed');

  FCodecCtx := avcodec_alloc_context3(FCodec);
  if FCodecCtx = nil then
    raise EFFException.Create('avcodec_alloc_context3 failed');

  FCodecCtx^.codec_id := FCodec^.id;
  FCodecCtx^.bit_rate := FBitRate;
  FCodecCtx^.time_base.num := FTimeBaseNum;
  FCodecCtx^.time_base.den := FTimeBaseDen;

  if FMediaType = AVMEDIA_TYPE_VIDEO then
  begin
    if (FWidth <= 0) or (FHeight <= 0) then
      raise EFFException.Create('TFFEncoder: Width and Height must be set for video');
    FCodecCtx^.width := FWidth;
    FCodecCtx^.height := FHeight;
    FCodecCtx^.pix_fmt := FPixelFormat;
    FCodecCtx^.gop_size := FGopSize;
    FCodecCtx^.max_b_frames := FMaxBFrames;
    FCodecCtx^.framerate.num := FFrameRateNum;
    FCodecCtx^.framerate.den := FFrameRateDen;
    if FCodecCtx^.pix_fmt = AV_PIX_FMT_NONE then
      FCodecCtx^.pix_fmt := AV_PIX_FMT_YUV420P;
  end
  else
  begin
    if FSourceCodecPar <> nil then
    begin
      Ret := avcodec_parameters_to_context(FCodecCtx, FSourceCodecPar);
      if Ret < 0 then
      begin
        CloseCodec;
        raise EFFException.CreateFmt('avcodec_parameters_to_context failed (%d)', [Ret]);
      end;
      if FBitRate > 0 then
        FCodecCtx^.bit_rate := FBitRate;
      if FSampleRate > 0 then
        FCodecCtx^.sample_rate := FSampleRate;
      if FSampleFormat <> AV_SAMPLE_FMT_NONE then
        FCodecCtx^.sample_fmt := FSampleFormat;
    end
    else
    begin
      FCodecCtx^.sample_rate := FSampleRate;
      FCodecCtx^.sample_fmt := FSampleFormat;
      av_channel_layout_default(Layout, FChannels);
      Ret := av_channel_layout_copy(FCodecCtx^.ch_layout, @Layout);
      if Ret < 0 then
      begin
        CloseCodec;
        raise EFFException.CreateFmt('av_channel_layout_copy failed (%d)', [Ret]);
      end;
    end;
  end;

  Opts := nil;
  try
    if FOptions <> nil then
      for I := 0 to FOptions.Count - 1 do
      begin
        S := Trim(FOptions[I]);
        if (S = '') or (S[1] = '#') then
          Continue;
        Eq := Pos('=', S);
        if Eq <= 0 then
          Continue;
        K := Trim(Copy(S, 1, Eq - 1));
        V := Trim(Copy(S, Eq + 1, MaxInt));
        if K = '' then
          Continue;
        av_dict_set(Opts, PAnsiChar(UTF8String(K)), PAnsiChar(UTF8String(V)), 0);
      end;

    Ret := avcodec_open2(FCodecCtx, FCodec, @Opts);
  finally
    if Opts <> nil then
      av_dict_free(Opts);
  end;
  if Ret < 0 then
  begin
    CloseCodec;
    raise EFFException.CreateFmt('avcodec_open2 failed (%d)', [Ret]);
  end;

  FInitialized := True;
end;

function TFFEncoder.ReceivePacket(APacket: TFFPacket): Integer;
begin
  if not FInitialized then
    raise EFFException.Create('TFFEncoder is not initialized');
  if APacket = nil then
    raise EFFException.Create('ReceivePacket: packet is nil');
  Result := avcodec_receive_packet(FCodecCtx, APacket.Raw);
end;

function TFFEncoder.SendFrame(AFrame: TFFFrame): Integer;
var
  Handled: Boolean;
begin
  if not FInitialized then
    raise EFFException.Create('TFFEncoder is not initialized');
  if AFrame = nil then
    Exit(avcodec_send_frame(FCodecCtx, nil));
  Handled := False;
  if Assigned(FOnFrameHook) then
    FOnFrameHook(Self, AFrame, Handled);
  if Handled then
    Exit(0);
  Result := avcodec_send_frame(FCodecCtx, AFrame.Raw);
end;

end.
