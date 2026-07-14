unit uFFWriter;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Muxer wrapper around AVFormatContext for writing media files. }

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
  uFFEncoder,
  uFFPacket,
  uFFMemoryAccessAdapter,
  uFFComponentBase,
  uFFComponentLink,
  uFFDesignTime;

type
  TFFStreamMapItem = record
    InIndex: Integer;
    OutIndex: Integer;
  end;

  TFFWriter = class(TComponent, IFFPacketSink)
  private
    FFileName: string;
    FFormatName: string;
    FOutputAdapter: TFFMemoryAccessAdapter;
    FFormatCtx: PAVFormatContext;
    FActive: Boolean;
    FHeaderWritten: Boolean;
    FAutoSetup: Boolean;
    FRemuxReader: TFFReader;
    FVideoEncoder: TFFEncoder;
    FAudioEncoder: TFFEncoder;
    FStreamMaps: array of TFFStreamMapItem;
    function FindOutIndex(AInIndex: Integer): Integer;
    function GetOutStream(AOutIndex: Integer): PAVStream;
    procedure SetRemuxReader(const Value: TFFReader);
    procedure SetVideoEncoder(const Value: TFFEncoder);
    procedure SetAudioEncoder(const Value: TFFEncoder);
    procedure SetOutputAdapter(const Value: TFFMemoryAccessAdapter);
    procedure SubscribeRemuxReader;
    procedure UnsubscribeRemuxReader;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;

    procedure Open;
    procedure Close;
    procedure SetupFromLinks;
    function AddStream(AEncoder: TFFEncoder): Integer;
    function AddStreamCopy(AReader: TFFReader; AStreamIndex: Integer): Integer;
    procedure WriteHeader;
    function WritePacket(APacket: TFFPacket; AStreamIndex: Integer): Integer;
    function WritePacketFromReader(APacket: TFFPacket; AReader: TFFReader): Integer;
    procedure WriteTrailer;

    procedure TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);

    function HasOutputTarget: Boolean;

    property FormatContext: PAVFormatContext read FFormatCtx;
    property Active: Boolean read FActive;
  published
    property FileName: string read FFileName write FFileName;
    property FormatName: string read FFormatName write FFormatName;
    property OutputAdapter: TFFMemoryAccessAdapter read FOutputAdapter write SetOutputAdapter;
    property AutoSetup: Boolean read FAutoSetup write FAutoSetup default False;
    property RemuxReader: TFFReader read FRemuxReader write SetRemuxReader;
    property VideoEncoder: TFFEncoder read FVideoEncoder write SetVideoEncoder;
    property AudioEncoder: TFFEncoder read FAudioEncoder write SetAudioEncoder;
  end;

implementation

uses
  uFFmpegPath;

constructor TFFWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFFWriter.Destroy;
begin
  UnsubscribeRemuxReader;
  if Assigned(FRemuxReader) then
    FRemuxReader.RemoveFreeNotification(Self);
  if Assigned(FVideoEncoder) then
    FVideoEncoder.RemoveFreeNotification(Self);
  if Assigned(FAudioEncoder) then
    FAudioEncoder.RemoveFreeNotification(Self);
  Close;
  inherited;
end;

procedure TFFWriter.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FOutputAdapter;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FOutputAdapter := TFFMemoryAccessAdapter(Link);
  Link := FRemuxReader;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    UnsubscribeRemuxReader;
    FRemuxReader := TFFReader(Link);
  end;
  Link := FVideoEncoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FVideoEncoder := TFFEncoder(Link);
  Link := FAudioEncoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FAudioEncoder := TFFEncoder(Link);
end;

procedure TFFWriter.SetOutputAdapter(const Value: TFFMemoryAccessAdapter);
var
  Link: TComponent;
begin
  if FOutputAdapter = Value then
    Exit;
  if FActive then
    Close;
  Link := FOutputAdapter;
  FFSetLinkedComponent(Self, Link, Value);
  FOutputAdapter := TFFMemoryAccessAdapter(Link);
end;

procedure TFFWriter.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SubscribeRemuxReader;
end;

procedure TFFWriter.SubscribeRemuxReader;
begin
  if (FRemuxReader = nil) or (csDesigning in ComponentState) then
    Exit;
  FRemuxReader.SubscribePacketSink(Self);
end;

procedure TFFWriter.UnsubscribeRemuxReader;
begin
  if FRemuxReader = nil then
    Exit;
  FRemuxReader.UnsubscribePacketSink(Self);
end;

procedure TFFWriter.SetRemuxReader(const Value: TFFReader);
var
  Link: TComponent;
begin
  if FRemuxReader = Value then
    Exit;
  UnsubscribeRemuxReader;
  Link := FRemuxReader;
  FFSetLinkedComponent(Self, Link, Value);
  FRemuxReader := TFFReader(Link);
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    SubscribeRemuxReader;
end;

procedure TFFWriter.SetVideoEncoder(const Value: TFFEncoder);
var
  Link: TComponent;
begin
  if FVideoEncoder = Value then
    Exit;
  Link := FVideoEncoder;
  FFSetLinkedComponent(Self, Link, Value);
  FVideoEncoder := TFFEncoder(Link);
end;

procedure TFFWriter.SetAudioEncoder(const Value: TFFEncoder);
var
  Link: TComponent;
begin
  if FAudioEncoder = Value then
    Exit;
  Link := FAudioEncoder;
  FFSetLinkedComponent(Self, Link, Value);
  FAudioEncoder := TFFEncoder(Link);
end;

procedure TFFWriter.SetupFromLinks;
var
  I: Integer;
begin
  if not FActive then
    Open;

  if FRemuxReader <> nil then
  begin
    if not FRemuxReader.Active then
      FRemuxReader.Open;
    for I := 0 to FRemuxReader.StreamCount - 1 do
      AddStreamCopy(FRemuxReader, I);
    if not FRemuxReader.AutoPump then
      FRemuxReader.AutoPump := True;
  end;

  if (FVideoEncoder <> nil) and FVideoEncoder.Initialized then
    AddStream(FVideoEncoder);
  if (FAudioEncoder <> nil) and FAudioEncoder.Initialized then
    AddStream(FAudioEncoder);

  if (FFormatCtx <> nil) and (FFormatCtx^.nb_streams > 0) and not FHeaderWritten then
    WriteHeader;
end;

procedure TFFWriter.TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
begin
  if (csDestroying in ComponentState) or (csDesigning in ComponentState) then
    Exit;
  if (APkt = nil) or (FRemuxReader = nil) or not FActive or not FHeaderWritten then
    Exit;
  WritePacketFromReader(APkt, FRemuxReader);
end;

procedure TFFWriter.Close;
var
  Pb: PAVIOContext;
begin
  if FFormatCtx = nil then
    Exit;
  if FFIsDesignTime(Self) then
  begin
    FFormatCtx := nil;
    SetLength(FStreamMaps, 0);
    FActive := False;
    FHeaderWritten := False;
    Exit;
  end;
  if FHeaderWritten then
  begin
    av_write_trailer(FFormatCtx);
    FHeaderWritten := False;
  end;
  if (FOutputAdapter = nil) and (FFormatCtx^.pb <> nil) and ((FFormatCtx^.oformat.flags and AVFMT_NOFILE) = 0) then
  begin
    Pb := FFormatCtx^.pb;
    avio_closep(Pb);
    FFormatCtx^.pb := Pb;
  end;
  avformat_free_context(FFormatCtx);
  FFormatCtx := nil;
  SetLength(FStreamMaps, 0);
  FActive := False;
end;

function TFFWriter.HasOutputTarget: Boolean;
begin
  Result := (FFileName <> '') or (FOutputAdapter <> nil);
end;

function TFFWriter.FindOutIndex(AInIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FStreamMaps) do
    if FStreamMaps[I].InIndex = AInIndex then
      Exit(FStreamMaps[I].OutIndex);
  Result := -1;
end;

function TFFWriter.GetOutStream(AOutIndex: Integer): PAVStream;
begin
  if (FFormatCtx = nil) or (AOutIndex < 0) or (AOutIndex >= Integer(FFormatCtx^.nb_streams)) then
    raise EFFException.CreateFmt('Invalid output stream index %d', [AOutIndex]);
  Result := PAVStream(FFormatCtx^.streams[AOutIndex]);
end;

procedure TFFWriter.Open;
var
  Path: UTF8String;
  FmtName: PAnsiChar;
  Ret: Integer;
begin
  if (FOutputAdapter = nil) and (FFileName = '') then
    raise EFFException.Create('TFFWriter.FileName is empty');
  if (FOutputAdapter <> nil) and (FFormatName = '') then
    raise EFFException.Create('TFFWriter.FormatName must be set when OutputAdapter is used');
  if FFormatCtx <> nil then
    Close;

  if FFormatName <> '' then
    FmtName := PAnsiChar(UTF8String(FFormatName))
  else
    FmtName := nil;

  if FOutputAdapter <> nil then
    Path := UTF8String('memory:')
  else
    Path := FFmpegUtf8Path(FFileName);

  Ret := avformat_alloc_output_context2(FFormatCtx, nil, FmtName, PAnsiChar(Path));
  if (Ret < 0) or (FFormatCtx = nil) then
    raise EFFException.CreateFmt('avformat_alloc_output_context2 failed (%d)', [Ret]);

  if FOutputAdapter <> nil then
  begin
    FOutputAdapter.EnsureAttached;
    FFormatCtx^.pb := FOutputAdapter.IOContext;
    FFormatCtx^.flags := FFormatCtx^.flags or AVFMT_FLAG_CUSTOM_IO;
  end
  else if (FFormatCtx^.oformat.flags and AVFMT_NOFILE) = 0 then
  begin
    Ret := avio_open(FFormatCtx^.pb, PAnsiChar(Path), AVIO_FLAG_WRITE);
    if Ret < 0 then
    begin
      Close;
      raise EFFException.CreateFmt('avio_open failed (%d) for %s', [Ret, FFileName]);
    end;
  end;

  FActive := True;
  FHeaderWritten := False;
  if FAutoSetup then
    SetupFromLinks;
end;

function TFFWriter.AddStream(AEncoder: TFFEncoder): Integer;
var
  St: PAVStream;
  Ret: Integer;
  Map: TFFStreamMapItem;
begin
  if not FActive then
    raise EFFException.Create('TFFWriter is not active');
  if (AEncoder = nil) or not AEncoder.Initialized then
    raise EFFException.Create('TFFWriter.AddStream: encoder is not initialized');

  St := avformat_new_stream(FFormatCtx, nil);
  if St = nil then
    raise EFFException.Create('avformat_new_stream failed');

  Ret := avcodec_parameters_from_context(St^.codecpar, AEncoder.CodecContext);
  if Ret < 0 then
    raise EFFException.CreateFmt('avcodec_parameters_from_context failed (%d)', [Ret]);

  St^.codecpar^.codec_tag.tag := 0;
  St^.time_base := AEncoder.CodecContext^.time_base;

  if (FFormatCtx^.oformat.flags and AVFMT_GLOBALHEADER) <> 0 then
    AEncoder.CodecContext^.flags := AEncoder.CodecContext^.flags or AV_CODEC_FLAG_GLOBAL_HEADER;

  Result := St^.index;
  Map.InIndex := -1;
  Map.OutIndex := Result;
  SetLength(FStreamMaps, Length(FStreamMaps) + 1);
  FStreamMaps[High(FStreamMaps)] := Map;
end;

function TFFWriter.AddStreamCopy(AReader: TFFReader; AStreamIndex: Integer): Integer;
var
  InStream: PAVStream;
  OutStream: PAVStream;
  Ret: Integer;
  Map: TFFStreamMapItem;
begin
  if not FActive then
    raise EFFException.Create('TFFWriter is not active');
  if (AReader = nil) or not AReader.Active then
    raise EFFException.Create('TFFWriter.AddStreamCopy: reader is not active');

  InStream := AReader.GetStream(AStreamIndex);
  OutStream := avformat_new_stream(FFormatCtx, nil);
  if OutStream = nil then
    raise EFFException.Create('avformat_new_stream failed');

  Ret := avcodec_parameters_copy(OutStream^.codecpar, InStream^.codecpar);
  if Ret < 0 then
    raise EFFException.CreateFmt('avcodec_parameters_copy failed (%d)', [Ret]);

  OutStream^.codecpar^.codec_tag.tag := 0;
  Result := OutStream^.index;

  Map.InIndex := AStreamIndex;
  Map.OutIndex := Result;
  SetLength(FStreamMaps, Length(FStreamMaps) + 1);
  FStreamMaps[High(FStreamMaps)] := Map;
end;

procedure TFFWriter.WriteHeader;
var
  Ret: Integer;
begin
  if not FActive then
    raise EFFException.Create('TFFWriter is not active');
  if FHeaderWritten then
    Exit;
  if FFormatCtx^.nb_streams = 0 then
    raise EFFException.Create('TFFWriter.WriteHeader: no streams added');

  Ret := avformat_write_header(FFormatCtx, nil);
  if Ret < 0 then
    raise EFFException.CreateFmt('avformat_write_header failed (%d)', [Ret]);
  FHeaderWritten := True;
end;

function TFFWriter.WritePacket(APacket: TFFPacket; AStreamIndex: Integer): Integer;
begin
  if not FActive then
    raise EFFException.Create('TFFWriter is not active');
  if not FHeaderWritten then
    raise EFFException.Create('TFFWriter.WriteHeader must be called first');
  if (APacket = nil) or (APacket.Raw = nil) then
    raise EFFException.Create('WritePacket: packet is nil');

  APacket.Raw^.stream_index := AStreamIndex;
  APacket.Raw^.pos := -1;
  Result := av_interleaved_write_frame(FFormatCtx, APacket.Raw);
end;

function TFFWriter.WritePacketFromReader(APacket: TFFPacket; AReader: TFFReader): Integer;
var
  InIndex: Integer;
  OutIndex: Integer;
  InStream: PAVStream;
  OutStream: PAVStream;
  Pkt: PAVPacket;
begin
  if (APacket = nil) or (APacket.Raw = nil) then
    raise EFFException.Create('WritePacketFromReader: packet is nil');

  InIndex := APacket.Raw^.stream_index;
  OutIndex := FindOutIndex(InIndex);
  if OutIndex < 0 then
    Exit(0);

  InStream := AReader.GetStream(InIndex);
  OutStream := GetOutStream(OutIndex);
  Pkt := APacket.Raw;

  Pkt^.stream_index := OutIndex;
  Pkt^.pts := av_rescale_q_rnd(Pkt^.pts, InStream^.time_base, OutStream^.time_base,
    Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
  Pkt^.dts := av_rescale_q_rnd(Pkt^.dts, InStream^.time_base, OutStream^.time_base,
    Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
  Pkt^.duration := av_rescale_q(Pkt^.duration, InStream^.time_base, OutStream^.time_base);
  Pkt^.pos := -1;

  Result := av_interleaved_write_frame(FFormatCtx, Pkt);
end;

procedure TFFWriter.WriteTrailer;
var
  Ret: Integer;
begin
  if not FActive then
    raise EFFException.Create('TFFWriter is not active');
  if not FHeaderWritten then
    Exit;
  Ret := av_write_trailer(FFormatCtx);
  if Ret < 0 then
    raise EFFException.CreateFmt('av_write_trailer failed (%d)', [Ret]);
  FHeaderWritten := False;
end;

end.
