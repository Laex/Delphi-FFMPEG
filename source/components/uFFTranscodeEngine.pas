unit uFFTranscodeEngine;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Video transcode pipeline: Reader + InputDecoder -> Encoder -> OutputWriter. }

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
  libavcodec,
  libavformat,
  libavutil,
  uFFException,
  uFFReader,
  uFFDecoder,
  uFFEncoder,
  uFFWriter,
  uFFFrame,
  uFFPacket,
  uFFHooks,
  uFFFrameFilter;

type
  TFFTranscodeProgressEvent = procedure(Sender: TObject; APositionMs, ADurationMs: Int64) of object;

  TFFTranscodeEngine = class
  private
    FReader: TFFReader;
    FInputDecoder: TFFDecoder;
    FEncoder: TFFEncoder;
    FWriter: TFFWriter;
    FStopRequested: Boolean;
    FPaused: Boolean;
    FCopyAudio: Boolean;
    FAudioStreamIndex: Integer;
    FAudioOutIndex: Integer;
    FTranscodeAudio: Boolean;
    FAudioCodecName: string;
    FAudioBitRate: Int64;
    FAudioSampleRate: Integer;
    FAudioChannels: Integer;
    FAudioSampleFormat: AVSampleFormat;
    FAudioOptions: TStrings;
    FOutputStreamIndex: Integer;
    FProgressLock: TCriticalSection;
    FLastProgressMs: Int64;
    FStartMs: Int64;
    FEndMs: Int64;
    FFrameFilter: TFFFrameFilter;
    FOnProgress: TFFTranscodeProgressEvent;
    FOnPreviewFrame: TFFPreviewFrameEvent;
    procedure NotifyProgress(APositionMs, ADurationMs: Int64);
    procedure WaitIfPaused;
    procedure ConfigureEncoder;
    procedure DrainEncoder(APacket: TFFPacket);
    function FindFirstAudioStreamIndex: Integer;
    function PacketPtsToMs(APacket: TFFPacket; AStreamIndex: Integer): Int64;
    function MsToStreamTimestamp(AStreamIndex, APositionMs: Int64): Int64;
    function PrepareVideoFrame(AFrame: TFFFrame; AStreamIndex: Integer): TFFFrame;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
    procedure RequestStop;
    procedure SetPaused(const Value: Boolean);

    property Reader: TFFReader read FReader write FReader;
    property InputDecoder: TFFDecoder read FInputDecoder write FInputDecoder;
    property Encoder: TFFEncoder read FEncoder write FEncoder;
    property Writer: TFFWriter read FWriter write FWriter;
    property OnProgress: TFFTranscodeProgressEvent read FOnProgress write FOnProgress;
    property OnPreviewFrame: TFFPreviewFrameEvent read FOnPreviewFrame write FOnPreviewFrame;
    property StartMs: Int64 read FStartMs write FStartMs;
    property EndMs: Int64 read FEndMs write FEndMs;
    property FrameFilter: TFFFrameFilter read FFrameFilter write FFrameFilter;
    property CopyAudio: Boolean read FCopyAudio write FCopyAudio;
    property AudioStreamIndex: Integer read FAudioStreamIndex write FAudioStreamIndex;
    property TranscodeAudio: Boolean read FTranscodeAudio write FTranscodeAudio;
    property AudioCodecName: string read FAudioCodecName write FAudioCodecName;
    property AudioBitRate: Int64 read FAudioBitRate write FAudioBitRate;
    property AudioSampleRate: Integer read FAudioSampleRate write FAudioSampleRate;
    property AudioChannels: Integer read FAudioChannels write FAudioChannels;
    property AudioSampleFormat: AVSampleFormat read FAudioSampleFormat write FAudioSampleFormat;
    property AudioOptions: TStrings read FAudioOptions;
  end;

implementation

constructor TFFTranscodeEngine.Create;
begin
  inherited Create;
  FProgressLock := TCriticalSection.Create;
  FLastProgressMs := -1;
  FStartMs := 0;
  FEndMs := 0;
  FOutputStreamIndex := -1;
  FAudioOutIndex := -1;
  FAudioStreamIndex := -1;
  FCopyAudio := False;
  FTranscodeAudio := False;
  FAudioCodecName := '';
  FAudioBitRate := 128000;
  FAudioSampleRate := 44100;
  FAudioChannels := 2;
  FAudioSampleFormat := AV_SAMPLE_FMT_FLTP;
  FAudioOptions := TStringList.Create;
end;

destructor TFFTranscodeEngine.Destroy;
begin
  FAudioOptions.Free;
  FProgressLock.Free;
  inherited;
end;

procedure TFFTranscodeEngine.RequestStop;
begin
  FStopRequested := True;
  FPaused := False;
end;

procedure TFFTranscodeEngine.SetPaused(const Value: Boolean);
begin
  FPaused := Value;
end;

procedure TFFTranscodeEngine.WaitIfPaused;
begin
  while FPaused and not FStopRequested do
    Sleep(10);
end;

procedure TFFTranscodeEngine.NotifyProgress(APositionMs, ADurationMs: Int64);
begin
  FProgressLock.Enter;
  try
    if (APositionMs >= 0) and (Abs(APositionMs - FLastProgressMs) < 200) then
      Exit;
    FLastProgressMs := APositionMs;
  finally
    FProgressLock.Leave;
  end;
  if Assigned(FOnProgress) then
    FOnProgress(Self, APositionMs, ADurationMs);
end;

function TFFTranscodeEngine.PacketPtsToMs(APacket: TFFPacket; AStreamIndex: Integer): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
  Ts: Int64;
begin
  Result := -1;
  if (APacket = nil) or (APacket.Raw = nil) or (FReader = nil) then
    Exit;
  Ts := APacket.Raw^.pts;
  if Ts = AV_NOPTS_VALUE then
    Ts := APacket.Raw^.dts;
  if Ts = AV_NOPTS_VALUE then
    Exit;
  St := FReader.GetStream(AStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(Ts, St^.time_base, MsBase);
end;

function TFFTranscodeEngine.FindFirstAudioStreamIndex: Integer;
var
  I: Integer;
  Info: TFFStreamInfo;
begin
  Result := -1;
  if FReader = nil then
    Exit;
  for I := 0 to FReader.Streams.Count - 1 do
  begin
    Info := FReader.Streams.GetInfo(I);
    if Info.MediaType = AVMEDIA_TYPE_AUDIO then
      Exit(I);
  end;
end;

function TFFTranscodeEngine.MsToStreamTimestamp(AStreamIndex, APositionMs: Int64): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
begin
  St := FReader.GetStream(AStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(APositionMs, MsBase, St^.time_base);
end;

function TFFTranscodeEngine.PrepareVideoFrame(AFrame: TFFFrame; AStreamIndex: Integer): TFFFrame;
begin
  if FFrameFilter <> nil then
    Result := FFrameFilter.ApplyFrame(AFrame, AStreamIndex)
  else
    Result := AFrame;
end;

procedure TFFTranscodeEngine.ConfigureEncoder;
var
  Details: TFFDecoderDetails;
begin
  if (FEncoder = nil) or (FInputDecoder = nil) then
    Exit;
  Details := FInputDecoder.GetStreamDetails;
  if Details.MediaType = AVMEDIA_TYPE_VIDEO then
  begin
    FEncoder.MediaType := AVMEDIA_TYPE_VIDEO;
    if FEncoder.Width <= 0 then
      FEncoder.Width := Details.Width;
    if FEncoder.Height <= 0 then
      FEncoder.Height := Details.Height;
    if (FEncoder.FrameRateNum <= 0) and (Details.FrameRateNum > 0) then
    begin
      FEncoder.FrameRateNum := Details.FrameRateNum;
      FEncoder.FrameRateDen := Details.FrameRateDen;
    end;
  end;
end;

procedure TFFTranscodeEngine.DrainEncoder(APacket: TFFPacket);
var
  Ret: Integer;
begin
  while not FStopRequested do
  begin
    Ret := FEncoder.ReceivePacket(APacket);
    if Ret = 0 then
    begin
      if FOutputStreamIndex >= 0 then
        FWriter.WritePacket(APacket, FOutputStreamIndex);
    end
    else if Ret = AVERROR_EAGAIN then
      Break
    else
      Break;
  end;
end;

procedure TFFTranscodeEngine.Execute;
var
  Packet: TFFPacket;
  Frame: TFFFrame;
  OutPacket: TFFPacket;
  AudioDecoder: TFFDecoder;
  AudioEncoder: TFFEncoder;
  AudioFrame: TFFFrame;
  AudioOutPacket: TFFPacket;
  EncodeFrame: TFFFrame;
  Ret: Integer;
  WasAutoPump: Boolean;
  StreamIdx: Integer;
  AudioIdx: Integer;
  DurationMs: Int64;
  PositionMs: Int64;
  InVideoStream: PAVStream;
  InAudioStream: PAVStream;

  procedure RescaleFramePts(AFrame: TFFFrame; InStream: PAVStream; EncCtx: PAVCodecContext);
  begin
    if (AFrame = nil) or (AFrame.Raw = nil) or (InStream = nil) or (EncCtx = nil) then
      Exit;
    if AFrame.Raw^.pts = AV_NOPTS_VALUE then
      Exit;
    AFrame.Raw^.pts := av_rescale_q(AFrame.Raw^.pts, InStream^.time_base, EncCtx^.time_base);
  end;

  procedure DrainAnyEncoder(AEnc: TFFEncoder; AOutPkt: TFFPacket; AOutStreamIndex: Integer);
  begin
    while not FStopRequested do
    begin
      Ret := AEnc.ReceivePacket(AOutPkt);
      if Ret = 0 then
      begin
        if AOutStreamIndex >= 0 then
          FWriter.WritePacket(AOutPkt, AOutStreamIndex);
      end
      else if Ret = AVERROR_EAGAIN then
        Break
      else
        Break;
    end;
  end;
begin
  if (FReader = nil) or (FInputDecoder = nil) or (FWriter = nil) then
    raise EFFException.Create('TFFTranscodeEngine: Reader, InputDecoder, Encoder and Writer are required');
  if not FWriter.HasOutputTarget then
    raise EFFException.Create('TFFTranscodeEngine: Writer needs FileName or OutputAdapter');

  FStopRequested := False;
  FPaused := False;
  FLastProgressMs := -1;

  if not FReader.Active then
    FReader.Open;
  if FInputDecoder.Reader = nil then
    FInputDecoder.Reader := FReader;
  if FInputDecoder.StreamIndex < 0 then
    raise EFFException.Create('TFFTranscodeEngine: InputDecoder.StreamIndex is not set');

  ConfigureEncoder;
  if not FInputDecoder.Initialized then
    FInputDecoder.Initialize;
  if not FEncoder.Initialized then
    FEncoder.Initialize;

  WasAutoPump := FReader.AutoPump;
  if WasAutoPump then
    FReader.AutoPump := False;
  try
    if not FWriter.Active then
      FWriter.Open;
    FOutputStreamIndex := FWriter.AddStream(FEncoder);

    AudioIdx := -1;
    if FTranscodeAudio or FCopyAudio then
    begin
      if FAudioStreamIndex >= 0 then
        AudioIdx := FAudioStreamIndex
      else
        AudioIdx := FindFirstAudioStreamIndex;

      if (AudioIdx >= 0) and (AudioIdx <> FInputDecoder.StreamIndex) then
      begin
        if FTranscodeAudio then
          FAudioOutIndex := -2
        else
          FAudioOutIndex := FWriter.AddStreamCopy(FReader, AudioIdx);
      end
      else
        FAudioOutIndex := -1;
    end;

    FWriter.WriteHeader;

    StreamIdx := FInputDecoder.StreamIndex;
    DurationMs := FInputDecoder.DurationMs;
    if (FEndMs > FStartMs) then
      DurationMs := FEndMs - FStartMs
    else if FStartMs > 0 then
      DurationMs := Max(0, DurationMs - FStartMs);

    NotifyProgress(0, DurationMs);

    InVideoStream := FReader.GetStream(StreamIdx);
    if AudioIdx >= 0 then
      InAudioStream := FReader.GetStream(AudioIdx)
    else
      InAudioStream := nil;

    AudioDecoder := nil;
    AudioEncoder := nil;
    AudioFrame := nil;
    AudioOutPacket := nil;
    if (FAudioOutIndex = -2) and (AudioIdx >= 0) then
    begin
      AudioDecoder := TFFDecoder.Create(nil);
      AudioEncoder := TFFEncoder.Create(nil);
      AudioFrame := TFFFrame.Create;
      AudioOutPacket := TFFPacket.Create;

      AudioDecoder.Reader := FReader;
      AudioDecoder.StreamIndex := AudioIdx;
      AudioDecoder.Initialize;

      AudioEncoder.MediaType := AVMEDIA_TYPE_AUDIO;
      if FAudioCodecName <> '' then
        AudioEncoder.CodecName := FAudioCodecName
      else
        AudioEncoder.CodecId := AV_CODEC_ID_AAC;
      AudioEncoder.BitRate := FAudioBitRate;
      AudioEncoder.ApplySourceCodecPar(InAudioStream^.codecpar);
      AudioEncoder.SampleFormat := FAudioSampleFormat;
      AudioEncoder.Options.Assign(FAudioOptions);
      AudioEncoder.Initialize;

      FAudioOutIndex := FWriter.AddStream(AudioEncoder);
    end;

    if FStartMs > 0 then
      FReader.Seek(MsToStreamTimestamp(StreamIdx, FStartMs), StreamIdx);
    FInputDecoder.Flush;

    Packet := TFFPacket.Create;
    Frame := TFFFrame.Create;
    OutPacket := TFFPacket.Create;
    try
      while not FStopRequested and FReader.ReadPacket(Packet) do
      begin
        WaitIfPaused;
        if (FAudioOutIndex >= 0) and (Packet.Raw^.stream_index = AudioIdx) then
        begin
          if (AudioDecoder = nil) then
          begin
            Ret := FWriter.WritePacketFromReader(Packet, FReader);
            if Ret < 0 then
              raise EFFException.CreateFmt('WritePacketFromReader(audio) failed (%d)', [Ret]);
          end
          else
          begin
            Ret := AudioDecoder.SendPacket(Packet);
            if (Ret < 0) and (Ret <> AVERROR_EAGAIN) then
              raise EFFException.CreateFmt('AudioDecoder.SendPacket failed (%d)', [Ret]);
            while not FStopRequested do
            begin
              Ret := AudioDecoder.ReceiveFrame(AudioFrame);
              if Ret = 0 then
              begin
                RescaleFramePts(AudioFrame, InAudioStream, AudioEncoder.CodecContext);
                Ret := AudioEncoder.SendFrame(AudioFrame);
                if Ret < 0 then
                  raise EFFException.CreateFmt('AudioEncoder.SendFrame failed (%d)', [Ret]);
                DrainAnyEncoder(AudioEncoder, AudioOutPacket, FAudioOutIndex);
              end
              else if Ret = AVERROR_EAGAIN then
                Break
              else
                Break;
            end;
          end;
          Continue;
        end;

        if Packet.Raw^.stream_index <> StreamIdx then
          Continue;

        PositionMs := PacketPtsToMs(Packet, StreamIdx);
        if (PositionMs >= 0) and (FStartMs > 0) and (PositionMs < FStartMs) then
          Continue;
        if (FEndMs > 0) and (PositionMs >= 0) and (PositionMs >= FEndMs) then
        begin
          NotifyProgress(Max(0, FEndMs - FStartMs), DurationMs);
          FStopRequested := True;
          Break;
        end;
        if PositionMs >= 0 then
          NotifyProgress(Max(0, PositionMs - FStartMs), DurationMs);

        Ret := FInputDecoder.SendPacket(Packet);
        if Ret = AVERROR_EAGAIN then
          Continue;
        if (Ret < 0) and (Ret <> AVERROR_EOF) then
          raise EFFException.CreateFmt('InputDecoder.SendPacket failed (%d)', [Ret]);

        while not FStopRequested do
        begin
          Ret := FInputDecoder.ReceiveFrame(Frame);
          if Ret = 0 then
          begin
            EncodeFrame := PrepareVideoFrame(Frame, StreamIdx);
            if EncodeFrame = nil then
              Continue;
            if Assigned(FOnPreviewFrame) then
              FOnPreviewFrame(Self, EncodeFrame, Max(0, PositionMs - FStartMs));
            RescaleFramePts(EncodeFrame, InVideoStream, FEncoder.CodecContext);
            Ret := FEncoder.SendFrame(EncodeFrame);
            if Ret < 0 then
              raise EFFException.CreateFmt('SendFrame failed (%d)', [Ret]);
            DrainEncoder(OutPacket);
          end
          else if Ret = AVERROR_EAGAIN then
            Break
          else
            Break;
        end;
      end;

      FInputDecoder.SendPacket(nil);
      while not FStopRequested do
      begin
        Ret := FInputDecoder.ReceiveFrame(Frame);
        if Ret = 0 then
        begin
          EncodeFrame := PrepareVideoFrame(Frame, StreamIdx);
          if EncodeFrame <> nil then
          begin
            if Assigned(FOnPreviewFrame) then
              FOnPreviewFrame(Self, EncodeFrame, -1);
            RescaleFramePts(EncodeFrame, InVideoStream, FEncoder.CodecContext);
            FEncoder.SendFrame(EncodeFrame);
            DrainEncoder(OutPacket);
          end;
        end
        else
          Break;
      end;

      FEncoder.Flush;
      DrainEncoder(OutPacket);

      if AudioEncoder <> nil then
      begin
        AudioDecoder.SendPacket(nil);
        while not FStopRequested do
        begin
          Ret := AudioDecoder.ReceiveFrame(AudioFrame);
          if Ret = 0 then
          begin
            RescaleFramePts(AudioFrame, InAudioStream, AudioEncoder.CodecContext);
            AudioEncoder.SendFrame(AudioFrame);
            DrainAnyEncoder(AudioEncoder, AudioOutPacket, FAudioOutIndex);
          end
          else
            Break;
        end;
        AudioEncoder.Flush;
        DrainAnyEncoder(AudioEncoder, AudioOutPacket, FAudioOutIndex);
      end;

      if not FStopRequested then
        NotifyProgress(DurationMs, DurationMs);
      FWriter.WriteTrailer;
    finally
      OutPacket.Free;
      Frame.Free;
      Packet.Free;
      AudioOutPacket.Free;
      AudioFrame.Free;
      AudioEncoder.Free;
      AudioDecoder.Free;
    end;
  finally
    if FWriter.Active then
      FWriter.Close;
    if WasAutoPump then
      FReader.AutoPump := True;
  end;
end;

end.
