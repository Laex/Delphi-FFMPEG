program reader_decoder_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ELSE}
  System.Classes,
  System.SysUtils,
  {$ENDIF}
  ffmpeg_types,
  libavutil,
  libavcodec,
  libavformat,
  uFFReader,
  uFFDecoder,
  uFFPacket,
  uFFFrame;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
var
  Candidates: array of string;
  I: Integer;
begin
  SetLength(Candidates, 3);
  Candidates[0] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\resource\768x576.avi';
  Candidates[1] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\..\resource\768x576.avi';
  Candidates[2] := 'D:\Work\Delphi\Delphi-FFMPEG\resource\768x576.avi';
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Candidates[I]) then
      Exit(Candidates[I]);
  Result := '';
end;

function FindDecodeStreamIndex(const Reader: TFFReader): Integer;
var
  I: Integer;
  Info: TFFStreamInfo;
begin
  Result := -1;
  for I := 0 to Reader.Streams.Count - 1 do
  begin
    Info := Reader.Streams.GetInfo(I);
    if Info.MediaType in [AVMEDIA_TYPE_VIDEO, AVMEDIA_TYPE_AUDIO] then
      Exit(I);
  end;
end;

procedure TestReaderDecoder(const MediaFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Packet: TFFPacket;
  Frame: TFFFrame;
  StreamIdx: Integer;
  Ret: Integer;
  PacketsRead: Integer;
  FrameDecoded: Boolean;
  Info: TFFStreamInfo;
begin
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Packet := TFFPacket.Create;
  Frame := TFFFrame.Create;
  try
    Reader.FileName := MediaFile;
    Reader.Open;
    if Reader.StreamCount = 0 then
      Fail('no streams in media file');

    WriteLn('Streams: ', Reader.StreamCount, ', duration (us): ', Reader.Duration);
    StreamIdx := FindDecodeStreamIndex(Reader);
    if StreamIdx < 0 then
      Fail('no audio/video stream found');

    Info := Reader.Streams.GetInfo(StreamIdx);
    WriteLn(Format('Decode stream %d: %s %dx%d', [StreamIdx, Info.CodecName, Info.Width, Info.Height]));

    Decoder.Reader := Reader;
    Decoder.StreamIndex := StreamIdx;
    Decoder.Initialize;
    if Decoder.CodecName = '' then
      Fail('decoder codec name is empty');

    FrameDecoded := False;
    PacketsRead := 0;
    while Reader.ReadPacket(Packet) and (PacketsRead < 2000) do
    begin
      Inc(PacketsRead);
      if Packet.Raw^.stream_index <> StreamIdx then
        Continue;

      Ret := Decoder.SendPacket(Packet);
      if Ret < 0 then
        Continue;

      while True do
      begin
        Ret := Decoder.ReceiveFrame(Frame);
        if Ret = 0 then
        begin
          FrameDecoded := True;
          Break;
        end;
        if Ret = AVERROR_EAGAIN then
          Break;
        if Ret = AVERROR_EOF then
          Break;
        Fail(Format('avcodec_receive_frame failed (%d)', [Ret]));
      end;

      if FrameDecoded then
        Break;
    end;

    if not FrameDecoded then
      Fail('no frame decoded from media file');

    WriteLn(Format('Decoded frame: samples=%d pict_type=%d', [Frame.GetSampleCount, Ord(Frame.GetPictureType)]));
  finally
    Frame.Free;
    Packet.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG reader/decoder test');
  if ParamCount >= 1 then
    TestReaderDecoder(ParamStr(1))
  else if DefaultMediaFile <> '' then
    TestReaderDecoder(DefaultMediaFile)
  else
  begin
    WriteLn('SKIP: no media file (pass path to .avi/.mkv or place resource/768x576.avi)');
    Halt(2);
  end;
  WriteLn('PASS: TFFReader and TFFDecoder OK');
end.
