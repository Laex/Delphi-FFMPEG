program audio_resampler_test;

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
  uFFReader,
  uFFDecoder,
  uFFPacket,
  uFFFrame,
  uFFAudioResampler;

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
  SetLength(Candidates, 4);
  Candidates[0] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test_media\test_av.mp4';
  Candidates[1] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\resource\test_av.mp4';
  Candidates[2] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\..\resource\test_av.mp4';
  Candidates[3] := 'D:\Work\Delphi\Delphi-FFMPEG\resource\test_av.mp4';
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Candidates[I]) then
      Exit(Candidates[I]);
  Result := '';
end;

function FindAudioStreamIndex(const Reader: TFFReader): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Reader.Streams.Count - 1 do
    if Reader.Streams.GetInfo(I).MediaType = AVMEDIA_TYPE_AUDIO then
      Exit(I);
end;

procedure TestAudioResampler(const MediaFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Packet: TFFPacket;
  Frame: TFFFrame;
  Resampler: TFFAudioResampler;
  StreamIdx: Integer;
  Ret: Integer;
  Pcm: PByte;
  PcmBytes: Integer;
  TotalBytes: Integer;
  FramesDecoded: Integer;
begin
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Packet := TFFPacket.Create;
  Frame := TFFFrame.Create;
  Resampler := TFFAudioResampler.Create;
  try
    Reader.FileName := MediaFile;
    Reader.Open;

    StreamIdx := FindAudioStreamIndex(Reader);
    if StreamIdx < 0 then
    begin
      WriteLn('SKIP: no audio stream in ', MediaFile);
      Halt(2);
    end;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := StreamIdx;
    Decoder.Initialize;

    TotalBytes := 0;
    FramesDecoded := 0;
    while Reader.ReadPacket(Packet) do
    begin
      if Packet.Raw^.stream_index <> StreamIdx then
        Continue;

      Ret := Decoder.SendPacket(Packet);
      if Ret < 0 then
        Continue;

      while Decoder.ReceiveFrame(Frame) = 0 do
      begin
        if FramesDecoded = 0 then
          Resampler.Configure(@Frame.Raw^.ch_layout, Frame.Raw^.sample_rate, AVSampleFormat(Frame.Raw^.format));

        Resampler.Convert(Frame.Raw, Pcm, PcmBytes);
        Inc(TotalBytes, PcmBytes);
        Inc(FramesDecoded);
        if FramesDecoded >= 10 then
          Break;
      end;

      if FramesDecoded >= 10 then
        Break;
    end;

    if FramesDecoded = 0 then
      Fail('no audio frames decoded');
    if TotalBytes <= 0 then
      Fail('resampler produced no PCM data');

    WriteLn(Format('Decoded %d audio frames, %d PCM bytes at %d Hz', [FramesDecoded, TotalBytes, Resampler.OutSampleRate]));
    WriteLn('PASS: TFFAudioResampler test OK');
  finally
    Resampler.Free;
    Frame.Free;
    Packet.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

var
  Media: string;
begin
  if ParamCount >= 1 then
    Media := ParamStr(1)
  else
    Media := DefaultMediaFile;

  if Media = '' then
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;

  TestAudioResampler(Media);
end.
