program scaler_test;

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
  {$IFDEF MSWINDOWS}
  Vcl.Graphics,
  {$ENDIF}
  ffmpeg_types,
  libavutil,
  libavcodec,
  libavformat,
  uFFReader,
  uFFDecoder,
  uFFPacket,
  uFFFrame,
  uFFFrameConverter
  {$IFDEF MSWINDOWS}
  , uFFFrameBitmap
  {$ENDIF}
  ;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 2] of string = (
    '..\..\resource\768x576.avi',
    '..\..\..\resource\768x576.avi',
    'D:\Work\Delphi\Delphi-FFMPEG\resource\768x576.avi'
  );
var
  Base: string;
  I: Integer;
begin
  Base := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Base + Candidates[I]) then
      Exit(Base + Candidates[I]);
  if FileExists(Candidates[2]) then
    Exit(Candidates[2]);
  Result := '';
end;

function FindVideoStreamIndex(const Reader: TFFReader): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Reader.Streams.Count - 1 do
    if Reader.Streams.GetInfo(I).MediaType = AVMEDIA_TYPE_VIDEO then
      Exit(I);
end;

function DecodeFirstVideoFrame(const Reader: TFFReader; AStreamIndex: Integer; AFrame: TFFFrame): Boolean;
var
  Decoder: TFFDecoder;
  Packet: TFFPacket;
  Ret: Integer;
  N: Integer;
begin
  Result := False;
  Decoder := TFFDecoder.Create(nil);
  Packet := TFFPacket.Create;
  try
    Decoder.Reader := Reader;
    Decoder.StreamIndex := AStreamIndex;
    Decoder.Initialize;

    N := 0;
    while Reader.ReadPacket(Packet) and (N < 2000) do
    begin
      Inc(N);
      if Packet.Raw^.stream_index <> AStreamIndex then
        Continue;
      Ret := Decoder.SendPacket(Packet);
      if Ret < 0 then
        Continue;
      Ret := Decoder.ReceiveFrame(AFrame);
      if Ret = 0 then
        Exit(True);
      if (Ret <> AVERROR_EAGAIN) and (Ret <> AVERROR_EOF) then
        Fail(Format('ReceiveFrame failed (%d)', [Ret]));
    end;
  finally
    Packet.Free;
    Decoder.Free;
  end;
end;

procedure TestScaler(const MediaFile: string);
var
  Reader: TFFReader;
  Frame: TFFFrame;
  Converter: TFFFrameConverter;
  Bgra: PAVFrame;
  StreamIdx: Integer;
  Info: TFFStreamInfo;
  {$IFDEF MSWINDOWS}
  Bmp: TBitmap;
  {$ENDIF}
begin
  Reader := TFFReader.Create(nil);
  Frame := TFFFrame.Create;
  Converter := TFFFrameConverter.Create;
  try
    Reader.FileName := MediaFile;
    Reader.Open;

    StreamIdx := FindVideoStreamIndex(Reader);
    if StreamIdx < 0 then
      Fail('no video stream');

    Info := Reader.Streams.GetInfo(StreamIdx);
    if not DecodeFirstVideoFrame(Reader, StreamIdx, Frame) then
      Fail('could not decode video frame');

    Bgra := Converter.Convert(Frame);
    if Bgra = nil then
      Fail('converter returned nil');

    if (Converter.DstWidth <> Info.Width) or (Converter.DstHeight <> Info.Height) then
      Fail(Format('unexpected size %dx%d (expected %dx%d)',
        [Converter.DstWidth, Converter.DstHeight, Info.Width, Info.Height]));

    if Bgra^.linesize[0] < Info.Width * 4 then
      Fail('BGRA stride too small');

    WriteLn(Format('Converted %s %dx%d -> BGRA stride=%d',
      [Info.CodecName, Info.Width, Info.Height, Bgra^.linesize[0]]));

    {$IFDEF MSWINDOWS}
    Bmp := TBitmap.Create;
    try
      TFFFrameBitmap.AssignFromConverter(Converter, Bgra, Bmp);
      if (Bmp.Width <> Info.Width) or (Bmp.Height <> Info.Height) then
        Fail('bitmap size mismatch');
      if Bmp.PixelFormat <> pf32bit then
        Fail('bitmap pixel format is not pf32bit');
      WriteLn('VCL bitmap assign OK');
    finally
      Bmp.Free;
    end;
    {$ENDIF}
  finally
    Converter.Free;
    Frame.Free;
    Reader.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG scaler test');
  if ParamCount >= 1 then
    TestScaler(ParamStr(1))
  else if DefaultMediaFile <> '' then
    TestScaler(DefaultMediaFile)
  else
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;
  WriteLn('PASS: TFFFrameConverter OK');
end.
