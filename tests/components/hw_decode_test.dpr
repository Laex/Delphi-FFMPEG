program hw_decode_test;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFDecoder,
  uFFFrame,
  uFFMediaInfo,
  uFFHardwareDecode;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 3] of string = (
    '..\..\resource\test_av.mp4',
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
  if FileExists(Candidates[3]) then
    Exit(Candidates[3]);
  Result := '';
end;

var
  Media: string;
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Frame: TFFFrame;
  Info: TFFMediaInfo;
  VideoIdx: Integer;
  Ret: Integer;
begin
  Media := DefaultMediaFile;
  if ParamCount >= 1 then
    Media := ParamStr(1);
  if Media = '' then
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;

  Info := TFFMediaInfo.Create(nil);
  try
    Info.FileName := Media;
    Info.Probe;
    VideoIdx := Info.FindBestStream(AVMEDIA_TYPE_VIDEO);
  finally
    Info.Free;
  end;
  if VideoIdx < 0 then
    Fail('no video stream');

  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Frame := TFFFrame.Create;
  try
    Reader.FileName := Media;
    Reader.Open;
    Decoder.Reader := Reader;
    Decoder.StreamIndex := VideoIdx;
    Decoder.HardwareDevice := ffhdAuto;

    try
      Decoder.Initialize;
    except
      on E: Exception do
      begin
        WriteLn('SKIP: HW decode init failed: ', E.Message);
        Halt(2);
      end;
    end;

    if (Decoder.CodecContext = nil) or (Decoder.CodecContext^.hw_device_ctx = nil) then
    begin
      WriteLn('SKIP: hardware device context not created');
      Halt(2);
    end;

    Ret := Decoder.DecodeFrameAt(0, Frame);
    if Ret < 0 then
      Fail('DecodeFrameAt failed: ' + IntToStr(Ret));

    if Frame.Raw^.width <= 0 then
      Fail('decoded frame width is zero');

    WriteLn(Format('HW decode OK: %dx%d format=%d', [Frame.Raw^.width, Frame.Raw^.height, Frame.Raw^.format]));
    WriteLn('PASS: TFFDecoder hardware decode smoke test OK');
  finally
    Frame.Free;
    Decoder.Free;
    Reader.Free;
  end;
end.
