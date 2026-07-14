program decoder_preview_test;

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
  uFFReader,
  uFFDecoder,
  uFFFrame;

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

procedure TestDecoderPreview(const MediaFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Frame: TFFFrame;
  Details: TFFDecoderDetails;
  Ret: Integer;
  PreviewMs: Int64;
  {$IFDEF MSWINDOWS}
  Bmp: TBitmap;
  OutJpg: string;
  {$ENDIF}
begin
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Frame := TFFFrame.Create;
  try
    Reader.FileName := MediaFile;
    Reader.Open;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FindVideoStreamIndex(Reader);
    if Decoder.StreamIndex < 0 then
      Fail('no video stream');

    Details := Decoder.GetStreamDetails;
    if (Details.Width <= 0) or (Details.Height <= 0) then
      Fail('stream details missing width/height');
    if Details.CodecName = '' then
      Fail('stream details missing codec name');

    WriteLn(Format('Stream %d: %s %dx%d duration=%d ms fps=%d/%d',
      [Details.Index, Details.CodecName, Details.Width, Details.Height, Details.DurationMs,
      Details.FrameRateNum, Details.FrameRateDen]));

    PreviewMs := Details.DurationMs div 2;
    if PreviewMs < 0 then
      PreviewMs := 0;

    Ret := Decoder.DecodeFrameAt(PreviewMs, Frame);
    if Ret <> 0 then
      Fail(Format('DecodeFrameAt(%d) failed (%d)', [PreviewMs, Ret]));

    if Frame.GetSampleCount > 0 then
      Fail('expected video frame, got audio samples');

    WriteLn(Format('Preview frame at %d ms: pict_type=%d', [PreviewMs, Ord(Frame.GetPictureType)]));

    {$IFDEF MSWINDOWS}
    Bmp := TBitmap.Create;
    try
      FFDecoderPreviewToBitmap(Decoder, PreviewMs, Bmp);
      if (Bmp.Width <> Details.Width) or (Bmp.Height <> Details.Height) then
        Fail('preview bitmap size mismatch');
      OutJpg := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'decoder_preview.bmp';
      Bmp.SaveToFile(OutJpg);
      WriteLn('Saved preview bitmap: ', OutJpg);
    finally
      Bmp.Free;
    end;
    {$ENDIF}
  finally
    Frame.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG decoder preview test');
  if ParamCount >= 1 then
    TestDecoderPreview(ParamStr(1))
  else if DefaultMediaFile <> '' then
    TestDecoderPreview(DefaultMediaFile)
  else
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;
  WriteLn('PASS: TFFDecoder preview OK');
end.
