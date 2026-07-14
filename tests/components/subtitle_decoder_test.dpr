program subtitle_decoder_test;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFSubtitleDecoder,
  uFFMediaInfo;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 6] of string = (
    '..\..\resource\test_subs.mp4',
    '..\..\resource\test_subs.mkv',
    '..\..\resource\test_av.mp4',
    '..\..\resource\768x576.avi',
    '..\..\..\resource\768x576.avi',
    'D:\Work\Delphi\Delphi-FFMPEG\resource\test_subs.mp4',
    'D:\Work\Delphi\Delphi-FFMPEG\resource\test_subs.mkv'
  );
var
  Base: string;
  I: Integer;
begin
  Base := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Base + Candidates[I]) then
      Exit(Base + Candidates[I]);
  if FileExists(Candidates[6]) then
    Exit(Candidates[6]);
  Result := '';
end;

var
  Media: string;
  Reader: TFFReader;
  SubDec: TFFSubtitleDecoder;
  Info: TFFMediaInfo;
  SubIdx: Integer;
  Text: string;
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
    SubIdx := Info.FindBestStream(AVMEDIA_TYPE_SUBTITLE);
  finally
    Info.Free;
  end;

  if SubIdx < 0 then
  begin
    WriteLn('SKIP: no subtitle stream in ' + Media);
    Halt(2);
  end;

  Reader := TFFReader.Create(nil);
  SubDec := TFFSubtitleDecoder.Create(nil);
  try
    Reader.FileName := Media;
    Reader.Open;
    SubDec.Reader := Reader;
    SubDec.StreamIndex := SubIdx;
    SubDec.LoadAll;

    if SubDec.EventCount <= 0 then
      Fail('expected at least one subtitle event');

    Text := SubDec.GetTextAt(2000);
    if Text = '' then
      Fail('expected subtitle text around 2000 ms');

    WriteLn(Format('Subtitles: events=%d sample="%s"', [SubDec.EventCount, Copy(Text, 1, 40)]));
    WriteLn('PASS: TFFSubtitleDecoder smoke test OK');
  finally
    SubDec.Free;
    Reader.Free;
  end;
end.
