program mediainfo_test;

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
  uFFMediaInfo;

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

procedure TestMediaInfo(const MediaFile: string);
var
  Info: TFFMediaInfo;
  VideoIdx, AudioIdx: Integer;
begin
  Info := TFFMediaInfo.Create(nil);
  try
    Info.FileName := MediaFile;
    Info.Probe;
    if not Info.Probed then
      Fail('Probe did not set Probed');
    if Info.StreamCount <= 0 then
      Fail('no streams');
    if Info.FormatName = '' then
      Fail('format name empty');

    VideoIdx := Info.FindBestStream(AVMEDIA_TYPE_VIDEO);
    if VideoIdx < 0 then
      Fail('no video stream');
    AudioIdx := Info.FindBestStream(AVMEDIA_TYPE_AUDIO);

    WriteLn(Info.SummaryText);
    WriteLn(Format('Video stream=%d audio stream=%d duration=%s',
      [VideoIdx, AudioIdx, Info.FormatDurationMs]));
  finally
    Info.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG TFFMediaInfo test');
  if ParamCount >= 1 then
    TestMediaInfo(ParamStr(1))
  else if DefaultMediaFile <> '' then
    TestMediaInfo(DefaultMediaFile)
  else
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;
  WriteLn('PASS: TFFMediaInfo OK');
end.
