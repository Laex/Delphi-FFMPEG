program binding_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$I ../source/ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavcodec,
  libavformat,
  uFFmpegPath;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

begin
  WriteLn('Delphi-FFMPEG binding test');
  WriteLn('Target FFmpeg: ', FFMPEG_VERSION);
  WriteLn;

  if avutil_version() = 0 then
    Fail('avutil_version returned 0');

  WriteLn('avutil_version: ', avutil_version());
  WriteLn('av_version_info: ', av_version_info());
  WriteLn('libavcodec config: ', avcodec_configuration());
  WriteLn('libavformat license: ', avformat_license());
  WriteLn('FFmpegUtf8Path: ', FFmpegUtf8Path('/tmp/test.avi'));

  if avcodec_version() = 0 then
    Fail('avcodec_version returned 0');

  WriteLn;
  WriteLn('PASS: bindings and helpers loaded; basic API calls succeeded.');
end.
