program player_control_smoke_test;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFDecoder,
  uFFPlaybackEngine,
  uFFPlayerControl,
  uFFMediaInfo;

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
  Result := '';
end;

var
  Form: TForm;
  Reader: TFFReader;
  VideoDecoder: TFFDecoder;
  Control: TFFPlayerControl;
  Media: string;
  VideoIdx: Integer;
  I: Integer;
begin
  Media := DefaultMediaFile;
  if ParamCount >= 1 then
    Media := ParamStr(1);
  if Media = '' then
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;

  VideoIdx := -1;
  with TFFMediaInfo.Create(nil) do
  try
    FileName := Media;
    Probe;
    VideoIdx := FindBestStream(AVMEDIA_TYPE_VIDEO);
  finally
    Free;
  end;
  if VideoIdx < 0 then
    Fail('no video stream');

  Application.Initialize;
  Form := TForm.Create(nil);
  Reader := TFFReader.Create(Form);
  VideoDecoder := TFFDecoder.Create(Form);
  Control := TFFPlayerControl.Create(Form);
  try
    Form.Width := 720;
    Form.Height := 480;

    Reader.FileName := Media;
    Reader.Open;
    VideoDecoder.Reader := Reader;
    VideoDecoder.StreamIndex := VideoIdx;

    Control.Parent := Form;
    Control.Align := alClient;
    Control.VideoDecoder := VideoDecoder;

    Form.Show;

    Control.Play;
    for I := 1 to 30 do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;

    if Control.State <> psPlaying then
      Fail('player control not playing');

    Control.Pause;
    if Control.State <> psPaused then
      Fail('pause failed');

    Control.Play;
    Sleep(200);
    Application.ProcessMessages;

    Control.Stop;
    if Control.State <> psStopped then
      Fail('stop failed');

    WriteLn('PASS: TFFPlayerControl smoke test OK');
  finally
    Control.Free;
    VideoDecoder.Free;
    Reader.Free;
    Form.Free;
  end;
end.
