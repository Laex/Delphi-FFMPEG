program linked_player_smoke_test;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFDecoder,
  uFFPlaybackEngine,
  uFFVideoPlayer,
  uFFMediaInfo;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 4] of string = (
    '..\..\resource\test_av.mkv',
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
  if FileExists(Candidates[4]) then
    Exit(Candidates[4]);
  Result := '';
end;

function FindStreamIndex(const AFileName: string; AType: AVMediaType): Integer;
var
  Info: TFFMediaInfo;
begin
  Result := -1;
  Info := TFFMediaInfo.Create(nil);
  try
    Info.FileName := AFileName;
    Info.Probe;
    Result := Info.FindBestStream(AType);
  finally
    Info.Free;
  end;
end;

var
  Form: TForm;
  Reader: TFFReader;
  VideoDecoder: TFFDecoder;
  AudioDecoder: TFFDecoder;
  Player: TFFVideoPlayer;
  Media: string;
  VideoIdx: Integer;
  AudioIdx: Integer;
  I: Integer;
  HasAudio: Boolean;
begin
  Media := '';
  if ParamCount >= 1 then
    Media := ParamStr(1)
  else
    Media := DefaultMediaFile;

  if Media = '' then
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;

  VideoIdx := FindStreamIndex(Media, AVMEDIA_TYPE_VIDEO);
  if VideoIdx < 0 then
    Fail('no video stream in ' + Media);

  AudioIdx := FindStreamIndex(Media, AVMEDIA_TYPE_AUDIO);
  HasAudio := AudioIdx >= 0;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  Form := TForm.Create(nil);
  Reader := TFFReader.Create(Form);
  VideoDecoder := TFFDecoder.Create(Form);
  AudioDecoder := TFFDecoder.Create(Form);
  Player := TFFVideoPlayer.Create(Form);
  try
    Form.Width := 640;
    Form.Height := 480;

    Reader.FileName := Media;
    Reader.Open;

    VideoDecoder.Reader := Reader;
    VideoDecoder.StreamIndex := VideoIdx;

    Player.Parent := Form;
    Player.Align := alClient;
    Player.VideoDecoder := VideoDecoder;
    if HasAudio then
    begin
      AudioDecoder.Reader := Reader;
      AudioDecoder.StreamIndex := AudioIdx;
      Player.AudioDecoder := AudioDecoder;
    end;

    Form.Show;

    Player.Play;
    for I := 1 to 40 do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;

    if Player.State <> psPlaying then
      Fail('linked player is not in psPlaying state');

    if Player.Duration <= 0 then
      Fail('duration should be > 0');

    if Player.Position <= 0 then
    begin
      for I := 1 to 20 do
      begin
        Application.ProcessMessages;
        Sleep(100);
        if Player.Position > 0 then
          Break;
      end;
    end;

    if Player.Position <= 0 then
      Fail('position should advance while playing');

    Player.Pause;
    if Player.State <> psPaused then
      Fail('pause failed');

    Player.Play;
    Sleep(200);
    Application.ProcessMessages;

    Player.Stop;
    if Player.State <> psStopped then
      Fail('stop failed');

    WriteLn(Format('Linked playback: duration=%d ms, position=%d ms, audio=%s',
      [Player.Duration, Player.Position, BoolToStr(HasAudio, True)]));
    WriteLn('PASS: linked TFFVideoPlayer smoke test OK');
  finally
    Player.Free;
    AudioDecoder.Free;
    VideoDecoder.Free;
    Reader.Free;
    Form.Free;
  end;
end.
