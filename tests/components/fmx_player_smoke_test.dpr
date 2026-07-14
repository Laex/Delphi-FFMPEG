program fmx_player_smoke_test;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  FMX.Forms,
  FMX.Controls,
  FMX.Types,
  uFFPlaybackEngine,
  uFFFMXVideoPlayer;

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

var
  Player: TFFFMXVideoPlayer;
  Media: string;
  I: Integer;
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

  Application.Initialize;
  Player := TFFFMXVideoPlayer.Create(nil);
  try
    Player.SetBounds(0, 0, 640, 480);
    Player.FileName := Media;

    Player.Play;
      for I := 1 to 50 do
      begin
        Application.ProcessMessages;
        Sleep(100);
      end;

      if Player.State <> psPlaying then
        Fail('player is not in psPlaying state');

      if Player.Duration <= 0 then
        Fail('duration should be > 0');

      Player.Volume := 0.5;
      if Abs(Player.Volume - 0.5) > 0.001 then
        Fail('volume property failed');

      Player.Pause;
      if Player.State <> psPaused then
        Fail('pause failed');

      Player.Play;
      Sleep(200);
      Application.ProcessMessages;

      Player.Stop;
      if Player.State <> psStopped then
        Fail('stop failed');

      WriteLn(Format('FMX played media: duration=%d ms, position=%d ms', [Player.Duration, Player.Position]));
      WriteLn('PASS: TFFFMXVideoPlayer smoke test OK');
  finally
    Player.Free;
  end;
end.
