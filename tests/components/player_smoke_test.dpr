program player_smoke_test;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  uFFPlaybackEngine,
  uFFVideoPlayer;

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
  Form: TForm;
  Player: TFFVideoPlayer;
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
  Application.MainFormOnTaskbar := True;

  Form := TForm.Create(nil);
  try
    Form.Width := 800;
    Form.Height := 600;
    Player := TFFVideoPlayer.Create(Form);
    try
      Player.Parent := Form;
      Player.Align := alClient;
      Player.FileName := Media;
      Form.Show;

      Player.Play;
      for I := 1 to 50 do
      begin
        Application.ProcessMessages;
        Sleep(100);
        if Player.Duration > 0 then
          Break;
      end;

      if Player.State <> psPlaying then
        Fail('player is not in psPlaying state');

      if Player.Duration <= 0 then
      begin
        for I := 1 to 30 do
        begin
          Application.ProcessMessages;
          Sleep(100);
          if Player.Duration > 0 then
            Break;
        end;
      end;

      if Player.Duration <= 0 then
        Fail('duration should be > 0');

      Player.Volume := 0.5;
      if Abs(Player.Volume - 0.5) > 0.001 then
        Fail('volume property failed');

      for I := 1 to 50 do
      begin
        Application.ProcessMessages;
        Sleep(100);
        if Player.Position > 0 then
          Break;
      end;

      if Player.Position <= 0 then
        Fail('position should advance while playing');

      Player.Pause;
      if Player.State <> psPaused then
        Fail('pause failed');

      Player.Play;
      for I := 1 to 20 do
      begin
        Application.ProcessMessages;
        Sleep(10);
      end;

      Player.Stop;
      for I := 1 to 100 do
      begin
        Application.ProcessMessages;
        if Player.State = psStopped then
          Break;
        Sleep(10);
      end;

      if Player.State <> psStopped then
        Fail('stop failed');

      WriteLn(Format('Played media: duration=%d ms, last position=%d ms', [Player.Duration, Player.Position]));
      WriteLn('PASS: TFFVideoPlayer smoke test OK');
    finally
      Player.Free;
    end;
  finally
    Form.Free;
  end;
end.
