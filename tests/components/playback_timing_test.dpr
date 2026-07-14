program playback_timing_test;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Controls,
  uFFPlaybackEngine,
  uFFVideoPlayer;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

var
  Form: TForm;
  Player: TFFVideoPlayer;
  Media: string;
  T0, T1: UInt64;
  PosBefore, PosAfter, DurMs: Int64;
  ElapsedMs, PosDelta: Int64;
  Ratio: Double;
begin
  if ParamCount >= 1 then
    Media := ParamStr(1)
  else
    Media := 'D:\Work\Delphi\Delphi-FFMPEG\resource\trailer.avi';

  if not FileExists(Media) then
  begin
    WriteLn('SKIP: media not found: ', Media);
    Halt(2);
  end;

  Application.Initialize;
  Form := TForm.Create(nil);
  try
    Form.Width := 640;
    Form.Height := 480;
    Player := TFFVideoPlayer.Create(Form);
    try
      Player.Parent := Form;
      Player.Align := alClient;
      Player.Volume := 0;
      Player.FileName := Media;
      Form.Show;

      Player.Play;
      Sleep(500);
      Application.ProcessMessages;

      DurMs := Player.Duration;
      if DurMs <= 0 then
        Fail('duration should be > 0');
      WriteLn(Format('Media duration: %d ms (%.2f s)', [DurMs, DurMs / 1000]));

      PosBefore := Player.Position;
      T0 := GetTickCount64;
      Sleep(3000);
      Application.ProcessMessages;
      T1 := GetTickCount64;
      PosAfter := Player.Position;

      ElapsedMs := Int64(T1 - T0);
      PosDelta := PosAfter - PosBefore;
      if PosDelta <= 0 then
        Fail(Format('position did not advance (%d -> %d)', [PosBefore, PosAfter]));

      Ratio := PosDelta / ElapsedMs;
      WriteLn(Format('Wall: %d ms, position delta: %d ms, ratio=%.3f', [ElapsedMs, PosDelta, Ratio]));

      { Allow 0.75..1.35x realtime (timer jitter, startup skew). }
      if (Ratio < 0.75) or (Ratio > 1.35) then
        Fail(Format('playback rate out of range (ratio=%.3f, expected ~1.0)', [Ratio]));

      Player.Stop;
      WriteLn('PASS: playback timing OK');
    finally
      Player.Free;
    end;
  finally
    Form.Free;
  end;
end.
