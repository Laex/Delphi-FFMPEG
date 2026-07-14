program loader_logger_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$I ../../source/ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  uFFLoader,
  uFFLogger;

type
  TLogSink = class
  public
    LogCount: Integer;
    procedure OnLog(Sender: TObject; ALevel: TFFLogLevel; const AMessage: string);
  end;

procedure TLogSink.OnLog(Sender: TObject; ALevel: TFFLogLevel; const AMessage: string);
begin
  Inc(LogCount);
  WriteLn('[', Ord(ALevel), '] ', Trim(AMessage));
end;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

var
  Loader: TFFLoader;
  Logger: TFFLogger;
  Sink: TLogSink;

begin
  WriteLn('Delphi-FFMPEG loader/logger test');
  Sink := TLogSink.Create;
  try
    Loader := TFFLoader.Create(nil);
    try
      Loader.SearchPath := ExtractFilePath(ParamStr(0));
      Loader.LoadLibraries;
      WriteLn('FFmpeg version: ', Loader.GetFFmpegVersion);
      WriteLn('avutil: ', Loader.GetLibraryVersion('avutil'));
      WriteLn('avcodec: ', Loader.GetLibraryVersion('avcodec'));
    finally
      Loader.Free;
    end;

    Logger := TFFLogger.Create(nil);
    try
      Logger.Level := llInfo;
      Logger.OnLog := Sink.OnLog;
      Logger.Active := True;
      av_log(nil, AV_LOG_INFO, PAnsiChar(AnsiString('loader_logger_test: hello from av_log')));
      av_log(nil, AV_LOG_DEBUG, PAnsiChar(AnsiString('loader_logger_test: debug should be hidden')));
      Logger.Active := False;
    finally
      Logger.Free;
    end;

    if Sink.LogCount < 1 then
      Fail('expected at least one log line from av_log');

    WriteLn('PASS: TFFLoader and TFFLogger OK (', Sink.LogCount, ' log line(s))');
  finally
    Sink.Free;
  end;
end.
