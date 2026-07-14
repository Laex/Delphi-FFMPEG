program memory_access_test;

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
  uFFReader,
  uFFMemoryAccessAdapter;

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

procedure TestMemoryReader(const FileName: string);
var
  Fs: TFileStream;
  Ms: TMemoryStream;
  Adapter: TFFMemoryAccessAdapter;
  Reader: TFFReader;
begin
  Fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Ms := TMemoryStream.Create;
    try
      Ms.CopyFrom(Fs, 0);
      Ms.Position := 0;

      Adapter := TFFMemoryAccessAdapter.Create(nil);
      Reader := TFFReader.Create(nil);
      try
        Adapter.Attach(Ms, mamRead, False);
        Reader.InputAdapter := Adapter;
        Reader.Open;
        if Reader.StreamCount <= 0 then
          Fail('no streams detected');
        WriteLn('Streams: ', Reader.StreamCount, ', duration(us): ', Reader.Duration);
        Reader.Close;
      finally
        Reader.Free;
        Adapter.Free;
      end;
    finally
      Ms.Free;
    end;
  finally
    Fs.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG memory access adapter test');
  try
  if ParamCount >= 1 then
    TestMemoryReader(ParamStr(1))
  else if DefaultMediaFile <> '' then
    TestMemoryReader(DefaultMediaFile)
  else
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;
  WriteLn('PASS: memory adapter ok');
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
      Halt(3);
    end;
  end;
end.

