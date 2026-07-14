program remux_job_test;

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
  uFFReader,
  uFFWriter,
  uFFRemuxJob;

function FileSizeBytes(const AFile: string): Int64;
var
  SR: TSearchRec;
begin
  if FindFirst(AFile, faAnyFile, SR) = 0 then
  try
    Result := SR.Size;
  finally
    FindClose(SR);
  end
  else
    Result := 0;
end;

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

procedure TestRemuxJob(const InFile, OutFile: string);
var
  Reader: TFFReader;
  Writer: TFFWriter;
  Job: TFFRemuxJob;
begin
  if FileExists(OutFile) then
    DeleteFile(OutFile);

  Reader := TFFReader.Create(nil);
  Writer := TFFWriter.Create(nil);
  Job := TFFRemuxJob.Create(nil);
  try
    Reader.FileName := InFile;
    Writer.FileName := OutFile;
    Writer.FormatName := 'matroska';

    Job.Reader := Reader;
    Job.Writer := Writer;
    Job.Start;
    while Job.State <> rsStopped do
      Sleep(50);
    Job.Stop;

    if not FileExists(OutFile) then
      Fail('output file was not created');
    if FileSizeBytes(OutFile) <= 0 then
      Fail('output file is empty');

    WriteLn(Format('Remux OK: %d bytes', [FileSizeBytes(OutFile)]));
  finally
    Job.Free;
    Writer.Free;
    Reader.Free;
  end;
end;

var
  InFile: string;
  OutFile: string;
begin
  WriteLn('Delphi-FFMPEG remux job test');
  if ParamCount >= 1 then
    InFile := ParamStr(1)
  else
    InFile := DefaultMediaFile;

  if InFile = '' then
  begin
    WriteLn('SKIP: no input media file');
    Halt(2);
  end;

  OutFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'remux_job_out.mkv';
  if ParamCount >= 2 then
    OutFile := ParamStr(2);

  TestRemuxJob(InFile, OutFile);
  WriteLn('PASS: TFFRemuxJob OK');
end.
