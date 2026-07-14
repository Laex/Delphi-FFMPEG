program transcode_preset_test;

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
  uFFDecoder,
  uFFEncoder,
  uFFWriter,
  uFFTranscodePreset;

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

function FindVideoStreamIndex(const Reader: TFFReader): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Reader.Streams.Count - 1 do
    if Reader.Streams.GetInfo(I).MediaType = AVMEDIA_TYPE_VIDEO then
      Exit(I);
end;

procedure TestPreset(const InFile, OutFile: string; APreset: TFFTranscodePreset);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Encoder: TFFEncoder;
  Writer: TFFWriter;
  Job: TFFTranscodeJob;
begin
  if FileExists(OutFile) then
    DeleteFile(OutFile);

  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Encoder := TFFEncoder.Create(nil);
  Writer := TFFWriter.Create(nil);
  Job := TFFTranscodeJob.Create(nil);
  try
    Reader.FileName := InFile;
    Reader.Open;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FindVideoStreamIndex(Reader);
    if Decoder.StreamIndex < 0 then
      Fail('no video stream');

    Job.Reader := Reader;
    Job.InputDecoder := Decoder;
    Job.Encoder := Encoder;
    Job.Writer := Writer;
    Job.Preset := APreset;
    Job.CopyAudio := False;
    Job.ConfigureOutput(OutFile);
    Job.ApplyPreset;

    if Encoder.CodecName = '' then
      Fail('codec name empty after ApplyPreset');

    Job.Start;
    while Encoder.State <> esStopped do
      Sleep(50);
    Job.Stop;

    if not FileExists(OutFile) then
      Fail('output file was not created: ' + OutFile);
    if FileSizeBytes(OutFile) <= 0 then
      Fail('output file is empty: ' + OutFile);

    WriteLn(Format('OK preset=%s codec=%s size=%d',
      [TFFTranscodePresetHelper.DisplayName(APreset), Encoder.CodecName, FileSizeBytes(OutFile)]));
  finally
    Job.Free;
    Writer.Free;
    Encoder.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

var
  InFile: string;
  OutDir: string;
begin
  WriteLn('Delphi-FFMPEG transcode preset test');
  if ParamCount >= 1 then
    InFile := ParamStr(1)
  else
    InFile := DefaultMediaFile;

  if InFile = '' then
  begin
    WriteLn('SKIP: no input media file');
    Halt(2);
  end;

  OutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  TestPreset(InFile, OutDir + 'preset_mpeg4.mkv', ftpMpeg4_800k);
  WriteLn('PASS: TFFTranscodeJob presets OK');
end.
