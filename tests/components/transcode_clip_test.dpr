program transcode_clip_test;

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
  uFFFrameFilter,
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

type
  TProgressCapture = class
  public
    LastProgress: Int64;
    constructor Create;
    procedure OnProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
  end;

constructor TProgressCapture.Create;
begin
  inherited Create;
  LastProgress := -1;
end;

procedure TProgressCapture.OnProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
begin
  if APositionMs > LastProgress then
    LastProgress := APositionMs;
end;

procedure TestClipWithFilter(const InFile, OutFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Encoder: TFFEncoder;
  Writer: TFFWriter;
  Filter: TFFFrameFilter;
  Job: TFFTranscodeJob;
  Progress: TProgressCapture;
const
  ClipStartMs = 500;
  ClipEndMs = 2500;
begin
  if FileExists(OutFile) then
    DeleteFile(OutFile);

  Progress := TProgressCapture.Create;
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Encoder := TFFEncoder.Create(nil);
  Writer := TFFWriter.Create(nil);
  Filter := TFFFrameFilter.Create(nil);
  Job := TFFTranscodeJob.Create(nil);
  try
    Reader.FileName := InFile;
    Reader.Open;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FindVideoStreamIndex(Reader);
    if Decoder.StreamIndex < 0 then
      Fail('no video stream');

    Filter.InputDecoder := Decoder;
    Filter.FilterDescription := 'scale=160:120';

    Writer.FileName := OutFile;
    Job.Reader := Reader;
    Job.InputDecoder := Decoder;
    Job.Encoder := Encoder;
    Job.Writer := Writer;
    Job.FrameFilter := Filter;
    Job.StartMs := ClipStartMs;
    Job.EndMs := ClipEndMs;
    Job.Preset := ftpMpeg4_800k;
    Job.CopyAudio := False;
    Job.ConfigureOutput(OutFile);
    Job.ApplyPreset;

    Encoder.Width := 160;
    Encoder.Height := 120;
    Encoder.OnProgress := Progress.OnProgress;

    Job.Start;
    while Encoder.State <> esStopped do
      Sleep(50);
    Job.Stop;

    if not FileExists(OutFile) then
      Fail('output file was not created');
    if FileSizeBytes(OutFile) <= 0 then
      Fail('output file is empty');
    if Progress.LastProgress < 0 then
    begin
      if (not FileExists(OutFile)) or (FileSizeBytes(OutFile) <= 0) then
        Fail('no progress events received');
    end;

    WriteLn(Format('Clip+filter transcode OK: %d bytes, last progress %d ms',
      [FileSizeBytes(OutFile), Progress.LastProgress]));
  finally
    Job.Free;
    Filter.Free;
    Writer.Free;
    Encoder.Free;
    Decoder.Free;
    Reader.Free;
    Progress.Free;
  end;
end;

var
  InFile: string;
  OutFile: string;
begin
  WriteLn('Delphi-FFMPEG transcode clip+filter test');
  if ParamCount >= 1 then
    InFile := ParamStr(1)
  else
    InFile := DefaultMediaFile;

  if InFile = '' then
  begin
    WriteLn('SKIP: no input media file');
    Halt(2);
  end;

  OutFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'transcode_clip_out.mkv';
  if ParamCount >= 2 then
    OutFile := ParamStr(2);

  TestClipWithFilter(InFile, OutFile);
  WriteLn('PASS: clip export + frame filter in transcode OK');
end.
