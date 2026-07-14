program encoder_transcode_test;

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
  uFFWriter;

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
  Write(#13, Format('progress %d / %d ms', [APositionMs, ADurationMs]));
end;

procedure TestTranscode(const InFile, OutFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Encoder: TFFEncoder;
  Writer: TFFWriter;
  Progress: TProgressCapture;
begin
  if FileExists(OutFile) then
    DeleteFile(OutFile);

  Progress := TProgressCapture.Create;
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Encoder := TFFEncoder.Create(nil);
  Writer := TFFWriter.Create(nil);
  try
    Reader.FileName := InFile;
    Reader.Open;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FindVideoStreamIndex(Reader);
    if Decoder.StreamIndex < 0 then
      Fail('no video stream');

    Writer.FileName := OutFile;
    Encoder.Reader := Reader;
    Encoder.InputDecoder := Decoder;
    Encoder.OutputWriter := Writer;
    Encoder.CopyAudio := True;
    Encoder.TranscodeAudio := False;
    Encoder.CodecName := 'mpeg4';
    Encoder.BitRate := 800000;
    Encoder.OnProgress := Progress.OnProgress;

    Encoder.Start;
    while Encoder.State <> esStopped do
      Sleep(50);
    Encoder.Stop;
    WriteLn;

    if not FileExists(OutFile) then
      Fail('output file was not created');
    if FileSizeBytes(OutFile) <= 0 then
      Fail('output file is empty');
    if Progress.LastProgress < 0 then
      Fail('no progress events received');

    WriteLn(Format('Transcoded %s -> %s (%d bytes, last progress %d ms)',
      [InFile, OutFile, FileSizeBytes(OutFile), Progress.LastProgress]));
  finally
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
  WriteLn('Delphi-FFMPEG encoder transcode test');
  if ParamCount >= 1 then
    InFile := ParamStr(1)
  else
    InFile := DefaultMediaFile;

  if InFile = '' then
  begin
    WriteLn('SKIP: no input media file');
    Halt(2);
  end;

  OutFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'encoder_transcode_out.mp4';
  if ParamCount >= 2 then
    OutFile := ParamStr(2);

  TestTranscode(InFile, OutFile);
  WriteLn('PASS: TFFEncoder transcode pipeline OK');
end.
