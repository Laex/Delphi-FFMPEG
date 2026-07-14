program writer_remux_test;

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
  uFFPacket;

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
var
  Candidates: array of string;
  I: Integer;
begin
  SetLength(Candidates, 3);
  Candidates[0] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\resource\768x576.avi';
  Candidates[1] := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\..\resource\768x576.avi';
  Candidates[2] := 'D:\Work\Delphi\Delphi-FFMPEG\resource\768x576.avi';
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Candidates[I]) then
      Exit(Candidates[I]);
  Result := '';
end;

function DefaultOutFile: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'writer_remux_out.mkv';
end;

procedure TestRemux(const InFile, OutFile: string);
var
  Reader: TFFReader;
  Writer: TFFWriter;
  Packet: TFFPacket;
  I: Integer;
  Info: TFFStreamInfo;
  Ret: Integer;
begin
  if FileExists(OutFile) then
    DeleteFile(OutFile);

  Reader := TFFReader.Create(nil);
  Writer := TFFWriter.Create(nil);
  Packet := TFFPacket.Create;
  try
    Reader.FileName := InFile;
    Reader.Open;

    Writer.FileName := OutFile;
    Writer.Open;

    for I := 0 to Reader.Streams.Count - 1 do
    begin
      Info := Reader.Streams.GetInfo(I);
      if Info.MediaType in [AVMEDIA_TYPE_VIDEO, AVMEDIA_TYPE_AUDIO, AVMEDIA_TYPE_SUBTITLE] then
        Writer.AddStreamCopy(Reader, I);
    end;

    Writer.WriteHeader;

    while Reader.ReadPacket(Packet) do
    begin
      Ret := Writer.WritePacketFromReader(Packet, Reader);
      if Ret < 0 then
        Fail(Format('WritePacketFromReader failed (%d)', [Ret]));
    end;

    Writer.WriteTrailer;
    Writer.Close;
    Reader.Close;

    if not FileExists(OutFile) then
      Fail('output file was not created');
    if FileSizeBytes(OutFile) <= 0 then
      Fail('output file is empty');

    WriteLn(Format('Remuxed %s -> %s (%d bytes)', [InFile, OutFile, FileSizeBytes(OutFile)]));
    WriteLn('PASS: TFFWriter remux test OK');
  finally
    Packet.Free;
    Writer.Free;
    Reader.Free;
  end;
end;

var
  InFile: string;
  OutFile: string;
begin
  if ParamCount >= 1 then
    InFile := ParamStr(1)
  else
    InFile := DefaultMediaFile;

  if ParamCount >= 2 then
    OutFile := ParamStr(2)
  else
    OutFile := DefaultOutFile;

  if InFile = '' then
  begin
    WriteLn('SKIP: no input media file');
    Halt(2);
  end;

  TestRemux(InFile, OutFile);
end.
