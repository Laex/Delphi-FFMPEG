program writer_memory_access_test;

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
  uFFPacket,
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

procedure TestWriterToMemory(const InFile: string);
var
  Reader: TFFReader;
  Writer: TFFWriter;
  Packet: TFFPacket;
  Adapter: TFFMemoryAccessAdapter;
  Mem: TMemoryStream;
  I: Integer;
  Info: TFFStreamInfo;
  Ret: Integer;
begin
  Reader := TFFReader.Create(nil);
  Writer := TFFWriter.Create(nil);
  Packet := TFFPacket.Create;
  Adapter := TFFMemoryAccessAdapter.Create(nil);
  Mem := TMemoryStream.Create;
  try
    Reader.FileName := InFile;
    Reader.Open;

    Adapter.Attach(Mem, mamWrite, False);
    Writer.OutputAdapter := Adapter;
    Writer.FormatName := 'matroska';
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

    if Mem.Size <= 0 then
      Fail('memory output is empty');
    WriteLn('Memory output bytes: ', Mem.Size);
  finally
    Mem.Free;
    Adapter.Free;
    Packet.Free;
    Writer.Free;
    Reader.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG writer memory access adapter test');
  if ParamCount >= 1 then
    TestWriterToMemory(ParamStr(1))
  else if DefaultMediaFile <> '' then
    TestWriterToMemory(DefaultMediaFile)
  else
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;
  WriteLn('PASS: writer memory adapter ok');
end.

