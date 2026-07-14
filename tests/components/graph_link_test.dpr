program graph_link_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  SyncObjs,
  {$ELSE}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  {$ENDIF}
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFDecoder,
  uFFWriter,
  uFFFrame,
  uFFComponentBase;

type
  TFrameCounter = class(TComponent, IFFFrameSink)
  private
    FLock: TCriticalSection;
    FCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
    function Count: Integer;
  end;

constructor TFrameCounter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
end;

destructor TFrameCounter.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TFrameCounter.TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
begin
  if AFrame = nil then
    Exit;
  FLock.Enter;
  try
    Inc(FCount);
  finally
    FLock.Leave;
  end;
end;

function TFrameCounter.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FCount;
  finally
    FLock.Leave;
  end;
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

function FindVideoStreamIndex(const Reader: TFFReader): Integer;
var
  I: Integer;
  Info: TFFStreamInfo;
begin
  Result := -1;
  for I := 0 to Reader.Streams.Count - 1 do
  begin
    Info := Reader.Streams.GetInfo(I);
    if Info.MediaType = AVMEDIA_TYPE_VIDEO then
      Exit(I);
  end;
end;

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

procedure TestReaderDecoderPush(const MediaFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Counter: TFrameCounter;
  StreamIdx: Integer;
begin
  WriteLn('Test: Reader(AutoPump) -> Decoder -> frame sink');

  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Counter := TFrameCounter.Create(nil);
  try
    Reader.FileName := MediaFile;
    Reader.Open;
    StreamIdx := FindVideoStreamIndex(Reader);
    if StreamIdx < 0 then
      Fail('no video stream');

    Decoder.Reader := Reader;
    Decoder.StreamIndex := StreamIdx;
    Decoder.SubscribeFrameSink(Counter);
    Reader.AutoPump := True;

    Sleep(1500);

    if Counter.Count = 0 then
      Fail('decoder graph produced no frames');

    WriteLn(Format('  decoded frames: %d', [Counter.Count]));
  finally
    Reader.AutoPump := False;
    Reader.Close;
    Counter.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

procedure TestReaderWriterRemuxGraph(const MediaFile, OutFile: string);
var
  Reader: TFFReader;
  Writer: TFFWriter;
begin
  WriteLn('Test: Reader(AutoPump) -> Writer(RemuxReader, AutoSetup)');

  if FileExists(OutFile) then
    DeleteFile(OutFile);

  Reader := TFFReader.Create(nil);
  Writer := TFFWriter.Create(nil);
  try
    Reader.FileName := MediaFile;
    Writer.FileName := OutFile;
    Writer.RemuxReader := Reader;
    Writer.AutoSetup := True;
    Writer.Open;

    Sleep(2000);

    Writer.WriteTrailer;
    Writer.Close;
    Reader.Close;

    if not FileExists(OutFile) then
      Fail('remux graph did not create output file');
    if FileSizeBytes(OutFile) <= 0 then
      Fail('remux graph output is empty');

    WriteLn(Format('  remuxed %d bytes -> %s', [FileSizeBytes(OutFile), OutFile]));
  finally
    Reader.AutoPump := False;
    Writer.Free;
    Reader.Free;
  end;
end;

var
  MediaFile: string;
  OutFile: string;
begin
  WriteLn('Delphi-FFMPEG component graph link test');

  if ParamCount >= 1 then
    MediaFile := ParamStr(1)
  else
    MediaFile := DefaultMediaFile;

  if MediaFile = '' then
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;

  OutFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'graph_link_out.mkv';

  TestReaderDecoderPush(MediaFile);
  TestReaderWriterRemuxGraph(MediaFile, OutFile);

  WriteLn('PASS: component graph linking OK');
end.
