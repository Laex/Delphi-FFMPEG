program frame_filter_test;

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
  libavfilter,
  uFFReader,
  uFFDecoder,
  uFFFrame,
  uFFComponentBase,
  uFFFrameFilter;

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
  TFrameCapture = class(TInterfacedObject, IFFFrameSink)
  private
    FWidth: Integer;
    FHeight: Integer;
    FGotFrame: Boolean;
  public
    procedure TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
    property GotFrame: Boolean read FGotFrame;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

procedure TFrameCapture.TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
begin
  if AFrame = nil then
    Exit;
  FWidth := AFrame.Raw^.width;
  FHeight := AFrame.Raw^.height;
  FGotFrame := True;
end;

procedure TestScaleFilter(const InFile: string);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Filter: TFFFrameFilter;
  Frame: TFFFrame;
  Capture: TFrameCapture;
begin
  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Filter := TFFFrameFilter.Create(nil);
  Frame := TFFFrame.Create;
  Capture := TFrameCapture.Create;
  try
    Reader.FileName := InFile;
    Reader.Open;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := FindVideoStreamIndex(Reader);
    if Decoder.StreamIndex < 0 then
      Fail('no video stream');

    Filter.InputDecoder := Decoder;
    Filter.FilterDescription := 'scale=160:120';
    Filter.SubscribeFrameSink(Capture);

    if Decoder.DecodeFrameAt(0, Frame) < 0 then
      Fail('DecodeFrameAt failed');

    Filter.TakeFrame(Decoder, Frame, Decoder.StreamIndex);

    if not Capture.GotFrame then
      Fail('filter did not output a frame');
    if (Capture.Width <> 160) or (Capture.Height <> 120) then
      Fail(Format('unexpected size %dx%d (expected 160x120)', [Capture.Width, Capture.Height]));

    WriteLn(Format('Filtered frame size %dx%d OK', [Capture.Width, Capture.Height]));
  finally
    Frame.Free;
    Filter.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;

var
  InFile: string;
begin
  WriteLn('Delphi-FFMPEG frame filter test');
  if ParamCount >= 1 then
    InFile := ParamStr(1)
  else
    InFile := DefaultMediaFile;

  if InFile = '' then
  begin
    WriteLn('SKIP: no input media file');
    Halt(2);
  end;

  TestScaleFilter(InFile);
  WriteLn('PASS: TFFFrameFilter scale OK');
end.
