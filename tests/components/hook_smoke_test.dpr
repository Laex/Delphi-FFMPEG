program hook_smoke_test;

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
  uFFEncoder,
  uFFFrame,
  uFFPacket,
  uFFHooks
  {$IFNDEF FPC}
  , uFFPlaybackEngine
  {$ENDIF}
  ;

{$IFNDEF FPC}
type
  TFFPlayerState = uFFPlaybackEngine.TFFPlayerState;
{$ENDIF}

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

procedure DrawGreenBar(ABgra: PByte; AWidth, AHeight, AStride: Integer);
var
  Y, X: Integer;
  Row: PByte;
begin
  if (ABgra = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;
  if AHeight > 4 then
    AHeight := 4;
  for Y := 0 to AHeight - 1 do
  begin
    Row := ABgra + Y * AStride;
    for X := 0 to AWidth - 1 do
    begin
      Row[X * 4 + 0] := 0;
      Row[X * 4 + 1] := 255;
      Row[X * 4 + 2] := 0;
      Row[X * 4 + 3] := 255;
    end;
  end;
end;

type
  THookCounters = class
  public
    FrameHooks: Integer;
    VideoHooks: Integer;
    AudioHooks: Integer;
    EncoderHooks: Integer;
    {$IFNDEF FPC}
    procedure EngineFrameHook(Sender: TObject; AFrame: TFFFrame; var AHandled: Boolean);
    procedure EngineVideoHook(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer);
    procedure EngineAudioHook(Sender: TObject; ABuffer: PByte; var AByteCount: Integer);
    {$ENDIF}
    procedure EncoderFrameHook(Sender: TObject; AFrame: TFFFrame; var AHandled: Boolean);
  end;

{$IFNDEF FPC}
procedure THookCounters.EngineFrameHook(Sender: TObject; AFrame: TFFFrame; var AHandled: Boolean);
begin
  Inc(FrameHooks);
  AHandled := False;
end;

procedure THookCounters.EngineVideoHook(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer);
begin
  Inc(VideoHooks);
  DrawGreenBar(ABgra, AWidth, AHeight, AStride);
end;

procedure THookCounters.EngineAudioHook(Sender: TObject; ABuffer: PByte; var AByteCount: Integer);
var
  I: Integer;
begin
  Inc(AudioHooks);
  if (ABuffer <> nil) and (AByteCount > 0) then
    for I := 0 to AByteCount - 1 do
      PByte(NativeUInt(ABuffer) + NativeUInt(I))^ := 0;
end;
{$ENDIF}

procedure THookCounters.EncoderFrameHook(Sender: TObject; AFrame: TFFFrame; var AHandled: Boolean);
var
  Y: Integer;
begin
  Inc(EncoderHooks);
  if (AFrame <> nil) and (AFrame.Raw <> nil) and (AFrame.Raw^.data[0] <> nil) then
  begin
    for Y := 0 to AFrame.Raw^.height - 1 do
      FillChar(PByte(NativeUInt(AFrame.Raw^.data[0]) + NativeUInt(AFrame.Raw^.linesize[0] * Y))^,
        AFrame.Raw^.width, Byte($80 + EncoderHooks));
  end;
  AHandled := False;
end;

procedure FillTestFrame(AFrame: TFFFrame; AWidth, AHeight, AIndex: Integer);
var
  X, Y: Integer;
  YPtr, UPtr, VPtr: PByte;
  Ret: Integer;
begin
  AFrame.Raw^.format := Ord(AV_PIX_FMT_YUV420P);
  AFrame.Raw^.width := AWidth;
  AFrame.Raw^.height := AHeight;
  AFrame.Raw^.pts := AIndex;
  Ret := av_frame_get_buffer(AFrame.Raw, 32);
  if Ret < 0 then
    Fail(Format('av_frame_get_buffer failed (%d)', [Ret]));

  for Y := 0 to AHeight - 1 do
  begin
    YPtr := PByte(NativeUInt(AFrame.Raw^.data[0]) + NativeUInt(AFrame.Raw^.linesize[0] * Y));
    for X := 0 to AWidth - 1 do
      YPtr[X] := Byte((X + Y + AIndex * 3) and $FF);
  end;

  for Y := 0 to (AHeight div 2) - 1 do
  begin
    UPtr := PByte(NativeUInt(AFrame.Raw^.data[1]) + NativeUInt(AFrame.Raw^.linesize[1] * Y));
    VPtr := PByte(NativeUInt(AFrame.Raw^.data[2]) + NativeUInt(AFrame.Raw^.linesize[2] * Y));
    for X := 0 to (AWidth div 2) - 1 do
    begin
      UPtr[X] := 128;
      VPtr[X] := 128;
    end;
  end;
end;

procedure TestEncoderHook(Counters: THookCounters);
const
  FrameCount = 5;
  Width = 160;
  Height = 120;
var
  Encoder: TFFEncoder;
  Frame: TFFFrame;
  Packet: TFFPacket;
  I, Ret: Integer;
begin
  Encoder := TFFEncoder.Create(nil);
  Frame := TFFFrame.Create;
  Packet := TFFPacket.Create;
  try
    Encoder.Width := Width;
    Encoder.Height := Height;
    Encoder.OnFrameHook := Counters.EncoderFrameHook;
    Encoder.Initialize;

    for I := 0 to FrameCount - 1 do
    begin
      FillTestFrame(Frame, Width, Height, I);
      Ret := Encoder.SendFrame(Frame);
      if Ret < 0 then
        Fail(Format('SendFrame failed (%d)', [Ret]));
      while Encoder.ReceivePacket(Packet) = 0 do
        Packet.Clear;
    end;

    if Counters.EncoderHooks <> FrameCount then
      Fail(Format('encoder OnFrameHook expected %d calls, got %d', [FrameCount, Counters.EncoderHooks]));
  finally
    Packet.Free;
    Frame.Free;
    Encoder.Free;
  end;
end;

function ResolveMediaFile: string;
const
  Candidates: array [0 .. 2] of string = (
    '..\..\resource\768x576.avi',
    '..\..\..\resource\768x576.avi',
    'resource\768x576.avi'
  );
var
  I: Integer;
begin
  for I := Low(Candidates) to High(Candidates) do
  begin
    Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + Candidates[I]);
    if FileExists(Result) then
      Exit;
  end;
  Result := '';
end;

{$IFNDEF FPC}
procedure TestPlaybackHooks(Counters: THookCounters; const AFileName: string);
var
  Engine: TFFPlaybackEngine;
  I: Integer;
begin
  Engine := TFFPlaybackEngine.Create;
  try
    Engine.OnFrameHook := Counters.EngineFrameHook;
    Engine.OnVideoHook := Counters.EngineVideoHook;
    Engine.OnAudioHook := Counters.EngineAudioHook;
    Engine.FileName := AFileName;
    Engine.Play;

    for I := 1 to 50 do
    begin
      Sleep(100);
      if Engine.State = psStopped then
        Break;
    end;

    Engine.Stop;

    if Counters.FrameHooks < 1 then
      Fail('playback OnFrameHook was not called');
    if Counters.VideoHooks < 1 then
      Fail('playback OnVideoHook was not called');
  finally
    Engine.Free;
  end;
end;
{$ENDIF}

var
  Counters: THookCounters;
  {$IFNDEF FPC}
  MediaFile: string;
  {$ENDIF}

begin
  WriteLn('Delphi-FFMPEG hook smoke test');
  TFFLoader.EnsureLoaded;

  Counters := THookCounters.Create;
  try
    TestEncoderHook(Counters);
    WriteLn('Encoder hook: OK (', Counters.EncoderHooks, ' calls)');

    {$IFNDEF FPC}
    MediaFile := ResolveMediaFile;
    if MediaFile = '' then
      WriteLn('SKIP: playback hooks (media file not found)')
    else
    begin
      TestPlaybackHooks(Counters, MediaFile);
      WriteLn('Playback hooks: OK (frame=', Counters.FrameHooks,
        ', video=', Counters.VideoHooks, ', audio=', Counters.AudioHooks, ')');
    end;
    {$ELSE}
    WriteLn('SKIP: playback hooks (Delphi playback engine)');
    {$ENDIF}
  finally
    Counters.Free;
  end;

  WriteLn('PASS: hook smoke test');
end.
