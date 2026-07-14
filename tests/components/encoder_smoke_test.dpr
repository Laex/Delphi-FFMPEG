program encoder_smoke_test;

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
  libavcodec,
  uFFEncoder,
  uFFFrame,
  uFFPacket;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

procedure FillTestFrame(AFrame: TFFFrame; AWidth, AHeight, AIndex: Integer);
var
  X, Y: Integer;
  YPtr, UPtr, VPtr: PByte;
begin
  av_frame_make_writable(AFrame.Raw);
  AFrame.Raw^.format := Ord(AV_PIX_FMT_YUV420P);
  AFrame.Raw^.width := AWidth;
  AFrame.Raw^.height := AHeight;
  AFrame.Raw^.pts := AIndex;

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
      UPtr[X] := Byte(128 + Y + AIndex * 2);
      VPtr[X] := Byte(64 + X + AIndex * 5);
    end;
  end;
end;

procedure TestEncoder(const AOutFile: string);
const
  FrameCount = 10;
  Width = 352;
  Height = 288;
var
  Encoder: TFFEncoder;
  Frame: TFFFrame;
  Packet: TFFPacket;
  OutStream: TFileStream;
  I, Ret, Packets: Integer;
  BufSize: Integer;
begin
  if FileExists(AOutFile) then
    DeleteFile(AOutFile);

  Encoder := TFFEncoder.Create(nil);
  Frame := TFFFrame.Create;
  Packet := TFFPacket.Create;
  OutStream := TFileStream.Create(AOutFile, fmCreate);
  try
    Encoder.MediaType := AVMEDIA_TYPE_VIDEO;
    Encoder.CodecName := 'mpeg4';
    Encoder.Width := Width;
    Encoder.Height := Height;
    Encoder.BitRate := 400000;
    Encoder.TimeBaseNum := 1;
    Encoder.TimeBaseDen := 25;
    Encoder.FrameRateNum := 25;
    Encoder.FrameRateDen := 1;
    Encoder.GopSize := 10;
    Encoder.PixelFormat := AV_PIX_FMT_YUV420P;
    Encoder.Initialize;

    Frame.Raw^.format := Ord(AV_PIX_FMT_YUV420P);
    Frame.Raw^.width := Width;
    Frame.Raw^.height := Height;
    Ret := av_frame_get_buffer(Frame.Raw, 32);
    if Ret < 0 then
      Fail(Format('av_frame_get_buffer failed (%d)', [Ret]));

    Packets := 0;
    for I := 0 to FrameCount - 1 do
    begin
      FillTestFrame(Frame, Width, Height, I);
      Ret := Encoder.SendFrame(Frame);
      if Ret < 0 then
        Fail(Format('SendFrame failed (%d)', [Ret]));

      while Encoder.ReceivePacket(Packet) = 0 do
      begin
        Inc(Packets);
        OutStream.WriteBuffer(Packet.Raw^.data^, Packet.Raw^.size);
        Packet.Clear;
      end;
    end;

    Encoder.Flush;
    while Encoder.ReceivePacket(Packet) = 0 do
    begin
      Inc(Packets);
      OutStream.WriteBuffer(Packet.Raw^.data^, Packet.Raw^.size);
      Packet.Clear;
    end;

    if Packets = 0 then
      Fail('encoder produced no packets');

    BufSize := OutStream.Size;
    if BufSize <= 0 then
      Fail('output file is empty');

    WriteLn(Format('Encoded %d frames -> %d packets, %d bytes', [FrameCount, Packets, BufSize]));
    WriteLn('PASS: TFFEncoder synthetic smoke test OK');
  finally
    OutStream.Free;
    Packet.Free;
    Frame.Free;
    Encoder.Free;
  end;
end;

var
  OutFile: string;
begin
  OutFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'encoder_smoke_out.m4v';
  if ParamCount >= 2 then
    OutFile := ParamStr(2);

  TestEncoder(OutFile);
end.
