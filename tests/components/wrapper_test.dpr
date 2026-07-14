program wrapper_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$I ../../source/ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavcodec,
  uFFPacket,
  uFFFrame;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

procedure TestPacket;
var
  Pkt, Copy: TFFPacket;
begin
  Pkt := TFFPacket.Create;
  try
    if Pkt.Raw = nil then
      Fail('TFFPacket.Raw is nil after Create');
    Pkt.Clear;
    Copy := Pkt.Clone;
    try
      if Copy.Raw = nil then
        Fail('cloned packet Raw is nil');
    finally
      Copy.Free;
    end;
  finally
    Pkt.Free;
  end;
end;

procedure TestFrame;
var
  Frame: TFFFrame;
begin
  Frame := TFFFrame.Create;
  try
    if Frame.Raw = nil then
      Fail('TFFFrame.Raw is nil after Create');
    if Frame.GetSampleCount <> 0 then
      Fail('empty frame should have nb_samples = 0');
    if Frame.GetPictureType <> AV_PICTURE_TYPE_NONE then
      Fail('empty frame pict_type should be NONE');
    Frame.Clear;
    Frame.Unref;
  finally
    Frame.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG component wrappers test');
  TestPacket;
  TestFrame;
  WriteLn('PASS: TFFPacket and TFFFrame lifecycle OK');
end.
