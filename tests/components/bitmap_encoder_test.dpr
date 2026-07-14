program bitmap_encoder_test;

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
  {$IFDEF MSWINDOWS}
  Vcl.Graphics,
  {$ENDIF}
  ffmpeg_types,
  uFFWriter,
  uFFBitmapEncoder;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

{$IFDEF MSWINDOWS}
procedure TestBitmapEncoder(const AOutFile: string);
const
  FrameCount = 5;
  Width = 320;
  Height = 240;
var
  Writer: TFFWriter;
  Encoder: TFFBitmapEncoder;
  Bmp: TBitmap;
  X, Y, I: Integer;
  Row: PByte;
  OutSize: Int64;
  SR: TSearchRec;
begin
  if FileExists(AOutFile) then
    DeleteFile(AOutFile);

  Writer := TFFWriter.Create(nil);
  Encoder := TFFBitmapEncoder.Create(nil);
  Bmp := TBitmap.Create;
  try
    Writer.FileName := AOutFile;
    Writer.FormatName := 'matroska';
    Encoder.OutputWriter := Writer;
    Encoder.Width := Width;
    Encoder.Height := Height;
    Encoder.FrameRateNum := 10;
    Encoder.CodecName := 'mpeg4';
    Encoder.BitRate := 500000;

    Bmp.PixelFormat := pf32bit;
    Bmp.SetSize(Width, Height);

    for I := 0 to FrameCount - 1 do
    begin
      for Y := 0 to Height - 1 do
      begin
        Row := PByte(Bmp.ScanLine[Y]);
        for X := 0 to Width - 1 do
        begin
          Row[X * 4 + 0] := Byte((X + I * 20) and $FF);
          Row[X * 4 + 1] := Byte((Y + I * 10) and $FF);
          Row[X * 4 + 2] := Byte((X + Y + I * 5) and $FF);
          Row[X * 4 + 3] := 255;
        end;
      end;
      Encoder.AddBitmap(Bmp);
    end;

    Encoder.Close;
    Writer.Close;
    if not FileExists(AOutFile) then
      Fail('output file was not created');
    OutSize := 0;
    if FindFirst(AOutFile, faAnyFile, SR) = 0 then
    try
      OutSize := SR.Size;
    finally
      FindClose(SR);
    end;
    if OutSize <= 0 then
      Fail('output file is empty');
    WriteLn('OK: bitmap encode -> ', AOutFile, ' (', OutSize, ' bytes)');
  finally
    Bmp.Free;
    Encoder.Free;
    Writer.Free;
  end;
end;
{$ENDIF}

begin
  try
    {$IFDEF MSWINDOWS}
    TestBitmapEncoder(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'bitmap_out.mkv');
    {$ELSE}
    WriteLn('SKIP: bitmap_encoder_test requires MSWINDOWS for VCL TBitmap');
    {$ENDIF}
  except
    on E: Exception do
      Fail(E.ClassName + ': ' + E.Message);
  end;
end.
