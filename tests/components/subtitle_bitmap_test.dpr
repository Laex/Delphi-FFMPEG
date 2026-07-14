program subtitle_bitmap_test;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFSubtitleDecoder,
  uFFSubtitleOverlay,
  uFFSubtitleBitmap,
  uFFMediaInfo;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 3] of string = (
    '..\..\resource\test_subs_bitmap.mkv',
    '..\..\..\resource\test_subs_bitmap.mkv',
    'D:\Work\Delphi\Delphi-FFMPEG\resource\test_subs_bitmap.mkv',
    ''
  );
var
  Base: string;
  I: Integer;
begin
  Base := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  for I := Low(Candidates) to High(Candidates) do
    if (Candidates[I] <> '') and FileExists(Base + Candidates[I]) then
      Exit(Base + Candidates[I]);
  if FileExists(Candidates[2]) then
    Exit(Candidates[2]);
  Result := '';
end;

procedure BuildSyntheticBitmap(out ABmp: TFFSubtitleBitmap);
begin
  FillChar(ABmp, SizeOf(ABmp), 0);
  ABmp.X := 20;
  ABmp.Y := 30;
  ABmp.W := 4;
  ABmp.H := 2;
  ABmp.Stride := 4;
  SetLength(ABmp.Palette, 8);
  ABmp.Palette[0] := 0;
  ABmp.Palette[1] := 0;
  ABmp.Palette[2] := 0;
  ABmp.Palette[3] := 0;
  ABmp.Palette[4] := 255;
  ABmp.Palette[5] := 0;
  ABmp.Palette[6] := 0;
  ABmp.Palette[7] := 255;
  SetLength(ABmp.Indices, ABmp.Stride * ABmp.H);
  FillChar(ABmp.Indices[0], Length(ABmp.Indices), 1);
end;

function BufferHasNonBlackInRect(ABgra: PByte; AWidth, AHeight, AStride, AX, AY, AW, AH: Integer): Boolean;
var
  Y, X: Integer;
  P: PByte;
begin
  Result := False;
  if ABgra = nil then
    Exit;
  for Y := AY to AY + AH - 1 do
  begin
    if (Y < 0) or (Y >= AHeight) then
      Continue;
    for X := AX to AX + AW - 1 do
    begin
      if (X < 0) or (X >= AWidth) then
        Continue;
      P := ABgra + Y * AStride + X * 4;
      if (P[0] <> 0) or (P[1] <> 0) or (P[2] <> 0) then
        Exit(True);
    end;
  end;
end;

procedure TestSyntheticBlend;
var
  Ev: TFFSubtitleEvent;
  Bgra: TArray<Byte>;
  W, H, Stride: Integer;
begin
  W := 160;
  H := 120;
  Stride := W * 4;
  SetLength(Bgra, Stride * H);
  FillChar(Bgra[0], Length(Bgra), 0);

  FillChar(Ev, SizeOf(Ev), 0);
  Ev.IsBitmap := True;
  BuildSyntheticBitmap(Ev.Bitmap);

  FFSubtitleBlendEventOnBgra(@Bgra[0], W, H, Stride, Ev);
  if not BufferHasNonBlackInRect(@Bgra[0], W, H, Stride, Ev.Bitmap.X, Ev.Bitmap.Y,
    Ev.Bitmap.W, Ev.Bitmap.H) then
    Fail('synthetic bitmap blend did not modify BGRA buffer');
end;

var
  Media: string;
  Reader: TFFReader;
  SubDec: TFFSubtitleDecoder;
  Info: TFFMediaInfo;
  SubIdx: Integer;
  Ev: TFFSubtitleEvent;
  I: Integer;
  Bgra: TArray<Byte>;
  Stride, W, H: Integer;
  FoundBitmap: Boolean;
begin
  TestSyntheticBlend;
  WriteLn('Synthetic bitmap blend OK');

  Media := DefaultMediaFile;
  if ParamCount >= 1 then
    Media := ParamStr(1);

  if Media = '' then
  begin
    WriteLn('PASS: bitmap/PGS subtitle overlay OK (synthetic only)');
    Exit;
  end;

  Info := TFFMediaInfo.Create(nil);
  try
    Info.FileName := Media;
    Info.Probe;
    SubIdx := Info.FindBestStream(AVMEDIA_TYPE_SUBTITLE);
  finally
    Info.Free;
  end;

  if SubIdx < 0 then
  begin
    WriteLn('PASS: bitmap/PGS subtitle overlay OK (synthetic only, no sub stream)');
    Exit;
  end;

  Reader := TFFReader.Create(nil);
  SubDec := TFFSubtitleDecoder.Create(nil);
  try
    Reader.FileName := Media;
    Reader.Open;
    SubDec.Reader := Reader;
    SubDec.StreamIndex := SubIdx;
    SubDec.LoadAll;

    FoundBitmap := False;
    for I := 1 to 8 do
    begin
      Ev := SubDec.GetEventAt(I * 1000);
      if Ev.IsBitmap and Ev.Bitmap.Valid then
      begin
        FoundBitmap := True;
        Break;
      end;
    end;

    if not FoundBitmap then
    begin
      WriteLn('PASS: bitmap/PGS subtitle overlay OK (synthetic only, no bitmap events in media)');
      Exit;
    end;

    W := 320;
    H := 240;
    Stride := W * 4;
    SetLength(Bgra, Stride * H);
    FillChar(Bgra[0], Length(Bgra), 0);
    FFSubtitleBlendEventOnBgra(@Bgra[0], W, H, Stride, Ev);

    if not BufferHasNonBlackInRect(@Bgra[0], W, H, Stride, Ev.Bitmap.X, Ev.Bitmap.Y,
      Ev.Bitmap.W, Ev.Bitmap.H) then
      Fail('decoded bitmap subtitle blend did not modify BGRA buffer');

    WriteLn(Format('Decoded bitmap: %dx%d at (%d,%d)', [Ev.Bitmap.W, Ev.Bitmap.H, Ev.Bitmap.X, Ev.Bitmap.Y]));
    WriteLn('PASS: bitmap/PGS subtitle overlay OK');
  finally
    SubDec.Free;
    Reader.Free;
  end;
end.
