unit uFFmpegCodecUtils;

interface

uses
  System.SysUtils,
  ffmpeg_types,
  libavcodec,
  libavutil;

function CodecStringFromParameters(par: PAVCodecParameters): string;

function VideoSizeFromParameters(par: PAVCodecParameters; out Width, Height: Integer): Boolean;

procedure ConfigureVideoEncoder(Ctx: PAVCodecContext; AWidth, AHeight, FpsNum, FpsDen: Integer);

implementation

function CodecStringFromParameters(par: PAVCodecParameters): string;
var
  ctx: PAVCodecContext;
  buf: array [0 .. 255] of AnsiChar;
begin
  Result := '';
  if not Assigned(par) then
    Exit;
  ctx := avcodec_alloc_context3(nil);
  if not Assigned(ctx) then
    Exit;
  try
    if avcodec_parameters_to_context(ctx, par) < 0 then
      Exit;
    avcodec_string(@buf[0], Length(buf), ctx, 0);
    Result := string(AnsiString(buf));
  finally
    avcodec_free_context(ctx);
  end;
end;

function VideoSizeFromParameters(par: PAVCodecParameters; out Width, Height: Integer): Boolean;
var
  ctx: PAVCodecContext;
  CodecText, Num: string;
  I, P, W, H: Integer;
begin
  Width := 0;
  Height := 0;
  Result := False;
  if not Assigned(par) then
    Exit;
  if (par.width > 0) and (par.height > 0) and (par.width >= 160) then
  begin
    Width := par.width;
    Height := par.height;
    Exit(True);
  end;
  ctx := avcodec_alloc_context3(nil);
  if Assigned(ctx) then
  try
    if avcodec_parameters_to_context(ctx, par) >= 0 then
      if (ctx^.width >= 160) and (ctx^.height > 0) then
      begin
        Width := ctx^.width;
        Height := ctx^.height;
        Exit(True);
      end;
  finally
    avcodec_free_context(ctx);
  end;
  CodecText := CodecStringFromParameters(par);
  P := Pos('x', CodecText);
  if P > 0 then
  begin
    Num := '';
    for I := P - 1 downto 1 do
      if CharInSet(CodecText[I], ['0'..'9']) then
        Num := CodecText[I] + Num
      else if Num <> '' then
        Break;
    if Num <> '' then W := StrToIntDef(Num, 0) else W := 0;
    Num := '';
    for I := P + 1 to Length(CodecText) do
      if CharInSet(CodecText[I], ['0'..'9']) then
        Num := Num + CodecText[I]
      else if Num <> '' then
        Break;
    if Num <> '' then H := StrToIntDef(Num, 0) else H := 0;
    if (W >= 160) and (H > 0) then
    begin
      Width := W;
      Height := H;
      Result := True;
    end;
  end;
end;

procedure ConfigureVideoEncoder(Ctx: PAVCodecContext; AWidth, AHeight, FpsNum, FpsDen: Integer);
var
  Tb, Fps: AnsiString;
  PixFmt: PAnsiChar;
begin
  if not Assigned(Ctx) then
    Exit;
  if AWidth <= 0 then AWidth := 640;
  if AHeight <= 0 then AHeight := 480;
  if FpsNum <= 0 then FpsNum := 25;
  if FpsDen <= 0 then FpsDen := 1;

  av_opt_set_int(Ctx, 'width', AWidth, 0);
  av_opt_set_int(Ctx, 'height', AHeight, 0);
  Tb := AnsiString(Format('%d/%d', [1, FpsNum]));
  Fps := AnsiString(Format('%d/%d', [FpsNum, FpsDen]));
  av_opt_set(Ctx, 'time_base', PAnsiChar(Tb), 0);
  av_opt_set(Ctx, 'framerate', PAnsiChar(Fps), 0);
  PixFmt := av_get_pix_fmt_name(AV_PIX_FMT_YUV420P);
  if Assigned(PixFmt) then
    av_opt_set(Ctx, 'pix_fmt', PixFmt, 0);
  av_opt_set_int(Ctx, 'gop_size', FpsNum, 0);
  av_opt_set_int(Ctx, 'max_b_frames', 0, 0);
  av_opt_set_int(Ctx, 'bit_rate', 2500000, 0);

  Ctx^.width := AWidth;
  Ctx^.height := AHeight;
  Ctx^.time_base.num := 1;
  Ctx^.time_base.den := FpsNum;
  Ctx^.framerate.num := FpsNum;
  Ctx^.framerate.den := FpsDen;
  Ctx^.pix_fmt := AV_PIX_FMT_YUV420P;
  Ctx^.gop_size := FpsNum;
  Ctx^.max_b_frames := 0;
  Ctx^.bit_rate := 2500000;
end;

end.
