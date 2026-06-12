program dumpframe;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  VCL.Graphics,
  Vcl.Imaging.jpeg,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libswresample,
  libswscale;

procedure usage();
begin
  writeln(format('Usage: %s -i [filename] -o [frame number] -save [filename]', [ParamStr(0)]));
end;

procedure SaveToBitmap(frame_rgba: PAVFrame; width, height: integer; var bitmap: TBitmap);
var
  i: integer;
  src, dst: PByte;
  rowBytes: integer;
begin
  rowBytes := width * 4;
  bitmap.PixelFormat := pf32bit;
  bitmap.Width := width;
  bitmap.Height := height;
  for i := 0 to height - 1 do
  begin
    src := PByte(NativeUInt(frame_rgba.data[0]) + NativeUInt(frame_rgba.linesize[0] * i));
    dst := bitmap.ScanLine[i];
    Move(src^, dst^, rowBytes);
  end;
end;

procedure SaveFrameImage(const filename: string; bitmap: TBitmap);
var
  ext: string;
  jpeg: TJPEGImage;
begin
  ext := LowerCase(ExtractFileExt(filename));
  if (ext = '.bmp') or (ext = '.png') then
    bitmap.SaveToFile(filename)
  else
  begin
    jpeg := TJPEGImage.Create;
    try
      jpeg.Assign(bitmap);
      jpeg.CompressionQuality := 90;
      jpeg.Compress;
      jpeg.SaveToFile(filename);
    finally
      jpeg.Free;
    end;
  end;
end;

var
  ret: integer = 0;
  pkt: PAVPacket = nil;
  fmt_ctx: PAVFormatContext = nil;
  input_file: string;
  st: PAVStream = nil;
  codec_ctx: PAVCodecContext = nil;
  codec: PAVCodec = nil;
  frame: PAVFrame = nil;
  frame_rgba: PAVFrame = nil;
  scale_ctx: PSwsContext = nil;
  video_stream_idx: integer;
  frame_index: integer;
  save_filename: string;
  arg_o: string;
  frame_bitmap: TBitmap;
  frame_saved: Boolean;
begin
  pkt := nil;
  fmt_ctx := nil;
  codec_ctx := nil;
  frame := nil;
  frame_rgba := nil;
  scale_ctx := nil;
  frame_saved := False;

  try
    if ParamCount < 3 then
    begin
      usage();
      ExitCode := 1;
      Exit;
    end;

    if not FindCmdLineSwitch('i', input_file, True) then
    begin
      usage();
      ExitCode := 1;
      Exit;
    end;

    frame_index := 0;
    if FindCmdLineSwitch('o', arg_o, True) then
      frame_index := StrToInt(arg_o);

    if not FindCmdLineSwitch('save', save_filename, True) then
      save_filename := '.\dump.jpeg';

    fmt_ctx := avformat_alloc_context();
    if not Assigned(fmt_ctx) then
      raise Exception.Create('Error allocate avformat context');

    ret := avformat_open_input(fmt_ctx, PAnsiChar(AnsiString(input_file)), nil, nil);
    if ret < 0 then
      raise Exception.Create('Could not open input');

    ret := avformat_find_stream_info(fmt_ctx, nil);
    if ret < 0 then
      raise Exception.Create('Could not find stream information');

    av_dump_format(fmt_ctx, 0, PAnsiChar(AnsiString(input_file)), 0);

    codec := nil;
    ret := av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, codec, 0);
    if ret < 0 then
      raise Exception.CreateFmt('Could not find %s stream', [string(AnsiString(av_get_media_type_string(AVMEDIA_TYPE_VIDEO)))]);

    video_stream_idx := ret;
    st := fmt_ctx.streams[video_stream_idx];

    codec := avcodec_find_decoder(st.codecpar.codec_id);
    if not Assigned(codec) then
      raise Exception.CreateFmt('Failed to find %s codec', [string(AnsiString(avcodec_get_name(st.codecpar.codec_id)))]);

    codec_ctx := avcodec_alloc_context3(codec);
    if not Assigned(codec_ctx) then
      raise Exception.Create('Failed to allocate codec context');

    ret := avcodec_parameters_to_context(codec_ctx, st.codecpar);
    if ret < 0 then
      raise Exception.Create('Failed to copy codec parameters');

    ret := avcodec_open2(codec_ctx, codec, nil);
    if ret < 0 then
      raise Exception.CreateFmt('Failed to open %s codec', [string(AnsiString(codec.name))]);

    frame := av_frame_alloc();
    if not Assigned(frame) then
      raise Exception.Create('Could not allocate frame');

    frame_rgba := av_frame_alloc();
    if not Assigned(frame_rgba) then
      raise Exception.Create('Could not allocate RGBA frame');

    ret := av_image_alloc(@frame_rgba.data[0], @frame_rgba.linesize[0], codec_ctx.width, codec_ctx.height, AV_PIX_FMT_BGRA, 1);
    if ret < 0 then
      raise Exception.Create('Could not allocate RGBA buffer');

    scale_ctx := sws_getContext(codec_ctx.width, codec_ctx.height, codec_ctx.pix_fmt, codec_ctx.width, codec_ctx.height, AV_PIX_FMT_BGRA,
      SWS_BICUBIC, nil, nil, nil);
    if not Assigned(scale_ctx) then
      raise Exception.Create('Could not create swscale context');

    if frame_index > 0 then
    begin
      ret := av_seek_frame(fmt_ctx, video_stream_idx, frame_index, AVSEEK_FLAG_FRAME or AVSEEK_FLAG_BACKWARD);
      if ret < 0 then
        raise Exception.CreateFmt('Error seeking to frame %d', [frame_index]);
      avcodec_flush_buffers(codec_ctx);
    end;

    pkt := av_packet_alloc();
    if not Assigned(pkt) then
      raise Exception.Create('Could not allocate packet');

    while av_read_frame(fmt_ctx, pkt) >= 0 do
    begin
      try
        if pkt.stream_index <> video_stream_idx then
          Continue;

        ret := avcodec_send_packet(codec_ctx, pkt);
        if ret < 0 then
          raise Exception.CreateFmt('Error submitting packet (%s)', [string(av_err2str(ret))]);

        while True do
        begin
          ret := avcodec_receive_frame(codec_ctx, frame);
          if ret = AVERROR_EAGAIN then
            Break;
          if ret = AVERROR_EOF then
            Break;
          if ret < 0 then
            raise Exception.CreateFmt('Error decoding video frame (%s)', [string(av_err2str(ret))]);

          sws_scale(scale_ctx, @frame.data, @frame.linesize, 0, codec_ctx.height, @frame_rgba.data, @frame_rgba.linesize);

          frame_bitmap := TBitmap.Create;
          try
            SaveToBitmap(frame_rgba, codec_ctx.width, codec_ctx.height, frame_bitmap);
            SaveFrameImage(save_filename, frame_bitmap);
            frame_saved := True;
          finally
            frame_bitmap.Free;
          end;

          av_frame_unref(frame);
          Break;
        end;
      finally
        av_packet_unref(pkt);
      end;

      if frame_saved then
        Break;
    end;

    if not frame_saved then
      raise Exception.CreateFmt('Could not decode frame %d', [frame_index]);

    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;

  sws_freeContext(scale_ctx);
  av_packet_free(pkt);
  av_freep(@frame_rgba.data[0]);
  av_frame_free(frame_rgba);
  av_frame_free(frame);
  avcodec_free_context(codec_ctx);
  avformat_close_input(fmt_ctx);
end.
