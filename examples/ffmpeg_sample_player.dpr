program ffmpeg_sample_player;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  SDL,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libswresample,
  libswscale;

var
  err: Integer;
  filename: AnsiString;
  format_context: pAVFormatContext = nil;
  video_stream: Integer;
  codec_context: pAVCodecContext = nil;
  codec: pAVCodec;
  screen: pSDL_Surface;
  bmp: pSDL_Overlay;
  img_convert_context: pSwsContext;
  frame: pAVFrame;
  packet: PAVPacket = nil;
  pict_data: array [0 .. 3] of PByte;
  pict_linesize: array [0 .. 3] of Integer;
  rect: TSDL_Rect;
  event: TSDL_Event;

const
  cResourceMedia = '..\..\resource\';
  std_filename   = cResourceMedia + '768x576.avi';

begin
  try
    if ParamCount < 1 then
      filename := std_filename
    else
      filename := AnsiString(ParamStr(1));

    avformat_network_init();

    err := SDL_Init(SDL_INIT_VIDEO);
    if err < 0 then
    begin
      WriteLn(Format('Unable to init SDL: %s', [SDL_GetError()]));
      Halt(1);
    end;

    err := avformat_open_input(format_context, PAnsiChar(filename), nil, nil);
    if err < 0 then
    begin
      WriteLn('ffmpeg: Unable to open input file');
      Halt(1);
    end;

    err := avformat_find_stream_info(format_context, nil);
    if err < 0 then
    begin
      WriteLn('ffmpeg: Unable to find stream info');
      Halt(1);
    end;

    av_dump_format(format_context, 0, PAnsiChar(filename), 0);

    err := av_find_best_stream(format_context, AVMEDIA_TYPE_VIDEO, -1, -1, codec, 0);
    if err < 0 then
    begin
      WriteLn('ffmpeg: Unable to find video stream');
      Halt(1);
    end;
    video_stream := err;

    codec := avcodec_find_decoder(format_context^.streams[video_stream]^.codecpar^.codec_id);
    if not Assigned(codec) then
    begin
      WriteLn('ffmpeg: Unable to find codec');
      Halt(1);
    end;

    codec_context := avcodec_alloc_context3(codec);
    if not Assigned(codec_context) then
    begin
      WriteLn('ffmpeg: Unable to allocate codec context');
      Halt(1);
    end;

    err := avcodec_parameters_to_context(codec_context, format_context^.streams[video_stream]^.codecpar);
    if err < 0 then
    begin
      WriteLn('ffmpeg: Unable to copy codec parameters');
      Halt(1);
    end;

    err := avcodec_open2(codec_context, codec, nil);
    if err < 0 then
    begin
      WriteLn('ffmpeg: Unable to open codec');
      Halt(1);
    end;

    screen := SDL_SetVideoMode(codec_context^.width, codec_context^.height, 0, 0);
    if screen = nil then
    begin
      WriteLn('Couldn''t set video mode');
      Halt(1);
    end;

    bmp := SDL_CreateYUVOverlay(codec_context^.width, codec_context^.height, SDL_YV12_OVERLAY, screen);

    img_convert_context := sws_getCachedContext(nil, codec_context^.width, codec_context^.height, codec_context^.pix_fmt, codec_context^.width,
      codec_context^.height, AV_PIX_FMT_YUV420P, SWS_BICUBIC, nil, nil, nil);

    if img_convert_context = nil then
    begin
      WriteLn('Cannot initialize the conversion context');
      Halt(1);
    end;

    frame := av_frame_alloc();
    packet := av_packet_alloc();
    if not Assigned(packet) then
    begin
      WriteLn('ffmpeg: Unable to allocate packet');
      Halt(1);
    end;

    while av_read_frame(format_context, packet) >= 0 do
    begin
      try
        if packet.stream_index <> video_stream then
          Continue;

        err := avcodec_send_packet(codec_context, packet);
        if err < 0 then
          Continue;

        while err >= 0 do
        begin
          err := avcodec_receive_frame(codec_context, frame);
          if (err = AVERROR_EAGAIN) or (err = AVERROR_EOF) then
            Break;
          if err < 0 then
            Break;

          SDL_LockYUVOverlay(bmp);

          pict_data[0] := PByte(bmp^.pixels[0]);
          pict_data[1] := PByte(bmp^.pixels[2]);
          pict_data[2] := PByte(bmp^.pixels[1]);

          pict_linesize[0] := bmp^.pitches[0];
          pict_linesize[1] := bmp^.pitches[2];
          pict_linesize[2] := bmp^.pitches[1];

          sws_scale(img_convert_context, @frame^.data, @frame^.linesize, 0, codec_context^.height, @pict_data, @pict_linesize);

          SDL_UnlockYUVOverlay(bmp);

          rect.x := 0;
          rect.y := 0;
          rect.w := codec_context^.width;
          rect.h := codec_context^.height;
          SDL_DisplayYUVOverlay(bmp, @rect);

          av_frame_unref(frame);
        end;
      finally
        av_packet_unref(packet);
      end;

      if SDL_PollEvent(@event) <> 0 then
        if event.type_ = SDL_QUITEV then
          Break;
    end;

    sws_freeContext(img_convert_context);
    av_frame_free(frame);
    av_packet_free(packet);
    avcodec_free_context(codec_context);
    avformat_close_input(format_context);
    avformat_network_deinit;

    SDL_Quit();
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
