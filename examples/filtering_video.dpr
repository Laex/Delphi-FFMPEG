program filtering_video;

{$APPTYPE CONSOLE}
{$R *.res}
{$POINTERMATH ON}

uses
  Winapi.Windows,
  System.SysUtils,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libswresample,
  libswscale;

const
  cResourceMedia = '..\..\resource\';
  std_filename = cResourceMedia + '768x576.avi';

Var
  filter_descr: pAnsiChar = 'scale=78:24';
  fmt_ctx: pAVFormatContext = nil;
  dec_ctx: pAVCodecContext = nil;
  video_stream: pAVStream = nil;
  buffersink_ctx: pAVFilterContext = nil;
  buffersrc_ctx: pAVFilterContext = nil;
  filter_graph: pAVFilterGraph = nil;
  video_stream_index: Integer = -1;
  last_pts: int64_t = AV_NOPTS_VALUE;

function open_input_file(const filename: pAnsiChar): Integer;
Var
  ret: Integer;
  dec: pAVCodec;
  stream: pAVStream;
begin
  ret := avformat_open_input(fmt_ctx, filename, nil, nil);
  if (ret < 0) then
    Exit(ret);

  ret := avformat_find_stream_info(fmt_ctx, nil);
  if (ret < 0) then
    Exit(ret);

  (* select the video stream *)
  ret := av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, dec, 0);
  if (ret < 0) then
    Exit(ret);
  video_stream_index := ret;
  stream := fmt_ctx^.streams[video_stream_index];
  video_stream := stream;

  dec_ctx := avcodec_alloc_context3(dec);
  if not Assigned(dec_ctx) then
    Exit(AVERROR_ENOMEM);

  ret := avcodec_parameters_to_context(dec_ctx, stream^.codecpar);
  if (ret < 0) then
    Exit(ret);

  (* init the video decoder *)
  ret := avcodec_open2(dec_ctx, dec, nil);
  if (ret < 0) then
    Exit(ret);

  Result := 0;
end;

function init_filters(const filters_descr: pAnsiChar): Integer;
Var
  args: AnsiString;
  ret: Integer;
  buffersrc: pAVFilter;
  buffersink: pAVFilter;
  outputs: pAVFilterInOut;
  inputs: pAVFilterInOut;
begin
  buffersrc := avfilter_get_by_name('buffer');
  buffersink := avfilter_get_by_name('buffersink');
  outputs := avfilter_inout_alloc();
  inputs := avfilter_inout_alloc();
  filter_graph := avfilter_graph_alloc();

  (* buffer video source: time_base comes from the stream, not the codec context *)
  args := Format('video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d', [
    dec_ctx^.width,
    dec_ctx^.height,
    Integer(dec_ctx^.pix_fmt),
    video_stream^.time_base.num,
    video_stream^.time_base.den,
    video_stream^.sample_aspect_ratio.num,
    video_stream^.sample_aspect_ratio.den
    ]);

  ret := avfilter_graph_create_filter(buffersrc_ctx, buffersrc, 'in', pAnsiChar(args), nil, filter_graph);
  if (ret < 0) then
    Exit(ret);

  (* buffer video sink: output format must be set at filter creation (FFmpeg 8.x) *)
  ret := avfilter_graph_create_filter(buffersink_ctx, buffersink, 'out', 'pixel_formats=gray8', nil, filter_graph);
  if (ret < 0) then
    Exit(ret);

  (* Endpoints for the filter graph. *)
  outputs^.name := av_strdup('in');
  outputs^.filter_ctx := buffersrc_ctx;
  outputs^.pad_idx := 0;
  outputs^.next := nil;

  inputs^.name := av_strdup('out');
  inputs^.filter_ctx := buffersink_ctx;
  inputs^.pad_idx := 0;
  inputs^.next := nil;

  ret := avfilter_graph_parse_ptr(filter_graph, filters_descr, inputs, outputs, nil);
  if (ret < 0) then
    Exit(ret);

  ret := avfilter_graph_config(filter_graph, nil);
  if (ret < 0) then
    Exit(ret);
  Result := 0;
end;

procedure display_frame(const vframe: pAVFrame; time_base: AVRational);
Const
  ds: array[0..4] of char = ' .-+#';
Var
  x, y: Integer;
  p0, p: pByte;
  hConsole: THandle;
  coordScreen: TCOORD;
begin
  if (vframe^.pts <> AV_NOPTS_VALUE) then
  begin
    last_pts := vframe^.pts;
  end;

  (* Trivial ASCII grayscale display. *)
  p0 := vframe^.data[0];
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  FillChar(coordScreen, SizeOf(coordScreen), 0);
  SetConsoleCursorPosition(hConsole, coordScreen);
  for y := 0 to vframe^.height - 1 do
  begin
    p := p0;
    for x := 0 to vframe^.width - 1 do
    begin
      Write(ds[p^ div 52]);
      Inc(p);
    end;
    Writeln;
    p0 := p0 + vframe^.linesize[0];
  end;
end;

procedure decode_and_filter(packet: pAVPacket; vframe, filt_frame: pAVFrame);
var
  ret: Integer;
begin
  ret := avcodec_send_packet(dec_ctx, packet);
  if ret < 0 then
    Exit;

  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(dec_ctx, vframe);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
      Break
    else if ret < 0 then
      Exit;

    (* push the decoded frame into the filtergraph *)
    if (av_buffersrc_add_frame_flags(buffersrc_ctx, vframe, AV_BUFFERSRC_FLAG_KEEP_REF) < 0) then
      Break;

    (* pull filtered frames from the filtergraph *)
    while True do
    begin
      ret := av_buffersink_get_frame(buffersink_ctx, filt_frame);
      if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
        break;
      if (ret < 0) then
        Halt(1);
      display_frame(filt_frame, buffersink_ctx^.inputs[0]^.time_base);
      av_frame_unref(filt_frame);
    end;
    av_frame_unref(vframe);
  end;
end;

Var
  ret: Integer;
  packet: AVPacket;
  vframe: pAVFrame = nil;
  filt_frame: pAVFrame = nil;
  filename: AnsiString;
  buf: array [0 .. 1023] of ansichar;

begin
  try
    vframe := av_frame_alloc();
    filt_frame := av_frame_alloc();

    if (not Assigned(vframe)) or (not Assigned(filt_frame)) then
    begin
      Writeln('Could not allocate frame');
      Halt(1);
    end;
    if (ParamCount < 1) then
      filename := std_filename
    else
      filename := ParamStr(1);

    avformat_network_init;
    try
      ret := open_input_file(pAnsiChar(filename));
      if (ret < 0) then
        Halt(1);
      ret := init_filters(pAnsiChar(filter_descr));
      if (ret < 0) then
        Halt(1);

      (* read all packets *)
      while True do
      begin
        ret := av_read_frame(fmt_ctx, @packet);
        if (ret < 0) then
          break;

        if (packet.stream_index = video_stream_index) then
        begin
          decode_and_filter(@packet, vframe, filt_frame);
        end;
        av_packet_unref(@packet);
      end;
      
      (* flush filter graph *)
      decode_and_filter(nil, vframe, filt_frame);

    finally
      avformat_network_deinit;
      avfilter_graph_free(filter_graph);
      avcodec_free_context(dec_ctx);
      avformat_close_input(fmt_ctx);
      av_frame_free(vframe);
      av_frame_free(filt_frame);

      if (ret < 0)  and (ret <> AVERROR_EOF) then
      begin
        av_strerror(ret, buf, sizeof(buf));
        Writeln('Error occurred: ', buf);
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
