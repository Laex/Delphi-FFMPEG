(*
  * Copyright (c) 2010 Nicolas George
  * Copyright (c) 2011 Stefano Sabatini
  * Copyright (c) 2012 Clément Boesch
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  * THE SOFTWARE.
 *)

(* *
  * @file
  * API example for audio decoding and filtering
  * @example filtering_audio.c
 *)

program filtering_audio;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$MINENUMSIZE 4} (* use 4-byte enums *)

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libswresample,
  libswscale;

const
  filter_descr = 'aresample=8000,aformat=sample_fmts=s16:channel_layouts=mono';
  player = 'ffplay -f s16le -ar 8000 -ac 1 -';

var
  fmt_ctx: PAVFormatContext;
  dec_ctx: PAVCodecContext;
  buffersink_ctx: PAVFilterContext;
  buffersrc_ctx: PAVFilterContext;
  filter_graph: PAVFilterGraph;
  audio_stream_index: Integer = -1;

function snprintf(buf: PAnsiChar; size: Cardinal; const fmt: PAnsiChar): Integer;
cdecl varargs;
external 'msvcrt' name '_snprintf';

function open_input_file(const filename: string): Integer;
var
  ret: Integer;
  avdec: PAVCodec;
begin
  ret := avformat_open_input(fmt_ctx, PAnsiChar(AnsiString(filename)), nil, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot open input file'#10);
    Result := ret;
    Exit;
  end;

  ret := avformat_find_stream_info(fmt_ctx, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot find stream information'#10);
    Result := ret;
    Exit;
  end;

  (* select the audio stream *)
  ret := av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, avdec, 0);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot find an audio stream in the input file'#10);
    Result := ret;
    Exit;
  end;
  audio_stream_index := ret;

  (* create decoding context *)
  dec_ctx := avcodec_alloc_context3(avdec);
  if not Assigned(dec_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    Exit;
  end;
  avcodec_parameters_to_context(dec_ctx, fmt_ctx.streams[audio_stream_index].codecpar);

  (* init the audio decoder *)
  ret := avcodec_open2(dec_ctx, avdec, nil);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot open audio decoder'#10);
    Result := ret;
    Exit;
  end;

  Result := 0;
end;

function init_filters(const filters_descr: PAnsiChar): Integer;
var
  args: array [0 .. 512 - 1] of AnsiChar;
  ch_layout_str: array [0 .. 63] of AnsiChar;
  ret: Integer;
  abuffersrc: PAVFilter;
  abuffersink: PAVFilter;
  outputs: PAVFilterInOut;
  inputs: PAVFilterInOut;
  outlink: PAVFilterLink;
  time_base: AVRational;
label
  the_end;
begin
  abuffersrc := avfilter_get_by_name('abuffer');
  abuffersink := avfilter_get_by_name('abuffersink');
  outputs := avfilter_inout_alloc();
  inputs := avfilter_inout_alloc();
  time_base := fmt_ctx.streams[audio_stream_index].time_base;

  filter_graph := avfilter_graph_alloc();
  if not Assigned(outputs) or not Assigned(inputs) or not Assigned(filter_graph) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  (* buffer audio source: the decoded frames from the decoder will be inserted here. *)
  av_channel_layout_describe(@dec_ctx.ch_layout, @ch_layout_str[0], SizeOf(ch_layout_str));
  snprintf(@args[0], SizeOf(args), 'time_base=%d/%d:sample_rate=%d:sample_fmt=%s:channel_layout=%s',
    time_base.num, time_base.den, dec_ctx.sample_rate,
    av_get_sample_fmt_name(dec_ctx.sample_fmt), @ch_layout_str[0]);
  ret := avfilter_graph_create_filter(buffersrc_ctx, abuffersrc, 'in', @args[0], nil, filter_graph);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot create audio buffer source'#10);
    goto the_end;
  end;

  (* buffer audio sink: output constraints at filter creation (FFmpeg 8.x) *)
  ret := avfilter_graph_create_filter(buffersink_ctx, abuffersink, 'out',
    'sample_formats=s16:ch_layouts=mono:samplerates=8000', nil, filter_graph);
  if ret < 0 then
  begin
    av_log(nil, AV_LOG_ERROR, 'Cannot create audio buffer sink'#10);
    goto the_end;
  end;

  outputs.name := av_strdup('in');
  outputs.filter_ctx := buffersrc_ctx;
  outputs.pad_idx := 0;
  outputs.next := nil;

  inputs.name := av_strdup('out');
  inputs.filter_ctx := buffersink_ctx;
  inputs.pad_idx := 0;
  inputs.next := nil;

  ret := avfilter_graph_parse_ptr(filter_graph, filters_descr, inputs, outputs, nil);
  if ret < 0 then
    goto the_end;

  ret := avfilter_graph_config(filter_graph, nil);
  if ret < 0 then
    goto the_end;

  outlink := buffersink_ctx.inputs^;
  av_channel_layout_describe(@outlink.ch_layout, @ch_layout_str[0], SizeOf(ch_layout_str));
  av_log(nil, AV_LOG_INFO, 'Output: srate:%dHz fmt:%s chlayout:%s'#10, outlink.sample_rate,
    av_x_if_null(av_get_sample_fmt_name(AVSampleFormat(outlink.format)), PAnsiChar('?')), @ch_layout_str[0]);

the_end:
  avfilter_inout_free(inputs);
  avfilter_inout_free(outputs);

  Result := ret;
end;

procedure print_frame(const frame: PAVFrame);
var
  n: Integer;
  p: PSmallInt;
  p_end: PSmallInt;
begin
  n := frame.nb_samples * frame.ch_layout.nb_channels;
  p := PSmallInt(frame.data[0]);
  p_end := p;
  Inc(p_end, n);

  while Integer(p) < Integer(p_end) do
  begin
    Write(AnsiChar(p^ and $FF));
    Write(AnsiChar((p^ shr 8) and $FF));
    Inc(p);
  end;
end;

function main(): Integer;
var
  ret: Integer;
  packet: AVPacket;
  frame: PAVFrame;
  filt_frame: PAVFrame;
label
  the_end;
begin
  frame := av_frame_alloc();
  filt_frame := av_frame_alloc();
  if not Assigned(frame) or not Assigned(filt_frame) then
  begin
    Writeln(ErrOutput, 'Could not allocate frame');
    Result := 1;
    Exit;
  end;
  if ParamCount <> 1 then
  begin
    Writeln(ErrOutput, format('Usage: %s file | %s', [ExtractFileName(ParamStr(0)), player]));
    Result := 1;
    Exit;
  end;

  ret := open_input_file(ParamStr(1));
  if ret < 0 then
    goto the_end;
  ret := init_filters(filter_descr);
  if ret < 0 then
    goto the_end;

  (* read all packets *)
  while True do
  begin
    ret := av_read_frame(fmt_ctx, @packet);
    if ret < 0 then
      Break;

    if packet.stream_index = audio_stream_index then
    begin
      ret := avcodec_send_packet(dec_ctx, @packet);
      if ret < 0 then
      begin
        av_log(nil, AV_LOG_ERROR, 'Error while sending a packet to the decoder'#10);
        Break;
      end;

      while ret >= 0 do
      begin
        ret := avcodec_receive_frame(dec_ctx, frame);
        if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
          Break
        else if ret < 0 then
        begin
          av_log(nil, AV_LOG_ERROR, 'Error while receiving a frame from the decoder'#10);
          goto the_end;
        end;

        if ret >= 0 then
        begin
          if av_buffersrc_add_frame_flags(buffersrc_ctx, frame, AV_BUFFERSRC_FLAG_KEEP_REF) < 0 then
          begin
            av_log(nil, AV_LOG_ERROR, 'Error while feeding the audio filtergraph'#10);
            Break;
          end;

          while True do
          begin
            ret := av_buffersink_get_frame(buffersink_ctx, filt_frame);
            if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
              Break;
            if ret < 0 then
              goto the_end;
            print_frame(filt_frame);
            av_frame_unref(filt_frame);
          end;
          av_frame_unref(frame);
        end;
      end;
    end;
    av_packet_unref(@packet);
  end;
the_end:
  avfilter_graph_free(filter_graph);
  avcodec_free_context(dec_ctx);
  avformat_close_input(fmt_ctx);
  av_frame_free(frame);
  av_frame_free(filt_frame);

  if (ret < 0) and (ret <> AVERROR_EOF) then
  begin
    Writeln(ErrOutput, format('Error occurred: %s', [string(av_err2str(ret))]));
    Result := 1;
    Exit;
  end;

  Result := 0;
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
