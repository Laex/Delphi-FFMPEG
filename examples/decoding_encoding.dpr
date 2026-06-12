(*
  * Copyright (c) 2001 Fabrice Bellard
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
  * libavcodec API use example.
  *
  * @example decoding_encoding.c
  * Note that libavcodec only handles codecs (mpeg, mpeg4, etc...),
  * not file formats (avi, vob, mp4, mov, mkv, mxf, flv, mpegts, mpegps, etc...). See library 'libavformat' for the
  * format handling
 *)

program decoding_encoding;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
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
  cResourceResultDefault = '..\..\result\';

Const
  INBUF_SIZE = 4096;
  AUDIO_INBUF_SIZE = 20480;
  AUDIO_REFILL_THRESH = 4096;

function check_sample_fmt(const codec: pAVCodec; const sample_fmt: AVSampleFormat): Boolean;
Var
  p: pAVSampleFormat;
begin
  p := codec^.sample_fmts;
  while (p^ <> AV_SAMPLE_FMT_NONE) do
  begin
    if (p^ = sample_fmt) then
      Exit(True);
    inc(p);
  end;
  Result := False;
end;

function select_sample_rate(const codec: pAVCodec): Integer;
Var
  p: pInt;
  best_samplerate: Integer;
begin
  best_samplerate := 0;
  if not Assigned(codec^.supported_samplerates) then
    Exit(44100);
  p := codec^.supported_samplerates;
  while (p^ <> 0) do
  begin
    best_samplerate := MAX(p^, best_samplerate);
    inc(p);
  end;
  Result := best_samplerate;
end;

procedure encode(avctx: pAVCodecContext; frame: pAVFrame; pkt: pAVPacket; var f: File);
var
  ret: Integer;
begin
  ret := avcodec_send_frame(avctx, frame);
  if ret < 0 then
  begin
    WriteLn('Error sending the frame to the encoder');
    Halt(1);
  end;

  while ret >= 0 do
  begin
    ret := avcodec_receive_packet(avctx, pkt);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
      Exit
    else if ret < 0 then
    begin
      WriteLn('Error during encoding');
      Halt(1);
    end;

    BlockWrite(f, pkt^.data^, pkt^.size);
    av_packet_unref(pkt);
  end;
end;

procedure audio_encode_example(const filename: String);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  frame: pAVFrame;
  pkt: pAVPacket;
  i, j, k, ret: Integer;
  buffer_size: Integer;
  f: File;
  samples: ^Int16;
  t, tincr: Single;
begin
  c := nil;
  WriteLn('Encode audio file ', filename);
  codec := avcodec_find_encoder(AV_CODEC_ID_MP2);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate audio codec context');
    Exit;
  end;
  c^.bit_rate := 64000;
  c^.sample_fmt := AV_SAMPLE_FMT_S16;
  if not check_sample_fmt(codec, c^.sample_fmt) then
  begin
    WriteLn('Encoder does not support sample format ', av_get_sample_fmt_name(c^.sample_fmt));
    Exit;
  end;
  c^.sample_rate := select_sample_rate(codec);

  if Assigned(codec^.ch_layouts) then
    av_channel_layout_copy(c^.ch_layout, codec^.ch_layouts)
  else
    av_channel_layout_default(c^.ch_layout, 2);

  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Rewrite(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;

  pkt := av_packet_alloc();
  if not Assigned(pkt) then
  begin
    WriteLn('Could not allocate packet');
    Exit;
  end;

  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    WriteLn('Could not allocate audio frame');
    Exit;
  end;
  frame^.nb_samples := c^.frame_size;
  frame^.format := Integer(c^.sample_fmt);
  av_channel_layout_copy(frame^.ch_layout, @c^.ch_layout);

  buffer_size := av_samples_get_buffer_size(nil, c^.ch_layout.nb_channels, c^.frame_size, c^.sample_fmt, 0);
  if (buffer_size < 0) then
  begin
    WriteLn('Could not get sample buffer size');
    Exit;
  end;
  samples := av_malloc(buffer_size);
  if not Assigned(samples) then
  begin
    WriteLn('Could not allocate samples buffer');
    Exit;
  end;
  ret := avcodec_fill_audio_frame(frame, c^.ch_layout.nb_channels, c^.sample_fmt, pByte(samples), buffer_size, 0);
  if (ret < 0) then
  begin
    WriteLn('Could not setup audio frame');
    Exit;
  end;
  t := 0;
  tincr := 2 * M_PI * 440.0 / c^.sample_rate;
  for i := 0 to 199 do
  begin
    for j := 0 to c^.frame_size - 1 do
    begin
      samples[2 * j] := Trunc((sin(t) * 10000));
      for k := 1 to c^.ch_layout.nb_channels - 1 do
        samples[2 * j + k] := samples[2 * j];
      t := t + tincr;
    end;
    encode(c, frame, pkt, f);
  end;
  (* flush the encoder *)
  encode(c, nil, pkt, f);

  Close(f);
  av_free(samples);
  av_frame_free(frame);
  av_packet_free(pkt);
  avcodec_free_context(c);
end;

procedure decode_audio(dec_ctx: pAVCodecContext; pkt: pAVPacket; frame: pAVFrame; var outfile: File);
var
  ret, data_size: Integer;
begin
  ret := avcodec_send_packet(dec_ctx, pkt);
  if ret < 0 then
  begin
    WriteLn('Error submitting the packet to the decoder');
    Halt(1);
  end;

  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(dec_ctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
      Exit
    else if ret < 0 then
    begin
      WriteLn('Error during decoding');
      Halt(1);
    end;

    data_size := av_samples_get_buffer_size(nil, dec_ctx^.ch_layout.nb_channels, frame^.nb_samples, dec_ctx^.sample_fmt, 1);
    if data_size < 0 then
    begin
      WriteLn('Failed to calculate data size');
      Halt(1);
    end;
    BlockWrite(outfile, frame^.data[0]^, data_size);
  end;
end;

procedure audio_decode_example(const outfilename: String; const filename: String);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  f, outfile: File;
  inbuf: array [0 .. AUDIO_INBUF_SIZE + AV_INPUT_BUFFER_PADDING_SIZE - 1] of byte;
  avpkt: pAVPacket;
  decoded_frame: pAVFrame;
begin
  c := nil;
  decoded_frame := nil;
  avpkt := av_packet_alloc();
  if not Assigned(avpkt) then
  begin
    WriteLn('Could not allocate packet');
    Exit;
  end;

  WriteLn('Decode audio file ', filename, ' to ', outfilename);
  codec := avcodec_find_decoder(AV_CODEC_ID_MP2);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate audio codec context');
    Exit;
  end;
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Reset(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;
  AssignFile(outfile, outfilename);
  try
    Rewrite(outfile, 1);
  except
    avcodec_free_context(c);
    Exit;
  end;

  decoded_frame := av_frame_alloc();
  if not Assigned(decoded_frame) then
  begin
    WriteLn('Could not allocate audio frame');
    Exit;
  end;

  BlockRead(f, inbuf, AUDIO_INBUF_SIZE, avpkt^.size);
  while (avpkt^.size > 0) do
  begin
    avpkt^.data := @inbuf;
    decode_audio(c, avpkt, decoded_frame, outfile);
    BlockRead(f, inbuf, AUDIO_INBUF_SIZE, avpkt^.size);
  end;
  
  (* flush the decoder *)
  avpkt^.data := nil;
  avpkt^.size := 0;
  decode_audio(c, avpkt, decoded_frame, outfile);

  Close(outfile);
  Close(f);
  av_frame_free(decoded_frame);
  av_packet_free(avpkt);
  avcodec_free_context(c);
end;

procedure video_encode_example(const filename: String; codec_id: AVCodecID);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  i, ret, x, y: Integer;
  f: File;
  frame: pAVFrame;
  pkt: pAVPacket;
  endcode: array [0 .. 3] of byte;
begin
  c := nil;
  endcode[0] := 0;
  endcode[1] := 0;
  endcode[2] := 1;
  endcode[3] := $B7;
  WriteLn('Encode video file ', filename);
  codec := avcodec_find_encoder(codec_id);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate video codec context');
    Exit;
  end;
  c^.bit_rate := 400000;
  c^.width := 352;
  c^.height := 288;
  c^.time_base.num := 1;
  c^.time_base.den := 25;
  c^.gop_size := 10;
  c^.max_b_frames := 1;
  c^.pix_fmt := AV_PIX_FMT_YUV420P;
  if (codec_id = AV_CODEC_ID_H264) then
    av_opt_set(c^.priv_data, 'preset', 'slow', 0);
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Rewrite(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;
  pkt := av_packet_alloc();
  if not Assigned(pkt) then
  begin
    WriteLn('Could not allocate packet');
    Exit;
  end;
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    WriteLn('Could not allocate video frame');
    Exit;
  end;
  frame^.format := Integer(c^.pix_fmt);
  frame^.width := c^.width;
  frame^.height := c^.height;
  ret := av_image_alloc(@frame^.data, @frame^.linesize, c^.width, c^.height, c^.pix_fmt, 32);
  if (ret < 0) then
  begin
    WriteLn('Could not allocate raw picture buffer');
    Exit;
  end;
  for i := 0 to 24 do
  begin
    for y := 0 to c^.height - 1 do
      for x := 0 to c^.width - 1 do
        frame^.data[0][y * frame^.linesize[0] + x] := x + y + i * 3;
    for y := 0 to (c^.height div 2) - 1 do
      for x := 0 to (c^.width div 2) - 1 do
      begin
        frame^.data[1][y * frame^.linesize[1] + x] := 128 + y + i * 2;
        frame^.data[2][y * frame^.linesize[2] + x] := 64 + x + i * 5;
      end;
    frame^.pts := i;
    encode(c, frame, pkt, f);
  end;
  (* flush the encoder *)
  encode(c, nil, pkt, f);
  BlockWrite(f, endcode, sizeof(endcode));
  Close(f);
  av_freep(@frame^.data[0]);
  av_frame_free(frame);
  av_packet_free(pkt);
  avcodec_free_context(c);
end;

procedure pgm_save(buf: pByte; wrap, xsize, ysize: Integer; filename: String);
Var
  f: TextFile;
  fb: File;
  i: Integer;
begin
  AssignFile(f, filename);
  Rewrite(f);
  WriteLn(f, format('P5' + #13#10 + '%d %d' + #13#10 + '%d', [xsize, ysize, 255]));
  Close(f);
  AssignFile(fb, filename);
  Reset(fb, 1);
  Seek(fb, FileSize(fb));
  for i := 0 to ysize - 1 do
    BlockWrite(fb, buf[i * wrap], xsize);
  Close(fb);
end;

procedure decode_video(dec_ctx: pAVCodecContext; frame: pAVFrame; Var frame_count: Integer; pkt: pAVPacket; const outfilename: String);
var
  ret: Integer;
begin
  ret := avcodec_send_packet(dec_ctx, pkt);
  if ret < 0 then
  begin
    WriteLn('Error submitting the packet to the decoder');
    Halt(1);
  end;

  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(dec_ctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
      Exit
    else if ret < 0 then
    begin
      WriteLn('Error during decoding');
      Halt(1);
    end;

    WriteLn(format('Saving frame %3d', [frame_count]));
    pgm_save(frame^.data[0], frame^.linesize[0], dec_ctx^.width, dec_ctx^.height, format(outfilename, [frame_count]));
    inc(frame_count);
  end;
end;

procedure video_decode_example(const outfilename: String; const filename: String);
Var
  codec: pAVCodec;
  c: pAVCodecContext;
  frame_count: Integer;
  f: File;
  frame: pAVFrame;
  inbuf: array [0 .. INBUF_SIZE + AV_INPUT_BUFFER_PADDING_SIZE - 1] of byte;
  avpkt: pAVPacket;
begin
  c := nil;
  avpkt := av_packet_alloc();
  if not Assigned(avpkt) then
  begin
    WriteLn('Could not allocate packet');
    Exit;
  end;
  FillChar(inbuf[INBUF_SIZE], AV_INPUT_BUFFER_PADDING_SIZE, 0);
  WriteLn(format('Decode video file %s to %s', [filename, outfilename]));
  codec := avcodec_find_decoder(AV_CODEC_ID_MPEG1VIDEO);
  if not Assigned(codec) then
  begin
    WriteLn('Codec not found');
    Exit;
  end;
  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    WriteLn('Could not allocate video codec context');
    Exit;
  end;
  if (avcodec_open2(c, codec, nil) < 0) then
  begin
    WriteLn('Could not open codec');
    Exit;
  end;
  AssignFile(f, filename);
  try
    Reset(f, 1);
  except
    WriteLn('Could not open ', filename);
    Exit;
  end;
  frame := av_frame_alloc();
  if not Assigned(frame) then
  begin
    WriteLn('Could not allocate video frame');
    Exit;
  end;
  frame_count := 0;
  While True do
  begin
    BlockRead(f, inbuf, INBUF_SIZE, avpkt^.size);
    if (avpkt^.size = 0) then
      break;
    avpkt^.data := @inbuf;
    decode_video(c, frame, frame_count, avpkt, outfilename);
  end;
  (* flush the decoder *)
  avpkt^.data := nil;
  avpkt^.size := 0;
  decode_video(c, frame, frame_count, avpkt, outfilename);
  Close(f);
  av_frame_free(frame);
  av_packet_free(avpkt);
  avcodec_free_context(c);
end;

procedure avlog(ptr: Pointer; level: Integer; fmt: PAnsiChar; vl: pva_list); cdecl;
Var
  line: array [0 .. 1023] of AnsiChar;
  print_prefix: Integer;
  A: AnsiString;
begin
  print_prefix := 1;
  av_log_format_line(ptr, level, fmt, vl, @line, sizeof(line), print_prefix);
  A := Trim(AnsiString(line));
  WriteLn(A);
end;

Var
  output_type: String;

begin
  try
    av_log_set_callback(avlog);
    if ParamCount > 0 then
      output_type := ParamStr(1)
    else
    begin
      output_type := 'mp2';
    end;
    if (SameText(output_type, 'h264')) then
      video_encode_example(cResourceResultDefault + 'test.h264', AV_CODEC_ID_H264)
    else if (SameText(output_type, 'mp2')) then
    begin
      audio_encode_example(cResourceResultDefault + 'test.mp2');
      audio_decode_example(cResourceResultDefault + 'test.pcm', cResourceResultDefault + 'test.mp2');
    end
    else if (SameText(output_type, 'mpg')) then
    begin
      video_encode_example(cResourceResultDefault + 'test.mpg', AV_CODEC_ID_MPEG1VIDEO);
      video_decode_example(cResourceResultDefault + 'test%02d.pgm', cResourceResultDefault + 'test.mpg');
    end
    else
    begin
      WriteLn(format('Invalid output type "%s", choose between "h264", "mp2", or "mpg"', [output_type]));
      Halt;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
