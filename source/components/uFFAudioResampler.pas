unit uFFAudioResampler;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ libswresample wrapper: decoded audio frame -> PCM S16 interleaved. }

interface

uses
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libswresample,
  uFFException,
  uFFFrame;

const
  FFAudioDefaultSampleRate = 44100;
  FFAudioDefaultChannels   = 2;

type
  TFFAudioResampler = class
  private
    FSwr: PSwrContext;
    FOutSampleRate: Integer;
    FOutChannels: Integer;
    FOutFormat: AVSampleFormat;
    FDstBuffer: PByte;
    FDstPlane: array [0 .. 0] of PByte;
    FDstLinesize: Integer;
    FDstCapacitySamples: Integer;
    procedure Release;
    procedure EnsureDstCapacity(ASamples: Integer);
  public
    destructor Destroy; override;

    procedure Configure(const ASrcLayout: PAVChannelLayout; ASrcRate: Integer; ASrcFormat: AVSampleFormat;
      AOutRate: Integer = FFAudioDefaultSampleRate; AOutChannels: Integer = FFAudioDefaultChannels);
    function Convert(ASrc: PAVFrame; out ABuffer: PByte; out AByteCount: Integer): Integer;

    property OutSampleRate: Integer read FOutSampleRate;
    property OutChannels: Integer read FOutChannels;
    property OutSampleFormat: AVSampleFormat read FOutFormat;
  end;

implementation

procedure TFFAudioResampler.Configure(const ASrcLayout: PAVChannelLayout; ASrcRate: Integer;
  ASrcFormat: AVSampleFormat; AOutRate: Integer; AOutChannels: Integer);
var
  DstLayout: AVChannelLayout;
  Ret: Integer;
begin
  if (ASrcLayout = nil) or (ASrcRate <= 0) then
    raise EFFException.Create('TFFAudioResampler.Configure: invalid source parameters');

  Release;

  FOutSampleRate := AOutRate;
  FOutChannels := AOutChannels;
  FOutFormat := AV_SAMPLE_FMT_S16;

  av_channel_layout_default(DstLayout, FOutChannels);

  FSwr := nil;
  Ret := swr_alloc_set_opts2(FSwr, @DstLayout, FOutFormat, FOutSampleRate, ASrcLayout, ASrcFormat, ASrcRate, 0, nil);
  if Ret < 0 then
    raise EFFException.CreateFmt('swr_alloc_set_opts2 failed (%d)', [Ret]);

  Ret := swr_init(FSwr);
  if Ret < 0 then
  begin
    Release;
    raise EFFException.CreateFmt('swr_init failed (%d)', [Ret]);
  end;
end;

function TFFAudioResampler.Convert(ASrc: PAVFrame; out ABuffer: PByte; out AByteCount: Integer): Integer;
var
  DstSamples: Integer;
  Ret: Integer;
begin
  if FSwr = nil then
    raise EFFException.Create('TFFAudioResampler is not configured');
  if ASrc = nil then
    raise EFFException.Create('TFFAudioResampler.Convert: source frame is nil');

  DstSamples := av_rescale_rnd(swr_get_delay(FSwr, ASrc^.sample_rate) + ASrc^.nb_samples,
    FOutSampleRate, ASrc^.sample_rate, AV_ROUND_UP);
  EnsureDstCapacity(DstSamples);

  Ret := swr_convert(FSwr, @FDstPlane, DstSamples, @ASrc^.data, ASrc^.nb_samples);
  if Ret < 0 then
    raise EFFException.CreateFmt('swr_convert failed (%d)', [Ret]);

  Result := Ret;
  AByteCount := av_samples_get_buffer_size(FDstLinesize, FOutChannels, Ret, FOutFormat, 1);
  if AByteCount < 0 then
    raise EFFException.Create('av_samples_get_buffer_size failed');
  ABuffer := FDstBuffer;
end;

destructor TFFAudioResampler.Destroy;
begin
  Release;
  inherited;
end;

procedure TFFAudioResampler.EnsureDstCapacity(ASamples: Integer);
var
  Ret: Integer;
begin
  if ASamples <= FDstCapacitySamples then
    Exit;

  if FDstBuffer <> nil then
    av_freep(@FDstBuffer);

  Ret := av_samples_alloc(FDstBuffer, @FDstLinesize, FOutChannels, ASamples, FOutFormat, 1);
  if Ret < 0 then
    raise EFFException.CreateFmt('av_samples_alloc failed (%d)', [Ret]);
  FDstPlane[0] := FDstBuffer;
  FDstCapacitySamples := ASamples;
end;

procedure TFFAudioResampler.Release;
begin
  if FSwr <> nil then
    swr_free(FSwr);
  FSwr := nil;
  if FDstBuffer <> nil then
    av_freep(@FDstBuffer);
  FDstBuffer := nil;
  FDstPlane[0] := nil;
  FDstCapacitySamples := 0;
end;

end.
