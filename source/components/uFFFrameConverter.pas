unit uFFFrameConverter;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ libswscale wrapper: decoded AVFrame -> BGRA/RGB (platform-neutral buffer). }

interface

uses
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libswscale,
  uFFException,
  uFFFrame,
  uFFDesignTime;

type
  TFFFrameConverter = class
  private
    FSwsCtx: PSwsContext;
    FDstFrame: PAVFrame;
    FDstBuffer: PByte;
    FSrcW: Integer;
    FSrcH: Integer;
    FSrcFormat: AVPixelFormat;
    FDstW: Integer;
    FDstH: Integer;
    FDstFormat: AVPixelFormat;
    FScaleFlags: Integer;
    procedure ReleaseResources;
    procedure AllocDstBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Configure(ASrcW, ASrcH: Integer; ASrcFormat: AVPixelFormat; ADstW: Integer = 0;
      ADstH: Integer = 0; ADstFormat: AVPixelFormat = AV_PIX_FMT_BGRA; AScaleFlags: Integer = SWS_BILINEAR);
    function Convert(ASrc: TFFFrame): PAVFrame; overload;
    function Convert(ASrc: PAVFrame): PAVFrame; overload;

    property DstWidth: Integer read FDstW;
    property DstHeight: Integer read FDstH;
    property DstPixelFormat: AVPixelFormat read FDstFormat;
  end;

implementation

procedure TFFFrameConverter.AllocDstBuffer;
var
  Ret: Integer;
begin
  FDstFrame := av_frame_alloc();
  if FDstFrame = nil then
    raise EFFException.Create('av_frame_alloc failed for converter output');

  Ret := av_image_alloc(@FDstFrame^.data[0], @FDstFrame^.linesize[0], FDstW, FDstH, FDstFormat, 1);
  if Ret < 0 then
    raise EFFException.CreateFmt('av_image_alloc failed (%d)', [Ret]);
  FDstBuffer := FDstFrame^.data[0];
  FDstFrame^.width := FDstW;
  FDstFrame^.height := FDstH;
  FDstFrame^.format := Ord(FDstFormat);
end;

procedure TFFFrameConverter.Configure(ASrcW, ASrcH: Integer; ASrcFormat: AVPixelFormat; ADstW: Integer;
  ADstH: Integer; ADstFormat: AVPixelFormat; AScaleFlags: Integer);
begin
  if ASrcW <= 0 then
    raise EFFException.Create('TFFFrameConverter.Configure: invalid source width');
  if ASrcH <= 0 then
    raise EFFException.Create('TFFFrameConverter.Configure: invalid source height');

  if ADstW <= 0 then
    ADstW := ASrcW;
  if ADstH <= 0 then
    ADstH := ASrcH;

  if (FSwsCtx <> nil) and (FSrcW = ASrcW) and (FSrcH = ASrcH) and (FSrcFormat = ASrcFormat) and
    (FDstW = ADstW) and (FDstH = ADstH) and (FDstFormat = ADstFormat) and (FScaleFlags = AScaleFlags) then
    Exit;

  ReleaseResources;

  FSrcW := ASrcW;
  FSrcH := ASrcH;
  FSrcFormat := ASrcFormat;
  FDstW := ADstW;
  FDstH := ADstH;
  FDstFormat := ADstFormat;
  FScaleFlags := AScaleFlags;

  FSwsCtx := sws_getContext(FSrcW, FSrcH, FSrcFormat, FDstW, FDstH, FDstFormat, FScaleFlags, nil, nil, nil);
  if FSwsCtx = nil then
    raise EFFException.Create('sws_getContext failed');

  AllocDstBuffer;
end;

constructor TFFFrameConverter.Create;
begin
  inherited Create;
  FScaleFlags := SWS_BILINEAR;
  FDstFormat := AV_PIX_FMT_BGRA;
end;

function TFFFrameConverter.Convert(ASrc: PAVFrame): PAVFrame;
var
  Ret: Integer;
  SrcFormat: AVPixelFormat;
begin
  if ASrc = nil then
    raise EFFException.Create('TFFFrameConverter.Convert: source frame is nil');
  if (ASrc^.width <= 0) or (ASrc^.height <= 0) then
    raise EFFException.Create('TFFFrameConverter.Convert: source frame has no size');

  SrcFormat := AVPixelFormat(ASrc^.format);
  Configure(ASrc^.width, ASrc^.height, SrcFormat, FDstW, FDstH, FDstFormat, FScaleFlags);

  Ret := sws_scale(FSwsCtx, @ASrc^.data, @ASrc^.linesize, 0, FSrcH, @FDstFrame^.data, @FDstFrame^.linesize);
  if Ret <= 0 then
    raise EFFException.CreateFmt('sws_scale failed (%d)', [Ret]);

  Result := FDstFrame;
end;

function TFFFrameConverter.Convert(ASrc: TFFFrame): PAVFrame;
begin
  if ASrc = nil then
    raise EFFException.Create('TFFFrameConverter.Convert: frame wrapper is nil');
  Result := Convert(ASrc.Raw);
end;

destructor TFFFrameConverter.Destroy;
begin
  ReleaseResources;
  inherited;
end;

procedure TFFFrameConverter.ReleaseResources;
begin
  if FFIsDesignTime(nil) then
  begin
    FSwsCtx := nil;
    FDstBuffer := nil;
    FDstFrame := nil;
    Exit;
  end;
  if FSwsCtx <> nil then
  begin
    sws_freeContext(FSwsCtx);
    FSwsCtx := nil;
  end;
  if FDstBuffer <> nil then
  begin
    av_freep(@FDstBuffer);
    FDstBuffer := nil;
  end;
  if FDstFrame <> nil then
  begin
    av_frame_free(FDstFrame);
    FDstFrame := nil;
  end;
end;

end.
