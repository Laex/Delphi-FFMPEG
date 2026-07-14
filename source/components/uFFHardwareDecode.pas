unit uFFHardwareDecode;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Helpers for FFmpeg hardware video decode (hw_device_ctx + frame transfer). }

interface

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  ffmpeg_types,
  libavcodec,
  libavutil,
  uFFFrame,
  uFFDesignTime;

type
  TFFHardwareDevice = (
    ffhdNone,
    ffhdAuto,
    ffhdD3D11VA,
    ffhdDXVA2,
    ffhdCUDA,
    ffhdQSV,
    ffhdVAAPI,
    ffhdVideoToolbox
  );

  TFFHardwareDecodeContext = class
  private
    FDeviceType: AVHWDeviceType;
    FHwDeviceCtx: pAVBufferRef;
    FHwPixFmt: AVPixelFormat;
    FActive: Boolean;
    function PickHwPixFmt(ACodec: pAVCodec; ADevice: AVHWDeviceType): AVPixelFormat;
    function DoGetFormat(ACodecCtx: pAVCodecContext; const AFmts: pAVPixelFormat): AVPixelFormat;
  public
    destructor Destroy; override;

    function TrySetup(ACodecCtx: pAVCodecContext; ACodec: pAVCodec; ADevice: TFFHardwareDevice): Boolean;
    function TransferToSoftware(AHwFrame, ADstFrame: TFFFrame): Integer;

    property Active: Boolean read FActive;
    property HwPixFmt: AVPixelFormat read FHwPixFmt;
  end;

function FFHardwareGetFormatCallback(ACodecCtx: pAVCodecContext; const AFmts: pAVPixelFormat): AVPixelFormat; cdecl;

function FFHardwareDeviceToAVType(ADevice: TFFHardwareDevice): AVHWDeviceType;
function FFResolveAutoHardwareDevice: TFFHardwareDevice;
function FFIsHardwarePixelFormat(AFmt: AVPixelFormat): Boolean;

implementation

function FFHardwareDeviceToAVType(ADevice: TFFHardwareDevice): AVHWDeviceType;
begin
  case ADevice of
    ffhdD3D11VA: Result := AV_HWDEVICE_TYPE_D3D11VA;
    ffhdDXVA2: Result := AV_HWDEVICE_TYPE_DXVA2;
    ffhdCUDA: Result := AV_HWDEVICE_TYPE_CUDA;
    ffhdQSV: Result := AV_HWDEVICE_TYPE_QSV;
    ffhdVAAPI: Result := AV_HWDEVICE_TYPE_VAAPI;
    ffhdVideoToolbox: Result := AV_HWDEVICE_TYPE_VIDEOTOOLBOX;
  else
    Result := AV_HWDEVICE_TYPE_NONE;
  end;
end;

function FFResolveAutoHardwareDevice: TFFHardwareDevice;
begin
  {$IFDEF MSWINDOWS}
  Result := ffhdD3D11VA;
  {$ELSEIF DEFINED(DARWIN)}
  Result := ffhdVideoToolbox;
  {$ELSE}
  Result := ffhdVAAPI;
  {$ENDIF}
end;

function FFIsHardwarePixelFormat(AFmt: AVPixelFormat): Boolean;
var
  Desc: pAVPixFmtDescriptor;
begin
  Desc := av_pix_fmt_desc_get(AFmt);
  Result := (Desc <> nil) and ((Desc^.flags and AV_PIX_FMT_FLAG_HWACCEL) <> 0);
end;

destructor TFFHardwareDecodeContext.Destroy;
begin
  if not FFIsDesignTime(nil) then
    av_buffer_unref(FHwDeviceCtx);
  FHwDeviceCtx := nil;
  inherited;
end;

function TFFHardwareDecodeContext.PickHwPixFmt(ACodec: pAVCodec; ADevice: AVHWDeviceType): AVPixelFormat;
var
  I: Integer;
  Cfg: pAVCodecHWConfig;
begin
  Result := AV_PIX_FMT_NONE;
  I := 0;
  while True do
  begin
    Cfg := avcodec_get_hw_config(ACodec, I);
    if Cfg = nil then
      Break;
    Inc(I);
    if (Cfg^.device_type = ADevice) and
       ((Cfg^.methods and AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX) <> 0) then
      Exit(Cfg^.pix_fmt);
  end;
end;

function TFFHardwareDecodeContext.TrySetup(ACodecCtx: pAVCodecContext; ACodec: pAVCodec;
  ADevice: TFFHardwareDevice): Boolean;
var
  Dev: AVHWDeviceType;
  Ret: Integer;
begin
  Result := False;
  FActive := False;
  FHwPixFmt := AV_PIX_FMT_NONE;
  av_buffer_unref(FHwDeviceCtx);

  if (ACodecCtx = nil) or (ACodec = nil) or (ADevice = ffhdNone) then
    Exit;

  if ADevice = ffhdAuto then
    Dev := FFHardwareDeviceToAVType(FFResolveAutoHardwareDevice)
  else
    Dev := FFHardwareDeviceToAVType(ADevice);

  if Dev = AV_HWDEVICE_TYPE_NONE then
    Exit;

  FHwPixFmt := PickHwPixFmt(ACodec, Dev);
  if FHwPixFmt = AV_PIX_FMT_NONE then
    Exit;

  Ret := av_hwdevice_ctx_create(FHwDeviceCtx, Dev, nil, nil, 0);
  if Ret < 0 then
    Exit;

  ACodecCtx^.opaque := Self;
  ACodecCtx^.get_format := @FFHardwareGetFormatCallback;
  ACodecCtx^.hw_device_ctx := av_buffer_ref(FHwDeviceCtx);
  FDeviceType := Dev;
  FActive := True;
  Result := True;
end;

function TFFHardwareDecodeContext.DoGetFormat(ACodecCtx: pAVCodecContext;
  const AFmts: pAVPixelFormat): AVPixelFormat;
var
  P: pAVPixelFormat;
begin
  P := AFmts;
  while (P <> nil) and (P^ <> AV_PIX_FMT_NONE) do
  begin
    if P^ = FHwPixFmt then
      Exit(P^);
    Inc(P);
  end;
  Result := avcodec_default_get_format(ACodecCtx, AFmts);
end;

function FFHardwareGetFormatCallback(ACodecCtx: pAVCodecContext;
  const AFmts: pAVPixelFormat): AVPixelFormat; cdecl;
begin
  if (ACodecCtx <> nil) and (ACodecCtx^.opaque <> nil) then
    Result := TFFHardwareDecodeContext(ACodecCtx^.opaque).DoGetFormat(ACodecCtx, AFmts)
  else
    Result := avcodec_default_get_format(ACodecCtx, AFmts);
end;

function TFFHardwareDecodeContext.TransferToSoftware(AHwFrame, ADstFrame: TFFFrame): Integer;
begin
  if (AHwFrame = nil) or (ADstFrame = nil) then
    Exit(-1);
  ADstFrame.Clear;
  Result := av_hwframe_transfer_data(ADstFrame.Raw, AHwFrame.Raw, 0);
end;

end.
