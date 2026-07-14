unit uFFFrameInputAdapter;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Frame input adapter: BGRA buffer (e.g. from VCL/FMX bitmap) -> AVFrame in encoder pixel format. }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libswscale,
  uFFException,
  uFFFrame,
  uFFDesignTime;

type
  TFFFrameInputAdapter = class
  private
    FSwsCtx: PSwsContext;
    FSrcW: Integer;
    FSrcH: Integer;
    FSrcStride: Integer;
    FDstFormat: AVPixelFormat;
    FScaleFlags: Integer;
    FOutFrame: TFFFrame;
    procedure EnsureConfigured(AWidth, AHeight, AStride: Integer; ADstFormat: AVPixelFormat);
    procedure EnsureOutFrame;
  public
    constructor Create;
    destructor Destroy; override;

    function ConvertBgraToFrame(ABgra: PByte; AWidth, AHeight, AStride: Integer; ADstFormat: AVPixelFormat): TFFFrame;
  end;

implementation

constructor TFFFrameInputAdapter.Create;
begin
  inherited Create;
  FScaleFlags := SWS_BILINEAR;
  FDstFormat := AV_PIX_FMT_YUV420P;
end;

destructor TFFFrameInputAdapter.Destroy;
begin
  if FFIsDesignTime(nil) then
  begin
    FSwsCtx := nil;
    FOutFrame := nil;
    inherited;
    Exit;
  end;
  if FSwsCtx <> nil then
    sws_freeContext(FSwsCtx);
  FOutFrame.Free;
  inherited;
end;

procedure TFFFrameInputAdapter.EnsureOutFrame;
begin
  if FOutFrame = nil then
    FOutFrame := TFFFrame.Create;
end;

procedure TFFFrameInputAdapter.EnsureConfigured(AWidth, AHeight, AStride: Integer; ADstFormat: AVPixelFormat);
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    raise EFFException.Create('TFFFrameInputAdapter: invalid size');
  if (AStride = 0) then
    raise EFFException.Create('TFFFrameInputAdapter: invalid stride');

  if (FSwsCtx <> nil) and (FSrcW = AWidth) and (FSrcH = AHeight) and (FSrcStride = AStride) and (FDstFormat = ADstFormat) then
    Exit;

  if FSwsCtx <> nil then
    sws_freeContext(FSwsCtx);
  FSwsCtx := nil;

  FSrcW := AWidth;
  FSrcH := AHeight;
  FSrcStride := AStride;
  FDstFormat := ADstFormat;

  FSwsCtx := sws_getContext(FSrcW, FSrcH, AV_PIX_FMT_BGRA, FSrcW, FSrcH, FDstFormat, FScaleFlags, nil, nil, nil);
  if FSwsCtx = nil then
    raise EFFException.Create('sws_getContext failed (BGRA->dst)');
end;

function TFFFrameInputAdapter.ConvertBgraToFrame(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  ADstFormat: AVPixelFormat): TFFFrame;
var
  Ret: Integer;
  SrcData: array [0 .. 3] of PByte;
  SrcLines: array [0 .. 3] of Integer;
begin
  if ABgra = nil then
    raise EFFException.Create('ConvertBgraToFrame: buffer is nil');

  EnsureOutFrame;
  EnsureConfigured(AWidth, AHeight, AStride, ADstFormat);

  FOutFrame.Raw^.width := AWidth;
  FOutFrame.Raw^.height := AHeight;
  FOutFrame.Raw^.format := Ord(ADstFormat);

  Ret := av_frame_get_buffer(FOutFrame.Raw, 1);
  if Ret < 0 then
    raise EFFException.CreateFmt('av_frame_get_buffer failed (%d)', [Ret]);
  Ret := av_frame_make_writable(FOutFrame.Raw);
  if Ret < 0 then
    raise EFFException.CreateFmt('av_frame_make_writable failed (%d)', [Ret]);

  SrcData[0] := ABgra;
  SrcData[1] := nil;
  SrcData[2] := nil;
  SrcData[3] := nil;
  SrcLines[0] := AStride;
  SrcLines[1] := 0;
  SrcLines[2] := 0;
  SrcLines[3] := 0;

  Ret := sws_scale(FSwsCtx, @SrcData[0], @SrcLines[0], 0, AHeight, @FOutFrame.Raw^.data[0], @FOutFrame.Raw^.linesize[0]);
  if Ret <= 0 then
    raise EFFException.CreateFmt('sws_scale failed (%d)', [Ret]);

  Result := FOutFrame;
end;

end.

