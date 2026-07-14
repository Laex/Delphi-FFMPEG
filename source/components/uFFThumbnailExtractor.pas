unit uFFThumbnailExtractor;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Extract a single preview frame from a media file to bitmap/file. }

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
  uFFException,
  uFFReader,
  uFFDecoder,
  uFFFrame,
  uFFFrameConverter,
  uFFMemoryAccessAdapter,
  uFFComponentLink
  {$IFDEF MSWINDOWS}
  , Vcl.Graphics
  , uFFFrameBitmap
  {$ENDIF}
  ;

type
  TFFThumbnailExtractor = class(TComponent)
  private
    FFileName: string;
    FInputAdapter: TFFMemoryAccessAdapter;
    FPositionMs: Int64;
    FVideoStreamIndex: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    procedure SetFileName(const Value: string);
    procedure SetInputAdapter(const Value: TFFMemoryAccessAdapter);
    function ResolveVideoStreamIndex(AReader: TFFReader): Integer;
    function ResolvePositionMs(AReader: TFFReader; ADecoder: TFFDecoder): Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    {$IFDEF MSWINDOWS}
    procedure ExtractToBitmap(ABitmap: TBitmap);
    {$ENDIF}
    function ExtractToFile(const AFileName: string): Boolean;

    property PositionMs: Int64 read FPositionMs write FPositionMs;
  published
    property FileName: string read FFileName write SetFileName;
    property InputAdapter: TFFMemoryAccessAdapter read FInputAdapter write SetInputAdapter;
    property VideoStreamIndex: Integer read FVideoStreamIndex write FVideoStreamIndex default -1;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 0;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 0;
  end;

implementation

constructor TFFThumbnailExtractor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPositionMs := -1;
  FVideoStreamIndex := -1;
end;

destructor TFFThumbnailExtractor.Destroy;
begin
  if Assigned(FInputAdapter) then
    FInputAdapter.RemoveFreeNotification(Self);
  inherited;
end;

procedure TFFThumbnailExtractor.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FInputAdapter;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FInputAdapter := TFFMemoryAccessAdapter(Link);
end;

procedure TFFThumbnailExtractor.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TFFThumbnailExtractor.SetInputAdapter(const Value: TFFMemoryAccessAdapter);
var
  Link: TComponent;
begin
  if FInputAdapter = Value then
    Exit;
  Link := FInputAdapter;
  FFSetLinkedComponent(Self, Link, Value);
  FInputAdapter := TFFMemoryAccessAdapter(Link);
end;

function TFFThumbnailExtractor.ResolveVideoStreamIndex(AReader: TFFReader): Integer;
var
  I: Integer;
begin
  if FVideoStreamIndex >= 0 then
    Exit(FVideoStreamIndex);
  for I := 0 to AReader.StreamCount - 1 do
    if AReader.Streams.GetInfo(I).MediaType = AVMEDIA_TYPE_VIDEO then
      Exit(I);
  Result := -1;
end;

function TFFThumbnailExtractor.ResolvePositionMs(AReader: TFFReader; ADecoder: TFFDecoder): Int64;
var
  Details: TFFDecoderDetails;
begin
  if FPositionMs >= 0 then
    Exit(FPositionMs);
  Details := ADecoder.GetStreamDetails;
  if Details.DurationMs > 0 then
    Result := Details.DurationMs div 2
  else if AReader.Duration > 0 then
    Result := AReader.Duration div 2000
  else
    Result := 0;
end;

{$IFDEF MSWINDOWS}
procedure TFFThumbnailExtractor.ExtractToBitmap(ABitmap: TBitmap);
var
  Reader: TFFReader;
  Decoder: TFFDecoder;
  Frame: TFFFrame;
  Converter: TFFFrameConverter;
  Converted: PAVFrame;
  PosMs: Int64;
  DstW, DstH, SrcW, SrcH: Integer;
  SrcFmt: AVPixelFormat;
begin
  if ABitmap = nil then
    raise EFFException.Create('TFFThumbnailExtractor.ExtractToBitmap: bitmap is nil');
  if (FFileName = '') and (FInputAdapter = nil) then
    raise EFFException.Create('TFFThumbnailExtractor: FileName or InputAdapter is required');

  Reader := TFFReader.Create(nil);
  Decoder := TFFDecoder.Create(nil);
  Frame := TFFFrame.Create;
  Converter := TFFFrameConverter.Create;
  try
    Reader.FileName := FFileName;
    Reader.InputAdapter := FInputAdapter;
    Reader.Open;

    Decoder.Reader := Reader;
    Decoder.StreamIndex := ResolveVideoStreamIndex(Reader);
    if Decoder.StreamIndex < 0 then
      raise EFFException.Create('TFFThumbnailExtractor: no video stream found');

    PosMs := ResolvePositionMs(Reader, Decoder);
    if Decoder.DecodeFrameAt(PosMs, Frame) < 0 then
      raise EFFException.CreateFmt('DecodeFrameAt(%d) failed', [PosMs]);

    SrcW := Frame.Raw^.width;
    SrcH := Frame.Raw^.height;
    SrcFmt := AVPixelFormat(Frame.Raw^.format);
    DstW := SrcW;
    DstH := SrcH;
    if (FMaxWidth > 0) and (DstW > FMaxWidth) then
    begin
      DstH := DstH * FMaxWidth div DstW;
      DstW := FMaxWidth;
    end;
    if (FMaxHeight > 0) and (DstH > FMaxHeight) then
    begin
      DstW := DstW * FMaxHeight div DstH;
      DstH := FMaxHeight;
    end;
    if DstW <= 0 then DstW := 1;
    if DstH <= 0 then DstH := 1;

    Converter.Configure(SrcW, SrcH, SrcFmt, DstW, DstH, AV_PIX_FMT_BGRA);
    Converted := Converter.Convert(Frame);
    TFFFrameBitmap.AssignBgraFrame(Converted, Converter.DstWidth, Converter.DstHeight, ABitmap);
  finally
    Converter.Free;
    Frame.Free;
    Decoder.Free;
    Reader.Free;
  end;
end;
{$ENDIF}

function TFFThumbnailExtractor.ExtractToFile(const AFileName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    ExtractToBitmap(Bmp);
    Bmp.SaveToFile(AFileName);
    Result := True;
  finally
    Bmp.Free;
  end;
{$ELSE}
begin
  raise EFFException.Create('TFFThumbnailExtractor.ExtractToFile requires MSWINDOWS');
{$ENDIF}
end;

end.
