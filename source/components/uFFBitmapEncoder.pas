unit uFFBitmapEncoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ High-level bitmap-to-video encoder: BGRA frames -> TFFEncoder -> TFFWriter. }

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
  uFFEncoder,
  uFFWriter,
  uFFFrame,
  uFFPacket,
  uFFFrameInputAdapter,
  uFFComponentLink
  {$IFDEF MSWINDOWS}
  , Vcl.Graphics
  {$ENDIF}
  ;

type
  TFFBitmapEncodeProgressEvent = procedure(Sender: TObject; AFrameIndex, AFrameCount: Int64) of object;

  TFFBitmapEncoder = class(TComponent)
  private
    FOutputWriter: TFFWriter;
    FWidth: Integer;
    FHeight: Integer;
    FFrameRateNum: Integer;
    FFrameRateDen: Integer;
    FCodecName: string;
    FBitRate: Int64;
    FOptions: TStrings;
    FActive: Boolean;
    FFrameIndex: Int64;
    FOutputStreamIndex: Integer;
    FFrameInput: TFFFrameInputAdapter;
    FEncoder: TFFEncoder;
    FOnProgress: TFFBitmapEncodeProgressEvent;
    procedure SetOptions(const Value: TStrings);
    procedure SetOutputWriter(const Value: TFFWriter);
    function GetOutputWriter: TFFWriter;
    procedure DrainEncoder;
    procedure WriteFrame(AFrame: TFFFrame);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Open;
    procedure Close;
    procedure AddBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer);
    {$IFDEF MSWINDOWS}
    procedure AddBitmap(ABitmap: TBitmap);
    {$ENDIF}

    property Active: Boolean read FActive;
    property FrameIndex: Int64 read FFrameIndex;
  published
    property OutputWriter: TFFWriter read GetOutputWriter write SetOutputWriter;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property FrameRateNum: Integer read FFrameRateNum write FFrameRateNum default 25;
    property FrameRateDen: Integer read FFrameRateDen write FFrameRateDen default 1;
    property CodecName: string read FCodecName write FCodecName;
    property BitRate: Int64 read FBitRate write FBitRate;
    property Options: TStrings read FOptions write SetOptions;
    property OnProgress: TFFBitmapEncodeProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

constructor TFFBitmapEncoder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrameRateNum := 25;
  FFrameRateDen := 1;
  FBitRate := 400000;
  FCodecName := '';
  FOptions := TStringList.Create;
  FFrameInput := TFFFrameInputAdapter.Create;
  FEncoder := TFFEncoder.Create(nil);
  FOutputStreamIndex := -1;
end;

destructor TFFBitmapEncoder.Destroy;
begin
  Close;
  if Assigned(FOutputWriter) then
    FOutputWriter.RemoveFreeNotification(Self);
  FOptions.Free;
  FEncoder.Free;
  FFrameInput.Free;
  inherited;
end;

procedure TFFBitmapEncoder.SetOptions(const Value: TStrings);
begin
  if (FOptions = nil) or (Value = nil) then
    Exit;
  FOptions.Assign(Value);
end;

function TFFBitmapEncoder.GetOutputWriter: TFFWriter;
begin
  Result := FOutputWriter;
end;

procedure TFFBitmapEncoder.SetOutputWriter(const Value: TFFWriter);
var
  Link: TComponent;
begin
  if FOutputWriter = Value then
    Exit;
  if FActive then
    Close;
  Link := FOutputWriter;
  FFSetLinkedComponent(Self, Link, Value);
  FOutputWriter := TFFWriter(Link);
end;

procedure TFFBitmapEncoder.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FOutputWriter;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FOutputWriter := TFFWriter(Link);
end;

procedure TFFBitmapEncoder.Open;
begin
  if FActive then
    Exit;
  if FOutputWriter = nil then
    raise EFFException.Create('TFFBitmapEncoder.OutputWriter is not assigned');
  if not FOutputWriter.HasOutputTarget then
    raise EFFException.Create('TFFBitmapEncoder: OutputWriter needs FileName or OutputAdapter');
  if (FWidth <= 0) or (FHeight <= 0) then
    raise EFFException.Create('TFFBitmapEncoder: Width and Height must be set');

  FEncoder.MediaType := AVMEDIA_TYPE_VIDEO;
  FEncoder.CodecName := FCodecName;
  FEncoder.BitRate := FBitRate;
  FEncoder.Width := FWidth;
  FEncoder.Height := FHeight;
  FEncoder.PixelFormat := AV_PIX_FMT_YUV420P;
  FEncoder.TimeBaseNum := FFrameRateDen;
  FEncoder.TimeBaseDen := FFrameRateNum;
  FEncoder.FrameRateNum := FFrameRateNum;
  FEncoder.FrameRateDen := FFrameRateDen;
  if FOptions <> nil then
    FEncoder.Options.Assign(FOptions);
  FEncoder.Initialize;

  if not FOutputWriter.Active then
    FOutputWriter.Open;
  FOutputStreamIndex := FOutputWriter.AddStream(FEncoder);
  FOutputWriter.WriteHeader;

  FFrameIndex := 0;
  FActive := True;
end;

procedure TFFBitmapEncoder.Close;
var
  Packet: TFFPacket;
begin
  if not FActive then
    Exit;

  Packet := TFFPacket.Create;
  try
    FEncoder.Flush;
    DrainEncoder;
    if FOutputWriter.Active then
      FOutputWriter.WriteTrailer;
  finally
    Packet.Free;
  end;

  FEncoder.CloseCodec;
  FOutputStreamIndex := -1;
  FActive := False;
end;

procedure TFFBitmapEncoder.DrainEncoder;
var
  Packet: TFFPacket;
  Ret: Integer;
begin
  Packet := TFFPacket.Create;
  try
    while True do
    begin
      Ret := FEncoder.ReceivePacket(Packet);
      if Ret = 0 then
      begin
        if FOutputStreamIndex >= 0 then
          FOutputWriter.WritePacket(Packet, FOutputStreamIndex);
      end
      else if Ret = AVERROR_EAGAIN then
        Break
      else
        Break;
    end;
  finally
    Packet.Free;
  end;
end;

procedure TFFBitmapEncoder.WriteFrame(AFrame: TFFFrame);
var
  Ret: Integer;
begin
  if not FActive then
    raise EFFException.Create('TFFBitmapEncoder is not active');
  AFrame.Raw^.pts := FFrameIndex;
  Inc(FFrameIndex);

  Ret := FEncoder.SendFrame(AFrame);
  if Ret < 0 then
    raise EFFException.CreateFmt('TFFBitmapEncoder.SendFrame failed (%d)', [Ret]);
  DrainEncoder;

  if Assigned(FOnProgress) then
    FOnProgress(Self, FFrameIndex, FFrameIndex);
end;

procedure TFFBitmapEncoder.AddBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer);
var
  Frame: TFFFrame;
begin
  if not FActive then
    Open;
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
    raise EFFException.CreateFmt('TFFBitmapEncoder.AddBgra: expected %dx%d, got %dx%d',
      [FWidth, FHeight, AWidth, AHeight]);
  Frame := FFrameInput.ConvertBgraToFrame(ABgra, AWidth, AHeight, AStride, AV_PIX_FMT_YUV420P);
  WriteFrame(Frame);
end;

{$IFDEF MSWINDOWS}
procedure TFFBitmapEncoder.AddBitmap(ABitmap: TBitmap);
var
  W, H, Stride, Y: Integer;
  Buf, SrcRow, DstRow: PByte;
begin
  if ABitmap = nil then
    raise EFFException.Create('TFFBitmapEncoder.AddBitmap: bitmap is nil');
  ABitmap.PixelFormat := pf32bit;
  W := ABitmap.Width;
  H := ABitmap.Height;
  if not FActive then
  begin
    if (FWidth <= 0) or (FHeight <= 0) then
    begin
      FWidth := W;
      FHeight := H;
    end;
    Open;
  end;

  Stride := W * 4;
  GetMem(Buf, NativeUInt(Stride) * NativeUInt(H));
  try
    for Y := 0 to H - 1 do
    begin
      SrcRow := PByte(ABitmap.ScanLine[H - 1 - Y]);
      DstRow := Buf + Stride * Y;
      Move(SrcRow^, DstRow^, Stride);
    end;
    AddBgra(Buf, W, H, Stride);
  finally
    FreeMem(Buf);
  end;
end;
{$ENDIF}

end.
