unit uFFFrameBitmap;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ VCL: copy BGRA AVFrame into TBitmap (Windows only). }

interface

{$IFDEF MSWINDOWS}

uses
  Vcl.Graphics,
  libavutil,
  uFFException,
  uFFFrame,
  uFFFrameConverter;

type
  TFFFrameBitmap = class
  public
    class procedure AssignBgraFrame(const AFrame: PAVFrame; AWidth, AHeight: Integer; ABitmap: TBitmap);
    class procedure AssignBgraBuffer(ABgra: PByte; AWidth, AHeight, AStride: Integer; ABitmap: TBitmap);
    class procedure AssignFromConverter(AConverter: TFFFrameConverter; ASrc: TFFFrame; ABitmap: TBitmap); overload;
    class procedure AssignFromConverter(AConverter: TFFFrameConverter; const AConverted: PAVFrame; ABitmap: TBitmap); overload;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

class procedure TFFFrameBitmap.AssignBgraFrame(const AFrame: PAVFrame; AWidth, AHeight: Integer; ABitmap: TBitmap);
var
  Y: Integer;
  Src, Dst: PByte;
  RowBytes: Integer;
begin
  if ABitmap = nil then
    raise EFFException.Create('TFFFrameBitmap.AssignBgraFrame: bitmap is nil');
  if AFrame = nil then
    raise EFFException.Create('TFFFrameBitmap.AssignBgraFrame: frame is nil');

  RowBytes := AWidth * 4;
  ABitmap.PixelFormat := pf32bit;
  ABitmap.SetSize(AWidth, AHeight);

  for Y := 0 to AHeight - 1 do
  begin
    Src := PByte(NativeUInt(AFrame^.data[0]) + NativeUInt(AFrame^.linesize[0] * Y));
    Dst := ABitmap.ScanLine[Y];
    Move(Src^, Dst^, RowBytes);
  end;
end;

class procedure TFFFrameBitmap.AssignBgraBuffer(ABgra: PByte; AWidth, AHeight, AStride: Integer; ABitmap: TBitmap);
var
  Y: Integer;
  Src, Dst: PByte;
  RowBytes: Integer;
begin
  if ABitmap = nil then
    raise EFFException.Create('TFFFrameBitmap.AssignBgraBuffer: bitmap is nil');
  if ABgra = nil then
    raise EFFException.Create('TFFFrameBitmap.AssignBgraBuffer: buffer is nil');

  RowBytes := AWidth * 4;
  ABitmap.PixelFormat := pf32bit;
  ABitmap.SetSize(AWidth, AHeight);

  for Y := 0 to AHeight - 1 do
  begin
    Src := ABgra + AStride * Y;
    Dst := ABitmap.ScanLine[Y];
    Move(Src^, Dst^, RowBytes);
  end;
end;

class procedure TFFFrameBitmap.AssignFromConverter(AConverter: TFFFrameConverter; const AConverted: PAVFrame;
  ABitmap: TBitmap);
begin
  AssignBgraFrame(AConverted, AConverter.DstWidth, AConverter.DstHeight, ABitmap);
end;

class procedure TFFFrameBitmap.AssignFromConverter(AConverter: TFFFrameConverter; ASrc: TFFFrame; ABitmap: TBitmap);
var
  Converted: PAVFrame;
begin
  Converted := AConverter.Convert(ASrc);
  AssignFromConverter(AConverter, Converted, ABitmap);
end;

{$ENDIF}

end.
