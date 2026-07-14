unit uFFFMXFrameBitmap;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ FMX: copy BGRA buffer into TBitmap. }

interface

uses
  FMX.Graphics,
  libavutil,
  uFFException,
  uFFFrame,
  uFFFrameConverter;

type
  TFFFMXFrameBitmap = class
  public
    class procedure AssignBgraBuffer(ABgra: PByte; AWidth, AHeight, AStride: Integer; ABitmap: TBitmap);
    class procedure AssignBgraFrame(const AFrame: PAVFrame; AWidth, AHeight: Integer; ABitmap: TBitmap);
    class procedure AssignFromConverter(AConverter: TFFFrameConverter; const AConverted: PAVFrame; ABitmap: TBitmap); overload;
    class procedure AssignFromConverter(AConverter: TFFFrameConverter; ASrc: TFFFrame; ABitmap: TBitmap); overload;
  end;

implementation

class procedure TFFFMXFrameBitmap.AssignBgraBuffer(ABgra: PByte; AWidth, AHeight, AStride: Integer; ABitmap: TBitmap);
var
  Map: TBitmapData;
  Y: Integer;
  Src, Dst: PByte;
  RowBytes: Integer;
begin
  if ABitmap = nil then
    raise EFFException.Create('TFFFMXFrameBitmap.AssignBgraBuffer: bitmap is nil');
  if ABgra = nil then
    raise EFFException.Create('TFFFMXFrameBitmap.AssignBgraBuffer: buffer is nil');

  RowBytes := AWidth * 4;
  ABitmap.SetSize(AWidth, AHeight);

  if not ABitmap.Map(TMapAccess.Write, Map) then
    raise EFFException.Create('TFFFMXFrameBitmap: Map failed');

  try
    for Y := 0 to AHeight - 1 do
    begin
      Src := ABgra + AStride * Y;
      Dst := PByte(NativeUInt(Map.Data) + NativeUInt(Map.Pitch * Y));
      Move(Src^, Dst^, RowBytes);
    end;
  finally
    ABitmap.Unmap(Map);
  end;
end;

class procedure TFFFMXFrameBitmap.AssignBgraFrame(const AFrame: PAVFrame; AWidth, AHeight: Integer; ABitmap: TBitmap);
begin
  if AFrame = nil then
    raise EFFException.Create('TFFFMXFrameBitmap.AssignBgraFrame: frame is nil');
  AssignBgraBuffer(AFrame^.data[0], AWidth, AHeight, AFrame^.linesize[0], ABitmap);
end;

class procedure TFFFMXFrameBitmap.AssignFromConverter(AConverter: TFFFrameConverter; const AConverted: PAVFrame;
  ABitmap: TBitmap);
begin
  AssignBgraFrame(AConverted, AConverter.DstWidth, AConverter.DstHeight, ABitmap);
end;

class procedure TFFFMXFrameBitmap.AssignFromConverter(AConverter: TFFFrameConverter; ASrc: TFFFrame; ABitmap: TBitmap);
var
  Converted: PAVFrame;
begin
  Converted := AConverter.Convert(ASrc);
  AssignFromConverter(AConverter, Converted, ABitmap);
end;

end.
