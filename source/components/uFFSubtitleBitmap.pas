unit uFFSubtitleBitmap;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Owned bitmap subtitle payload (palette + indexed plane) for overlay. }

interface

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  libavcodec;

type
  TFFSubtitleBitmap = record
    X: Integer;
    Y: Integer;
    W: Integer;
    H: Integer;
    Stride: Integer;
    Palette: TBytes;
    Indices: TBytes;
    function Valid: Boolean;
  end;

procedure FFSubtitleCopyBitmap(const ARect: pAVSubtitleRect; out ADest: TFFSubtitleBitmap);
procedure FFSubtitleFreeBitmap(var ABmp: TFFSubtitleBitmap);
procedure FFSubtitleBlendBitmapOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const ABmp: TFFSubtitleBitmap);

implementation

procedure FFSubtitleCopyBitmap(const ARect: pAVSubtitleRect; out ADest: TFFSubtitleBitmap);
var
  PalSize: Integer;
  IdxSize: Integer;
begin
  FillChar(ADest, SizeOf(ADest), 0);
  if ARect = nil then
    Exit;

  ADest.X := ARect^.x;
  ADest.Y := ARect^.y;
  ADest.W := ARect^.w;
  ADest.H := ARect^.h;
  ADest.Stride := ARect^.linesize[0];
  if ADest.Stride <= 0 then
    ADest.Stride := ADest.W;

  if (ARect^.nb_colors > 0) and (ARect^.data[1] <> nil) then
  begin
    PalSize := ARect^.nb_colors * 4;
    if PalSize > 0 then
    begin
      SetLength(ADest.Palette, PalSize);
      Move(ARect^.data[1]^, ADest.Palette[0], PalSize);
    end;
  end;

  if (ADest.W > 0) and (ADest.H > 0) and (ARect^.data[0] <> nil) then
  begin
    IdxSize := ADest.Stride * ADest.H;
    SetLength(ADest.Indices, IdxSize);
    Move(ARect^.data[0]^, ADest.Indices[0], IdxSize);
  end;
end;

procedure FFSubtitleFreeBitmap(var ABmp: TFFSubtitleBitmap);
begin
  SetLength(ABmp.Palette, 0);
  SetLength(ABmp.Indices, 0);
  FillChar(ABmp, SizeOf(ABmp), 0);
end;

function TFFSubtitleBitmap.Valid: Boolean;
begin
  Result := (W > 0) and (H > 0) and (Length(Indices) > 0) and (Length(Palette) >= 4);
end;

procedure FFSubtitleBlendBitmapOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const ABmp: TFFSubtitleBitmap);
var
  Y, X: Integer;
  DstX, DstY: Integer;
  Idx, PalOff: Integer;
  R, G, B, A: Byte;
  Dst: PByte;
  Alpha: Single;
  Inv: Single;
begin
  if (ABgra = nil) or not ABmp.Valid or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  for Y := 0 to ABmp.H - 1 do
  begin
    DstY := ABmp.Y + Y;
    if (DstY < 0) or (DstY >= AHeight) then
      Continue;
    for X := 0 to ABmp.W - 1 do
    begin
      DstX := ABmp.X + X;
      if (DstX < 0) or (DstX >= AWidth) then
        Continue;

      Idx := ABmp.Indices[Y * ABmp.Stride + X];
      if Idx = 0 then
        Continue;

      PalOff := Idx * 4;
      if PalOff + 3 >= Length(ABmp.Palette) then
        Continue;

      R := ABmp.Palette[PalOff];
      G := ABmp.Palette[PalOff + 1];
      B := ABmp.Palette[PalOff + 2];
      A := ABmp.Palette[PalOff + 3];
      if A = 0 then
        Continue;

      Dst := ABgra + DstY * AStride + DstX * 4;
      if A >= 255 then
      begin
        Dst[0] := B;
        Dst[1] := G;
        Dst[2] := R;
        Dst[3] := 255;
      end
      else
      begin
        Alpha := A / 255.0;
        Inv := 1.0 - Alpha;
        Dst[0] := Round(B * Alpha + Dst[0] * Inv);
        Dst[1] := Round(G * Alpha + Dst[1] * Inv);
        Dst[2] := Round(R * Alpha + Dst[2] * Inv);
        Dst[3] := 255;
      end;
    end;
  end;
end;

end.
