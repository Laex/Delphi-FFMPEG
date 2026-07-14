unit uFFSubtitleOverlay;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Blend decoded subtitle events onto a BGRA video buffer. }

interface

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  uFFSubtitleDecoder,
  uFFSubtitleBitmap,
  uFFSubtitleAss;

procedure FFSubtitleBlendOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const AText: string; AOutline: Boolean = True);

procedure FFSubtitleBlendEventOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const AEvent: TFFSubtitleEvent);

implementation

uses
  {$IFDEF MSWINDOWS}
  System.Types,
  Winapi.Windows,
  Vcl.Graphics;
  {$ENDIF}

procedure FFSubtitleBlendAssOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const AAssLine: string);
{$IFDEF MSWINDOWS}
var
  Layout: TFFAssLayout;
  Bmp: TBitmap;
  R: TRect;
  Flags: UINT;
  Canvas: TCanvas;
  Margin: Integer;
begin
  if (ABgra = nil) or (AAssLine = '') or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Layout := FFAssParseLayout(AAssLine);
  if Layout.Text = '' then
    Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf32bit;
    Bmp.SetSize(AWidth, AHeight);
    Move(ABgra^, Bmp.ScanLine[AHeight - 1]^, AStride * AHeight);

    Canvas := Bmp.Canvas;
    Margin := 8;
    Flags := DT_WORDBREAK or DT_NOPREFIX;

    if Layout.HasPos then
      R := TRect.Create(Layout.PosX, Layout.PosY, AWidth - Margin, AHeight - Margin)
    else
    begin
      case Layout.Align of
        1, 4, 7:
          R := TRect.Create(Margin, Margin, AWidth div 2, AHeight - Margin);
        3, 6, 9:
          R := TRect.Create(AWidth div 2, Margin, AWidth - Margin, AHeight - Margin);
      else
        R := TRect.Create(Margin, Margin, AWidth - Margin, AHeight - Margin);
      end;

      case Layout.Align of
        7, 8, 9:
          begin
            R.Top := Margin;
            R.Bottom := AHeight div 3;
          end;
        4, 5, 6:
          begin
            R.Top := AHeight div 3;
            R.Bottom := (AHeight * 2) div 3;
          end;
      else
        begin
          R.Top := (AHeight * 2) div 3;
          R.Bottom := AHeight - Margin;
        end;
      end;

      if Layout.Align in [1, 2, 3, 4, 5, 6, 7, 8, 9] then
      begin
        if Layout.Align in [1, 4, 7] then
          Flags := Flags or DT_LEFT
        else if Layout.Align in [3, 6, 9] then
          Flags := Flags or DT_RIGHT
        else
          Flags := Flags or DT_CENTER;
      end
      else
        Flags := Flags or DT_CENTER;
    end;

    Canvas.Font.Name := 'Segoe UI';
    Canvas.Font.Size := 14;
    Canvas.Font.Style := [fsBold];
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clBlack;
    OffsetRect(R, -1, 0);
    DrawText(Canvas.Handle, PChar(Layout.Text), Length(Layout.Text), R, Flags);
    OffsetRect(R, 2, 0);
    DrawText(Canvas.Handle, PChar(Layout.Text), Length(Layout.Text), R, Flags);
    OffsetRect(R, -1, -1);
    DrawText(Canvas.Handle, PChar(Layout.Text), Length(Layout.Text), R, Flags);
    OffsetRect(R, 0, 2);
    DrawText(Canvas.Handle, PChar(Layout.Text), Length(Layout.Text), R, Flags);
    OffsetRect(R, 0, -1);
    Canvas.Font.Color := clWhite;
    DrawText(Canvas.Handle, PChar(Layout.Text), Length(Layout.Text), R, Flags);

    Move(Bmp.ScanLine[AHeight - 1]^, ABgra^, AStride * AHeight);
  finally
    Bmp.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure FFSubtitleBlendOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const AText: string; AOutline: Boolean);
{$IFDEF MSWINDOWS}
var
  Bmp: TBitmap;
  R: TRect;
  Flags: UINT;
  Canvas: TCanvas;
begin
  if (ABgra = nil) or (AText = '') or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf32bit;
    Bmp.SetSize(AWidth, AHeight);
    Move(ABgra^, Bmp.ScanLine[AHeight - 1]^, AStride * AHeight);

    Canvas := Bmp.Canvas;
    R := TRect.Create(8, AHeight - 48, AWidth - 8, AHeight - 8);
    Flags := DT_WORDBREAK or DT_CENTER or DT_NOPREFIX;
    Canvas.Font.Name := 'Segoe UI';
    Canvas.Font.Size := 14;
    Canvas.Font.Color := clWhite;
    Canvas.Font.Style := [fsBold];
    if AOutline then
    begin
      Canvas.Font.Color := clBlack;
      Canvas.Brush.Style := bsClear;
      OffsetRect(R, -1, 0);
      DrawText(Canvas.Handle, PChar(AText), Length(AText), R, Flags);
      OffsetRect(R, 2, 0);
      DrawText(Canvas.Handle, PChar(AText), Length(AText), R, Flags);
      OffsetRect(R, -1, -1);
      DrawText(Canvas.Handle, PChar(AText), Length(AText), R, Flags);
      OffsetRect(R, 0, 2);
      DrawText(Canvas.Handle, PChar(AText), Length(AText), R, Flags);
      OffsetRect(R, 0, -1);
    end;
    Canvas.Font.Color := clWhite;
    DrawText(Canvas.Handle, PChar(AText), Length(AText), R, Flags);

    Move(Bmp.ScanLine[AHeight - 1]^, ABgra^, AStride * AHeight);
  finally
    Bmp.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure FFSubtitleBlendEventOnBgra(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  const AEvent: TFFSubtitleEvent);
begin
  if AEvent.IsBitmap then
  begin
    FFSubtitleBlendBitmapOnBgra(ABgra, AWidth, AHeight, AStride, AEvent.Bitmap);
    Exit;
  end;

  if AEvent.IsAss and (AEvent.AssRaw <> '') then
  begin
    FFSubtitleBlendAssOnBgra(ABgra, AWidth, AHeight, AStride, AEvent.AssRaw);
    Exit;
  end;

  if AEvent.Text <> '' then
    FFSubtitleBlendOnBgra(ABgra, AWidth, AHeight, AStride, AEvent.Text, True);
end;

end.
