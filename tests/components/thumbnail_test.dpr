program thumbnail_test;

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ELSE}
  System.Classes,
  System.SysUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Vcl.Graphics,
  {$ENDIF}
  uFFThumbnailExtractor;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 2] of string = (
    '..\..\resource\768x576.avi',
    '..\..\..\resource\768x576.avi',
    'D:\Work\Delphi\Delphi-FFMPEG\resource\768x576.avi'
  );
var
  Base: string;
  I: Integer;
begin
  Base := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  for I := Low(Candidates) to High(Candidates) do
    if FileExists(Base + Candidates[I]) then
      Exit(Base + Candidates[I]);
  if FileExists(Candidates[2]) then
    Exit(Candidates[2]);
  Result := '';
end;

procedure TestThumbnail(const MediaFile, OutFile: string);
var
  Extractor: TFFThumbnailExtractor;
  {$IFDEF MSWINDOWS}
  Bmp: TBitmap;
  {$ENDIF}
begin
  Extractor := TFFThumbnailExtractor.Create(nil);
  try
    Extractor.FileName := MediaFile;
    Extractor.MaxWidth := 320;
    Extractor.MaxHeight := 240;
    {$IFDEF MSWINDOWS}
    Bmp := TBitmap.Create;
    try
      Extractor.ExtractToBitmap(Bmp);
      if (Bmp.Width <= 0) or (Bmp.Height <= 0) then
        Fail('empty thumbnail bitmap');
      Bmp.SaveToFile(OutFile);
      WriteLn(Format('Thumbnail %dx%d -> %s', [Bmp.Width, Bmp.Height, OutFile]));
    finally
      Bmp.Free;
    end;
    {$ELSE}
    if not Extractor.ExtractToFile(OutFile) then
      Fail('ExtractToFile failed');
    {$ENDIF}
  finally
    Extractor.Free;
  end;
end;

begin
  WriteLn('Delphi-FFMPEG TFFThumbnailExtractor test');
  {$IFDEF MSWINDOWS}
  if ParamCount >= 1 then
    TestThumbnail(ParamStr(1), IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'thumb_out.bmp')
  else if DefaultMediaFile <> '' then
    TestThumbnail(DefaultMediaFile, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'thumb_out.bmp')
  else
  begin
    WriteLn('SKIP: no media file');
    Halt(2);
  end;
  WriteLn('PASS: TFFThumbnailExtractor OK');
  {$ELSE}
  WriteLn('SKIP: requires MSWINDOWS');
  Halt(2);
  {$ENDIF}
end.
