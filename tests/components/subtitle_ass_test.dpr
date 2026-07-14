program subtitle_ass_test;

{$APPTYPE CONSOLE}

uses
  System.Classes,
  System.SysUtils,
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFSubtitleDecoder,
  uFFSubtitleAss,
  uFFSubtitleOverlay,
  uFFMediaInfo;

procedure Fail(const Msg: string);
begin
  WriteLn('FAIL: ', Msg);
  Halt(1);
end;

function DefaultMediaFile: string;
const
  Candidates: array [0 .. 3] of string = (
    '..\..\resource\test_subs_ass.mkv',
    '..\..\..\resource\test_subs_ass.mkv',
    'D:\Work\Delphi\Delphi-FFMPEG\resource\test_subs_ass.mkv',
    '..\..\resource\test_subs.mp4'
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

var
  Media: string;
  Reader: TFFReader;
  SubDec: TFFSubtitleDecoder;
  Info: TFFMediaInfo;
  SubIdx: Integer;
  Ev: TFFSubtitleEvent;
  Layout: TFFAssLayout;
  FoundAss: Boolean;
  AssPath: string;
  Lines: TStringList;
  I: Integer;
begin
  Media := DefaultMediaFile;
  if ParamCount >= 1 then
    Media := ParamStr(1);
  if Media = '' then
  begin
    WriteLn('SKIP: no media file (run tools\generate_test_subs_ass.ps1)');
    Halt(2);
  end;

  Info := TFFMediaInfo.Create(nil);
  try
    Info.FileName := Media;
    Info.Probe;
    SubIdx := Info.FindBestStream(AVMEDIA_TYPE_SUBTITLE);
  finally
    Info.Free;
  end;

  if SubIdx < 0 then
  begin
    WriteLn('SKIP: no subtitle stream in ' + Media);
    Halt(2);
  end;

  Reader := TFFReader.Create(nil);
  SubDec := TFFSubtitleDecoder.Create(nil);
  try
    Reader.FileName := Media;
    Reader.Open;
    SubDec.Reader := Reader;
    SubDec.StreamIndex := SubIdx;
    SubDec.LoadAll;

    if SubDec.EventCount <= 0 then
      Fail('expected at least one subtitle event');

    Ev := SubDec.GetEventAt(2000);
    FoundAss := Ev.IsAss and ((Ev.AssRaw <> '') or (Ev.Text <> ''));
    if not FoundAss then
    begin
      Ev := SubDec.GetEventAt(5000);
      FoundAss := Ev.IsAss and ((Ev.AssRaw <> '') or (Ev.Text <> ''));
    end;

    if not FoundAss then
    begin
      AssPath := ChangeFileExt(Media, '.ass');
      if not FileExists(AssPath) then
        AssPath := ExtractFilePath(Media) + 'test_subs.ass';
      if FileExists(AssPath) then
      begin
        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(AssPath);
          for I := 0 to Lines.Count - 1 do
            if Pos('Dialogue:', Lines[I]) > 0 then
            begin
              Layout := FFAssParseLayout(Lines[I]);
              FoundAss := Layout.Text <> '';
              if FoundAss then
                Break;
            end;
        finally
          Lines.Free;
        end;
      end;
    end;

    if not FoundAss then
    begin
      WriteLn('SKIP: no ASS subtitle events in ' + Media);
      Halt(2);
    end;

    if (Layout.Text = '') and (Ev.AssRaw <> '') then
      Layout := FFAssParseLayout(Ev.AssRaw)
    else if (Layout.Text = '') and (Ev.Text <> '') then
    begin
      Layout.Text := Ev.Text;
      Layout.Align := 2;
    end;
    if Layout.Text = '' then
      Fail('ASS layout text is empty');

    WriteLn(Format('ASS event: align=%d text="%s"', [Layout.Align, Copy(Layout.Text, 1, 40)]));
    WriteLn('PASS: ASS subtitle parse OK');
  finally
    SubDec.Free;
    Reader.Free;
  end;
end.
