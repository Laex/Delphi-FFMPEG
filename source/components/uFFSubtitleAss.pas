unit uFFSubtitleAss;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Lightweight ASS/SSA dialogue parsing for GDI overlay (no libass). }

interface

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  System.Types;

type
  TFFAssLayout = record
    Text: string;
    Align: Integer;
    HasPos: Boolean;
    PosX: Integer;
    PosY: Integer;
  end;

function FFAssExtractDialogueText(const AAssLine: string): string;
function FFAssParseLayout(const AAssLine: string): TFFAssLayout;

implementation

function FFAssFindFrom(const S: string; C: Char; AStart: Integer): Integer;
var
  I: Integer;
begin
  for I := AStart to Length(S) do
    if S[I] = C then
      Exit(I);
  Result := 0;
end;

function FFAssFindTagValue(const AText, ATag: string): string;
var
  P, Q: Integer;
  Tag: string;
begin
  Result := '';
  Tag := '{\' + ATag;
  P := Pos(Tag, AText);
  if P <= 0 then
    Exit;
  P := P + Length(Tag);
  if (P <= Length(AText)) and (AText[P] = '}') then
    Exit;
  Q := FFAssFindFrom(AText, '}', P);
  if Q <= 0 then
    Exit;
  Result := Copy(AText, P, Q - P);
end;

function FFAssStripInlineTags(const AText: string): string;
var
  I: Integer;
  InTag: Boolean;
  C: Char;
begin
  Result := '';
  InTag := False;
  for I := 1 to Length(AText) do
  begin
    C := AText[I];
    if C = '{' then
      InTag := True
    else if C = '}' then
      InTag := False
    else if not InTag then
      Result := Result + C;
  end;
end;

function FFAssExtractDialogueText(const AAssLine: string): string;
var
  S, Part: string;
  I, CommaCount, TagPos: Integer;
begin
  S := Trim(AAssLine);
  if SameText(Copy(S, 1, 9), 'Dialogue:') then
    Delete(S, 1, 9);
  S := Trim(S);

  TagPos := Pos('{\', S);
  if TagPos > 0 then
  begin
    Result := FFAssStripInlineTags(Copy(S, TagPos, MaxInt));
    Result := StringReplace(Result, '\N', sLineBreak, [rfReplaceAll]);
    Result := StringReplace(Result, '\n', sLineBreak, [rfReplaceAll]);
    Result := StringReplace(Result, '\h', ' ', [rfReplaceAll]);
    Exit(Trim(Result));
  end;

  Part := '';
  CommaCount := 0;
  for I := 1 to Length(S) do
  begin
    if S[I] = ',' then
    begin
      Inc(CommaCount);
      if CommaCount = 9 then
      begin
        Part := Copy(S, I + 1, MaxInt);
        Break;
      end;
    end;
  end;

  if Part = '' then
  begin
    I := Length(S);
    while I > 1 do
    begin
      if (S[I - 1] = ',') and (S[I] = ',') then
      begin
        Part := Copy(S, I + 1, MaxInt);
        Break;
      end;
      Dec(I);
    end;
    if Part = '' then
      Part := S;
  end;

  Result := FFAssStripInlineTags(Part);
  Result := StringReplace(Result, '\N', sLineBreak, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', sLineBreak, [rfReplaceAll]);
  Result := StringReplace(Result, '\h', ' ', [rfReplaceAll]);
  Result := Trim(Result);
end;

function FFAssParseLayout(const AAssLine: string): TFFAssLayout;
var
  RawText, TagVal: string;
  P, CommaPos: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Align := 2;

  RawText := FFAssExtractDialogueText(AAssLine);
  Result.Text := RawText;

  TagVal := FFAssFindTagValue(AAssLine, 'an');
  if TagVal <> '' then
    Result.Align := StrToIntDef(TagVal, 2);

  TagVal := FFAssFindTagValue(AAssLine, 'pos');
  if TagVal <> '' then
  begin
    CommaPos := Pos(',', TagVal);
    if CommaPos > 0 then
    begin
      Result.HasPos := True;
      Result.PosX := StrToIntDef(Copy(TagVal, 1, CommaPos - 1), 0);
      Result.PosY := StrToIntDef(Copy(TagVal, CommaPos + 1, MaxInt), 0);
    end;
  end
  else
  begin
    P := Pos('{\pos(', AAssLine);
    if P > 0 then
    begin
      TagVal := Copy(AAssLine, P + 6, MaxInt);
      CommaPos := Pos(',', TagVal);
      if CommaPos > 0 then
      begin
        Result.HasPos := True;
        Result.PosX := StrToIntDef(Copy(TagVal, 1, CommaPos - 1), 0);
        P := FFAssFindFrom(TagVal, ')', CommaPos + 1);
        if P > 0 then
          Result.PosY := StrToIntDef(Copy(TagVal, CommaPos + 1, P - CommaPos - 1), 0);
      end;
    end;
  end;
end;

end.
