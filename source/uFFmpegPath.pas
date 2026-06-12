unit uFFmpegPath;

{ UTF-8 file paths for FFmpeg C API (avformat_open_input, avio_open, etc.).
  Do not pass AnsiString(UnicodePath) — that breaks non-ASCII paths on Windows. }

interface

uses
  System.SysUtils;

function FFmpegUtf8Path(const Path: string): UTF8String;

implementation

function FFmpegUtf8Path(const Path: string): UTF8String;
begin
  Result := UTF8String(Path);
end;

end.
