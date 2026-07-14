unit uFFLoader;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Runtime FFmpeg library discovery and pre-loading (Windows delayed imports). }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  ffmpeg_types,
  libavutil,
  uFFDesignTime;

type
  TFFLoadErrorEvent = procedure(Sender: TObject; const DLLName, ErrorMessage: string) of object;

  TFFLoader = class(TComponent)
  private
    FSearchPath: string;
    FLoaded: Boolean;
    FAutoLoad: Boolean;
    FOnLoadError: TFFLoadErrorEvent;
    procedure SetSearchPath(const Value: string);
    procedure SetAutoLoad(const Value: Boolean);
    procedure DoLoadError(const DLLName, ErrorMessage: string);
    procedure LoadLibrariesDesignTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadLibraries;
    procedure UnloadLibraries;
    function IsLoaded: Boolean;
    function GetFFmpegVersion: string;
    function GetLibraryVersion(const LibName: string): string;
    class function Default: TFFLoader;
    class procedure EnsureLoaded;
    class procedure EnsureDesignTimeLoaded;
    class function ResolveSearchPath(const APreferred: string): string;
    class function IsLibrariesLoaded: Boolean;
  published
    property SearchPath: string read FSearchPath write SetSearchPath;
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad default True;
    property OnLoadError: TFFLoadErrorEvent read FOnLoadError write FOnLoadError;
  end;

implementation

uses
  {$IFDEF FPC}
  SysUtils,
  {$ELSE}
  System.SysUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  {$IFDEF FPC}
  Windows,
  {$ELSE}
  Winapi.Windows,
  {$ENDIF}
  {$ENDIF}
  libavcodec,
  libavformat,
  libswscale,
  libswresample;

type
  TDllEntry = record
    Name: string;
    Required: Boolean;
  end;

var
  GDefaultLoader: TFFLoader;

const
  FFmpegDlls: array [0 .. 6] of TDllEntry = (
    (Name: avutil_dll; Required: True),
    (Name: swresample_dll; Required: True),
    (Name: swscale_dll; Required: True),
    (Name: avcodec_dll; Required: True),
    (Name: avformat_dll; Required: True),
    (Name: avfilter_dll; Required: True),
    (Name: avdevice_dll; Required: True)
  );

{$IFDEF MSWINDOWS}
var
  GLoadedModules: array of HMODULE;
{$ENDIF}

function VersionTriple(AVersion: Cardinal): string;
begin
  Result := Format('%d.%d.%d', [AVersion shr 16, (AVersion shr 8) and $FF, AVersion and $FF]);
end;

function NormalizeDir(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFileName(APath));
end;

{$IFDEF MSWINDOWS}
function HostModuleDir: string;
var
  Buf: array [0 .. MAX_PATH - 1] of Char;
  Len: Cardinal;
begin
  Len := GetModuleFileName(HInstance, Buf, MAX_PATH);
  if Len = 0 then
    Exit('');
  SetString(Result, Buf, Len);
  Result := NormalizeDir(ExtractFilePath(Result));
end;
{$ENDIF}

class function TFFLoader.ResolveSearchPath(const APreferred: string): string;
var
  Root, Candidate: string;
begin
  if APreferred <> '' then
    Exit(NormalizeDir(APreferred));

{$IFDEF MSWINDOWS}
  Candidate := HostModuleDir;
  if (Candidate <> '') and FileExists(Candidate + avutil_dll) then
    Exit(Candidate);
{$ENDIF}

  Candidate := NormalizeDir(ExtractFilePath(ParamStr(0)));
  if FileExists(Candidate + avutil_dll) then
    Exit(Candidate);

  Root := ExtractFilePath(ExcludeTrailingPathDelimiter(Candidate));
  if SameText(ExtractFileName(Root), 'dcu') then
    Root := ExtractFilePath(ExcludeTrailingPathDelimiter(Root));
  if SameText(ExtractFileName(Root), 'Debug') or SameText(ExtractFileName(Root), 'Release') then
    Root := ExtractFilePath(ExcludeTrailingPathDelimiter(Root));
  if SameText(ExtractFileName(Root), 'Win64') or SameText(ExtractFileName(Root), 'Win32') then
    Root := ExtractFilePath(ExcludeTrailingPathDelimiter(Root));
  if SameText(ExtractFileName(Root), 'lib') then
    Root := ExtractFilePath(ExcludeTrailingPathDelimiter(Root));

{$IFDEF MSWINDOWS}
  Candidate := NormalizeDir(Root + '..\..\bin\win64');
  if FileExists(Candidate + avutil_dll) then
    Exit(Candidate);
  Candidate := NormalizeDir(Root + '..\..\bin\win32');
  if FileExists(Candidate + avutil_dll) then
    Exit(Candidate);

  Candidate := NormalizeDir(GetEnvironmentVariable('PUBLIC') +
    '\Documents\Embarcadero\Studio\37.0\Bpl\Win64\');
  if FileExists(Candidate + avutil_dll) then
    Exit(Candidate);
  Candidate := NormalizeDir(GetEnvironmentVariable('PUBLIC') +
    '\Documents\Embarcadero\Studio\37.0\Bpl\Win32\');
  if FileExists(Candidate + avutil_dll) then
    Exit(Candidate);
{$ENDIF}

  Result := NormalizeDir(ExtractFilePath(ParamStr(0)));
end;

procedure TFFLoader.DoLoadError(const DLLName, ErrorMessage: string);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, DLLName, ErrorMessage);
end;

constructor TFFLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoLoad := True;
  if GDefaultLoader = nil then
    GDefaultLoader := Self;
end;

destructor TFFLoader.Destroy;
begin
  if GDefaultLoader = Self then
    GDefaultLoader := nil;
  if FLoaded then
    UnloadLibraries;
  inherited Destroy;
end;

procedure TFFLoader.SetSearchPath(const Value: string);
begin
  if FSearchPath <> Value then
  begin
    if FLoaded then
      UnloadLibraries;
    FSearchPath := Value;
    if FAutoLoad and not (csDesigning in ComponentState) then
      LoadLibraries;
  end;
end;

procedure TFFLoader.SetAutoLoad(const Value: Boolean);
begin
  if FAutoLoad <> Value then
  begin
    FAutoLoad := Value;
    if FAutoLoad and not (csDesigning in ComponentState) and not FLoaded then
      LoadLibraries;
  end;
end;

class function TFFLoader.Default: TFFLoader;
begin
  if GDefaultLoader = nil then
    GDefaultLoader := TFFLoader.Create(nil);
  Result := GDefaultLoader;
end;

class function TFFLoader.IsLibrariesLoaded: Boolean;
begin
  Result := Assigned(GDefaultLoader) and GDefaultLoader.FLoaded;
end;

class procedure TFFLoader.EnsureLoaded;
begin
  Default.LoadLibraries;
end;

class procedure TFFLoader.EnsureDesignTimeLoaded;
begin
  if IsLibrariesLoaded then
    Exit;
  Default.LoadLibrariesDesignTime;
end;

procedure TFFLoader.LoadLibraries;
begin
  if FLoaded then
    Exit;
  if csDesigning in ComponentState then
    Exit;
  LoadLibrariesDesignTime;
end;

procedure TFFLoader.LoadLibrariesDesignTime;
var
  Dir, FullPath, Err: string;
  I: Integer;
{$IFDEF MSWINDOWS}
  DllHandle: HMODULE;
{$IFNDEF FPC}
  PrevDir: array [0 .. MAX_PATH - 1] of Char;
  HadPrevDir: Boolean;
{$ENDIF}
{$ENDIF}
begin
  if FLoaded then
    Exit;

  Dir := ResolveSearchPath(FSearchPath);

{$IFDEF MSWINDOWS}
  SetLength(GLoadedModules, 0);
{$IFNDEF FPC}
  HadPrevDir := GetDllDirectory(MAX_PATH, PrevDir) > 0;
  SetDllDirectory(PChar(Dir));
{$ENDIF}
  try
    for I := Low(FFmpegDlls) to High(FFmpegDlls) do
    begin
      FullPath := Dir + FFmpegDlls[I].Name;
      if not FileExists(FullPath) then
      begin
        if FFmpegDlls[I].Required then
        begin
          Err := Format('FFmpeg DLL not found: %s', [FullPath]);
          DoLoadError(FFmpegDlls[I].Name, Err);
          raise Exception.Create(Err);
        end;
        Continue;
      end;

      DllHandle := LoadLibrary(PChar(FullPath));
      if DllHandle = 0 then
      begin
        Err := SysErrorMessage(GetLastError);
        DoLoadError(FFmpegDlls[I].Name, Err);
        raise Exception.CreateFmt('LoadLibrary failed for %s: %s', [FullPath, Err]);
      end;
      GLoadedModules := GLoadedModules + [DllHandle];
    end;
  finally
{$IFNDEF FPC}
    if HadPrevDir then
      SetDllDirectory(PrevDir)
    else
      SetDllDirectory(nil);
{$ENDIF}
  end;

  if avutil_version = 0 then
    raise Exception.Create('avutil loaded but avutil_version returned 0');
{$ELSE}
  if not FileExists(Dir + avutil_dll) then
  begin
    Err := Format('FFmpeg library not found in %s (set SearchPath or LD_LIBRARY_PATH)', [Dir]);
    DoLoadError(avutil_dll, Err);
    raise Exception.Create(Err);
  end;
  if avutil_version = 0 then
    raise Exception.Create('libavutil is not available');
{$ENDIF}

  FLoaded := True;
end;

procedure TFFLoader.UnloadLibraries;
var
  I: Integer;
begin
  if not FLoaded then
    Exit;
  if FFIsDesignTime(Self) then
    Exit;
{$IFDEF MSWINDOWS}
  for I := High(GLoadedModules) downto Low(GLoadedModules) do
  begin
    if GLoadedModules[I] <> 0 then
      FreeLibrary(GLoadedModules[I]);
  end;
  SetLength(GLoadedModules, 0);
{$ENDIF}
  FLoaded := False;
end;

function TFFLoader.IsLoaded: Boolean;
begin
  Result := FLoaded;
end;

function TFFLoader.GetFFmpegVersion: string;
begin
  EnsureLoaded;
  Result := string(AnsiString(av_version_info));
  if Result = '' then
    Result := FFMPEG_VERSION;
end;

function TFFLoader.GetLibraryVersion(const LibName: string): string;
begin
  EnsureLoaded;
  if SameText(LibName, 'avutil') then
    Exit(VersionTriple(avutil_version))
  else if SameText(LibName, 'avcodec') then
    Exit(VersionTriple(avcodec_version))
  else if SameText(LibName, 'avformat') then
    Exit(VersionTriple(avformat_version))
  else if SameText(LibName, 'swscale') then
    Exit(VersionTriple(swscale_version))
  else if SameText(LibName, 'swresample') then
    Exit(VersionTriple(swresample_version))
  else
    raise Exception.CreateFmt('Unknown FFmpeg library name: %s', [LibName]);
end;

initialization

finalization
  FreeAndNil(GDefaultLoader);

end.
