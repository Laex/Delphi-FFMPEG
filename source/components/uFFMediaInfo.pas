unit uFFMediaInfo;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Read-only media probe (format + stream metadata without decoding). }

interface

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavcodec,
  libavformat,
  uFFException,
  uFFReader,
  uFFMemoryAccessAdapter,
  uFFComponentLink;

type
  TFFMediaStreamInfo = record
    Index: Integer;
    MediaType: AVMediaType;
    CodecName: string;
    Width: Integer;
    Height: Integer;
    SampleRate: Integer;
    Channels: Integer;
    BitRate: Int64;
    DurationMs: Int64;
    FrameRateNum: Integer;
    FrameRateDen: Integer;
    PixelFormatName: string;
    SampleFormatName: string;
  end;

  TFFMediaInfo = class(TComponent)
  private
    FFileName: string;
    FInputAdapter: TFFMemoryAccessAdapter;
    FProbed: Boolean;
    FFormatName: string;
    FFormatLongName: string;
    FDurationMs: Int64;
    FBitRate: Int64;
    FStreams: TArray<TFFMediaStreamInfo>;
    function GetStreamCount: Integer;
    procedure SetFileName(const Value: string);
    procedure SetInputAdapter(const Value: TFFMemoryAccessAdapter);
    procedure ClearCache;
    procedure BuildFromReader(AReader: TFFReader);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Probe;
    procedure Close;

    function GetStreamInfo(AIndex: Integer): TFFMediaStreamInfo;
    function FindBestStream(AMediaType: AVMediaType): Integer;
    function FormatDurationMs: string;
    function SummaryText: string;

    property Probed: Boolean read FProbed;
    property StreamCount: Integer read GetStreamCount;
    property DurationMs: Int64 read FDurationMs;
    property BitRate: Int64 read FBitRate;
  published
    property FileName: string read FFileName write SetFileName;
    property InputAdapter: TFFMemoryAccessAdapter read FInputAdapter write SetInputAdapter;
    property FormatName: string read FFormatName;
    property FormatLongName: string read FFormatLongName;
  end;

function FFMediaTypeToString(AMediaType: AVMediaType): string;
function FFFormatDurationMs(ADurationMs: Int64): string;

implementation

uses
  uFFmpegPath;

function FFMediaTypeToString(AMediaType: AVMediaType): string;
begin
  case AMediaType of
    AVMEDIA_TYPE_VIDEO: Result := 'video';
    AVMEDIA_TYPE_AUDIO: Result := 'audio';
    AVMEDIA_TYPE_SUBTITLE: Result := 'subtitle';
    AVMEDIA_TYPE_DATA: Result := 'data';
    AVMEDIA_TYPE_ATTACHMENT: Result := 'attachment';
  else
    Result := 'unknown';
  end;
end;

function FFFormatDurationMs(ADurationMs: Int64): string;
var
  H, M, S, Ms: Integer;
  TotalSec: Int64;
begin
  if ADurationMs < 0 then
    Exit('');
  TotalSec := ADurationMs div 1000;
  Ms := ADurationMs mod 1000;
  H := TotalSec div 3600;
  M := (TotalSec div 60) mod 60;
  S := TotalSec mod 60;
  Result := Format('%.2d:%.2d:%.2d.%.3.3d', [H, M, S, Ms]);
end;

function TFFMediaInfo.GetStreamCount: Integer;
begin
  Result := Length(FStreams);
end;

constructor TFFMediaInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDurationMs := -1;
  FBitRate := -1;
end;

destructor TFFMediaInfo.Destroy;
begin
  Close;
  if Assigned(FInputAdapter) then
    FInputAdapter.RemoveFreeNotification(Self);
  inherited;
end;

procedure TFFMediaInfo.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FInputAdapter;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    FInputAdapter := TFFMemoryAccessAdapter(Link);
    ClearCache;
  end;
end;

procedure TFFMediaInfo.SetFileName(const Value: string);
begin
  if FFileName = Value then
    Exit;
  FFileName := Value;
  ClearCache;
end;

procedure TFFMediaInfo.SetInputAdapter(const Value: TFFMemoryAccessAdapter);
var
  Link: TComponent;
begin
  if FInputAdapter = Value then
    Exit;
  Link := FInputAdapter;
  FFSetLinkedComponent(Self, Link, Value);
  FInputAdapter := TFFMemoryAccessAdapter(Link);
  ClearCache;
end;

procedure TFFMediaInfo.ClearCache;
begin
  FProbed := False;
  FFormatName := '';
  FFormatLongName := '';
  FDurationMs := -1;
  FBitRate := -1;
  SetLength(FStreams, 0);
end;

procedure TFFMediaInfo.Close;
begin
  ClearCache;
end;

procedure TFFMediaInfo.BuildFromReader(AReader: TFFReader);
var
  I: Integer;
  St: PAVStream;
  Par: PAVCodecParameters;
  Info: TFFMediaStreamInfo;
  MsBase: AVRational;
  Fmt: PAVInputFormat;
begin
  SetLength(FStreams, AReader.StreamCount);
  if AReader.FormatContext <> nil then
  begin
    Fmt := AReader.FormatContext^.iformat;
    if Fmt <> nil then
    begin
      FFormatName := string(Fmt^.name);
      FFormatLongName := string(Fmt^.long_name);
    end;
    if AReader.FormatContext^.bit_rate > 0 then
      FBitRate := AReader.FormatContext^.bit_rate;
    if AReader.Duration > 0 then
      FDurationMs := AReader.Duration div 1000;
  end;

  MsBase := av_make_q(1, 1000);
  for I := 0 to AReader.StreamCount - 1 do
  begin
    FillChar(Info, SizeOf(Info), 0);
    St := AReader.GetStream(I);
    Par := St^.codecpar;
    Info.Index := I;
    Info.MediaType := Par^.codec_type;
    Info.CodecName := string(avcodec_get_name(Par^.codec_id));
    Info.Width := Par^.width;
    Info.Height := Par^.height;
    Info.SampleRate := Par^.sample_rate;
    Info.Channels := Par^.ch_layout.nb_channels;
    if Par^.bit_rate > 0 then
      Info.BitRate := Par^.bit_rate;

    if (St^.avg_frame_rate.num > 0) and (St^.avg_frame_rate.den > 0) then
    begin
      Info.FrameRateNum := St^.avg_frame_rate.num;
      Info.FrameRateDen := St^.avg_frame_rate.den;
    end
    else if (St^.r_frame_rate.num > 0) and (St^.r_frame_rate.den > 0) then
    begin
      Info.FrameRateNum := St^.r_frame_rate.num;
      Info.FrameRateDen := St^.r_frame_rate.den;
    end;

    if St^.duration <> AV_NOPTS_VALUE then
      Info.DurationMs := av_rescale_q(St^.duration, St^.time_base, MsBase)
    else if FDurationMs >= 0 then
      Info.DurationMs := FDurationMs;

    if Par^.codec_type = AVMEDIA_TYPE_VIDEO then
      Info.PixelFormatName := string(av_get_pix_fmt_name(AVPixelFormat(Par^.format)));
    if Par^.codec_type = AVMEDIA_TYPE_AUDIO then
      Info.SampleFormatName := string(av_get_sample_fmt_name(AVSampleFormat(Par^.format)));

    FStreams[I] := Info;
  end;
  FProbed := True;
end;

procedure TFFMediaInfo.Probe;
var
  Reader: TFFReader;
begin
  Close;
  if (FFileName = '') and (FInputAdapter = nil) then
    raise EFFException.Create('TFFMediaInfo: FileName or InputAdapter is required');

  Reader := TFFReader.Create(nil);
  try
    Reader.FileName := FFileName;
    Reader.InputAdapter := FInputAdapter;
    Reader.Open;
    BuildFromReader(Reader);
    Reader.Close;
  finally
    Reader.Free;
  end;
end;

function TFFMediaInfo.GetStreamInfo(AIndex: Integer): TFFMediaStreamInfo;
begin
  if not FProbed then
    raise EFFException.Create('TFFMediaInfo is not probed');
  if (AIndex < 0) or (AIndex >= Length(FStreams)) then
    raise EFFException.CreateFmt('Invalid stream index %d', [AIndex]);
  Result := FStreams[AIndex];
end;

function TFFMediaInfo.FindBestStream(AMediaType: AVMediaType): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not FProbed then
    Exit;
  for I := 0 to High(FStreams) do
    if FStreams[I].MediaType = AMediaType then
      Exit(I);
end;

function TFFMediaInfo.FormatDurationMs: string;
begin
  Result := FFFormatDurationMs(FDurationMs);
end;

function TFFMediaInfo.SummaryText: string;
var
  I: Integer;
  Info: TFFMediaStreamInfo;
  Lines: TStringList;
  Line: string;
begin
  if not FProbed then
    Exit('');
  Lines := TStringList.Create;
  try
    Lines.Add(Format('File: %s', [FFileName]));
    Lines.Add(Format('Format: %s (%s)', [FFormatName, FFormatLongName]));
    if FDurationMs >= 0 then
      Lines.Add(Format('Duration: %s (%d ms)', [FormatDurationMs, FDurationMs]));
    if FBitRate > 0 then
      Lines.Add(Format('Bitrate: %d kb/s', [FBitRate div 1000]));
    Lines.Add(Format('Streams: %d', [Length(FStreams)]));
    for I := 0 to High(FStreams) do
    begin
      Info := FStreams[I];
      Line := Format('#%d %s: %s', [Info.Index, FFMediaTypeToString(Info.MediaType), Info.CodecName]);
      if Info.MediaType = AVMEDIA_TYPE_VIDEO then
      begin
        Line := Line + Format(' %dx%d', [Info.Width, Info.Height]);
        if (Info.FrameRateNum > 0) and (Info.FrameRateDen > 0) then
          Line := Line + Format(' %.3f fps', [Info.FrameRateNum / Info.FrameRateDen]);
        if Info.PixelFormatName <> '' then
          Line := Line + ' ' + Info.PixelFormatName;
      end
      else if Info.MediaType = AVMEDIA_TYPE_AUDIO then
      begin
        Line := Line + Format(' %d Hz', [Info.SampleRate]);
        if Info.Channels > 0 then
          Line := Line + Format(' %d ch', [Info.Channels]);
        if Info.SampleFormatName <> '' then
          Line := Line + ' ' + Info.SampleFormatName;
      end;
      if Info.BitRate > 0 then
        Line := Line + Format(' %d kb/s', [Info.BitRate div 1000]);
      Lines.Add(Line);
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

end.
