unit uFFTranscodePreset;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Named transcode presets and TFFTranscodeJob helper component. }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  uFFReader,
  uFFDecoder,
  uFFEncoder,
  uFFWriter,
  uFFComponentLink,
  uFFFrameFilter;

type
  TFFTranscodePreset = (
    ftpMpeg4_800k,
    ftpH264_Medium,
    ftpH264_Fast,
    ftpH264_High,
    ftpH264_NVENC,
    ftpYouTube_1080p,
    ftpWeb_Preview_480p,
    ftpWebM_VP9
  );

  TFFTranscodePresetHelper = record
  public
    class function DisplayName(AValue: TFFTranscodePreset): string; static;
    class function SuggestedExtension(AValue: TFFTranscodePreset): string; static;
    class function SuggestedFilter(AValue: TFFTranscodePreset): string; static;
  end;

procedure FFApplyTranscodePreset(AEncoder: TFFEncoder; AWriter: TFFWriter; APreset: TFFTranscodePreset);
procedure FFConfigureWriterFormat(AWriter: TFFWriter; const AOutputFileName: string; APreset: TFFTranscodePreset);

type
  TFFTranscodeJob = class(TComponent)
  private
    FReader: TFFReader;
    FInputDecoder: TFFDecoder;
    FEncoder: TFFEncoder;
    FWriter: TFFWriter;
    FPreset: TFFTranscodePreset;
    FCopyAudio: Boolean;
    FFrameFilter: TFFFrameFilter;
    FStartMs: Int64;
    FEndMs: Int64;
    procedure SetReader(const Value: TFFReader);
    procedure SetInputDecoder(const Value: TFFDecoder);
    procedure SetEncoder(const Value: TFFEncoder);
    procedure SetWriter(const Value: TFFWriter);
    procedure SetFrameFilter(const Value: TFFFrameFilter);
    procedure SyncEncoderLinks;
  public
    procedure ApplyPreset;
    procedure ConfigureOutput(const AOutputFileName: string);
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
  published
    property Reader: TFFReader read FReader write SetReader;
    property InputDecoder: TFFDecoder read FInputDecoder write SetInputDecoder;
    property Encoder: TFFEncoder read FEncoder write SetEncoder;
    property Writer: TFFWriter read FWriter write SetWriter;
    property Preset: TFFTranscodePreset read FPreset write FPreset default ftpMpeg4_800k;
    property CopyAudio: Boolean read FCopyAudio write FCopyAudio default True;
    property FrameFilter: TFFFrameFilter read FFrameFilter write SetFrameFilter;
    property StartMs: Int64 read FStartMs write FStartMs default 0;
    property EndMs: Int64 read FEndMs write FEndMs default 0;
  end;

implementation

class function TFFTranscodePresetHelper.DisplayName(AValue: TFFTranscodePreset): string;
begin
  case AValue of
    ftpMpeg4_800k: Result := 'MPEG-4 800 kbps';
    ftpH264_Medium: Result := 'H.264 medium (libx264)';
    ftpH264_Fast: Result := 'H.264 fast (libx264)';
    ftpH264_High: Result := 'H.264 high quality (libx264)';
    ftpH264_NVENC: Result := 'H.264 NVENC (GPU, h264_nvenc)';
    ftpYouTube_1080p: Result := 'YouTube 1080p (H.264 + AAC, MP4)';
    ftpWeb_Preview_480p: Result := 'Web preview 480p (H.264 + AAC)';
    ftpWebM_VP9: Result := 'WebM VP9';
  else
    Result := 'Unknown';
  end;
end;

class function TFFTranscodePresetHelper.SuggestedExtension(AValue: TFFTranscodePreset): string;
begin
  case AValue of
    ftpWebM_VP9: Result := '.webm';
    ftpYouTube_1080p, ftpWeb_Preview_480p: Result := '.mp4';
  else
    Result := '.mkv';
  end;
end;

class function TFFTranscodePresetHelper.SuggestedFilter(AValue: TFFTranscodePreset): string;
begin
  case AValue of
    ftpWeb_Preview_480p: Result := 'scale=854:480';
  else
    Result := '';
  end;
end;

procedure FFApplyTranscodePreset(AEncoder: TFFEncoder; AWriter: TFFWriter; APreset: TFFTranscodePreset);
begin
  if AEncoder = nil then
    Exit;

  AEncoder.MediaType := AVMEDIA_TYPE_VIDEO;
  AEncoder.Options.Clear;
  AEncoder.AudioOptions.Clear;
  AEncoder.TranscodeAudio := False;

  case APreset of
    ftpMpeg4_800k:
      begin
        AEncoder.CodecName := 'mpeg4';
        AEncoder.BitRate := 800000;
        AEncoder.GopSize := 12;
      end;
    ftpH264_Medium:
      begin
        AEncoder.CodecName := 'libx264';
        AEncoder.BitRate := 1500000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['preset'] := 'medium';
        AEncoder.Options.Values['crf'] := '23';
      end;
    ftpH264_Fast:
      begin
        AEncoder.CodecName := 'libx264';
        AEncoder.BitRate := 1200000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['preset'] := 'veryfast';
        AEncoder.Options.Values['crf'] := '26';
      end;
    ftpH264_High:
      begin
        AEncoder.CodecName := 'libx264';
        AEncoder.BitRate := 4000000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['preset'] := 'slow';
        AEncoder.Options.Values['crf'] := '18';
      end;
    ftpH264_NVENC:
      begin
        AEncoder.CodecName := 'h264_nvenc';
        AEncoder.BitRate := 4000000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['preset'] := 'p4';
        AEncoder.Options.Values['rc'] := 'vbr';
      end;
    ftpYouTube_1080p:
      begin
        AEncoder.CodecName := 'libx264';
        AEncoder.BitRate := 8000000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['preset'] := 'medium';
        AEncoder.Options.Values['crf'] := '23';
        AEncoder.Options.Values['movflags'] := '+faststart';
        AEncoder.TranscodeAudio := True;
        AEncoder.CopyAudio := False;
        AEncoder.AudioCodecName := 'aac';
        AEncoder.AudioBitRate := 192000;
        AEncoder.AudioSampleRate := 48000;
        AEncoder.AudioChannels := 2;
      end;
    ftpWeb_Preview_480p:
      begin
        AEncoder.CodecName := 'libx264';
        AEncoder.BitRate := 900000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['preset'] := 'veryfast';
        AEncoder.Options.Values['crf'] := '28';
        AEncoder.Options.Values['movflags'] := '+faststart';
        AEncoder.TranscodeAudio := True;
        AEncoder.CopyAudio := False;
        AEncoder.AudioCodecName := 'aac';
        AEncoder.AudioBitRate := 96000;
        AEncoder.AudioSampleRate := 44100;
        AEncoder.AudioChannels := 2;
      end;
    ftpWebM_VP9:
      begin
        AEncoder.CodecName := 'libvpx-vp9';
        AEncoder.BitRate := 1500000;
        AEncoder.GopSize := 48;
        AEncoder.Options.Values['deadline'] := 'good';
        AEncoder.Options.Values['cpu-used'] := '2';
      end;
  end;

  if AWriter <> nil then
  begin
    case APreset of
      ftpWebM_VP9:
        AWriter.FormatName := 'webm';
      ftpYouTube_1080p, ftpWeb_Preview_480p:
        AWriter.FormatName := 'mp4';
    else
      AWriter.FormatName := 'matroska';
    end;
  end;
end;

procedure FFConfigureWriterFormat(AWriter: TFFWriter; const AOutputFileName: string; APreset: TFFTranscodePreset);
begin
  if AWriter = nil then
    Exit;
  AWriter.FileName := AOutputFileName;
  if LowerCase(ExtractFileExt(AOutputFileName)) = '.mp4' then
    AWriter.FormatName := 'mp4'
  else if LowerCase(ExtractFileExt(AOutputFileName)) = '.webm' then
    AWriter.FormatName := 'webm'
  else if APreset in [ftpWebM_VP9] then
    AWriter.FormatName := 'webm'
  else if APreset in [ftpYouTube_1080p, ftpWeb_Preview_480p] then
    AWriter.FormatName := 'mp4'
  else
    AWriter.FormatName := 'matroska';
end;

{ TFFTranscodeJob }

procedure TFFTranscodeJob.SyncEncoderLinks;
begin
  if FEncoder = nil then
    Exit;
  FEncoder.Reader := FReader;
  FEncoder.InputDecoder := FInputDecoder;
  FEncoder.OutputWriter := FWriter;
  FEncoder.CopyAudio := FCopyAudio;
  FEncoder.FrameFilter := FFrameFilter;
  FEncoder.StartMs := FStartMs;
  FEncoder.EndMs := FEndMs;
  if FFrameFilter <> nil then
    FFrameFilter.InputDecoder := FInputDecoder;
end;

procedure TFFTranscodeJob.SetReader(const Value: TFFReader);
var
  Link: TComponent;
begin
  Link := FReader;
  FFSetLinkedComponent(Self, Link, Value);
  FReader := TFFReader(Link);
  SyncEncoderLinks;
end;

procedure TFFTranscodeJob.SetInputDecoder(const Value: TFFDecoder);
var
  Link: TComponent;
begin
  Link := FInputDecoder;
  FFSetLinkedComponent(Self, Link, Value);
  FInputDecoder := TFFDecoder(Link);
  SyncEncoderLinks;
end;

procedure TFFTranscodeJob.SetEncoder(const Value: TFFEncoder);
var
  Link: TComponent;
begin
  Link := FEncoder;
  FFSetLinkedComponent(Self, Link, Value);
  FEncoder := TFFEncoder(Link);
  SyncEncoderLinks;
end;

procedure TFFTranscodeJob.SetWriter(const Value: TFFWriter);
var
  Link: TComponent;
begin
  Link := FWriter;
  FFSetLinkedComponent(Self, Link, Value);
  FWriter := TFFWriter(Link);
  SyncEncoderLinks;
end;

procedure TFFTranscodeJob.SetFrameFilter(const Value: TFFFrameFilter);
var
  Link: TComponent;
begin
  Link := FFrameFilter;
  FFSetLinkedComponent(Self, Link, Value);
  FFrameFilter := TFFFrameFilter(Link);
  SyncEncoderLinks;
end;

procedure TFFTranscodeJob.ApplyPreset;
var
  FilterDesc: string;
begin
  SyncEncoderLinks;
  FFApplyTranscodePreset(FEncoder, FWriter, FPreset);
  FCopyAudio := FEncoder.CopyAudio;
  FilterDesc := TFFTranscodePresetHelper.SuggestedFilter(FPreset);
  if (FilterDesc <> '') and (FFrameFilter <> nil) then
    FFrameFilter.FilterDescription := FilterDesc;
end;

procedure TFFTranscodeJob.ConfigureOutput(const AOutputFileName: string);
begin
  FFConfigureWriterFormat(FWriter, AOutputFileName, FPreset);
end;

procedure TFFTranscodeJob.Start;
begin
  ApplyPreset;
  if FEncoder <> nil then
    FEncoder.Start;
end;

procedure TFFTranscodeJob.Stop;
begin
  if FEncoder <> nil then
    FEncoder.Stop;
end;

procedure TFFTranscodeJob.Pause;
begin
  if FEncoder <> nil then
    FEncoder.Pause;
end;

procedure TFFTranscodeJob.Resume;
begin
  if FEncoder <> nil then
    FEncoder.Resume;
end;

end.
