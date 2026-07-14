unit uMultiDemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, ffmpeg_types, libavutil,
  uFFLoader, uFFLogger, uFFReader, uFFDecoder, uFFEncoder, uFFWriter,
  uFFMediaInfo, uFFThumbnailExtractor, uFFPlayerControl, uFFSubtitleDecoder,
  uFFTranscodePreset, uFFRemuxJob, uFFFrameFilter,
  uFFFrame, uFFFrameConverter, uFFFrameBitmap;

type
  TFormMultiDemo = class(TForm)
  private
    FPage: TPageControl;
    FTabPlayer, FTabTranscode, FTabProbe, FTabLog: TTabSheet;

    FEdtFile: TEdit;
    FBtnOpen: TButton;
    FLblStatus: TLabel;
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    FOpenSrtDialog: TOpenDialog;
    FCurrentFile: string;

    FLoader: TFFLoader;
    FLogger: TFFLogger;
    FMediaInfo: TFFMediaInfo;

    FPlayerControl: TFFPlayerControl;
    FSubtitleDecoder: TFFSubtitleDecoder;
    FSubtitleReader: TFFReader;

    FReader: TFFReader;
    FDecoder: TFFDecoder;
    FEncoder: TFFEncoder;
    FWriter: TFFWriter;
    FThumbnail: TFFThumbnailExtractor;
    FTranscodeJob: TFFTranscodeJob;
    FRemuxJob: TFFRemuxJob;
    FFrameFilter: TFFFrameFilter;
    FEdtDest, FEdtStartMs, FEdtEndMs, FEdtFilter: TEdit;
    FBtnBrowseOut, FBtnStart, FBtnStop, FBtnRemux: TButton;
    FCmbPreset: TComboBox;
    FProgress: TProgressBar;
    FLblTranscodeStatus: TLabel;
    FMemoTranscodeInfo: TMemo;
    FImgPreview: TImage;
    FChkCopyAudio: TCheckBox;

    FProbeThumbnail: TFFThumbnailExtractor;
    FMemoStreams: TMemo;
    FImgThumbA, FImgThumbB, FImgThumbC: TImage;
    FLblThumbA, FLblThumbB, FLblThumbC: TLabel;
    FBtnRefreshProbe: TButton;

    FMemoLog: TMemo;
    FBtnClearLog: TButton;

    FMemoSubtitles: TMemo;
    FBtnLoadEmbeddedSubs, FBtnLoadSrt: TButton;

    procedure DoOpen(Sender: TObject);
    procedure DoLog(Sender: TObject; ALevel: TFFLogLevel; const AMessage: string);
    procedure DoClearLog(Sender: TObject);

    procedure LoadMedia(const AFileName: string);
    procedure UpdatePlayerTab;
    procedure UpdateProbeTab;
    procedure UpdateSubtitleTab;

    procedure DoBrowseOut(Sender: TObject);
    procedure DoStartTranscode(Sender: TObject);
    procedure DoRemux(Sender: TObject);
    procedure DoStopTranscode(Sender: TObject);
    procedure DoTranscodeProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
    procedure DoRemuxProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
    procedure DoRemuxStateChange(Sender: TObject);
    procedure DoPreview(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64);
    procedure DoEncoderStateChange(Sender: TObject);
    procedure DoRefreshProbe(Sender: TObject);
    procedure DoLoadEmbeddedSubs(Sender: TObject);
    procedure DoLoadSrt(Sender: TObject);

    function FindVideoStreamIndex: Integer;
    function FindSubtitleStreamIndex: Integer;
    function ParseMs(const AText: string): Int64;
    procedure ApplyClipAndFilter;
    procedure LoadTranscodeSourceInfo(const AFileName: string);
    procedure ExtractThumbAt(APositionMs: Int64; AImage: TImage; ACaption: TLabel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMultiDemo: TFormMultiDemo;

implementation

{$R *.dfm}

constructor TFormMultiDemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLoader := TFFLoader.Create(Self);
  FLogger := TFFLogger.Create(Self);
  FLogger.Active := True;
  FLogger.Level := llInfo;
  FLogger.OnLog := DoLog;

  FMediaInfo := TFFMediaInfo.Create(Self);

  FEdtFile := TEdit.Create(Self);
  FEdtFile.Parent := Self;
  FEdtFile.SetBounds(8, 10, 760, 23);
  FEdtFile.ReadOnly := True;

  FBtnOpen := TButton.Create(Self);
  FBtnOpen.Parent := Self;
  FBtnOpen.Caption := 'Open media...';
  FBtnOpen.SetBounds(776, 8, 100, 25);
  FBtnOpen.OnClick := DoOpen;

  FLblStatus := TLabel.Create(Self);
  FLblStatus.Parent := Self;
  FLblStatus.SetBounds(888, 12, 120, 16);
  FLblStatus.Caption := 'No file';

  FPage := TPageControl.Create(Self);
  FPage.Parent := Self;
  FPage.SetBounds(0, 40, ClientWidth, ClientHeight - 40);
  FPage.Anchors := [akLeft, akTop, akRight, akBottom];

  FTabPlayer := TTabSheet.Create(Self);
  FTabPlayer.PageControl := FPage;
  FTabPlayer.Caption := 'Player';

  FTabTranscode := TTabSheet.Create(Self);
  FTabTranscode.PageControl := FPage;
  FTabTranscode.Caption := 'Transcode / Remux';

  FTabProbe := TTabSheet.Create(Self);
  FTabProbe.PageControl := FPage;
  FTabProbe.Caption := 'Media probe';

  FTabLog := TTabSheet.Create(Self);
  FTabLog.PageControl := FPage;
  FTabLog.Caption := 'FFmpeg log';

  { --- Player tab --- }
  FPlayerControl := TFFPlayerControl.Create(Self);
  FPlayerControl.Parent := FTabPlayer;
  FPlayerControl.Align := alClient;

  FSubtitleReader := TFFReader.Create(Self);
  FSubtitleDecoder := TFFSubtitleDecoder.Create(Self);
  FSubtitleDecoder.Reader := FSubtitleReader;
  FPlayerControl.SubtitleDecoder := FSubtitleDecoder;

  { --- Transcode tab --- }
  FReader := TFFReader.Create(Self);
  FDecoder := TFFDecoder.Create(Self);
  FEncoder := TFFEncoder.Create(Self);
  FWriter := TFFWriter.Create(Self);
  FThumbnail := TFFThumbnailExtractor.Create(Self);
  FThumbnail.MaxWidth := 240;
  FThumbnail.MaxHeight := 180;
  FFrameFilter := TFFFrameFilter.Create(Self);
  FRemuxJob := TFFRemuxJob.Create(Self);
  FRemuxJob.Reader := FReader;
  FRemuxJob.Writer := FWriter;
  FRemuxJob.OnProgress := DoRemuxProgress;
  FRemuxJob.OnStateChange := DoRemuxStateChange;

  FTranscodeJob := TFFTranscodeJob.Create(Self);
  FTranscodeJob.Reader := FReader;
  FTranscodeJob.InputDecoder := FDecoder;
  FTranscodeJob.Encoder := FEncoder;
  FTranscodeJob.Writer := FWriter;
  FTranscodeJob.FrameFilter := FFrameFilter;
  FTranscodeJob.Preset := ftpMpeg4_800k;
  FTranscodeJob.CopyAudio := True;

  FEncoder.OnProgress := DoTranscodeProgress;
  FEncoder.OnPreviewFrame := DoPreview;
  FEncoder.OnStateChange := DoEncoderStateChange;
  FEncoder.CopyAudio := True;

  FEdtDest := TEdit.Create(Self);
  FEdtDest.Parent := FTabTranscode;
  FEdtDest.SetBounds(8, 8, 560, 23);

  FBtnBrowseOut := TButton.Create(Self);
  FBtnBrowseOut.Parent := FTabTranscode;
  FBtnBrowseOut.Caption := 'Output...';
  FBtnBrowseOut.SetBounds(574, 6, 75, 25);
  FBtnBrowseOut.OnClick := DoBrowseOut;

  FEdtStartMs := TEdit.Create(Self);
  FEdtStartMs.Parent := FTabTranscode;
  FEdtStartMs.SetBounds(8, 40, 80, 23);
  FEdtStartMs.Text := '0';

  FEdtEndMs := TEdit.Create(Self);
  FEdtEndMs.Parent := FTabTranscode;
  FEdtEndMs.SetBounds(100, 40, 80, 23);
  FEdtEndMs.Text := '0';

  FEdtFilter := TEdit.Create(Self);
  FEdtFilter.Parent := FTabTranscode;
  FEdtFilter.SetBounds(192, 40, 300, 23);
  FEdtFilter.Hint := 'Optional libavfilter, e.g. scale=640:480';
  FEdtFilter.ShowHint := True;

  FCmbPreset := TComboBox.Create(Self);
  FCmbPreset.Parent := FTabTranscode;
  FCmbPreset.Style := csDropDownList;
  FCmbPreset.SetBounds(660, 36, 220, 23);
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpMpeg4_800k));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_Medium));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_Fast));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_High));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpWebM_VP9));
  FCmbPreset.ItemIndex := 0;

  FChkCopyAudio := TCheckBox.Create(Self);
  FChkCopyAudio.Parent := FTabTranscode;
  FChkCopyAudio.Caption := 'Copy audio';
  FChkCopyAudio.Checked := True;
  FChkCopyAudio.SetBounds(660, 8, 100, 17);

  FBtnStart := TButton.Create(Self);
  FBtnStart.Parent := FTabTranscode;
  FBtnStart.Caption := 'Transcode';
  FBtnStart.SetBounds(660, 64, 75, 25);
  FBtnStart.OnClick := DoStartTranscode;

  FBtnRemux := TButton.Create(Self);
  FBtnRemux.Parent := FTabTranscode;
  FBtnRemux.Caption := 'Remux';
  FBtnRemux.SetBounds(740, 64, 75, 25);
  FBtnRemux.OnClick := DoRemux;

  FBtnStop := TButton.Create(Self);
  FBtnStop.Parent := FTabTranscode;
  FBtnStop.Caption := 'Stop';
  FBtnStop.SetBounds(820, 64, 75, 25);
  FBtnStop.OnClick := DoStopTranscode;
  FBtnStop.Enabled := False;

  FProgress := TProgressBar.Create(Self);
  FProgress.Parent := FTabTranscode;
  FProgress.SetBounds(8, 96, 884, 17);
  FProgress.Min := 0;
  FProgress.Max := 1000;

  FLblTranscodeStatus := TLabel.Create(Self);
  FLblTranscodeStatus.Parent := FTabTranscode;
  FLblTranscodeStatus.SetBounds(8, 118, 400, 16);
  FLblTranscodeStatus.Caption := 'Ready';

  FImgPreview := TImage.Create(Self);
  FImgPreview.Parent := FTabTranscode;
  FImgPreview.SetBounds(660, 140, 240, 180);
  FImgPreview.Proportional := True;
  FImgPreview.Stretch := True;

  FMemoTranscodeInfo := TMemo.Create(Self);
  FMemoTranscodeInfo.Parent := FTabTranscode;
  FMemoTranscodeInfo.SetBounds(8, 140, 640, 400);
  FMemoTranscodeInfo.ReadOnly := True;
  FMemoTranscodeInfo.ScrollBars := ssVertical;
  FMemoTranscodeInfo.Anchors := [akLeft, akTop, akRight, akBottom];

  { --- Probe tab --- }
  FProbeThumbnail := TFFThumbnailExtractor.Create(Self);
  FProbeThumbnail.MaxWidth := 200;
  FProbeThumbnail.MaxHeight := 150;

  FBtnRefreshProbe := TButton.Create(Self);
  FBtnRefreshProbe.Parent := FTabProbe;
  FBtnRefreshProbe.Caption := 'Refresh thumbnails';
  FBtnRefreshProbe.SetBounds(8, 8, 140, 25);
  FBtnRefreshProbe.OnClick := DoRefreshProbe;

  FLblThumbA := TLabel.Create(Self);
  FLblThumbA.Parent := FTabProbe;
  FLblThumbA.SetBounds(8, 40, 200, 16);
  FLblThumbA.Caption := 'Start';

  FImgThumbA := TImage.Create(Self);
  FImgThumbA.Parent := FTabProbe;
  FImgThumbA.SetBounds(8, 56, 200, 150);
  FImgThumbA.Proportional := True;
  FImgThumbA.Stretch := True;

  FLblThumbB := TLabel.Create(Self);
  FLblThumbB.Parent := FTabProbe;
  FLblThumbB.SetBounds(220, 40, 200, 16);
  FLblThumbB.Caption := '25%';

  FImgThumbB := TImage.Create(Self);
  FImgThumbB.Parent := FTabProbe;
  FImgThumbB.SetBounds(220, 56, 200, 150);
  FImgThumbB.Proportional := True;
  FImgThumbB.Stretch := True;

  FLblThumbC := TLabel.Create(Self);
  FLblThumbC.Parent := FTabProbe;
  FLblThumbC.SetBounds(432, 40, 200, 16);
  FLblThumbC.Caption := '50%';

  FImgThumbC := TImage.Create(Self);
  FImgThumbC.Parent := FTabProbe;
  FImgThumbC.SetBounds(432, 56, 200, 150);
  FImgThumbC.Proportional := True;
  FImgThumbC.Stretch := True;

  FMemoStreams := TMemo.Create(Self);
  FMemoStreams.Parent := FTabProbe;
  FMemoStreams.SetBounds(8, 220, 860, 320);
  FMemoStreams.ReadOnly := True;
  FMemoStreams.ScrollBars := ssVertical;
  FMemoStreams.Anchors := [akLeft, akTop, akRight, akBottom];

  FBtnLoadEmbeddedSubs := TButton.Create(Self);
  FBtnLoadEmbeddedSubs.Parent := FTabProbe;
  FBtnLoadEmbeddedSubs.Caption := 'Load embedded subtitles';
  FBtnLoadEmbeddedSubs.SetBounds(650, 8, 160, 25);
  FBtnLoadEmbeddedSubs.OnClick := DoLoadEmbeddedSubs;

  FBtnLoadSrt := TButton.Create(Self);
  FBtnLoadSrt.Parent := FTabProbe;
  FBtnLoadSrt.Caption := 'Load .srt...';
  FBtnLoadSrt.SetBounds(650, 40, 160, 25);
  FBtnLoadSrt.OnClick := DoLoadSrt;

  FMemoSubtitles := TMemo.Create(Self);
  FMemoSubtitles.Parent := FTabProbe;
  FMemoSubtitles.SetBounds(650, 72, 220, 140);
  FMemoSubtitles.ReadOnly := True;
  FMemoSubtitles.ScrollBars := ssVertical;

  { --- Log tab --- }
  FMemoLog := TMemo.Create(Self);
  FMemoLog.Parent := FTabLog;
  FMemoLog.Align := alClient;
  FMemoLog.ReadOnly := True;
  FMemoLog.ScrollBars := ssBoth;
  FMemoLog.Font.Name := 'Consolas';

  FBtnClearLog := TButton.Create(Self);
  FBtnClearLog.Parent := FTabLog;
  FBtnClearLog.Caption := 'Clear';
  FBtnClearLog.SetBounds(8, 8, 75, 25);
  FBtnClearLog.OnClick := DoClearLog;

  FOpenDialog := TOpenDialog.Create(Self);
  FOpenDialog.Filter := 'Media files|*.avi;*.mkv;*.mp4;*.mov;*.wmv;*.mp3;*.wav;*.srt|All files|*.*';
  FOpenDialog.Options := [ofFileMustExist];

  FSaveDialog := TSaveDialog.Create(Self);
  FSaveDialog.Filter := 'MKV|*.mkv|MP4|*.mp4|All files|*.*';
  FSaveDialog.DefaultExt := 'mkv';

  FOpenSrtDialog := TOpenDialog.Create(Self);
  FOpenSrtDialog.Filter := 'SubRip|*.srt|All files|*.*';
end;

destructor TFormMultiDemo.Destroy;
begin
  if FPlayerControl <> nil then
    FPlayerControl.Stop;
  if (FEncoder <> nil) and (FEncoder.State = esRunning) then
    FEncoder.Stop;
  if (FRemuxJob <> nil) and (FRemuxJob.State = rsRunning) then
    FRemuxJob.Stop;
  inherited;
end;

function TFormMultiDemo.ParseMs(const AText: string): Int64;
begin
  Result := StrToInt64Def(Trim(AText), 0);
  if Result < 0 then
    Result := 0;
end;

function TFormMultiDemo.FindVideoStreamIndex: Integer;
begin
  Result := FMediaInfo.FindBestStream(AVMEDIA_TYPE_VIDEO);
end;

function TFormMultiDemo.FindSubtitleStreamIndex: Integer;
begin
  Result := FMediaInfo.FindBestStream(AVMEDIA_TYPE_SUBTITLE);
end;

procedure TFormMultiDemo.DoLog(Sender: TObject; ALevel: TFFLogLevel; const AMessage: string);
var
  LevelName: string;
begin
  case ALevel of
    llQuiet: LevelName := 'QUIET';
    llPanic: LevelName := 'PANIC';
    llFatal: LevelName := 'FATAL';
    llError: LevelName := 'ERROR';
    llWarning: LevelName := 'WARNING';
    llInfo: LevelName := 'INFO';
    llVerbose: LevelName := 'VERBOSE';
    llDebug: LevelName := 'DEBUG';
  else
    LevelName := 'TRACE';
  end;
  if FMemoLog.Lines.Count > 4000 then
    FMemoLog.Lines.Delete(0);
  FMemoLog.Lines.Add(Format('[%s] %s', [LevelName, Trim(AMessage)]));
end;

procedure TFormMultiDemo.DoClearLog(Sender: TObject);
begin
  FMemoLog.Clear;
end;

procedure TFormMultiDemo.ExtractThumbAt(APositionMs: Int64; AImage: TImage; ACaption: TLabel);
var
  Bmp: TBitmap;
  BaseCaption: string;
begin
  if FCurrentFile = '' then
    Exit;
  BaseCaption := ACaption.Caption;
  FProbeThumbnail.FileName := FCurrentFile;
  FProbeThumbnail.PositionMs := APositionMs;
  Bmp := TBitmap.Create;
  try
    try
      FProbeThumbnail.ExtractToBitmap(Bmp);
      AImage.Picture.Bitmap.Assign(Bmp);
      ACaption.Caption := Format('%s @ %s', [BaseCaption, FFFormatDurationMs(APositionMs)]);
    except
      on E: Exception do
        ACaption.Caption := BaseCaption + ': ' + E.Message;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TFormMultiDemo.UpdateProbeTab;
var
  I: Integer;
  Info: TFFMediaStreamInfo;
  Lines: TStringList;
  DurMs: Int64;
begin
  if FCurrentFile = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Add(FMediaInfo.SummaryText);
    Lines.Add('');
    Lines.Add('Streams:');
    for I := 0 to FMediaInfo.StreamCount - 1 do
    begin
      Info := FMediaInfo.GetStreamInfo(I);
      case Info.MediaType of
        AVMEDIA_TYPE_VIDEO:
          Lines.Add(Format('#%d video %s %dx%d %s', [Info.Index, Info.CodecName, Info.Width, Info.Height,
            Info.PixelFormatName]));
        AVMEDIA_TYPE_AUDIO:
          Lines.Add(Format('#%d audio %s %d Hz ch=%d', [Info.Index, Info.CodecName, Info.SampleRate,
            Info.Channels]));
        AVMEDIA_TYPE_SUBTITLE:
          Lines.Add(Format('#%d subtitle %s', [Info.Index, Info.CodecName]));
      else
        Lines.Add(Format('#%d type=%d %s', [Info.Index, Ord(Info.MediaType), Info.CodecName]));
      end;
    end;
    FMemoStreams.Lines.Assign(Lines);
  finally
    Lines.Free;
  end;

  DurMs := FMediaInfo.DurationMs;
  FLblThumbA.Caption := 'Start';
  FLblThumbB.Caption := '25%';
  FLblThumbC.Caption := '50%';
  ExtractThumbAt(0, FImgThumbA, FLblThumbA);
  if DurMs > 0 then
  begin
    ExtractThumbAt(DurMs div 4, FImgThumbB, FLblThumbB);
    ExtractThumbAt(DurMs div 2, FImgThumbC, FLblThumbC);
  end;
end;

procedure TFormMultiDemo.UpdateSubtitleTab;
var
  I, DurMs: Int64;
  LastText: string;
  S: string;
begin
  FMemoSubtitles.Clear;
  if not FSubtitleDecoder.HasEvents then
  begin
    FMemoSubtitles.Lines.Add('No subtitle events loaded.');
    Exit;
  end;

  FMemoSubtitles.Lines.Add(Format('Events: %d', [FSubtitleDecoder.EventCount]));
  DurMs := FMediaInfo.DurationMs;
  if DurMs <= 0 then
    DurMs := 120000;
  LastText := '';
  I := 0;
  while I <= DurMs do
  begin
    S := FSubtitleDecoder.GetTextAt(I);
    if (S <> '') and (S <> LastText) then
    begin
      FMemoSubtitles.Lines.Add(Format('[%s] %s', [FFFormatDurationMs(I), S]));
      LastText := S;
    end;
    Inc(I, 500);
  end;
end;

procedure TFormMultiDemo.UpdatePlayerTab;
var
  SubIdx: Integer;
begin
  if FCurrentFile = '' then
    Exit;

  FPlayerControl.Stop;
  FPlayerControl.FileName := FCurrentFile;

  SubIdx := FindSubtitleStreamIndex;
  FSubtitleDecoder.ClearEvents;
  if SubIdx >= 0 then
  begin
    FSubtitleReader.FileName := FCurrentFile;
    FSubtitleDecoder.StreamIndex := SubIdx;
    try
      FSubtitleDecoder.Initialize;
      FSubtitleDecoder.LoadAll;
    except
      on E: Exception do
        FMemoSubtitles.Lines.Add('Subtitle load: ' + E.Message);
    end;
  end;
end;

procedure TFormMultiDemo.LoadTranscodeSourceInfo(const AFileName: string);
var
  Bmp: TBitmap;
  VideoIdx: Integer;
  Info: TFFMediaStreamInfo;
begin
  FMemoTranscodeInfo.Lines.Text := FMediaInfo.SummaryText;

  FThumbnail.FileName := AFileName;
  Bmp := TBitmap.Create;
  try
    FThumbnail.ExtractToBitmap(Bmp);
    FImgPreview.Picture.Bitmap.Assign(Bmp);
  finally
    Bmp.Free;
  end;

  VideoIdx := FindVideoStreamIndex;
  if VideoIdx >= 0 then
  begin
    Info := FMediaInfo.GetStreamInfo(VideoIdx);
    FDecoder.StreamIndex := VideoIdx;
    FEncoder.Width := Info.Width;
    FEncoder.Height := Info.Height;
    if (Info.FrameRateNum > 0) and (Info.FrameRateDen > 0) then
    begin
      FEncoder.FrameRateNum := Info.FrameRateNum;
      FEncoder.FrameRateDen := Info.FrameRateDen;
      FEncoder.TimeBaseNum := Info.FrameRateDen;
      FEncoder.TimeBaseDen := Info.FrameRateNum;
    end;
  end;
end;

procedure TFormMultiDemo.LoadMedia(const AFileName: string);
begin
  FCurrentFile := AFileName;
  FEdtFile.Text := AFileName;
  FLblStatus.Caption := ExtractFileName(AFileName);

  FMediaInfo.FileName := AFileName;
  FMediaInfo.Probe;

  UpdatePlayerTab;
  LoadTranscodeSourceInfo(AFileName);
  UpdateProbeTab;
  UpdateSubtitleTab;
end;

procedure TFormMultiDemo.DoOpen(Sender: TObject);
begin
  if FOpenDialog.Execute then
    LoadMedia(FOpenDialog.FileName);
end;

procedure TFormMultiDemo.DoRefreshProbe(Sender: TObject);
begin
  UpdateProbeTab;
end;

procedure TFormMultiDemo.DoLoadEmbeddedSubs(Sender: TObject);
var
  SubIdx: Integer;
begin
  if FCurrentFile = '' then
  begin
    ShowMessage('Open a media file first');
    Exit;
  end;
  SubIdx := FindSubtitleStreamIndex;
  if SubIdx < 0 then
  begin
    ShowMessage('No subtitle stream in this file');
    Exit;
  end;
  FSubtitleReader.FileName := FCurrentFile;
  FSubtitleDecoder.StreamIndex := SubIdx;
  FSubtitleDecoder.Initialize;
  FSubtitleDecoder.LoadAll;
  UpdateSubtitleTab;
end;

procedure TFormMultiDemo.DoLoadSrt(Sender: TObject);
begin
  if FOpenSrtDialog.Execute then
  begin
    FSubtitleDecoder.LoadFromSrt(FOpenSrtDialog.FileName);
    UpdateSubtitleTab;
  end;
end;

procedure TFormMultiDemo.ApplyClipAndFilter;
begin
  FTranscodeJob.StartMs := ParseMs(FEdtStartMs.Text);
  FTranscodeJob.EndMs := ParseMs(FEdtEndMs.Text);
  if Trim(FEdtFilter.Text) <> '' then
  begin
    FFrameFilter.FilterDescription := Trim(FEdtFilter.Text);
    FTranscodeJob.FrameFilter := FFrameFilter;
  end
  else
    FTranscodeJob.FrameFilter := nil;
end;

procedure TFormMultiDemo.DoBrowseOut(Sender: TObject);
begin
  if FSaveDialog.Execute then
    FEdtDest.Text := FSaveDialog.FileName;
end;

procedure TFormMultiDemo.DoStartTranscode(Sender: TObject);
begin
  if FCurrentFile = '' then
  begin
    ShowMessage('Open input file first');
    Exit;
  end;
  if FEdtDest.Text = '' then
  begin
    ShowMessage('Select output file');
    Exit;
  end;

  if FEncoder.State = esRunning then
    FEncoder.Stop;

  FReader.FileName := FCurrentFile;
  FDecoder.Reader := FReader;
  if FDecoder.StreamIndex < 0 then
    FDecoder.StreamIndex := FindVideoStreamIndex;
  if FDecoder.StreamIndex < 0 then
  begin
    ShowMessage('No video stream');
    Exit;
  end;

  FWriter.FileName := FEdtDest.Text;
  FTranscodeJob.CopyAudio := FChkCopyAudio.Checked;
  case FCmbPreset.ItemIndex of
    1: FTranscodeJob.Preset := ftpH264_Medium;
    2: FTranscodeJob.Preset := ftpH264_Fast;
    3: FTranscodeJob.Preset := ftpH264_High;
    4: FTranscodeJob.Preset := ftpWebM_VP9;
  else
    FTranscodeJob.Preset := ftpMpeg4_800k;
  end;
  FTranscodeJob.ConfigureOutput(FEdtDest.Text);
  FTranscodeJob.ApplyPreset;
  ApplyClipAndFilter;

  FProgress.Position := 0;
  FLblTranscodeStatus.Caption := 'Transcoding...';
  FBtnStart.Enabled := False;
  FBtnRemux.Enabled := False;
  FBtnStop.Enabled := True;
  FTranscodeJob.Start;
end;

procedure TFormMultiDemo.DoRemux(Sender: TObject);
begin
  if FCurrentFile = '' then
  begin
    ShowMessage('Open input file first');
    Exit;
  end;
  if FEdtDest.Text = '' then
  begin
    ShowMessage('Select output file');
    Exit;
  end;

  if FEncoder.State = esRunning then
    FEncoder.Stop;
  if FRemuxJob.State = rsRunning then
    FRemuxJob.Stop;

  FReader.FileName := FCurrentFile;
  FWriter.FileName := FEdtDest.Text;
  if LowerCase(ExtractFileExt(FEdtDest.Text)) = '.mp4' then
    FWriter.FormatName := 'mp4'
  else
    FWriter.FormatName := 'matroska';

  FRemuxJob.StartMs := ParseMs(FEdtStartMs.Text);
  FRemuxJob.EndMs := ParseMs(FEdtEndMs.Text);

  FProgress.Position := 0;
  FLblTranscodeStatus.Caption := 'Remuxing...';
  FBtnStart.Enabled := False;
  FBtnRemux.Enabled := False;
  FBtnStop.Enabled := True;
  FRemuxJob.Start;
end;

procedure TFormMultiDemo.DoStopTranscode(Sender: TObject);
begin
  FEncoder.Stop;
  FRemuxJob.Stop;
end;

procedure TFormMultiDemo.DoTranscodeProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
begin
  TThread.Queue(nil,
    procedure
    begin
      if ADurationMs > 0 then
        FProgress.Position := Min(FProgress.Max, Round(APositionMs / ADurationMs * FProgress.Max))
      else
        FProgress.Position := 0;
      FLblTranscodeStatus.Caption := Format('%s / %s', [FFFormatDurationMs(APositionMs),
        FFFormatDurationMs(ADurationMs)]);
    end);
end;

procedure TFormMultiDemo.DoRemuxProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
begin
  DoTranscodeProgress(Sender, APositionMs, ADurationMs);
end;

procedure TFormMultiDemo.DoRemuxStateChange(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      case FRemuxJob.State of
        rsStopped:
          if FEncoder.State = esStopped then
          begin
            FLblTranscodeStatus.Caption := 'Finished';
            FBtnStart.Enabled := True;
            FBtnRemux.Enabled := True;
            FBtnStop.Enabled := False;
          end;
        rsRunning:
          FLblTranscodeStatus.Caption := 'Remuxing...';
        rsStopping:
          FLblTranscodeStatus.Caption := 'Stopping...';
      end;
    end);
end;

procedure TFormMultiDemo.DoEncoderStateChange(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      case FEncoder.State of
        esStopped:
          begin
            FLblTranscodeStatus.Caption := 'Finished';
            FBtnStart.Enabled := True;
            FBtnRemux.Enabled := True;
            FBtnStop.Enabled := False;
          end;
        esRunning:
          FLblTranscodeStatus.Caption := 'Running...';
        esPaused:
          FLblTranscodeStatus.Caption := 'Paused';
        esStopping:
          FLblTranscodeStatus.Caption := 'Stopping...';
      end;
    end);
end;

procedure TFormMultiDemo.DoPreview(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64);
var
  Converter: TFFFrameConverter;
  Converted: PAVFrame;
  LocalBmp: TBitmap;
begin
  if AFrame = nil then
    Exit;
  LocalBmp := TBitmap.Create;
  Converter := TFFFrameConverter.Create;
  try
    Converted := Converter.Convert(AFrame);
    TFFFrameBitmap.AssignBgraFrame(Converted, Converter.DstWidth, Converter.DstHeight, LocalBmp);
    TThread.Queue(nil,
      procedure
      begin
        FImgPreview.Picture.Bitmap.Assign(LocalBmp);
        LocalBmp.Free;
      end);
    LocalBmp := nil;
  finally
    Converter.Free;
    if LocalBmp <> nil then
      LocalBmp.Free;
  end;
end;

end.
