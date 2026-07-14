unit uDemoTranscodeForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, ffmpeg_types, libavutil,
  uFFReader, uFFDecoder, uFFEncoder, uFFWriter, uFFMediaInfo, uFFThumbnailExtractor,
  uFFTranscodePreset, uFFRemuxJob, uFFFrameFilter,
  uFFFrame, uFFFrameConverter, uFFFrameBitmap;

type
  TFormDemoTranscode = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FReader: TFFReader;
    FDecoder: TFFDecoder;
    FEncoder: TFFEncoder;
    FWriter: TFFWriter;
    FMediaInfo: TFFMediaInfo;
    FThumbnail: TFFThumbnailExtractor;
    FTranscodeJob: TFFTranscodeJob;
    FRemuxJob: TFFRemuxJob;
    FFrameFilter: TFFFrameFilter;
    FEdtSource, FEdtDest, FEdtStartMs, FEdtEndMs, FEdtFilter: TEdit;
    FBtnBrowseIn, FBtnBrowseOut, FBtnStart, FBtnStop, FBtnRemux: TButton;
    FCmbPreset: TComboBox;
    FProgress: TProgressBar;
    FLblStatus: TLabel;
    FMemoInfo: TMemo;
    FImgPreview: TImage;
    FChkCopyAudio: TCheckBox;
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    procedure DoBrowseIn(Sender: TObject);
    procedure DoBrowseOut(Sender: TObject);
    procedure DoStart(Sender: TObject);
    procedure DoRemux(Sender: TObject);
    procedure DoStop(Sender: TObject);
    procedure DoProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
    procedure DoRemuxProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
    procedure DoRemuxStateChange(Sender: TObject);
    procedure DoPreview(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64);
    procedure DoStateChange(Sender: TObject);
    procedure LoadSourceInfo(const AFileName: string);
    function FindVideoStreamIndex: Integer;
    function ParseMs(const AText: string): Int64;
    procedure ApplyClipAndFilter;
  public
  end;

var
  FormDemoTranscode: TFormDemoTranscode;

implementation

{$R *.dfm}

procedure TFormDemoTranscode.FormCreate(Sender: TObject);
begin
  Caption := 'Delphi-FFMPEG Demo Transcode';
  Width := 900;
  Height := 560;
  Position := poScreenCenter;

  FReader := TFFReader.Create(Self);
  FDecoder := TFFDecoder.Create(Self);
  FEncoder := TFFEncoder.Create(Self);
  FWriter := TFFWriter.Create(Self);
  FMediaInfo := TFFMediaInfo.Create(Self);
  FThumbnail := TFFThumbnailExtractor.Create(Self);
  FThumbnail.MaxWidth := 240;
  FThumbnail.MaxHeight := 180;

  FTranscodeJob := TFFTranscodeJob.Create(Self);
  FTranscodeJob.Reader := FReader;
  FTranscodeJob.InputDecoder := FDecoder;
  FTranscodeJob.Encoder := FEncoder;
  FTranscodeJob.Writer := FWriter;
  FTranscodeJob.Preset := ftpMpeg4_800k;
  FTranscodeJob.CopyAudio := True;

  FFrameFilter := TFFFrameFilter.Create(Self);
  FRemuxJob := TFFRemuxJob.Create(Self);
  FRemuxJob.Reader := FReader;
  FRemuxJob.Writer := FWriter;
  FRemuxJob.OnProgress := DoRemuxProgress;
  FRemuxJob.OnStateChange := DoRemuxStateChange;

  FTranscodeJob.FrameFilter := FFrameFilter;
  FEncoder.OnProgress := DoProgress;
  FEncoder.OnPreviewFrame := DoPreview;
  FEncoder.OnStateChange := DoStateChange;
  FEncoder.CopyAudio := True;

  FEdtStartMs := TEdit.Create(Self);
  FEdtStartMs.Parent := Self;
  FEdtStartMs.SetBounds(8, 68, 80, 23);
  FEdtStartMs.Text := '0';

  FEdtEndMs := TEdit.Create(Self);
  FEdtEndMs.Parent := Self;
  FEdtEndMs.SetBounds(100, 68, 80, 23);
  FEdtEndMs.Text := '0';

  FEdtFilter := TEdit.Create(Self);
  FEdtFilter.Parent := Self;
  FEdtFilter.SetBounds(192, 68, 300, 23);
  FEdtFilter.Text := '';
  FEdtFilter.Hint := 'Optional filter, e.g. scale=640:480';
  FEdtFilter.ShowHint := True;

  FCmbPreset := TComboBox.Create(Self);
  FCmbPreset.Parent := Self;
  FCmbPreset.Style := csDropDownList;
  FCmbPreset.SetBounds(660, 96, 220, 23);
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpMpeg4_800k));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_Medium));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_Fast));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_High));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpH264_NVENC));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpYouTube_1080p));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpWeb_Preview_480p));
  FCmbPreset.Items.Add(TFFTranscodePresetHelper.DisplayName(ftpWebM_VP9));
  FCmbPreset.ItemIndex := 0;

  FEdtSource := TEdit.Create(Self);
  FEdtSource.Parent := Self;
  FEdtSource.SetBounds(8, 12, 560, 23);

  FBtnBrowseIn := TButton.Create(Self);
  FBtnBrowseIn.Parent := Self;
  FBtnBrowseIn.Caption := 'Input...';
  FBtnBrowseIn.SetBounds(574, 10, 75, 25);
  FBtnBrowseIn.OnClick := DoBrowseIn;

  FEdtDest := TEdit.Create(Self);
  FEdtDest.Parent := Self;
  FEdtDest.SetBounds(8, 44, 560, 23);

  FBtnBrowseOut := TButton.Create(Self);
  FBtnBrowseOut.Parent := Self;
  FBtnBrowseOut.Caption := 'Output...';
  FBtnBrowseOut.SetBounds(574, 42, 75, 25);
  FBtnBrowseOut.OnClick := DoBrowseOut;

  FChkCopyAudio := TCheckBox.Create(Self);
  FChkCopyAudio.Parent := Self;
  FChkCopyAudio.Caption := 'Copy audio';
  FChkCopyAudio.Checked := True;
  FChkCopyAudio.SetBounds(660, 12, 100, 17);

  FBtnStart := TButton.Create(Self);
  FBtnStart.Parent := Self;
  FBtnStart.Caption := 'Start';
  FBtnStart.SetBounds(660, 40, 75, 25);
  FBtnStart.OnClick := DoStart;

  FBtnStop := TButton.Create(Self);
  FBtnStop.Parent := Self;
  FBtnStop.Caption := 'Stop';
  FBtnStop.SetBounds(740, 40, 75, 25);
  FBtnStop.OnClick := DoStop;
  FBtnStop.Enabled := False;

  FBtnRemux := TButton.Create(Self);
  FBtnRemux.Parent := Self;
  FBtnRemux.Caption := 'Remux';
  FBtnRemux.SetBounds(820, 40, 75, 25);
  FBtnRemux.OnClick := DoRemux;

  FProgress := TProgressBar.Create(Self);
  FProgress.Parent := Self;
  FProgress.SetBounds(8, 100, 860, 17);
  FProgress.Min := 0;
  FProgress.Max := 1000;

  FLblStatus := TLabel.Create(Self);
  FLblStatus.Parent := Self;
  FLblStatus.SetBounds(8, 122, 400, 16);
  FLblStatus.Caption := 'Ready';

  FImgPreview := TImage.Create(Self);
  FImgPreview.Parent := Self;
  FImgPreview.SetBounds(660, 148, 240, 180);
  FImgPreview.Proportional := True;
  FImgPreview.Stretch := True;

  FMemoInfo := TMemo.Create(Self);
  FMemoInfo.Parent := Self;
  FMemoInfo.SetBounds(8, 148, 640, 372);
  FMemoInfo.ReadOnly := True;
  FMemoInfo.ScrollBars := ssVertical;

  FOpenDialog := TOpenDialog.Create(Self);
  FOpenDialog.Filter := 'Media files|*.avi;*.mkv;*.mp4;*.mov;*.wmv|All files|*.*';

  FSaveDialog := TSaveDialog.Create(Self);
  FSaveDialog.Filter := 'MKV|*.mkv|MP4|*.mp4|All files|*.*';
  FSaveDialog.DefaultExt := 'mkv';
end;

procedure TFormDemoTranscode.FormDestroy(Sender: TObject);
begin
  if FEncoder.State = esRunning then
    FEncoder.Stop;
  if FRemuxJob.State = rsRunning then
    FRemuxJob.Stop;
end;

function TFormDemoTranscode.ParseMs(const AText: string): Int64;
begin
  Result := StrToInt64Def(Trim(AText), 0);
  if Result < 0 then
    Result := 0;
end;

procedure TFormDemoTranscode.ApplyClipAndFilter;
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

function TFormDemoTranscode.FindVideoStreamIndex: Integer;
begin
  Result := FMediaInfo.FindBestStream(AVMEDIA_TYPE_VIDEO);
end;

procedure TFormDemoTranscode.LoadSourceInfo(const AFileName: string);
var
  Bmp: TBitmap;
  VideoIdx: Integer;
  Info: TFFMediaStreamInfo;
begin
  FMediaInfo.FileName := AFileName;
  FMediaInfo.Probe;
  FMemoInfo.Lines.Text := FMediaInfo.SummaryText;

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

procedure TFormDemoTranscode.DoBrowseIn(Sender: TObject);
begin
  if FOpenDialog.Execute then
  begin
    FEdtSource.Text := FOpenDialog.FileName;
    LoadSourceInfo(FOpenDialog.FileName);
  end;
end;

procedure TFormDemoTranscode.DoBrowseOut(Sender: TObject);
begin
  if FSaveDialog.Execute then
    FEdtDest.Text := FSaveDialog.FileName;
end;

procedure TFormDemoTranscode.DoStart(Sender: TObject);
begin
  if FEdtSource.Text = '' then
  begin
    ShowMessage('Select input file');
    Exit;
  end;
  if FEdtDest.Text = '' then
  begin
    ShowMessage('Select output file');
    Exit;
  end;

  if FEncoder.State = esRunning then
    FEncoder.Stop;

  FReader.FileName := FEdtSource.Text;
  FDecoder.Reader := FReader;
  if FDecoder.StreamIndex < 0 then
    FDecoder.StreamIndex := FindVideoStreamIndex;
  if FDecoder.StreamIndex < 0 then
  begin
    ShowMessage('No video stream in input file');
    Exit;
  end;

  FWriter.FileName := FEdtDest.Text;
  FTranscodeJob.CopyAudio := FChkCopyAudio.Checked;
  case FCmbPreset.ItemIndex of
    1: FTranscodeJob.Preset := ftpH264_Medium;
    2: FTranscodeJob.Preset := ftpH264_Fast;
    3: FTranscodeJob.Preset := ftpH264_High;
    4: FTranscodeJob.Preset := ftpH264_NVENC;
    5: FTranscodeJob.Preset := ftpYouTube_1080p;
    6: FTranscodeJob.Preset := ftpWeb_Preview_480p;
    7: FTranscodeJob.Preset := ftpWebM_VP9;
  else
    FTranscodeJob.Preset := ftpMpeg4_800k;
  end;
  FTranscodeJob.ConfigureOutput(FEdtDest.Text);
  FTranscodeJob.ApplyPreset;
  ApplyClipAndFilter;

  FProgress.Position := 0;
  FLblStatus.Caption := 'Transcoding...';
  FBtnStart.Enabled := False;
  FBtnRemux.Enabled := False;
  FBtnStop.Enabled := True;

  try
    FTranscodeJob.Start;
  except
    on E: Exception do
    begin
      FLblStatus.Caption := E.Message;
      FBtnStart.Enabled := True;
      FBtnRemux.Enabled := True;
      FBtnStop.Enabled := False;
      raise;
    end;
  end;
end;

procedure TFormDemoTranscode.DoRemux(Sender: TObject);
begin
  if FEdtSource.Text = '' then
  begin
    ShowMessage('Select input file');
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

  FReader.FileName := FEdtSource.Text;
  FWriter.FileName := FEdtDest.Text;
  if LowerCase(ExtractFileExt(FEdtDest.Text)) = '.mp4' then
    FWriter.FormatName := 'mp4'
  else
    FWriter.FormatName := 'matroska';

  FRemuxJob.StartMs := ParseMs(FEdtStartMs.Text);
  FRemuxJob.EndMs := ParseMs(FEdtEndMs.Text);

  FProgress.Position := 0;
  FLblStatus.Caption := 'Remuxing...';
  FBtnStart.Enabled := False;
  FBtnRemux.Enabled := False;
  FBtnStop.Enabled := True;

  try
    FRemuxJob.Start;
  except
    on E: Exception do
    begin
      FLblStatus.Caption := E.Message;
      FBtnStart.Enabled := True;
      FBtnRemux.Enabled := True;
      FBtnStop.Enabled := False;
      raise;
    end;
  end;
end;

procedure TFormDemoTranscode.DoStop(Sender: TObject);
begin
  FEncoder.Stop;
  FRemuxJob.Stop;
end;

procedure TFormDemoTranscode.DoRemuxProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
begin
  DoProgress(Sender, APositionMs, ADurationMs);
end;

procedure TFormDemoTranscode.DoRemuxStateChange(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      case FRemuxJob.State of
        rsStopped:
          begin
            if FEncoder.State = esStopped then
            begin
              FLblStatus.Caption := 'Finished';
              FBtnStart.Enabled := True;
              FBtnRemux.Enabled := True;
              FBtnStop.Enabled := False;
            end;
          end;
        rsRunning:
          FLblStatus.Caption := 'Remuxing...';
        rsStopping:
          FLblStatus.Caption := 'Stopping...';
      end;
    end);
end;

procedure TFormDemoTranscode.DoProgress(Sender: TObject; APositionMs, ADurationMs: Int64);
begin
  TThread.Queue(nil,
    procedure
    begin
      if ADurationMs > 0 then
        FProgress.Position := Min(FProgress.Max, Round(APositionMs / ADurationMs * FProgress.Max))
      else
        FProgress.Position := 0;
      FLblStatus.Caption := Format('%s / %s', [FFFormatDurationMs(APositionMs), FFFormatDurationMs(ADurationMs)]);
    end);
end;

procedure TFormDemoTranscode.DoPreview(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64);
var
  Converter: TFFFrameConverter;
  Converted: PAVFrame;
  LocalBmp: TBitmap;
begin
  if (AFrame = nil) or (FImgPreview = nil) then
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

procedure TFormDemoTranscode.DoStateChange(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      case FEncoder.State of
        esStopped:
          begin
            FLblStatus.Caption := 'Finished';
            FBtnStart.Enabled := True;
            FBtnRemux.Enabled := True;
            FBtnStop.Enabled := False;
          end;
        esRunning:
          FLblStatus.Caption := 'Running...';
        esPaused:
          FLblStatus.Caption := 'Paused';
        esStopping:
          FLblStatus.Caption := 'Stopping...';
      end;
    end);
end;

end.
