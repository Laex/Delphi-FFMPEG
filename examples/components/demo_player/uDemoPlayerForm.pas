unit uDemoPlayerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, uFFVideoPlayer, uFFMediaInfo, uFFThumbnailExtractor;

type
  TFormDemoPlayer = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPlayer: TFFVideoPlayer;
    FMediaInfo: TFFMediaInfo;
    FThumbnail: TFFThumbnailExtractor;
    FBtnOpen, FBtnPlay, FBtnPause, FBtnStop: TButton;
    FMemoInfo: TMemo;
    FImgThumb: TImage;
    FTrackPosition: TTrackBar;
    FLblPosition: TLabel;
    FTimer: TTimer;
    FOpenDialog: TOpenDialog;
    procedure DoOpen(Sender: TObject);
    procedure DoPlay(Sender: TObject);
    procedure DoPause(Sender: TObject);
    procedure DoStop(Sender: TObject);
    procedure DoTrackChange(Sender: TObject);
    procedure DoTimer(Sender: TObject);
    procedure LoadMedia(const AFileName: string);
    procedure UpdatePositionLabel;
  public
  end;

var
  FormDemoPlayer: TFormDemoPlayer;

implementation

{$R *.dfm}

procedure TFormDemoPlayer.FormCreate(Sender: TObject);
begin
  Caption := 'Delphi-FFMPEG Demo Player';
  Width := 960;
  Height := 640;
  Position := poScreenCenter;

  FPlayer := TFFVideoPlayer.Create(Self);
  FPlayer.Parent := Self;
  FPlayer.Align := alClient;

  FMediaInfo := TFFMediaInfo.Create(Self);
  FThumbnail := TFFThumbnailExtractor.Create(Self);
  FThumbnail.MaxWidth := 160;
  FThumbnail.MaxHeight := 120;

  FBtnOpen := TButton.Create(Self);
  FBtnOpen.Parent := Self;
  FBtnOpen.Caption := 'Open...';
  FBtnOpen.SetBounds(8, 8, 75, 25);
  FBtnOpen.OnClick := DoOpen;

  FBtnPlay := TButton.Create(Self);
  FBtnPlay.Parent := Self;
  FBtnPlay.Caption := 'Play';
  FBtnPlay.SetBounds(88, 8, 60, 25);
  FBtnPlay.OnClick := DoPlay;

  FBtnPause := TButton.Create(Self);
  FBtnPause.Parent := Self;
  FBtnPause.Caption := 'Pause';
  FBtnPause.SetBounds(152, 8, 60, 25);
  FBtnPause.OnClick := DoPause;

  FBtnStop := TButton.Create(Self);
  FBtnStop.Parent := Self;
  FBtnStop.Caption := 'Stop';
  FBtnStop.SetBounds(216, 8, 60, 25);
  FBtnStop.OnClick := DoStop;

  FLblPosition := TLabel.Create(Self);
  FLblPosition.Parent := Self;
  FLblPosition.SetBounds(290, 12, 200, 16);
  FLblPosition.Caption := '00:00 / 00:00';

  FTrackPosition := TTrackBar.Create(Self);
  FTrackPosition.Parent := Self;
  FTrackPosition.SetBounds(500, 8, 220, 25);
  FTrackPosition.Min := 0;
  FTrackPosition.Max := 1000;
  FTrackPosition.Frequency := 100;
  FTrackPosition.OnChange := DoTrackChange;

  FImgThumb := TImage.Create(Self);
  FImgThumb.Parent := Self;
  FImgThumb.SetBounds(730, 4, 160, 120);
  FImgThumb.Proportional := True;
  FImgThumb.Stretch := True;

  FMemoInfo := TMemo.Create(Self);
  FMemoInfo.Parent := Self;
  FMemoInfo.SetBounds(8, 36, 744, 88);
  FMemoInfo.ReadOnly := True;
  FMemoInfo.ScrollBars := ssVertical;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 250;
  FTimer.OnTimer := DoTimer;
  FTimer.Enabled := True;

  FOpenDialog := TOpenDialog.Create(Self);
  FOpenDialog.Filter := 'Media files|*.avi;*.mkv;*.mp4;*.mov;*.wmv;*.mp3;*.wav|All files|*.*';
  FOpenDialog.Options := [ofFileMustExist];
end;

procedure TFormDemoPlayer.FormDestroy(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

procedure TFormDemoPlayer.UpdatePositionLabel;
var
  PosMs, DurMs: Int64;
begin
  PosMs := FPlayer.Position;
  DurMs := FPlayer.Duration;
  FLblPosition.Caption := Format('%s / %s', [FFFormatDurationMs(PosMs), FFFormatDurationMs(DurMs)]);
  if DurMs > 0 then
    FTrackPosition.Position := Min(FTrackPosition.Max, Round(PosMs / DurMs * FTrackPosition.Max))
  else
    FTrackPosition.Position := 0;
end;

procedure TFormDemoPlayer.LoadMedia(const AFileName: string);
var
  Bmp: TBitmap;
begin
  FPlayer.Stop;
  FPlayer.FileName := AFileName;

  FMediaInfo.FileName := AFileName;
  FMediaInfo.Probe;
  FMemoInfo.Lines.Text := FMediaInfo.SummaryText;

  FThumbnail.FileName := AFileName;
  Bmp := TBitmap.Create;
  try
    FThumbnail.ExtractToBitmap(Bmp);
    FImgThumb.Picture.Bitmap.Assign(Bmp);
  finally
    Bmp.Free;
  end;

  UpdatePositionLabel;
end;

procedure TFormDemoPlayer.DoOpen(Sender: TObject);
begin
  if FOpenDialog.Execute then
    LoadMedia(FOpenDialog.FileName);
end;

procedure TFormDemoPlayer.DoPlay(Sender: TObject);
begin
  FPlayer.Play;
end;

procedure TFormDemoPlayer.DoPause(Sender: TObject);
begin
  FPlayer.Pause;
end;

procedure TFormDemoPlayer.DoStop(Sender: TObject);
begin
  FPlayer.Stop;
  UpdatePositionLabel;
end;

procedure TFormDemoPlayer.DoTrackChange(Sender: TObject);
var
  DurMs: Int64;
begin
  if not FTrackPosition.Focused then
    Exit;
  DurMs := FPlayer.Duration;
  if DurMs > 0 then
    FPlayer.SeekTo(Round(DurMs * FTrackPosition.Position / FTrackPosition.Max));
end;

procedure TFormDemoPlayer.DoTimer(Sender: TObject);
begin
  UpdatePositionLabel;
end;

end.
