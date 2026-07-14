unit uFFPlayerControl;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ VCL composite control: video surface + transport bar (play/pause/stop/seek). }

interface

{$IFDEF MSWINDOWS}

uses
  System.Classes,
  System.SysUtils,
  Winapi.Messages,
  Winapi.CommCtrl,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Graphics,
  uFFVideoPlayer,
  uFFDecoder,
  uFFSubtitleDecoder,
  uFFPlaybackEngine,
  uFFDesignTime;

type
  TFFSeekTrackBar = class(TTrackBar)
  private
    FOnSeekPreview: TNotifyEvent;
    FOnSeekCommit: TNotifyEvent;
  protected
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
  public
    property OnSeekPreview: TNotifyEvent read FOnSeekPreview write FOnSeekPreview;
    property OnSeekCommit: TNotifyEvent read FOnSeekCommit write FOnSeekCommit;
  end;

  TFFPlayerState = uFFPlaybackEngine.TFFPlayerState;

  TFFPlayerControl = class(TPanel)
  private
    FPlayer: TFFVideoPlayer;
    FControlPanel: TPanel;
    FTrackPosition: TFFSeekTrackBar;
    FLblTime: TLabel;
    FBtnPlay: TButton;
    FBtnPause: TButton;
    FBtnStop: TButton;
    FTimer: TTimer;
    FUpdatingTrack: Boolean;
    FTrackDragging: Boolean;
    FOnStateChange: TNotifyEvent;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
    function GetState: TFFPlayerState;
    function GetPosition: Int64;
    function GetDuration: Int64;
    function GetVideoDecoder: TFFDecoder;
    procedure SetVideoDecoder(const Value: TFFDecoder);
    function GetAudioDecoder: TFFDecoder;
    procedure SetAudioDecoder(const Value: TFFDecoder);
    function GetSubtitleDecoder: TFFSubtitleDecoder;
    procedure SetSubtitleDecoder(const Value: TFFSubtitleDecoder);
    procedure DoPlay(Sender: TObject);
    procedure DoPause(Sender: TObject);
    procedure DoStop(Sender: TObject);
    procedure DoTrackPreview(Sender: TObject);
    procedure DoTrackCommit(Sender: TObject);
    procedure DoTimer(Sender: TObject);
    procedure DoPlayerStateChange(Sender: TObject);
    function TrackPositionMs: Int64;
    procedure ApplyTrackSeek;
    procedure UpdateTimeLabel(APositionMs: Int64 = -1);
    procedure SyncTrackFromPlayer;
    procedure LayoutTransportBar;
    procedure EnsureSubControls;
    procedure ActivateTransport;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure SeekTo(APositionMs: Int64);

    property Player: TFFVideoPlayer read FPlayer;
  published
    property FileName: string read GetFileName write SetFileName;
    property VideoDecoder: TFFDecoder read GetVideoDecoder write SetVideoDecoder;
    property AudioDecoder: TFFDecoder read GetAudioDecoder write SetAudioDecoder;
    property SubtitleDecoder: TFFSubtitleDecoder read GetSubtitleDecoder write SetSubtitleDecoder;
    property State: TFFPlayerState read GetState;
    property Position: Int64 read GetPosition;
    property Duration: Int64 read GetDuration;
    property Volume: Single read GetVolume write SetVolume;
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

procedure TFFSeekTrackBar.CNHScroll(var Message: TWMHScroll);
begin
  inherited;
  case Message.ScrollCode of
    TB_THUMBTRACK, TB_THUMBPOSITION:
      if Assigned(FOnSeekPreview) then
        FOnSeekPreview(Self);
    TB_ENDTRACK, TB_LINEUP, TB_LINEDOWN, TB_PAGEUP, TB_PAGEDOWN, TB_TOP, TB_BOTTOM:
      if Assigned(FOnSeekCommit) then
        FOnSeekCommit(Self);
  end;
end;

procedure TFFPlayerControl.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if AParent <> nil then
    EnsureSubControls;
end;

procedure TFFPlayerControl.EnsureSubControls;
begin
  if FPlayer <> nil then
    Exit;

  FPlayer := TFFVideoPlayer.Create(Self);
  FPlayer.Parent := Self;
  FPlayer.Align := alClient;
  FPlayer.OnStateChange := DoPlayerStateChange;

  FControlPanel := TPanel.Create(Self);
  FControlPanel.Parent := Self;
  FControlPanel.Align := alBottom;
  FControlPanel.Height := 56;
  FControlPanel.BevelOuter := bvNone;
  FControlPanel.Color := clBtnFace;

  FBtnPlay := TButton.Create(Self);
  FBtnPlay.Parent := FControlPanel;
  FBtnPlay.Caption := 'Play';
  FBtnPlay.Anchors := [akLeft, akTop];
  FBtnPlay.OnClick := DoPlay;

  FBtnPause := TButton.Create(Self);
  FBtnPause.Parent := FControlPanel;
  FBtnPause.Caption := 'Pause';
  FBtnPause.Anchors := [akLeft, akTop];
  FBtnPause.OnClick := DoPause;

  FBtnStop := TButton.Create(Self);
  FBtnStop.Parent := FControlPanel;
  FBtnStop.Caption := 'Stop';
  FBtnStop.Anchors := [akLeft, akTop];
  FBtnStop.OnClick := DoStop;

  FTrackPosition := TFFSeekTrackBar.Create(Self);
  FTrackPosition.Parent := FControlPanel;
  FTrackPosition.Min := 0;
  FTrackPosition.Max := 1000;
  FTrackPosition.Frequency := 100;
  FTrackPosition.TickMarks := tmBottomRight;
  FTrackPosition.Anchors := [akLeft, akTop];
  FTrackPosition.OnSeekPreview := DoTrackPreview;
  FTrackPosition.OnSeekCommit := DoTrackCommit;

  FLblTime := TLabel.Create(Self);
  FLblTime.Parent := FControlPanel;
  FLblTime.AutoSize := False;
  FLblTime.Caption := '00:00 / 00:00';
  FLblTime.Anchors := [akTop, akRight];
  FLblTime.Alignment := taRightJustify;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 200;
  FTimer.OnTimer := DoTimer;
  FTimer.Enabled := False;

  LayoutTransportBar;
  ActivateTransport;
end;

procedure TFFPlayerControl.ActivateTransport;
begin
  if FFIsDesignTime(Self) then
    Exit;
  if FTimer <> nil then
    FTimer.Enabled := True;
  SyncTrackFromPlayer;
end;

procedure TFFPlayerControl.Loaded;
begin
  inherited;
  EnsureSubControls;
  LayoutTransportBar;
  ActivateTransport;
end;

procedure TFFPlayerControl.LayoutTransportBar;
const
  BtnW = 60;
  BtnH = 28;
  BtnY = 14;
  BtnGap = 4;
  LeftMargin = 8;
  TrackTop = 10;
  TrackH = 36;
  LabelW = 112;
  LabelH = 16;
  LabelY = 18;
  RightMargin = 8;
  TrackLabelGap = 8;
var
  X, LabelLeft, TrackW: Integer;
begin
  if FControlPanel = nil then
    Exit;

  X := LeftMargin;
  FBtnPlay.SetBounds(X, BtnY, BtnW, BtnH);
  Inc(X, BtnW + BtnGap);
  FBtnPause.SetBounds(X, BtnY, BtnW, BtnH);
  Inc(X, BtnW + BtnGap);
  FBtnStop.SetBounds(X, BtnY, BtnW, BtnH);
  Inc(X, BtnW + BtnGap);

  LabelLeft := FControlPanel.ClientWidth - RightMargin - LabelW;
  FLblTime.SetBounds(LabelLeft, LabelY, LabelW, LabelH);

  TrackW := LabelLeft - TrackLabelGap - X;
  if TrackW < 0 then
    TrackW := 0;
  FTrackPosition.SetBounds(X, TrackTop, TrackW, TrackH);
end;

procedure TFFPlayerControl.Resize;
begin
  inherited;
  if FControlPanel <> nil then
    LayoutTransportBar;
end;

constructor TFFPlayerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Color := clBlack;
  Constraints.MinWidth := 400;
end;

destructor TFFPlayerControl.Destroy;
begin
  if (FPlayer <> nil) and not FFIsDesignTime(Self) then
    FPlayer.Stop;
  if FTimer <> nil then
    FTimer.Enabled := False;
  inherited;
end;

function TFFPlayerControl.GetFileName: string;
begin
  if FPlayer = nil then
    Result := ''
  else
    Result := FPlayer.FileName;
end;

procedure TFFPlayerControl.SetFileName(const Value: string);
begin
  EnsureSubControls;
  FPlayer.FileName := Value;
  SyncTrackFromPlayer;
end;

function TFFPlayerControl.GetVolume: Single;
begin
  if FPlayer = nil then
    Result := 1
  else
    Result := FPlayer.Volume;
end;

procedure TFFPlayerControl.SetVolume(const Value: Single);
begin
  EnsureSubControls;
  FPlayer.Volume := Value;
end;

function TFFPlayerControl.GetState: TFFPlayerState;
begin
  if FPlayer = nil then
    Result := psStopped
  else
    Result := FPlayer.State;
end;

function TFFPlayerControl.GetPosition: Int64;
begin
  if FPlayer = nil then
    Result := 0
  else
    Result := FPlayer.Position;
end;

function TFFPlayerControl.GetDuration: Int64;
begin
  if FPlayer = nil then
    Result := 0
  else
    Result := FPlayer.Duration;
end;

function TFFPlayerControl.GetVideoDecoder: TFFDecoder;
begin
  if FPlayer = nil then
    Result := nil
  else
    Result := FPlayer.VideoDecoder;
end;

procedure TFFPlayerControl.SetVideoDecoder(const Value: TFFDecoder);
begin
  EnsureSubControls;
  FPlayer.VideoDecoder := Value;
end;

function TFFPlayerControl.GetAudioDecoder: TFFDecoder;
begin
  if FPlayer = nil then
    Result := nil
  else
    Result := FPlayer.AudioDecoder;
end;

procedure TFFPlayerControl.SetAudioDecoder(const Value: TFFDecoder);
begin
  EnsureSubControls;
  FPlayer.AudioDecoder := Value;
end;

function TFFPlayerControl.GetSubtitleDecoder: TFFSubtitleDecoder;
begin
  if FPlayer = nil then
    Result := nil
  else
    Result := FPlayer.SubtitleDecoder;
end;

procedure TFFPlayerControl.SetSubtitleDecoder(const Value: TFFSubtitleDecoder);
begin
  EnsureSubControls;
  FPlayer.SubtitleDecoder := Value;
end;

procedure TFFPlayerControl.Play;
begin
  EnsureSubControls;
  ActivateTransport;
  FPlayer.Play;
  SyncTrackFromPlayer;
end;

procedure TFFPlayerControl.Pause;
begin
  EnsureSubControls;
  FPlayer.Pause;
end;

procedure TFFPlayerControl.Stop;
begin
  if FPlayer = nil then
    Exit;
  FPlayer.Stop;
  SyncTrackFromPlayer;
end;

procedure TFFPlayerControl.SeekTo(APositionMs: Int64);
begin
  EnsureSubControls;
  FPlayer.SeekTo(APositionMs);
  SyncTrackFromPlayer;
end;

function TFFPlayerControl.TrackPositionMs: Int64;
begin
  Result := 0;
  if (FPlayer = nil) or (FTrackPosition = nil) then
    Exit;
  if FPlayer.Duration > 0 then
    Result := (FPlayer.Duration * FTrackPosition.Position) div FTrackPosition.Max;
end;

procedure TFFPlayerControl.ApplyTrackSeek;
var
  PosMs: Int64;
  WasPlaying: Boolean;
begin
  if (FPlayer = nil) or (FPlayer.Duration <= 0) then
    Exit;
  PosMs := TrackPositionMs;
  WasPlaying := FPlayer.State = psPlaying;
  FPlayer.SeekTo(PosMs);
  // Linked playback resumes inside TFFLinkedPlayback.SeekTo.
  if WasPlaying and (FPlayer.VideoDecoder = nil) then
    FPlayer.Play;
  SyncTrackFromPlayer;
end;

procedure TFFPlayerControl.DoPlay(Sender: TObject);
begin
  Play;
end;

procedure TFFPlayerControl.DoPause(Sender: TObject);
begin
  Pause;
end;

procedure TFFPlayerControl.DoStop(Sender: TObject);
begin
  Stop;
end;

procedure TFFPlayerControl.DoTrackPreview(Sender: TObject);
begin
  if FUpdatingTrack then
    Exit;
  FTrackDragging := True;
  UpdateTimeLabel(TrackPositionMs);
end;

procedure TFFPlayerControl.DoTrackCommit(Sender: TObject);
begin
  if FUpdatingTrack then
    Exit;
  FTrackDragging := False;
  ApplyTrackSeek;
end;

procedure TFFPlayerControl.UpdateTimeLabel(APositionMs: Int64);
var
  PosSec, DurSec: Int64;
  PosMs: Int64;
begin
  if FLblTime = nil then
    Exit;
  if APositionMs >= 0 then
    PosMs := APositionMs
  else if FPlayer = nil then
    PosMs := 0
  else
    PosMs := FPlayer.Position;
  PosSec := PosMs div 1000;
  if FPlayer = nil then
    DurSec := 0
  else
    DurSec := FPlayer.Duration div 1000;
  FLblTime.Caption := Format('%.2d:%.2d / %.2d:%.2d',
    [PosSec div 60, PosSec mod 60, DurSec div 60, DurSec mod 60]);
end;

procedure TFFPlayerControl.SyncTrackFromPlayer;
begin
  if FFIsDesignTime(Self) or (FPlayer = nil) or (FTrackPosition = nil) then
    Exit;
  FUpdatingTrack := True;
  try
    if FPlayer.Duration > 0 then
      FTrackPosition.Position := (FPlayer.Position * FTrackPosition.Max) div FPlayer.Duration
    else
      FTrackPosition.Position := 0;
    UpdateTimeLabel;
  finally
    FUpdatingTrack := False;
  end;
end;

procedure TFFPlayerControl.DoTimer(Sender: TObject);
begin
  if FFIsDesignTime(Self) or (FPlayer = nil) then
    Exit;
  if FUpdatingTrack or FTrackDragging then
    Exit;
  if FPlayer.State = psPlaying then
    SyncTrackFromPlayer;
end;

procedure TFFPlayerControl.DoPlayerStateChange(Sender: TObject);
begin
  SyncTrackFromPlayer;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

{$ENDIF}

end.
