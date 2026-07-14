unit uFFVideoPlayer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ VCL video player (wraps TFFPlaybackEngine). Windows / MSWINDOWS. }

interface

{$IFDEF MSWINDOWS}

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  libavutil,
  uFFPlaybackEngine,
  uFFFrameBitmap,
  uFFFrameConverter,
  uFFDecoder,
  uFFFrame,
  uFFHooks,
  uFFComponentBase,
  uFFComponentLink,
  uFFLinkedPlayback,
  uFFSubtitleDecoder,
  uFFSubtitleOverlay,
  uFFHardwareDecode,
  uFFDesignTime;

type
  TFFPlayerState = uFFPlaybackEngine.TFFPlayerState;

  TFFVideoPlayer = class(TCustomControl, IFFFrameSink)
  private
    FEngine: TFFPlaybackEngine;
    FLinked: TFFLinkedPlayback;
    FVideoDecoder: TFFDecoder;
    FAudioDecoder: TFFDecoder;
    FSubtitleDecoder: TFFSubtitleDecoder;
    FLinkedConverter: TFFFrameConverter;
    FFrameBitmap: TBitmap;
    FBitmapLock: TCriticalSection;
    FDestroying: Boolean;
    FOnStateChange: TNotifyEvent;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
    function GetState: TFFPlayerState;
    function GetPosition: Int64;
    function GetDuration: Int64;
    function GetOnFrameHook: TFFFrameHookEvent;
    procedure SetOnFrameHook(const Value: TFFFrameHookEvent);
    function GetOnVideoHook: TFFVideoHookEvent;
    procedure SetOnVideoHook(const Value: TFFVideoHookEvent);
    function GetOnAudioHook: TFFAudioHookEvent;
    procedure SetOnAudioHook(const Value: TFFAudioHookEvent);
    function GetVideoDecoder: TFFDecoder;
    procedure SetVideoDecoder(const Value: TFFDecoder);
    function GetAudioDecoder: TFFDecoder;
    procedure SetAudioDecoder(const Value: TFFDecoder);
    function GetSubtitleDecoder: TFFSubtitleDecoder;
    procedure SetSubtitleDecoder(const Value: TFFSubtitleDecoder);
    function GetHardwareDevice: TFFHardwareDevice;
    procedure SetHardwareDevice(const Value: TFFHardwareDevice);
    function InLinkedMode: Boolean;
    procedure SubscribeDecoder;
    procedure UnsubscribeDecoder;
    procedure HandlePresentFrame(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer);
    procedure PresentBgraBuffer(ABgra: PByte; AWidth, AHeight, AStride: Integer);
    procedure HandlePositionChange(Sender: TObject; APositionMs: Int64);
    procedure HandleEngineStateChange(Sender: TObject);
    procedure HandleLinkedStateChange(Sender: TObject);
    procedure HandleLinkedPositionChange(Sender: TObject; APositionMs: Int64);
    procedure ApplySubtitleOverlay(ABgra: PByte; AWidth, AHeight, AStride: Integer; APositionMs: Int64);
    procedure DoStateChange;
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure SeekTo(APositionMs: Int64);
    procedure TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
    procedure SubscribeFrameSink(const ASink: IFFFrameSink);
    procedure UnsubscribeFrameSink(const ASink: IFFFrameSink);
  published
    property FileName: string read GetFileName write SetFileName;
    property VideoDecoder: TFFDecoder read GetVideoDecoder write SetVideoDecoder;
    property AudioDecoder: TFFDecoder read GetAudioDecoder write SetAudioDecoder;
    property SubtitleDecoder: TFFSubtitleDecoder read GetSubtitleDecoder write SetSubtitleDecoder;
    property HardwareDevice: TFFHardwareDevice read GetHardwareDevice write SetHardwareDevice default ffhdNone;
    property State: TFFPlayerState read GetState;
    property Position: Int64 read GetPosition;
    property Duration: Int64 read GetDuration;
    property Volume: Single read GetVolume write SetVolume;
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnFrameHook: TFFFrameHookEvent read GetOnFrameHook write SetOnFrameHook;
    property OnVideoHook: TFFVideoHookEvent read GetOnVideoHook write SetOnVideoHook;
    property OnAudioHook: TFFAudioHookEvent read GetOnAudioHook write SetOnAudioHook;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

function TFFVideoPlayer.InLinkedMode: Boolean;
begin
  Result := FVideoDecoder <> nil;
end;

function TFFVideoPlayer.GetDuration: Int64;
begin
  if InLinkedMode then
    Result := FLinked.GetDuration
  else
    Result := FEngine.Duration;
end;

function TFFVideoPlayer.GetPosition: Int64;
begin
  if InLinkedMode then
    Result := FLinked.GetPosition
  else
    Result := FEngine.Position;
end;

function TFFVideoPlayer.GetState: TFFPlayerState;
begin
  if InLinkedMode then
    Result := FLinked.GetState
  else
    Result := FEngine.State;
end;

function TFFVideoPlayer.GetFileName: string;
begin
  Result := FEngine.FileName;
end;

procedure TFFVideoPlayer.SetFileName(const Value: string);
begin
  FEngine.FileName := Value;
end;

function TFFVideoPlayer.GetVolume: Single;
begin
  if InLinkedMode then
    Result := FLinked.GetVolume
  else
    Result := FEngine.Volume;
end;

procedure TFFVideoPlayer.SetVolume(const Value: Single);
begin
  if InLinkedMode then
    FLinked.SetVolume(Value)
  else
    FEngine.Volume := Value;
end;

function TFFVideoPlayer.GetOnFrameHook: TFFFrameHookEvent;
begin
  Result := FEngine.OnFrameHook;
end;

procedure TFFVideoPlayer.SetOnFrameHook(const Value: TFFFrameHookEvent);
begin
  FEngine.OnFrameHook := Value;
end;

function TFFVideoPlayer.GetOnVideoHook: TFFVideoHookEvent;
begin
  Result := FEngine.OnVideoHook;
end;

procedure TFFVideoPlayer.SetOnVideoHook(const Value: TFFVideoHookEvent);
begin
  FEngine.OnVideoHook := Value;
end;

function TFFVideoPlayer.GetOnAudioHook: TFFAudioHookEvent;
begin
  if InLinkedMode then
    Result := FLinked.OnAudioHook
  else
    Result := FEngine.OnAudioHook;
end;

procedure TFFVideoPlayer.SetOnAudioHook(const Value: TFFAudioHookEvent);
begin
  FEngine.OnAudioHook := Value;
  FLinked.OnAudioHook := Value;
end;

function TFFVideoPlayer.GetVideoDecoder: TFFDecoder;
begin
  Result := FVideoDecoder;
end;

function TFFVideoPlayer.GetAudioDecoder: TFFDecoder;
begin
  Result := FAudioDecoder;
end;

procedure TFFVideoPlayer.SubscribeDecoder;
begin
  if (FVideoDecoder = nil) or (csDesigning in ComponentState) then
    Exit;
  FVideoDecoder.SubscribeFrameSink(Self);
end;

procedure TFFVideoPlayer.UnsubscribeDecoder;
begin
  if FVideoDecoder = nil then
    Exit;
  FVideoDecoder.UnsubscribeFrameSink(Self);
end;

procedure TFFVideoPlayer.SetVideoDecoder(const Value: TFFDecoder);
var
  Link: TComponent;
begin
  if FVideoDecoder = Value then
    Exit;
  UnsubscribeDecoder;
  Link := FVideoDecoder;
  FFSetLinkedComponent(Self, Link, Value);
  FVideoDecoder := TFFDecoder(Link);
  FLinked.SetVideoDecoder(FVideoDecoder);
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    SubscribeDecoder;
end;

procedure TFFVideoPlayer.SetAudioDecoder(const Value: TFFDecoder);
var
  Link: TComponent;
begin
  if FAudioDecoder = Value then
    Exit;
  Link := FAudioDecoder;
  FFSetLinkedComponent(Self, Link, Value);
  FAudioDecoder := TFFDecoder(Link);
  FLinked.SetAudioDecoder(FAudioDecoder);
end;

function TFFVideoPlayer.GetSubtitleDecoder: TFFSubtitleDecoder;
begin
  Result := FSubtitleDecoder;
end;

procedure TFFVideoPlayer.SetSubtitleDecoder(const Value: TFFSubtitleDecoder);
var
  Link: TComponent;
begin
  if FSubtitleDecoder = Value then
    Exit;
  Link := FSubtitleDecoder;
  FFSetLinkedComponent(Self, Link, Value);
  FSubtitleDecoder := TFFSubtitleDecoder(Link);
  if (FSubtitleDecoder <> nil) and (FSubtitleDecoder.Reader <> nil) and
     FSubtitleDecoder.Reader.Active and (FSubtitleDecoder.StreamIndex >= 0) and
     not FSubtitleDecoder.HasEvents then
    FSubtitleDecoder.LoadAll;
end;

function TFFVideoPlayer.GetHardwareDevice: TFFHardwareDevice;
begin
  if InLinkedMode then
    Result := FVideoDecoder.HardwareDevice
  else
    Result := FEngine.HardwareDevice;
end;

procedure TFFVideoPlayer.SetHardwareDevice(const Value: TFFHardwareDevice);
begin
  if InLinkedMode then
  begin
    if FVideoDecoder <> nil then
      FVideoDecoder.HardwareDevice := Value;
  end
  else
    FEngine.HardwareDevice := Value;
end;

procedure TFFVideoPlayer.ApplySubtitleOverlay(ABgra: PByte; AWidth, AHeight, AStride: Integer;
  APositionMs: Int64);
var
  Ev: TFFSubtitleEvent;
begin
  if (FSubtitleDecoder = nil) or (ABgra = nil) then
    Exit;
  Ev := FSubtitleDecoder.GetEventAt(APositionMs);
  if (Ev.Text = '') and not Ev.IsBitmap and not Ev.IsAss then
    Exit;
  FFSubtitleBlendEventOnBgra(ABgra, AWidth, AHeight, AStride, Ev);
end;

procedure TFFVideoPlayer.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FVideoDecoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    UnsubscribeDecoder;
    FVideoDecoder := TFFDecoder(Link);
    FLinked.SetVideoDecoder(FVideoDecoder);
  end;
  Link := FAudioDecoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    FAudioDecoder := TFFDecoder(Link);
    FLinked.SetAudioDecoder(FAudioDecoder);
  end;
  Link := FSubtitleDecoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
    FSubtitleDecoder := TFFSubtitleDecoder(Link);
end;

procedure TFFVideoPlayer.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SubscribeDecoder;
end;

procedure TFFVideoPlayer.TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
var
  Converted: PAVFrame;
  Handled: Boolean;
begin
  if FDestroying or (AFrame = nil) or (FLinkedConverter = nil) then
    Exit;

  Handled := False;
  if Assigned(FEngine.OnFrameHook) then
    FEngine.OnFrameHook(Self, AFrame, Handled);
  if Handled then
    Exit;

  if not FLinked.ShouldPresentVideoFrame(AFrame) then
    Exit;

  Converted := FLinkedConverter.Convert(AFrame);
  if Assigned(FEngine.OnVideoHook) then
    FEngine.OnVideoHook(Self, Converted^.data[0], FLinkedConverter.DstWidth, FLinkedConverter.DstHeight,
      Converted^.linesize[0]);
  ApplySubtitleOverlay(Converted^.data[0], FLinkedConverter.DstWidth, FLinkedConverter.DstHeight,
    Converted^.linesize[0], GetPosition);
  HandlePresentFrame(Self, Converted^.data[0], FLinkedConverter.DstWidth, FLinkedConverter.DstHeight,
    Converted^.linesize[0]);
end;

procedure TFFVideoPlayer.SubscribeFrameSink(const ASink: IFFFrameSink);
begin
end;

procedure TFFVideoPlayer.UnsubscribeFrameSink(const ASink: IFFFrameSink);
begin
end;

procedure TFFVideoPlayer.DoStateChange;
begin
  if FDestroying then
    Exit;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TFFVideoPlayer.HandleEngineStateChange(Sender: TObject);
begin
  DoStateChange;
end;

procedure TFFVideoPlayer.HandleLinkedStateChange(Sender: TObject);
begin
  DoStateChange;
end;

procedure TFFVideoPlayer.HandlePositionChange(Sender: TObject; APositionMs: Int64);
begin
  if FDestroying then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if not FDestroying then
        Invalidate;
    end);
end;

procedure TFFVideoPlayer.HandleLinkedPositionChange(Sender: TObject; APositionMs: Int64);
begin
  HandlePositionChange(Sender, APositionMs);
end;

procedure TFFVideoPlayer.PresentBgraBuffer(ABgra: PByte; AWidth, AHeight, AStride: Integer);
var
  LocalBmp: TBitmap;
  BmpCopy: TBitmap;
begin
  if FDestroying or (ABgra = nil) then
    Exit;

  LocalBmp := TBitmap.Create;
  try
    TFFFrameBitmap.AssignBgraBuffer(ABgra, AWidth, AHeight, AStride, LocalBmp);
    if TThread.CurrentThread.ThreadID = MainThreadID then
    begin
      FBitmapLock.Enter;
      try
        FFrameBitmap.Assign(LocalBmp);
      finally
        FBitmapLock.Leave;
      end;
      Invalidate;
    end
    else if FDestroying or ((FEngine <> nil) and FEngine.StopRequested) then
    begin
      FBitmapLock.Enter;
      try
        FFrameBitmap.Assign(LocalBmp);
      finally
        FBitmapLock.Leave;
      end;
    end
    else
    begin
      BmpCopy := TBitmap.Create;
      BmpCopy.Assign(LocalBmp);
      TThread.Queue(nil,
        procedure
        begin
          try
            if FDestroying then
              Exit;
            FBitmapLock.Enter;
            try
              FFrameBitmap.Assign(BmpCopy);
            finally
              FBitmapLock.Leave;
            end;
            Invalidate;
          finally
            BmpCopy.Free;
          end;
        end);
    end;
  finally
    LocalBmp.Free;
  end;
end;

procedure TFFVideoPlayer.HandlePresentFrame(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer);
begin
  if FDestroying or (ABgra = nil) then
    Exit;

  if not InLinkedMode then
    ApplySubtitleOverlay(ABgra, AWidth, AHeight, AStride, GetPosition);

  PresentBgraBuffer(ABgra, AWidth, AHeight, AStride);
end;

constructor TFFVideoPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;
  Width := 320;
  Height := 240;
  Color := clBlack;
  FFrameBitmap := TBitmap.Create;
  FBitmapLock := TCriticalSection.Create;
  FLinkedConverter := TFFFrameConverter.Create;
  FLinked := TFFLinkedPlayback.Create;
  FLinked.OnStateChange := HandleLinkedStateChange;
  FLinked.OnPositionChange := HandleLinkedPositionChange;
  FEngine := TFFPlaybackEngine.Create;
  FEngine.OnPresentFrame := HandlePresentFrame;
  FEngine.OnPositionChange := HandlePositionChange;
  FEngine.OnStateChange := HandleEngineStateChange;
end;

destructor TFFVideoPlayer.Destroy;
var
  I: Integer;
begin
  FDestroying := True;
  UnsubscribeDecoder;
  if Assigned(FVideoDecoder) then
    FVideoDecoder.RemoveFreeNotification(Self);
  if Assigned(FAudioDecoder) then
    FAudioDecoder.RemoveFreeNotification(Self);
  FLinked.OnStateChange := nil;
  FLinked.OnPositionChange := nil;
  FLinked.OnAudioHook := nil;
  FEngine.OnPresentFrame := nil;
  FEngine.OnPositionChange := nil;
  FEngine.OnStateChange := nil;

  if FFIsDesignTime(Self) then
  begin
    FreeAndNil(FEngine);
    FreeAndNil(FLinked);
    FreeAndNil(FLinkedConverter);
    FreeAndNil(FBitmapLock);
    FreeAndNil(FFrameBitmap);
    inherited;
    Exit;
  end;

  FLinked.Stop;
  FEngine.Stop;
  for I := 1 to 50 do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
  FEngine.Free;
  FLinked.Free;
  FLinkedConverter.Free;
  FBitmapLock.Free;
  FFrameBitmap.Free;
  inherited;
end;

procedure TFFVideoPlayer.Paint;
var
  Dst: TRect;
  Bmp: TBitmap;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(ClientRect);

  Bmp := TBitmap.Create;
  try
    FBitmapLock.Enter;
    try
      if not FFrameBitmap.Empty then
        Bmp.Assign(FFrameBitmap);
    finally
      FBitmapLock.Leave;
    end;

    if not Bmp.Empty then
    begin
      Dst := ClientRect;
      Canvas.StretchDraw(Dst, Bmp);
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TFFVideoPlayer.Play;
begin
  if InLinkedMode then
  begin
    FLinked.Play(FEngine.FileName);
    Exit;
  end;
  FEngine.Play;
end;

procedure TFFVideoPlayer.Pause;
begin
  if InLinkedMode then
  begin
    FLinked.Pause;
    Exit;
  end;
  FEngine.Pause;
end;

procedure TFFVideoPlayer.SeekTo(APositionMs: Int64);
begin
  if InLinkedMode then
  begin
    FLinked.SeekTo(APositionMs);
    Exit;
  end;
  FEngine.SeekTo(APositionMs);
end;

procedure TFFVideoPlayer.Stop;
begin
  if InLinkedMode then
  begin
    FLinked.Stop;
    Exit;
  end;
  FEngine.Stop;
end;

{$ENDIF}

end.
