unit uFFFMXVideoPlayer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ FMX video player (wraps TFFPlaybackEngine). }

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Types,
  System.UITypes,
  FMX.Controls,
  FMX.Graphics,
  FMX.Types,
  libavutil,
  uFFPlaybackEngine,
  uFFFMXFrameBitmap,
  uFFFrameConverter,
  uFFDecoder,
  uFFFrame,
  uFFHooks,
  uFFComponentBase,
  uFFComponentLink,
  uFFLinkedPlayback,
  uFFDesignTime;

type
  TFFPlayerState = uFFPlaybackEngine.TFFPlayerState;

  TFFFMXVideoPlayer = class(TStyledControl, IFFFrameSink)
  private
    FEngine: TFFPlaybackEngine;
    FLinked: TFFLinkedPlayback;
    FVideoDecoder: TFFDecoder;
    FAudioDecoder: TFFDecoder;
    FLinkedConverter: TFFFrameConverter;
    FFrameBitmap: FMX.Graphics.TBitmap;
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
    function InLinkedMode: Boolean;
    procedure SubscribeDecoder;
    procedure UnsubscribeDecoder;
    procedure HandlePresentFrame(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer);
    procedure HandlePositionChange(Sender: TObject; APositionMs: Int64);
    procedure HandleEngineStateChange(Sender: TObject);
    procedure HandleLinkedStateChange(Sender: TObject);
    procedure HandleLinkedPositionChange(Sender: TObject; APositionMs: Int64);
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
    property State: TFFPlayerState read GetState;
    property Position: Int64 read GetPosition;
    property Duration: Int64 read GetDuration;
    property Volume: Single read GetVolume write SetVolume;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnFrameHook: TFFFrameHookEvent read GetOnFrameHook write SetOnFrameHook;
    property OnVideoHook: TFFVideoHookEvent read GetOnVideoHook write SetOnVideoHook;
    property OnAudioHook: TFFAudioHookEvent read GetOnAudioHook write SetOnAudioHook;
  end;

implementation

uses
  FMX.Forms;

function TFFFMXVideoPlayer.InLinkedMode: Boolean;
begin
  Result := FVideoDecoder <> nil;
end;

function TFFFMXVideoPlayer.GetDuration: Int64;
begin
  if InLinkedMode then
    Result := FLinked.GetDuration
  else
    Result := FEngine.Duration;
end;

function TFFFMXVideoPlayer.GetPosition: Int64;
begin
  if InLinkedMode then
    Result := FLinked.GetPosition
  else
    Result := FEngine.Position;
end;

function TFFFMXVideoPlayer.GetState: TFFPlayerState;
begin
  if InLinkedMode then
    Result := FLinked.GetState
  else
    Result := FEngine.State;
end;

function TFFFMXVideoPlayer.GetFileName: string;
begin
  Result := FEngine.FileName;
end;

procedure TFFFMXVideoPlayer.SetFileName(const Value: string);
begin
  FEngine.FileName := Value;
end;

function TFFFMXVideoPlayer.GetVolume: Single;
begin
  if InLinkedMode then
    Result := FLinked.GetVolume
  else
    Result := FEngine.Volume;
end;

procedure TFFFMXVideoPlayer.SetVolume(const Value: Single);
begin
  if InLinkedMode then
    FLinked.SetVolume(Value)
  else
    FEngine.Volume := Value;
end;

function TFFFMXVideoPlayer.GetOnFrameHook: TFFFrameHookEvent;
begin
  Result := FEngine.OnFrameHook;
end;

procedure TFFFMXVideoPlayer.SetOnFrameHook(const Value: TFFFrameHookEvent);
begin
  FEngine.OnFrameHook := Value;
end;

function TFFFMXVideoPlayer.GetOnVideoHook: TFFVideoHookEvent;
begin
  Result := FEngine.OnVideoHook;
end;

procedure TFFFMXVideoPlayer.SetOnVideoHook(const Value: TFFVideoHookEvent);
begin
  FEngine.OnVideoHook := Value;
end;

function TFFFMXVideoPlayer.GetOnAudioHook: TFFAudioHookEvent;
begin
  if InLinkedMode then
    Result := FLinked.OnAudioHook
  else
    Result := FEngine.OnAudioHook;
end;

procedure TFFFMXVideoPlayer.SetOnAudioHook(const Value: TFFAudioHookEvent);
begin
  FEngine.OnAudioHook := Value;
  FLinked.OnAudioHook := Value;
end;

function TFFFMXVideoPlayer.GetVideoDecoder: TFFDecoder;
begin
  Result := FVideoDecoder;
end;

function TFFFMXVideoPlayer.GetAudioDecoder: TFFDecoder;
begin
  Result := FAudioDecoder;
end;

procedure TFFFMXVideoPlayer.SubscribeDecoder;
begin
  if (FVideoDecoder = nil) or (csDesigning in ComponentState) then
    Exit;
  FVideoDecoder.SubscribeFrameSink(Self);
end;

procedure TFFFMXVideoPlayer.UnsubscribeDecoder;
begin
  if FVideoDecoder = nil then
    Exit;
  FVideoDecoder.UnsubscribeFrameSink(Self);
end;

procedure TFFFMXVideoPlayer.SetVideoDecoder(const Value: TFFDecoder);
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

procedure TFFFMXVideoPlayer.SetAudioDecoder(const Value: TFFDecoder);
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

procedure TFFFMXVideoPlayer.Notification(AComponent: TComponent; Operation: TOperation);
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
end;

procedure TFFFMXVideoPlayer.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SubscribeDecoder;
end;

procedure TFFFMXVideoPlayer.TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
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
  HandlePresentFrame(Self, Converted^.data[0], FLinkedConverter.DstWidth, FLinkedConverter.DstHeight,
    Converted^.linesize[0]);
end;

procedure TFFFMXVideoPlayer.SubscribeFrameSink(const ASink: IFFFrameSink);
begin
end;

procedure TFFFMXVideoPlayer.UnsubscribeFrameSink(const ASink: IFFFrameSink);
begin
end;

procedure TFFFMXVideoPlayer.DoStateChange;
begin
  if FDestroying then
    Exit;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TFFFMXVideoPlayer.HandleEngineStateChange(Sender: TObject);
begin
  DoStateChange;
end;

procedure TFFFMXVideoPlayer.HandleLinkedStateChange(Sender: TObject);
begin
  DoStateChange;
end;

procedure TFFFMXVideoPlayer.HandlePositionChange(Sender: TObject; APositionMs: Int64);
begin
  if FDestroying then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if not FDestroying then
        Repaint;
    end);
end;

procedure TFFFMXVideoPlayer.HandleLinkedPositionChange(Sender: TObject; APositionMs: Int64);
begin
  HandlePositionChange(Sender, APositionMs);
end;

procedure TFFFMXVideoPlayer.HandlePresentFrame(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer);
var
  LocalBmp: FMX.Graphics.TBitmap;
  BmpCopy: FMX.Graphics.TBitmap;
begin
  if FDestroying or (ABgra = nil) then
    Exit;

  LocalBmp := FMX.Graphics.TBitmap.Create;
  try
    TFFFMXFrameBitmap.AssignBgraBuffer(ABgra, AWidth, AHeight, AStride, LocalBmp);
    if TThread.CurrentThread.ThreadID = MainThreadID then
    begin
      FBitmapLock.Enter;
      try
        FFrameBitmap.Assign(LocalBmp);
      finally
        FBitmapLock.Leave;
      end;
      Repaint;
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
      BmpCopy := FMX.Graphics.TBitmap.Create;
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
            Repaint;
          finally
            BmpCopy.Free;
          end;
        end);
    end;
  finally
    LocalBmp.Free;
  end;
end;

constructor TFFFMXVideoPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 320;
  Height := 240;
  FFrameBitmap := FMX.Graphics.TBitmap.Create;
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

destructor TFFFMXVideoPlayer.Destroy;
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
    if Application <> nil then
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

procedure TFFFMXVideoPlayer.Paint;
var
  Bmp: FMX.Graphics.TBitmap;
  Dst, Src: TRectF;
begin
  Canvas.Fill.Color := TAlphaColors.Black;
  Canvas.FillRect(LocalRect, 0, 0, [], 1);

  Bmp := FMX.Graphics.TBitmap.Create;
  try
    FBitmapLock.Enter;
    try
      if (FFrameBitmap <> nil) and not FFrameBitmap.IsEmpty then
        Bmp.Assign(FFrameBitmap);
    finally
      FBitmapLock.Leave;
    end;

    if not Bmp.IsEmpty then
    begin
      Src := TRectF.Create(0, 0, Bmp.Width, Bmp.Height);
      Dst := LocalRect;
      Canvas.DrawBitmap(Bmp, Src, Dst, 1);
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TFFFMXVideoPlayer.Play;
begin
  if InLinkedMode then
  begin
    FLinked.Play(FEngine.FileName);
    Exit;
  end;
  FEngine.Play;
end;

procedure TFFFMXVideoPlayer.Pause;
begin
  if InLinkedMode then
  begin
    FLinked.Pause;
    Exit;
  end;
  FEngine.Pause;
end;

procedure TFFFMXVideoPlayer.SeekTo(APositionMs: Int64);
begin
  if InLinkedMode then
  begin
    FLinked.SeekTo(APositionMs);
    Exit;
  end;
  FEngine.SeekTo(APositionMs);
end;

procedure TFFFMXVideoPlayer.Stop;
begin
  if InLinkedMode then
  begin
    FLinked.Stop;
    Exit;
  end;
  FEngine.Stop;
end;

end.
