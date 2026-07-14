unit uFFFrame;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ RAII wrapper for PAVFrame (av_frame_alloc / av_frame_free). }

interface

uses
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  uFFException,
  uFFDesignTime;

type
  TFFFrame = class
  private
    FFrame: PAVFrame;
    constructor CreateOwned(AFrame: PAVFrame);
    procedure EnsureAllocated;
    function GetRaw: PAVFrame;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Unref;
    function Clone: TFFFrame;

    function GetSampleCount: Integer;
    function GetPictureType: AVPictureType;

    property Raw: PAVFrame read GetRaw;
  end;

implementation

constructor TFFFrame.Create;
begin
  inherited Create;
  FFrame := nil;
end;

constructor TFFFrame.CreateOwned(AFrame: PAVFrame);
begin
  inherited Create;
  FFrame := AFrame;
end;

procedure TFFFrame.EnsureAllocated;
begin
  if FFrame <> nil then
    Exit;
  FFrame := av_frame_alloc();
  if FFrame = nil then
    raise EFFException.Create('av_frame_alloc failed');
end;

function TFFFrame.GetRaw: PAVFrame;
begin
  EnsureAllocated;
  Result := FFrame;
end;

destructor TFFFrame.Destroy;
begin
  if (FFrame <> nil) and not FFIsDesignTime(nil) then
    av_frame_free(FFrame)
  else
    FFrame := nil;
  inherited;
end;

procedure TFFFrame.Clear;
begin
  if (FFrame <> nil) and not FFIsDesignTime(nil) then
    av_frame_unref(FFrame);
end;

procedure TFFFrame.Unref;
begin
  Clear;
end;

function TFFFrame.Clone: TFFFrame;
var
  Cloned: PAVFrame;
begin
  EnsureAllocated;
  Cloned := av_frame_clone(FFrame);
  if Cloned = nil then
    raise EFFException.Create('av_frame_clone failed');
  Result := TFFFrame.CreateOwned(Cloned);
end;

function TFFFrame.GetSampleCount: Integer;
begin
  if FFrame = nil then
    Exit(0);
  Result := FFrame^.nb_samples;
end;

function TFFFrame.GetPictureType: AVPictureType;
begin
  if FFrame = nil then
    Exit(AV_PICTURE_TYPE_NONE);
  Result := FFrame^.pict_type;
end;

end.
