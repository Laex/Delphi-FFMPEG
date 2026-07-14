unit uFFPacket;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ RAII wrapper for PAVPacket (av_packet_alloc / av_packet_free). }

interface

uses
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavcodec,
  uFFException,
  uFFDesignTime;

type
  TFFPacket = class
  private
    FPkt: PAVPacket;
    constructor CreateOwned(APkt: PAVPacket);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Unref;
    function Clone: TFFPacket;

    property Raw: PAVPacket read FPkt;
  end;

implementation

constructor TFFPacket.Create;
begin
  inherited Create;
  FPkt := av_packet_alloc();
  if FPkt = nil then
    raise EFFException.Create('av_packet_alloc failed');
end;

constructor TFFPacket.CreateOwned(APkt: PAVPacket);
begin
  inherited Create;
  FPkt := APkt;
end;

destructor TFFPacket.Destroy;
begin
  if (FPkt <> nil) and not FFIsDesignTime(nil) then
    av_packet_free(FPkt)
  else
    FPkt := nil;
  inherited;
end;

procedure TFFPacket.Clear;
begin
  if (FPkt <> nil) and not FFIsDesignTime(nil) then
    av_packet_unref(FPkt);
end;

procedure TFFPacket.Unref;
begin
  Clear;
end;

function TFFPacket.Clone: TFFPacket;
var
  Cloned: PAVPacket;
begin
  if FPkt = nil then
    raise EFFException.Create('TFFPacket.Clone: packet not allocated');
  Cloned := av_packet_clone(FPkt);
  if Cloned = nil then
    raise EFFException.Create('av_packet_clone failed');
  Result := TFFPacket.CreateOwned(Cloned);
end;

end.
