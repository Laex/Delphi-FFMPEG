unit uFFComponentBase;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Push-graph interfaces for linking FFmpeg components (packet / frame flow). }

interface

uses
  {$IFDEF FPC}
  Classes,
  SyncObjs,
  {$ELSE}
  System.Classes,
  System.SyncObjs,
  {$ENDIF}
  uFFPacket,
  uFFFrame;

type
  IFFPacketSink = interface
    ['{A4E8F1C2-3B5D-4E6A-9F0C-1D2E3F4A5B6C}']
    procedure TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
  end;

  IFFPacketSource = interface
    ['{B5F912D3-4C6E-5F7B-A01D-2E3F4A5B6C7D}']
    procedure SubscribePacketSink(const ASink: IFFPacketSink);
    procedure UnsubscribePacketSink(const ASink: IFFPacketSink);
  end;

  IFFFrameSink = interface
    ['{C601034E-5D7F-6A8C-B12E-3F4A5B6C7D8E}']
    procedure TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
  end;

  IFFFrameSource = interface
    ['{D712145F-6E80-7B9D-C23F-4A5B6C7D8E9F}']
    procedure SubscribeFrameSink(const ASink: IFFFrameSink);
    procedure UnsubscribeFrameSink(const ASink: IFFFrameSink);
  end;

  TFFPacketSinkList = class
  private
    FLock: TCriticalSection;
    FItems: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ASink: IFFPacketSink);
    procedure Remove(const ASink: IFFPacketSink);
    procedure Notify(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
    function Count: Integer;
  end;

  TFFFrameSinkList = class
  private
    FLock: TCriticalSection;
    FItems: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ASink: IFFFrameSink);
    procedure Remove(const ASink: IFFFrameSink);
    procedure Notify(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
    function Count: Integer;
  end;

implementation

{ TFFPacketSinkList }

constructor TFFPacketSinkList.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FItems := TInterfaceList.Create;
end;

destructor TFFPacketSinkList.Destroy;
begin
  FItems.Free;
  FLock.Free;
  inherited;
end;

procedure TFFPacketSinkList.Add(const ASink: IFFPacketSink);
begin
  FLock.Enter;
  try
    if FItems.IndexOf(ASink) < 0 then
      FItems.Add(ASink);
  finally
    FLock.Leave;
  end;
end;

procedure TFFPacketSinkList.Remove(const ASink: IFFPacketSink);
var
  Idx: Integer;
begin
  FLock.Enter;
  try
    Idx := FItems.IndexOf(ASink);
    if Idx >= 0 then
      FItems.Delete(Idx);
  finally
    FLock.Leave;
  end;
end;

function TFFPacketSinkList.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TFFPacketSinkList.Notify(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
var
  I: Integer;
  Sink: IFFPacketSink;
  ClonePkt: TFFPacket;
begin
  FLock.Enter;
  try
    if FItems.Count = 0 then
      Exit;

    if FItems.Count = 1 then
    begin
      Sink := IFFPacketSink(FItems[0]);
      Sink.TakePacket(ASource, APkt, AStreamIndex);
      Exit;
    end;

    for I := 0 to FItems.Count - 1 do
    begin
      Sink := IFFPacketSink(FItems[I]);
      ClonePkt := APkt.Clone;
      try
        Sink.TakePacket(ASource, ClonePkt, AStreamIndex);
      finally
        ClonePkt.Free;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TFFFrameSinkList }

constructor TFFFrameSinkList.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FItems := TInterfaceList.Create;
end;

destructor TFFFrameSinkList.Destroy;
begin
  FItems.Free;
  FLock.Free;
  inherited;
end;

procedure TFFFrameSinkList.Add(const ASink: IFFFrameSink);
begin
  FLock.Enter;
  try
    if FItems.IndexOf(ASink) < 0 then
      FItems.Add(ASink);
  finally
    FLock.Leave;
  end;
end;

procedure TFFFrameSinkList.Remove(const ASink: IFFFrameSink);
var
  Idx: Integer;
begin
  FLock.Enter;
  try
    Idx := FItems.IndexOf(ASink);
    if Idx >= 0 then
      FItems.Delete(Idx);
  finally
    FLock.Leave;
  end;
end;

function TFFFrameSinkList.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TFFFrameSinkList.Notify(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
var
  I: Integer;
  Sink: IFFFrameSink;
  CloneFrame: TFFFrame;
begin
  FLock.Enter;
  try
    if FItems.Count = 0 then
      Exit;

    if FItems.Count = 1 then
    begin
      Sink := IFFFrameSink(FItems[0]);
      Sink.TakeFrame(ASource, AFrame, AStreamIndex);
      Exit;
    end;

    CloneFrame := AFrame.Clone;
    try
      for I := 0 to FItems.Count - 1 do
      begin
        Sink := IFFFrameSink(FItems[I]);
        Sink.TakeFrame(ASource, CloneFrame, AStreamIndex);
      end;
    finally
      CloneFrame.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
