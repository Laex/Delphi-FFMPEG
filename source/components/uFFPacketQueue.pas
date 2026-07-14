unit uFFPacketQueue;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Thread-safe FIFO queue of owned TFFPacket instances. }

interface

uses
  {$IFDEF FPC}
  Classes,
  SyncObjs,
  {$ELSE}
  System.Classes,
  System.SyncObjs,
  {$ENDIF}
  uFFPacket;

type
  TFFPacketQueue = class
  private
    FLock: TCriticalSection;
    FNotEmpty: TEvent;
    FItems: TList;
    FMaxSize: Integer;
    FClosed: Boolean;
    function GetCount: Integer;
  public
    constructor Create(AMaxSize: Integer = 64);
    destructor Destroy; override;

    procedure Push(APacket: TFFPacket);
    function Pop(ATimeoutMs: Cardinal): TFFPacket;
    function TryPop: TFFPacket;
    procedure Close;
    procedure Clear;

    property Count: Integer read GetCount;
    property MaxSize: Integer read FMaxSize;
    property Closed: Boolean read FClosed;
  end;

implementation

constructor TFFPacketQueue.Create(AMaxSize: Integer);
begin
  inherited Create;
  FMaxSize := AMaxSize;
  if FMaxSize < 1 then
    FMaxSize := 1;
  FLock := TCriticalSection.Create;
  FNotEmpty := TEvent.Create(nil, False, False, '');
  FItems := TList.Create;
end;

destructor TFFPacketQueue.Destroy;
begin
  Clear;
  FItems.Free;
  FNotEmpty.Free;
  FLock.Free;
  inherited;
end;

procedure TFFPacketQueue.Clear;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to FItems.Count - 1 do
      TFFPacket(FItems[I]).Free;
    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TFFPacketQueue.Close;
begin
  FLock.Enter;
  try
    FClosed := True;
  finally
    FLock.Leave;
  end;
  FNotEmpty.SetEvent;
end;

function TFFPacketQueue.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

function TFFPacketQueue.Pop(ATimeoutMs: Cardinal): TFFPacket;
begin
  while True do
  begin
    FLock.Enter;
    try
      if FItems.Count > 0 then
      begin
        Result := TFFPacket(FItems[0]);
        FItems.Delete(0);
        Exit;
      end;
      if FClosed then
        Exit(nil);
    finally
      FLock.Leave;
    end;

    if FNotEmpty.WaitFor(ATimeoutMs) = wrTimeout then
      Exit(nil);
  end;
end;

procedure TFFPacketQueue.Push(APacket: TFFPacket);
var
  Owned: TFFPacket;
begin
  if (APacket = nil) or FClosed then
    Exit;

  Owned := APacket.Clone;
  FLock.Enter;
  try
    if FClosed then
    begin
      Owned.Free;
      Exit;
    end;
    FItems.Add(Owned);
  finally
    FLock.Leave;
  end;
  FNotEmpty.SetEvent;
end;

function TFFPacketQueue.TryPop: TFFPacket;
begin
  Result := Pop(0);
end;

end.
