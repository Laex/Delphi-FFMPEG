unit uFFMemoryAccessAdapter;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Custom IO adapter for libavformat based on AVIOContext + TStream. }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavformat,
  libavutil,
  uFFException,
  uFFDesignTime;

type
  TFFMemoryAccessMode = (mamRead, mamWrite);

  TFFMemoryAccessAdapter = class(TComponent)
  private
    FStream: TStream;
    FOwnStream: Boolean;
    FMode: TFFMemoryAccessMode;
    FBufferSize: Integer;
    FBuffer: PByte;
    FIO: PAVIOContext;
    procedure SetStream(const Value: TStream);
    procedure FreeIO;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Attach(AStream: TStream; AMode: TFFMemoryAccessMode; AOwnStream: Boolean = False);
    procedure Detach;
    procedure EnsureAttached;

    property IOContext: PAVIOContext read FIO;
  published
    property Stream: TStream read FStream write SetStream;
    property OwnStream: Boolean read FOwnStream write FOwnStream default False;
    property Mode: TFFMemoryAccessMode read FMode write FMode default mamRead;
    property BufferSize: Integer read FBufferSize write FBufferSize default 65536;
  end;

implementation

function ReadPacketOpaque(opaque: Pointer; buf: puint8_t; buf_size: int): int; cdecl;
var
  A: TFFMemoryAccessAdapter;
  N: Integer;
begin
  Result := AVERROR_EOF;
  if (opaque = nil) or (buf = nil) or (buf_size <= 0) then
    Exit;
  A := TFFMemoryAccessAdapter(opaque);
  if (A.FStream = nil) then
    Exit;
  try
    N := A.FStream.Read(buf^, buf_size);
    if N > 0 then
      Result := N
    else
      Result := AVERROR_EOF;
  except
    Result := AVERROR_EOF;
  end;
end;

function WritePacketOpaque(opaque: Pointer; buf: puint8_t; buf_size: int): int; cdecl;
var
  A: TFFMemoryAccessAdapter;
begin
  Result := -1;
  if (opaque = nil) or (buf = nil) or (buf_size < 0) then
    Exit;
  A := TFFMemoryAccessAdapter(opaque);
  if A.FStream = nil then
    Exit;
  try
    A.FStream.WriteBuffer(buf^, buf_size);
    Result := buf_size;
  except
    Result := -1;
  end;
end;

function SeekOpaque(opaque: Pointer; offset: int64_t; whence: int): int64_t; cdecl;
var
  A: TFFMemoryAccessAdapter;
  Origin: TSeekOrigin;
begin
  Result := -1;
  if opaque = nil then
    Exit;
  A := TFFMemoryAccessAdapter(opaque);
  if A.FStream = nil then
    Exit;

  if whence = AVSEEK_SIZE then
    Exit(A.FStream.Size);

  case (whence and (not AVSEEK_FORCE)) of
    0 { SEEK_SET }: Origin := soBeginning;
    1 { SEEK_CUR }: Origin := soCurrent;
    2 { SEEK_END }: Origin := soEnd;
  else
    Origin := soBeginning;
  end;

  try
    Result := A.FStream.Seek(offset, Origin);
  except
    Result := -1;
  end;
end;

constructor TFFMemoryAccessAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufferSize := 64 * 1024;
  FMode := mamRead;
end;

destructor TFFMemoryAccessAdapter.Destroy;
begin
  Detach;
  inherited;
end;

procedure TFFMemoryAccessAdapter.FreeIO;
begin
  if FFIsDesignTime(Self) then
  begin
    FIO := nil;
    FBuffer := nil;
    Exit;
  end;
  if FIO <> nil then
  begin
    // avio_context_free() frees both the context and its buffer.
    avio_context_free(FIO);
    FIO := nil;
    FBuffer := nil;
    Exit;
  end;
  if FBuffer <> nil then
    av_free(FBuffer);
  FBuffer := nil;
end;

procedure TFFMemoryAccessAdapter.Attach(AStream: TStream; AMode: TFFMemoryAccessMode; AOwnStream: Boolean);
var
  WriteFlag: Integer;
  ReadCb: Tavio_alloc_context_read_packet;
  WriteCb: Tavio_alloc_context_write_packet;
begin
  Detach;
  if AStream = nil then
    raise EFFException.Create('TFFMemoryAccessAdapter.Attach: stream is nil');

  FStream := AStream;
  FOwnStream := AOwnStream;
  FMode := AMode;

  if FBufferSize <= 0 then
    FBufferSize := 64 * 1024;

  FBuffer := av_malloc(FBufferSize);
  if FBuffer = nil then
    raise EFFException.Create('av_malloc failed for AVIO buffer');

  if FMode = mamWrite then
  begin
    WriteFlag := 1;
    ReadCb := nil;
    WriteCb := WritePacketOpaque;
  end
  else
  begin
    WriteFlag := 0;
    ReadCb := ReadPacketOpaque;
    WriteCb := nil;
  end;

  FIO := avio_alloc_context(punsigned_char(FBuffer), FBufferSize, WriteFlag, Self, ReadCb, WriteCb, SeekOpaque);
  if FIO = nil then
  begin
    FreeIO;
    raise EFFException.Create('avio_alloc_context failed');
  end;
end;

procedure TFFMemoryAccessAdapter.Detach;
begin
  FreeIO;
  if FOwnStream then
    FreeAndNil(FStream)
  else
    FStream := nil;
  FOwnStream := False;
end;

procedure TFFMemoryAccessAdapter.SetStream(const Value: TStream);
begin
  if FStream = Value then
    Exit;
  Detach;
  FStream := Value;
  if FStream <> nil then
    Attach(FStream, FMode, FOwnStream);
end;

procedure TFFMemoryAccessAdapter.EnsureAttached;
begin
  if (FIO = nil) and (FStream <> nil) then
    Attach(FStream, FMode, FOwnStream);
end;

end.

