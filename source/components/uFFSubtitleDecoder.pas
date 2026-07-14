unit uFFSubtitleDecoder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Decode and index subtitle streams for playback overlay. }

interface

uses
  {$IFDEF FPC}
  Classes,
  SysUtils,
  {$ELSE}
  System.Classes,
  System.SysUtils,
  {$ENDIF}
  libavcodec,
  libavformat,
  libavutil,
  uFFException,
  uFFReader,
  uFFPacket,
  uFFComponentBase,
  uFFComponentLink,
  uFFDesignTime,
  uFFSubtitleBitmap,
  uFFSubtitleAss;

type
  TFFSubtitleEvent = record
    StartMs: Int64;
    EndMs: Int64;
    Text: string;
    IsBitmap: Boolean;
    Bitmap: TFFSubtitleBitmap;
    IsAss: Boolean;
    AssRaw: string;
  end;

  TFFSubtitleDecoder = class(TComponent, IFFPacketSink)
  private
    FReader: TFFReader;
    FStreamIndex: Integer;
    FCodecCtx: PAVCodecContext;
    FCodec: PAVCodec;
    FInitialized: Boolean;
    FEvents: array of TFFSubtitleEvent;
    FHasEvents: Boolean;
    procedure SetReader(const Value: TFFReader);
    procedure SubscribeReader;
    procedure UnsubscribeReader;
    procedure CloseCodec;
    procedure AppendSubtitle(var ASub: AVSubtitle);
    function PacketPtsToMs(APts: Int64): Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;

    procedure Initialize;
    procedure LoadAll;
    procedure LoadFromSrt(const AFileName: string);
    procedure ClearEvents;
    function GetEventAt(APositionMs: Int64): TFFSubtitleEvent;
    function GetTextAt(APositionMs: Int64): string;
    function EventCount: Integer;

    procedure TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);

    property Initialized: Boolean read FInitialized;
    property HasEvents: Boolean read FHasEvents;
  published
    property Reader: TFFReader read FReader write SetReader;
    property StreamIndex: Integer read FStreamIndex write FStreamIndex default -1;
  end;

implementation

procedure FFSubtitleStripAss(const AAss: string; var APlain: string);
var
  I: Integer;
  InTag: Boolean;
  C: Char;
begin
  APlain := '';
  InTag := False;
  for I := 1 to Length(AAss) do
  begin
    C := AAss[I];
    if C = '{' then
      InTag := True
    else if C = '}' then
      InTag := False
    else if not InTag then
      APlain := APlain + C;
  end;
  if Pos(',', APlain) > 0 then
    APlain := Copy(APlain, Pos(',', APlain) + 1, MaxInt);
  while (Length(APlain) > 0) and (APlain[1] = ' ') do
    Delete(APlain, 1, 1);
end;

constructor TFFSubtitleDecoder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStreamIndex := -1;
end;

destructor TFFSubtitleDecoder.Destroy;
begin
  UnsubscribeReader;
  if Assigned(FReader) then
    FReader.RemoveFreeNotification(Self);
  ClearEvents;
  CloseCodec;
  inherited;
end;

procedure TFFSubtitleDecoder.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FReader;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    UnsubscribeReader;
    CloseCodec;
    FReader := TFFReader(Link);
  end;
end;

procedure TFFSubtitleDecoder.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SubscribeReader;
end;

procedure TFFSubtitleDecoder.SubscribeReader;
begin
  if (FReader = nil) or (csDesigning in ComponentState) then
    Exit;
  FReader.SubscribePacketSink(Self);
end;

procedure TFFSubtitleDecoder.UnsubscribeReader;
begin
  if FReader = nil then
    Exit;
  FReader.UnsubscribePacketSink(Self);
end;

procedure TFFSubtitleDecoder.SetReader(const Value: TFFReader);
var
  Link: TComponent;
begin
  if FReader = Value then
    Exit;
  UnsubscribeReader;
  CloseCodec;
  FHasEvents := False;
  ClearEvents;
  Link := FReader;
  FFSetLinkedComponent(Self, Link, Value);
  FReader := TFFReader(Link);
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    SubscribeReader;
end;

procedure TFFSubtitleDecoder.CloseCodec;
begin
  if FFIsDesignTime(Self) then
  begin
    FCodecCtx := nil;
    FCodec := nil;
    FInitialized := False;
    Exit;
  end;
  if FCodecCtx <> nil then
    avcodec_free_context(FCodecCtx);
  FCodecCtx := nil;
  FCodec := nil;
  FInitialized := False;
end;

function TFFSubtitleDecoder.PacketPtsToMs(APts: Int64): Int64;
var
  St: PAVStream;
  MsBase: AVRational;
begin
  Result := -1;
  if (FReader = nil) or (FStreamIndex < 0) or (APts = AV_NOPTS_VALUE) then
    Exit;
  St := FReader.GetStream(FStreamIndex);
  MsBase := av_make_q(1, 1000);
  Result := av_rescale_q(APts, St^.time_base, MsBase);
end;

procedure TFFSubtitleDecoder.Initialize;
var
  St: PAVStream;
  Ret: Integer;
begin
  CloseCodec;
  if FReader = nil then
    raise EFFException.Create('TFFSubtitleDecoder.Reader is not assigned');
  if not FReader.Active then
    raise EFFException.Create('TFFSubtitleDecoder.Reader is not active');
  if FStreamIndex < 0 then
    raise EFFException.Create('TFFSubtitleDecoder.StreamIndex is not set');

  St := FReader.GetStream(FStreamIndex);
  if St^.codecpar^.codec_type <> AVMEDIA_TYPE_SUBTITLE then
    raise EFFException.Create('TFFSubtitleDecoder: stream is not a subtitle track');

  FCodec := avcodec_find_decoder(St^.codecpar^.codec_id);
  if FCodec = nil then
    raise EFFException.Create('avcodec_find_decoder failed for subtitle');

  FCodecCtx := avcodec_alloc_context3(FCodec);
  if FCodecCtx = nil then
    raise EFFException.Create('avcodec_alloc_context3 failed');

  Ret := avcodec_parameters_to_context(FCodecCtx, St^.codecpar);
  if Ret < 0 then
  begin
    CloseCodec;
    raise EFFException.CreateFmt('avcodec_parameters_to_context failed (%d)', [Ret]);
  end;

  Ret := avcodec_open2(FCodecCtx, FCodec, nil);
  if Ret < 0 then
  begin
    CloseCodec;
    raise EFFException.CreateFmt('avcodec_open2 failed (%d)', [Ret]);
  end;

  FInitialized := True;
end;

procedure TFFSubtitleDecoder.ClearEvents;
var
  I: Integer;
begin
  for I := 0 to High(FEvents) do
    FFSubtitleFreeBitmap(FEvents[I].Bitmap);
  SetLength(FEvents, 0);
  FHasEvents := False;
end;

procedure TFFSubtitleDecoder.AppendSubtitle(var ASub: AVSubtitle);
var
  I: Integer;
  Ev: TFFSubtitleEvent;
  Rect: pAVSubtitleRect;
  StartMs: Int64;
begin
  StartMs := PacketPtsToMs(ASub.pts);
  if StartMs < 0 then
    StartMs := 0;

  for I := 0 to Integer(ASub.num_rects) - 1 do
  begin
    Rect := ASub.rects[I];
    if Rect = nil then
      Continue;

    FillChar(Ev, SizeOf(Ev), 0);
    Ev.StartMs := StartMs + ASub.start_display_time;
    Ev.EndMs := StartMs + ASub.end_display_time;
    if Ev.EndMs <= Ev.StartMs then
      Ev.EndMs := Ev.StartMs + 3000;

    if Rect^._type = SUBTITLE_BITMAP then
    begin
      Ev.IsBitmap := True;
      FFSubtitleCopyBitmap(Rect, Ev.Bitmap);
    end
    else
    begin
      if Rect^.ass <> nil then
      begin
        Ev.IsAss := True;
        Ev.AssRaw := string(Rect^.ass);
      end;
      if Rect^.text <> nil then
        Ev.Text := Trim(string(Rect^.text));
      if Ev.IsAss then
      begin
        if Ev.AssRaw = '' then
          Ev.AssRaw := Ev.Text;
        Ev.Text := FFAssExtractDialogueText(Ev.AssRaw);
      end
      else if (Ev.Text = '') and (Rect^.ass <> nil) then
        FFSubtitleStripAss(string(Rect^.ass), Ev.Text);
      if (not Ev.IsAss) and (Pos('Dialogue:', Ev.Text) = 1) then
      begin
        Ev.IsAss := True;
        Ev.AssRaw := Ev.Text;
        Ev.Text := FFAssExtractDialogueText(Ev.AssRaw);
      end
      else if (not Ev.IsAss) and (Pos('{\', Ev.Text) > 0) then
      begin
        Ev.IsAss := True;
        Ev.AssRaw := Ev.Text;
        Ev.Text := FFAssExtractDialogueText(Ev.AssRaw);
      end;
    end;

    if (Ev.Text = '') and not Ev.IsBitmap and (not Ev.IsAss or (Ev.AssRaw = '')) then
      Continue;

    SetLength(FEvents, Length(FEvents) + 1);
    FEvents[High(FEvents)] := Ev;
  end;
end;

procedure TFFSubtitleDecoder.LoadFromSrt(const AFileName: string);
var
  Lines: TStringList;
  I, P: Integer;
  Ev: TFFSubtitleEvent;
  StartMs, EndMs: Int64;
  TextLines: TStringList;

  function ParseTimeMs(const S: string): Int64;
  var
    P: Integer;
    H, M, Sec, Ms: Integer;
    Part: string;
  begin
    Result := 0;
    Part := StringReplace(S, ',', '.', []);
    P := Pos(':', Part);
    if P <= 0 then Exit;
    H := StrToIntDef(Copy(Part, 1, P - 1), 0);
    Delete(Part, 1, P);
    P := Pos(':', Part);
    if P <= 0 then Exit;
    M := StrToIntDef(Copy(Part, 1, P - 1), 0);
    Delete(Part, 1, P);
    P := Pos(' ', Part);
    if P > 0 then
      Delete(Part, P, MaxInt);
    P := Pos('.', Part);
    if P > 0 then
    begin
      Sec := StrToIntDef(Copy(Part, 1, P - 1), 0);
      Ms := StrToIntDef(Copy(Part, P + 1, 3), 0);
    end
    else
    begin
      Sec := StrToIntDef(Part, 0);
      Ms := 0;
    end;
    Result := Int64(H) * 3600000 + Int64(M) * 60000 + Int64(Sec) * 1000 + Ms;
  end;

begin
  if not FileExists(AFileName) then
    Exit;
  Lines := TStringList.Create;
  TextLines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    I := 0;
    while I < Lines.Count do
    begin
      while (I < Lines.Count) and (Trim(Lines[I]) = '') do
        Inc(I);
      if I >= Lines.Count then
        Break;
      Inc(I); { cue number }
      if I >= Lines.Count then
        Break;
      P := Pos('-->', Lines[I]);
      if P <= 0 then
      begin
        Inc(I);
        Continue;
      end;
      StartMs := ParseTimeMs(Trim(Copy(Lines[I], 1, P - 1)));
      EndMs := ParseTimeMs(Trim(Copy(Lines[I], P + 3, MaxInt)));
      Inc(I);
      TextLines.Clear;
      while (I < Lines.Count) and (Trim(Lines[I]) <> '') do
      begin
        TextLines.Add(Trim(Lines[I]));
        Inc(I);
      end;
      if TextLines.Count = 0 then
        Continue;
      FillChar(Ev, SizeOf(Ev), 0);
      Ev.StartMs := StartMs;
      Ev.EndMs := EndMs;
      Ev.Text := TextLines.Text;
      SetLength(FEvents, Length(FEvents) + 1);
      FEvents[High(FEvents)] := Ev;
    end;
    FHasEvents := Length(FEvents) > 0;
  finally
    TextLines.Free;
    Lines.Free;
  end;
end;

procedure TFFSubtitleDecoder.LoadAll;
var
  Packet: TFFPacket;
  Sub: AVSubtitle;
  GotSub: Integer;
  Ret: Integer;
  WasAutoPump: Boolean;
  Sidecar: string;
begin
  ClearEvents;
  Initialize;

  WasAutoPump := FReader.AutoPump;
  if WasAutoPump then
    FReader.AutoPump := False;
  try
    FReader.Seek(0, -1);
    Packet := TFFPacket.Create;
    try
      while FReader.ReadPacket(Packet) do
      begin
      if Packet.Raw^.stream_index <> FStreamIndex then
        Continue;
      FillChar(Sub, SizeOf(Sub), 0);
      GotSub := 0;
      Ret := avcodec_decode_subtitle2(FCodecCtx, Sub, GotSub, Packet.Raw);
      if (Ret >= 0) and (GotSub <> 0) then
      begin
        try
          if Sub.pts = AV_NOPTS_VALUE then
            Sub.pts := Packet.Raw^.pts;
          AppendSubtitle(Sub);
        finally
          avsubtitle_free(@Sub);
        end;
      end;
      end;
    finally
      Packet.Free;
    end;
    FHasEvents := Length(FEvents) > 0;
    if not FHasEvents then
    begin
      Sidecar := ChangeFileExt(FReader.FileName, '.srt');
      if FileExists(Sidecar) then
        LoadFromSrt(Sidecar);
    end;
  finally
    if WasAutoPump then
      FReader.AutoPump := True;
  end;
end;

function TFFSubtitleDecoder.EventCount: Integer;
begin
  Result := Length(FEvents);
end;

function TFFSubtitleDecoder.GetEventAt(APositionMs: Int64): TFFSubtitleEvent;
var
  I: Integer;
  Found: Boolean;
begin
  FillChar(Result, SizeOf(Result), 0);
  Found := False;
  for I := 0 to High(FEvents) do
  begin
    if (APositionMs >= FEvents[I].StartMs) and (APositionMs < FEvents[I].EndMs) then
    begin
      Result := FEvents[I];
      Found := True;
      Break;
    end;
  end;
  if not Found then
    Result.Text := '';
end;

function TFFSubtitleDecoder.GetTextAt(APositionMs: Int64): string;
begin
  Result := GetEventAt(APositionMs).Text;
end;

procedure TFFSubtitleDecoder.TakePacket(ASource: TObject; APkt: TFFPacket; AStreamIndex: Integer);
var
  Sub: AVSubtitle;
  GotSub: Integer;
  Ret: Integer;
begin
  if (csDestroying in ComponentState) or (APkt = nil) or (AStreamIndex <> FStreamIndex) then
    Exit;
  if not FInitialized then
    Initialize;

  FillChar(Sub, SizeOf(Sub), 0);
  GotSub := 0;
  Ret := avcodec_decode_subtitle2(FCodecCtx, Sub, GotSub, APkt.Raw);
  if (Ret >= 0) and (GotSub <> 0) then
  begin
    try
      AppendSubtitle(Sub);
    finally
      avsubtitle_free(@Sub);
    end;
  end;
end;

end.
