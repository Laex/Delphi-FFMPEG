unit uFFFrameFilter;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ libavfilter graph wrapper for decoded video frames (graph link node). }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  {$I ../ffmpeg_rtl.inc},
  ffmpeg_types,
  libavutil,
  libavfilter,
  libavformat,
  uFFException,
  uFFReader,
  uFFDecoder,
  uFFFrame,
  uFFComponentBase,
  uFFComponentLink,
  uFFDesignTime;

type
  TFFFrameFilter = class(TComponent, IFFFrameSink, IFFFrameSource)
  private
    FInputDecoder: TFFDecoder;
    FFilterDescription: string;
    FGraph: PAVFilterGraph;
    FBufferSrc: PAVFilterContext;
    FBufferSink: PAVFilterContext;
    FConfigured: Boolean;
    FFrameSinks: TFFFrameSinkList;
    FOutFrame: TFFFrame;
    procedure SetInputDecoder(const Value: TFFDecoder);
    procedure SetFilterDescription(const Value: string);
    procedure SubscribeInput;
    procedure UnsubscribeInput;
    procedure CloseGraph;
    procedure EnsureOutFrame;
    procedure EnsureGraph(AFrame: TFFFrame; AStreamIndex: Integer);
    function ProcessFrame(AFrame: TFFFrame): TFFFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;

    function ApplyFrame(AFrame: TFFFrame; AStreamIndex: Integer): TFFFrame;
    procedure TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
    procedure SubscribeFrameSink(const ASink: IFFFrameSink);
    procedure UnsubscribeFrameSink(const ASink: IFFFrameSink);

    property Configured: Boolean read FConfigured;
  published
    property InputDecoder: TFFDecoder read FInputDecoder write SetInputDecoder;
    property FilterDescription: string read FFilterDescription write SetFilterDescription;
  end;

implementation

procedure TFFFrameFilter.CloseGraph;
begin
  if FGraph <> nil then
  begin
    if not FFIsDesignTime(Self) then
      avfilter_graph_free(FGraph);
    FGraph := nil;
  end;
  FBufferSrc := nil;
  FBufferSink := nil;
  FConfigured := False;
end;

constructor TFFFrameFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilterDescription := 'null';
  FFrameSinks := TFFFrameSinkList.Create;
end;

procedure TFFFrameFilter.EnsureOutFrame;
begin
  if FOutFrame = nil then
    FOutFrame := TFFFrame.Create;
end;

destructor TFFFrameFilter.Destroy;
begin
  UnsubscribeInput;
  if Assigned(FInputDecoder) then
    FInputDecoder.RemoveFreeNotification(Self);
  FFrameSinks.Free;
  CloseGraph;
  FOutFrame.Free;
  inherited;
end;

procedure TFFFrameFilter.Notification(AComponent: TComponent; Operation: TOperation);
var
  Link: TComponent;
begin
  inherited Notification(AComponent, Operation);
  Link := FInputDecoder;
  if FFHandleLinkRemoval(Link, AComponent, Operation) then
  begin
    UnsubscribeInput;
    CloseGraph;
    FInputDecoder := TFFDecoder(Link);
  end;
end;

procedure TFFFrameFilter.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SubscribeInput;
end;

procedure TFFFrameFilter.SubscribeInput;
begin
  if (FInputDecoder = nil) or (csDesigning in ComponentState) then
    Exit;
  FInputDecoder.SubscribeFrameSink(Self);
end;

procedure TFFFrameFilter.UnsubscribeInput;
begin
  if FInputDecoder = nil then
    Exit;
  FInputDecoder.UnsubscribeFrameSink(Self);
end;

procedure TFFFrameFilter.SetInputDecoder(const Value: TFFDecoder);
var
  Link: TComponent;
begin
  if FInputDecoder = Value then
    Exit;
  UnsubscribeInput;
  CloseGraph;
  Link := FInputDecoder;
  FFSetLinkedComponent(Self, Link, Value);
  FInputDecoder := TFFDecoder(Link);
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    SubscribeInput;
end;

procedure TFFFrameFilter.SetFilterDescription(const Value: string);
begin
  if FFilterDescription = Value then
    Exit;
  FFilterDescription := Value;
  CloseGraph;
end;

procedure TFFFrameFilter.EnsureGraph(AFrame: TFFFrame; AStreamIndex: Integer);
var
  Reader: TFFReader;
  St: PAVStream;
  Args: AnsiString;
  Desc: AnsiString;
  BufferSrcFilter: pAVFilter;
  BufferSinkFilter: pAVFilter;
  Outputs: pAVFilterInOut;
  Inputs: pAVFilterInOut;
  Ret: Integer;
begin
  if (FFIsDesignTime(Self)) or FConfigured or (AFrame = nil) or
    (FInputDecoder = nil) or (FInputDecoder.Reader = nil) then
    Exit;

  Reader := FInputDecoder.Reader;
  St := Reader.GetStream(AStreamIndex);

  CloseGraph;
  FGraph := avfilter_graph_alloc;
  if FGraph = nil then
    raise EFFException.Create('TFFFrameFilter: avfilter_graph_alloc failed');

  BufferSrcFilter := avfilter_get_by_name('buffer');
  BufferSinkFilter := avfilter_get_by_name('buffersink');
  if (BufferSrcFilter = nil) or (BufferSinkFilter = nil) then
    raise EFFException.Create('TFFFrameFilter: buffer/buffersink filters not found');

  Args := AnsiString(Format('video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=%d/%d',
    [AFrame.Raw^.width, AFrame.Raw^.height, Integer(AFrame.Raw^.format),
    St^.time_base.num, St^.time_base.den,
    St^.sample_aspect_ratio.num, St^.sample_aspect_ratio.den]));

  Ret := avfilter_graph_create_filter(FBufferSrc, BufferSrcFilter, 'in', PAnsiChar(Args), nil, FGraph);
  if Ret < 0 then
    raise EFFException.CreateFmt('TFFFrameFilter: buffersrc failed (%d)', [Ret]);

  Ret := avfilter_graph_create_filter(FBufferSink, BufferSinkFilter, 'out', nil, nil, FGraph);
  if Ret < 0 then
    raise EFFException.CreateFmt('TFFFrameFilter: buffersink failed (%d)', [Ret]);

  Outputs := avfilter_inout_alloc;
  Inputs := avfilter_inout_alloc;
  try
    Outputs^.name := av_strdup('in');
    Outputs^.filter_ctx := FBufferSrc;
    Outputs^.pad_idx := 0;
    Outputs^.next := nil;

    Inputs^.name := av_strdup('out');
    Inputs^.filter_ctx := FBufferSink;
    Inputs^.pad_idx := 0;
    Inputs^.next := nil;

    if FFilterDescription = '' then
      Desc := 'null'
    else
      Desc := AnsiString(FFilterDescription);

    Ret := avfilter_graph_parse_ptr(FGraph, PAnsiChar(Desc), Inputs, Outputs, nil);
    if Ret < 0 then
      raise EFFException.CreateFmt('TFFFrameFilter: parse failed (%d)', [Ret]);

    Ret := avfilter_graph_config(FGraph, nil);
    if Ret < 0 then
      raise EFFException.CreateFmt('TFFFrameFilter: config failed (%d)', [Ret]);
  finally
    avfilter_inout_free(Inputs);
    avfilter_inout_free(Outputs);
  end;

  FConfigured := True;
end;

function TFFFrameFilter.ProcessFrame(AFrame: TFFFrame): TFFFrame;
var
  Ret: Integer;
begin
  Result := AFrame;
  if not FConfigured then
    Exit;

  EnsureOutFrame;
  Ret := av_buffersrc_add_frame_flags(FBufferSrc, AFrame.Raw, AV_BUFFERSRC_FLAG_KEEP_REF);
  if Ret < 0 then
    raise EFFException.CreateFmt('TFFFrameFilter: buffersrc add frame failed (%d)', [Ret]);

  Ret := av_buffersink_get_frame(FBufferSink, FOutFrame.Raw);
  if Ret < 0 then
    raise EFFException.CreateFmt('TFFFrameFilter: buffersink get frame failed (%d)', [Ret]);

  Result := FOutFrame;
end;

function TFFFrameFilter.ApplyFrame(AFrame: TFFFrame; AStreamIndex: Integer): TFFFrame;
begin
  if (AFrame = nil) or (csDestroying in ComponentState) then
    Exit(nil);
  EnsureGraph(AFrame, AStreamIndex);
  Result := ProcessFrame(AFrame);
end;

procedure TFFFrameFilter.TakeFrame(ASource: TObject; AFrame: TFFFrame; AStreamIndex: Integer);
var
  OutFrame: TFFFrame;
begin
  if (csDestroying in ComponentState) or (csDesigning in ComponentState) or (AFrame = nil) then
    Exit;

  OutFrame := ApplyFrame(AFrame, AStreamIndex);
  if OutFrame <> nil then
    FFrameSinks.Notify(Self, OutFrame, AStreamIndex);
end;

procedure TFFFrameFilter.SubscribeFrameSink(const ASink: IFFFrameSink);
begin
  if (ASink = nil) or (csDesigning in ComponentState) then
    Exit;
  FFrameSinks.Add(ASink);
end;

procedure TFFFrameFilter.UnsubscribeFrameSink(const ASink: IFFFrameSink);
begin
  if ASink = nil then
    Exit;
  FFrameSinks.Remove(ASink);
end;

end.
