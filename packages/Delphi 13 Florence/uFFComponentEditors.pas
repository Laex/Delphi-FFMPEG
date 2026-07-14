unit uFFComponentEditors;

{ Design-time dropdown editors for FFmpeg component link properties. }

interface

procedure Register;

implementation

uses
  System.Classes,
  DesignIntf,
  DesignEditors,
  uFFReader,
  uFFDecoder,
  uFFEncoder,
  uFFWriter,
  uFFMemoryAccessAdapter,
  uFFBitmapEncoder,
  uFFTranscodePreset,
  uFFRemuxJob,
  uFFFrameFilter,
  uFFFMXVideoPlayer
  {$IFDEF MSWINDOWS}
  , uFFVideoPlayer
  {$ENDIF}
  ;

type
  TFFComponentLinkProperty = class(TComponentProperty)
  private
    FLinkClass: TComponentClass;
  protected
    procedure SetLinkClass(const AClass: TComponentClass);
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TFFReaderProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

  TFFDecoderProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

  TFFEncoderProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

  TFFWriterProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

  TFFMemoryAccessAdapterProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

  TFFFrameFilterProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

  TFFEncoderWriterProperty = class(TFFComponentLinkProperty)
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
  end;

{ TFFComponentLinkProperty }

constructor TFFComponentLinkProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  FLinkClass := nil;
end;

procedure TFFComponentLinkProperty.SetLinkClass(const AClass: TComponentClass);
begin
  FLinkClass := AClass;
end;

procedure TFFComponentLinkProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Current, Comp, Root: TComponent;
begin
  if PropCount <= 0 then
    Exit;
  Proc('');
  if FLinkClass = nil then
    Exit;

  Current := GetComponent(0) as TComponent;
  if Designer = nil then
    Exit;
  Root := Designer.Root;
  if Root = nil then
    Exit;

  for I := 0 to Root.ComponentCount - 1 do
  begin
    Comp := Root.Components[I];
    if (Comp is FLinkClass) and (Comp <> Current) then
      Proc(Comp.Name);
  end;
end;

{ TFFReaderProperty }

constructor TFFReaderProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFReader);
end;

{ TFFDecoderProperty }

constructor TFFDecoderProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFDecoder);
end;

{ TFFEncoderProperty }

constructor TFFEncoderProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFEncoder);
end;

{ TFFWriterProperty }

constructor TFFWriterProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFWriter);
end;

{ TFFMemoryAccessAdapterProperty }

constructor TFFMemoryAccessAdapterProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFMemoryAccessAdapter);
end;

{ TFFFrameFilterProperty }

constructor TFFFrameFilterProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFFrameFilter);
end;

{ TFFEncoderWriterProperty }

constructor TFFEncoderWriterProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  SetLinkClass(TFFWriter);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TComponent), TFFEncoder, 'OutputWriter', TFFEncoderWriterProperty);
  RegisterPropertyEditor(TypeInfo(TFFWriter), TFFBitmapEncoder, 'OutputWriter', TFFWriterProperty);
  RegisterPropertyEditor(TypeInfo(TFFMemoryAccessAdapter), TFFReader, 'InputAdapter', TFFMemoryAccessAdapterProperty);
  RegisterPropertyEditor(TypeInfo(TFFMemoryAccessAdapter), TFFWriter, 'OutputAdapter', TFFMemoryAccessAdapterProperty);

  RegisterPropertyEditor(TypeInfo(TFFReader), TFFEncoder, 'Reader', TFFReaderProperty);
  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFEncoder, 'InputDecoder', TFFDecoderProperty);
  RegisterPropertyEditor(TypeInfo(TFFFrameFilter), TFFEncoder, 'FrameFilter', TFFFrameFilterProperty);

  RegisterPropertyEditor(TypeInfo(TFFReader), TFFTranscodeJob, 'Reader', TFFReaderProperty);
  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFTranscodeJob, 'InputDecoder', TFFDecoderProperty);
  RegisterPropertyEditor(TypeInfo(TFFEncoder), TFFTranscodeJob, 'Encoder', TFFEncoderProperty);
  RegisterPropertyEditor(TypeInfo(TFFWriter), TFFTranscodeJob, 'Writer', TFFWriterProperty);
  RegisterPropertyEditor(TypeInfo(TFFFrameFilter), TFFTranscodeJob, 'FrameFilter', TFFFrameFilterProperty);

  RegisterPropertyEditor(TypeInfo(TFFReader), TFFRemuxJob, 'Reader', TFFReaderProperty);
  RegisterPropertyEditor(TypeInfo(TFFWriter), TFFRemuxJob, 'Writer', TFFWriterProperty);

  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFFrameFilter, 'InputDecoder', TFFDecoderProperty);

  {$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFVideoPlayer, 'VideoDecoder', TFFDecoderProperty);
  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFVideoPlayer, 'AudioDecoder', TFFDecoderProperty);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFFMXVideoPlayer, 'VideoDecoder', TFFDecoderProperty);
  RegisterPropertyEditor(TypeInfo(TFFDecoder), TFFFMXVideoPlayer, 'AudioDecoder', TFFDecoderProperty);
end;

end.
