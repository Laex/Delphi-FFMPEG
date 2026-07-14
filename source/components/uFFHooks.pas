unit uFFHooks;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{ Shared hook event types for playback and encoding pipelines. }

interface

uses
  {$IFDEF FPC}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  uFFFrame;

type
  { Called on the decode thread with the native AVFrame wrapper.
    Set AHandled = True to skip default processing (convert / present / resample). }
  TFFFrameHookEvent = procedure(Sender: TObject; AFrame: TFFFrame; var AHandled: Boolean) of object;

  { Called on the decode thread with a BGRA buffer (in-place edits allowed). }
  TFFVideoHookEvent = procedure(Sender: TObject; ABgra: PByte; AWidth, AHeight, AStride: Integer) of object;

  { Called on the decode thread with PCM S16 data before audio output.
    AByteCount may be reduced; set to 0 to drop the buffer. }
  TFFAudioHookEvent = procedure(Sender: TObject; ABuffer: PByte; var AByteCount: Integer) of object;

  { Called during encode/transcode with a decoded source frame before encoding. }
  TFFPreviewFrameEvent = procedure(Sender: TObject; AFrame: TFFFrame; APositionMs: Int64) of object;

implementation

end.
