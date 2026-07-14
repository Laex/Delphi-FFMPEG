unit uFFmpegReg;

interface

procedure Register;

implementation

{$R FFmpegComponents.dcr}

uses
  System.Classes,
  DesignIntf,
  uFFLoader,
  uFFLogger,
  uFFReader,
  uFFDecoder,
  uFFEncoder,
  uFFWriter,
  uFFMemoryAccessAdapter,
  uFFBitmapEncoder,
  uFFMediaInfo,
  uFFThumbnailExtractor,
  uFFTranscodePreset,
  uFFRemuxJob,
  uFFFrameFilter,
  uFFHardwareDecode,
  uFFSubtitleDecoder,
  uFFPlayerControl,
  uFFFMXVideoPlayer
  {$IFDEF MSWINDOWS}
  , uFFVideoPlayer
  {$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('FFmpeg', [
    TFFLoader,
    TFFLogger,
    TFFReader,
    TFFDecoder,
    TFFEncoder,
    TFFWriter,
    TFFMemoryAccessAdapter,
    TFFBitmapEncoder,
    TFFMediaInfo,
    TFFThumbnailExtractor,
    TFFTranscodeJob,
    TFFRemuxJob,
    TFFFrameFilter,
    TFFSubtitleDecoder,
    TFFFMXVideoPlayer
    {$IFDEF MSWINDOWS}
    , TFFVideoPlayer
    , TFFPlayerControl
    {$ENDIF}
  ]);
end;

initialization
  try
    TFFLoader.EnsureDesignTimeLoaded;
  except
    // Design-time package must load even when FFmpeg DLLs are not on PATH yet.
  end;

end.
