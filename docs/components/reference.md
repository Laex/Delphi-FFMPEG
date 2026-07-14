## Component reference

### TFFReader (`uFFReader.pas`)

| | |
|---|---|
| **Properties** | `FileName`, `InputAdapter`, `Active`, `AutoPump`, `Duration` (µs), `Streams` |
| **Methods** | `Open`, `Close`, `ReadPacket`, `Seek` (µs; `StreamIndex = -1` → `AV_TIME_BASE`) |
| **Events** | `OnOpen`, `OnClose` |
| **Graph** | `SubscribePacketSink` / `UnsubscribePacketSink` |

### TFFDecoder (`uFFDecoder.pas`)

| | |
|---|---|
| **Properties** | `Reader`, `StreamIndex`, `AutoInitialize`, `PreviewMaxPackets`, `CodecName`, `HardwareDevice` |
| **Methods** | `Initialize`, `CloseCodec`, `SendPacket`, `ReceiveFrame`, `Flush`, `DecodeFrameAt`, `DecodeNextFrame` |
| **Events** | `OnFrameHook`, `OnFrameDecoded` |
| **Graph** | `TakePacket`, `SubscribeFrameSink` / `UnsubscribeFrameSink` |
| **Windows** | `FFDecoderPreviewToBitmap`, `FFDecoderFrameToBitmap` |

### TFFEncoder (`uFFEncoder.pas`)

| | |
|---|---|
| **Properties** | `MediaType`, `CodecName`, `BitRate`, `Width`/`Height`, `SampleRate`/`Channels`, `Reader`, `InputDecoder`, `OutputWriter`, `CopyAudio`, `TranscodeAudio` |
| **Methods** | `Initialize`, `SendFrame`, `ReceivePacket`, `Flush`, `Start`, `Pause`, `Resume`, `Stop` |
| **Events** | `OnProgress`, `OnPreviewFrame`, `OnStateChange`, `OnFrameHook` |

### TFFWriter (`uFFWriter.pas`)

| | |
|---|---|
| **Properties** | `FileName`, `FormatName`, `OutputAdapter`, `RemuxReader`, `VideoEncoder`, `AudioEncoder` |
| **Methods** | `Open`, `AddStream`, `AddStreamCopy`, `WriteHeader`, `WritePacket`, `WriteTrailer`, `Close`, `SetupFromLinks` |
| **Graph** | `TakePacket` (remux sink) |

### TFFPlaybackEngine (`uFFPlaybackEngine.pas`)

| | |
|---|---|
| **Properties** | `FileName`, `HardwareDevice`, `State`, `Position`, `Duration`, `Volume`, `VideoStreamIndex`, `AudioStreamIndex` |
| **Methods** | `Play`, `Pause`, `Stop`, `SeekTo` |
| **Events** | `OnPresentFrame`, `OnPositionChange`, `OnStateChange`, `OnFrameHook`, `OnVideoHook`, `OnAudioHook` |

### TFFVideoPlayer (`uFFVideoPlayer.pas`, VCL)

| | |
|---|---|
| **Properties** | `FileName`, `VideoDecoder`, `AudioDecoder`, `SubtitleDecoder`, `HardwareDevice`, `State`, `Position`, `Duration`, `Volume` |
| **Methods** | `Play`, `Pause`, `Stop`, `SeekTo` |
| **Events** | `OnStateChange`, `OnFrameHook`, `OnVideoHook`, `OnAudioHook` |

### TFFSubtitleDecoder (`uFFSubtitleDecoder.pas`)

| | |
|---|---|
| **Properties** | `Reader`, `StreamIndex`, `Initialized`, `HasEvents` |
| **Methods** | `Initialize`, `LoadAll`, `LoadFromSrt`, `GetEventAt`, `GetTextAt`, `TakePacket` |

`TFFSubtitleEvent`: `StartMs`, `EndMs`, `Text`, `IsBitmap` + `Bitmap`, `IsAss` + `AssRaw`.

### Helper classes

#### TFFFrameConverter (`uFFFrameConverter.pas`)

`Configure`, `Convert` → BGRA (`AV_PIX_FMT_BGRA` by default).

#### TFFFrameBitmap (`uFFFrameBitmap.pas`, VCL)

`AssignBgraBuffer`, copy BGRA → `TBitmap`.

#### TFFAudioResampler (`uFFAudioResampler.pas`)

PCM S16, 44100 Hz, stereo by default.

#### TFFAudioOutput (`uFFAudioOutput.pas`)

| Platform | Unit |
|----------|------|
| Windows | `uFFAudioOutputWin` (WaveOut) |
| Linux | `uFFAudioOutputALSA` → `uFFAudioOutputSDL` |

#### TFFPacketQueue (`uFFPacketQueue.pas`)

Thread-safe `TFFPacket` queue for demux.

#### TFFPlaybackClock (`uFFPlaybackClock.pas`)

`Reset`, `Pause`, `Resume`, `GetTimeMs`, `SetAudioTimeMs`, `WaitUntil`, `IsLate`.

#### TFFMemoryAccessAdapter (`uFFMemoryAccessAdapter.pas`)

`AVIOContext` over `TStream` for `TFFReader` / `TFFWriter`.

#### TFFBitmapEncoder (`uFFBitmapEncoder.pas`)

Write video from BGRA frames / `TBitmap`.

#### TFFMediaInfo (`uFFMediaInfo.pas`)

`Probe`, `FindBestStream`, duration and codecs without decode.

#### TFFThumbnailExtractor (`uFFThumbnailExtractor.pas`)

Frame at position → bitmap / file.

#### TFFFrameFilter (`uFFFrameFilter.pas`)

libavfilter graph node: `FilterDescription` (e.g. `scale=1280:720`).

#### TFFTranscodeJob / TFFRemuxJob

High-level preset-based transcode and stream-copy remux.

#### TFFHardwareDecode (`uFFHardwareDecode.pas`)

`TFFHardwareDecodeContext.TrySetup`, `TransferToSoftware` – used inside `TFFDecoder`.

#### TFFLoader / TFFLogger

DLL loading and `av_log` capture.

### Exceptions

`EFFException` (`uFFException.pas`) – FFmpeg errors with text from `av_strerror`.
