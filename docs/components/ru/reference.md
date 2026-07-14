## Справочник компонентов

### TFFReader (`uFFReader.pas`)

| | |
|---|---|
| **Свойства** | `FileName`, `InputAdapter`, `Active`, `AutoPump`, `Duration` (мкс), `Streams` |
| **Методы** | `Open`, `Close`, `ReadPacket`, `Seek` (мкс; `StreamIndex = -1` → `AV_TIME_BASE`) |
| **События** | `OnOpen`, `OnClose` |
| **Graph** | `SubscribePacketSink` / `UnsubscribePacketSink` |

### TFFDecoder (`uFFDecoder.pas`)

| | |
|---|---|
| **Свойства** | `Reader`, `StreamIndex`, `AutoInitialize`, `PreviewMaxPackets`, `CodecName`, `HardwareDevice` |
| **Методы** | `Initialize`, `CloseCodec`, `SendPacket`, `ReceiveFrame`, `Flush`, `DecodeFrameAt`, `DecodeNextFrame` |
| **События** | `OnFrameHook`, `OnFrameDecoded` |
| **Graph** | `TakePacket`, `SubscribeFrameSink` / `UnsubscribeFrameSink` |
| **Windows** | `FFDecoderPreviewToBitmap`, `FFDecoderFrameToBitmap` |

### TFFEncoder (`uFFEncoder.pas`)

| | |
|---|---|
| **Свойства** | `MediaType`, `CodecName`, `BitRate`, `Width`/`Height`, `SampleRate`/`Channels`, `Reader`, `InputDecoder`, `OutputWriter`, `CopyAudio`, `TranscodeAudio` |
| **Методы** | `Initialize`, `SendFrame`, `ReceivePacket`, `Flush`, `Start`, `Pause`, `Resume`, `Stop` |
| **События** | `OnProgress`, `OnPreviewFrame`, `OnStateChange`, `OnFrameHook` |

### TFFWriter (`uFFWriter.pas`)

| | |
|---|---|
| **Свойства** | `FileName`, `FormatName`, `OutputAdapter`, `RemuxReader`, `VideoEncoder`, `AudioEncoder` |
| **Методы** | `Open`, `AddStream`, `AddStreamCopy`, `WriteHeader`, `WritePacket`, `WriteTrailer`, `Close`, `SetupFromLinks` |
| **Graph** | `TakePacket` (remux sink) |

### TFFPlaybackEngine (`uFFPlaybackEngine.pas`)

| | |
|---|---|
| **Свойства** | `FileName`, `HardwareDevice`, `State`, `Position`, `Duration`, `Volume`, `VideoStreamIndex`, `AudioStreamIndex` |
| **Методы** | `Play`, `Pause`, `Stop`, `SeekTo` |
| **События** | `OnPresentFrame`, `OnPositionChange`, `OnStateChange`, `OnFrameHook`, `OnVideoHook`, `OnAudioHook` |

### TFFVideoPlayer (`uFFVideoPlayer.pas`, VCL)

| | |
|---|---|
| **Свойства** | `FileName`, `VideoDecoder`, `AudioDecoder`, `SubtitleDecoder`, `HardwareDevice`, `State`, `Position`, `Duration`, `Volume` |
| **Методы** | `Play`, `Pause`, `Stop`, `SeekTo` |
| **События** | `OnStateChange`, `OnFrameHook`, `OnVideoHook`, `OnAudioHook` |

### TFFSubtitleDecoder (`uFFSubtitleDecoder.pas`)

| | |
|---|---|
| **Свойства** | `Reader`, `StreamIndex`, `Initialized`, `HasEvents` |
| **Методы** | `Initialize`, `LoadAll`, `LoadFromSrt`, `GetEventAt`, `GetTextAt`, `TakePacket` |

`TFFSubtitleEvent`: `StartMs`, `EndMs`, `Text`, `IsBitmap` + `Bitmap`, `IsAss` + `AssRaw`.

### Вспомогательные классы

#### TFFFrameConverter (`uFFFrameConverter.pas`)

`Configure`, `Convert` → BGRA (`AV_PIX_FMT_BGRA` по умолчанию).

#### TFFFrameBitmap (`uFFFrameBitmap.pas`, VCL)

`AssignBgraBuffer`, копирование BGRA → `TBitmap`.

#### TFFAudioResampler (`uFFAudioResampler.pas`)

PCM S16, 44100 Hz, stereo по умолчанию.

#### TFFAudioOutput (`uFFAudioOutput.pas`)

| Платформа | Модуль |
|-----------|--------|
| Windows | `uFFAudioOutputWin` (WaveOut) |
| Linux | `uFFAudioOutputALSA` → `uFFAudioOutputSDL` |

#### TFFPacketQueue (`uFFPacketQueue.pas`)

Потокобезопасная очередь `TFFPacket` для demux.

#### TFFPlaybackClock (`uFFPlaybackClock.pas`)

`Reset`, `Pause`, `Resume`, `GetTimeMs`, `SetAudioTimeMs`, `WaitUntil`, `IsLate`.

#### TFFMemoryAccessAdapter (`uFFMemoryAccessAdapter.pas`)

`AVIOContext` поверх `TStream` для `TFFReader` / `TFFWriter`.

#### TFFBitmapEncoder (`uFFBitmapEncoder.pas`)

Запись видео из BGRA-кадров / `TBitmap`.

#### TFFMediaInfo (`uFFMediaInfo.pas`)

`Probe`, `FindBestStream`, длительность, кодеки без decode.

#### TFFThumbnailExtractor (`uFFThumbnailExtractor.pas`)

Кадр по позиции → bitmap / файл.

#### TFFFrameFilter (`uFFFrameFilter.pas`)

Узел libavfilter в graph: `FilterDescription` (например `scale=1280:720`).

#### TFFTranscodeJob / TFFRemuxJob

Высокоуровневые preset-based transcode и stream-copy remux.

#### TFFHardwareDecode (`uFFHardwareDecode.pas`)

`TFFHardwareDecodeContext.TrySetup`, `TransferToSoftware` – используется внутри `TFFDecoder`.

#### TFFLoader / TFFLogger

Загрузка DLL и перехват `av_log`.

### Исключения

`EFFException` (`uFFException.pas`) – ошибки FFmpeg с текстом из `av_strerror`.
