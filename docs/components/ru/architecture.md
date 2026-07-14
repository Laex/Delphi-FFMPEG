## Архитектура

### Слои

```
+---------------------------------------------------------------+
|  Приложение (формы, UI, бизнес-логика)                        |
+---------------------------------------------------------------+
|  Компоненты (TFFReader, TFFDecoder, TFFVideoPlayer, …)      |
+---------------------------------------------------------------+
|  Обёртки ресурсов (TFFPacket, TFFFrame, TFFException)         |
+---------------------------------------------------------------+
|  Биндинги (libavcodec, libavformat, libavutil, …)             |
+---------------------------------------------------------------+
```

#### Обёртки ресурсов

| Класс | Модуль | FFmpeg API |
|-------|--------|------------|
| `TFFPacket` | `uFFPacket.pas` | `av_packet_alloc` / `av_packet_free` |
| `TFFFrame` | `uFFFrame.pas` | `av_frame_alloc` / `av_frame_free` |

#### Невизуальные компоненты

Чтение, декодирование, кодирование, запись, transcode – см. [Справочник](reference.md).

#### Визуальные

`TFFVideoPlayer` (VCL), `TFFFMXVideoPlayer` (FMX), `TFFPlayerControl` (VCL + UI transport).

### Graph linking

Push-модель через интерфейсы в `uFFComponentBase`:
- `IFFPacketSink` / `IFFPacketSource` – пакеты demux
- `IFFFrameSink` / `IFFFrameSource` – декодированные кадры

```
TFFReader (AutoPump)
    ├─► TFFDecoder (video) ──► TFFVideoPlayer
    ├─► TFFDecoder (audio) ──► TFFAudioOutput (linked)
    └─► TFFWriter (remux)

TFFReader + TFFDecoder + TFFEncoder + TFFWriter   → transcode (TFFEncoder.Start)
TFFBitmapEncoder + TFFWriter                      → bitmap → video
TFFMemoryAccessAdapter + TFFReader/TFFWriter      → TStream I/O
```

Свойства связей (`Reader`, `VideoDecoder`, …) поддерживают `FreeNotification` и design-time dropdown (`uFFComponentEditors`).

`TFFReader.AutoPump := True` – фоновый поток читает пакеты и рассылает подписчикам (`TakePacket`).

### Многопоточность воспроизведения

**Engine mode** (`TFFPlaybackEngine`):
1. **Demux thread** – `ReadPacket` → `TFFPacketQueue` (video/audio).
2. **Video decode thread** – decode → `TFFFrameConverter` (BGRA) → `OnPresentFrame`.
3. **Audio decode thread** – decode → `TFFAudioResampler` → `TFFAudioOutput`.

**Linked mode** (`TFFLinkedPlayback`):
- Отдельные demux / video / audio threads.
- Декодеры – компоненты на форме; кадры приходят в плеер через `IFFFrameSink`.

Синхронизация: `TFFPlaybackClock` – audio PTS как master clock, иначе wall clock (`uFFPlatformTime`).
