## Обзор

Библиотека компонентов скрывает сложность C API FFmpeg и предоставляет привычный для Delphi стиль: `TComponent`, свойства, методы, события, design-time редакторы.

> **Статус: альфа** — см. [README компонентов](README.md).

### Платформы

| Область | Delphi Win32/Win64 | FPC Win64 | Linux64 (FPC / Delphi) |
|---------|-------------------|-----------|------------------------|
| Биндинги `source/` | ✓ | ✓ | ✓ |
| Невизуальные компоненты | ✓ | ✓ | ✓ |
| `TFFVideoPlayer` (VCL) | ✓ | – | – |
| `TFFFMXVideoPlayer` (FMX) | ✓ | – | ✓ (PAServer) |
| Аудиовыход | WaveOut | – | ALSA → SDL2 fallback |

FFmpeg **8.1.x** shared libraries должны лежать рядом с исполняемым файлом (`bin/win64/`, `bin/linux64/`).

### Установка

1. Соберите `rtpFFMPEG` (биндинги).
2. Соберите `rtpFFMPEGComponents` (runtime-компоненты).
3. Установите `dclFFMPEGComponents` в IDE (палитра **FFmpeg**).

Скопируйте FFmpeg DLL/SO в каталог с `.exe` или используйте `TFFLoader` / `tools/setup_dev_environment.ps1`.

### Компоненты на палитре

| Компонент | Назначение |
|-----------|------------|
| `TFFLoader` | Загрузка DLL FFmpeg |
| `TFFLogger` | Перехват `av_log` |
| `TFFReader` | Demux, `AutoPump`, custom I/O |
| `TFFDecoder` | Decode, preview, graph sink/source |
| `TFFEncoder` | Encode, transcode pipeline |
| `TFFWriter` | Mux / remux |
| `TFFMemoryAccessAdapter` | `AVIOContext` поверх `TStream` |
| `TFFBitmapEncoder` | BGRA / `TBitmap` → видеофайл |
| `TFFMediaInfo` | Probe метаданных |
| `TFFThumbnailExtractor` | Кадр-превью |
| `TFFTranscodeJob` | Preset-based transcode |
| `TFFRemuxJob` | Stream-copy remux |
| `TFFFrameFilter` | Узел libavfilter (scale, fps, …) |
| `TFFVideoPlayer` | VCL-плеер |
| `TFFPlayerControl` | Плеер + transport bar |
| `TFFFMXVideoPlayer` | FMX-плеер |
| `TFFSubtitleDecoder` | Декодирование субтитров для overlay |

Вспомогательные модули (не на палитре): `uFFPlaybackEngine`, `uFFLinkedPlayback`, `uFFSubtitleOverlay`, `uFFSubtitleBitmap`, `uFFSubtitleAss`, `uFFHardwareDecode`.

### Режимы работы плеера

| Режим | Условие | Ядро |
|-------|---------|------|
| **Engine** | `FileName` задан, `VideoDecoder` = nil | `TFFPlaybackEngine` |
| **Linked** | `VideoDecoder` назначен | `TFFLinkedPlayback` + graph |

См. [Воспроизведение](playback.md).
