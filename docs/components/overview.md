## Overview

The component library hides FFmpeg C API complexity and provides a familiar Delphi style: `TComponent`, properties, methods, events, and design-time editors.

> **Status: alpha** — see [Components README](README.md).

### Platforms

| Area | Delphi Win32/Win64 | FPC Win64 | Linux64 (FPC / Delphi) |
|------|-------------------|-----------|-------------------------|
| Bindings `source/` | ✓ | ✓ | ✓ |
| Non-visual components | ✓ | ✓ | ✓ |
| `TFFVideoPlayer` (VCL) | ✓ | – | – |
| `TFFFMXVideoPlayer` (FMX) | ✓ | – | ✓ (PAServer) |
| Audio output | WaveOut | – | ALSA → SDL2 fallback |

FFmpeg **8.1.x** shared libraries must be next to the executable (`bin/win64/`, `bin/linux64/`).

### Installation

1. Build `rtpFFMPEG` (bindings).
2. Build `rtpFFMPEGComponents` (runtime components).
3. Install `dclFFMPEGComponents` in the IDE (**FFmpeg** palette).

Copy FFmpeg DLL/SO into the `.exe` directory or use `TFFLoader` / `tools/setup_dev_environment.ps1`.

### IDE palette components

| Component | Purpose |
|-----------|---------|
| `TFFLoader` | Load FFmpeg DLLs |
| `TFFLogger` | Capture `av_log` |
| `TFFReader` | Demux, `AutoPump`, custom I/O |
| `TFFDecoder` | Decode, preview, graph sink/source |
| `TFFEncoder` | Encode, transcode pipeline |
| `TFFWriter` | Mux / remux |
| `TFFMemoryAccessAdapter` | `AVIOContext` over `TStream` |
| `TFFBitmapEncoder` | BGRA / `TBitmap` → video file |
| `TFFMediaInfo` | Metadata probe |
| `TFFThumbnailExtractor` | Frame preview |
| `TFFTranscodeJob` | Preset-based transcode |
| `TFFRemuxJob` | Stream-copy remux |
| `TFFFrameFilter` | libavfilter node (scale, fps, …) |
| `TFFVideoPlayer` | VCL player |
| `TFFPlayerControl` | Player + transport bar |
| `TFFFMXVideoPlayer` | FMX player |
| `TFFSubtitleDecoder` | Subtitle decode for overlay |

Helper modules (not on palette): `uFFPlaybackEngine`, `uFFLinkedPlayback`, `uFFSubtitleOverlay`, `uFFSubtitleBitmap`, `uFFSubtitleAss`, `uFFHardwareDecode`.

### Player modes

| Mode | Condition | Core |
|------|-----------|------|
| **Engine** | `FileName` set, `VideoDecoder` = nil | `TFFPlaybackEngine` |
| **Linked** | `VideoDecoder` assigned | `TFFLinkedPlayback` + graph |

See [Playback](playback.md).
