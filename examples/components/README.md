# Delphi-FFMPEG component demos

VCL demo applications using `rtpFFMPEGComponents` (**alpha**).

## demo_player

Simple media player with:
- `TFFVideoPlayer` playback
- `TFFMediaInfo` metadata panel
- `TFFThumbnailExtractor` preview thumbnail

Build from Delphi IDE (Win64) or:

```powershell
dcc64 -U"..\..\source;..\..\source\components" demo_player\demo_player.dpr
```

## demo_transcode

Transcode UI with:
- `TFFMediaInfo` source inspection
- `TFFEncoder` pipeline (`Reader` + `Decoder` + `Writer`)
- Progress bar and live preview via `OnPreviewFrame`

```powershell
dcc64 -U"..\..\source;..\..\source\components" demo_transcode\demo_transcode.dpr
```

Preset combo uses `TFFTranscodeJob` (`MPEG-4`, `H.264`, `WebM VP9`).

## multidemo

All-in-one showcase with tabbed UI:

| Tab | Components |
|-----|------------|
| **Player** | `TFFPlayerControl`, `TFFSubtitleDecoder` |
| **Transcode / Remux** | `TFFTranscodeJob`, `TFFRemuxJob`, `TFFEncoder`, `TFFFrameFilter` |
| **Media probe** | `TFFMediaInfo`, `TFFThumbnailExtractor`, subtitles |
| **FFmpeg log** | `TFFLogger`, `TFFLoader` |

Open one media file from the toolbar; all tabs share the same source path.

```powershell
dcc64 -U"..\..\source;..\..\source\components" multidemo\multidemo.dpr
```

Build all demos:

```powershell
.\build_demos.ps1
```

Requires FFmpeg DLLs on `PATH` (see project README).
