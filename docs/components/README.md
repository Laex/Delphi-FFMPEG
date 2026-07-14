## Delphi-FFMPEG Components

High-level VCL/FMX components on top of Pascal bindings for FFmpeg 8.1.x.

> **Alpha** — the component library is in early alpha. APIs, design-time behavior, and stability may change; not recommended for production without thorough testing.

[Русская версия](ru/README.md)

### Contents

| Document | Description |
|----------|-------------|
| [Overview](overview.md) | Purpose, platforms, package installation |
| [Architecture](architecture.md) | Library layers, graph linking |
| [Playback](playback.md) | Engine / linked mode, A/V sync, HW decode |
| [Subtitles](subtitles.md) | Text, ASS, bitmap/PGS overlay |
| [Component reference](reference.md) | Properties and methods by class |
| [Testing](testing.md) | Smoke tests, media, CI |

### Quick start

```delphi
uses uFFVideoPlayer;

Player.Parent := Self;
Player.Align := alClient;
Player.FileName := 'video.mp4';
Player.HardwareDevice := ffhdAuto;  // optional
Player.Play;
```

Packages: `packages/Delphi 13 Florence/DelphiFFMPEG.groupproj` (`rtpFFMPEG` + `rtpFFMPEGComponents` + `dclFFMPEGComponents`).

See also: [Overview](overview.md).
