# Delphi-FFMPEG

Pascal bindings for **FFmpeg 4.2.2** shared libraries (Windows).

## Requirements

- Delphi 10.3 Rio or newer (verified on **Delphi 13 Florence**)
- Free Pascal 3.0.4+ (optional)
- FFmpeg **4.2.2** shared DLLs for Windows (see below)

## Repository layout

| Path | Description |
|------|-------------|
| `source/` | Pascal units: `libavcodec`, `libavformat`, `libavutil`, … |
| `examples/` | Official FFmpeg API samples; open `examples/Examples.groupproj` |
| `sdl/` | JEDI-SDL bindings (used by `ffmpeg_sample_player`) |
| `buildall/` | Smoke-test (`BuildAll`) and FFmpeg build scripts |

## Build examples

1. Open `examples/Examples.groupproj` in Delphi.
2. Build All — search paths to `source/` (and `sdl/` for the player) are set in each `.dproj`.
3. Place FFmpeg DLLs next to the built `.exe` or add their folder to `PATH`.

## FFmpeg 4.2.2 DLLs (runtime)

Bindings expect these library names (from `source/ffmpeg.inc`):

- `avcodec-58.dll`, `avformat-58.dll`, `avutil-56.dll`
- `avdevice-58.dll`, `avfilter-7.dll`, `postproc-55.dll`
- `swresample-3.dll`, `swscale-5.dll`

Download a **FFmpeg 4.2.2 shared** build for Windows (e.g. from [BtbN/FFmpeg-Builds releases](https://github.com/BtbN/FFmpeg-Builds/releases) — pick a 4.2.x shared build, or build from source in `buildall/`).

Copy the DLLs into your output folder (`examples/Win32/Debug/` etc.) or into a directory on `PATH`.

## Integration with Delphi-OpenCV

[Delphi-OpenCV](https://github.com/Laex/Delphi-OpenCV) uses this project via the embedded copy at `Delphi-OpenCV/Delphi-FFMPEG/` and the runtime package `rtpFFMPEG`. Keep both copies of `source/` in sync when updating bindings.

Install order in OpenCV: `rtpFFMPEG` → `rclVCLOpenCV` → …

OpenCV samples using FFmpeg: `samples/Components/cFFmpegIPCamSource`, `samples/MultiDemo/IPCamVideoCapture`.

## Notes

- Many API symbols are marked deprecated in FFmpeg 4.2 — compiler warnings W1000 are expected.
- `ffmpeg_sample_player` requires `SDL.dll` (SDL 1.2) in addition to FFmpeg DLLs.
