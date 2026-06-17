# Delphi-FFMPEG

Pascal bindings for **FFmpeg 8.1.x** shared libraries (Windows), synced with upstream tag **n8.1.1**.

## Requirements

- Delphi 10.3 Rio or newer (verified on **Delphi 13 Florence**)
- Free Pascal 3.0.4+ (optional)
- FFmpeg **8.1.x** shared DLLs for Windows (see below)

## Repository layout

| Path | Description |
|------|-------------|
| `source/` | Pascal units: `libavcodec`, `libavformat`, `libavutil`, … |
| `source/uFFmpegCodecUtils.pas` | Helpers (`CodecStringFromParameters`, encoder setup) |
| `source/uFFmpegPath.pas` | UTF-8 paths for FFmpeg C API |
| `examples/` | Official FFmpeg API samples; open `examples/Examples.groupproj` |
| `sdl/` | JEDI-SDL bindings (used by `ffmpeg_sample_player`) |
| `bin/win32/`, `bin/win64/` | Build output and FFmpeg DLLs per platform |
| `bin/scripts/` | DLL download scripts |
| `bin/out/` | Example run / verification artifacts |

Version constants live in `source/ffmpeg.inc` (target **n8.1.1**). When auditing structs against a local tree, check out that tag: `git -C C:\FFmpeg checkout n8.1.1`.

## Build examples

### Delphi IDE

1. Open `examples/Examples.groupproj` in Delphi.
2. Build All — search paths to `source/` (and `sdl/` for the player) are set in each `.dproj`.
3. FFmpeg DLLs for the active platform are in `bin/win32/` or `bin/win64/` (same folder as the built `.exe`).

### Command line

```powershell
cd examples
.\build_all.ps1              # Win64 (default)
.\build_all.ps1 -Platform Win32
```

Download runtime DLLs:

```powershell
cd bin/scripts
.\download_ffmpeg_dll.ps1         # Win64 — BtbN shared build → bin/win64/
.\download_ffmpeg_dll_win32.ps1   # Win32 — defisym shared build → bin/win32/
```

Win32: `bin/win32/` (defisym). Win64: `bin/win64/` (BtbN). Each folder holds both DLLs and built `.exe` files.

Or build/install FFmpeg **8.1.x** yourself and copy DLLs from the install `bin/` folder.

## Runtime DLL downloads

Place DLLs next to the built `.exe` (`bin/win32/` or `bin/win64/`). Scripts in `bin/scripts/` fetch FFmpeg automatically; links below are for manual download.

### FFmpeg 8.1.x (shared)

| Platform | Archive | Project |
|----------|---------|---------|
| **Win32** | [ffmpeg-n8.1-latest-win32-gpl-shared-8.1.zip](https://github.com/defisym/FFmpeg-Builds-Win32/releases/download/latest/ffmpeg-n8.1-latest-win32-gpl-shared-8.1.zip) | [defisym/FFmpeg-Builds-Win32](https://github.com/defisym/FFmpeg-Builds-Win32) |
| **Win64** | [ffmpeg-n8.1-latest-win64-gpl-shared-8.1.zip](https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-n8.1-latest-win64-gpl-shared-8.1.zip) | [BtbN/FFmpeg-Builds](https://github.com/BtbN/FFmpeg-Builds) |

Copy `bin\*.dll` from the unpacked archive into `bin/win32/` or `bin/win64/`.

### SDL 1.2 (`ffmpeg_sample_player` only)

Bindings expect **`SDL.dll`** (SDL 1.2; see `sdl/sdl.pas`). Source and build files: [libsdl-org/SDL-1.2](https://github.com/libsdl-org/SDL-1.2).

| Platform | Download | Notes |
|----------|----------|-------|
| **Win32** | [SDL-1.2.15-win32.zip](https://www.libsdl.org/release/SDL-1.2.15-win32.zip) | Prebuilt `SDL.dll` |
| **Win32** (alt.) | [SDL-devel-1.2.15-VC.zip](https://www.libsdl.org/release/SDL-devel-1.2.15-VC.zip) | MSVC package; `SDL.dll` in `lib\` |
| **Win64** | [VisualC](https://github.com/libsdl-org/SDL-1.2/tree/main/VisualC) | No official Win64 runtime on libsdl.org; build `SDL.dll` from the VS solution |

Copy `SDL.dll` into `bin/win32/` or `bin/win64/` beside `ffmpeg_sample_player.exe`.

## FFmpeg DLLs (runtime)

Bindings expect these library names (from `source/ffmpeg.inc`):

- `avcodec-62.dll`, `avformat-62.dll`, `avutil-60.dll`
- `avdevice-62.dll`, `avfilter-11.dll`
- `swresample-6.dll`, `swscale-9.dll`

`postproc-58.dll` is optional legacy; it is not part of the FFmpeg 8.x source tree and is not required by the examples.

## Integration with Delphi-OpenCV

[Delphi-OpenCV](https://github.com/Laex/Delphi-OpenCV) uses this project via the embedded copy at `Delphi-OpenCV/Delphi-FFMPEG/` and the runtime package `rtpFFMPEG`. Keep both copies of `source/` in sync when updating bindings.

Install order in OpenCV: `rtpFFMPEG` → `rclVCLOpenCV` → …

Related: [Delphi-OpenCV5](https://github.com/Laex/Delphi-OpenCV5) — OpenCV 5.0 Delphi wrapper (Win64).

## Notes

- Bindings follow the public FFmpeg API only (no `avpriv_*` imports); inline helpers (`av_mallocz_array`, etc.) are implemented in Pascal.
- Many legacy API symbols are deprecated in FFmpeg 8.x — compiler warnings W1000 are expected.
- `ffmpeg_sample_player` requires `SDL.dll` (SDL 1.2) in addition to FFmpeg DLLs — see [Runtime DLL downloads](#runtime-dll-downloads).
- Examples use `codecpar` and `avcodec_send_packet` / `avcodec_receive_frame`.
