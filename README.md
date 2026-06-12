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
| `bin/` | Build output, DLL download script |

Version constants live in `source/ffmpeg.inc` (target **n8.1.1**). When auditing structs against a local tree, check out that tag: `git -C C:\FFmpeg checkout n8.1.1`.

## Build examples

### Delphi IDE

1. Open `examples/Examples.groupproj` in Delphi.
2. Build All — search paths to `source/` (and `sdl/` for the player) are set in each `.dproj`.
3. Place FFmpeg DLLs next to the built `.exe` or add their folder to `PATH`.

### Command line (Win64)

```powershell
cd examples
.\build_all.ps1
```

Download runtime DLLs (BtbN shared build):

```powershell
cd bin
.\download_ffmpeg_dll.ps1
```

Or build/install FFmpeg **8.1.x** yourself and copy DLLs from the install `bin/` folder.

## FFmpeg DLLs (runtime)

Bindings expect these library names (from `source/ffmpeg.inc`):

- `avcodec-62.dll`, `avformat-62.dll`, `avutil-60.dll`
- `avdevice-62.dll`, `avfilter-11.dll`
- `swresample-6.dll`, `swscale-9.dll`

`postproc-58.dll` is optional legacy; it is not part of the FFmpeg 8.x source tree and is not required by the examples.

## Integration with Delphi-OpenCV

[Delphi-OpenCV](https://github.com/Laex/Delphi-OpenCV) uses this project via the embedded copy at `Delphi-OpenCV/Delphi-FFMPEG/` and the runtime package `rtpFFMPEG`. Keep both copies of `source/` in sync when updating bindings.

Install order in OpenCV: `rtpFFMPEG` → `rclVCLOpenCV` → …

## Notes

- Bindings follow the public FFmpeg API only (no `avpriv_*` imports); inline helpers (`av_mallocz_array`, etc.) are implemented in Pascal.
- Many legacy API symbols are deprecated in FFmpeg 8.x — compiler warnings W1000 are expected.
- `ffmpeg_sample_player` requires `SDL.dll` (SDL 1.2) in addition to FFmpeg DLLs.
- Examples use `codecpar` and `avcodec_send_packet` / `avcodec_receive_frame`.
