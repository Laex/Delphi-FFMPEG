## Delphi-FFMPEG

Pascal bindings for **FFmpeg 8.1.x** shared libraries, synced with upstream tag **n8.1.1**.

**Primary platform:**

* Windows (Win32 / Win64).
* **`source/`** is unified for **Delphi** (Win32 / Win64 / Linux64) and **FPC** (Win32 / Win64 / Linux64) via `ffmpeg_platform.inc` and `ffmpeg_rtl.inc`. Linux bindings are verified in WSL;
* `examples/` and build scripts still target Windows.

### Requirements

#### Windows (examples and IDE)

- Delphi 10.3 Rio or newer (verified on **Delphi 13 Florence**)
- FFmpeg **8.1.x** shared DLLs (see [Runtime downloads](#runtime-downloads))

#### Linux (bindings only)

- [WSL2](https://learn.microsoft.com/windows/wsl/) or native Linux x86_64
- Free Pascal 3.0.4+ (used for the binding smoke test)
- FFmpeg **8.1.x** shared `.so` libraries

Optional: **Delphi Linux64** (`DCCLINUX64`, RAD Studio Enterprise/Architect) + [PAServer](https://docwiki.embarcadero.com/RADStudio/Athens/en/DCCLINUX64.EXE,_the_Delphi_Compiler_for_Linux) on Ubuntu/WSL – see [Delphi on Linux](#delphi-on-linux).

### Repository layout

| Path | Description |
|------|-------------|
| `docs/` | Documentation ([components](docs/components/README.md), bindings notes) |
| `source/` | Pascal units: `libavcodec`, `libavformat`, `libavutil`, … |
| `source/ffmpeg.inc` | Version constants and per-OS library file names |
| `source/ffmpeg_platform.inc` | Maps FPC `WINDOWS` → `MSWINDOWS`, Delphi `POSIX` → `LINUX` |
| `source/ffmpeg_rtl.inc` | `SysUtils` (FPC) vs `System.SysUtils` (Delphi) for helpers |
| `source/uFFmpegCodecUtils.pas` | Helpers (`CodecStringFromParameters`, encoder setup) |
| `source/uFFmpegPath.pas` | UTF-8 paths for FFmpeg C API |
| `source/components/` | High-level VCL/FMX components (**alpha**) – see [docs/components/](docs/components/README.md) |
| `examples/` | Official FFmpeg API samples; open `examples/Examples.groupproj` |
| `sdl/` | JEDI-SDL bindings (used by `ffmpeg_sample_player`) |
| `tests/linux/` | Linux binding smoke test (`binding_test.lpr`, `run_wsl_test.sh`) |
| `tests/windows/` | Optional FPC smoke test on Windows (`run_fpc_test.ps1`, needs `fpc` in PATH) |
| `bin/win32/`, `bin/win64/` | Windows build output and FFmpeg DLLs |
| `bin/linux64/` | Linux FFmpeg `.so` and FPC test binary (not committed) |
| `bin/scripts/` | FFmpeg download scripts (Windows) |
| `bin/out/` | Example run / verification artifacts |

Version constants live in `source/ffmpeg.inc` (target **n8.1.1**). When auditing structs against a local tree, check out that tag: `git -C <ffmpeg-source-dir> checkout n8.1.1`.

### Build examples (Windows)

#### Delphi IDE

1. Open `examples/Examples.groupproj` in Delphi.
2. Build All – search paths to `source/` (and `sdl/` for the player) are set in each `.dproj`.
3. FFmpeg DLLs for the active platform are in `bin/win32/` or `bin/win64/` (same folder as the built `.exe`).

#### Command line

```powershell
cd examples
.\build_all.ps1              # Win64 (default)
.\build_all.ps1 -Platform Win32
```

Download runtime DLLs:

```powershell
cd bin/scripts
.\download_ffmpeg_dll.ps1         # Win64 – BtbN → bin/win64/
.\download_ffmpeg_dll_win32.ps1   # Win32 – defisym → bin/win32/
```

Each `bin/win32/` or `bin/win64/` folder holds both FFmpeg DLLs and built `.exe` files.

Or build/install FFmpeg **8.1.x** yourself and copy libraries from the install `bin/` folder.

### Linux binding test (WSL)

Smoke test for `source/` only (not the full `examples/` tree). Downloads FFmpeg, compiles with **FPC**, runs `av_version_info()` and related calls.

```powershell
# from repository root (repo on a drive visible to WSL):
wsl -d <distro> bash tests/linux/run_wsl_test.sh
```

On native Linux:

```bash
./tests/linux/run_wsl_test.sh
```

Expected output ends with `PASS: bindings loaded and basic API calls succeeded.`

### Six-platform binding test (Delphi + FPC)

Unified smoke test for `tests/binding_test.dpr` on all six targets.

**Cross-platform toolchain (FPC Linux64 cross-compile, optional i386-win32 host) is not installed automatically** – set it up manually with [fpcupdeluxe releases](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases) (install FPC targets and cross compilers, then point the script at the install root). A standard Windows-only FPC install (e.g. `C:\FPC\3.2.2`) is enough for **Win32 + Win64** FPC tests only; **Linux64 FPC** from Windows still requires fpcupdeluxe `x86_64-linux` cross binutils, or run `tests/linux/run_wsl_test.sh` with native FPC in WSL.

```powershell
cd tests
.\run_all_platforms.ps1
# optional: FPC from fpcupdeluxe (after manual setup)
.\run_all_platforms.ps1 -FpcRoot '<path-to-fpcupdeluxe>'
# optional: standard FPC install (Win32/Win64 only)
.\run_all_platforms.ps1 -FpcRoot '<path-to-fpc>'
```

| Target | Compiler | How the script tests |
|--------|----------|----------------------|
| Win32 | Delphi | `dcc32` → run `bin/win32/binding_test_delphi32.exe` |
| Win64 | Delphi | `dcc64` → run `bin/win64/binding_test_delphi64.exe` |
| Linux64 | Delphi | `dcclinux64` → run ELF in WSL (needs **PAServer** Linux SDK) |
| Win32 | FPC | `fpc -Twin32 -Pi386` (needs **native i386-win32** FPC in fpcupdeluxe) |
| Win64 | FPC | `fpc -Twin64` from `<fpcupdeluxe>/fpc/bin/x86_64-win64/fpc.exe` |
| Linux64 | FPC | cross-compile with fpcupdeluxe Linux binutils → run in WSL |

Logs: `bin/out/platform_tests/`. **SKIP** means missing toolchain (PAServer SDK or FPC i386-win32), not a binding defect.

**FPC Win32 note:** an x86_64-win64 FPC host cannot cross-build `i386-win32` (FPC limitation). Add target **i386-win32** in fpcupdeluxe (Cross compilers tab).

**Delphi Linux64 note:** link step needs `<rad-studio>/PAServer/linux/bin` sysroot after installing PAServer on WSL.

### Runtime downloads

Place libraries next to the built binary (`bin/win32/`, `bin/win64/`, or `bin/linux64/`). Windows scripts live in `bin/scripts/`; links below are for manual download.

#### FFmpeg 8.1.x (shared)

| Platform | Archive | Project |
|----------|---------|---------|
| **Win32** | [ffmpeg-n8.1-latest-win32-gpl-shared-8.1.zip](https://github.com/defisym/FFmpeg-Builds-Win32/releases/download/latest/ffmpeg-n8.1-latest-win32-gpl-shared-8.1.zip) | [defisym/FFmpeg-Builds-Win32](https://github.com/defisym/FFmpeg-Builds-Win32) |
| **Win64** | [ffmpeg-n8.1-latest-win64-gpl-shared-8.1.zip](https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-n8.1-latest-win64-gpl-shared-8.1.zip) | [BtbN/FFmpeg-Builds](https://github.com/BtbN/FFmpeg-Builds) |
| **Linux x64** | [ffmpeg-n8.1-latest-linux64-gpl-shared-8.1.tar.xz](https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-n8.1-latest-linux64-gpl-shared-8.1.tar.xz) | [BtbN/FFmpeg-Builds](https://github.com/BtbN/FFmpeg-Builds) |

- Windows: copy `bin\*.dll` from the unpacked archive into `bin/win32/` or `bin/win64/`.
- Linux: copy `lib/*.so*` into `bin/linux64/` (or set `LD_LIBRARY_PATH`).

Or run the automated setup (Win64, downloads DLLs + `ffmpeg.exe`, generates test media):

```powershell
tools\setup_dev_environment.ps1 -RunTests -IncludeGuiTests
```

Generates `resource/test_av.mp4`, `test_subs.mp4`, `test_subs_ass.mkv`, and copies media to `bin/win64/test_media/` for component tests.

Component tests only (after setup):

```powershell
tests\run_components.ps1 -Compiler Delphi -IncludeGuiTests
```

GUI smoke tests (`player_smoke_test`, `linked_player_smoke_test`) need `-IncludeGuiTests` and `test_av.mp4` in `bin/win64/test_media/`.

CI: GitHub Actions workflow `.github/workflows/component-tests.yml` (FPC on Linux + optional Delphi on Windows).

#### SDL 1.2 (`ffmpeg_sample_player` only, Windows)

Bindings expect **`SDL.dll`** (SDL 1.2; see `sdl/sdl.pas`). Source: [libsdl-org/SDL-1.2](https://github.com/libsdl-org/SDL-1.2).

| Platform | Download | Notes |
|----------|----------|-------|
| **Win32** | [SDL-1.2.15-win32.zip](https://www.libsdl.org/release/SDL-1.2.15-win32.zip) | Prebuilt `SDL.dll` |
| **Win32** (alt.) | [SDL-devel-1.2.15-VC.zip](https://www.libsdl.org/release/SDL-devel-1.2.15-VC.zip) | MSVC package; `SDL.dll` in `lib\` |
| **Win64** | [VisualC](https://github.com/libsdl-org/SDL-1.2/tree/main/VisualC) | No official Win64 runtime on libsdl.org; build `SDL.dll` from the VS solution |

Copy `SDL.dll` into `bin/win32/` or `bin/win64/` beside `ffmpeg_sample_player.exe`.

### Runtime library names

From `source/ffmpeg.inc`:

| Windows (DLL) | Linux (shared object) |
|---------------|----------------------|
| `avcodec-62.dll` | `libavcodec.so.62` |
| `avformat-62.dll` | `libavformat.so.62` |
| `avutil-60.dll` | `libavutil.so.60` |
| `avdevice-62.dll` | `libavdevice.so.62` |
| `avfilter-11.dll` | `libavfilter.so.11` |
| `swresample-6.dll` | `libswresample.so.6` |
| `swscale-9.dll` | `libswscale.so.9` |

`postproc-58` is optional legacy; it is not part of the FFmpeg 8.x source tree and is not required by the examples.

### Platform support

| Platform | Compiler | Bindings (`source/`) | Examples (`examples/`) | Build / test |
|----------|----------|----------------------|------------------------|--------------|
| **Windows Win32** | Delphi | Yes | Yes | `build_all.ps1`, Delphi IDE |
| **Windows Win64** | Delphi | Yes | Yes | `build_all.ps1`, Delphi IDE |
| **Windows Win32** | FPC | Yes | No | `run_all_platforms.ps1` / `tests/windows/run_fpc_test.ps1` |
| **Windows Win64** | FPC | Yes | No | `run_all_platforms.ps1` / `tests/windows/run_fpc_test.ps1` |
| **Linux x64** | Delphi | Yes | Not yet | `run_all_platforms.ps1` (`DCCLINUX64` + PAServer SDK) |
| **Linux x64** | FPC | Yes (tested) | Not yet | `run_all_platforms.ps1` / `tests/linux/run_wsl_test.sh` |
| **Android** | – | Hooks in `ffmpeg.inc` only | No | – |

Examples still use `Winapi.Windows` (mostly unused) and VCL in `dumpframe`; porting to Linux is a separate task.

### Delphi on Linux

[Embarcadero RAD Studio](https://docwiki.embarcadero.com/RADStudio/en/Supported_Target_Platforms) supports **Linux Intel 64-bit** via `DCCLINUX64` (Enterprise/Architect): compile on Windows, deploy through **PAServer** on Ubuntu or WSL2.

`source/` is suitable for a Linux64 console project (no `Winapi` in the bindings). Steps:
1. Install PAServer on Ubuntu/WSL.
2. Add **Linux64** target in a Delphi project; search path → `source/`.
3. Place FFmpeg 8.1 `.so` on the Linux side (`bin/linux64/` or system path).
4. Do not use `Winapi.Windows` / VCL units.

The WSL test above uses **FPC** instead of `DCCLINUX64` – it validates the same Pascal units and `.so` loading.

### Integration with Delphi-OpenCV

[Delphi-OpenCV](https://github.com/Laex/Delphi-OpenCV) uses this project via the embedded copy at `Delphi-OpenCV/Delphi-FFMPEG/` and the runtime package `rtpFFMPEG`. Keep both copies of `source/` in sync when updating bindings.

Install order in OpenCV: `rtpFFMPEG` → `rclVCLOpenCV` → …

Related:
* [Delphi-OpenCV5](https://github.com/Laex/Delphi-OpenCV5) – OpenCV 5.0 Delphi wrapper (Win64).
* [MediaMCPServer](https://github.com/Laex/MediaMCPServer) – MCP server for media tools (uses Delphi-FFMPEG bindings).

### Notes

- Bindings follow the public FFmpeg API only (no `avpriv_*` imports); inline helpers (`av_mallocz_array`, etc.) are implemented in Pascal.
- Many legacy API symbols are deprecated in FFmpeg 8.x – compiler warnings W1000 are expected.
- `ffmpeg_sample_player` requires `SDL.dll` (SDL 1.2) in addition to FFmpeg DLLs – see [SDL downloads](#sdl-12-ffmpeg_sample_player-only-windows).
- Examples use `codecpar` and `avcodec_send_packet` / `avcodec_receive_frame`.
- Struct layout was audited against FFmpeg **n8.1.1** on Windows; re-verify on Linux if you use low-level record field access.
- High-level components (**alpha**): see [docs/components/](docs/components/README.md).
