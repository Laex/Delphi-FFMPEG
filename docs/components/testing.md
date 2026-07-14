## Component testing

### Quick run

```powershell
tools\setup_dev_environment.ps1 -RunTests
# or
tests\run_components.ps1 -Compiler Delphi -IncludeGuiTests
```

`-IncludeGuiTests` is required for VCL smoke tests (`player_smoke_test`, `linked_player_smoke_test`).

Linux (FPC):

```bash
tests/linux/run_components.sh
```

CI: `.github/workflows/component-tests.yml`.

### Test media

| Script | Output | Purpose |
|--------|--------|---------|
| `tools/generate_test_av.ps1` | `resource/test_av.mp4` | A/V playback (6 s, H.264 + AAC) |
| `tools/generate_test_subs.ps1` | `test_subs.mp4`, `.srt` | mov_text |
| `tools/generate_test_subs_ass.ps1` | `test_subs_ass.mkv`, `.ass` | ASS |
| `tools/generate_test_subs_bitmap.ps1` | `test_subs_bitmap.mkv` | dvdsub (optional) |

`run_components.ps1` copies media to `bin/win64/test_media/`.

### Test map

| Test | Verifies |
|------|----------|
| `wrapper_test` | `TFFPacket`, `TFFFrame` |
| `reader_decoder_test` | `TFFReader` + `TFFDecoder` |
| `scaler_test` | `TFFFrameConverter` + VCL bitmap |
| `player_smoke_test` | Engine mode, VCL player |
| `linked_player_smoke_test` | Linked mode + audio |
| `audio_resampler_test` | `TFFAudioResampler` (needs audio track) |
| `encoder_smoke_test` | Synthetic encode (no media) |
| `encoder_transcode_test` | Transcode pipeline |
| `writer_remux_test` | Remux to MKV |
| `graph_link_test` | Component graph |
| `hook_smoke_test` | `OnFrameHook` / hooks |
| `memory_access_test` | `TStream` I/O |
| `bitmap_encoder_test` | `TFFBitmapEncoder` |
| `mediainfo_test` | `TFFMediaInfo` |
| `thumbnail_test` | `TFFThumbnailExtractor` |
| `frame_filter_test` | `TFFFrameFilter` |
| `remux_job_test` | `TFFRemuxJob` |
| `transcode_clip_test` | Clip transcode |
| `hw_decode_test` | `TFFDecoder.HardwareDevice` (manual / CI skip) |
| `subtitle_decoder_test` | `TFFSubtitleDecoder` |
| `subtitle_ass_test` | ASS parse |
| `subtitle_bitmap_test` | Bitmap blit (synthetic + optional media) |
| `player_control_smoke_test` | `TFFPlayerControl` |
| `fmx_player_smoke_test` | FMX (compile separately) |

Exit codes: `0` = PASS, `1` = FAIL, `2` = SKIP.

### Logs

`bin/out/component_tests/*.log` – compile log.

`bin/out/component_tests/*.run.log` – test stdout.

### Platforms

| Target | Component tests |
|--------|-----------------|
| Delphi Win64 | Full set (VCL with `-IncludeGuiTests`) |
| FPC Win64 | Console tests |
| FPC Linux64 | Console (no VCL player) |

All binding targets: `tests/run_all_platforms.ps1`.
