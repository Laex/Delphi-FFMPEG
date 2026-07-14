## Тестирование компонентов

### Быстрый прогон

```powershell
tools\setup_dev_environment.ps1 -RunTests
# или
tests\run_components.ps1 -Compiler Delphi -IncludeGuiTests
```

`-IncludeGuiTests` нужен для VCL smoke-тестов (`player_smoke_test`, `linked_player_smoke_test`).

Linux (FPC):

```bash
tests/linux/run_components.sh
```

CI: `.github/workflows/component-tests.yml`.

### Тестовые медиа

| Скрипт | Результат | Назначение |
|--------|-----------|------------|
| `tools/generate_test_av.ps1` | `resource/test_av.mp4` | A/V playback (6 s, H.264 + AAC) |
| `tools/generate_test_subs.ps1` | `test_subs.mp4`, `.srt` | mov_text |
| `tools/generate_test_subs_ass.ps1` | `test_subs_ass.mkv`, `.ass` | ASS |
| `tools/generate_test_subs_bitmap.ps1` | `test_subs_bitmap.mkv` | dvdsub (опционально) |

`run_components.ps1` копирует медиа в `bin/win64/test_media/`.

### Карта тестов

| Тест | Что проверяет |
|------|---------------|
| `wrapper_test` | `TFFPacket`, `TFFFrame` |
| `reader_decoder_test` | `TFFReader` + `TFFDecoder` |
| `scaler_test` | `TFFFrameConverter` + VCL bitmap |
| `player_smoke_test` | Engine mode, VCL player |
| `linked_player_smoke_test` | Linked mode + audio |
| `audio_resampler_test` | `TFFAudioResampler` (нужен файл с audio) |
| `encoder_smoke_test` | Синтетический encode (без медиа) |
| `encoder_transcode_test` | Transcode pipeline |
| `writer_remux_test` | Remux в MKV |
| `graph_link_test` | Component graph |
| `hook_smoke_test` | `OnFrameHook` / hooks |
| `memory_access_test` | `TStream` I/O |
| `bitmap_encoder_test` | `TFFBitmapEncoder` |
| `mediainfo_test` | `TFFMediaInfo` |
| `thumbnail_test` | `TFFThumbnailExtractor` |
| `frame_filter_test` | `TFFFrameFilter` |
| `remux_job_test` | `TFFRemuxJob` |
| `transcode_clip_test` | Clip transcode |
| `hw_decode_test` | `TFFDecoder.HardwareDevice` (manual / skip в CI) |
| `subtitle_decoder_test` | `TFFSubtitleDecoder` |
| `subtitle_ass_test` | ASS parse |
| `subtitle_bitmap_test` | Bitmap blit (synthetic + optional media) |
| `player_control_smoke_test` | `TFFPlayerControl` |
| `fmx_player_smoke_test` | FMX (compile separately) |

Коды выхода: `0` = PASS, `1` = FAIL, `2` = SKIP.

### Логи

`bin/out/component_tests/*.log` – compile log.

`bin/out/component_tests/*.run.log` – stdout теста.

### Платформы

| Target | Компонентные тесты |
|--------|-------------------|
| Delphi Win64 | Полный набор (VCL с `-IncludeGuiTests`) |
| FPC Win64 | Консольные тесты |
| FPC Linux64 | Консольные (без VCL player) |

Биндинги всех платформ: `tests/run_all_platforms.ps1`.
