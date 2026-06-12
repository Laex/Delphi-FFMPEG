# Аудит Delphi-FFMPEG vs FFmpeg n8.1.1

Дата: 2026-06-12  
Целевая версия: **FFmpeg 8.1.1** (тег `n8.1.1`, `LIBAVCODEC_VERSION_MAJOR = 62`)  
Источник C: `C:\FFmpeg` на теге `n8.1.1` (не `master` / `n8.2-dev`)

## Инструмент

```powershell
git -C C:\FFmpeg checkout n8.1.1
python tools\audit_structs.py
```

Сравнивает порядок полей записей Pascal с `typedef struct` в заголовках FFmpeg.  
Перед запуском убедитесь, что `C:\FFmpeg` на теге **n8.1.1**, а не на `n8.2-dev`.

## Критическая проблема

**Прямой доступ к полям `AVCodecContext` через Delphi-запись не совпадал с layout DLL FFmpeg 8.x.**

Симптомы в MediaMCPServer:
- `video_scale` / libx264: `Picture size 720x0`, `Invalid video pixel format: -1`
- `video_probe`: неверные `width`/`height` из `codecpar` (обход через `avcodec_string`)

### Корневые причины

| Область | FFmpeg 8 C | Старый Pascal |
|--------|------------|---------------|
| После `bit_rate` | сразу `flags` | лишние `bit_rate_tolerance`, `global_quality`, `compression_level` (+12 байт) |
| После `time_base` | `pkt_timebase`, `framerate`, `delay` | `ticks_per_frame`, `delay` (удалено в FFmpeg 8) |
| После `coded_height` | `sample_aspect_ratio`, `pix_fmt`, `sw_pix_fmt`, цвет, `refs`, `has_b_frames`, `slice_flags`, callbacks… | `gop_size`, `pix_fmt`, callbacks без промежуточных полей |
| `gop_size` | после `keyint_min` | стоял сразу после `coded_height` |
| `nsse_weight` | после `dark_masking` | был в конце структуры |
| `FF_API_PRIVATE_OPT` | удалены из C | устаревшие `b_frame_strategy`, `mpeg_quant` и др. |

## Исправления (libavcodec.pas)

- Удалены `bit_rate_tolerance`, `global_quality`, `compression_level`
- `ticks_per_frame` заменён на `pkt_timebase` + `framerate`
- Перестроен блок `coded_width` … `slices` по `C:\FFmpeg\libavcodec\avcodec.h`
- Убраны дубликаты `nsse_weight`, `sw_pix_fmt`, `chroma_intra_matrix`
- В `ffmpeg.inc` добавлены `FF_API_*` для libavcodec 62

## Рекомендации для прикладного кода

1. **Настройка энкодера** — `uFFmpegCodecUtils.ConfigureVideoEncoder` + при необходимости `av_opt_set_*` / опции в `avcodec_open2`.
2. **Размер видео из probe** — `VideoSizeFromParameters` (не слепо `codecpar.width`).
3. **Пути** — `FFmpegUtf8Path` / `uFFmpegPath.pas` для `avformat_open_input`.
4. **Не использовать** `sizeof(AVCodecContext)` и прямую запись в поля без сверки с заголовками.

## Остаётся проверить

| Структура | Статус |
|-----------|--------|
| `AVCodecContext` (хвост записи) | частично синхронизирован; хвост (после `initial_padding`) ещё расходится |
| `AVFormatContext` | смещение с ~поля `max_analyze_duration` / `probesize` |
| `AVStream` | лишнее/смещённое `r_frame_rate` |
| `AVCodecParameters` | заголовок: `libavcodec/codec_par.h` |
| `AVPacket` | заголовок: `libavcodec/packet.h` |

## Сборка и проверка

```powershell
cd examples
.\build_all.ps1
.\encode_video.exe out.h264 libx264
```

После правок пересобрать MediaMCPServer и прогнать `scripts\tests\test_lecture_scenario.ps1`.
