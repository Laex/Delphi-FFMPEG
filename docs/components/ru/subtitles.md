## Субтитры

### Обзор

```
TFFReader ──► TFFSubtitleDecoder (LoadAll / TakePacket)
                      │
                      ▼
              TFFVideoPlayer.SubtitleDecoder
                      │
                      ▼
              uFFSubtitleOverlay → BGRA в Paint
```

`TFFSubtitleDecoder` индексирует события с таймкодами. `TFFVideoPlayer` накладывает активное событие (`GetEventAt(Position)`) на видеокадр.

### Подключение

```delphi
Reader.FileName := 'movie.mkv';
Reader.Open;

SubDec.Reader := Reader;
SubDec.StreamIndex := SubStreamIdx;  // AVMEDIA_TYPE_SUBTITLE
SubDec.LoadAll;  // или sidecar .srt при отсутствии embedded subs

Player.SubtitleDecoder := SubDec;
Player.VideoDecoder := VideoDecoder;  // linked mode
Player.Play;
```

`LoadAll` также ищет `ChangeFileExt(FileName, '.srt')` как fallback.

### Поддерживаемые форматы

| Тип | Источник | Отрисовка |
|-----|----------|-----------|
| Text / mov_text | `AVSubtitleRect.text` | GDI `DrawText`, низ экрана |
| ASS / SSA | `Rect^.ass`, теги в text | `uFFSubtitleAss` – `{\anN}`, `{\pos}`, strip тегов; GDI |
| Bitmap / PGS / DVDsub | `SUBTITLE_BITMAP` | `uFFSubtitleBitmap` – palette blit с alpha |

#### Bitmap / PGS

Декодер копирует палитру и индексную плоскость в owned-буферы (`TFFSubtitleBitmap`) – указатели FFmpeg не сохраняются после `avsubtitle_free`.

Binding `AVSubtitleRect.data` в `libavcodec.pas`:

```pascal
data: array [0 .. 3] of puint8_t;  // [0] bitmap, [1] palette
linesize: Tint_array_4;
```

Blit: `FFSubtitleBlendBitmapOnBgra` – index 0 = прозрачный, RGBA из палитры, alpha-blend в BGRA.

#### ASS

`uFFSubtitleAss` извлекает текст dialogue-строки и базовую геометрию. Полноценный рендеринг (шрифты, стили, karaoke) планируется через **libass**; сейчас – GDI fallback.

### Модули

| Модуль | Роль |
|--------|------|
| `uFFSubtitleDecoder.pas` | Decode, `TFFSubtitleEvent`, `LoadAll` |
| `uFFSubtitleBitmap.pas` | Deep-copy + blit |
| `uFFSubtitleAss.pas` | Парсинг ASS dialogue |
| `uFFSubtitleOverlay.pas` | `FFSubtitleBlendEventOnBgra` |

### Тестовые медиа

```powershell
tools\generate_test_subs.ps1          # test_subs.mp4 + .srt (mov_text)
tools\generate_test_subs_ass.ps1      # test_subs_ass.mkv + test_subs.ass
tools\generate_test_subs_bitmap.ps1   # test_subs_bitmap.mkv (dvdsub)
```

Тесты: `subtitle_decoder_test`, `subtitle_ass_test`, `subtitle_bitmap_test`.
