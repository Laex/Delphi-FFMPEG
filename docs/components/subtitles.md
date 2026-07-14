## Subtitles

### Overview

```
TFFReader ──► TFFSubtitleDecoder (LoadAll / TakePacket)
                      │
                      ▼
              TFFVideoPlayer.SubtitleDecoder
                      │
                      ▼
              uFFSubtitleOverlay → BGRA in Paint
```

`TFFSubtitleDecoder` indexes timed events. `TFFVideoPlayer` overlays the active event (`GetEventAt(Position)`) on each video frame.

### Wiring

```delphi
Reader.FileName := 'movie.mkv';
Reader.Open;

SubDec.Reader := Reader;
SubDec.StreamIndex := SubStreamIdx;  // AVMEDIA_TYPE_SUBTITLE
SubDec.LoadAll;  // or sidecar .srt when embedded subs are missing

Player.SubtitleDecoder := SubDec;
Player.VideoDecoder := VideoDecoder;  // linked mode
Player.Play;
```

`LoadAll` also tries `ChangeFileExt(FileName, '.srt')` as a fallback.

### Supported formats

| Type | Source | Rendering |
|------|--------|-----------|
| Text / mov_text | `AVSubtitleRect.text` | GDI `DrawText`, bottom of frame |
| ASS / SSA | `Rect^.ass`, tags in text | `uFFSubtitleAss` – `{\anN}`, `{\pos}`, tag strip; GDI |
| Bitmap / PGS / DVDsub | `SUBTITLE_BITMAP` | `uFFSubtitleBitmap` – palette blit with alpha |

#### Bitmap / PGS

The decoder deep-copies palette and index plane into owned buffers (`TFFSubtitleBitmap`) – FFmpeg pointers are not kept after `avsubtitle_free`.

`AVSubtitleRect.data` binding in `libavcodec.pas`:

```pascal
data: array [0 .. 3] of puint8_t;  // [0] bitmap, [1] palette
linesize: Tint_array_4;
```

Blit: `FFSubtitleBlendBitmapOnBgra` – index 0 = transparent, RGBA from palette, alpha-blend into BGRA.

#### ASS

`uFFSubtitleAss` extracts dialogue text and basic layout. Full rendering (fonts, styles, karaoke) is planned via **libass**; current implementation uses a GDI fallback.

### Modules

| Unit | Role |
|------|------|
| `uFFSubtitleDecoder.pas` | Decode, `TFFSubtitleEvent`, `LoadAll` |
| `uFFSubtitleBitmap.pas` | Deep-copy + blit |
| `uFFSubtitleAss.pas` | ASS dialogue parsing |
| `uFFSubtitleOverlay.pas` | `FFSubtitleBlendEventOnBgra` |

### Test media

```powershell
tools\generate_test_subs.ps1          # test_subs.mp4 + .srt (mov_text)
tools\generate_test_subs_ass.ps1      # test_subs_ass.mkv + test_subs.ass
tools\generate_test_subs_bitmap.ps1   # test_subs_bitmap.mkv (dvdsub)
```

Tests: `subtitle_decoder_test`, `subtitle_ass_test`, `subtitle_bitmap_test`.
