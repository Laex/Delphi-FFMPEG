## Воспроизведение

### TFFVideoPlayer

VCL-контрол с двумя режимами.

#### Engine mode

Минимальная настройка – только `FileName`:

```delphi
Player.FileName := 'clip.mp4';
Player.HardwareDevice := ffhdAuto;  // опционально, только до Play
Player.Play;
```

Внутри создаётся `TFFPlaybackEngine` с собственными `TFFReader`/`TFFDecoder` в worker-потоках.

#### Linked mode

Граф компонентов на форме:

```delphi
Reader.FileName := 'clip.mp4';
Reader.Open;

VideoDecoder.Reader := Reader;
VideoDecoder.StreamIndex := VideoIdx;
VideoDecoder.HardwareDevice := ffhdD3D11VA;

AudioDecoder.Reader := Reader;
AudioDecoder.StreamIndex := AudioIdx;

Player.VideoDecoder := VideoDecoder;
Player.AudioDecoder := AudioDecoder;
Player.Play;
```

`Play` / `Pause` / `Stop` / `SeekTo` управляют `TFFLinkedPlayback`. `Position`, `Duration`, `State` – из linked-ядра.

### Hardware decode

`TFFHardwareDevice` (`uFFHardwareDecode`):

| Значение | Описание |
|----------|----------|
| `ffhdNone` | Software (по умолчанию) |
| `ffhdAuto` | D3D11VA на Windows, VAAPI на Linux |
| `ffhdD3D11VA`, `ffhdDXVA2`, `ffhdCUDA`, `ffhdQSV`, … | Явный бэкенд |

| Режим | Где задавать |
|-------|--------------|
| Linked | `TFFDecoder.HardwareDevice` |
| Engine | `TFFVideoPlayer.HardwareDevice` или `TFFPlaybackEngine.HardwareDevice` |

Менять `HardwareDevice` можно только в состоянии `psStopped`. При ошибке init HW – fallback на software (`TrySetup`).

HW-кадры переносятся в system memory через `av_hwframe_transfer_data` перед `libswscale`.

### A/V sync

`TFFPlaybackClock`:
- при наличии аудио – master time из audio PTS (`SetAudioTimeMs`);
- до первого audio frame – wall clock (избегает deadlock при старте);
- `WaitUntil` / `IsLate` – задержка или пропуск «опоздавших» видеокадров.

Аудиовыход Windows: WaveOut (`uFFAudioOutputWin`), ring buffer 8 слотов, `Flush` при `Stop`.

### События плеера

| Событие | Назначение |
|---------|------------|
| `OnStateChange` | `psStopped` / `psPlaying` / `psPaused` |
| `OnFrameHook` | Перехват `TFFFrame` до конвертации |
| `OnVideoHook` | Перехват BGRA-буфера |
| `OnAudioHook` | Перехват PCM перед выводом |

### TFFPlayerControl

Композитный VCL-контрол: `TFFVideoPlayer` + trackbar + play/pause/stop. Свойства `FileName`, `VideoDecoder`, `SubtitleDecoder` пробрасываются на внутренний плеер.

### FMX

`TFFFMXVideoPlayer` – FMX-аналог на базе `TFFPlaybackEngine` / linked mode. Требует FMX-зависимости при сборке.
