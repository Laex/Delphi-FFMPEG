## Playback

### TFFVideoPlayer

VCL control with two modes.

#### Engine mode

Minimal setup – set `FileName` only:

```delphi
Player.FileName := 'clip.mp4';
Player.HardwareDevice := ffhdAuto;  // optional; set before Play
Player.Play;
```

Internally `TFFPlaybackEngine` creates its own `TFFReader` / `TFFDecoder` in worker threads.

#### Linked mode

Component graph on the form:

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

`Play` / `Pause` / `Stop` / `SeekTo` control `TFFLinkedPlayback`. `Position`, `Duration`, `State` come from the linked core.

### Hardware decode

`TFFHardwareDevice` (`uFFHardwareDecode`):

| Value | Description |
|-------|-------------|
| `ffhdNone` | Software (default) |
| `ffhdAuto` | D3D11VA on Windows, VAAPI on Linux |
| `ffhdD3D11VA`, `ffhdDXVA2`, `ffhdCUDA`, `ffhdQSV`, … | Explicit backend |

| Mode | Where to set |
|------|--------------|
| Linked | `TFFDecoder.HardwareDevice` |
| Engine | `TFFVideoPlayer.HardwareDevice` or `TFFPlaybackEngine.HardwareDevice` |

Change `HardwareDevice` only while `psStopped`. On HW init failure – software fallback (`TrySetup`).

HW frames are transferred to system memory via `av_hwframe_transfer_data` before `libswscale`.

### A/V sync

`TFFPlaybackClock`:
- with audio – master time from audio PTS (`SetAudioTimeMs`);
- before the first audio frame – wall clock (avoids startup deadlock);
- `WaitUntil` / `IsLate` – delay or drop late video frames.

Windows audio: WaveOut (`uFFAudioOutputWin`), 8-slot ring buffer, `Flush` on `Stop`.

### Player events

| Event | Purpose |
|-------|---------|
| `OnStateChange` | `psStopped` / `psPlaying` / `psPaused` |
| `OnFrameHook` | Intercept `TFFFrame` before conversion |
| `OnVideoHook` | Intercept BGRA buffer |
| `OnAudioHook` | Intercept PCM before output |

### TFFPlayerControl

Composite VCL control: `TFFVideoPlayer` + trackbar + play/pause/stop. `FileName`, `VideoDecoder`, `SubtitleDecoder` forward to the inner player.

### FMX

`TFFFMXVideoPlayer` – FMX equivalent based on `TFFPlaybackEngine` / linked mode. Requires FMX dependencies at build time.
