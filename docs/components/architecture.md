## Architecture

### Layers

```
+---------------------------------------------------------------+
|  Application (forms, UI, business logic)                      |
+---------------------------------------------------------------+
|  Components (TFFReader, TFFDecoder, TFFVideoPlayer, …)      |
+---------------------------------------------------------------+
|  Resource wrappers (TFFPacket, TFFFrame, TFFException)        |
+---------------------------------------------------------------+
|  Bindings (libavcodec, libavformat, libavutil, …)             |
+---------------------------------------------------------------+
```

#### Resource wrappers

| Class | Unit | FFmpeg API |
|-------|------|------------|
| `TFFPacket` | `uFFPacket.pas` | `av_packet_alloc` / `av_packet_free` |
| `TFFFrame` | `uFFFrame.pas` | `av_frame_alloc` / `av_frame_free` |

#### Non-visual components

Read, decode, encode, write, transcode – see [Reference](reference.md).

#### Visual components

`TFFVideoPlayer` (VCL), `TFFFMXVideoPlayer` (FMX), `TFFPlayerControl` (VCL + transport UI).

### Graph linking

Push model via interfaces in `uFFComponentBase`:
- `IFFPacketSink` / `IFFPacketSource` – demux packets
- `IFFFrameSink` / `IFFFrameSource` – decoded frames

```
TFFReader (AutoPump)
    ├─► TFFDecoder (video) ──► TFFVideoPlayer
    ├─► TFFDecoder (audio) ──► TFFAudioOutput (linked)
    └─► TFFWriter (remux)

TFFReader + TFFDecoder + TFFEncoder + TFFWriter   → transcode (TFFEncoder.Start)
TFFBitmapEncoder + TFFWriter                      → bitmap → video
TFFMemoryAccessAdapter + TFFReader/TFFWriter      → TStream I/O
```

Link properties (`Reader`, `VideoDecoder`, …) support `FreeNotification` and design-time dropdowns (`uFFComponentEditors`).

`TFFReader.AutoPump := True` – background thread reads packets and dispatches to subscribers (`TakePacket`).

### Playback threading

**Engine mode** (`TFFPlaybackEngine`):
1. **Demux thread** – `ReadPacket` → `TFFPacketQueue` (video/audio).
2. **Video decode thread** – decode → `TFFFrameConverter` (BGRA) → `OnPresentFrame`.
3. **Audio decode thread** – decode → `TFFAudioResampler` → `TFFAudioOutput`.

**Linked mode** (`TFFLinkedPlayback`):
- Separate demux / video / audio threads.
- Decoders are form components; frames reach the player via `IFFFrameSink`.

Synchronization: `TFFPlaybackClock` – audio PTS as master clock, otherwise wall clock (`uFFPlatformTime`).
