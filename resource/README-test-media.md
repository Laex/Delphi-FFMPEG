# Test media with audio (optional)

Linked player and audio tests prefer `test_av.mp4` in this folder.

- Generate A/V and subtitle fixtures (needs `ffmpeg` on PATH or `bin/win64/ffmpeg.exe` from the download script):

```powershell
tools\setup_dev_environment.ps1
# or separately:
tools\generate_test_av.ps1
tools\generate_test_subs.ps1
```

Without `test_av.mp4`, component tests use `768x576.avi` (video-only).
