# Generate resource/test_subs.mp4 with mov_text subtitles (reliable for demux/decode tests)
$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$OutDir = Join-Path $Root 'resource'
$Out = Join-Path $OutDir 'test_subs.mp4'
$Tmp = Join-Path $OutDir 'test_subs.tmp.mp4'
$Srt = Join-Path $OutDir 'test.srt'

$Ffmpeg = & (Join-Path $PSScriptRoot 'Resolve-FFmpegCli.ps1')
if (-not $Ffmpeg) {
    Write-Host 'SKIP: ffmpeg CLI not found. Run tools\setup_dev_environment.ps1' -ForegroundColor Yellow
    exit 2
}

New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

@'
1
00:00:01,000 --> 00:00:04,000
Hello FFmpeg subtitles

2
00:00:04,500 --> 00:00:07,500
Delphi-FFMPEG component test
'@ | Set-Content -Path $Srt -Encoding UTF8

if (Test-Path $Out) { Remove-Item $Out -Force }
if (Test-Path $Tmp) { Remove-Item $Tmp -Force }

$prevEap = $ErrorActionPreference
$ErrorActionPreference = 'Continue'
try {
  & $Ffmpeg -y `
    -f lavfi -i 'testsrc=duration=8:size=320x240:rate=25' `
    -f lavfi -i 'sine=frequency=440:duration=8' `
    -f srt -i $Srt `
    -map 0:v:0 -map 1:a:0 -map 2:0 `
    -c:v libx264 -pix_fmt yuv420p -c:a aac -c:s mov_text `
    -avoid_negative_ts make_zero -shortest $Tmp *> $null
} finally {
  $ErrorActionPreference = $prevEap
}

if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Move-Item -Force $Tmp $Out

# Keep mkv alias for older paths
$Mkv = Join-Path $OutDir 'test_subs.mkv'
Copy-Item -Force $Out $Mkv

# Sidecar for LoadAll fallback (ChangeFileExt on test_subs.mp4 -> test_subs.srt)
$SubsSrt = Join-Path $OutDir 'test_subs.srt'
Copy-Item -Force $Srt $SubsSrt

Write-Host "Created $Out (and $Mkv, $SubsSrt)" -ForegroundColor Green
