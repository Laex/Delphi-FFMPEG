# Generate resource/test_av.mp4 with video + audio
$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$Out = Join-Path $Root 'resource\test_av.mp4'

. (Join-Path $PSScriptRoot 'Resolve-FFmpegCli.ps1') | Out-Null
$Ffmpeg = & (Join-Path $PSScriptRoot 'Resolve-FFmpegCli.ps1')
if (-not $Ffmpeg) {
    Write-Host 'SKIP: ffmpeg CLI not found. Run tools\setup_dev_environment.ps1' -ForegroundColor Yellow
    exit 2
}

New-Item -ItemType Directory -Force -Path (Split-Path $Out) | Out-Null
$prevEap = $ErrorActionPreference
$ErrorActionPreference = 'Continue'
try {
  & $Ffmpeg -y `
    -f lavfi -i 'testsrc=duration=6:size=320x240:rate=25' `
    -f lavfi -i 'sine=frequency=880:duration=6' `
    -map 0:v:0 -map 1:a:0 `
    -pix_fmt yuv420p -c:v libx264 -c:a aac -movflags +faststart `
    -shortest $Out *> $null
} finally {
  $ErrorActionPreference = $prevEap
}

if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host "Created $Out" -ForegroundColor Green
