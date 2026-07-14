# Generate resource/test_subs_ass.mkv with ASS subtitles
$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$OutDir = Join-Path $Root 'resource'
$Out = Join-Path $OutDir 'test_subs_ass.mkv'
$Tmp = Join-Path $OutDir 'test_subs_ass.tmp.mkv'
$Ass = Join-Path $OutDir 'test_subs.ass'

$Ffmpeg = & (Join-Path $PSScriptRoot 'Resolve-FFmpegCli.ps1')
if (-not $Ffmpeg) {
    Write-Host 'SKIP: ffmpeg CLI not found. Run tools\setup_dev_environment.ps1' -ForegroundColor Yellow
    exit 2
}

New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

@'
[Script Info]
ScriptType: v4.00+

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Arial,20,&H00FFFFFF,&H000000FF,&H00000000,&H80000000,0,0,0,0,100,100,0,0,1,2,0,2,10,10,20,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
Dialogue: 0,0:00:01.00,0:00:04.00,Default,,0,0,0,,{\an8}ASS subtitle top
Dialogue: 0,0:00:04.50,0:00:07.50,Default,,0,0,0,,{\an2}ASS subtitle bottom
'@ | Set-Content -Path $Ass -Encoding UTF8

if (Test-Path $Out) { Remove-Item $Out -Force }
if (Test-Path $Tmp) { Remove-Item $Tmp -Force }

$prevEap = $ErrorActionPreference
$ErrorActionPreference = 'Continue'
try {
  & $Ffmpeg -y `
    -f lavfi -i 'testsrc=duration=8:size=320x240:rate=25' `
    -f lavfi -i 'sine=frequency=440:duration=8' `
    -f ass -i $Ass `
    -map 0:v:0 -map 1:a:0 -map 2:0 `
    -c:v libx264 -pix_fmt yuv420p -c:a aac -c:s ass `
    -avoid_negative_ts make_zero -shortest $Tmp *> $null
} finally {
  $ErrorActionPreference = $prevEap
}

if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Move-Item -Force $Tmp $Out
Write-Host "Created $Out (and $Ass)" -ForegroundColor Green
