# One-shot dev environment setup for Delphi-FFMPEG component tests (Windows)
param(
    [switch]$SkipDownload,
    [switch]$SkipMedia,
    [switch]$RunTests,
    [switch]$IncludeGuiTests,
    [string]$DelphiBin = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin'
)

$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$BinWin64 = Join-Path $Root 'bin\win64'
$Resource = Join-Path $Root 'resource'

Write-Host '=== Delphi-FFMPEG environment setup ===' -ForegroundColor Cyan

# 1. FFmpeg shared DLLs + CLI tools
if (-not $SkipDownload) {
    if (-not (Test-Path (Join-Path $BinWin64 'avutil-60.dll'))) {
        Write-Host '[1/4] Downloading FFmpeg Win64 DLLs...' -ForegroundColor Green
        & (Join-Path $Root 'bin\scripts\download_ffmpeg_dll.ps1')
    } else {
        Write-Host '[1/4] FFmpeg DLLs present' -ForegroundColor Gray
    }

    if (-not (Test-Path (Join-Path $BinWin64 'ffmpeg.exe'))) {
        Write-Host '      ffmpeg.exe missing — re-running download to fetch CLI tools...' -ForegroundColor Yellow
        & (Join-Path $Root 'bin\scripts\download_ffmpeg_dll.ps1')
    }
} else {
    Write-Host '[1/4] Skipped download (-SkipDownload)' -ForegroundColor Gray
}

if (-not (Test-Path (Join-Path $BinWin64 'avutil-60.dll'))) {
    throw 'FFmpeg DLLs still missing in bin/win64'
}

# 2. Test media
if (-not $SkipMedia) {
    Write-Host '[2/4] Generating test media...' -ForegroundColor Green
    & (Join-Path $Root 'tools\generate_test_av.ps1')
    if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne 2) { throw 'generate_test_av failed' }

    & (Join-Path $Root 'tools\generate_test_subs.ps1')
    if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne 2) { throw 'generate_test_subs failed' }

    if (-not (Test-Path (Join-Path $Resource '768x576.avi'))) {
        Write-Host '      WARNING: resource/768x576.avi not found (some tests will SKIP)' -ForegroundColor Yellow
    }
} else {
    Write-Host '[2/4] Skipped media generation (-SkipMedia)' -ForegroundColor Gray
}

# 3. Verify Delphi compiler
Write-Host '[3/4] Checking Delphi compiler...' -ForegroundColor Green
$Dcc = Join-Path $DelphiBin 'dcc64.exe'
if (-not (Test-Path $Dcc)) {
    $Dcc = (Get-Command dcc64 -ErrorAction SilentlyContinue).Source
}
if (-not $Dcc) {
    Write-Host '      WARNING: dcc64 not found — component tests require Delphi or use FPC on Linux' -ForegroundColor Yellow
} else {
    Write-Host "      dcc64: $Dcc" -ForegroundColor Gray
}

# 4. Summary
Write-Host '[4/4] Environment summary' -ForegroundColor Green
$media = @(
    (Join-Path $Resource '768x576.avi'),
    (Join-Path $Resource 'test_av.mp4'),
    (Join-Path $Resource 'test_subs.mkv')
)
foreach ($m in $media) {
    $mark = if (Test-Path $m) { '[OK]' } else { '[--]' }
    Write-Host "      $mark $(Split-Path $m -Leaf)" -ForegroundColor $(if (Test-Path $m) { 'Green' } else { 'DarkGray' })
}

$ff = & (Join-Path $Root 'tools\Resolve-FFmpegCli.ps1')
if ($ff) {
    Write-Host "      [OK] ffmpeg: $ff" -ForegroundColor Green
} else {
    Write-Host '      [--] ffmpeg CLI' -ForegroundColor DarkGray
}

if ($RunTests) {
    Write-Host ''
    Write-Host '=== Running component tests ===' -ForegroundColor Cyan
    $testArgs = @{
        Compiler = 'Delphi'
        DelphiBin = $DelphiBin
    }
    if ($IncludeGuiTests) { $testArgs['IncludeGuiTests'] = $true }
    & (Join-Path $Root 'tests\run_components.ps1') @testArgs
    exit $LASTEXITCODE
}

Write-Host ''
Write-Host 'Setup complete. Run tests:' -ForegroundColor Cyan
Write-Host '  tests\run_components.ps1 -Compiler Delphi -IncludeGuiTests' -ForegroundColor White
