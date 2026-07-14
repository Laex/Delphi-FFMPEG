# FPC binding smoke test on Windows (Win32 or Win64)
param(
    [ValidateSet('Win32', 'Win64')]
    [string]$Platform = 'Win64',
    [string]$FpcRoot = 'D:\Work\Delphi\OS\compilers\fpcupdeluxe'
)

$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$Bin = Join-Path $Root "bin\$($Platform.ToLower())"
$Src = Join-Path $Root 'source'
$Test = Join-Path $Root 'tests\binding_test.dpr'
$Fpc = Join-Path $FpcRoot 'fpc\bin\x86_64-win64\fpc.exe'

if (-not (Test-Path $Fpc)) {
    Write-Host "[ERROR] fpc not found: $Fpc" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path (Join-Path $Bin 'avutil-60.dll'))) {
    Write-Host "[ERROR] FFmpeg DLLs not found in $Bin" -ForegroundColor Red
    Write-Host 'Run bin/scripts/download_ffmpeg_dll.ps1 or download_ffmpeg_dll_win32.ps1'
    exit 1
}

Write-Host "=== FPC binding test ($Platform) ===" -ForegroundColor Green
$Exe = Join-Path $Bin "binding_test_fpc$($Platform.Substring(3)).exe"
if ($Platform -eq 'Win32') {
    $ppc386 = Join-Path $FpcRoot 'fpc\bin\i386-win32\ppc386.exe'
    if (-not (Test-Path $ppc386)) {
        Write-Host '[SKIP] FPC Win32 needs native i386-win32 FPC (x86_64 host cannot cross-build i386)' -ForegroundColor Yellow
        exit 2
    }
    & $Fpc -Mdelphi -Twin32 -Pi386 -Fu"$Src" -Fl"$Bin" -FE"$Bin" -o$Exe "$Test"
} else {
    & $Fpc -Mdelphi -Px86_64 -Twin64 -Fu"$Src" -Fl"$Bin" -FE"$Bin" -o$Exe "$Test"
}
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

& $Exe
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
