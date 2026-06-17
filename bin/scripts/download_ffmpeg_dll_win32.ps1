# PowerShell script: download FFmpeg 8.1.x shared DLLs (Win32/x86) into bin/win32/# Source: defisym/FFmpeg-Builds-Win32 (BtbN does not publish win32 builds)

$ProgressPreference = 'SilentlyContinue'

$Url = "https://github.com/defisym/FFmpeg-Builds-Win32/releases/download/latest/ffmpeg-n8.1-latest-win32-gpl-shared-8.1.zip"
$BinRoot = Split-Path $PSScriptRoot -Parent
$DestDir = Join-Path $BinRoot "win32"
$ZipPath = Join-Path $BinRoot "ffmpeg-win32.zip"
$ExtractPath = Join-Path $BinRoot "temp_extract_win32"

Write-Host "=== Download FFmpeg 8.1.x Shared (Win32) ===" -ForegroundColor Green
Write-Host "URL: $Url" -ForegroundColor Gray

try {
    if (-not (Test-Path $DestDir)) {
        New-Item -ItemType Directory -Path $DestDir | Out-Null
    }

    Write-Host "Downloading..." -NoNewline
    Invoke-WebRequest -Uri $Url -OutFile $ZipPath -UseBasicParsing
    Write-Host " [OK]" -ForegroundColor Green

    Write-Host "Extracting..." -NoNewline
    if (Test-Path $ExtractPath) { Remove-Item $ExtractPath -Recurse -Force }
    Expand-Archive -Path $ZipPath -DestinationPath $ExtractPath
    Write-Host " [OK]" -ForegroundColor Green

    $DllFolder = Get-ChildItem -Path $ExtractPath -Directory | Select-Object -First 1
    $DllSourcePath = Join-Path $DllFolder.FullName "bin"

    Write-Host "Copying DLLs to $DestDir ..." -NoNewline
    $Dlls = Get-ChildItem -Path $DllSourcePath -Filter *.dll
    foreach ($Dll in $Dlls) {
        Copy-Item $Dll.FullName $DestDir -Force
    }
    Write-Host " [OK]" -ForegroundColor Green

    Write-Host "`nCopied libraries:" -ForegroundColor Green
    $Dlls | ForEach-Object { Write-Host " - $($_.Name)" -ForegroundColor Yellow }

} catch {
    Write-Host "`n[ERROR] Failed to download or extract FFmpeg: $_" -ForegroundColor Red
    exit 1
} finally {
    if (Test-Path $ZipPath) { Remove-Item $ZipPath -Force }
    if (Test-Path $ExtractPath) { Remove-Item $ExtractPath -Recurse -Force }
}
