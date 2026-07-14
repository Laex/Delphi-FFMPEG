# Download FFmpeg 8.1.x shared DLLs (Win64/x64) into bin/win64/
# Source: BtbN/FFmpeg-Builds

$ProgressPreference = 'SilentlyContinue'

$Url = "https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-n8.1-latest-win64-gpl-shared-8.1.zip"
$BinRoot = Split-Path $PSScriptRoot -Parent
$DestDir = Join-Path $BinRoot "win64"
$ZipPath = Join-Path $BinRoot "ffmpeg-win64.zip"
$ExtractPath = Join-Path $BinRoot "temp_extract_win64"

Write-Host "=== Download FFmpeg 8.1.x Shared (Win64) ===" -ForegroundColor Green
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

    foreach ($Tool in @('ffmpeg.exe', 'ffprobe.exe')) {
        $Src = Join-Path $DllSourcePath $Tool
        if (Test-Path $Src) {
            Copy-Item $Src $DestDir -Force
            Write-Host "Copied $Tool" -ForegroundColor Yellow
        }
    }

    Write-Host "`nCopied libraries:" -ForegroundColor Green
    $Dlls | ForEach-Object { Write-Host " - $($_.Name)" -ForegroundColor Yellow }

} catch {
    Write-Host "`n[ERROR] Failed to download or extract FFmpeg: $_" -ForegroundColor Red
    exit 1
} finally {
    if (Test-Path $ZipPath) { Remove-Item $ZipPath -Force }
    if (Test-Path $ExtractPath) { Remove-Item $ExtractPath -Recurse -Force }
}
