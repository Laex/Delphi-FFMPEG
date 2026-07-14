# Copy FFmpeg DLLs next to built packages (required when the IDE loads rtpFFMPEG.bpl)
param(
    [ValidateSet('Win32', 'Win64')]
    [string]$Platform = 'Win64',
    [ValidateSet('Debug', 'Release')]
    [string]$Config = 'Debug',
    [switch]$AlsoPublicBpl
)

$ErrorActionPreference = 'Stop'
$Root = Split-Path $PSScriptRoot -Parent | Split-Path -Parent
$Src = Join-Path $Root "bin\$($Platform.ToLower())"
$Dst = Join-Path $PSScriptRoot "lib\$Platform\$Config"

if (-not (Test-Path $Src)) {
    Write-Error "FFmpeg DLL folder not found: $Src (run bin/scripts/download_ffmpeg_dll.ps1)"
}

New-Item -ItemType Directory -Force -Path $Dst | Out-Null
Copy-Item -Path (Join-Path $Src '*.dll') -Destination $Dst -Force
Write-Host "Copied FFmpeg DLLs to $Dst"

if ($AlsoPublicBpl) {
    $PublicBpl = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\37.0\Bpl\$Platform"
    if (Test-Path (Split-Path $PublicBpl -Parent)) {
        New-Item -ItemType Directory -Force -Path $PublicBpl | Out-Null
        Copy-Item -Path (Join-Path $Src '*.dll') -Destination $PublicBpl -Force
        Write-Host "Copied FFmpeg DLLs to $PublicBpl"
    }
}
