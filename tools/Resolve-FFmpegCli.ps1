# Resolve ffmpeg.exe: PATH first, then bin/win64/ffmpeg.exe from BtbN bundle
param(
    [switch]$Require
)

$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$Bundled = Join-Path $Root 'bin\win64\ffmpeg.exe'

$cmd = Get-Command ffmpeg -ErrorAction SilentlyContinue
if ($cmd) {
    return $cmd.Source
}

if (Test-Path $Bundled) {
    return (Resolve-Path $Bundled).Path
}

if ($Require) {
    throw "ffmpeg.exe not found. Run: tools\setup_dev_environment.ps1"
}

return $null
