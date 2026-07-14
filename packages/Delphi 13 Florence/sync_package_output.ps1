# Fix orphaned .dcp in Public\Dcp (dcp without matching .bpl) and sync local package outputs.
param(
    [ValidateSet('Win32', 'Win64')]
    [string]$Platform = 'Win64',
    [ValidateSet('Debug', 'Release')]
    [string]$Config = 'Debug',
    [string]$PackageNames = 'rtpFFMPEG,rtpFFMPEGComponents,dclFFMPEGComponents'
)

$ErrorActionPreference = 'Stop'
$PkgDir = $PSScriptRoot
$LibDir = Join-Path $PkgDir "lib\$Platform\$Config"
$PublicDcp = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\37.0\Dcp\$Platform"
$PublicBpl = Join-Path $env:PUBLIC "Documents\Embarcadero\Studio\37.0\Bpl\$Platform"
$Names = $PackageNames -split ',' | ForEach-Object { $_.Trim() } | Where-Object { $_ }
$Root = Split-Path $PkgDir -Parent | Split-Path -Parent
$DllSrc = Join-Path $Root "bin\$($Platform.ToLower())"

function Remove-OrphanDcp([string]$Dir, [string]$Name) {
    if (-not (Test-Path $Dir)) { return }
    $dcp = Join-Path $Dir "$Name.dcp"
    $bpl = Join-Path $Dir "$Name.bpl"
    if ((Test-Path $dcp) -and -not (Test-Path $bpl)) {
        Write-Host "Removing orphan $dcp"
        Remove-Item $dcp -Force
        Remove-Item (Join-Path $Dir "$Name.bpi") -Force -ErrorAction SilentlyContinue
        Remove-Item (Join-Path $Dir "$Name.a") -Force -ErrorAction SilentlyContinue
    }
}

foreach ($name in $Names) {
    Remove-OrphanDcp $PublicDcp $name
}

New-Item -ItemType Directory -Force -Path $LibDir | Out-Null

if (Test-Path $DllSrc) {
    Copy-Item -Path (Join-Path $DllSrc '*.dll') -Destination $LibDir -Force
    if (Test-Path (Split-Path $PublicBpl -Parent)) {
        New-Item -ItemType Directory -Force -Path $PublicBpl | Out-Null
        Copy-Item -Path (Join-Path $DllSrc '*.dll') -Destination $PublicBpl -Force
        Write-Host "Copied FFmpeg DLLs to $PublicBpl"
    }
}

foreach ($name in $Names) {
    $bpl = Join-Path $LibDir "$name.bpl"
    $dcp = Join-Path $LibDir "$name.dcp"
    if (Test-Path $bpl) {
        Copy-Item $bpl $PublicDcp -Force
        if (Test-Path $dcp) {
            Copy-Item $dcp $PublicDcp -Force
        }
        Write-Host "Synced $name to $PublicDcp"
    }
}
