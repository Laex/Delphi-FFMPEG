# Build component demos (Win64)
$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$Src = Join-Path $Root 'source'
$Comp = Join-Path $Root 'source\components'
$Dcc = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc64.exe'
$UnitPath = "$Src;$Comp"
$Ns = 'System;System.Win;Winapi;Vcl'

if (-not (Test-Path $Dcc)) {
    Write-Error "dcc64 not found: $Dcc"
}

Push-Location (Join-Path $PSScriptRoot 'demo_player')
& $Dcc -NS$Ns "-U$UnitPath" demo_player.dpr
if ($LASTEXITCODE -ne 0) { Pop-Location; exit $LASTEXITCODE }
Pop-Location

Push-Location (Join-Path $PSScriptRoot 'demo_transcode')
& $Dcc -NS$Ns "-U$UnitPath" demo_transcode.dpr
if ($LASTEXITCODE -ne 0) { Pop-Location; exit $LASTEXITCODE }
Pop-Location

Push-Location (Join-Path $PSScriptRoot 'multidemo')
& $Dcc -NS$Ns "-U$UnitPath" multidemo.dpr
if ($LASTEXITCODE -ne 0) { Pop-Location; exit $LASTEXITCODE }
Pop-Location

Write-Host 'Demos built OK' -ForegroundColor Green
