# Run all component tests (Delphi or FPC)
param(
    [ValidateSet('Delphi', 'FPC')]
    [string]$Compiler = 'Delphi',
    [string]$Platform = 'Win64',
    [string]$DelphiBin = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin',
    [string]$FpcRoot = 'D:\Work\Delphi\OS\compilers\fpcupdeluxe',
    [string]$MediaFile = '',
    [switch]$IncludeGuiTests
)

$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$Src = Join-Path $Root 'source'
$Comp = Join-Path $Root 'source\components'
$TestsDir = Join-Path $Root 'tests\components'
$Bin = Join-Path $Root "bin\$($Platform.ToLower())"
$LogDir = Join-Path $Root 'bin\out\component_tests'
New-Item -ItemType Directory -Force -Path $LogDir, $Bin | Out-Null

$GuiOnly = @(
    'player_smoke_test.dpr',
    'linked_player_smoke_test.dpr',
    'fmx_player_smoke_test.dpr',
    'player_control_smoke_test.dpr',
    'thumbnail_test.dpr',
    'bitmap_encoder_test.dpr',
    'decoder_preview_test.dpr'
)

$FmxTests = @('fmx_player_smoke_test.dpr')
$OptionalHwTests = @('hw_decode_test.dpr')

function Ensure-TestMedia {
    $genAv = Join-Path $Root 'tools\generate_test_av.ps1'
    $genSubs = Join-Path $Root 'tools\generate_test_subs.ps1'
    $genBitmap = Join-Path $Root 'tools\generate_test_subs_bitmap.ps1'
    $genAss = Join-Path $Root 'tools\generate_test_subs_ass.ps1'
    if (Test-Path $genAv) { & $genAv | Out-Null }
    if (Test-Path $genSubs) { & $genSubs | Out-Null }
    if (Test-Path $genBitmap) { & $genBitmap | Out-Null }
    if (Test-Path $genAss) { & $genAss | Out-Null }

    $destDir = Join-Path $Bin 'test_media'
    New-Item -ItemType Directory -Force -Path $destDir | Out-Null
    foreach ($name in @('test_av.mp4', '768x576.avi', 'test_subs.mp4', 'test_subs.mkv', 'test_subs.srt',
        'test_subs_bitmap.mkv', 'test_subs_ass.mkv', 'test_subs.ass')) {
        $src = Join-Path $Root "resource\$name"
        if (Test-Path $src) {
            Copy-Item -Force $src (Join-Path $destDir $name)
        }
    }
    return $destDir
}

$TestMediaDir = Ensure-TestMedia

$SkipMediaArg = @(
    'encoder_smoke_test.dpr',
    'loader_logger_test.dpr',
    'bitmap_encoder_test.dpr',
    'wrapper_test.dpr',
    'platform_preset_test.dpr'
)

function Resolve-TestMedia {
    param(
        [string]$TestName,
        [string]$Override
    )
    if ($Override -and (Test-Path $Override)) {
        return (Resolve-Path $Override).Path
    }

    $resource = $TestMediaDir
    $avi = Join-Path $resource '768x576.avi'
    $av = Join-Path $resource 'test_av.mp4'
    $subs = Join-Path $resource 'test_subs.mp4'
    if (-not (Test-Path $subs)) { $subs = Join-Path $resource 'test_subs.mkv' }

    if ($TestName -eq 'subtitle_decoder_test.dpr') {
        if (Test-Path $subs) { return (Resolve-Path $subs).Path }
    }
    if ($TestName -eq 'subtitle_bitmap_test.dpr') {
        $bm = Join-Path $resource 'test_subs_bitmap.mkv'
        if (Test-Path $bm) { return (Resolve-Path $bm).Path }
    }
    if ($TestName -eq 'subtitle_ass_test.dpr') {
        $ass = Join-Path $resource 'test_subs_ass.mkv'
        if (Test-Path $ass) { return (Resolve-Path $ass).Path }
    }

    $preferAv = @(
        'audio_resampler_test.dpr',
        'hook_smoke_test.dpr',
        'player_smoke_test.dpr',
        'linked_player_smoke_test.dpr',
        'player_control_smoke_test.dpr',
        'encoder_transcode_test.dpr',
        'transcode_clip_test.dpr',
        'decoder_preview_test.dpr',
        'frame_filter_test.dpr',
        'thumbnail_test.dpr',
        'bitmap_encoder_test.dpr'
    )
    if ($preferAv -contains $TestName) {
        if (Test-Path $av) { return (Resolve-Path $av).Path }
        if (Test-Path $avi) { return (Resolve-Path $avi).Path }
    }

    if (Test-Path $av) { return (Resolve-Path $av).Path }
    if (Test-Path $avi) { return (Resolve-Path $avi).Path }
    return ''
}

function Ensure-WinLibs {
    if (-not (Test-Path (Join-Path $Bin 'avutil-60.dll'))) {
        $dl = Join-Path $Root 'bin\scripts\download_ffmpeg_dll.ps1'
        if (Test-Path $dl) { & $dl }
    }
    if (-not (Test-Path (Join-Path $Bin 'avutil-60.dll'))) {
        throw "FFmpeg DLLs missing in $Bin"
    }
}

function Test-OutputPass {
    param([string]$LogPath)
    if (-not (Test-Path $LogPath)) { return $false }
    $text = Get-Content $LogPath -Raw -ErrorAction SilentlyContinue
    return ($text -match 'PASS:')
}

function Invoke-ComponentTest {
    param(
        [string]$Exe,
        [string[]]$Args,
        [string]$RunLog
    )
    $argLine = ''
    if ($Args -and $Args.Count -gt 0) {
        $argLine = ($Args | ForEach-Object {
            if ($_ -match '\s') { '"' + ($_ -replace '"','\"') + '"' } else { $_ }
        }) -join ' '
    }
    $cmd = 'cmd /c ""{0}" {1} > "{2}" 2>&1"' -f $Exe, $argLine, $RunLog
    cmd /c $cmd
    return $LASTEXITCODE
}

function Stop-TestProcesses {
    Get-Process -ErrorAction SilentlyContinue | Where-Object {
        $_.Path -and ($_.Path.StartsWith($Bin, [StringComparison]::OrdinalIgnoreCase))
    } | ForEach-Object {
        Stop-Process -Id $_.Id -Force -ErrorAction SilentlyContinue
    }
    Start-Sleep -Milliseconds 500
}

function Test-OutputSkip {
    param([string]$LogPath)
    if (-not (Test-Path $LogPath)) { return $false }
    $text = Get-Content $LogPath -Raw -ErrorAction SilentlyContinue
    return ($text -match 'SKIP:')
}

$Results = [System.Collections.Generic.List[object]]::new()

function Add-Result {
    param([string]$Name, [string]$Status, [string]$Detail = '')
    $Results.Add([pscustomobject]@{ Test = $Name; Status = $Status; Detail = $Detail })
    $color = switch ($Status) {
        'PASS' { 'Green' }
        'FAIL' { 'Red' }
        'SKIP' { 'Yellow' }
        default { 'White' }
    }
    Write-Host ("[{0}] {1}" -f $Status, $Name) -ForegroundColor $color
    if ($Detail) { Write-Host "       $Detail" }
}

Ensure-WinLibs

$dcc = Join-Path $DelphiBin 'dcc64.exe'
$fpc = Join-Path $FpcRoot 'fpc\bin\x86_64-win64\fpc.exe'
$prevDir = Get-Location

try {
    Set-Location $Bin

    $testFiles = Get-ChildItem -Path $TestsDir -Filter '*.dpr' | Sort-Object Name
    foreach ($test in $testFiles) {
        $name = $test.Name
        if ((-not $IncludeGuiTests) -and ($GuiOnly -contains $name)) {
            Add-Result $name 'SKIP' 'GUI test (use -IncludeGuiTests)'
            continue
        }

        if ($FmxTests -contains $name) {
            Add-Result $name 'SKIP' 'FMX test (compile separately with FMX deps)'
            continue
        }

        if ($OptionalHwTests -contains $name) {
            Add-Result $name 'SKIP' 'HW decode (requires GPU/D3D11VA; run manually)'
            continue
        }

            $log = Join-Path $LogDir ($name -replace '\.dpr$', '.log')
            $runOut = Join-Path $LogDir ($name -replace '\.dpr$', '.run.log')
            if (Test-Path $runOut) { Remove-Item $runOut -Force }
        $exeName = ($name -replace '\.dpr$', '.exe')
        $exe = Join-Path $Bin $exeName
        $exitCode = 0
        $media = Resolve-TestMedia $name $MediaFile

        try {
            Stop-TestProcesses
            if ($Compiler -eq 'Delphi') {
                if (-not (Test-Path $dcc)) { throw "dcc64 not found: $dcc" }
                & $dcc '-NSSystem;Vcl;Vcl.Imaging;ComCtrls' "-U$Src;$Comp" "-E$Bin" $test.FullName *> $log
                if ($LASTEXITCODE -ne 0) { throw "compile failed ($LASTEXITCODE)" }
            } else {
                if (-not (Test-Path $fpc)) { throw "fpc not found: $fpc" }
                & $fpc -Mdelphi -Px86_64 -Twin64 "-Fu$Src;$Comp" "-Fl$Bin" "-FE$Bin" "-o$exe" $test.FullName *> $log
                if ($LASTEXITCODE -ne 0) { throw "compile failed ($LASTEXITCODE)" }
            }

            $runArgs = @()
            if ($media -and ($SkipMediaArg -notcontains $name)) { $runArgs += $media }

            $prevEap = $ErrorActionPreference
            $ErrorActionPreference = 'Continue'
            try {
            $exitCode = Invoke-ComponentTest -Exe $exe -Args $runArgs -RunLog $runOut
            } finally {
                $ErrorActionPreference = $prevEap
            }
            if (Test-Path $runOut) { Get-Content $runOut | Add-Content $log }

            $passText = ''
            if (Test-Path $runOut) { $passText = Get-Content $runOut -Raw -ErrorAction SilentlyContinue }
            $runtimePass = ($passText -match 'PASS:')

            if ($GuiOnly -contains $name) {
                if ($exitCode -eq 2 -or ($passText -match 'SKIP:')) {
                    Add-Result $name 'SKIP' $log
                } elseif ($exitCode -eq 0) {
                    Add-Result $name 'PASS'
                } else {
                    Add-Result $name 'FAIL' $(if ($runOut) { $runOut } else { $log })
                }
            } elseif ($exitCode -eq 2 -or ($passText -match 'SKIP:')) {
                Add-Result $name 'SKIP' $log
            } elseif ($exitCode -eq 0 -and $runtimePass) {
                Add-Result $name 'PASS'
            } else {
                Add-Result $name 'FAIL' $(if ($runOut) { $runOut } else { $log })
            }
        } catch {
            Add-Result $name 'FAIL' $_.Exception.Message
        }
    }
} finally {
    Set-Location $prevDir
}

$summary = @{
    compiler = $Compiler
    platform = $Platform
    mediaDefault = (Resolve-TestMedia '' $MediaFile)
    timestamp = (Get-Date).ToString('o')
    results = $Results
}
$summaryPath = Join-Path $LogDir 'summary.json'
$summary | ConvertTo-Json -Depth 4 | Set-Content -Path $summaryPath -Encoding utf8

$failed = @($Results | Where-Object { $_.Status -eq 'FAIL' }).Count
Write-Host ""
Write-Host ("Summary: PASS={0} SKIP={1} FAIL={2} -> {3}" -f `
    (@($Results | Where-Object { $_.Status -eq 'PASS' }).Count), `
    (@($Results | Where-Object { $_.Status -eq 'SKIP' }).Count), `
    $failed, $summaryPath)

if ($failed -gt 0) { exit 1 }
exit 0
