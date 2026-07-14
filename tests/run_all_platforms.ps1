# Smoke-test bindings on all 6 platforms (Delphi + FPC x Win32/Win64/Linux64)
param(
    [string]$FpcRoot = 'D:\Work\Delphi\OS\compilers\fpcupdeluxe',
    [string]$DelphiBin = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin',
    [string]$WslDistro = 'Ubuntu-24.04'
)

$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$Src = Join-Path $Root 'source'
$Test = Join-Path $Root 'tests\binding_test.dpr'
$LogDir = Join-Path $Root 'bin\out\platform_tests'
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null

function Get-WslPath {
    param([string]$WinPath)
    $p = (Resolve-Path $WinPath).Path.Replace('\', '/')
    if ($p -match '^([A-Za-z]):(.*)$') {
        return ('/mnt/{0}{1}' -f $Matches[1].ToLower(), $Matches[2])
    }
    return $p
}

function Resolve-FpcToolchain {
    param([string]$RootPath)
    $candidates = @(
        @{ Layout = 'fpcupdeluxe-x64'; Exe = Join-Path $RootPath 'fpc\bin\x86_64-win64\fpc.exe'; CrossLinux = Join-Path $RootPath 'cross\bin\x86_64-linux' },
        @{ Layout = 'fpcupdeluxe-i386'; Exe = Join-Path $RootPath 'fpc\bin\i386-win32\fpc.exe'; CrossLinux = Join-Path $RootPath 'cross\bin\x86_64-linux' },
        @{ Layout = 'standard-i386'; Exe = Join-Path $RootPath 'bin\i386-win32\fpc.exe'; CrossLinux = '' },
        @{ Layout = 'standard-x64'; Exe = Join-Path $RootPath 'bin\x86_64-win64\fpc.exe'; CrossLinux = '' }
    )
    foreach ($item in $candidates) {
        if (Test-Path $item.Exe) { return $item }
    }
    foreach ($child in Get-ChildItem -Path $RootPath -Directory -ErrorAction SilentlyContinue) {
        foreach ($item in $candidates) {
            $exe = Join-Path $child.FullName ($item.Exe.Substring($RootPath.Length + 1))
            if (Test-Path $exe) {
                return @{
                    Layout = $item.Layout
                    Exe = $exe
                    CrossLinux = if ($item.CrossLinux) { Join-Path $child.FullName ($item.CrossLinux.Substring($RootPath.Length + 1)) } else { '' }
                }
            }
        }
    }
    return $null
}

$FpcToolchain = Resolve-FpcToolchain $FpcRoot
$Fpc = if ($FpcToolchain) { $FpcToolchain.Exe } else { Join-Path $FpcRoot 'fpc\bin\x86_64-win64\fpc.exe' }
$CrossLinux = if ($FpcToolchain) { $FpcToolchain.CrossLinux } else { Join-Path $FpcRoot 'cross\bin\x86_64-linux' }
$FpcLayout = if ($FpcToolchain) { $FpcToolchain.Layout } else { 'unknown' }
$Results = [System.Collections.Generic.List[object]]::new()

function Add-Result {
    param([string]$Platform, [string]$Compiler, [string]$Status, [string]$Detail = '')
    $Results.Add([pscustomobject]@{ Platform = $Platform; Compiler = $Compiler; Status = $Status; Detail = $Detail })
    $color = switch ($Status) {
        'PASS' { 'Green' }
        'FAIL' { 'Red' }
        'SKIP' { 'Yellow' }
        default { 'White' }
    }
    Write-Host ("[{0}] {1} / {2}" -f $Status, $Compiler, $Platform) -ForegroundColor $color
    if ($Detail) { Write-Host "       $Detail" }
}

function Invoke-Logged {
    param([string]$Name, [scriptblock]$Block)
    $log = Join-Path $LogDir "$Name.log"
    $exitCode = 0
    try {
        & $Block *> $log
        if ($null -ne $LASTEXITCODE) { $exitCode = $LASTEXITCODE }
    } catch {
        $_ | Out-File $log -Append -Encoding utf8
        $exitCode = 1
    }
    return @{ Ok = ($exitCode -eq 0); Log = $log; ExitCode = $exitCode }
}

function Test-OutputPass {
    param([string]$LogPath)
    if (-not (Test-Path $LogPath)) { return $false }
    $text = Get-Content $LogPath -Raw -ErrorAction SilentlyContinue
    return ($text -match 'PASS:')
}

function Ensure-WinLibs {
    param([string]$Platform)
    $bin = Join-Path $Root "bin\$($Platform.ToLower())"
    if (-not (Test-Path (Join-Path $bin 'avutil-60.dll'))) {
        throw "FFmpeg DLLs missing in $bin (run bin/scripts/download_ffmpeg_dll*.ps1)"
    }
    return $bin
}

function Ensure-LinuxLibs {
    $bin = Join-Path $Root 'bin\linux64'
    if (-not (Test-Path (Join-Path $bin 'libavutil.so.60'))) {
        throw 'FFmpeg .so missing in bin/linux64 (run tests/linux/run_wsl_test.sh once)'
    }
    return $bin
}

# --- Delphi Win32 ---
try {
    $bin = Ensure-WinLibs 'Win32'
    $dcc = Join-Path $DelphiBin 'dcc32.exe'
    if (-not (Test-Path $dcc)) { throw "dcc32 not found: $dcc" }
    $exe = Join-Path $bin 'binding_test_delphi32.exe'
    $r = Invoke-Logged 'delphi_win32' {
        & $dcc -NSSystem -U"$Src" -E"$bin" "$Test"
        if ($LASTEXITCODE -ne 0) { throw "dcc64 exit $LASTEXITCODE" }
        $built = Join-Path $bin 'binding_test.exe'
        if (Test-Path $built) { Move-Item -Force $built $exe }
        & $exe
    }
    if ($r.Ok -and (Test-OutputPass $r.Log)) { Add-Result 'Win32' 'Delphi' 'PASS' } else { Add-Result 'Win32' 'Delphi' 'FAIL' $r.Log }
} catch {
    Add-Result 'Win32' 'Delphi' 'FAIL' $_.Exception.Message
}

# --- Delphi Win64 ---
try {
    $bin = Ensure-WinLibs 'Win64'
    $dcc = Join-Path $DelphiBin 'dcc64.exe'
    if (-not (Test-Path $dcc)) { throw "dcc64 not found: $dcc" }
    $exe = Join-Path $bin 'binding_test_delphi64.exe'
    $r = Invoke-Logged 'delphi_win64' {
        & $dcc -NSSystem -U"$Src" -E"$bin" "$Test"
        if ($LASTEXITCODE -ne 0) { throw "dcc64 exit $LASTEXITCODE" }
        $built = Join-Path $bin 'binding_test.exe'
        if (Test-Path $built) { Move-Item -Force $built $exe }
        & $exe
    }
    if ($r.Ok -and (Test-OutputPass $r.Log)) { Add-Result 'Win64' 'Delphi' 'PASS' } else { Add-Result 'Win64' 'Delphi' 'FAIL' $r.Log }
} catch {
    Add-Result 'Win64' 'Delphi' 'FAIL' $_.Exception.Message
}

# --- Delphi Linux64 ---
try {
    $bin = Ensure-LinuxLibs
    $dcc = Join-Path $DelphiBin 'dcclinux64.exe'
    if (-not (Test-Path $dcc)) { throw "dcclinux64 not found: $dcc" }
    $sdkRoot = Join-Path (Split-Path $DelphiBin -Parent) 'PAServer\linux\bin'
    if (-not (Test-Path $sdkRoot)) {
        throw 'SKIP: Delphi Linux SDK missing. Install PAServer on WSL (RAD Studio redistributable).'
    }
    $elf = Join-Path $bin 'binding_test_delphi_linux64'
    $wslElf = Get-WslPath $elf
    $wslLib = Get-WslPath $bin
    $r = Invoke-Logged 'delphi_linux64' {
        & $dcc -NSSystem -U"$Src" -E"$bin" -L"$sdkRoot" "$Test"
        if ($LASTEXITCODE -ne 0) { throw "dcclinux64 exit $LASTEXITCODE" }
        $built = Join-Path $bin 'binding_test'
        if (Test-Path $built) { Move-Item -Force $built $elf }
        wsl -d $WslDistro bash -lc "export LD_LIBRARY_PATH=$wslLib; $wslElf"
    }
    if ($r.Ok -and (Test-OutputPass $r.Log)) { Add-Result 'Linux64' 'Delphi' 'PASS' } else { Add-Result 'Linux64' 'Delphi' 'FAIL' $r.Log }
} catch {
    if ($_.Exception.Message -like 'SKIP:*') {
        Add-Result 'Linux64' 'Delphi' 'SKIP' $_.Exception.Message.Substring(6)
    } else {
        Add-Result 'Linux64' 'Delphi' 'FAIL' $_.Exception.Message
    }
}

# --- FPC Win64 ---
try {
    if (-not (Test-Path $Fpc)) { throw "fpc not found under $FpcRoot (tried fpcupdeluxe and standard layouts)" }
    $bin = Ensure-WinLibs 'Win64'
    $exe = Join-Path $bin 'binding_test_fpc64.exe'
    $r = Invoke-Logged 'fpc_win64' {
        & $Fpc -Mdelphi -Px86_64 -Twin64 -Fu"$Src" -Fl"$bin" -FE"$bin" -o"$exe" "$Test"
        if ($LASTEXITCODE -ne 0) { throw "fpc win64 exit $LASTEXITCODE" }
        & $exe
    }
    if ($r.Ok -and (Test-OutputPass $r.Log)) { Add-Result 'Win64' 'FPC' 'PASS' } else { Add-Result 'Win64' 'FPC' 'FAIL' $r.Log }
} catch {
    Add-Result 'Win64' 'FPC' 'FAIL' $_.Exception.Message
}

# --- FPC Win32 ---
try {
    if (-not (Test-Path $Fpc)) { throw "fpc not found under $FpcRoot (tried fpcupdeluxe and standard layouts)" }
    $bin = Ensure-WinLibs 'Win32'
    $exe = Join-Path $bin 'binding_test_fpc32.exe'
    $ppc386 = Join-Path (Split-Path $Fpc -Parent) 'ppc386.exe'
    if ($FpcLayout -eq 'fpcupdeluxe-x64' -and -not (Test-Path $ppc386)) {
        throw 'ppc386 missing: FPC x86_64 host cannot cross-build i386-win32 (needs native i386-win32 FPC in fpcupdeluxe)'
    }
    $r = Invoke-Logged 'fpc_win32' {
        & $Fpc -Mdelphi -Twin32 -Pi386 -Fu"$Src" -Fl"$bin" -FE"$bin" -o"$exe" "$Test"
        if ($LASTEXITCODE -ne 0) { throw "fpc win32 exit $LASTEXITCODE" }
        & $exe
    }
    if ($r.Ok -and (Test-OutputPass $r.Log)) { Add-Result 'Win32' 'FPC' 'PASS' } else { Add-Result 'Win32' 'FPC' 'FAIL' $r.Log }
} catch {
    if ($_.Exception.Message -like 'ppc386 missing*') {
        Add-Result 'Win32' 'FPC' 'SKIP' 'needs native i386-win32 FPC in fpcupdeluxe (Cross tab: i386-win32; x86_64 host cannot cross-build i386)'
    } else {
        Add-Result 'Win32' 'FPC' 'FAIL' $_.Exception.Message
    }
}

# --- FPC Linux64 (cross from Windows, run in WSL) ---
try {
    if (-not (Test-Path $Fpc)) { throw "fpc not found under $FpcRoot (tried fpcupdeluxe and standard layouts)" }
    if (-not $CrossLinux -or -not (Test-Path $CrossLinux)) {
        throw 'SKIP: Linux cross binutils missing (install fpcupdeluxe x86_64-linux cross compiler)'
    }
    $bin = Ensure-LinuxLibs
    $elf = Join-Path $bin 'binding_test_fpc_linux64'
    $wslElf = Get-WslPath $elf
    $wslLib = Get-WslPath $bin
    $r = Invoke-Logged 'fpc_linux64' {
        & $Fpc -Mdelphi -Px86_64 -Tlinux -XPx86_64-linux- -XR"$CrossLinux" -Fu"$Src" -Fl"$bin" -FE"$bin" -o"$elf" "$Test"
        if ($LASTEXITCODE -ne 0) { throw "fpc linux64 exit $LASTEXITCODE" }
        wsl -d $WslDistro bash -lc "export LD_LIBRARY_PATH=$wslLib; $wslElf"
    }
    if ($r.Ok -and (Test-OutputPass $r.Log)) { Add-Result 'Linux64' 'FPC' 'PASS' } else { Add-Result 'Linux64' 'FPC' 'FAIL' $r.Log }
} catch {
    if ($_.Exception.Message -like 'SKIP:*') {
        Add-Result 'Linux64' 'FPC' 'SKIP' $_.Exception.Message.Substring(6)
    } else {
        Add-Result 'Linux64' 'FPC' 'FAIL' $_.Exception.Message
    }
}

Write-Host ''
Write-Host '=== Summary ===' -ForegroundColor Cyan
$Results | Format-Table -AutoSize
$fail = @($Results | Where-Object { $_.Status -eq 'FAIL' }).Count
$summaryPath = Join-Path $LogDir 'summary.json'
$Results | ConvertTo-Json | Set-Content $summaryPath -Encoding utf8
Write-Host "Logs: $LogDir"
if ($fail -gt 0) { exit 1 }
