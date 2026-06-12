# PowerShell script to compile FFmpeg binding examples into bin/

Write-Host "=== Building examples ===" -ForegroundColor Green

$RootDir = (Get-Item ..).FullName
$BinDir = Join-Path $RootDir "bin"
$Dcc64 = "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\dcc64.exe"

if (-not (Test-Path $Dcc64)) {
    $Dcc64 = (Get-Command dcc64 -ErrorAction SilentlyContinue).Source
}
if (-not $Dcc64) {
    Write-Host "[ERROR] dcc64 not found in PATH" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path $BinDir)) {
    New-Item -ItemType Directory -Path $BinDir | Out-Null
    Write-Host "Created bin directory" -ForegroundColor Yellow
}

$CommonArgs = @(
    "-NSSystem;Winapi;Vcl",
    "-U`"$RootDir\source`"",
    "-E`"$BinDir`""
)

$Projects = @(
    @{ Path = "avio_dir_cmd.dpr"; Extra = @() },
    @{ Path = "avio_reading.dpr"; Extra = @() },
    @{ Path = "decode_audio.dpr"; Extra = @() },
    @{ Path = "decode_video.dpr"; Extra = @() },
    @{ Path = "decoding_encoding.dpr"; Extra = @() },
    @{ Path = "demuxing_decoding.dpr"; Extra = @() },
    @{ Path = "encode_audio.dpr"; Extra = @() },
    @{ Path = "encode_video.dpr"; Extra = @() },
    @{ Path = "extract_mvs.dpr"; Extra = @() },
    @{ Path = "filtering_audio.dpr"; Extra = @() },
    @{ Path = "filtering_video.dpr"; Extra = @() },
    @{ Path = "filter_audio.dpr"; Extra = @() },
    @{ Path = "hw_decode.dpr"; Extra = @() },
    @{ Path = "metadata.dpr"; Extra = @() },
    @{ Path = "muxing.dpr"; Extra = @() },
    @{ Path = "remuxing.dpr"; Extra = @() },
    @{ Path = "resampling_audio.dpr"; Extra = @() },
    @{ Path = "scaling_video.dpr"; Extra = @() },
    @{ Path = "mediainfo\MediaInfo.dpr"; Extra = @("-U`"$RootDir\examples\mediainfo`"") },
    @{ Path = "dump-frame\dumpframe.dpr"; Extra = @("-U`"$RootDir\examples\mediainfo`"") },
    @{ Path = "ffmpeg_sample_player.dpr"; Extra = @("-U`"$RootDir\sdl`"") }
)

$SuccessCount = 0
$FailedCount = 0

foreach ($Project in $Projects) {
    $Name = Split-Path $Project.Path -Leaf
    Write-Host "Building $Name..." -NoNewline

    $Args = $CommonArgs + $Project.Extra + @("`"$((Join-Path $PSScriptRoot $Project.Path))`"")
    $Process = Start-Process $Dcc64 -ArgumentList $Args -NoNewWindow -PassThru -Wait

    if ($Process.ExitCode -eq 0) {
        Write-Host " [OK]" -ForegroundColor Green
        $SuccessCount++
    } else {
        Write-Host " [FAILED]" -ForegroundColor Red
        $FailedCount++
    }
}

Write-Host "`n=== Build finished ===" -ForegroundColor Green
$Color = if ($FailedCount -gt 0) { "Red" } else { "Green" }
Write-Host "Success: $SuccessCount, Failed: $FailedCount" -ForegroundColor $Color

if ($FailedCount -gt 0) { exit 1 }
