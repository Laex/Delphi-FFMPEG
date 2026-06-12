# PowerShell скрипт для скачивания DLL FFmpeg 8.1.x (x64) в папку bin

$ProgressPreference = 'SilentlyContinue' # Отключаем медленный GUI-прогрессбар для ускорения скачивания

# URL-адрес стабильной shared-сборки FFmpeg 8.1.x от BtbN
$Url = "https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-n8.1-latest-win64-gpl-shared-8.1.zip"
$ZipPath = Join-Path $PSScriptRoot "ffmpeg.zip"
$ExtractPath = Join-Path $PSScriptRoot "temp_extract"

Write-Host "=== Скачивание FFmpeg 8.1.x Shared (Win64) ===" -ForegroundColor Green
Write-Host "URL: $Url" -ForegroundColor Gray

try {
    # Скачиваем архив
    Write-Host "Загрузка архива..." -NoNewline
    Invoke-WebRequest -Uri $Url -OutFile $ZipPath -UseBasicParsing
    Write-Host " [ГОТОВО]" -ForegroundColor Green

    # Распаковываем
    Write-Host "Распаковка..." -NoNewline
    if (Test-Path $ExtractPath) { Remove-Item $ExtractPath -Recurse -Force }
    Expand-Archive -Path $ZipPath -DestinationPath $ExtractPath
    Write-Host " [ГОТОВО]" -ForegroundColor Green

    # Находим папку bin внутри распакованного архива
    $DllFolder = Get-ChildItem -Path $ExtractPath -Directory | Select-Object -First 1
    $DllSourcePath = Join-Path $DllFolder.FullName "bin"

    # Копируем DLL-файлы в текущую папку (bin/)
    Write-Host "Копирование библиотек..." -NoNewline
    $Dlls = Get-ChildItem -Path $DllSourcePath -Filter *.dll
    foreach ($Dll in $Dlls) {
        Copy-Item $Dll.FullName $PSScriptRoot -Force
    }
    Write-Host " [ГОТОВО]" -ForegroundColor Green

    Write-Host "`nУспешно скопированы библиотеки:" -ForegroundColor Green
    $Dlls | ForEach-Object { Write-Host " - $($_.Name)" -ForegroundColor Yellow }

} catch {
    Write-Host "`n[ОШИБКА] Не удалось скачать или распаковать библиотеки FFmpeg: $_" -ForegroundColor Red
} finally {
    # Очистка временных файлов
    Write-Host "`nОчистка временных файлов..." -NoNewline
    if (Test-Path $ZipPath) { Remove-Item $ZipPath -Force }
    if (Test-Path $ExtractPath) { Remove-Item $ExtractPath -Recurse -Force }
    Write-Host " [ГОТОВО]" -ForegroundColor Green
}
