## Компоненты Delphi-FFMPEG

Высокоуровневые VCL/FMX-компоненты поверх Pascal-биндингов FFmpeg 8.1.x.

> **Альфа** — библиотека компонентов в ранней альфа-версии. API, поведение в design-time и стабильность могут меняться; для production нужно тщательное тестирование.

> English (primary): [../README.md](../README.md)

### Содержание

| Документ | Описание |
|----------|----------|
| [Обзор](overview.md) | Назначение, платформы, установка пакетов |
| [Архитектура](architecture.md) | Слои библиотеки, graph linking |
| [Воспроизведение](playback.md) | Engine / linked mode, A/V sync, HW decode |
| [Субтитры](subtitles.md) | Text, ASS, bitmap/PGS overlay |
| [Справочник компонентов](reference.md) | Свойства и методы по классам |
| [Тестирование](testing.md) | Smoke-тесты, медиа, CI |

### Быстрый старт

```delphi
uses uFFVideoPlayer;

Player.Parent := Self;
Player.Align := alClient;
Player.FileName := 'video.mp4';
Player.HardwareDevice := ffhdAuto;  // опционально
Player.Play;
```

Пакеты: `packages/Delphi 13 Florence/DelphiFFMPEG.groupproj` (`rtpFFMPEG` + `rtpFFMPEGComponents` + `dclFFMPEGComponents`).

Подробнее: [Обзор](overview.md).
