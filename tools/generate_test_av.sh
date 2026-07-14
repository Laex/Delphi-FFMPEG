#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/resource/test_av.mp4"
mkdir -p "$ROOT/resource"
if ! command -v ffmpeg >/dev/null 2>&1; then
  echo 'SKIP: ffmpeg not found'
  exit 2
fi
ffmpeg -y -f lavfi -i 'testsrc=duration=6:size=320x240:rate=25' \
  -f lavfi -i 'sine=frequency=880:duration=6' \
  -pix_fmt yuv420p -c:v libx264 -c:a aac -shortest "$OUT"
echo "Created $OUT"
