#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BIN="$ROOT/bin/linux64"
SRC="$ROOT/source"
TEST="$ROOT/tests/linux"

mkdir -p "$BIN"

URL="https://github.com/BtbN/FFmpeg-Builds/releases/download/latest/ffmpeg-n8.1-latest-linux64-gpl-shared-8.1.tar.xz"
ARCHIVE="$BIN/ffmpeg-linux64.tar.xz"
EXTRACT="$BIN/temp_extract"

if ! command -v fpc >/dev/null 2>&1; then
  echo "Installing fpc..."
  sudo apt-get update -qq
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y -qq fpc wget xz-utils
fi

if [ ! -f "$BIN/libavutil.so.60" ]; then
  echo "Downloading FFmpeg 8.1 shared (linux64)..."
  wget -q -O "$ARCHIVE" "$URL"
  rm -rf "$EXTRACT"
  mkdir -p "$EXTRACT"
  tar -xJf "$ARCHIVE" -C "$EXTRACT"
  LIBDIR="$(find "$EXTRACT" -type d -name lib | head -1)"
  if [ -z "$LIBDIR" ]; then
    echo "ERROR: lib/ not found in archive" >&2
    exit 1
  fi
  cp -f "$LIBDIR"/*.so* "$BIN/"
  rm -rf "$EXTRACT" "$ARCHIVE"
  echo "FFmpeg libraries installed to $BIN"
fi

echo "Building binding_test..."
fpc -Mdelphi -Fu"$SRC" -Fl"$BIN" -FE"$BIN" "$TEST/binding_test.lpr"

export LD_LIBRARY_PATH="$BIN${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
echo "Running binding_test..."
"$BIN/binding_test"
