#!/usr/bin/env bash
# Run component tests on Linux with FPC
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$ROOT/source"
COMP="$ROOT/source/components"
TESTS="$ROOT/tests/components"
BIN="$ROOT/bin/linux64"
LOGDIR="$ROOT/bin/out/component_tests"
mkdir -p "$LOGDIR" "$BIN"

GUI_ONLY=(
  player_smoke_test.dpr
  linked_player_smoke_test.dpr
  fmx_player_smoke_test.dpr
  player_control_smoke_test.dpr
  thumbnail_test.dpr
  bitmap_encoder_test.dpr
  decoder_preview_test.dpr
)

MEDIA=""
for c in "$ROOT/resource/test_subs.mkv" "$ROOT/resource/test_av.mp4" "$ROOT/resource/768x576.avi"; do
  if [[ -f "$c" ]]; then MEDIA="$c"; break; fi
done

export LD_LIBRARY_PATH="$BIN:${LD_LIBRARY_PATH:-}"

pass=0
skip=0
fail=0

for test in "$TESTS"/*.dpr; do
  name="$(basename "$test")"
  log="$LOGDIR/${name%.dpr}.log"
  exe="$BIN/${name%.dpr}"

  skip_test=0
  for g in "${GUI_ONLY[@]}"; do
    if [[ "$name" == "$g" ]]; then skip_test=1; break; fi
  done
  if [[ $skip_test -eq 1 ]]; then
    echo "[SKIP] $name (GUI/VCL)"
    skip=$((skip + 1))
    continue
  fi

  echo "=== $name ==="
  if ! fpc -Mdelphi -Twin64 -Px86_64 -Fu"$SRC" -Fu"$COMP" -Fl"$BIN" -FE"$BIN" -o"$exe" "$test" >"$log" 2>&1; then
    echo "[FAIL] $name (compile)"
    fail=$((fail + 1))
    continue
  fi

  set +e
  if [[ -n "$MEDIA" ]]; then
    "$exe" "$MEDIA" >>"$log" 2>&1
  else
    "$exe" >>"$log" 2>&1
  fi
  code=$?
  set -e

  if [[ $code -eq 2 ]] || grep -q 'SKIP:' "$log"; then
    echo "[SKIP] $name"
    skip=$((skip + 1))
  elif [[ $code -eq 0 ]] && grep -q 'PASS:' "$log"; then
    echo "[PASS] $name"
    pass=$((pass + 1))
  else
    echo "[FAIL] $name (exit $code)"
    fail=$((fail + 1))
  fi
done

echo "Summary: PASS=$pass SKIP=$skip FAIL=$fail"
[[ $fail -eq 0 ]]
