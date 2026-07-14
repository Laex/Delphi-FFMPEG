#!/usr/bin/env python3
"""Add Delphi delay-load to FFmpeg external imports (re-run after regenerating lib*.pas)."""
from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1] / "source"
UNITS = (
    "libavutil.pas",
    "libavcodec.pas",
    "libavformat.pas",
    "libavdevice.pas",
    "libavfilter.pas",
    "libswscale.pas",
    "libswresample.pas",
    "libpostproc.pas",
)
PATTERN = re.compile(r"external (\w+_dll);")
REPLACEMENT = r"external \1{$IFDEF FF_DELAYED} delayed{$ENDIF};"


def main() -> int:
    changed = 0
    for name in UNITS:
        path = ROOT / name
        text = path.read_text(encoding="utf-8")
        new_text = PATTERN.sub(REPLACEMENT, text)
        if new_text != text:
            path.write_text(new_text, encoding="utf-8", newline="\n")
            print(f"updated: {path}")
            changed += 1
    print(f"done ({changed} file(s) changed)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
