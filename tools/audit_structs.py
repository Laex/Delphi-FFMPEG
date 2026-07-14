#!/usr/bin/env python3
"""Compare Delphi FFmpeg binding struct field order with C headers."""
from __future__ import annotations

import re
import sys
from pathlib import Path

# Bindings target FFmpeg n8.1.1 — audit against that tag, not n8.2-dev master:
#   git -C C:\FFmpeg checkout n8.1.1
FFMPEG_ROOT = Path(r"C:\FFmpeg")
FFMPEG_TAG = "n8.1.1"
DELPHI_ROOT = Path(__file__).resolve().parents[1] / "source"

STRUCTS = [
    ("libavcodec/avcodec.h", "libavcodec.pas", "AVCodecContext"),
    ("libavcodec/avcodec.h", "libavcodec.pas", "AVCodecParameters"),
    ("libavcodec/avcodec.h", "libavcodec.pas", "AVPacket"),
    ("libavutil/frame.h", "libavutil.pas", "AVFrame"),
    ("libavformat/avformat.h", "libavformat.pas", "AVFormatContext"),
    ("libavformat/avformat.h", "libavformat.pas", "AVStream"),
]


def extract_c_struct_fields(header: Path, struct_name: str) -> list[str]:
    text = header.read_text(encoding="utf-8", errors="ignore")
    marker = f"typedef struct {struct_name} {{"
    start = text.find(marker)
    if start < 0:
        marker = f"}} {struct_name};"
        return []
    i = start + len(f"typedef struct {struct_name} {{")
    depth = 0
    buf = ""
    fields: list[str] = []
    while i < len(text):
        ch = text[i]
        if ch == "{":
            depth += 1
        elif ch == "}":
            if depth == 0:
                break
            depth -= 1
        elif depth == 0 and ch == ";":
            line = re.sub(r"/\*.*?\*/", "", buf, flags=re.S)
            line = re.sub(r"//.*", "", line).strip()
            if line and not line.startswith("#"):
                names = re.findall(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*(?:,|\[|$)", line.split("(")[0])
                if names:
                    if len(names) >= 2 and "," in line.split("(")[0]:
                        fields.extend(names[-2:])
                    else:
                        fields.append(names[-1])
            buf = ""
            i += 1
            continue
        if depth == 0:
            buf += ch
        i += 1
    return fields


def extract_pascal_record_fields(unit: Path, record_name: str) -> list[str]:
    text = unit.read_text(encoding="utf-8", errors="ignore")
    start = text.find(f"{record_name} = record")
    if start < 0:
        return []
    fields: list[str] = []
    for line in text[start:].splitlines()[1:]:
        ls = line.strip()
        if ls == "end;":
            break
        if not ls or ls.startswith("(*") or ls.startswith("//"):
            continue
        if ls.startswith("{$IFDEF") or ls.startswith("{$IFNDEF") or ls.startswith("{$ENDIF"):
            continue
        if "deprecated" in ls:
            continue
        m2 = re.match(r"([A-Za-z_][\w]*)\s*,\s*([A-Za-z_][\w]*)\s*:", ls)
        if m2:
            fields.extend([m2.group(1), m2.group(2)])
            continue
        m = re.match(r"([A-Za-z_][\w]*)\s*:", ls)
        if m:
            fields.append(m.group(1))
    return fields


def compare_struct(c_header: Path, pas_unit: Path, struct_name: str) -> list[str]:
    c_fields = extract_c_struct_fields(c_header, struct_name)
    p_fields = extract_pascal_record_fields(pas_unit, struct_name)
    issues: list[str] = []
    if not c_fields:
        issues.append(f"  C struct {struct_name} not found in {c_header}")
        return issues
    if not p_fields:
        issues.append(f"  Pascal record {struct_name} not found in {pas_unit}")
        return issues
    n = min(len(c_fields), len(p_fields))
    first_mismatch = None
    for i in range(n):
        if c_fields[i] != p_fields[i]:
            first_mismatch = i
            break
    if first_mismatch is None and len(c_fields) != len(p_fields):
        first_mismatch = n
    if first_mismatch is not None:
        issues.append(
            f"  MISMATCH at index {first_mismatch}: "
            f"C={c_fields[first_mismatch] if first_mismatch < len(c_fields) else 'EOF'} "
            f"PAS={p_fields[first_mismatch] if first_mismatch < len(p_fields) else 'EOF'}"
        )
        lo = max(0, first_mismatch - 3)
        hi = min(max(len(c_fields), len(p_fields)), first_mismatch + 8)
        for j in range(lo, hi):
            c = c_fields[j] if j < len(c_fields) else "-"
            p = p_fields[j] if j < len(p_fields) else "-"
            mark = ">>>" if j == first_mismatch else "   "
            issues.append(f"    {mark} {j:3d}: C={c:28} PAS={p:28}")
    else:
        issues.append(f"  OK ({len(c_fields)} fields)")
    issues.append(f"  Field counts: C={len(c_fields)} PAS={len(p_fields)}")
    return issues


def main() -> int:
    print(f"FFmpeg source: {FFMPEG_ROOT} (expected tag {FFMPEG_TAG})")
    print(f"Delphi source: {DELPHI_ROOT}\n")
    total_issues = 0
    for c_rel, pas_rel, struct in STRUCTS:
        c_path = FFMPEG_ROOT / c_rel
        pas_path = DELPHI_ROOT / pas_rel
        print(f"[{struct}]")
        if not c_path.exists():
            print(f"  MISSING C header: {c_path}")
            total_issues += 1
            continue
        issues = compare_struct(c_path, pas_path, struct)
        for line in issues:
            print(line)
            if "MISMATCH" in line:
                total_issues += 1
        print()
    return total_issues


if __name__ == "__main__":
    sys.exit(main())
