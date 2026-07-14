#!/usr/bin/env python3
"""Generate Delphi component palette icons (24x24 BMP) and FFmpegComponents.dcr."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

from PIL import Image, ImageDraw

ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "packages" / "Delphi 13 Florence" / "icons"
RC_PATH = ROOT / "packages" / "Delphi 13 Florence" / "FFmpegComponents.rc"
DCR_PATH = ROOT / "packages" / "Delphi 13 Florence" / "FFmpegComponents.dcr"
CGRC = Path(r"c:\program files (x86)\embarcadero\studio\37.0\bin\cgrc.exe")
BRCC32 = Path(r"c:\program files (x86)\embarcadero\studio\37.0\bin\brcc32.exe")

SIZE = 16
BG = (26, 26, 46)
WHITE = (235, 235, 240)
GRAY = (110, 110, 125)
RED = (233, 69, 96)
BLUE = (52, 120, 220)
GREEN = (34, 180, 90)
AMBER = (245, 166, 35)
TEAL = (45, 190, 175)
PURPLE = (160, 95, 220)


def new_canvas() -> tuple[Image.Image, ImageDraw.ImageDraw]:
    img = Image.new("RGB", (SIZE, SIZE), BG)
    return img, ImageDraw.Draw(img)


def save_icon(name: str, img: Image.Image) -> Path:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    path = OUT_DIR / f"{name.lower()}.bmp"
    # Delphi palette icons: 16x16, 16-color indexed BMP.
    img.resize((SIZE, SIZE), Image.Resampling.LANCZOS).convert(
        "P", palette=Image.ADAPTIVE, colors=16
    ).save(path, format="BMP")
    return path


def draw_loader(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rounded_rectangle((5, 6, 18, 17), radius=2, outline=BLUE, width=2)
    draw.polygon([(9, 14), (13, 10), (13, 18)], fill=GREEN)
    draw.rectangle((14, 4, 16, 6), fill=AMBER)


def draw_logger(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rectangle((6, 5, 17, 18), outline=GRAY, width=1)
    for y, c in ((8, WHITE), (11, AMBER), (14, TEAL)):
        draw.line((8, y, 15, y), fill=c, width=2)


def draw_reader(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rounded_rectangle((4, 7, 19, 18), radius=2, outline=BLUE, width=2)
    for x in (8, 11, 14):
        draw.rectangle((x, 9, x + 1, 16), fill=RED)


def draw_decoder(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rectangle((4, 8, 11, 16), outline=GRAY, width=1)
    draw.polygon([(12, 12), (18, 8), (18, 16)], fill=GREEN)
    draw.line((11, 12, 12, 12), fill=GREEN, width=2)


def draw_encoder(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.polygon([(6, 8), (12, 12), (6, 16)], fill=AMBER)
    draw.rectangle((13, 8, 19, 16), outline=RED, width=2)
    for x in (15, 17):
        draw.line((x, 10, x, 14), fill=WHITE, width=1)


def draw_writer(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rounded_rectangle((5, 6, 16, 17), radius=2, outline=TEAL, width=2)
    draw.rectangle((14, 9, 19, 15), fill=BLUE, outline=WHITE, width=1)
    draw.polygon([(16, 11), (18, 12), (16, 13)], fill=WHITE)


def draw_memory_adapter(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rectangle((5, 8, 18, 16), outline=AMBER, width=2)
    for x in (7, 10, 13, 16):
        draw.line((x, 16, x, 18), fill=AMBER, width=1)
    draw.line((8, 12, 15, 12), fill=WHITE, width=2)


def draw_bitmap_encoder(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    colors = (RED, GREEN, BLUE, AMBER)
    i = 0
    for y in (7, 12):
        for x in (6, 11):
            draw.rectangle((x, y, x + 3, y + 3), fill=colors[i])
            i += 1
    draw.polygon([(16, 12), (20, 9), (20, 15)], fill=TEAL)


def draw_media_info(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.ellipse((5, 5, 17, 17), outline=BLUE, width=2)
    draw.text((10, 7), "i", fill=WHITE)
    draw.arc((6, 14, 18, 20), start=20, end=160, fill=TEAL, width=2)


def draw_thumbnail(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rectangle((4, 8, 14, 18), outline=GRAY, width=1)
    draw.rectangle((6, 10, 12, 14), fill=BLUE)
    draw.rectangle((14, 6, 20, 12), outline=AMBER, width=2)
    draw.line((15, 11, 19, 7), fill=AMBER, width=1)


def draw_transcode_job(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.ellipse((5, 5, 13, 13), outline=GRAY, width=1)
    draw.ellipse((11, 11, 19, 19), outline=GRAY, width=1)
    draw.polygon([(10, 12), (14, 10), (14, 14)], fill=GREEN)
    draw.polygon([(14, 12), (10, 14), (10, 10)], fill=RED)


def draw_remux_job(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rounded_rectangle((4, 9, 10, 15), radius=1, outline=BLUE, width=1)
    draw.rounded_rectangle((14, 9, 20, 15), radius=1, outline=BLUE, width=1)
    draw.polygon([(11, 10), (13, 12), (11, 14)], fill=AMBER)
    draw.polygon([(13, 10), (11, 12), (13, 14)], fill=TEAL)


def draw_frame_filter(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.polygon([(12, 4), (20, 9), (20, 15), (12, 20), (4, 15), (4, 9)], outline=PURPLE, width=2)
    draw.line((8, 12, 16, 12), fill=WHITE, width=2)


def draw_subtitle_decoder(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rounded_rectangle((4, 13, 20, 18), radius=1, fill=(20, 20, 35), outline=WHITE, width=1)
    draw.text((6, 5), "CC", fill=AMBER)
    draw.line((6, 15, 18, 15), fill=WHITE, width=1)
    draw.line((6, 17, 14, 17), fill=GRAY, width=1)


def draw_fmx_player(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rounded_rectangle((4, 6, 19, 17), radius=2, outline=RED, width=2)
    draw.polygon([(10, 9), (16, 12), (10, 15)], fill=GREEN)


def draw_video_player(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rectangle((4, 6, 19, 17), outline=BLUE, width=2)
    draw.polygon([(10, 9), (16, 12), (10, 15)], fill=AMBER)


def draw_player_control(img: Image.Image, draw: ImageDraw.ImageDraw) -> None:
    draw.rectangle((4, 10, 6, 14), fill=WHITE)
    draw.polygon([(9, 9), (9, 15), (14, 12)], fill=GREEN)
    draw.rectangle((16, 10, 18, 14), fill=WHITE)
    draw.polygon([(12, 5), (14, 7), (10, 7)], fill=RED)


# Delphi matches icon resource names to component class names (with leading 'T').
ICON_SPECS: list[tuple[str, callable]] = [
    ("TFFLOADER", draw_loader),
    ("TFFLOGGER", draw_logger),
    ("TFFREADER", draw_reader),
    ("TFFDECODER", draw_decoder),
    ("TFFENCODER", draw_encoder),
    ("TFFWRITER", draw_writer),
    ("TFFMEMORYACCESSADAPTER", draw_memory_adapter),
    ("TFFBITMAPENCODER", draw_bitmap_encoder),
    ("TFFMEDIAINFO", draw_media_info),
    ("TFFTHUMBNAILEXTRACTOR", draw_thumbnail),
    ("TFFTRANSCODEJOB", draw_transcode_job),
    ("TFFREMUXJOB", draw_remux_job),
    ("TFFFRAMEFILTER", draw_frame_filter),
    ("TFFSUBTITLEDECODER", draw_subtitle_decoder),
    ("TFFFMXVIDEOPLAYER", draw_fmx_player),
    ("TFFVIDEOPLAYER", draw_video_player),
    ("TFFPLAYERCONTROL", draw_player_control),
]


def write_rc(icon_names: list[str]) -> None:
    lines = ["// Auto-generated by tools/build_component_icons.py", ""]
    for name in icon_names:
        bmp = f"icons\\{name.lower()}.bmp"
        lines.append(f"{name} BITMAP \"{bmp}\"")
    lines.append("")
    RC_PATH.write_text("\r\n".join(lines), encoding="ascii")


def compile_dcr() -> None:
    if BRCC32.is_file():
        cmd = [str(BRCC32), f"-fo{DCR_PATH}", str(RC_PATH)]
        subprocess.run(cmd, check=True, cwd=RC_PATH.parent)
        return
    if not CGRC.is_file():
        raise SystemExit("Neither brcc32.exe nor cgrc.exe found in Delphi bin folder")
    cmd = [str(CGRC), "-c65001", f"-fo{DCR_PATH.with_suffix('.res')}", str(RC_PATH)]
    subprocess.run(cmd, check=True, cwd=RC_PATH.parent)
    res_path = DCR_PATH.with_suffix(".res")
    if res_path.exists():
        res_path.replace(DCR_PATH)


def main() -> int:
    icon_names: list[str] = []
    for name, drawer in ICON_SPECS:
        img, draw = new_canvas()
        drawer(img, draw)
        save_icon(name, img)
        icon_names.append(name)
        print(f"  {name}")

    write_rc(icon_names)
    compile_dcr()
    print(f"Wrote {DCR_PATH}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
