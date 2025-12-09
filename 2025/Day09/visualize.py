import sys
import re
import argparse
from typing import List, Tuple

#!/usr/bin/env python3
"""
visualize.py

Read a text file (or stdin) where each line is a tuple ``x,y`` (integers).
This script can downscale coordinates for faster computation, optionally fill
an axis-aligned rectangle, then produce an image. The final image can be
upscaled (nearest-neighbor) for viewing.

Features:
- --down-scale N: integer divisor applied to input coordinates before drawing
- --up-scale M: integer multiplier applied to the finished image (nearest-neighbor)
- --rect x0,y0,x1,y1: fill a rectangle (original coordinate space)
- --padding N: add N pixels of empty border around the logical image
- --no-lines: disable drawing green connecting lines

Output: PNG via Pillow when available, otherwise ASCII PPM (P3).
"""

TUPLE_RE = re.compile(r'^\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*$')

RECT_RE = re.compile(r'^\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*$')


def parse_lines(lines: List[str]) -> List[Tuple[int, int]]:
    out: List[Tuple[int, int]] = []
    for i, raw in enumerate(lines, start=1):
        line = raw.rstrip('\n')
        if line == '':
            raise ValueError(f'Line {i}: empty line (expected one tuple per line)')
        m = TUPLE_RE.match(line)
        if not m:
            raise ValueError(f'Line {i}: invalid format: {line!r} (expected "int,int")')
        a_s, b_s = m.group(1), m.group(2)
        a, b = int(a_s), int(b_s)
        out.append((a, b))
    return out


def bresenham(x0: int, y0: int, x1: int, y1: int):
    """Yield integer points on a line from (x0,y0) to (x1,y1) using Bresenham."""
    dx = abs(x1 - x0)
    sx = 1 if x0 < x1 else -1
    dy = -abs(y1 - y0)
    sy = 1 if y0 < y1 else -1
    err = dx + dy
    x, y = x0, y0
    while True:
        yield x, y
        if x == x1 and y == y1:
            break
        e2 = 2 * err
        if e2 >= dy:
            err += dy
            x += sx
        if e2 <= dx:
            err += dx
            y += sy


def _write_ppm_with_lines(path: str, width: int, height: int, points_set, lines_set, rect_set=None, bg=(0,0,0), p_color=(255,0,0), l_color=(0,255,0), rect_color=(128,0,128), upscale: int = 1):
    with open(path, 'w', encoding='utf-8') as f:
        out_w = width * max(1, upscale)
        out_h = height * max(1, upscale)
        f.write('P3\n')
        f.write(f'{out_w} {out_h}\n')
        f.write('255\n')
        for y in range(height):
            # build expanded row for this y (repeat each pixel `upscale` times horizontally)
            row = []
            for x in range(width):
                if (x, y) in points_set:
                    color = p_color
                elif (x, y) in lines_set:
                    color = l_color
                elif rect_set and (x, y) in rect_set:
                    color = rect_color
                else:
                    color = bg
                for _ in range(max(1, upscale)):
                    row.extend(map(str, color))
            line_str = ' '.join(row) + '\n'
            # write this expanded row `upscale` times vertically
            for _ in range(max(1, upscale)):
                f.write(line_str)


def main(argv: List[str]):
    p = argparse.ArgumentParser(description='Create image from x,y tuples')
    p.add_argument('input', nargs='?', help='Input file (default stdin)')
    p.add_argument('-o', '--output', default='coords.png', help='Output filename')
    p.add_argument('--down-scale', type=int, default=1, help='Downscale divisor (integer, default 1)')
    p.add_argument('--up-scale', type=int, default=1, help='Upscale multiplier (integer, default 1). Can be used together with --down-scale')
    p.add_argument('--padding', type=int, default=8, help='Padding in output pixels to add around the image (default 8)')
    p.add_argument('--rect', help='Optional rectangle to fill: x0,y0,x1,y1 (original coordinates)')
    p.add_argument('--no-lines', action='store_true', help='Disable drawing green connecting lines')
    args = p.parse_args(argv[1:])

    if args.input:
        with open(args.input, 'r', encoding='utf-8') as f:
            src = f.readlines()
    else:
        src = sys.stdin.readlines()

    try:
        tuples = parse_lines(src)
    except ValueError as e:
        print(f'Error: {e}', file=sys.stderr)
        sys.exit(2)

    if not tuples:
        print('No coordinates to draw.', file=sys.stderr)
        sys.exit(3)

    # Try to use Pillow to write a PNG, otherwise fall back to ASCII PPM
    try:
        from PIL import Image, ImageDraw
        use_pillow = True
    except Exception:
        Image = None
        ImageDraw = None
        use_pillow = False

    xs = [c[0] for c in tuples]
    ys = [c[1] for c in tuples]
    minx, maxx = min(xs), max(xs)
    miny, maxy = min(ys), max(ys)

    out_path = args.output

    # Normalize points to non-negative grid
    norm_points = [(x - minx, y - miny) for x, y in tuples]

    # Apply downscaling for computation (reduces number of pixels to process)
    divisor = max(1, int(args.down_scale))
    scaled_points = [((x) // divisor, (y) // divisor) for x, y in norm_points]
    # Logical width/height after downscaling (before final upscale)
    width = ((maxx - minx) // divisor) + 1
    height = ((maxy - miny) // divisor) + 1
    # Final upscale factor to apply to resulting image for viewing
    upscale_factor = max(1, int(args.up_scale))

    # Apply padding: expand canvas and offset coordinates by padding pixels
    pad = max(0, int(args.padding))
    if pad:
        width = width + pad * 2
        height = height + pad * 2
        scaled_points = [(x + pad, y + pad) for x, y in scaled_points]

    # Optional rectangle fill (in original coordinates). Downscale and normalize.
    rect_set = set()
    if args.rect:
        m = RECT_RE.match(args.rect)
        if not m:
            print('Error: --rect must be in format x0,y0,x1,y1', file=sys.stderr)
            sys.exit(4)
        rx0, ry0, rx1, ry1 = int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4))
        # Normalize rectangle corners
        rminx, rmaxx = min(rx0, rx1), max(rx0, rx1)
        rminy, rmaxy = min(ry0, ry1), max(ry0, ry1)
        # Convert to normalized coords (relative to minx/miny of input)
        rminx_rel = rminx - minx
        rminy_rel = rminy - miny
        rmaxx_rel = rmaxx - minx
        rmaxy_rel = rmaxy - miny
        # Always map rectangle into downscaled/logical coordinates; final upscale
        # will be applied to the finished image.
        sx0 = rminx_rel // divisor
        sy0 = rminy_rel // divisor
        sx1 = rmaxx_rel // divisor
        sy1 = rmaxy_rel // divisor
        # shift by padding
        sx0 += pad
        sy0 += pad
        sx1 += pad
        sy1 += pad
        # Clamp to image bounds
        sx0 = max(0, sx0)
        sy0 = max(0, sy0)
        sx1 = min(width - 1, sx1)
        sy1 = min(height - 1, sy1)
        if sx0 <= sx1 and sy0 <= sy1:
            for yy in range(sy0, sy1 + 1):
                for xx in range(sx0, sx1 + 1):
                    rect_set.add((xx, yy))

    # Build set of line pixels between consecutive downscaled points (and close the loop)
    draw_lines = not args.no_lines
    line_pts = set()
    if draw_lines and len(scaled_points) >= 2:
        for (x0, y0), (x1, y1) in zip(scaled_points, scaled_points[1:]):
            for px, py in bresenham(x0, y0, x1, y1):
                line_pts.add((px, py))
        # connect last -> first to close the polygon
        x0, y0 = scaled_points[-1]
        x1, y1 = scaled_points[0]
        for px, py in bresenham(x0, y0, x1, y1):
            line_pts.add((px, py))

    point_set = set(scaled_points)

    if use_pillow:
        # Create a base image at the logical (pre-upscale) size, draw pixels there,
        # then resize using nearest-neighbor to produce the upscaled output.
        base_w, base_h = width, height
        base_img = Image.new('RGB', (base_w, base_h), (0,0,0))
        draw = ImageDraw.Draw(base_img)
        # Draw rectangle fill first (only where unset): we'll render rectangle color
        # but ensure points/lines take precedence by drawing them later; however we
        # avoid drawing rectangle pixels that would be occupied by points/lines.
        if rect_set:
            for x, y in rect_set:
                if (x, y) in point_set or (x, y) in line_pts:
                    continue
                draw.point((x, y), fill=(128,0,128))
        # Draw lines in green connecting downscaled points onto base image.
        if draw_lines and len(scaled_points) >= 2:
            line_coords = []
            for x, y in scaled_points:
                line_coords.append((x, y))
            # close the loop by appending the first coordinate again
            line_coords.append((scaled_points[0][0], scaled_points[0][1]))
            draw.line(line_coords, fill=(0,255,0), width=1)
        # Draw points in red (single logical pixel)
        for x, y in scaled_points:
            draw.point((x, y), fill=(255,0,0))

        # Now upscale the base image if requested using nearest-neighbor (resize)
        if upscale_factor > 1:
            out_img = base_img.resize((base_w * upscale_factor, base_h * upscale_factor), resample=Image.NEAREST)
        else:
            out_img = base_img
        out_img.save(out_path)
        print(f'Saved {out_path} ({out_img.width}x{out_img.height})')
    else:
        # If the user asked for .png but Pillow isn't available, still write PPM
        if out_path.lower().endswith('.png'):
            out_path = out_path[:-4] + '.ppm'
        # For PPM, draw rectangle pixels only where unset by points/lines
        if rect_set:
            # subtract any pixels that would be taken by points/lines
            rect_set = {p for p in rect_set if p not in point_set and p not in line_pts}
        _write_ppm_with_lines(out_path, width, height, point_set, line_pts, rect_set=rect_set, upscale=upscale_factor)
        print(f'Pillow not available; wrote ASCII PPM to {out_path}')


if __name__ == '__main__':
    main(sys.argv)
