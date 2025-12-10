#!/usr/bin/env python3
"""Create a file per line and run a command on each file in parallel.

Usage examples:
  python3 run_per_line.py --input input.txt --cmd "python3 check-stuck.py {file}" --jobs 8
  python3 run_per_line.py --input input.txt --cmd "python3 check-stuck.py" --stdin

The command template may include '{file}' to substitute the per-line filename.
If '--stdin' is provided and '{file}' is not in the template, the file contents are piped to the process stdin.
"""
from __future__ import annotations

import argparse
import concurrent.futures
import os
import shutil
import subprocess
import sys
from pathlib import Path
import time
from typing import List, Tuple


def split_lines_to_files(input_path: Path, out_dir: Path, prefix: str = "line_") -> List[Path]:
    out_dir.mkdir(parents=True, exist_ok=True)
    files: List[Path] = []
    with input_path.open("r", encoding="utf-8") as f:
        for i, line in enumerate(f, start=1):
            name = f"{prefix}{i:04d}.txt"
            p = out_dir / name
            # Keep original line endings and content as-is
            p.write_text(line, encoding="utf-8")
            files.append(p)
    return files


def run_cmd_on_file(cmd_template: str, file_path: Path, use_stdin: bool) -> Tuple[int, str, str]:
    # Replace {file} if present
    if "{file}" in cmd_template:
        cmd = cmd_template.format(file=str(file_path))
        proc = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    else:
        # If template does not include {file} and use_stdin is True, pipe file to stdin
        if use_stdin:
            with file_path.open("rb") as fh:
                proc = subprocess.run(cmd_template, shell=True, stdin=fh, capture_output=True)
                # subprocess.run with binary stdin -> bytes stdout/stderr
                try:
                    out = proc.stdout.decode() if isinstance(proc.stdout, (bytes, bytearray)) else proc.stdout
                except Exception:
                    out = str(proc.stdout)
                try:
                    err = proc.stderr.decode() if isinstance(proc.stderr, (bytes, bytearray)) else proc.stderr
                except Exception:
                    err = str(proc.stderr)
                return proc.returncode, out, err
        else:
            proc = subprocess.run(cmd_template, shell=True, capture_output=True, text=True)

    out = proc.stdout if proc.stdout is not None else ""
    err = proc.stderr if proc.stderr is not None else ""
    return proc.returncode, out, err


def main(argv: List[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Run a command on a per-line file in parallel and report completion with line number.")
    parser.add_argument("--input", "-i", required=True, help="Input file with one case per line")
    parser.add_argument("--cmd", "-c", required=True, help="Command template to run for each file. Use '{file}' to substitute filename.")
    parser.add_argument("--jobs", "-j", type=int, default=0, help="Number of parallel jobs (0 = cpu count)")
    parser.add_argument("--out-dir", "-o", default="per_line_tmp", help="Directory to write per-line files")
    parser.add_argument("--prefix", default="line_", help="Prefix for per-line filenames")
    parser.add_argument("--keep-files", action="store_true", help="Keep the per-line files after run (default: remove)")
    parser.add_argument("--stdin", action="store_true", help="Pipe each per-line file to the process stdin if the command template doesn't include '{file}'")
    args = parser.parse_args(argv)

    input_path = Path(args.input)
    if not input_path.exists():
        print(f"ERROR: input file '{input_path}' not found", file=sys.stderr)
        return 2

    out_dir = Path(args.out_dir)
    files = split_lines_to_files(input_path, out_dir, prefix=args.prefix)
    if not files:
        print("No lines found in input file. Nothing to run.")
        return 0

    jobs = args.jobs if args.jobs > 0 else os.cpu_count() or 1

    # Map file to its line number index
    file_index = {str(p): i + 1 for i, p in enumerate(files)}

    print(f"Launched {len(files)} jobs with concurrency={jobs} (out_dir={out_dir})")

    def _task(p: Path):
        start = time.perf_counter()
        rc, out, err = run_cmd_on_file(args.cmd, p, args.stdin)
        elapsed = time.perf_counter() - start
        idx = file_index[str(p)]
        # Truncate outputs for terminal readability
        out_snip = (out[:500] + "...") if len(out) > 500 else out
        err_snip = (err[:500] + "...") if len(err) > 500 else err
        # Use a single-line summary including runtime
        summary = f"Line {idx} finished: rc={rc} time={elapsed:.3f}s"
        return idx, rc, summary, out_snip, err_snip

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as exe:
        futures = {exe.submit(_task, p): p for p in files}
        for fut in concurrent.futures.as_completed(futures):
            try:
                idx, rc, summary, out_snip, err_snip = fut.result()
                print('----------------------------------------')
            except Exception as e:
                p = futures[fut]
                idx = file_index[str(p)]
                print(f"Line {idx} finished: exception: {e}")
                continue

            print(summary)
            if out_snip:
                print(f"--- stdout (line {idx}) ---")
                print(out_snip)
            if err_snip:
                print(f"--- stderr (line {idx}) ---", file=sys.stderr)
                print(err_snip, file=sys.stderr)

    if not args.keep_files:
        try:
            shutil.rmtree(out_dir)
        except Exception:
            pass

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
