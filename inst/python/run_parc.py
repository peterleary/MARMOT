"""Standalone PARC clustering subprocess driver.

Called by MARMOT's R side via system2(). Reads a raw float64 matrix from
--input (column-major, as R writes), runs PARC, writes integer labels as
raw int32 to --output.

Running in a separate process avoids the macOS arm64 libomp-collision
segfault that happens when R's data.table and Python's hnswlib share one
process via reticulate.
"""
import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import marmot_setup  # noqa: E402,F401

import numpy as np  # noqa: E402
import parc  # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    ap.add_argument("--output", required=True)
    ap.add_argument("--rows", type=int, required=True)
    ap.add_argument("--cols", type=int, required=True)
    ap.add_argument("--knn", type=int, default=30)
    ap.add_argument("--num-threads", type=int, default=-1)
    ap.add_argument("--random-seed", type=int, default=42)
    ap.add_argument("--resolution", type=float, default=1.0)
    args = ap.parse_args()

    X = np.fromfile(args.input, dtype=np.float64)
    if X.size != args.rows * args.cols:
        sys.exit(
            "PARC subprocess: expected %d doubles, got %d"
            % (args.rows * args.cols, X.size)
        )
    X = X.reshape((args.rows, args.cols), order="F")

    model = parc.PARC(
        X,
        knn=args.knn,
        num_threads=args.num_threads,
        random_seed=args.random_seed,
        resolution_parameter=args.resolution,
    )
    model.run_PARC()
    labels = np.asarray(model.labels, dtype=np.int32)
    labels.tofile(args.output)


if __name__ == "__main__":
    main()
