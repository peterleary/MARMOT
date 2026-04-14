"""Standalone PaCMAP subprocess driver.

Called by MARMOT's R side via system2(). Reads a raw float64 matrix from
--input (column-major, as R writes), runs pacmap_fit, writes the embedding
as raw float64 (row-major) to --output.

Running in a separate process avoids the macOS arm64 libomp-collision
segfault that happens when R's data.table and Python's annoy share one
process via reticulate.
"""
import argparse
import os
import sys

# sys.path priming so `import marmot_setup` works regardless of cwd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import marmot_setup  # noqa: E402,F401  (rpath libs + annoy patch on macOS)

import numpy as np  # noqa: E402
import pacmap  # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    ap.add_argument("--output", required=True)
    ap.add_argument("--rows", type=int, required=True)
    ap.add_argument("--cols", type=int, required=True)
    ap.add_argument("--n-components", type=int, default=2)
    ap.add_argument("--n-neighbors", type=int, default=10)
    ap.add_argument("--mn-ratio", type=float, default=0.5)
    ap.add_argument("--fp-ratio", type=float, default=2.0)
    ap.add_argument("--distance", default="euclidean")
    ap.add_argument("--lr", type=float, default=1.0)
    ap.add_argument("--num-iters", type=int, default=450)
    ap.add_argument("--apply-pca", action="store_true")
    ap.add_argument("--random-state", type=int, default=None)
    ap.add_argument("--verbose", action="store_true")
    args = ap.parse_args()

    # R writes doubles in column-major (Fortran) order
    X = np.fromfile(args.input, dtype=np.float64)
    if X.size != args.rows * args.cols:
        sys.exit(
            "PaCMAP subprocess: expected %d doubles, got %d"
            % (args.rows * args.cols, X.size)
        )
    X = X.reshape((args.rows, args.cols), order="F")

    model = pacmap.PaCMAP(
        n_components=args.n_components,
        n_neighbors=args.n_neighbors,
        MN_ratio=args.mn_ratio,
        FP_ratio=args.fp_ratio,
        distance=args.distance,
        lr=args.lr,
        num_iters=args.num_iters,
        apply_pca=args.apply_pca,
        random_state=args.random_state,
        verbose=args.verbose,
    )
    emb = model.fit_transform(X)

    # Write row-major float64; R reads back with byrow=TRUE
    np.ascontiguousarray(emb, dtype=np.float64).tofile(args.output)


if __name__ == "__main__":
    main()
