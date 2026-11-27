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

    import inspect
    import time as _real_time

    import parc._parc as _parc_module

    # --- Four repairs to PARC 0.40, all so that it finishes and repeats. ---
    # Each one is skipped if PARC no longer has the fault, so a fixed PARC is
    # left alone. Nothing here changes how PARC clusters.

    # 1. The loop that tidies small clusters after re-clustering a too-big
    #    population is written `while (a) == True & (b):`, which Python reads
    #    as `a == (True & b)`. Once no small clusters are left and the time is
    #    up, that is `False == False`, and the loop never ends. Put the
    #    brackets where they were meant to be (as PARC's other loop has them).
    _bad = "while (small_pop_exist) == True & (time.time() - time_smallpop_start < self.time_smallpop):"
    _good = "while (small_pop_exist == True) & ((time.time() - time_smallpop_start) < self.time_smallpop):"
    _source = inspect.getsource(_parc_module)
    if _bad in _source:
        exec(compile(_source.replace(_bad, _good), _parc_module.__file__, "exec"),
             _parc_module.__dict__)

    model = _parc_module.PARC(
        X,
        knn=args.knn,
        num_threads=args.num_threads,
        random_seed=args.random_seed,
        resolution_parameter=args.resolution,
    )

    # 2. When a cluster is larger than too_big_factor of the cells,
    #    run_toobig_subPARC() uses `small_pop` (and nearby `time_smallpop`) as
    #    if they were module-level names, which they are not, so it stops
    #    with a NameError. They are the object's own settings, so give the
    #    module those values.
    for name, default in (("small_pop", 10), ("time_smallpop", 15)):
        if not hasattr(_parc_module, name):
            setattr(_parc_module, name, getattr(model, name, default))

    # 3. Adding cells to an hnswlib index on several threads at once builds a
    #    slightly different index each run, so PARC did not repeat. PARC also
    #    sets the thread count only on its main index; the one for a too-big
    #    population used every core, so it varied even on one thread. So
    #    every index is built on one thread, and searched on ours (each
    #    cell's search is separate, so that part repeats on any number).
    _Index = _parc_module.hnswlib.Index
    _threads = model.num_threads

    class _IndexBuiltOnOneThread(_Index):
        def __init__(self, *a, **k):
            super().__init__(*a, **k)
            if _threads and _threads > 0:
                self.set_num_threads(_threads)

        def add_items(self, data, *a, **k):
            k["num_threads"] = 1
            return super().add_items(data, *a, **k)

    _parc_module.hnswlib.Index = _IndexBuiltOnOneThread

    # 4. PARC stops its small-cluster tidying on a clock (time_smallpop
    #    seconds), so where it stops can depend on how fast or busy the
    #    computer is. Inside PARC, the clock is replaced by a counter: each
    #    look at the time moves it on by a hundredth of time_smallpop, so the
    #    tidying stops after at most 100 rounds, the same on every computer.
    #    PARC only uses the time for these limits and for progress messages.
    class _CountingClock:
        def __init__(self, step):
            self._now = 0.0
            self._step = step

        def time(self):
            self._now += self._step
            return self._now

        def __getattr__(self, name):
            return getattr(_real_time, name)

    _parc_module.time = _CountingClock(float(model.time_smallpop) / 100.0)

    model.run_PARC()
    labels = np.asarray(model.labels, dtype=np.int32)
    labels.tofile(args.output)


if __name__ == "__main__":
    main()
