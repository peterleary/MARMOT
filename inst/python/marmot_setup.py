"""
One-time environment setup for MARMOT's Python environment.
Imported by f_parc.py and f_pacmap.py before any heavy package imports.
All functions are idempotent (safe to call multiple times).
"""
import os
import sys


def fix_rpath_libs():
    """
    On macOS, R overrides DYLD_FALLBACK_LIBRARY_PATH (omitting /usr/lib),
    which prevents llvmlite from resolving its @rpath-linked system dylibs
    (libz.1.dylib, libc++.1.dylib) when loaded inside R via reticulate.

    Fix: create symlinks to the real library files in the current Python
    env's lib directory — the first @rpath location libllvmlite.dylib searches.
    Works for conda envs (sys.prefix points to the env root).
    Must run BEFORE any import that transitively loads llvmlite (numba,
    umap-learn, parc, pacmap).
    """
    if sys.platform != "darwin":
        return
    import glob

    python_lib = os.path.join(sys.prefix, "lib")
    if not os.path.isdir(python_lib):
        return
    lib_dirs = [python_lib]

    candidates = {
        "libz.1.dylib": (
            glob.glob("/opt/homebrew/opt/zlib/lib/libz.1.dylib") +
            glob.glob("/usr/local/opt/zlib/lib/libz.1.dylib")
        ),
        "libc++.1.dylib": (
            glob.glob("/opt/homebrew/Cellar/llvm/*/lib/c++/libc++.1.dylib") +
            ["/opt/homebrew/opt/llvm/lib/c++/libc++.1.dylib",
             "/usr/local/lib/libc++.1.dylib"]
        ),
    }

    for lib_dir in lib_dirs:
        for name, sources in candidates.items():
            dest = os.path.join(lib_dir, name)
            if os.path.exists(dest) or os.path.islink(dest):
                continue
            for src in sources:
                if os.path.exists(src):
                    try:
                        os.symlink(src, dest)
                    except OSError:
                        pass
                    break


def ensure_annoy_patched():
    """
    Fix broken annoy on macOS arm64 (spotify/annoy#682).

    All released annoy wheels (PyPI) are compiled with -ffast-math, which
    corrupts priority queue comparisons inside get_nns_by_vector, causing
    it to return only 1 neighbour regardless of k. The fix is in PR #690
    (BabaSanfour fork, commit 3b558eb): remove -ffast-math and pack Node
    structs to avoid SIGBUS on ARM64.

    Detection: PyPI wheel installs do NOT write direct_url.json to the
    dist-info directory; git/VCS installs DO. Absence of that file means
    we have a broken wheel. A marker file prevents repeat runs.

    Must run BEFORE import pacmap (which imports annoy at the top level).
    """
    if sys.platform != "darwin":
        return
    import glob
    import sysconfig
    import subprocess

    sp = sysconfig.get_paths()["purelib"]
    marker = os.path.join(sp, ".marmot_annoy_ok")
    if os.path.exists(marker):
        return

    already_patched = bool(
        glob.glob(os.path.join(sp, "annoy-*.dist-info", "direct_url.json"))
    )
    if not already_patched:
        print("Patching annoy for macOS arm64 compatibility (one-time, ~30s) ...",
              flush=True)
        ret = subprocess.call(
            [sys.executable, "-m", "pip", "install", "-q", "--force-reinstall",
             "git+https://github.com/BabaSanfour/annoy.git"
             "@3b558eb6763b058b78d0c78a313e59b2dbad08d3"],
            timeout=300,
        )
        if ret != 0:
            print(
                "Warning: annoy patch failed (exit %d). PaCMAP may not work correctly."
                % ret, flush=True,
            )
            return
        # Evict the broken annoy from the current session so the re-import
        # picks up the freshly compiled .so
        for key in list(sys.modules.keys()):
            if "annoy" in key:
                del sys.modules[key]

    try:
        open(marker, "w").close()
    except OSError:
        pass


# Run setup automatically on import so callers only need `import marmot_setup`
fix_rpath_libs()
ensure_annoy_patched()
