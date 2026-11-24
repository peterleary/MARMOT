# Setup

How to get MARMOT installed and ready to run. There are two paths — pick the one that fits the user.

## Two paths

1. **Tauri desktop app (`MARMOT.app`)** — recommended for users who don't want to touch a terminal. Has a graphical install panel that handles R and the package for them.
2. **R-based install** — for users already comfortable with R / RStudio.

Always ask which one they're trying to use before walking them through commands.

## Path A — Tauri desktop app

1. Go to <https://github.com/peterleary/MARMOT/releases/latest>.
2. Download the file matching their OS:
   - **macOS (Apple Silicon or Intel)**: `MARMOT_*_aarch64.dmg` (Intel Macs run it via Rosetta — same performance for a webview wrapper).
   - **Windows**: `MARMOT_*_x64-setup.exe`.
   - **Linux x86**: `MARMOT_*_amd64.AppImage`.
3. Open the app. The first screen is the **Install panel** — it checks for R and the MARMOT package, and offers to install whatever's missing.
4. Once the install panel reports everything green, switch to the **Setup** tab to fill in metadata (see `metadata-sheet.md`) and the **Run** tab to launch the pipeline.

If the install panel fails, ask which step it failed on and check `troubleshooting.md`.

### First-launch warnings (unsigned app)

MARMOT.app is not signed by Apple / Microsoft as a verified developer, so the OS will show a scary-looking warning the first time the user tries to open it. This is a one-time hurdle.

> 💡 The MARMOT wiki has annotated screenshots for both walkthroughs:
> - macOS: <https://github.com/peterleary/MARMOT/wiki/Opening-the-MARMOT-App-on-macOS>
> - Windows: <https://github.com/peterleary/MARMOT/wiki/Opening-the-MARMOT-App-on-Windows>
>
> Point users there if they want to see exactly which dialog to click.

**macOS — Gatekeeper "cannot be verified":**

1. Double-click `MARMOT.app`. macOS shows a warning that it "cannot be verified". Click **Done** (or **Cancel**) to dismiss.
2. Open **System Settings → Privacy & Security**.
3. Scroll to the **Security** section. There's a line about MARMOT being blocked, with an **Open Anyway** button next to it. Click it.
4. Confirm with **Open Anyway** in the popup; enter password if prompted.

After this once-off acknowledgement, MARMOT opens normally for all future launches.

**Windows — SmartScreen "Windows protected your PC":**

1. Double-click the downloaded installer. SmartScreen says *"Windows protected your PC"*.
2. Click **More info**.
3. Click **Run anyway**.

The installer then proceeds normally.

If a corporate-managed machine has SmartScreen / Gatekeeper hardened beyond user override, the user needs to contact IT — there's nothing MARMOT can do about that.

## Path B — Install in R yourself

Prerequisites:

- **R ≥ 4.5** (download from <https://cran.r-project.org/>).
- **RStudio** (optional but recommended): <https://posit.co/download/rstudio-desktop/>.
- **Quarto** (for rendering reports): <https://quarto.org/docs/get-started/>. RStudio and Positron include a copy, and MARMOT finds it itself.
- No compiler and no conda are needed: everything comes ready-built.

Then in R:

```r
# 1. Install MARMOT itself (ready-built, from MARMOT's r-universe)
install.packages("BiocManager")
install.packages("MARMOT", repos = c("https://peterleary.r-universe.dev", BiocManager::repositories()))

# 2. Install everything MARMOT can use, including the original methods
MARMOT::install_marmot_extras()

# 3. Check what works
MARMOT::check_setup()
```

`install_marmot_extras()` reads MARMOT's own DESCRIPTION, so it never misses a package, and installs ready-built copies only (it never compiles). Packages come from MARMOT's r-universe (ready-built MARMOT, Rphenograph and the GitHub-only packages), CRAN and Bioconductor. With `include_python = TRUE` (the default) it also sets up the original Python PARC and PaCMAP with **pixi**, from a lock file shipped with MARMOT: no conda, no compiler, the same versions for everyone. If pixi isn't installed, MARMOT fetches it into its own data folder.

`check_setup()` says in plain words what works and gives the one command that fixes each gap.

## Path C — Docker / Apptainer (servers and HPC)

For users on a server, an HPC cluster, or anyone who explicitly wants a containerised setup. Same image, two runtimes.

### Docker

```bash
docker pull ghcr.io/peterleary/marmot:latest
```

Image is ~11.8 GB uncompressed (~4–5 GB on the registry). Run with:

```bash
docker run --rm -v /path/to/data:/data ghcr.io/peterleary/marmot:latest \
  Rscript -e 'MARMOT::marmot("/data/MARMOT_Metadata.xlsx", name="MyRun", render=TRUE)'
```

### Apptainer / Singularity (HPC)

Most university clusters don't allow Docker but do support [Apptainer](https://apptainer.org/) (formerly Singularity). Apptainer pulls the Docker image directly — no root needed:

```bash
# Pull once (creates a ~5 GB .sif file)
apptainer pull docker://ghcr.io/peterleary/marmot:latest
```

Run a pipeline:

```bash
apptainer run --bind /path/to/data:/data marmot_latest.sif \
  Rscript -e 'MARMOT::marmot("/data/MARMOT_Metadata.xlsx", name="MyRun", render=TRUE)'
```

Or shell in interactively:

```bash
apptainer shell --bind /path/to/data:/data marmot_latest.sif
```

If the cluster only has `singularity` rather than `apptainer`, the commands are identical — substitute the binary name.

For SLURM jobs, put the `apptainer run …` line in the submission script.

### Apptainer RAM gotcha

If an Apptainer run fails during **FCS import** with an error that doesn't obviously look like an out-of-memory message, **try giving the job more RAM first** before chasing other causes — this has bitten a user with a misleading error. Bump the SLURM `--mem=` allocation (or equivalent on the cluster's scheduler) and re-run before going deeper.

## What to verify after install

Have them run, in R:

```r
library(MARMOT)
packageVersion("MARMOT")    # should be ≥ 1.3.2
```

If that loads without errors, they're ready to move on to `metadata-sheet.md`.

## Common install failures

Cross-link these in `troubleshooting.md`:

- **Python PARC/PaCMAP not set up** → run `MARMOT::install_marmot_python()`. It uses pixi (fetched automatically if missing); an existing conda is only a fallback.
- **Rphenograph missing** → it installs ready-built from MARMOT's r-universe with `install_marmot_extras()`; no compiler needed. Until then MARMOT's `Mphenograph` stands in, and the report says so.
- **Quarto not found** → MARMOT looks on the search path and inside RStudio and Positron. If none has it, install Quarto and restart R.

## M-equivalents (MARMOT's bundled fallbacks)

All of MARMOT's algorithms come from external packages that are awkward to install: they need a C++ compiler, or a working Python/conda env. So that the pipeline can always run, the marmots worked up their own **pure-R versions** of the awkward ones, prefixed with `M`. They do much the same job, but they are fall-backs: not validated or peer-reviewed, and not supported. The marmots would be much happier if users used and cited the originals.

| External (preferred) | MARMOT fallback | What it does | Why the original is awkward |
|---|---|---|---|
| `Rphenograph` | `Mphenograph` | Phenograph clustering | Rcpp/C++ — needs Rtools (Windows) or Xcode CLT (macOS) |
| `FastPG` | `MfastPG` | Faster Phenograph variant | Same — C++ compilation |
| `PARC` (Python) | `Mparc` | PARC clustering | Needs conda + working Python env |
| `PaCMAP` (Python) | `Mpacmap` | PaCMAP dimensionality reduction | Same — Python-side install |

**Why we still prefer the originals.** The external packages are the **published methods**. The M-equivalents are AI-assisted reimplementations. They have not been scientifically validated or peer-reviewed, and MARMOT offers no support for them. **Always recommend the original**, and that the user cites it:

- For Phenograph: `install_marmot_extras()` installs the original `Rphenograph` ready-built from MARMOT's r-universe.
- For PARC / PaCMAP: `install_marmot_python()` sets up the originals with pixi.

Every report's provenance tab has a "Methods, and whom to cite" box naming the implementation that ran and its authors.

**Best option if the user cannot install the originals but wants to publish.** Recommend they use **FlowSOM** for clustering and **UMAP** for dimensionality reduction instead of the M-equivalents. FlowSOM is on Bioconductor, well-maintained, peer-reviewed, and is already MARMOT's default clustering method — so no extra install. UMAP is similarly standard and built in. The biological interpretation of their results will not be meaningfully different from a Phenograph + PaCMAP run, and reviewers will have nothing to flag. To switch, set `clusteringMethodToUse = FlowSOM` and `dimRedMethodToUse = UMAP` in the `Pipeline Settings` sheet (these are the defaults, so usually no change needed).

**When the M-equivalents will do.** Quick exploration, or getting unstuck on a workshop laptop where nothing else can be installed, **and** no intention to publish. The pipeline runs end to end; the report says which version ran. Remember they are unsupported.

`check_setup()` in R will tell the user which of the originals are available and confirms that the M-equivalents are present (they're bundled with MARMOT, so they're always available).

## Tactics for awkward packages

When an R install gets stuck — long compilation, missing system library, repeated download failures — try these in order before giving up.

0. **Restart R.** Before anything else. Close RStudio completely (not just the script tab) and reopen. R is uniquely susceptible to "stale session" problems — half-loaded packages, broken namespaces, conda/reticulate state, masked functions — and a fresh session fixes a startling fraction of install issues with no further diagnosis needed. See `troubleshooting.md` → *Step zero: restart R*. Always try this first.
1. **Check the toolchain.** Most failures on Windows / macOS come down to a missing compiler.
   - **macOS**: `xcode-select --install` in Terminal. One-time, ~1 GB.
   - **Windows**: install Rtools — see the dedicated subsection below, this trips up most Windows users.
   - **Linux**: `sudo apt install build-essential gfortran` (Debian/Ubuntu) or distro equivalent.
2. **Use `pak` instead of `install.packages` / `remotes`.** `pak` resolves dependencies more aggressively and gives clearer errors:
   ```r
   install.packages("pak")
   pak::pkg_install("i-cyto/Rphenograph")
   ```
3. **Increase the download timeout** for big binary packages on slow networks:
   ```r
   options(timeout = 600)   # 10 minutes
   ```
4. **Prefer pre-built binaries** over source builds where available:
   - **Posit Public Package Manager** (binaries for many platforms): <https://packagemanager.posit.co/>.
   - **R-Universe** (per-author binary repos): e.g. `install.packages("Rphenograph", repos = "https://i-cyto.r-universe.dev")`.
5. **For Bioconductor packages**, always use `BiocManager`, never `install.packages`:
   ```r
   BiocManager::install("CATALYST")
   ```
6. **If a single package keeps failing**, install everything else first and come back to it. `install_marmot_extras()` already does this with `tryCatch` — a single failure won't block the rest.
7. **As a last resort**, fall back to the M-equivalent and document that choice in the analysis notes.

If after all that a user is still stuck, ask them for:

- OS and version
- R version (`R.version.string`)
- The exact error message (last ~30 lines)
- Whether they're on a corporate network with a proxy / firewall

…and direct them to open an issue.

## Windows: Rtools

MARMOT itself no longer needs Rtools: everything it installs comes ready-built, including Rphenograph from MARMOT's r-universe. This section matters only when a user installs something else from source.

Rtools is the C/C++/Fortran toolchain R uses to compile packages from source on Windows. It is **not** installed by default with R, and many MARMOT-relevant packages (`Rphenograph`, `FastPG`, anything Rcpp-based) need it. This is the single most common source of install pain on Windows — expect to walk users through it.

### Symptoms of a missing/broken Rtools

- `install.packages()` or `remotes::install_github()` fails with `WARNING: Rtools is required to build R packages, but is not currently installed`.
- The Tauri install panel **silently skips Rphenograph** and tells the user MARMOT's bundled `Mphenograph` / `MfastPG` will be used instead. (This is intentional — `install_marmot_extras()` checks for a toolchain via `pkgbuild::has_build_tools()` before attempting Rcpp packages, to avoid hanging on an interactive Rtools prompt. So a user with no Rtools won't see an error, they'll just quietly miss `Rphenograph`.)
- `pak::pkg_install("...")` errors with messages about `make`, `g++`, or `gfortran` not found.

### Install steps

1. Find the user's R version: `R.version.string`.
2. Download the **matching** Rtools from <https://cran.r-project.org/bin/windows/Rtools/>. **Versions must line up** — see the table below; mismatched Rtools won't be recognised.
3. Run the installer. Defaults are fine — install to the suggested path (don't change it; CRAN's R looks in the default location).
4. **Restart R / RStudio.** Rtools is detected on R startup, not at install time.
5. Verify in R:
   ```r
   pkgbuild::has_build_tools(debug = TRUE)
   # TRUE = good
   # FALSE = R can't see the toolchain (PATH issue or version mismatch)
   ```
6. Re-run `install_marmot_extras()`. `Rphenograph` should now install on the second attempt.

### R version ↔ Rtools version

| R version | Rtools version |
|---|---|
| R 4.5.x | Rtools45 |
| R 4.4.x | Rtools44 |
| R 4.3.x | Rtools43 |
| R 4.2.x | Rtools42 |

Always confirm the user's R version first. Installing the wrong Rtools is a very common cause of "I installed Rtools but it still doesn't work."

### Common Windows-specific gotchas

- **Didn't restart R after install.** Most common fix: close R/RStudio completely (not just the script tab) and reopen. Then re-check `pkgbuild::has_build_tools(debug = TRUE)`.
- **Installed to a non-default path.** R looks in standard locations. If the user changed the install path, either reinstall to defaults or add Rtools to PATH manually (advanced — usually easier to reinstall).
- **Running R as a portable / Microsoft Store install.** Rtools detection is unreliable here. Recommend installing R from the CRAN .exe instead.
- **Corporate-managed laptops.** Rtools installer may need admin rights; some IT setups also block compilation. If the user can't install Rtools at all, fall back to `Mphenograph` / `MfastPG` and tell them why (see *M-equivalents*).
- **Antivirus killing the compiler mid-build.** Rare but real — `make.exe` and `gcc.exe` sometimes get quarantined. If install fails partway with "process killed" or no clear error, suggest temporarily whitelisting the Rtools install directory.

If Rtools is genuinely impossible on the user's machine, the M-equivalents are the right fallback — see that section above.

## What you should never do

- Never run `R CMD INSTALL` or `install.packages("MARMOT")` on the user's behalf — let them run it.
- **Never run toolchain installers on the user's behalf** — that includes `xcode-select --install` (macOS), the Rtools installer (Windows), and `sudo apt install build-essential` / equivalent (Linux). Walk the user through running these themselves; don't execute them in their shell. They affect system-wide state and may need admin rights or interactive prompts you can't see.
- Never modify their `.Renviron`, `.Rprofile`, or shell config without asking first.
- Never recommend `sudo` for R installs — break the system instead, recommend a per-user library.
