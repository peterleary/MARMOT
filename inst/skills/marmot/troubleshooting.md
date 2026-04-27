# Troubleshooting

Always check this file before improvising a fix. MARMOT errors usually have known causes.

## Step zero: restart R

Before doing anything else, **have the user restart R**. Close RStudio (or the R session) completely — not just clear the workspace, not just close a script tab — and reopen. Then re-run the failing step.

This sounds glib but it is the single most effective intervention in R. R's environment accumulates loaded namespaces, partially-loaded packages, broken Python bindings, stale `library()` paths, and connection handles that it cannot cleanly recover from while running. A fresh session resolves a startling fraction of "it just stopped working" reports — toolchain detection issues, conda env confusion, package masking, reticulate state, all of it.

It is genuinely the *Have you tried turning it off and on again?* of statistical computing, and it has never been more appropriate than for R. Always try this first. Always.

## Diagnostic ritual

When a user reports "it's broken," collect three things before suggesting anything:

1. **What were they doing?** (Installing? Rendering? Launching Shiny?)
2. **What's the exact error message?** Ask for the last ~20 lines of output, not a paraphrase.
3. **What version of MARMOT?** `packageVersion("MARMOT")`. Many bugs were fixed in v1.3.1.

## Install errors

### `conda not found` (macOS Apple Silicon)

Cause: user has Anaconda or no conda at all. MARMOT expects miniforge on arm64.

Fix: install miniforge from <https://github.com/conda-forge/miniforge>, then re-run `install_marmot_extras()`.

### Python imports fail on macOS arm64

Cause: rpath / annoy issues that `inst/python/marmot_setup.py` is meant to patch on first import.

Fix:

```r
reticulate::py_run_string("import marmot_setup")
```

If that errors, the conda env is probably broken — recreate it with `install_marmot_extras(include_python = TRUE)`.

### `Rphenograph` or `FastPG` won't compile

Cause: GitHub-only Rcpp packages — need a working C/C++ toolchain (Rtools on Windows, Xcode CLT on macOS, build-essential on Linux).

Fix path A — **install the toolchain** (best, because the originals are the published methods):

- macOS: `xcode-select --install`
- Windows: install Rtools matching the R version, restart R — see `setup.md` → *Windows: Rtools* for version table and gotchas (this is the most common Windows install issue)
- Linux: `sudo apt install build-essential gfortran`

Then re-run `install_marmot_extras()`.

Fix path B — **use the M-equivalents.** MARMOT bundles `Mphenograph` and `MfastPG` (pure-R reimplementations). They're always available, no compilation needed. The pipeline will use them automatically when `Rphenograph` / `FastPG` aren't installed. To explicitly skip the originals:

```r
install_marmot_extras(include_suggests = FALSE)
```

See `setup.md` → *M-equivalents* for the trade-off — the originals are the published methods; M-versions are AI-assisted reimplementations and should be treated as fallbacks for analyses the user plans to publish.

For Python-based clustering / DR (`PARC` / `PaCMAP`) the same logic applies: `Mparc` and `Mpacmap` are pure-R fallbacks if conda isn't available.

### `Quarto is required to render the HTML report but was not found on your system.`

Cause: Quarto isn't installed or isn't on `PATH`. The exact message — emitted by `marmot()` when `render = TRUE` — points the user to the install URL.

Fix: install Quarto from <https://quarto.org/docs/get-started/>, **restart R**, verify with `MARMOT::check_setup()`. Note that the templated `.qmd` was already saved to the results folder before the failure — they can render it manually once Quarto is installed.

### `check_setup()` to triage in one go

Before going down a rabbit hole, suggest `MARMOT::check_setup()`. It prints a single status report covering: every Tier-1 package (red ✗ if missing), GitHub packages (yellow ⚠️ if missing — fine because of M-equivalents), Quarto (with detected version), and the Python `p4r` env (with `python_path` if found). Faster than diagnosing piecemeal.

## Metadata / file errors

> **Read this first.** The single biggest source of MARMOT problems is the metadata sheet. Typos, case mismatches, wrong file names, marker names that don't match the FCS — these account for the majority of "the pipeline broke" reports, and they keep happening no matter how careful users try to be. **R is case-sensitive everywhere**: every condition name, file name, and marker name must match across `File Data`, `Study Data`, and the FCS files exactly — `Ki67` and `KI67` are different markers; `Control` and `control` are different conditions.
>
> Before going down a specific error rabbit hole below, the highest-yield diagnostic is to ask the user to re-do the metadata triple-check ritual (see `metadata-sheet.md` → *Triple-check ritual*) and check obvious culprits:
>
> - **File names**: copy-paste from Finder into Excel, don't retype. The Tauri app can autopopulate `file_name` directly from the FCS folder — recommend it for users who keep getting filename mismatches.
> - **Marker names**: export marker names directly from FlowJo (or read from a sample FCS) rather than retyping from a panel design doc. The Tauri app can check marker names against the FCS files and flag mismatches before the user runs the pipeline. `KI67` vs `Ki67`, `CD8` vs `CD8a`, `MHCII` vs `MHC-II`, `IL-7Ra` vs `IL7Ra` are all real, common mismatch patterns.
> - **Condition names**: copy-paste between cells inside Excel rather than retyping. Once spelled, never retype.
>
> Be patient with users about this — they will find new and creative ways to make typos. It's not their fault that R is fastidious; it just is. Treat metadata-debugging as the default first move whenever a user reports a problem they can't explain.

### `The marmots can't find a 'Pipeline Settings' tab in your Excel Metadata file.`

Cause: the `Pipeline Settings` sheet has been deleted, renamed, or the user's Excel file was hand-built from scratch instead of from `addMetadataToFCSFolder()`.

Fix: re-create the file from the template via `MARMOT::addMetadataToFCSFolder("/path/to/FCS_folder/", name = "MyStudy")`, then re-fill it. The error message itself recommends this.

### `<param> is blank! Please enter a value in the Excel Metadata file.`

Where `<param>` is one of: `clusteringMethodToUse`, `markersToClusterBy`, `kValuesIWant`, `knn`, `dimRedMethodToUse`, `markersToDimRedBy`, `runQC`, `useQC`, `gimmePDFs`, `quantileNormaliseAll`, `runInParallel`, `nCores`, `ramPerCore`, `themeToUse`, `viridisColour`.

Cause: that variable's `Setting` cell is empty in the `Pipeline Settings` sheet. These can't be NA.

Fix: open `Pipeline Settings`, find that `Variable`, fill in the `Setting` from the `Options` sheet's allowed values. (Note: `downsampleTo`, `RDataFolder`, and `excludeTheseSamples` *can* be left blank — they'll be set to `NULL` rather than erroring.)

### `Missing required variables: ... Please define them in the 'input settings' chunk.`

Cause: this error comes from inside the rendered Quarto, after templating succeeded but before the analysis runs. Usually means a parameter was templated incorrectly — e.g. a non-numeric value supplied where a number was expected.

Fix: check the templated `.qmd` (in the `Results_Files_*` folder) line by line against the `Pipeline Settings` sheet. If the templated values look wrong, regenerate via `marmot(..., render = FALSE)` and inspect the qmd before re-running with `render = TRUE`.

### `FCS files listed in metadata not found in fcsDir: …`

Cause: a `file_name` in `File Data` doesn't match a real file in the metadata's folder.

Causes ranked by likelihood:

1. Typo / extra space / case mismatch in `file_name`.
2. Missing `.fcs` extension in `file_name` (must be included).
3. FCS files are in a subfolder, not alongside the metadata Excel — MARMOT looks for FCS files in the **same folder** as the metadata file. No separate FCS-folder argument exists.
4. (Linux only) Case mismatch in the filename itself.

Fix: ask for the exact `file_name` cell value and the output of `list.files(dirname("/path/to/MARMOT_Metadata_*.xlsx"))`. The error message lists which files are missing — line up the names character-by-character.

### `❌ Markers in metadata not found in FCS: …`

Cause: a name in `Markers to include` (`Study Data` sheet) doesn't appear among the antigens parsed from the FCS files. Even one space or capitalisation difference triggers this.

Fix: the error lists the missing markers. In FlowJo, open one of the FCS files and copy the antigen names directly. Update the `Markers to include` cells to match exactly. The wiki has a screenshot recipe — recommend that workflow rather than retyping.

Tip: if the user has dozens of mismatches, it's likely a single systematic difference (everything has a leading space, a `-` was used instead of a `_`, etc.). Look for the pattern.

### Contrast errors / DA or DS section empty

Cause: malformed contrasts are **silently filtered**. The pipeline keeps only entries matching `.+ over .+` exactly — anything else is dropped without warning, and you end up with an empty `daList` / `dsList`. So an empty DA section is usually "all my contrasts got filtered" rather than "the analysis failed."

Common reasons each contrast fails the filter:

- Wrong separator: `.vs.`, `vs`, `-`, `_vs_`. Must be ` over ` (literal word, single space each side).
- Double space: `Treatment over  Control`.
- No spaces: `Treatment overControl`.
- Case mismatch: `treatment over control` when `File Data > condition` has `Treatment` / `Control`.
- Trailing whitespace in the Excel cell.

Fix: open `Study Data > Conditions To Test`, eyeball each row character-by-character, and re-type any that look off. **Copy condition names directly from `Conditions Order`** rather than retyping. Then check both halves against `File Data > condition` exactly (case-sensitive).

### All clusters look identical / one giant cluster

Cause: clustering ran on too few markers, or markers that don't differentiate populations. Depends on what `markersToClusterBy` is set to.

Fix: open the `Study Data` sheet and check:

- `Marker Type` values are sensible and lowercase (`type` / `state`), and positionally aligned with `Markers to include`.
- Scatter / time / dump / CD45 / comp channels are listed in `Markers to exclude completely`, not in `Markers to include`.
- The `markersToClusterBy` setting (in `Pipeline Settings`) matches what's actually in the panel:
  - `type` — needs a reasonable set of lineage markers (CD3, CD4, CD8, CD19, CD56…).
  - `state` — needs state markers, but clustering on state alone is unusual.
  - `all` (default) — make sure most markers haven't been excluded into `Markers to exclude completely`.

If the panel is homogeneous (only `type` or only `state`), MARMOT auto-uses all markers regardless — so a homogeneous panel that still looks underpowered probably has too few markers under `Markers to include` full stop.

## Pipeline behaviour that surprises users

These aren't errors — they're silent / quiet behaviour the pipeline does on purpose, and users often interpret them as bugs. Recognise them before suggesting fixes.

### "Why did QC not run?"

QC auto-skips on small samples. If any sample has **<2000 cells**, all QC is skipped (`runQC` is forced to `"None"`). If any sample has **2000–5000 cells** and `runQC = "PeacoQC"`, PeacoQC is skipped with a recommendation to switch to FlowAI. The skip is reported in the QC section of the rendered HTML, not as a render failure.

Fix: if QC is important, the user needs more cells per sample. Otherwise the skip is correct.

### "Why was my data downsampled?"

MARMOT auto-applies a **RAM cap** at FCS import. It estimates available RAM and caps cells per sample if the dataset would exceed budget. This is reported via a callout in the rendered HTML's import section, with the per-sample cap shown.

On Windows, if RAM detection fails, the pipeline falls back to a conservative `ramPerCore × nCores` budget — so Windows users sometimes get capped more aggressively than they expect. The user can raise `ramPerCore` and `nCores` in `Pipeline Settings` to relax the cap.

### "Why did clustering / DR run on all markers when I set `markersToClusterBy = 'type'`?"

If the panel is **homogeneous** (only `type` markers, or only `state` markers), MARMOT auto-uses all markers regardless. An info message says so at import time. This is by design — there's no useful subset to pick from a homogeneous panel.

### "PARC / PaCMAP didn't run — what happened?"

If conda or the `p4r` env isn't available, the pipeline **automatically falls back** to the pure-R `Mparc` / `Mpacmap` and prints a message like `"Python PARC unavailable -- falling back to Mparc (R)."` This is not a failure. To use the originals, run `MARMOT::install_marmot_python()` and re-run.

### "I changed contrasts and re-ran with `RDataFolder` — why is it slow?"

The reload path skips FCS import, clustering, and DR — but **DA and DS always re-run from scratch**. They're not cached. So changing contrasts and re-rendering still takes the time of the diffcyt analyses. This is intentional (contrasts can change cheaply); not a bug.

### "Why is my DA results table missing some clusters?"

`diffcyt` can return `NA` for `p_adj` on individual clusters when edgeR can't fit the model (e.g. singular contrast, insufficient replication, perfect separation). Those clusters get dropped via `complete.cases()` when building `selectedClustersList`, with no warning. So missing rows in DA usually mean "edgeR couldn't fit that cluster", not "we forgot it."

### "I have no significant DA clusters / DS markers — is the pipeline broken?"

Often the pipeline is fine and **the answer just is "no significant changes."** Before assuming a bug, consider:

- **The biology genuinely doesn't move much under this contrast.** Null results are real results. If the experiment isn't expected to produce dramatic shifts, an empty significant-clusters list is not a malfunction.
- **Under-clustering** can hide real DA. If a true responder population is split across one big cluster with non-responders, the average won't shift enough to be significant. Try a higher k.
- **Over-clustering** can also hide DA. If the population is split into many tiny clusters, each one has too few cells per sample to reach significance. Try a lower k.
- **Borderline significance** — check the underlying p-values, not just the FDR-passing list. If several clusters are sitting at p_adj of 0.06–0.10, the analysis may genuinely be at the edge; tweaking k may push them either side. Reviewers often want to see the full table, not just the significant rows.
- **Confirm contrasts actually ran.** An empty `daList` / `dsList` can mean "all contrasts got silently filtered as malformed" (see *Contrast errors* above) rather than "no significant findings." Distinguish the two by checking whether the DA section in the rendered HTML is empty (filtered contrasts) or present-but-with-no-significant-rows (real null result).

If the user wants to push on this, suggest they re-run with a different `knn` value and compare. The wiki recommends running multiple metadata files with different methods / resolutions and comparing — a robust biological signal should appear under multiple settings.

### "Why does the DS section say `treating all markers as state`?"

If no `state` markers are defined in `Marker Type`, MARMOT auto-coerces all markers to `state` for the DS analysis (DA is unaffected). A message says so at the DS step. Useful for type-only or homogeneous panels.

## Render errors

### `Unsupported clustering method: …` (only on reload)

Cause: `RDataFolder` was set, and the `clusteringMethodToUse` recorded in the cached h5ad is not one of the recognised methods (`FlowSOM`, `Rphenograph`, `Mphenograph`, `MfastPG`, `PARC`, `Mparc`).

Fix: the cache was probably written by an old / forked version of MARMOT. Re-run from scratch with `RDataFolder = NULL`.

### `No h5ad data found in RDataFolder. Please re-run the pipeline to generate h5ad output.`

Cause: `RDataFolder` points at a folder that doesn't contain `marmot_results.h5ad` — either because it's pre-h5ad-migration data (old `.qs` sidecar format) or the path is wrong.

Fix: check the path. `RDataFolder` should be either:

- A `Results_Files_<timestamp>/R_files/` folder (preferred), or
- A `Results_Files_<timestamp>/` parent folder (auto-resolves to `R_files/`).

If the data is genuinely pre-h5ad, the user has to re-run from scratch — no migration tool exists.

### `PeacoQC` step fails / no cells survive QC

Cause: a known issue around FCS keyword handling. The current pipeline names `framesList` entries with the `.fcs` suffix specifically to keep PeacoQC's QC files matchable by CATALYST's `prepData()`.

Fix: confirm they're on **v1.3.1**. If yes and it still fails, check that no FCS file has 0 events post-pre-gating; PeacoQC will reject empty frames.

### Pandoc error: "literal `<p>::::</p>` in HTML output"

Cause: a Pandoc parser bug triggered by `n. ` in kable column names inside fenced divs (fixed in v1.3.1).

Fix: confirm v1.3.1. If they're on an older version, recommend upgrading.

### Out-of-memory during FCS import

Cause: panel × cell-count exceeds available RAM, OR the auto-RAM-cap was disabled / its detection failed.

Fixes (any of):

- Set `downsampleTo` to `100000` (or similar) in `Pipeline Settings`.
- Lower `ramPerCore` and/or `nCores` so the auto-cap kicks in earlier.
- On Windows, RAM detection sometimes fails silently — see the `import_fcs_scan` warning in the rendered HTML. If it did fail, set `ramPerCore` explicitly to a value matching the actual machine.
- Close other apps; rerun.

If the user is running in Apptainer / Singularity on an HPC and the error is unintuitive, **try increasing the SLURM `--mem=` allocation first** before chasing other causes. See `setup.md` → *Apptainer RAM gotcha*.

### `featuresToUmap` not found on reload

Cause: a v1.2-era bug where this variable was set inside the `dim_reduction` chunk, which is skipped on reload.

Fix: upgrade to v1.3.1; the variable is now derived in an always-running chunk.

### Differential state results look wrong / corrupted SCE

Cause: a v1.2-era bug where `rowData(sce)$marker_class <- "state"` mutated the main SCE instead of the DS copy.

Fix: upgrade to v1.3.1.

## The diagnostic log (`marmot_run_log.json`)

Every render writes a JSON diagnostic log to the `Results_Files_*/` folder. It records:

- Pipeline version, timestamp.
- Data shape: `n_samples`, `n_conditions`, `n_contrasts`, `n_markers_requested`, `homogeneous_panel`.
- Each pipeline checkpoint (when each chunk completed).
- DA / DS shape diagnostics: number of rows, NA `p_adj` count, missing/extra clusters relative to the SCE.

When a user reports an issue mid-pipeline, ask them to share this file alongside the error — it's much more informative than the user's verbal recap of where things stopped.

## Shiny app issues

### Shiny app won't launch / "file not found"

Cause: `R_files/` was moved or the path passed to `shinyMarmot()` is wrong.

Fix: check the path; the app needs the `R_files/` folder produced by the pipeline.

### DR plot shows weird cluster IDs (`p1`, `c1`, `k1`…)

Cause: pre-v1.3.1 behaviour. Cluster IDs were prefixed with `p`/`c`/`k` depending on method.

Fix: upgrade to v1.3.1; cluster IDs are now plain numbers (`1`, `2`, `3`…).

### Empty DS tab in the rendered report

Cause: pre-v1.3.1 fenced-div nesting bug from `cat()` inside `for` loops.

Fix: upgrade to v1.3.1; the DS tab now follows the DA pattern (lapply + flat headers).

## When you don't know

If you've checked this file and the error doesn't match anything here:

1. Don't guess. Don't fabricate a fix.
2. Suggest the user open an issue at <https://github.com/peterleary/MARMOT/issues> with the full error log, their MARMOT version, OS, and a description of what they were doing. The marmots respond to good bug reports with carrots 🥕 (metaphorically).
3. Offer to help them write a clear bug report.
