# Running the pipeline

Once the metadata Excel file is filled in and the FCS files are in place, the user runs the pipeline. This file covers what to expect.

## The folder convention

MARMOT assumes **the metadata Excel and the FCS files live in the same folder**. The pipeline derives the FCS folder from the location of the metadata file — there's no separate FCS-folder argument. Output also lands in this folder, in a timestamped `Results_Files_YYYY-MM-DD_HH.MM.SS/` subfolder.

Typical layout before a run:

```
MyStudy/
├── MARMOT_Metadata_MyStudy.xlsx
├── Sample01.fcs
├── Sample02.fcs
└── …
```

## Two ways to run

### Tauri app

1. Open `MARMOT.app`.
2. **Setup** tab → load the metadata Excel and edit values; the app saves it back to the same xlsx.
3. **Run** tab → click the run button. Logs stream live in the panel.

The app calls `marmot()` under the hood with the metadata path and a name for the run.

### From R

Two helpers, used in sequence on a fresh study:

```r
library(MARMOT)

# 1. Drop a copy of the blank metadata template into the FCS folder
addMetadataToFCSFolder(
  FCS_folder = "/path/to/MyStudy/",
  name       = "MyStudy"
)
# → creates /path/to/MyStudy/MARMOT_Metadata_MyStudy.xlsx

# 2. (User edits the xlsx in Excel, then…)

# 3. Run
marmot(
  metadata = "/path/to/MyStudy/MARMOT_Metadata_MyStudy.xlsx",
  name     = "MyStudy",
  render   = TRUE
)
```

Arguments to `marmot()`:

- `metadata` — full path to the Excel file. **Required.** The folder containing this file is treated as the FCS folder.
- `name` — short label for the run; used in output filenames (e.g. `MARMOT_Pipeline_MyStudy.qmd`). Default `"Title"`.
- `render` — `TRUE` to render the HTML report; `FALSE` (the default) just templates the Quarto file and stops, useful if the user wants to edit the `.qmd` before rendering.

`addMetadataToFCSFolder()` arguments:

- `FCS_folder` — path to the folder where the FCS files live. Default `"."`.
- `name` — optional suffix (yields `MARMOT_Metadata_<name>.xlsx`); without it the file is just `MARMOT_Metadata.xlsx`.
- `overwrite` — default `FALSE`; safety flag to avoid clobbering an edited template.

## What happens, in order

1. **Read metadata.** Reads the Excel file (`Pipeline Settings` first; errors if that sheet is missing or any required setting is blank). Validates parameters.
2. **Template the Quarto file.** Copies `MARMOT_Pipeline.qmd` from the package into the new `Results_Files_<timestamp>/` folder, fills in the parameter values from `Pipeline Settings`.
3. **Render** (if `render = TRUE`):
   1. Import FCS files (with QC via PeacoQC or FlowAI, depending on `runQC`).
   2. Build the `SingleCellExperiment` (SCE) object.
   3. Cluster (FlowSOM by default; also Rphenograph / Mphenograph / MfastPG / PARC / Mparc).
   4. Dimensionality reduction (UMAP by default; also TSNE / pacmap / Mpacmap).
   5. Differential abundance (DA) — cluster proportions across conditions.
   6. Differential state (DS) — marker expression within clusters across conditions.
   7. Render the HTML report.

## How long does it take?

Depends almost entirely on cell count and clustering method.

| Cells | Method | Rough render time |
|---|---|---|
| 100k | FlowSOM | minutes |
| 1M | FlowSOM | 10–30 min |
| 5M+ | FlowSOM | 1–3 h |
| any | PARC / Phenograph | longer than FlowSOM |

If the user reports "it's been running for hours and nothing's happening," ask which stage it's stuck on (the last Quarto log line) before assuming it's hung.

## What lands in the output folder

After a successful render:

```
MyStudy/
├── MARMOT_Metadata_MyStudy.xlsx                     # original metadata (untouched)
├── Sample01.fcs … (FCS files)
└── Results_Files_2026-04-15_09.48.28/
    ├── MARMOT_Pipeline_MyStudy.qmd                  # the templated Quarto file
    ├── MARMOT_Pipeline_MyStudy.html                 # the rendered report
    ├── MARMOT_Pipeline_MyStudy_files/               # report assets (figures, JS)
    ├── My_MARMOT_Analysis_metadata.xlsx             # copy of the metadata used
    ├── Excel_Files/                                 # exported tables (DA / DS results, panel summaries)
    ├── PDF_figures/                                 # standalone plot exports
    └── R_files/                                     # data sidecars used by the Shiny app
        ├── marmot_results.h5ad                      # main SCE store (HDF5/AnnData)
        ├── framesList.qs2                           # FCS frames sidecar (for FCS export)
        ├── da_ds_results.qs2                        # DA + DS results sidecar
        └── pipeline_settings.json                   # snapshot of run parameters
```

Each run creates a **new** timestamped `Results_Files_*` folder — previous runs are not overwritten. Disk fills up on repeated runs; tell the user they can delete old `Results_Files_*` folders they no longer need.

The Shiny exploration app reads from `R_files/`.

## Re-running

The pipeline supports a **reload path**: if `RDataFolder` is set in `Pipeline Settings` (or passed via the metadata) and points at a previous `Results_Files_*/R_files/` folder, MARMOT skips re-importing FCS and re-clustering, and uses the cached results. Useful for:

- Tweaking plot parameters without recomputing.
- Adding new contrasts to an existing analysis.
- Re-rendering after a small metadata change (e.g. renaming a condition).

## Launching the Shiny app

After a successful render:

```r
shinyMarmot(marmot_output = "/path/to/MyStudy/Results_Files_2026-04-15_09.48.28/R_files/")
```

Or with no argument to load the **bundled demo data**:

```r
shinyMarmot()
```

In the Tauri app, click **Explore** after the run finishes. See `shiny-app.md` for what each tab does.

## Choosing k (cluster resolution)

`kValuesIWant` (in `Pipeline Settings`) is a space-separated list of cluster resolutions to compute — e.g. `20 40 60`. `knn` is the one used downstream for DA / DS and the default plots.

There is no universally optimal k — it depends on dataset, biology, and the question. The marmots' practical advice:

- **Slightly over-cluster, then merge post-hoc.** Better to start with too many clusters (which you can merge based on biological markers in the Shiny app's relabelling tab) than too few (which you can't easily split).
- **Even k for FlowSOM, odd k for Phenograph / PARC.** The combinatorics of FlowSOM metaclustering favour even numbers; graph-based methods (Phenograph, PARC) favour odd. This is folklore that has worked well.
- **Use the K-tree** in the rendered report. MARMOT computes a clustree-style visualisation of how clustering changes as k increases — stable cell groupings stay together across k values, unstable ones jump around. Use it to pick a `knn` that lies in a stable region.
- **Check stability across nearby k values.** If clusters change dramatically between, say, k=20 and k=30, that resolution is unstable.

## Comparing methods

The wiki recommends running the pipeline **multiple times with different `clusteringMethodToUse`** (and possibly different `dimRedMethodToUse`) and comparing outputs. To do this cleanly:

1. Make a copy of the metadata Excel and edit `Pipeline Settings` for each run.
2. Use distinct `name` values (`marmot(metadata = ..., name = "FlowSOM_k40", render = TRUE)`) so each run lands in its own clearly-named `Results_Files_*` folder.
3. Compare key plots and tables side by side. If the same broad trends appear under multiple methods, you can be more confident they're real.

## Things to flag for the user

- **Disk space**: a 5M-cell run can produce several GB in `R_files/`, and each new run creates a fresh `Results_Files_*` folder. Suggest deleting old result folders periodically.
- **RAM**: MARMOT caps cell import based on available RAM; on Windows there's a fallback that may downsample more aggressively. If they have a small machine, suggest setting `downsampleTo` explicitly in `Pipeline Settings`.
- **Default `downsampleTo` in the shipped template is 1000** — way too low for real analysis. Remind them to raise it (or leave blank to use all cells, subject to RAM cap).
- **Don't move the `Results_Files_*` folder** while the Shiny app is running on it — paths are absolute internally.
- **Don't edit the metadata Excel** mid-render. Wait until it finishes.
- **Don't put the metadata file outside the FCS folder.** MARMOT looks for FCS files alongside the metadata; a metadata in `Documents/` and FCS in `Data/` won't work.

## Errors during rendering

If Quarto fails part-way, the partial `.html` may exist but be unusable. Send them to `troubleshooting.md` with the error message — most pipeline failures have known causes (PeacoQC matching, RAM exhaustion, conda env, contrast format).
