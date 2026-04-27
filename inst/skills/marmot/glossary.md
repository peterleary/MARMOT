# Glossary

A translation table between immunology / cytometry vocabulary, R / Bioconductor vocabulary, and MARMOT-internal vocabulary. Use this when a user says one thing and you need to know what it maps to in the codebase, or vice versa.

## Cells and samples

| Wet-lab term | R / MARMOT term | Where it lives |
|---|---|---|
| A single FACS event | A row in the expression matrix; "cell" in MARMOT | `assay(sce)` columns |
| One FCS file | A "sample" | One row of the `File Data` sheet |
| Biological replicate | "patient_id" or whatever the user named it | A column of `colData(sce)` |
| Acquisition batch | "batch" | A column of `colData(sce)` |
| Experimental group | "condition" | A column of `colData(sce)`; halves of contrasts |

## Markers and channels

| Wet-lab term | R / MARMOT term | Where it lives |
|---|---|---|
| Detector / channel name | `fcs_colname` (internal) | parsed from the FCS itself; not user-entered |
| Antibody target | `marker_name` / antigen | `Study Data > Markers to include` (and row name in the expression matrix) |
| Lineage marker | `Marker Type` value `type` | `Study Data > Marker Type` column (positionally aligned with `Markers to include`) |
| Activation / functional marker | `Marker Type` value `state` | `Study Data > Marker Type` column |
| Scatter / dump / time | listed under `Markers to exclude completely` | `Study Data > Markers to exclude completely` — **not** given a `Marker Type` of `none`; just dropped before import |
| Compensation matrix | (already applied; MARMOT doesn't see it) | Inside the FCS file, before export |
| Unmixed signal | The expression values MARMOT reads | `assay(sce, "exprs")` |

## Analysis steps

| Wet-lab / paper term | R / MARMOT term |
|---|---|
| "Population" / "subset" | "cluster" |
| Manual gating | (Not in MARMOT — done before export) |
| Automated population identification | Clustering (FlowSOM / PARC / Phenograph / Kmeans) |
| t-SNE / UMAP plot | Dimensionality reduction; `reducedDims(sce)` |
| "Cluster A is enriched in treated samples" | Differential abundance (DA) |
| "Marker X is upregulated in cluster A in treated" | Differential state (DS) |
| Heatmap of mean expression per cluster | `plotExprHeatmap()` |
| Frequency plot per condition | `plotFreqHeatmap()` |

## Statistical / pipeline terms

| Term | What it means in MARMOT |
|---|---|
| **Contrast** | A pairwise comparison `A over B` between two values of `condition` (A is the numerator, B the reference). Drives DA and DS. |
| **Resolution** (clustering) | How many clusters: `meta10` = 10 metaclusters, `meta20` = 20, `k20` = 20-means, etc. The user can switch resolutions in the Shiny app. |
| **Downsampling** | Capping cells per sample (`downsampleTo`) so very large experiments still fit in RAM. |
| **RAM cap** | An auto-applied upper limit on imported cells based on available memory. Different fallback on Windows. |
| **Reload** | Re-using a previous run's `R_files/` to skip FCS import + clustering. Driven by `RDataFolder`. |
| **Sidecar files** | The `.qs2` files next to `marmot_results.h5ad` (`framesList.qs2`, `da_ds_results.qs2`) that store things HDF5 can't, plus `pipeline_settings.json`. |

## R / Bioconductor terms a wet-lab user might bump into

| Term | Translation |
|---|---|
| `SingleCellExperiment` (SCE) | The big object holding everything: cells × markers + metadata + DR coordinates. |
| `colData` | Per-cell metadata. |
| `rowData` | Per-marker metadata. |
| `reducedDims` | The UMAP / PaCMAP coordinates. |
| `assay` | The actual expression matrix. |
| `h5ad` | A file format for SCE-like data, readable from R and Python. |
| `qs2` | A fast R-only serialisation format. Used for sidecars. |
| `Quarto` | The notebook format MARMOT uses to produce its HTML report. |
| `reticulate` | The R↔Python bridge. Used for PARC / PaCMAP. |
| `conda` (miniforge) | The Python environment manager. Used to install PARC / PaCMAP cleanly. |

## Things that look the same but aren't

- **PARC** (the clustering algorithm) vs **PaCMAP** (the dimensionality reduction). Both are Python, both run via reticulate, but they do different jobs.
- **FlowSOM** (clustering) vs **FlowAI / PeacoQC** (QC). Different stages of the pipeline.
- **`cluster_id`** (the chosen clustering used everywhere downstream) vs **`meta10` / `meta20` / `k20`** (alternative resolutions stored alongside, available in the Shiny app dropdown).
