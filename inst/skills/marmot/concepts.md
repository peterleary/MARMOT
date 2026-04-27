# Concepts

Reference for the technical terms MARMOT uses. Translate these into the user's language — don't recite the definitions verbatim. For a side-by-side translation table, see `glossary.md`.

## Spectral flow cytometry

Conventional flow uses a small number of broad fluorophores read by matched detectors. **Spectral flow** captures the full emission spectrum of every cell across many detectors and unmixes the signals computationally. This allows panels of 30–40+ markers with much smaller spectral compromise.

Implication for MARMOT: panels are large, files are big, and cell counts per experiment can run into the tens of millions.

## FCS file

The standardised file format from the cytometer, one per sample. Contains:

- Per-cell measurements for every channel (markers + scatter + time).
- A header (keywords) with acquisition metadata.

MARMOT expects **pre-gated FCS files** — the user has already excluded debris, doublets, and dead cells in their cytometer software (FlowJo, Cytobank, OMIQ, SpectroFlo). MARMOT does not gate.

## Panel

The set of fluorophore-conjugated antibodies (and their channels) used in the experiment. In MARMOT terms, the panel is described across columns of the `Study Data` sheet:

- `Markers to include` — the markers you want analysed (must match antigen names parsed from the FCS).
- `Marker Type` — `type` or `state` for each, positionally aligned with `Markers to include`.
- `Cofactors for markers to use` — per-marker arcsinh cofactor.
- `Markers to exclude completely` — channels dropped entirely (scatter, time, dump, CD45, comp).

Channel names as written inside the FCS (the internal `fcs_colname`) are parsed automatically — the user works with antigen names, not channel names.

## Marker types: type vs state vs none

A conceptual classification of what each marker tells you about a cell.

- **`type` markers** — lineage / cell-identity markers (CD3, CD4, CD8, CD19, CD56…). Conceptually, the markers you'd use to *define* cell populations.
- **`state` markers** — activation / functional markers (CD69, CD25, Ki-67, IFN-γ, granzymes…). Conceptually, markers whose expression varies *within* a population depending on cell state.
- **`none`** — channels to ignore: scatter (FSC, SSC), time, live/dead, dump channels, anything not biologically meaningful for the analysis.

The classification is conceptual, not enforced. The pipeline lets the user choose what to cluster on via **`markersToClusterBy`** (default `"all"`; also `"type"` or `"state"`) and what to use for dimensionality reduction via **`markersToDimRedBy`** (same options). Differential state analysis always runs on whatever markers are classed `state`.

The classic CATALYST-style workflow is to cluster on `type` only and run DS on `state` only. MARMOT's defaults are more permissive (`"all"` for both clustering and DR) on the assumption that with modern spectral panels, the user often wants the full marker set to drive clustering. If a user wants the classic workflow, they set `markersToClusterBy = "type"` and `markersToDimRedBy = "type"`.

Getting `Marker Type` values wrong (or letting scatter/time/dump channels into `Markers to include` instead of `Markers to exclude completely`) is the most common source of "the clustering looks weird" complaints — but the fix usually involves both the `Study Data` sheet and the `markersToClusterBy` setting in `Pipeline Settings`.

## SingleCellExperiment (SCE)

The Bioconductor data structure MARMOT uses internally. Think of it as:

- A big numeric matrix (markers × cells) of expression values.
- Per-cell metadata (`colData`): which sample, which condition, which cluster.
- Per-marker metadata (`rowData`): antigen name, marker class.
- Slots for dimensionality reductions (`reducedDims`): UMAP, PaCMAP coordinates.

Stored on disk as `marmot_results.h5ad` (HDF5/AnnData format) for cross-language access and lower memory load.

## Compensation, spillover, unmixing

- **Spillover** — light from one fluorophore bleeding into another detector.
- **Compensation** — the conventional-flow correction (subtract a fraction of channel A from channel B).
- **Unmixing** — the spectral-flow equivalent: a least-squares decomposition across the full spectrum.

The user does this in their cytometer's software *before* exporting FCS for MARMOT. MARMOT does not unmix.

## Clustering

Grouping cells by similarity in marker expression. The user controls which markers are used via `markersToClusterBy` (default `"all"`). MARMOT supports:

- **FlowSOM** (default) — fast self-organising map + metaclustering. Best for most datasets.
- **PARC** — Python-based, scales to very large datasets.
- **Rphenograph** — graph-based clustering, slower but visually similar to scRNA-seq workflows.
- **MfastPG / Mphenograph / Mparc / Mpacmap** — pure-R MARMOT-internal fallbacks (see `setup.md` → *M-equivalents*).

### Algorithm character (from authors' experience)

These are anecdotal observations from the marmots, not benchmarks — but they capture the practical differences a user will see:

- **FlowSOM** — fast. Sometimes "stubborn" about *not* sub-clustering populations the user expected to see split. If FlowSOM gives one fat cluster where biology says there should be two, try a higher k or a different method.
- **Rphenograph** — slower. Output looks more like what scRNA-seq users are used to (graph-based, more granular clustering).
- **PARC** — middle ground between FlowSOM and Rphenograph in both speed and granularity.
- **FastPG** — very fast, but has a known **seed reproducibility issue** (same input → different clusters across runs). MARMOT's bundled **MfastPG** addresses this. Even with the fix it's the marmots' least-favourite — use the others first.

Output: a `cluster_id` column added to the SCE's `colData`. Resolutions like `meta10`, `meta20`, `k20`, `p20` are also stored so the user can switch in the Shiny app.

### The cluster-column prefix depends on the method

Different clustering methods write their per-resolution columns under different prefixes:

| Method | Prefix |
|---|---|
| FlowSOM | `meta` (e.g. `meta10`, `meta20`, `meta40`) |
| Rphenograph / Mphenograph / MfastPG | `k` (e.g. `k20`, `k40`) |
| PARC / Mparc | `p` (e.g. `p20`, `p40`) |

Internally MARMOT tracks this via a `mergeBy` variable. Mixing methods within a single run is **not supported** — `clusteringMethodToUse` is a single value per run. To compare methods, the wiki recommends running the pipeline multiple times with different `clusteringMethodToUse` values (use the `name` argument to keep outputs separate) and comparing.

### Homogeneous-panel auto-override

If a panel has only `type` markers or only `state` markers — i.e. a "homogeneous" panel — MARMOT detects this and **auto-uses all markers for both clustering and DR**, regardless of `markersToClusterBy` / `markersToDimRedBy`. The pipeline emits an info message at import time when this kicks in. Useful to know if a user expected `markersToClusterBy = "type"` to take effect and is wondering why all markers are being used.

## Dimensionality reduction (DR)

A 2D embedding for visualisation. MARMOT runs:

- **UMAP** — standard, fast.
- **t-SNE** — strongly preserves *local* structure; pulls nearest neighbours tight.
- **PaCMAP** — preserves more *global* structure than UMAP (Python; pure-R `Mpacmap` fallback).
- **PCA** — always run as a preprocessing step.

Coordinates are stored in `reducedDims(sce)` and shown in the Shiny app's DR tab.

### What DR is for (and what it is not for)

The marmots use DRs **mostly for QC** (spotting cells that escaped pre-gating) and **as an aid for cluster labelling** (does cluster 7 sit where the CD4s sit?). They explicitly recommend *not over-interpreting the ordination of cells* — DR positions are sensitive to marker selection, transformation, and algorithm parameters, and the same data can produce different-looking embeddings.

### Picking a DR method (heuristics)

- **t-SNE** — better when investigating **fine-grained heterogeneity within a population** (e.g. subsets within Tregs).
- **UMAP / PaCMAP** — better for **broader datasets** (e.g. CD45⁺) where distinct lineages (CD4 T, B, NK, monocytes…) should form clearly separated islands.

There is no "correct" DR method; the wiki recommends trying multiple as **complementary views** rather than picking one.

## Differential abundance (DA)

"Are some clusters more or less common in condition A vs condition B?"

Uses `diffcyt`'s GLMM / edgeR backends. Reads contrasts from the `Study Data > Conditions To Test` column. Output: a table per contrast with cluster, log fold change, p-value, FDR.

## Differential state (DS)

"Within a given cluster, do `state` markers behave differently between conditions?"

Same `diffcyt` backend. Output: a table per contrast × cluster × marker.

## Quality control (QC)

MARMOT runs **PeacoQC** (default), **FlowAI**, or no QC on each FCS to flag time-window anomalies. Cells in flagged windows are excluded before clustering.

This is *not* a substitute for the user's pre-gating — it's a second-pass cleanup.

### How they differ

- **flowAI** — detects anomalies based on **flow-rate irregularities, signal instability over time, and abrupt intensity changes**. Good for catching technical artefacts during acquisition (clogs, pressure fluctuations, unstable signal).
- **PeacoQC** — uses **density-based detection** to remove signal drift, low-quality tails, and unstable acquisition regions. The marmots have found PeacoQC tends to be **more aggressive** than flowAI in practice — it removes more cells, sometimes including biologically real but rare populations.

### Use with care

QC steps can substantially alter the dataset, and over-filtering can remove rare populations that are biologically important (the *whole point* of the experiment in some immunology contexts). The marmots recommend:

- Use QC, but **inspect outputs before and after filtering** to see what's being removed.
- Treat QC as support for, not a substitute for, careful manual gating before export.

### Auto-skip on small samples

MARMOT silently skips QC if any sample is too small to QC reliably — **<2000 cells skips QC entirely**, **2000–5000 cells with `runQC = "PeacoQC"` falls back to "None"** with a recommendation to use FlowAI instead. The skip is reported in the rendered HTML's QC section, not as a pipeline error. A user wondering "why didn't QC run?" should check sample sizes first.

## Shiny exploration app

After rendering, `shinyMarmot()` (or the Tauri **Explore** button) launches an interactive app reading from `R_files/`. Lets the user re-colour, re-subset, swap clustering resolutions, and export plots and FCS subsets. See `shiny-app.md`.
