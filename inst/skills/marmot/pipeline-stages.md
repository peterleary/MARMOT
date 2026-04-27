# Pipeline stages

What each section of the rendered HTML report contains and what the user is meant to learn from it. Use this when a user opens the report and asks "what does this section mean?" or "is this number good?"

The sections appear in the order the pipeline runs them.

## 1. Setup / parameters

A summary of how the run was configured: input folder, sample count, panel size, clustering method, contrasts, downsample setting.

**What to look for:** confirm the run actually used what they intended. If the parameters table shows `downsampleTo: 1000` and they expected the full data, that's a metadata-sheet bug.

## 2. FCS import + QC

For each FCS file: events imported, events surviving QC, percentage retained.

QC is **PeacoQC** (default) or **FlowAI**. It flags time-window anomalies — flow-rate spikes, signal drift — and excludes cells in those windows.

**What to look for:**

- Samples with very low retention (<70%) — possibly poor acquisition; flag for the user.
- Samples with 0 events — pre-gating excluded everything; broken FCS.
- A `QCmini` table with per-sample event counts. (Pre-v1.3.1 had a Pandoc bug here that broke layout — confirm version if anything looks misformatted.)

## 3. Panel summary

Lists every marker, its `fcs_colname`, antigen, and `marker_class`. Sometimes shown as a kable, sometimes a heatmap.

**What to look for:** the user should sanity-check that each marker is in the right class. Common surprise: a forgotten dump channel showing as `state`.

## 4. Clustering

The chosen algorithm runs on the markers selected by `markersToClusterBy` (default `"all"`; also `"type"` or `"state"`) and produces `cluster_id`. Multiple resolutions are computed and stored (e.g. `meta10`, `meta20`, `k20`).

If the panel is **homogeneous** (only `type` markers, or only `state` markers), MARMOT auto-detects this and clusters on all markers regardless of what `markersToClusterBy` is set to — there's no useful subset to pick from.

The report shows:

- A heatmap of mean expression per cluster across the clustering markers (`plotExprHeatmap`) — used to give clusters a biological identity.
- Cluster sizes (cell counts per cluster).
- Per-sample cluster proportions (`plotFreqHeatmap`).

**What to look for:**

- Expected lineages appear (CD4 T, CD8 T, B, NK, monocytes, etc., depending on the panel).
- No single cluster dominates (>50% of all cells often means under-clustering).
- No cluster has near-zero cells in most samples (often means over-clustering).

## 5. Dimensionality reduction (DR)

UMAP (and optionally PaCMAP) coordinates, plotted coloured by:

- Cluster ID
- Sample
- Condition
- Each marker (feature plots)

**What to look for:**

- Clusters separate visibly in the UMAP. Total overlap suggests the clustering markers don't carry enough signal.
- Condition-coloured plots: heavy left/right separation can indicate batch effect rather than biology — sanity check against the `batch` colouring.
- Density borders (added in v1.3.1) emphasise cluster cores; if a "cluster" has no dense core in the embedding, treat it cautiously.

## 6. Differential abundance (DA)

For each contrast (`A over B`, meaning A relative to B as reference), a table per cluster: log fold change, p-value, FDR-adjusted p-value, model used.

**What to look for:**

- FDR threshold applied (typically 0.05).
- Direction of change (positive logFC = enriched in the first half of the contrast).
- Confidence intervals or boxplots if generated — single-replicate "significance" is a warning sign.

## 7. Differential state (DS)

For each contrast × cluster × `state` marker: a similar table.

Note: pre-v1.3.1 the DS tab in the rendered HTML often appeared empty due to a fenced-div nesting bug from `cat()` inside `for` loops. If the user reports an empty DS tab, check version first.

**What to look for:**

- Markers that change in specific clusters rather than globally — that's the typical interpretation.
- Effect sizes alongside p-values; tiny effects with low p-values in huge datasets are usually not biologically meaningful.

## 8. Session info

The exact R + package versions used. Useful when reporting bugs.

## After the report

The user typically opens the Shiny exploration app (`shinyMarmot()` or the Tauri **Explore** button) to dig deeper. See `shiny-app.md`.

## How to behave when answering report questions

- Tell them what the section *shows*, not what it *means biologically*. You are not interpreting their experiment.
- If they ask "is this result real?" — that's their statistical / biological judgment. Help them understand the numbers, not whether they should celebrate.
- If a section looks broken (formatting, missing tab, wrong cluster IDs), check the version against the bugs noted in `troubleshooting.md`.
