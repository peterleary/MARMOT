# Shiny exploration app

Launched with `shinyMarmot("path/to/output/R_files/")` or the Tauri **Explore** button. Reads from the `R_files/` folder produced by the pipeline. Lets the user re-plot, re-subset, recolour, relabel, and export.

The app is themed in **Crimson** (zinc + red on near-white).

## Tabs

### Import / overview

Loads the SCE from `marmot_results.h5ad` plus the sidecars (`da_ds_results.qs2`, `pipeline_settings.json`). Shows sample / panel summaries.

The app auto-enables **rasterisation** when `ncell > 150,000` (uses `geom_scattermore`) for performance; the Feature Plot raster checkbox is auto-ticked.

### Colours

- Pickers for cluster colours, condition colours, batch colours, marker colour scales.
- Palette presets (sequential / diverging / qualitative).
- Changes apply across the app — not just the current plot.

### Cluster relabelling

A table where the user types a biological name for each cluster (e.g. cluster 3 → "CD4 T effector"). *Apply* adds a `relabelled_clusters` column to the SCE in memory; `cluster_id` itself is never changed. The relabelled column becomes available everywhere the user could colour by `cluster_id`.

When relabelling `cluster_id`, the table also shows each cluster's size and most distinctive markers, and **Save for the MARMOT extension** writes the names into the results' `MARMOT_Cluster_Labels.xlsx`. The Explorer opens on those saved names next time. See `annotate-and-batch.md` for the rules (same name merges, `drop`, blank keeps).

### Subset

Drops cells from the displayed plots. Three modes:

- **None** — use everything.
- **Absolute** — fixed N cells per group.
- **Proportional** — sample N cells per group proportional to group size.

Subsetting is for visualisation only; doesn't change the underlying SCE on disk.

### DR (dimensionality reduction)

The big UMAP / PaCMAP scatter plot. Two render paths:

- **Static ggplot2** — for export.
- **Interactive plotly** — for exploration.

Controls:

- **Column to plot** (`umapColumnToPlot`): any `colData` column with <100 unique levels — `cluster_id`, `relabelled_clusters`, `condition`, `sample_id`, `batch`, `meta10`, `meta20`, `k20`, etc.
- **DR method** (`umap` / `pacmap`).
- **Border style**: None / Per-cell / Density contours.
  - **Per-cell**: `pch=21`, customisable border size + colour.
  - **Density contours**: `stat_density_2d` with adjustable line width, threshold (default 0.4), and colour.
- **Density borders (kde2d)**: a 3-layer "sandwich" — large dark border cells underneath, grey base, coloured foreground. Driven by MASS::kde2d and a quantile threshold; emphasises cluster cores. Available alongside the contour option.
- Cluster labels: white bold `geom_label_repel` over `ggnewscale::new_scale_fill()`; legend in 2 columns at size 3, alpha 1.

If the user asks about cluster labels: they're computed via `compute_label_positions()` from the helpers; resilient to cluster-ID switching.

### Feature Plot

Coloured DR scatter, one panel per marker.

- **Pre-extracts** the entire expression matrix once (`extract_expr_matrix()`), then loops only over ggplot — ~10× faster than naive per-marker extraction.
- Markers selected via a `selectizeInput` backed server-side by `sorted_markers_cache` (mixedsort of rownames).
- **Click-to-add**: a DT marker table; user ticks rows and clicks "Add selected markers to plot list" → markers append via `updateSelectizeInput`.
- **Per-group subset**: `fpSubsetMode` radio — None / Absolute / Proportional.
- Layout via `patchwork::wrap_plots(..., ncol = input$ncolFPGene)`.
- Default `alpha = 0.6`. SCpubr-style `coord_fixed()` applied.

### Nebulosa (density)

Marker-density visualisation in DR space. Implemented via `ks::kde` weighted scatter (rewritten from `Nebulosa::plot_density()` to remove the dependency). Reuses `make_feature_scatter()` so the visual style matches the Feature Plot.

### Other plots

- **Heatmap** — per-cell or per-cluster marker expression.
- **Violin / box / ridge / dot / bar plots** — distributions of markers per cluster or condition.

All built from the same helpers (`make_violin_plot`, `make_dot_plot`, `make_ridge_plot`, `make_percell_heatmap`, `make_barplot`).

### Differential abundance (DA) / Differential state (DS)

Reads `da_ds_results.qs2`. Tables filterable by contrast, cluster, marker. Includes a contrast dropdown (case-insensitive matched against pipeline output).

### Download

- **Plot exports**: PDF, SVG, PNG of the currently visible plot.
- **FCS export**: writes one FCS file per sample (zipped), holding only the cells in the report. The cells are read from the original FCS files, with UMAP x/y and cluster-code columns appended, plus a `cluster_codes.xlsx` explaining the codes. Useful for re-importing into FlowJo / similar. If the original FCS files have moved, type their folder into the box under the button; a file that has changed since the analysis is refused.
- **Data export**: CSV of the colData / DR coordinates / DA-DS tables.

## Behaviour notes

- The app uses **adaptive debouncing**: UMAP inputs at 500–700 ms, Feature Plot inputs at 100–400 ms depending on cell count. So small UI changes feel snappy but don't trigger a re-render mid-edit.
- **`data.table::chmatch`** is used for cell relabelling — O(n), fast even on millions of cells.
- The reference visual style is from **exploreSingleCell** (an FGCZ Shiny app) and adheres to **SCpubr** conventions (`coord_fixed()`, white bold cluster labels, alpha tuning). If the user sees an unfamiliar visual choice, this is the lineage.

## Common questions

- **"Why don't my clusters look the same colour as in the report?"** — palettes can differ; use the Colours tab to fix and re-export.
- **"Can I save the relabelling?"** — yes, for `cluster_id`: *Save for the MARMOT extension* in the Relabel Clusters tab. The names go into `MARMOT_Cluster_Labels.xlsx` in the results folder and are used by the extension (see `annotate-and-batch.md`).
- **"Why does it say `meta10` and `meta20`?"** — alternative clustering resolutions stored alongside `cluster_id`. See `concepts.md` and `glossary.md`.
- **"How do I export the clusters as FCS?"** — Download tab → FCS files. One FCS file per sample, with a cluster-code column; gate on that column in FlowJo to pick out a single cluster.

## What you should *not* do

- Don't suggest editing the SCE on disk through the app — the app is a viewer, not an editor.
- Don't claim a tab or button exists without checking — the UI evolves; ask the user what they see if you're unsure.
