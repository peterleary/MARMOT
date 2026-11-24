# Annotate, then extend; and batch correction

The two big features of MARMOT 2.0. Both are optional and both are switched on
from the metadata sheet or the desktop app.

## Annotate, then extend

The user reads their first report, names the clusters, and MARMOT redraws the
report with extra figures and DA/DS for the named populations. QC, clustering
and the maps are **not** recomputed, so the clusters and cells are exactly the
ones in the first report.

### The labels sheet

Every results folder (`Results_Files_*`) holds `MARMOT_Cluster_Labels.xlsx`:

| Column | What it is |
|---|---|
| `Cluster` | the cluster number (`cluster_id`, the clustering the report is built on) |
| `Cells` | how many cells the cluster has |
| `Share of cells (%)` | its share of all cells |
| `Most distinctive markers` | the markers most above average in it, to help name it |
| `Label` | the name the user gives it (blank at first) |

Rules for the `Label` column:

- Clusters with the **same name merge** into one population.
- A **blank** label keeps the cluster as it is (under its number).
- **`drop`** leaves a cluster out. Only for small, clearly noisy islands: frequencies are then out of the remaining cells, and the report says how many cells this removed from each sample.
- **Capitals, spaces, hyphens and underscores don't count**: "T cells", "t-cells" and "T_Cells" are one population, spelt as first typed.

### Three ways to name the clusters

1. **The Explorer** (Shiny app): *Relabel Clusters* tab, colouring by `cluster_id`. Each cluster's size and distinctive markers show beside the Name column. Type names, click *Apply* to see them on the maps, then **Save for the MARMOT extension**. That writes the names into the sheet (the previous sheet is kept as `MARMOT_Cluster_Labels_<date>_<time>.xlsx`). The Explorer opens on saved labels next time.
2. **Excel**: type straight into the `Label` column and save.
3. Either, then edit again later: the sheet is the single source of truth.

### Running the extension

- **Desktop app**: the **Annotate & extend** tab. Choose the results folder; it shows e.g. "8 of 21 clusters named, in 5 populations; 1 dropped"; then *Run extension*. The app reuses the first run's own metadata (copied into its results folder) with `RDataFolder` set to those results. The new report goes next to the first one.
- **R**: set `RDataFolder` in the Pipeline Settings sheet to the first run's `Results_Files_*` folder and run `marmot()` again.

The new report has a **Labelled populations** tab: which clusters went into each population and what was dropped, a map coloured by population, a marker heatmap, each sample's share of each population, and DA and DS for the populations.

### Common questions

- **"Can I label a different clustering resolution (k)?"** No: labels are for `cluster_id`, the clustering the report is built on. Other columns can be renamed in the Explorer for exploring, but only `cluster_id` names are saved for the extension.
- **"My results are from MARMOT 1.x and have no labels sheet."** Results from before 2.0 can't be extended; run MARMOT 2.0 on the data again.
- **"It says a label names clusters that are not in these results."** The sheet belongs to other results; those labels are ignored and the report says so.

## Batch correction (Harmony)

For samples stained or acquired in different batches.

1. In **File Data**, add a column naming each sample's batch (e.g. `Batch`: `B1`, `B2`, …). Any File Data column works, `patient_id` included, except `file_name`, `sample_id` and `condition`.
2. In **Pipeline Settings**, set `batchColumn` to that column's name. In the desktop app: *Batch column* (a dropdown of the File Data columns) under Preprocessing.

What MARMOT does: a PCA of the clustering markers, corrected for batch with Harmony; clustering and dimension reduction then use the corrected PCs; batch enters the DA and DS models. **Marker values are never changed.** The report gains a *Batch correction* tab (maps before and after, by batch and by condition; each cluster's share of cells from each batch). The Explorer offers "UMAP, before batch correction".

Restrictions and checks (the run stops in plain words, and the desktop app's pre-flight check says so before anything runs):

- Graph clustering methods only (MfastPG, Mphenograph, Mparc, PARC, Rphenograph), not FlowSOM.
- Every sample needs a batch; a batch needs at least two samples; there must be at least two batches.
- Batch must not be the condition under another name (each batch holding only one condition, and each condition only one batch), and the design must leave samples to estimate the noise.
- Needs the `harmony` package (`MARMOT::install_marmot_extras()` installs it).

If a user's batches and conditions are completely confounded, explain that no method can separate the two effects, and that the design has to change (or batch correction be left off).
