# MARMOT 2.0.0 (in development; builds are numbered 1.99.0 until release)

*Draft: written as the steps are merged, and finalised at release.*

**Results may differ** from 1.3.2, and the differences are listed below. Results folders made by MARMOT 1.x cannot be opened by 2.0; you get a clear message saying so.

## Added

- **Annotate, then extend.** After the first report, name the clusters (in the Explorer or in `MARMOT_Cluster_Labels.xlsx`), and MARMOT redraws the report with a *Labelled populations* tab: populations, a map, a marker heatmap, each sample's share of each population, and DA and DS per population. Nothing is clustered again. Clusters with the same name merge, a blank keeps a cluster, `drop` leaves one out. The desktop app has an **Annotate & extend** tab, and the Explorer saves names straight into the labels sheet.
- **Batch correction with Harmony** (`batchColumn`). The cells' positions are corrected before clustering and dimension reduction; batch enters the DA and DS models; marker values are never changed. A *Batch correction* report tab shows the maps before and after. The design is checked before anything runs, in the pipeline and in the desktop app.
- **Forgiving names.** Capitals, spaces, hyphens and underscores no longer break a run: "IL7R" and "Il7R", or "CD8a" and "CD8-A", line up, across FCS files, the metadata sheet, conditions and contrasts. The report says what it lined up.
- **A fancier report.** Overview and Report provenance tabs, a Find panel, downloads, figure captions, and a figure-resolution setting. The provenance tab says which methods ran and whom to cite. Every figure and table is centred and has a `</>` button that shows the code behind it. The Pipeline Settings Summary lists every setting, and the markers, conditions and contrasts the run used. *Methods, and whom to cite* starts with the MARMOT paper and cites the papers behind each method (UMAP, t-SNE, FastPG, PARC, PaCMAP, FlowAI, PeacoQC, clustree and more), not just the R packages.
- **Greek letters in figures.** Names with Greek letters (a condition "IFN-γ treated", a population "γδ T cells") are drawn properly everywhere, PDFs included, with R's plain PDF device (so files stay friendly to Affinity and Illustrator). A new optional column, *Display names for markers*, sets how each marker is named in figures (IFN-γ, CD8α) while matching keeps the marker's own name. The desktop app has Greek letter buttons for those cells.
- **Choose the condition colours.** A new optional column, *Condition colours*, beside *Conditions Order* in the template, gives a condition its colour in every figure and in the Explorer: a colour name (`tomato`) or a code (`#E41A1C`). Blank keeps the automatic colour, so sheets without the column give the same figures as before. The desktop app's pre-flight checks the entries.
- **Manuscript-style maps** in the report and the Explorer: no axes, a fixed aspect ratio, a black halo around the cells, and two corner arrows naming the axes.
- **Installation without compiling.** MARMOT and its R packages come ready-built from `peterleary.r-universe.dev`. The original Python PARC and PaCMAP are set up with pixi, with no conda needed. Quarto is found automatically (RStudio's and Positron's copies count), and `check_setup()` says in plain words what works.

## Changed

- **Licence: GPL-3 or later** (was MIT). Anything shared that builds on MARMOT must be shared under the same terms.
- **Results format 2.** Results are saved in one `marmot_results.h5ad` with a version check.
- **Speed.** Mphenograph and Rphenograph share one neighbour search across all k values (the large reference run went from about 29 to 22 minutes, with identical clusters). The halo on every map makes the report slower again, about 10% on small runs.
- **One list of settings** shared by the metadata template, the pipeline and the desktop app. Old settings (`runInParallel`, `themeToUse`, `condaDir`, `parcScriptDir`) are ignored with a message.
- **The Explorer.** Every map is listed once with a plain name, starting on the report's version, with "every cell" and "before batch correction" alongside. Feature plots fit their box and keep their markers when the plot type changes. Large runs start rasterised. Maps and feature plots start with a dot size of 1.3, and cell borders 2 thick.
- **The desktop app.** The batch column is a dropdown of the File Data columns.
- `Median_Markers_Per_Sample_Expression.xlsx` no longer has a `patient_id` column (it was a copy of `sample_id`). A user's own `patient_id` column is now kept, and can be a batch.
- **Contrasts** may be written "A vs B", "A versus B" or "A v B" as well as "A over B"; the Overview says how each was read. An entry that cannot be read as two conditions now stops the run, where before it was silently skipped.
- The report and the documentation say plainly that MARMOT's own versions (MfastPG, Mphenograph, Mparc, Mpacmap) are fall-backs, not validated or supported, and ask you to use and cite the originals. The report names the *main k* (used for DA, DS and most figures) and says how far re-runs were tested to repeat.
- The by-condition DA map PDFs are named like the others (`Contrast_1_..._subset.pdf`).
- **A built-in example that works.** The Explorer opens on MARMOT 2.0 results from the public PBMC data that comes with CATALYST (Bodenmiller et al. 2012; Ref vs BCR/FcR-XL, 8 samples, 24 markers), and the example now ships with the package. `inst/examples/make_example.R` rebuilds it. The old 1.x example is gone.
- t-SNE uses `nCores` threads where Rtsne can (Linux and Docker; the macOS build has one thread). Results are unchanged.
- The *markers by cluster* boxplots label their axis "Marker" (it said "Condition").
- **A reload shows the first run's warnings again** (memory cap, downsampling, QC skipped, *Names lined up*).
- **Labelled populations are saved in the results**: an extension run records which population each cluster became, adds *Populations* and *Per sample* sheets to `Labelled_populations_DA_DS.xlsx`, and the Explorer offers a *population* column wherever clusters are.

## Fixed

- **QC judged channels that are not biology**, and some it could not read. FCS 3.2 files (from newer BD instruments such as the FACSDiscover S8) can store a few channels as whole numbers; flowCore reads those as nonsense, and FlowAI and PeacoQC then threw out good cells because of them (a third of the cells in one test file). QC now leaves out such channels on any instrument, plus a list of non-biology channels for each instrument we know (imaging features, light loss and instrument flags on the FACSDiscover S8). The QC tab lists what was left out, the Overview warns about unreadable channels, and a marker on an unreadable channel stops the run. **Results with QC on may differ for files with such channels**; Aurora files are unaffected.
- **scGate gated nothing:** every cell came out "Negative", because MARMOT looked for a value scGate never writes. Cells that pass a gate are now kept, and each gets the best-scoring gate among those it passed. **Results with `runScGate` on will differ.**
- The Explorer no longer greys out (ends the session) on an error. Among the crashes fixed: clearing a subset box, an unreadable top-marker table, and a bad table edit.
- The Explorer showed the wrong DA clusters for a contrast.
- Explorer plots that went stale or wrong after relabelling or subsetting, the DS heatmap for "Merged" contrasts, the DA filter when colouring by other columns, and "Heatmap per cell" on Macs without XQuartz.
- Cofactor histograms were transformed twice when `useQC` was on.
- QC'd files are reused only when the inputs and QC settings are unchanged.
- Condition names and marker names can contain any characters in the DA/DS statistics and in file names.
- A condition missing from Conditions Order now stops the run at once, in plain words.
- The Python PaCMAP is seeded, so it repeats exactly.

## Internal

- A reference-run safety net (results fingerprints compared cell by cell), and pipeline render tests.
- The pipeline QMD was reorganised chunk by chunk, with every change reviewed.
