# Metadata sheet

Every MARMOT run is driven by an Excel file (canonical name **`MARMOT_Metadata.xlsx`**, but the user can rename it). The pipeline reads this file, fills in a Quarto template, and renders. **If the metadata is wrong, the pipeline either fails loudly or — worse — produces a misleading report.** Most user pain happens here, so be careful and concrete.

The Tauri desktop app fills this sheet via a GUI; users on Path B (R) edit it directly in Excel / LibreOffice / Numbers. A blank template ships with the package at `inst/pipeline/MARMOT_Metadata.xlsx` — recommend they start from a copy of that, not a hand-built spreadsheet.

## Sheets in the metadata file

The file has **four sheets** — names and capitalisation matter exactly:

| Sheet | Purpose |
|---|---|
| `Pipeline Settings` | Global pipeline parameters as a `Variable` / `Setting` / `Info` key-value table. |
| `Study Data` | Conditions, contrasts, and the marker/panel definition (one column per concern). |
| `File Data` | One row per FCS file — `file_name`, `sample_id`, `condition`, plus optional extra grouping columns. |
| `Options` | Allowed values for the GUI dropdowns (clustering methods, DR methods, palettes, QC choices). Users **don't edit this**; treat it as read-only reference. |

If a sheet is missing or renamed, `marmot()` errors immediately. Ask the user to share the file (or just the sheet names) if you're unsure.

## `Pipeline Settings` sheet

A three-column key-value table: `Variable` | `Setting` | `Info`. The `Info` column is human-readable description; the pipeline reads only `Variable` and `Setting`. Variables in the shipped template:

| Variable | Typical value | Notes |
|---|---|---|
| `downsampleTo` | `1000` | Cells per FCS to keep. The shipped template default is 1000 — **far too low for most real analyses** (it's there so the template renders quickly as a smoke test). Recommend the user raise this for real runs (or leave blank for "use all, subject to RAM cap"). |
| `clusteringMethodToUse` | `FlowSOM` | One of: `FlowSOM`, `Rphenograph`, `Mphenograph`, `MfastPG`, `PARC`, `Mparc`. See the `Options` sheet for the full list. |
| `markersToClusterBy` | `all` | `all`, `type`, or `state`. Auto-overridden to `all` if the panel is homogeneous. |
| `kValuesIWant` | `20 40 60` | Space-separated integers — k values for FlowSOM metaclusters / kmeans. |
| `knn` | `20` | The k value used downstream (DA/DS, plots). Should be one of the values in `kValuesIWant`. |
| `dimRedMethodToUse` | `UMAP` | `UMAP`, `TSNE`, `pacmap`, `Mpacmap`. |
| `markersToDimRedBy` | `type` | `all`, `type`, or `state`. Note the template default here is `type`, not `all`. |
| `runQC` | `PeacoQC` | `None`, `FlowAI`, or `PeacoQC`. |

Other parameters (`RAMBudgetGB`, `excludeTheseSamples`, `RDataFolder`) can be passed directly to `marmot()` from R rather than the sheet — the sheet covers the most-edited settings.

## `Study Data` sheet

This is where conditions, contrasts, and the panel live. Columns are independent — each holds a list (one item per row, with trailing rows blank).

| Column | What goes in it | Example |
|---|---|---|
| `Conditions Order` | All conditions, in the order they should appear in plots. | `Control`, `Treatment` |
| `Cells per condition in UMAPs etc.` | Per-condition cell count for DR plots. **Evaluated as an R expression**, so `12000/6` = 2000 cells. One value per condition, in the same order. | `12000/6`, `12000/5` |
| `Conditions To Test` | The contrasts (DA + DS comparisons), one per row. **Format: `A over B`** with literal spaces around `over`. A is the numerator, B is the reference. | `Treatment over Control` |
| `Markers to include` | Marker names you want analysed — must exactly match the antigen names parsed from the FCS files. | `CD4`, `CD8a`, `Ki-67` |
| `Marker Type` | The type/state class for each marker, in the **same order** as `Markers to include`. Lowercase: `type` or `state`. | `type`, `type`, `state` |
| `Cofactors for markers to use` | Per-marker arcsinh cofactor (one number per marker, same order). `2500` is a sensible default for spectral. | `2500`, `2500`, `2500` |
| `Markers to exclude completely` | Channels to drop entirely from the imported data — scatter (FSC/SSC), time, dump (LD), CD45, comp channels, etc. | `Time`, `SSC-H`, `FSC-A`, `LD` |
| `Marker Pairs` | Optional: `Label: Marker1 Marker2` strings, one per row. Used to generate biaxial scatter plots in the QC section. | `Tregs: FoxP3 CD4` |

Important things about this sheet:

- **`Markers to include` + `Marker Type` are positionally aligned.** Row 5 of `Marker Type` describes the marker on row 5 of `Markers to include`. Mis-aligning them silently mis-classifies markers.
- **`Markers to exclude completely`** is the right place for scatter / time / dump channels — not "give them a marker class of `none`". Excluded markers never enter the SCE.
- **Contrasts go *here*, not in their own sheet.** Format: `A over B` (literal space-`over`-space).
- **Marker Pairs** is for biaxial QC plots only — it doesn't drive analysis. If unsure, leave blank.

## `File Data` sheet

One row per FCS file. Required columns:

- `file_name` — exact filename of the FCS file, **including the `.fcs` extension** (e.g. `export_A1 Well_001_FMT013_Cleaned Leukocytes.fcs`). Spaces are allowed. Must exist in the FCS folder. Case-sensitive on Linux.
- `sample_id` — short unique label used in plots (e.g. `S001`). Can include letters, numbers, underscores; avoid `-`, `/`, `.`, spaces.
- `condition` — experimental group (e.g. `Treated`, `Control`). Each value must also appear in `Study Data > Conditions Order`.

Optional columns the user can add (any name):

- Anything else (sex, age, timepoint, batch, injection_side…). These become additional grouping factors in the SCE's `colData` and are available as colour / facet / subset options in the Shiny app.

`patient_id` is **not** a column the user provides — the pipeline auto-derives it from `sample_id`.

### Common mistakes in `File Data`

- **`file_name` doesn't match the FCS file**: extra/missing space, wrong case, missing `.fcs`. Pipeline fails at the import stage.
- **Duplicate `sample_id`s**: must be unique across all rows.
- **`condition` value not listed in `Study Data > Conditions Order`**: that sample's condition won't get a colour and ordering will break.
- **Empty rows at the bottom**: Excel sometimes saves trailing blank rows with formatting. Delete them.
- **Special characters in `condition`** (`-`, `/`, `.`): the pipeline rewrites `-` to `0` internally for column safety. Stick to letters, numbers, and underscores.

## `Options` sheet

A reference sheet whose columns are the allowed values for each dropdown:

- Clustering methods: `FlowSOM`, `Rphenograph`, `Mphenograph`, `MfastPG`, `PARC`, `Mparc`
- Marker class options for `markersToClusterBy` / `markersToDimRedBy`: `type`, `state`, `all`
- DR methods: `UMAP`, `TSNE`, `pacmap`, `Mpacmap`
- QC methods: `None`, `FlowAI`, `PeacoQC`
- Palette themes (`prism`): `classic`, `bw`, `minimal`, `void`
- Continuous palettes: `viridis`, `magma`, `plasma`, `inferno`, `cividis`, `mako`, `rocket`, `turbo`
- Boolean: `TRUE`, `FALSE`

The Tauri app reads from this sheet to populate dropdowns. **Users don't edit it**; if a user has changed it, ask them to revert from the shipped template.

## Practical tips and tricks

These come straight from the wiki and from Lydia & Peter's experience teaching MARMOT — almost every user error in metadata is a typo or naming inconsistency, not a real bug.

> 💡 The wiki page <https://github.com/peterleary/MARMOT/wiki/Metadata-Tips-and-Tricks> covers the same material with a screenshot of the FlowJo "export marker names" trick — recommend it for users who'd like to see it in action.

### Get marker names from FlowJo, not from a panel design doc

The marker / antigen names in `Markers to include` must exactly match what's parsed from the FCS files (case included). The reliable way to get them right is to **export the marker list directly from FlowJo** rather than retyping from a panel design document — design docs and the names burnt into the FCS often differ subtly. The wiki has a screenshot recipe; if the user is unsure, point them at it.

### Naming your conditions

Condition values feed into R column names and `model.matrix()` calls — they need to be R-safe.

**Do:**
- Use letters, numbers, and underscores (`_`).
- Start with a letter.
- Keep them short but descriptive.

**Don't:**
- ❌ Start with a number — `1st_group` will break.
- ❌ Use spaces — `Treatment A` will break.
- ❌ Use special characters — the pipeline auto-rewrites `-`→`0`, `+`→`1`, `/`→`""` and spaces→`.` to keep things working, but the easiest path is to avoid them in the first place.

**Examples:**
- ✅ `Control`, `Treatment_A`, `Hpylori_positive`
- ❌ `1st_group`, `Treatment--A`, `Treatment A `

Once a condition name is chosen — **stick to it everywhere**. The exact same string must appear in `File Data > condition`, `Study Data > Conditions Order`, and both halves of every `Conditions To Test` entry.

### Importing file names from FlowJo (don't retype)

Don't type FCS filenames by hand. The reliable workflow:

1. Open the folder of FlowJo-exported FCS files in Finder / File Explorer.
2. Select the files.
3. Copy (`Cmd + C` / `Ctrl + C`).
4. Paste directly into the `file_name` column of `File Data` in Excel.

Excel pastes the filenames as text, including the `.fcs` extension. Fast, clean, no typos.

### Watch for double-spaces in contrasts

The pipeline parses contrasts on a literal `" over "` (one space, the word, one space). A double-space — `TreatmentA over  Control` — will silently fail to match and that contrast gets dropped. Same for `TreatmentA overControl` (no spaces) and `treatmentA over control` (case mismatch).

Best practice: **copy the condition names directly from `Conditions Order`** when filling in `Conditions To Test`. Don't retype.

### Triple-check ritual before running

Before clicking Run, take one minute to verify:

- 🔁 Are condition names spelled **identically everywhere** (`File Data > condition`, `Study Data > Conditions Order`, `Study Data > Conditions To Test`)?
- 🔁 Any accidental spaces hiding at the start or end of cells?
- 🔁 Does every contrast in `Conditions To Test` use exactly two condition names that both exist in `Conditions Order`?
- 🔁 Do `file_name` values in `File Data` match the FCS files in the folder 1:1 (case-sensitive on Linux, including the `.fcs` extension)?
- 🔁 Are `Markers to include` and `Marker Type` the same length?

Most "MARMOT errored on import" reports come down to one of these. If a user reports a problem, suggest they run through this list before anything else.

## What to do when a user shares their metadata

1. Ask for the file (or screenshots of each sheet).
2. Check the four sheets exist with the exact names above.
3. **`File Data`**: check `file_name` ends in `.fcs` (and matches a real file in the FCS folder), `sample_id`s are unique, every `condition` value also appears in `Study Data > Conditions Order`.
4. **`Study Data`**:
   - `Conditions Order` complete and matches `File Data > condition`.
   - `Conditions To Test` uses the literal ` over ` separator and both halves exist in `Conditions Order`.
   - `Markers to include` and `Marker Type` are the same length and positionally aligned. `Marker Type` values are lowercase (`type` / `state`).
   - `Markers to exclude completely` covers scatter, time, dump (LD), and any CD45 / comp channels that shouldn't enter analysis.
5. **`Pipeline Settings`**: `downsampleTo` isn't still 1000 (or, if it is, confirm that's intentional); `clusteringMethodToUse` and `dimRedMethodToUse` use values from `Options`.
6. Suggest fixes; **do not edit the file for them**. Tell them which sheet, which column, which row to change.
