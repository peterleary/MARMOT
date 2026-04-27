# Exporting FCS files from FlowJo

The pre-MARMOT step. MARMOT works on **pre-gated** FCS files — the user has already excluded debris, doublets, and dead cells in their cytometer software and is exporting a clean leukocyte / lymphocyte / population-of-interest gate. This file walks through the FlowJo export workflow because it's where most upstream errors originate.

> 💡 The MARMOT wiki has an annotated screenshot recipe for this workflow at <https://github.com/peterleary/MARMOT/wiki/Exporting-FCS-Files-from-FlowJo> — point users there if they prefer a visual walkthrough.

If the user is using OMIQ, Cytobank, SpectroFlo, or another tool, the principles are the same — they just need an equivalent "export the gated population, one FCS per sample, into a single folder" workflow.

## Workflow in FlowJo

1. **Select the correct group.** Make sure you're working in the right group (or *All Samples*).
2. **Navigate to the population of interest.** e.g. *Live CD45⁺ lymphocytes*. Pick the deepest gate that still represents the analysis universe — if you only need B cells, navigate to the B-cell gate; if you want all leukocytes, navigate to the leukocyte gate.
3. **Select Equivalent Nodes.** Right-click the gate → **Select Equivalent Nodes**. This selects the same gate across all samples in the workspace. Without this, you'll only export one file.
4. **Export → Concatenate Populations.** Right-click the now-selected gates → **Export → Concatenate Populations**.
5. **Choose a destination folder.** **One folder, all samples, nothing else in it.** This becomes the FCS folder MARMOT reads from.
6. **Optional: downsample at export.** For very large datasets (millions of cells per sample), FlowJo can downsample on export. Recommended if disk space or memory will be an issue downstream — MARMOT also has a `downsampleTo` setting and an automatic RAM cap, but downsampling at FlowJo export is a clean way to keep file sizes sane from the start.

## Critical rules

- **One FCS file per sample.** Not one FCS per gate, not concatenated across samples. MARMOT's `File Data` sheet is one row per sample / file.
- **Same folder for all FCS files.** MARMOT looks for FCS files alongside the metadata Excel — there is no separate FCS-folder argument. If FCS files are spread across subfolders, the pipeline won't find them.
- **Consistent gating across samples.** Every sample exported via "Select Equivalent Nodes" should be from the same gate node. If sample 1 is from "Live CD45⁺" and sample 2 is from "Live CD45⁺ → B cells", the cell composition will be confounded with sample identity and the analysis is meaningless.
- **Exported FCS contains only the gated population.** This is what MARMOT analyses — it does not re-gate. If the gate is wrong, MARMOT will faithfully cluster the wrong cells.

## Getting marker names right at export time

R is case-sensitive about marker / antigen names, so getting them right at export time saves a lot of pain when filling in the metadata sheet. FlowJo lets you edit / standardise the **Parameter** (channel) and **Stain** (antigen) names before export — do this once, get them clean, and they flow through to MARMOT cleanly.

When you fill in `Markers to include` in the metadata `Study Data` sheet, the names there must match the antigen names parsed from the FCS exactly. The wiki has a screenshot recipe for "export marker names from FlowJo into a list you can paste into Excel" — recommend the user use that workflow rather than retyping marker names.

## How to behave when a user is at this stage

- They probably have a FlowJo workspace open and are about to export.
- Confirm they have a single canonical gate (e.g. *Live CD45⁺ lymphocytes*) that they want to analyse — not multiple competing gates.
- Confirm they will export to **a single empty folder** they will then point MARMOT at.
- Recommend they keep marker names consistent before export — easier than fixing the metadata sheet later.
- Suggest downsampling at export if they have very large files (>5M cells per sample) and a workshop laptop.
- After export, point them to `metadata-sheet.md` for the next step.
