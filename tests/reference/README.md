# Reference checks

Two scripts for answering one question: **did a change to MARMOT alter the
results?**

- `fingerprint.R` summarises a MARMOT results folder as a small JSON file.
- `compare.R` reports exactly what differs between two of those files.

They are run by hand, not by `devtools::test()`.

## How to use them

1. Run the pipeline on a dataset with the version of MARMOT you trust, and
   fingerprint the results folder:

   ```bash
   Rscript tests/reference/fingerprint.R path/to/Results_Files_... before.json
   ```

2. Make your change, reinstall MARMOT, run the pipeline again on the same
   dataset with the same metadata sheet, and fingerprint the new folder:

   ```bash
   Rscript tests/reference/fingerprint.R path/to/Results_Files_... after.json
   ```

3. Compare:

   ```bash
   Rscript tests/reference/compare.R before.json after.json
   ```

`compare.R` prints one line per section, `IDENTICAL` or `DIFFERENT`, then the
details of every difference, and ends with `RESULT: IDENTICAL` or
`RESULT: DIFFERENT`. Its exit status is 0 if identical, 1 if different and 2 if
it could not read its inputs.

Numbers are compared exactly by default. To allow small numerical differences,
add `--tolerance`, for example `--tolerance 1e-8`.

## What a fingerprint covers

- Cell counts, in total and per sample.
- Every expression layer: its size, a digest, and the mean of each marker.
- Every clustering, and the cluster-by-sample counts.
- Every embedding (PCA, UMAP, t-SNE and any other that was run).
- The DA and DS tables, and the clusters selected as significant.
- Whether the two saved copies of the DA and DS results agree with each other.
- Every sheet of every Excel file.
- Any warnings raised while loading the results.

The folder's timestamp is left out, so two runs of the same analysis can match.

## What it does not cover

- **Figures and the HTML report.** A broken plot with correct numbers behind it
  would pass.
- **Anything MARMOT's loader cannot read.** The fingerprint reads results
  through the installed MARMOT package, so it depends on that loader working.

## Keep real data out of the repository

A fingerprint contains sample names, cell counts and statistics from the
dataset it was made from. Keep fingerprints of real data outside the
repository, alongside the data, and never commit them.

## Which methods should match

Two runs with the same settings should give identical fingerprints for FlowSOM
and for MARMOT's own methods (MfastPG, Mphenograph, Mparc, Mpacmap). In our
tests the Python PARC also repeated, and the Python PaCMAP repeats now that
the pipeline sets its seed. Both depend on the installed Python packages, so
check them again if those change.

## The quick check for report changes

Running the whole pipeline to check a change that only alters how the report
is drawn wastes most of an hour. Three more scripts make that a few minutes:

- `change_level.R` looks at the uncommitted changes and says how much
  checking they need: `report` (only chunks that run when results are
  reloaded), `analysis` (a chunk that is skipped on reload, so the pipeline
  has to run again) or `everything` (code outside the pipeline QMD).
- `report_snapshot.R` summarises what a report shows: every line of text, a
  digest of every figure and interactive table, and a digest of every PDF,
  Excel and CSV file written beside it. Code listings are left out.
- `report_compare.R` says exactly what differs between two snapshots.

`quick_check.sh` ties them together. It redraws the report from a saved run
(the pipeline's reload mode, which skips the analysis) twice, once with the
last commit and once with the working tree, and compares the two:

```bash
MARMOT_QUICK=/path/to/folder tests/reference/quick_check.sh
```

`$MARMOT_QUICK` is a folder outside the repository holding `seed/`: one
metadata sheet and the `R_files` folder of a finished run made with it. The
script refuses to run if `change_level.R` says the change needs more than a
redraw.

What it cannot see:

- Anything drawn by a chunk that is skipped on reload (the QC and cofactor
  figures, for example). Those need a full run.
- Anything the saved run does not exercise. If the seed data has no
  significant DA clusters, a change to how DA clusters are labelled changes
  nothing in the report, and the check passes.

So it is a fast first check, not a replacement for the full reference runs
before a step is merged.
