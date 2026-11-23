#!/bin/bash
# quick_check.sh -- the fast check for changes to how the report is drawn.
#
# Redraws the report from a saved run (the pipeline's reload mode, which skips
# the analysis), once with the code at a git ref and once with the working
# tree, and says exactly what differs: report text, figures, tables, PDF and
# Excel files. Takes a few minutes instead of a full run.
#
# Usage, from the top of the repository:
#   MARMOT_QUICK=/path/to/folder tests/reference/quick_check.sh [--against <git ref>] [--force]
#
# $MARMOT_QUICK is a folder outside the repository that holds:
#   seed/            one metadata sheet (.xlsx) and the R_files folder of a
#                    finished run made with it
# and where this script keeps its private R libraries, the redrawn reports
# and their snapshots. The report drawn at the git ref is kept and reused
# until the ref moves.
#
# It refuses to run when change_level.R says the change needs more than a
# redraw (use --force to redraw anyway). Quarto must be on the PATH.
# Exit status: 0 identical, 1 different, 2 could not run, 3 refused.

set -u
ref=HEAD; force=0
while [ $# -gt 0 ]; do
  case "$1" in
    --against) ref="$2"; shift 2 ;;
    --force) force=1; shift ;;
    *) echo "unknown argument: $1"; exit 2 ;;
  esac
done
Q="${MARMOT_QUICK:?set MARMOT_QUICK to the folder that holds seed/}"
here="$(cd "$(dirname "$0")" && pwd)"; repo="$(cd "$here/../.." && pwd)"; cd "$repo" || exit 2
sheet="$(ls "$Q"/seed/*.xlsx 2>/dev/null | head -1)"
if [ -z "$sheet" ] || [ ! -f "$Q/seed/R_files/marmot_results.h5ad" ]; then
  echo "quick_check: $Q/seed needs a metadata sheet and the R_files folder of a finished run"; exit 2
fi

level="$(Rscript "$here/change_level.R" "$ref")"; echo "$level"
case "$level" in
  "LEVEL: report"*|"LEVEL: none"*) ;;
  *) if [ $force -eq 0 ]; then
       echo "quick_check: this change needs more than a redraw of the report. Run the pipeline checks instead."; exit 3
     fi ;;
esac

# Redraw the report from the seed, using the MARMOT installed in library $1, into folder $2
redraw() {
  local lib="$1" dir="$2"
  rm -rf "$dir"; mkdir -p "$dir/Results"
  cp "$sheet" "$dir/"; cp -R "$Q/seed/R_files" "$dir/Results/R_files"
  local md="$dir/$(basename "$sheet")"
  R_LIBS="$lib" Rscript -e '
    a <- commandArgs(TRUE); wb <- openxlsx::loadWorkbook(a[1])
    s <- openxlsx::read.xlsx(a[1], sheet = "Pipeline Settings")
    row <- match("RDataFolder", s$Variable); if (is.na(row)) row <- nrow(s) + 1L
    openxlsx::writeData(wb, "Pipeline Settings", data.frame("RDataFolder", a[2]), startRow = row + 1L, startCol = 1, colNames = FALSE)
    openxlsx::saveWorkbook(wb, a[1], overwrite = TRUE)
    MARMOT::marmot(metadata = a[1], name = "quick", render = TRUE)' "$md" "$dir/Results/R_files" > "$dir/render.log" 2>&1
  local html; html="$(ls "$dir"/Results_Files_*/*.html 2>/dev/null | head -1)"
  if [ -z "$html" ]; then echo "quick_check: the redraw failed, see $dir/render.log"; tail -15 "$dir/render.log"; return 1; fi
  R_LIBS="$lib" Rscript "$here/report_snapshot.R" "$html" "$dir/Results" "$dir/snapshot.json" --mask "$dir" | tail -1
}

# Before: the code at the git ref (kept until the ref moves)
hash="$(git rev-parse --short=12 "$ref")" || exit 2
if [ ! -f "$Q/before_$hash/snapshot.json" ]; then
  echo "== redrawing with the code at $ref ($hash)"
  rm -rf "$Q"/before_* "$Q/src_before" "$Q/lib_before"; mkdir -p "$Q/src_before" "$Q/lib_before"
  git archive "$ref" | tar -x -C "$Q/src_before"
  R CMD INSTALL --no-docs --no-multiarch --library="$Q/lib_before" "$Q/src_before" > "$Q/install_before.log" 2>&1 \
    || { echo "quick_check: could not install the code at $ref, see $Q/install_before.log"; exit 2; }
  redraw "$Q/lib_before" "$Q/before_$hash" || exit 2
fi

# After: the working tree
echo "== redrawing with the working tree"
mkdir -p "$Q/lib_after"
R CMD INSTALL --no-docs --no-multiarch --library="$Q/lib_after" . > "$Q/install_after.log" 2>&1 \
  || { echo "quick_check: could not install the working tree, see $Q/install_after.log"; exit 2; }
redraw "$Q/lib_after" "$Q/after" || exit 2

echo "== before ($ref) against after (working tree)"
R_LIBS="$Q/lib_after" Rscript "$here/report_compare.R" "$Q/before_$hash/snapshot.json" "$Q/after/snapshot.json"
