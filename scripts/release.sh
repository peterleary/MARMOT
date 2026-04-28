#!/usr/bin/env bash
#
# scripts/release.sh — cut a new tagged release of MARMOT off main.
#
# Usage:    ./scripts/release.sh <new-version> [--dry-run]
# Example:  ./scripts/release.sh 1.3.3
#           ./scripts/release.sh 1.3.3 --dry-run
#
# Bumps the version in 5 files, commits, pushes main, tags, pushes the tag.
# CI takes over from there:
#   - tauri-build.yml auto-publishes the GitHub Release with desktop builds
#   - docker-publish.yml auto-pushes to ghcr.io/peterleary/marmot:vX.Y.Z
#
# --dry-run: applies the bumps locally, shows the diff, then reverts and exits.
# Nothing is committed, pushed, or tagged. Useful for verifying the regex
# patterns bump the right lines (especially on a new codename or new file).
#
# Codename is NOT auto-bumped. If the codename changes (typically on minor or
# major bumps), edit inst/skills/marmot/SKILL.md and welcome-widget.md by hand
# first, then run this script to bump just the version digits.

set -euo pipefail

# --- Args ---
DRY_RUN=false
NEW_VERSION=""
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=true ;;
    -h|--help)
      echo "Usage: $0 <new-version> [--dry-run]"
      echo "Example: $0 1.3.3"
      exit 0 ;;
    *) NEW_VERSION="$arg" ;;
  esac
done
if [[ -z "$NEW_VERSION" ]]; then
  echo "Usage: $0 <new-version> [--dry-run]"
  exit 1
fi
if ! [[ "$NEW_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "ERROR: version must be x.y.z (got: '$NEW_VERSION')"
  exit 1
fi
NEW_TAG="v$NEW_VERSION"

# --- Resolve repo root ---
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# --- Pre-flight ---
echo "==> Pre-flight"

if [[ -n "$(git status --porcelain)" ]]; then
  echo "ERROR: working tree is dirty. Commit or stash first."
  git status --short
  exit 1
fi

CURRENT_BRANCH="$(git branch --show-current)"
if [[ "$CURRENT_BRANCH" != "main" ]]; then
  echo "ERROR: must be on 'main' (currently on '$CURRENT_BRANCH')"
  exit 1
fi

git pull --ff-only origin main

OLD_VERSION="$(awk '/^Version:/ {print $2}' DESCRIPTION)"
if [[ -z "$OLD_VERSION" ]]; then
  echo "ERROR: could not read current version from DESCRIPTION"
  exit 1
fi
if [[ "$OLD_VERSION" == "$NEW_VERSION" ]]; then
  echo "ERROR: DESCRIPTION already at $NEW_VERSION — nothing to bump"
  exit 1
fi

echo "    branch:  main (clean, up to date)"
echo "    bumping: $OLD_VERSION -> $NEW_VERSION"

# --- Files we bump. Used for pre/post checks and (with directory expansion) for git ops below. ---
BUMPED_FILES=(
  DESCRIPTION
  .claude-plugin/marketplace.json
  inst/skills/marmot/SKILL.md
  inst/skills/marmot/welcome-widget.md
  inst/skills/marmot/setup.md
  tauri-app/src-tauri/tauri.conf.json
  tauri-app/src-tauri/Cargo.toml
  tauri-app/package.json
  tauri-app/package-lock.json
)

# --- Helper: count lines containing $1 in the first 15 lines of file $2 (for package-lock.json) ---
count_in_lockfile_top() {
  awk 'NR<=15' "$2" | grep -c -F "$1" || true
}

# --- Helper: assert a file is at $expected_version (or, for the lockfile, at it twice in the top) ---
file_has_version() {
  local file="$1" expected="$2"
  if [[ "$file" == "tauri-app/package-lock.json" ]]; then
    [[ "$(count_in_lockfile_top "$expected" "$file")" -ge 2 ]]
  else
    grep -qF "$expected" "$file"
  fi
}

# --- Pre-flight: every file must currently contain OLD_VERSION (catches drift) ---
echo "==> Pre-flight: verify all ${#BUMPED_FILES[@]} files are at $OLD_VERSION"
DRIFT=""
for f in "${BUMPED_FILES[@]}"; do
  if ! file_has_version "$f" "$OLD_VERSION"; then
    DRIFT="${DRIFT}  $f does not contain '$OLD_VERSION'"$'\n'
  fi
done
if [[ -n "$DRIFT" ]]; then
  echo "ERROR: version files are out of sync with DESCRIPTION ($OLD_VERSION):"
  printf "%s" "$DRIFT"
  echo
  echo "Manually bump the out-of-sync files to $OLD_VERSION first, then re-run."
  exit 1
fi

# --- Bump in 9 files (perl -i for cross-platform: works on mac BSD + GNU sed boxes) ---
echo "==> Bumping version in ${#BUMPED_FILES[@]} files"
# R package + skill artifacts (5)
perl -i -pe "s/^Version: \Q$OLD_VERSION\E\$/Version: $NEW_VERSION/" DESCRIPTION
perl -i -pe "s/\"version\": \"\Q$OLD_VERSION\E\"/\"version\": \"$NEW_VERSION\"/" .claude-plugin/marketplace.json
perl -i -pe "s/\*\*v\Q$OLD_VERSION\E \(/\*\*v$NEW_VERSION \(/" inst/skills/marmot/SKILL.md
perl -i -pe "s/v\Q$OLD_VERSION\E ·/v$NEW_VERSION ·/" inst/skills/marmot/welcome-widget.md
perl -i -pe "s/should be ≥ \Q$OLD_VERSION\E/should be ≥ $NEW_VERSION/" inst/skills/marmot/setup.md
# Tauri desktop app (4) — version is baked into the binary; drives the About dialog and bundle metadata
perl -i -pe "s/\"version\": \"\Q$OLD_VERSION\E\"/\"version\": \"$NEW_VERSION\"/" tauri-app/src-tauri/tauri.conf.json
perl -i -pe "s/^version = \"\Q$OLD_VERSION\E\"\$/version = \"$NEW_VERSION\"/" tauri-app/src-tauri/Cargo.toml
perl -i -pe "s/\"version\": \"\Q$OLD_VERSION\E\"/\"version\": \"$NEW_VERSION\"/" tauri-app/package.json
# package-lock.json: project version lives on lines 3 and 9; restrict to top of file to avoid touching deps
perl -i -pe "s/\"version\": \"\Q$OLD_VERSION\E\"/\"version\": \"$NEW_VERSION\"/ if \$. <= 15" tauri-app/package-lock.json

# --- Post-bump: every file must now contain NEW_VERSION (catches silent regex misses) ---
echo "==> Verifying every bumped file now contains $NEW_VERSION"
MISSING=""
for f in "${BUMPED_FILES[@]}"; do
  if ! file_has_version "$f" "$NEW_VERSION"; then
    MISSING="${MISSING}  $f did NOT pick up '$NEW_VERSION'"$'\n'
  fi
done
if [[ -n "$MISSING" ]]; then
  echo "ERROR: bump didn't take effect in some files:"
  printf "%s" "$MISSING"
  echo
  echo "Likely a regex mismatch. Check the perl patterns against the actual file contents."
  echo "Working tree is dirty (partial bumps applied). Run \`git restore .\` to undo."
  exit 1
fi

BUMPED_PATHS=(
  DESCRIPTION
  .claude-plugin/marketplace.json
  inst/skills/marmot/
  tauri-app/src-tauri/tauri.conf.json
  tauri-app/src-tauri/Cargo.toml
  tauri-app/package.json
  tauri-app/package-lock.json
)

# --- Confirm before destructive steps ---
echo
echo "==> Diff of bumped files:"
git --no-pager diff --stat -- "${BUMPED_PATHS[@]}"
echo
git --no-pager diff -- "${BUMPED_PATHS[@]}"
echo

if $DRY_RUN; then
  echo "==> DRY RUN — reverting bumped files, no commit/push/tag"
  git restore "${BUMPED_PATHS[@]}"
  echo "✓ Working tree restored. Re-run without --dry-run to do it for real."
  exit 0
fi

read -r -p "==> Commit, push to main, tag $NEW_TAG, push tag? [y/N] " REPLY
if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
  echo "Aborted. Files are bumped but not committed."
  echo "  - To undo:  git restore ${BUMPED_PATHS[*]}"
  echo "  - To keep:  commit manually."
  exit 0
fi

# --- Commit, push, tag, push tag ---
git add "${BUMPED_PATHS[@]}"
git commit -m "Bump version to $NEW_TAG (R package, skill, Tauri app)"
git push origin main

git tag -a "$NEW_TAG" -m "$NEW_TAG"
git push origin "$NEW_TAG"

cat <<EOF

✓ $NEW_TAG pushed to origin.

CI now running:
  • tauri-build.yml   (~10 min)  -> auto-publishes the GitHub Release with desktop builds
  • docker-publish.yml (~20 min) -> ghcr.io/peterleary/marmot:$NEW_TAG

Remaining manual steps:
  1. Draft release notes to /tmp/$NEW_TAG-release-notes.md
  2. gh release edit $NEW_TAG --title "Marmotterhorn ($NEW_TAG)" --notes-file /tmp/$NEW_TAG-release-notes.md
  3. (If tauri-build.yml does not yet upload the skill zip)
       cd inst/skills && rm -f ~/Desktop/marmot-skill.zip
       zip -r ~/Desktop/marmot-skill.zip marmot -x '*.DS_Store'
       gh release upload $NEW_TAG ~/Desktop/marmot-skill.zip
  4. Smoke test — see CLAUDE.md "Release procedure" step 5

EOF
