# MARMOT Development Instructions

## Git Rules

- **NEVER touch main.** Do not checkout main, do not merge into main, do not push to main. Ever.
- We work on the `dev` branch ONLY.
- Push to `origin/dev` only: `git push origin dev`

## Current Release: v1.3.0 ("Hohsaassy")

We are working on v1.3.0. Everything is tagged as v1.3.0. The goal is to get everything spic and span before promoting to main.

## Deployment Procedure

When pushing changes, the sequence is:

1. **Commit and push to dev** — `git push origin dev`
2. **Rebuild Docker images manually** — we are on dev so CI does not auto-trigger
3. **Rebuild Tauri GUI apps manually** — same reason
4. **Replace GUI builds and source code** in the current v1.3.0 GitHub release (titled "Hohsaassy") once builds finish

Do NOT skip steps or combine steps. Do NOT touch main at any point.

## CI Notes

- **Docker** (`docker-publish.yml`): Trigger manually with `gh workflow run docker-publish.yml --ref dev -f ref=dev`. Builds linux/amd64 + linux/arm64. Takes ~20 min.
- **Tauri GUI** (`tauri-build.yml`): Trigger manually with `gh workflow run tauri-build.yml --ref dev`. Builds macOS ARM, Linux x86, Windows x86. Takes ~10 min.
- **No macOS x86 build**: `macos-13` Intel runners were deprecated by GitHub (Mar 2026). Dropped from matrix. ARM build runs on Intel Macs via Rosetta with no performance difference for a webview wrapper.
