#' @title install_claude_skill
#' @description Install the bundled MARMOT Claude Skill into the user's local
#'   Claude Code skills directory (\code{~/.claude/skills/marmot/}). Once
#'   installed, the skill auto-loads in Claude Code conversations whenever the
#'   user mentions MARMOT, asks about analysing flow data with it, etc. For
#'   the Claude desktop app or claude.ai web app, use
#'   \code{export_claude_skill()} to produce an uploadable zip instead.
#' @param destination Path to the user's Claude skills folder. Defaults to
#'   \code{~/.claude/skills/}. The skill is installed as \code{<destination>/marmot/}.
#' @param overwrite Logical; overwrite an existing \code{marmot/} skill folder
#'   at \code{destination}? Default \code{FALSE} (refuses to clobber).
#' @return Invisibly, the path the skill was installed to.
#' @author Peter Leary
#' @export
#' @examples
#' \dontrun{
#' install_claude_skill()
#' install_claude_skill(overwrite = TRUE)
#' }
install_claude_skill <- function(destination = "~/.claude/skills/",
                                 overwrite   = FALSE) {

  src <- system.file("skills", "marmot", package = "MARMOT", mustWork = TRUE)

  destination <- normalizePath(destination, winslash = "/", mustWork = FALSE)
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }

  target <- file.path(destination, "marmot")

  if (dir.exists(target)) {
    if (!overwrite) {
      stop(
        "A 'marmot' skill already exists at: ", target, "\n",
        "Re-run with overwrite = TRUE to replace it.",
        call. = FALSE
      )
    }
    unlink(target, recursive = TRUE, force = TRUE)
  }

  ok <- file.copy(from = src, to = destination,
                  recursive = TRUE, copy.mode = TRUE)
  if (!ok) {
    stop("Failed to copy the MARMOT skill to: ", target, call. = FALSE)
  }

  message(
    "Installed the MARMOT Claude Skill to:\n  ", target, "\n\n",
    "Open a new Claude Code conversation and mention MARMOT to try it out. \U0001F3D4\n\n",
    "Tip: Claude Code users can also install via the plugin marketplace ",
    "(no manual copy needed):\n",
    "  /plugin marketplace add peterleary/MARMOT\n",
    "  /plugin install marmot@marmot\n"
  )
  invisible(target)
}


#' @title export_claude_skill
#' @description Bundle the MARMOT Claude Skill into a zip file suitable for
#'   uploading to the Claude desktop app or claude.ai web app
#'   (\strong{Customize \eqn{\to} Skills \eqn{\to} "+" button \eqn{\to}
#'   Upload a skill}). Code execution and file creation must be enabled for
#'   skill uploads to be available. For Claude Code users, use
#'   \code{install_claude_skill()} instead, which copies the skill directly
#'   into the skills folder.
#' @param path Path to write the zip file to. Defaults to
#'   \code{~/Desktop/marmot-skill.zip}. If the file already exists it will be
#'   overwritten.
#' @return Invisibly, the path to the written zip file.
#' @author Peter Leary
#' @export
#' @examples
#' \dontrun{
#' export_claude_skill()
#' export_claude_skill(path = "~/Downloads/marmot-skill.zip")
#' }
export_claude_skill <- function(path = "~/Desktop/marmot-skill.zip") {

  src <- system.file("skills", "marmot", package = "MARMOT", mustWork = TRUE)

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  if (file.exists(path)) file.remove(path)

  # Claude requires the zip to contain a single top-level folder ("marmot/")
  # holding SKILL.md and the topic files. We zip the parent of `src` and
  # include only the marmot/ subtree, so the archive's root is marmot/.
  parent_dir   <- dirname(src)
  files_to_zip <- list.files(src, recursive = TRUE, all.files = FALSE)
  rel_paths    <- file.path("marmot", files_to_zip)

  old_wd <- setwd(parent_dir)
  on.exit(setwd(old_wd), add = TRUE)

  # Prefer the `zip` package (pure-R + libzip, works on Windows without a
  # system zip binary). Fall back to utils::zip otherwise.
  if (requireNamespace("zip", quietly = TRUE)) {
    tryCatch(
      zip::zip(zipfile = path, files = rel_paths, mode = "cherry-pick"),
      error = function(e) {
        stop("Failed to create zip at: ", path, "\n",
             conditionMessage(e), call. = FALSE)
      }
    )
  } else {
    tryCatch(
      utils::zip(zipfile = path, files = rel_paths, flags = "-q"),
      error = function(e) {
        stop("Failed to create zip at: ", path, "\n",
             "If you are on Windows without a system zip binary, ",
             "install.packages('zip') and try again.\n",
             conditionMessage(e), call. = FALSE)
      }
    )
  }

  if (!file.exists(path)) {
    stop("Zip command completed but no file was written to: ", path,
         call. = FALSE)
  }

  message(
    "Wrote the MARMOT Claude Skill zip to:\n  ", path, "\n\n",
    "To upload in the Claude desktop app or claude.ai:\n",
    "  1. Click 'Customize' in the left sidebar (under your sessions list).\n",
    "  2. Find the 'Skills' section.\n",
    "  3. Click the '+' button.\n",
    "  4. Choose 'Upload a skill' and pick the zip above.\n",
    "  5. The skill auto-loads when you mention MARMOT, ",
    "or invoke it manually with /marmot.\n\n",
    "Note: code execution + file creation must be enabled in your Claude ",
    "settings for skill uploads to be available.\n\n",
    "Claude Code users: skip the zip and use ",
    "MARMOT::install_claude_skill() instead, or:\n",
    "  /plugin marketplace add peterleary/MARMOT\n",
    "  /plugin install marmot@marmot\n",
    "\nMmm, carrots. \U0001F954"
  )
  invisible(path)
}
