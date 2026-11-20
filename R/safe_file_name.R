# Plain-letter spellings for characters that are fine in a report but risky in
# a file name. Keyed by Unicode code point so this file stays plain ASCII and
# the result does not depend on the language setting R is running under.
.plain_letters <- local({
  map <- list()
  add <- function(code_points, replacements) {
    for (i in seq_along(code_points)) map[[as.character(code_points[i])]] <<- replacements[i]
  }
  greek <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
             "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho",
             "sigma", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega")
  add(0x3B1:0x3C9, greek)                                   # lower-case Greek
  capitalise <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  add(c(0x391:0x3A1, 0x3A3:0x3A9), capitalise(greek[-18]))  # upper-case Greek
  add(c(0x2070, 0xB9, 0xB2, 0xB3, 0x2074:0x2079), as.character(0:9))   # superscripts
  add(0x207A:0x207F, c("+", "-", "=", "(", ")", "n"))
  add(0x2080:0x2089, as.character(0:9))                                # subscripts
  add(0x208A:0x208E, c("+", "-", "=", "(", ")"))
  add(c(0xB5, 0xB1, 0xD7, 0xB0, 0xA0), c("u", "+-", "x", "deg", " "))  # micro, +-, times, degree
  add(c(0x2010:0x2015, 0x2212), rep("-", 7))                           # dashes and minus
  add(c(0x2018, 0x2019, 0x201C, 0x201D), c("'", "'", "", ""))          # curly quotes
  add(0xC0:0xC5, rep("A", 6)); add(0xE0:0xE5, rep("a", 6))             # accented Latin
  add(0xC8:0xCB, rep("E", 4)); add(0xE8:0xEB, rep("e", 4))
  add(0xCC:0xCF, rep("I", 4)); add(0xEC:0xEF, rep("i", 4))
  add(c(0xD2:0xD6, 0xD8), rep("O", 6)); add(c(0xF2:0xF6, 0xF8), rep("o", 6))
  add(0xD9:0xDC, rep("U", 4)); add(0xF9:0xFC, rep("u", 4))
  add(c(0xC7, 0xE7, 0xD1, 0xF1, 0xDD, 0xFD, 0xFF), c("C", "c", "N", "n", "Y", "y", "y"))
  add(c(0xDF, 0xC6, 0xE6), c("ss", "AE", "ae"))
  map
})

#' Turn any name into a safe file name
#'
#' Condition, contrast and marker names can contain anything, but file names
#' cannot: a `/` is read as a folder, and Greek letters or superscripts cannot
#' be written at all when R runs with a plain language setting, as it does in
#' Docker. The report shows names as typed; files get a plain-letter version.
#'
#' Greek letters are spelled out, subscript and superscript characters become
#' ordinary ones, accents are dropped, and anything a file system forbids
#' (`/ \ : * ? " < > |`) or that is not plain ASCII becomes `_`.
#'
#' @param x Character vector of names.
#' @return Character vector of the same length, safe to use as file names.
#' @examples
#' safe_file_name("\u03b1-PD1 (high) 5mg/kg over IFN-\u03b3\u207a H\u2082O\u2082")
#' @export
safe_file_name <- function(x) {
  forbidden <- utf8ToInt("/\\:*?\"<>|")
  vapply(as.character(x), function(name) {
    if (is.na(name)) return("NA")
    pieces <- vapply(utf8ToInt(enc2utf8(name)), function(code_point) {
      plain <- .plain_letters[[as.character(code_point)]]
      if (!is.null(plain)) return(plain)
      if (code_point < 32 || code_point > 126 || code_point %in% forbidden) return("_")
      intToUtf8(code_point)
    }, character(1))
    safe <- sub("[. ]+$", "", paste(pieces, collapse = ""))   # Windows forbids these at the end
    if (nzchar(safe)) safe else "unnamed"
  }, character(1), USE.NAMES = FALSE)
}
