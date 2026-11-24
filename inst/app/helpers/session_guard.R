# Session guard
# In Shiny, an error inside an observer ends the whole session: the grey
# screen. The Explorer's server makes every observer with the two functions
# below instead of Shiny's own (see app.R), so an error is caught, logged and
# shown as a message, and the Explorer keeps running. Shiny's "not ready yet"
# signals (req(), validate()) pass through quietly, as before.

#' Run an observer's code; an error becomes a message, never the end of the
#' session
#' @param where Where the code runs, for the log
#' @param expr The observer's code
marmot_guard <- function(where, expr) {
  tryCatch(expr,
    shiny.silent.error = function(e) invisible(NULL),
    error = function(e) {
      message("Explorer error in ", where, ": ", conditionMessage(e))
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shiny::showNotification(
          paste0("The marmots tripped over something (", conditionMessage(e),
                 "). The Explorer is still running; please try again or report it."),
          type = "error", duration = 12)
      }
      invisible(NULL)
    })
}

#' shiny::observeEvent(), with the handler guarded by marmot_guard()
guarded_observeEvent <- function(eventExpr, handlerExpr, ..., label = NULL) {
  ev <- substitute(eventExpr)
  where <- label %||% deparse(ev, width.cutoff = 60L)[1]
  shiny::observeEvent(
    ev, bquote(marmot_guard(.(where), .(substitute(handlerExpr)))),
    event.env = parent.frame(), event.quoted = TRUE,
    handler.env = parent.frame(), handler.quoted = TRUE,
    ..., label = label)
}

#' shiny::observe(), with the code guarded by marmot_guard()
guarded_observe <- function(x, ..., label = NULL) {
  ex <- substitute(x)
  where <- label %||% paste("observe:", deparse(ex, width.cutoff = 60L)[2])
  shiny::observe(bquote(marmot_guard(.(where), .(ex))),
                 env = parent.frame(), quoted = TRUE, ..., label = label)
}
