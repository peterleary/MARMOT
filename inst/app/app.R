# ============================================================================
# Shiny MARMOT v2 — app.R
# Main entry point for the MARMOT Shiny app.
# Adapted from exploreSingleCell with Crimson theme and SCE data model.
# ============================================================================

# Only load packages needed when running standalone.
# Everything else is available via the MARMOT R package namespace.
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(fresh)
  library(waiter)
  library(plotly)
  library(DT)
  library(ggplot2)
  library(patchwork)
})

# Source helper files
source("helpers/data_helpers.R",   local = TRUE)
source("helpers/colour_helpers.R", local = TRUE)
source("helpers/plot_helpers.R",   local = TRUE)

# ── Forest Theme ─────────────────────────────────────────────────────────────
# Forest green primary (#013220), darker shade (#012418), orange accent
# (#ea580c / #c2410c), zinc secondary (#3f3f46), near-black navy (#18181b)
my_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#013220",
    aqua       = "#3f3f46",
    green      = "#27272a",
    navy       = "#18181b",
    orange     = "#ea580c"
  ),
  fresh::adminlte_sidebar(width = "400px")
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  title = "Shiny MARMOT",

  # ── Header ──────────────────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(
        src    = "MARMOT_Logo_2_bw_small.png",
        height = "46px",
        width  = "auto",
        class  = "me-3",
        alt    = "MARMOT"
      ),
      "Shiny MARMOT"
    ),
    # Bug report link
    tags$li(
      a(
        href   = "https://github.com/peterleary/marmot/issues",
        target = "_blank",
        "Report Bugs"
      ),
      class = "dropdown"
    ),
    # Institutional logos
    tags$li(
      a(
        href   = "http://www.fgcz.ch",
        target = "_blank",
        img(src = "fgcz_logo.png", title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href   = "http://www.ethz.ch/en.html",
        target = "_blank",
        img(src = "eth_logo.png", title = "ETH Zurich", height = "22px"),
        style = "padding-top:13px; padding-bottom:10px;"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href   = "http://www.uzh.ch/en.html",
        target = "_blank",
        img(src = "University_of_Zurich_Logo.png", title = "University of Zurich", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"
      ),
      class = "dropdown"
    )
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Explorer",  tabName = "umapTab",     icon = icon("map")),
      menuItem(text = "Analysis",  tabName = "analysisTab", icon = icon("chart-bar"))
    ),

    # ── Download Figures ────────────────────────────────────────────────
    tags$hr(style = "border-color: #3f3f46; margin: 10px 15px;"),
    tags$div(
      style = "padding: 0 15px;",
      h4("Download Figures", style = "color: #a1a1aa; font-size: 0.85rem; margin-bottom: 8px;"),
      selectInput(
        inputId  = "dlFormat",
        label    = NULL,
        choices  = c("PDF", "SVG", "PNG"),
        selected = "PDF",
        width    = "100%"
      ),
      conditionalPanel(
        condition = "input.dlFormat == 'PNG'",
        sliderInput(
          inputId = "pngRes",
          label   = "PNG Resolution",
          min     = 100, max = 1000, value = 600, step = 100,
          width   = "100%", ticks = FALSE
        )
      ),
      tags$div(
        style = "display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 10px;",
        downloadButton(outputId = "dlUMAP", label = "DR Plot",
                       style = "flex: 1; min-width: 80px;"),
        downloadButton(outputId = "dlFP",   label = "Feature Plot",
                       style = "flex: 1; min-width: 80px;")
      ),

      # ── Download Data ──────────────────────────────────────────────
      tags$hr(style = "border-color: #3f3f46; margin: 8px 0;"),
      h4("Download Data", style = "color: #a1a1aa; font-size: 0.85rem; margin-bottom: 8px;"),
      downloadButton("downloadInputsE",    "Settings (xlsx)",
                     style = "width: 100%; margin-bottom: 4px;"),
      downloadButton("downloadFCS",        "FCS files",
                     style = "width: 100%; margin-bottom: 4px;"),
      downloadButton("downloadClusterCodes", "Cluster codes",
                     style = "width: 100%; margin-bottom: 10px;")
    ),

    # ── Citation ────────────────────────────────────────────────────────
    tags$hr(style = "border-color: #3f3f46; margin: 10px 15px;"),
    tags$div(
      style = "padding: 10px 15px; color: #a1a1aa; font-size: 0.80rem; line-height: 1.5;",
      tags$p(
        tags$b("MARMOT"),
        tags$br(),
        "Kirsche L, He J, Muller A, Leary P (2025)",
        tags$br(),
        tags$em("J. Immunological Methods"),
        tags$br(),
        tags$a(
          href   = "https://doi.org/10.1016/j.jim.2025.113854",
          target = "_blank",
          style  = "color: #dc2626;",
          "doi:10.1016/j.jim.2025.113854"
        )
      )
    ),
    collapsed = TRUE
  ),

  # ── Body ────────────────────────────────────────────────────────────────
  dashboardBody(
    use_theme(my_theme),
    use_waiter(),

    # Head: favicon + Forest Theme CSS
    tags$head(
      tags$link(rel = "shortcut icon", href = "MARMOT_Logo_2_bw_small.png"),
      tags$style(HTML("
        /* ── Forest Theme CSS ─────────────────────────────────────── */

        /* Primary box headers: forest green gradient */
        .box.box-solid.box-primary > .box-header {
          color: #fff;
          background: linear-gradient(135deg, #013220, #012418) !important;
          box-shadow: 0 3px 6px rgba(1, 50, 32, 0.3);
        }
        .box.box-solid.box-primary {
          border-bottom-color: #013220;
          border-left-color:   #013220;
          border-right-color:  #013220;
          border-top-color:    #013220;
        }
        .box.box-solid.box-primary:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(1, 50, 32, 0.15);
        }

        /* Success box headers: dark zinc gradient */
        .box.box-solid.box-success > .box-header {
          color: #fff;
          background: linear-gradient(135deg, #27272a, #18181b) !important;
          box-shadow: 0 3px 6px rgba(39, 39, 42, 0.4);
        }
        .box.box-solid.box-success {
          border-bottom-color: #27272a;
          border-left-color:   #27272a;
          border-right-color:  #27272a;
          border-top-color:    #27272a;
        }
        .box.box-solid.box-success:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(39, 39, 42, 0.15);
        }

        /* Warning box headers: orange gradient */
        .box.box-solid.box-warning > .box-header {
          color: #fff;
          background: linear-gradient(135deg, #ea580c, #c2410c) !important;
          box-shadow: 0 3px 6px rgba(234, 88, 12, 0.3);
        }
        .box.box-solid.box-warning {
          border-bottom-color: #ea580c;
          border-left-color:   #ea580c;
          border-right-color:  #ea580c;
          border-top-color:    #ea580c;
        }
        .box.box-solid.box-warning:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(234, 88, 12, 0.15);
        }

        /* Box: soft shadow, no top border */
        .box {
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          border-top: none;
        }

        /* Box title typography */
        .box-title {
          font-weight: 600;
          letter-spacing: 0.03em;
          font-size: 0.92rem;
        }

        /* Sidebar active item: orange accent bar */
        .sidebar-menu > li.active > a {
          border-left: 3px solid #ea580c;
        }

        /* Content wrapper: near-white gradient */
        .content-wrapper, .right-side {
          background: linear-gradient(180deg, #fafafa 0%, #f4f4f5 100%) !important;
        }

        /* Sidebar hover */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #27272a;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          border-left-color: #ea580c;
        }

        /* Buttons: primary = forest green gradient */
        .btn-primary {
          background: linear-gradient(135deg, #013220, #012418) !important;
          border-color: #013220 !important;
          color: #fff !important;
        }
        .btn-primary:hover {
          background: linear-gradient(135deg, #012418, #011a10) !important;
          transform: translateY(-1px);
          box-shadow: 0 3px 8px rgba(1, 50, 32, 0.3);
        }

        /* Buttons: success = dark zinc gradient */
        .btn-success {
          background: linear-gradient(135deg, #3f3f46, #27272a) !important;
          border-color: #3f3f46 !important;
          color: #fff !important;
        }
        .btn-success:hover {
          background: linear-gradient(135deg, #27272a, #18181b) !important;
          transform: translateY(-1px);
          box-shadow: 0 3px 8px rgba(63, 63, 70, 0.3);
        }

        /* Input focus: orange ring */
        .form-control:focus {
          border-color: #ea580c;
          box-shadow: 0 0 0 3px rgba(234, 88, 12, 0.1);
        }

        /* Download buttons: uniform dark zinc styling */
        .btn-default.shiny-download-link {
          background: linear-gradient(135deg, #3f3f46, #27272a);
          border-color: #3f3f46;
          color: #fff;
          margin-bottom: 6px;
        }
        .btn-default.shiny-download-link:hover {
          background: linear-gradient(135deg, #27272a, #18181b);
          color: #fff;
          transform: translateY(-1px);
          box-shadow: 0 3px 8px rgba(63, 63, 70, 0.3);
        }

        /* Sidebar toggle hover */
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #27272a;
        }

        /* Footer */
        .main-footer {
          background-color: #18181b;
          color: #a1a1aa;
          border-top: 2px solid #013220;
          text-align: center;
          font-size: 0.85rem;
        }
        .main-footer a {
          color: #ea580c;
        }

        /* Checkbox accent */
        input[type='checkbox']:checked {
          accent-color: #013220;
        }

        /* Tab panel headers */
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          border-top: 2px solid #ea580c;
        }
      "))
    ),

    # ── Tab content ─────────────────────────────────────────────────────
    tabItems(
      source("ui-tab-umap.R", local = TRUE)$value
    ),

    # ── Footer ──────────────────────────────────────────────────────────
    tags$footer(
      class = "main-footer",
      tags$div(
        style = "padding: 8px 0; display: flex; justify-content: space-between; align-items: center;",
        HTML(paste0(
          "Made with \u2665 in Switzerland &middot; ",
          "<a href='https://github.com/peterleary/marmot' target='_blank'>MARMOT</a>",
          " &middot; FGCZ &middot; ETH Z\u00fcrich &middot; University of Z\u00fcrich"
        )),
        tags$span(
          paste0("v", utils::packageVersion("MARMOT")),
          style = "color: #94a3b8; font-size: 0.85rem;"
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Default reactive scaffolding ──────────────────────────────────────────
  # These are overwritten by server-colours.R once data loads, but must exist
  # at source time so that downstream modules can reference them.
  inputDataReactive    <- reactiveValues(Results = NULL)
  colourPaletteList    <- reactiveValues()
  genesReactive        <- reactiveValues(genes = NULL)
  cellsToKeepReactive  <- reactiveValues(sc2 = NULL)
  featurePlotReactive  <- reactiveValues(fp = NULL, needs_arrange = FALSE, ncol = 1)
  drDataVersion        <- reactiveVal(0L)

  source("server-import.R",   local = TRUE)
  source("server-colours.R",  local = TRUE)
  source("server-relabel.R",  local = TRUE)
  source("server-subset.R",   local = TRUE)
  source("server-dr.R",       local = TRUE)
  source("server-plots.R",    local = TRUE)
  source("server-download.R", local = TRUE)
  source("server-analysis.R", local = TRUE)
  source("server-dads.R",     local = TRUE)

}

shinyApp(ui = ui, server = server)
