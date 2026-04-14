cat("loading packages...\n\n")
packagesToLoad <- c(
  "shiny", "shinydashboard", "ggplot2", "dplyr", "tidyr", "purrr", "tibble",
  "RColorBrewer", "DT", "colourpicker",
  "writexl", "circlize", "kableExtra", "ggrepel", "sortable", "waiter",
  "ggprism", "rstatix", "gridExtra", "Matrix", "fresh", "viridis",
  "plotly", "shinycssloaders", "shinyBS", "CATALYST", "ComplexHeatmap", "gtools",
  "fireworks", "ggnewscale", "scattermore", "scico", "chameleon",
  "pals", "scales", "MARMOT", "flowCore", "readxl", "ggridges", "colorspace",
  "SingleCellExperiment", "shinyalert", "patchwork", "data.table"
)
library(SummarizedExperiment)
invisible(lapply(packagesToLoad, function(pkg) {
  suppressPackageStartupMessages(suppressWarnings(library(pkg, character.only = TRUE, quietly = TRUE)))
}))

# Source helper files
source("helpers/colour_helpers.R", local = TRUE)
source("helpers/data_helpers.R", local = TRUE)
source("helpers/plot_helpers.R", local = TRUE)
cat("... packages loaded!\n\n")

# Crimson: dark zinc primary, red accent
my_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#3f3f46",   # Zinc primary
    aqua       = "#27272a",   # Dark zinc
    green      = "#27272a",   # Dark zinc (success boxes)
    navy       = "#18181b",   # Near-black zinc
    orange     = "#ef4444"    # Red accent
  ),
  fresh::adminlte_sidebar(width = "400px")
)


ui <- dashboardPage(
  title = "Shiny Marmot",
  dashboardHeader(
    title = tags$span(
      tags$img(
        src = "MARMOT_Logo_2_bw.png",
        width = "46px",
        height = "auto",
        class = "me-3",
        alt = "MARMOT"
      ),
      "Shiny Marmot"
    ),
    tags$li(
      tags$span(
        paste0("v", utils::packageVersion("MARMOT")),
        style = "color: #94a3b8; font-size: 0.78rem; padding: 15px 10px; display: inline-block;"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = "mailto:peter.leary@uzh.ch?subject=flow-cytometry-shiny-app-feedback",
        "Request Features/Report Bugs"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = "http://www.fgcz.ch",
        target = "_blank",
        img(src = "fgcz_logo.png", title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = "http://www.ethz.ch/en.html",
        target = "_blank",
        img(src = "eth_logo.png", title = "ETH Zurich", height = "22px"),
        style = "padding-top:13px; padding-bottom:10px;"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = "http://www.uzh.ch/en.html",
        target = "_blank",
        img(src = "University_of_Zurich_Logo.png", title = "University of Zurich", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Shiny marmots", tabName = "umapTab", icon = icon("map"))
    ),
    collapsed = TRUE
  ),
  dashboardBody(
    use_theme(my_theme),
    tags$head(
      tags$link(rel = "shortcut icon", href = "MARMOT_Logo_2_bw.png"),
      tags$style(HTML("
        /* Crimson: dark zinc primary, red accent */
        .box.box-solid.box-primary > .box-header {
          color: #fff;
          background: linear-gradient(135deg, #3f3f46, #27272a) !important;
          box-shadow: 0 3px 6px rgba(63, 63, 70, 0.4);
        }
        .box.box-solid.box-primary {
          border-bottom-color: #3f3f46;
          border-left-color:   #3f3f46;
          border-right-color:  #3f3f46;
          border-top-color:    #3f3f46;
        }
        .box.box-solid.box-primary:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(63, 63, 70, 0.15);
        }
        /* Success box headers: deep zinc gradient */
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
        /* Box: soft shadow */
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
        /* Sidebar active item: red accent */
        .sidebar-menu > li.active > a {
          border-left: 3px solid #ef4444;
        }
        /* Content wrapper: warm off-white */
        .content-wrapper, .right-side {
          background-color: #fafafa !important;
        }
        /* Sidebar hover */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #27272a;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          border-left-color: #ef4444;
        }
        /* Buttons: red gradient */
        .btn-primary {
          background: linear-gradient(135deg, #ef4444, #dc2626) !important;
          border-color: #ef4444 !important;
          color: #fff !important;
        }
        .btn-primary:hover {
          background: linear-gradient(135deg, #dc2626, #b91c1c) !important;
          transform: translateY(-1px);
          box-shadow: 0 3px 8px rgba(239, 68, 68, 0.3);
        }
        /* Input focus: red ring */
        .form-control:focus {
          border-color: #ef4444;
          box-shadow: 0 0 0 3px rgba(239, 68, 68, 0.1);
        }
      "))
    ),
    use_waiter(),
    tabItems(
      source("ui-tab-umap.R", local = TRUE)$value
    )
  )
)

server <- function(input, output, session) {

  source("server-import.R", local = TRUE)
  source("server-colours.R", local = TRUE)
  source("server-relabel.R", local = TRUE)
  source("server-subset.R", local = TRUE)
  source("server-dr.R", local = TRUE)
  source("server-plots.R", local = TRUE)
  source("server-download.R", local = TRUE)

}

shinyApp(ui = ui, server = server)
