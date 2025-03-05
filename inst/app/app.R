cat("loading packages...\n\n")
packagesToLoad <- c(
  "shiny", "shinydashboard", "tidyverse", "RColorBrewer", "DT", "colourpicker", 
  "writexl", "circlize", "kableExtra", "ggrepel", "gplots", "sortable", "waiter", 
  "ggprism", "rstatix", "gridExtra", "Matrix", "SCpubr", "fresh", "viridis", 
  "plotly", "shinycssloaders", "shinyBS", "CATALYST", "ComplexHeatmap", "gtools",
  "fireworks", "ggnewscale", "scattermore", "Nebulosa", "scico", "chameleon", 
  "pals", "scales", "shinyMarmot"
)
invisible(lapply(packagesToLoad, function(pkg) {
  suppressPackageStartupMessages(suppressWarnings(library(pkg, character.only = TRUE, quietly = TRUE)))
}))
cat("... packages loaded!\n\n")
reactiveConsole(TRUE)

my_theme = create_theme(
  adminlte_color(
    light_blue = "#627e9c"
  )
)


ui = dashboardPage(
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
      a(
        href = 'mailto:peter.leary@uzh.ch?subject=flow-cytometry-shiny-app-feedback', 
        "Request Features/Report Bugs"), 
      class = "dropdown"
    ),
    tags$li(
      a(href = 'http://www.fgcz.ch', 
        target = "_blank",
        img(src = 'fgcz_logo.png', title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"),
      class = "dropdown"),
    tags$li(
      a(href = 'http://www.ethz.ch/en.html',
        target = "_blank",
        img(src = 'eth_logo.png', title = "FGCZ", height = "22px"),
        style = "padding-top:13px; padding-bottom:10px;"),
      class = "dropdown"),
    tags$li(
      a(href = 'http://www.uzh.ch/en.html',
        target = "_blank",
        img(src = 'University_of_Zurich_Logo.png', title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"),
      class = "dropdown")
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
      tags$style(HTML(
        '
        .box.box-solid.box-primary>.box-header {
        color:#fff; background:#96B3D2}
        .box.box-solid.box-primary{
        border-bottom-color:#96B3D2;
        border-left-color:#96B3D2;
        border-right-color:#96B3D2;
        border-top-color:#96B3D2;
        }
        .box.box-solid.box-success>.box-header {
        color:#fff; background:#627e9c}
        .box.box-solid.box-success{
        border-bottom-color:#627e9c;
        border-left-color:#627e9c;
        border-right-color:#627e9c;
        border-top-color:#627e9c;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #93bac2;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          border-left-color: #455b73;
        }
        /* body */
        .content-wrapper, .right-side {
        background-color: #FFFFFF;
        }'))
      ),
    
    use_waiter(),
    tabItems(
      source("ui-tab-umap.R", local = TRUE)$value
    )
  )
)

server = function(input, output, session) {
  
  source("server-import.R", local = TRUE)
  source("server-umap.R", local = TRUE)
  
}

shinyApp(ui = ui, server = server)
