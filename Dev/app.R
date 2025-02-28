library(shiny)
library(bslib)

bs_theme <- bs_theme(
  preset = "shiny",
  primary = "#96B3D2",
  success = "#627e9c"
)

ui <- page_navbar(
  theme = bs_theme, 
  navbar_options = navbar_options(bg = "#627e9c", theme = "dark"),
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
  nav_spacer(), 
  nav_panel(
    title = "Shiny Marmot",
    sidebar = sidebar(
      title = "Settings",
      "This sidebar can contain inputs and filters",
      selectInput("example", "Choose an option:", c("A", "B", "C"))
    )
  ),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

server <- function(input, output) { }

shinyApp(ui, server)
