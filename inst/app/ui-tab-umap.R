tabItem(
  tabName = "umapTab",
  fluidRow(
    column(
      width = 3, offset = 0, style='padding:5px;', 
      box(
        title = "Dim Red Settings",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        width = NULL,
        tabsetPanel(
          tabPanel(
            title = "Inputs",
            selectInput(inputId = "umapColumnToPlot", label = "Select a column to colour by", choices = NULL, selected = NULL, multiple = FALSE),
            selectInput(inputId = "umapColumnToSplit", label = "Select a column to split by", choices = NULL, selected = NULL, multiple = FALSE),
            selectInput(inputId = "umapContrastToUse", label = "Select a contrast for DA clusters", choices = NULL, selected = NULL, multiple = FALSE),
            radioButtons(inputId = "umapShowDAClusters", label = "Show DA clusters", choices = c("None", "All", "Up only", "Down only"))
          ),
          tabPanel(
            title = "Sizes",
            checkboxInput(inputId = "umapShowAxes", label = "Show plot axes?", value = FALSE),
            checkboxInput(inputId = "umapShowLabels", label = "Show cluster labels?", value = FALSE),
            hr(style = "border-top: 1px solid #000000;"), h4("Dot Settings"),
            splitLayout(
              sliderInput(inputId = "pointSizeUMAP", label = "Dot size", min = 0.1, max = 4, value = 1, step = 0.1, width = "85%", ticks = F),
              sliderInput(inputId = "pointAlphaUMAP", label = "Dot alpha", min = 0.1, max = 1, value = 1, step = 0.1, width = "85%", ticks = F),
              sliderInput(inputId = "borderSizeUMAP", label = "Dot border size", min = 0, max = 1, value = 0, step = 0.1, width = "85%", ticks = F)
            ),
            selectInput(inputId = "umapBorderColour", label = "Dot border colour", choices = c("black", "white", "grey"), selected = "black"),
            hr(style = "border-top: 1px solid #000000;"), h4("Font Settings"),
            splitLayout(
              sliderInput(inputId = "textSizeUMAP", label = "Font size", min = 4, max = 30, value = 12, step = 0.5, width = "85%", ticks = F),
              sliderInput(inputId = "labelSizeUMAP", label = "Label size", min = 1, max = 12, value = 4, step = 0.5, width = "85%", ticks = F),
              sliderInput(inputId = "labelShiftUMAP", label = "Label shift", min = -20, max = 20, value = 0, step = 0.5, width = "85%", ticks = F),
            ),
            hr(style = "border-top: 1px solid #000000;"), h4("Layout Settings"),
            splitLayout(
              sliderInput(inputId = "figWidthUMAP", label = "Figure width", min = 100, max = 2000, value = 650, step = 10, width = "85%", ticks = F),
              sliderInput(inputId = "figHeightUMAP", label = "Figure height", min = 100, max = 2000, value = 500, step = 10, width = "85%", ticks = F),
              sliderInput(inputId = "umapMainNcol", label = "Facet Columns", value = 1, min = 1, max = 10, step = 1, width = "85%", ticks = F)
            )
          ),
          tabPanel(
            title = "Colours",
            uiOutput(outputId = "uiColourPicker", inline = TRUE)
          )
        )
      ),
      box(
        title = "Download",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        width = NULL,
        
        column(
          width = 12,
          h4("Before you download..."),
          tags$p("If you plan to publish any of the results generated in the MARMOT pipeline or Shiny Marmot app, please cite us!"),
          tags$b("Marmot: Kirsche et al., 2025"),
          tags$p("Kirsche L, He J, Müller A, Leary P (2025) Marmot. Big Journal."),
          actionButton(inputId = "acceptCite", label = "I promise to cite you! Now gimme PDFs!!")
        ),
        useFireworks(),
        uiOutput(outputId = "showPDFs")
      )
    ),
    column(
      width = 9, offset = 0, style='padding:5px;',
      box(
        title = "Dim Red Plot",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        width = NULL,
        tabsetPanel(
          tabPanel(
            title = "Pretty plot (static)",
            plotOutput(outputId = "umapStatic", inline = TRUE)
          ),
          tabPanel(
            title = "Interactive Plot",
            uiOutput(outputId = "umapInteractive", inline = TRUE)
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 3, offset = 0, style='padding:5px;',
      box(
        title = "Feature Plot Settings",
        solidHeader = TRUE,
        status = "success",
        collapsible = TRUE,
        width = NULL,
        tabsetPanel(
          tabPanel(
            title = "Inputs",
            selectInput(inputId = "featurePlotType", label = "Select a plot type", choices = c("Feature Plot", "Nebulosa Plot", "Violin Plot", "Dot Plot", "Ridge Plot", "Heatmap", "Individual Heatmap", "Barplot"), selected = "", multiple = FALSE),
            selectizeInput(inputId = "fpFeatureToPlot", label = "Select marker to plot", choices = "", selected = "", multiple = TRUE),
            radioButtons(inputId = "fpAssayToPlot", label = "Select counts to plot", choiceNames = c("Quantile Normalised", "Transformed", "Scaled Counts"), choiceValues = c("data", "counts", "scale.data")),
            selectInput(inputId = "fpColumnToPlot", label = "Select a Column to Plot By", choices = NULL, selected = NULL, multiple = FALSE),
            selectInput(inputId = "fpColumnToSplit", label = "Select a Column to Split By", choices = NULL, selected = NULL, multiple = FALSE),
            selectInput(inputId = "fpContrastToUse", label = "Select a contrast for DA clusters", choices = NULL, selected = NULL, multiple = FALSE),
            radioButtons(inputId = "fpShowDAClusters", label = "Show DA clusters", choices = c("None", "All", "Up only", "Down only"))
          ),
          tabPanel(
            title = "Size+Colour",
            selectInput(
              inputId = "viridisColourFP", 
              label = "Colour palette", 
              choices = list(
                "virids" = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
                "scico" = c("bam", "berlin", "brocO", "corkO", "lapaz", "lisbon", "romaO", "vikO"),
                "diverging" = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
                ), 
              selected = "viridis"),
            checkboxInput(inputId = "flipViridisFP", label = "Flip colour scale?", value = FALSE),
            splitLayout(
              sliderInput(inputId = "textSizeFP", label = "Font Size", min = 4, max = 30, value = 14, step = 0.5, width = "85%", ticks = F),
              sliderInput(inputId = "ncolFPGene", label = "Columns per marker", value = 1, min = 1, max = 10, step = 1, width = "85%", ticks = F),
              sliderInput(inputId = "ncolFPSplit", label = "Columns per split", value = 1, min = 1, max = 10, step = 1, width = "85%", ticks = F)
            ),
            splitLayout(
              sliderInput(inputId = "figWidthFP", label = "Figure Width", min = 100, max = 2000, value = 650, step = 10, width = "85%", ticks = F),
              sliderInput(inputId = "figHeightFP", label = "Figure Height", min = 100, max = 2000, value = 500, step = 10, width = "85%", ticks = F)
            ),
            selectInput(inputId = "fpLegendPosition", label = "Legend Position", choices = c("Right", "Bottom", "None"), selected = "Right")
          ),
          tabPanel(
            title = "Plot-By",
            uiOutput("plotByBucket")
          ),
          tabPanel(
            title = "Split-By",
            uiOutput("splitByBucket")
          )
        )
      ),
      box(
        title = "Feature Bucket", 
        solidHeader = TRUE,
        status = "success",
        collapsible = TRUE,
        width = NULL,
        uiOutput(outputId = "geneBucket1"),
        actionButton(inputId = "resetGeneBucketFP", label = "Empty the bucket?", icon = icon("bucket")),
      ),
    ),
    column(
      width = 9, offset = 0, style='padding:5px;',
      box(
        title = "Feature Plot",
        solidHeader = TRUE,
        status = "success",
        collapsible = TRUE,
        width = NULL,
        column(
          width = 2,
          h4("Specific Settings"), hr(style = "border-top: 1px solid #000000;"),
          uiOutput(outputId = "fpNebulosaOutputUI1", inline = TRUE),
          uiOutput(outputId = "fpNebulosaOutputUI2", inline = TRUE),
          lapply(1:11, function(i) {
            uiOutput(outputId = paste0("umapFeaturePlotSettingsUI", i), inline = TRUE)
          }),
          uiOutput(outputId = "umapFeaturePlotDotPlotUI1", inline = TRUE),
          uiOutput(outputId = "umapFeaturePlotDotPlotUI2", inline = TRUE),
          uiOutput(outputId = "umapFeaturePlotHeatmapUI1", inline = TRUE),
          uiOutput(outputId = "umapFeaturePlotHeatmapUI2", inline = TRUE),
          uiOutput(outputId = "umapFeaturePlotWarningUI", inline = TRUE),
          uiOutput(outputId = "fpBarplotOptionsUI1", inline = TRUE),
          uiOutput(outputId = "fpBarplotOptionsUI2", inline = TRUE),
          uiOutput(outputId = "fpBarplotOutputUI2", inline = TRUE),
          uiOutput(outputId = "fpBarplotOutputUI3", inline = TRUE),
          uiOutput(outputId = "fpHeatmapOutputUI1", inline = TRUE)
        ),
        column(
          width = 10,
          h4("Plot"), hr(style = "border-top: 1px solid #000000;"),
          # uiOutput(outputId = "umapFeaturePlotUI", inline = TRUE)
          plotOutput(outputId = "featurePlotOutput", inline = TRUE)
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12, offset = 0, style='padding:2px;',
      box(
        title = "Metadata",
        solidHeader = TRUE,
        status = "warning", 
        collapsible = TRUE,
        width = 4,
        tabsetPanel(
          tabPanel(
            title = "Metadata",
            DT::dataTableOutput(outputId = "metadataTable"),
            style = "overflow-y: scroll;"
            )
          # tabPanel(
          #   title = "Change labels",
          #   DT::dataTableOutput(outputId = "changeLabelTable")
          # )
        )
      ),
      box(
        title = "Marker Table", 
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        width = 4,
        uiOutput(outputId = "posMarkerUI"),
        style = "overflow-y: scroll;"
      ),
      box(
        title = "Label Clusters",
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        width = 4,
        downloadButton(outputId = "saveClusterLabels", label = "Download New Labels", icon = icon("save")), 
        fileInput(inputId = "importFile", accept = ".xlsx", width = "85%", label = "Upload previously-filled in table"),
        DT::dataTableOutput(outputId = "clusterLabelTable")
      )
    )
  )
  
)