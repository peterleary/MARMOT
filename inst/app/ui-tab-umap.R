## ---------------------------------------------------------------
## ui-tab-umap.R
## Main UI layout for the MARMOT Shiny app
## Adapted from exploreSingleCell's clean layout + MARMOT-specific
## features (DA filtering, analysis plots, MARMOT palettes, etc.)
## ---------------------------------------------------------------

tabItem(
  tabName = "umapTab",

  # ============================================================
  # Row 1: Dimensionality Reduction Plot

  # ============================================================
  fluidRow(
    column(
      width = 3, offset = 0, style = "padding:5px;",
      box(
        title = "Dim Red Settings",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        width = NULL,
        tabsetPanel(

          # --- Inputs tab ---
          tabPanel(
            title = "Inputs",
            selectInput(
              inputId = "umapDRToPlot",
              label = "Select a DR method to plot",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            selectInput(
              inputId = "umapColumnToPlot",
              label = "Select a column to colour by",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            selectInput(
              inputId = "umapColumnToSplit",
              label = "Select a column to split by",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            selectInput(
              inputId = "umapContrast",
              label = "Select a contrast for DA clusters",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            radioButtons(
              inputId = "umapDAFilter",
              label = "Show DA clusters",
              choices = c("None", "All", "Up only", "Down only"),
              selected = "None"
            )
          ),

          # --- Size + Colour tab ---
          tabPanel(
            title = "Size & Colour",
            selectInput(
              inputId = "umapColourPalette",
              label = "Colour palette",
              choices = list(
                "Qualitative" = c("Set1", "Set2", "Set3", "Paired", "Dark2",
                                   "Accent", "Pastel1", "Pastel2"),
                "pals"        = c("alphabet", "alphabet2", "cols25", "polychrome",
                                   "glasbey", "kelly", "watlington"),
                "viridis"     = c("magma", "inferno", "plasma", "viridis",
                                   "cividis", "rocket", "mako", "turbo")
              ),
              selected = "Set1"
            ),
            checkboxInput(
              inputId = "rasteriseUMAP",
              label = "Rasterise plot (faster for large data)",
              value = FALSE
            ),
            numericInput(
              inputId = "rasterUMAP_DPI",
              label = "Raster DPI",
              value = 300, min = 72, max = 600, step = 50
            ),
            checkboxInput(
              inputId = "umapShowAxes",
              label = "Show plot axes?",
              value = FALSE
            ),

            hr(style = "border-top: 1px solid #000000;"),
            h4("Dot Settings"),
            splitLayout(
              sliderInput(
                inputId = "pointSizeUMAP", label = "Dot size",
                min = 0.1, max = 4, value = 0.3, step = 0.1,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "pointAlphaUMAP", label = "Dot alpha",
                min = 0.1, max = 1, value = 1, step = 0.1,
                width = "85%", ticks = FALSE
              )
            ),

            hr(style = "border-top: 1px solid #000000;"),
            h4("Border Settings"),
            selectInput(
              inputId = "umapBorderType",
              label = "Border type",
              choices = c("None", "Per-cell borders", "Density borders"),
              selected = "Density borders",
              width = "100%"
            ),
            conditionalPanel(
              condition = "input.umapBorderType == 'Per-cell borders'",
              sliderInput(
                inputId = "borderSizeUMAP", label = "Border thickness",
                min = 0.1, max = 3, value = 0.5, step = 0.1,
                width = "85%", ticks = FALSE
              ),
              colourpicker::colourInput(
                inputId = "umapBorderColour",
                label = "Border colour",
                value = "black"
              )
            ),
            conditionalPanel(
              condition = "input.umapBorderType == 'Density borders'",
              splitLayout(
                sliderInput(
                  inputId = "scpubrBorderSize", label = "Border size",
                  min = 1, max = 5, value = 3, step = 0.1,
                  width = "85%", ticks = FALSE
                ),
                sliderInput(
                  inputId = "scpubrBorderDensity", label = "Border density",
                  min = 0.05, max = 1, value = 1, step = 0.05,
                  width = "85%", ticks = FALSE
                )
              ),
              colourpicker::colourInput(
                inputId = "scpubrBorderColour",
                label = "Border colour",
                value = "black"
              )
            ),

            hr(style = "border-top: 1px solid #000000;"),
            h4("Label Settings"),
            checkboxInput(
              inputId = "umapShowLabels",
              label = "Show cluster labels?",
              value = FALSE
            ),
            splitLayout(
              sliderInput(
                inputId = "labelSizeUMAP", label = "Label size",
                min = 1, max = 12, value = 4, step = 0.5,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "labelAlphaUMAP", label = "Label alpha",
                min = 0.1, max = 1, value = 0.9, step = 0.1,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "labelShiftUMAP", label = "Label shift",
                min = -20, max = 20, value = 0, step = 0.5,
                width = "85%", ticks = FALSE
              )
            ),

            hr(style = "border-top: 1px solid #000000;"),
            h4("Layout Settings"),
            splitLayout(
              sliderInput(
                inputId = "textSizeUMAP", label = "Font size",
                min = 4, max = 30, value = 12, step = 0.5,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "figWidthUMAP", label = "Figure width",
                min = 100, max = 2000, value = 650, step = 10,
                width = "85%", ticks = FALSE
              )
            ),
            splitLayout(
              sliderInput(
                inputId = "figHeightUMAP", label = "Figure height",
                min = 100, max = 2000, value = 500, step = 10,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "umapMainNcol", label = "Facet columns",
                value = 1, min = 1, max = 10, step = 1,
                width = "85%", ticks = FALSE
              )
            )
          ),

          # --- Relabel Clusters tab ---
          tabPanel(
            title = "Relabel Clusters",
            uiOutput(outputId = "relabelColumnIndicator"),
            helpText(
              "Edit the 'relabelled_clusters' column to rename clusters. ",
              "Edit the 'colours' column to set hex colours. ",
              "Changes take effect when you click 'Apply'."
            ),
            fluidRow(
              column(
                width = 6,
                actionButton(
                  inputId = "applyRelabelling",
                  label = "Apply",
                  icon = icon("check"),
                  class = "btn-sm btn-primary"
                )
              ),
              column(
                width = 6,
                downloadButton(
                  outputId = "saveClusterLabels",
                  label = "Download",
                  icon = icon("save"),
                  class = "btn-sm"
                )
              )
            ),
            hr(style = "border-top: 1px solid #000000;"),
            fileInput(
              inputId = "importFile",
              label = "Upload previously-filled in label table (.xlsx)",
              accept = ".xlsx",
              width = "100%"
            ),
            DT::dataTableOutput(outputId = "clusterLabelTable")
          )

        ) # end tabsetPanel
      ) # end Dim Red Settings box
    ), # end left column

    column(
      width = 9, offset = 0, style = "padding:5px;",
      box(
        title = "Dim Red Plot",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        width = NULL,
        tabsetPanel(
          tabPanel(
            title = "Pretty plot (static)",
            shinycssloaders::withSpinner(
              plotOutput(outputId = "umapStatic", inline = TRUE),
              type = 6, color = "#dc2626"
            )
          ),
          tabPanel(
            title = "Interactive Plot",
            plotlyOutput(
              outputId = "umapInteractive",
              inline = TRUE, height = "100%", width = "100%"
            )
          )
        )
      )
    ) # end right column
  ), # end Row 1

  # ============================================================
  # Row 2: Feature Plots
  # ============================================================
  fluidRow(
    column(
      width = 3, offset = 0, style = "padding:5px;",
      box(
        title = "Feature Plot Settings",
        solidHeader = TRUE,
        status = "success",
        collapsible = TRUE,
        width = NULL,
        tabsetPanel(

          # --- Controls tab: plot type + common inputs + dynamic per-type block ---
          tabPanel(
            title = "Controls",
            selectInput(
              inputId = "featurePlotType",
              label = "Select a plot type",
              choices = list(
                "Expression" = c("Feature Plot", "Nebulosa Plot",
                                 "Violin Plot", "Dot Plot", "Ridge Plot"),
                "Heatmaps"   = c("Heatmap per cell", "Heatmap per cluster")
              ),
              selected = "Feature Plot",
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "fpFeatureToPlot",
              label = "Select markers to plot:",
              multiple = TRUE,
              choices = NULL,
              selected = NULL,
              options = list(
                placeholder = "Select features",
                plugins = list(
                  "remove_button",
                  "drag_drop",
                  "restore_on_backspace",
                  "clear_button"
                )
              )
              # server = TRUE populated by updateSelectizeInput in server
            ),
            textAreaInput(
              inputId = "fpFeatureToPlotText",
              label = "Or paste a list of markers (one per line):",
              value = "",
              rows = 3,
              placeholder = "CD3\nCD4\nCD8a"
            ),
            selectInput(
              inputId = "fpAssayToPlot",
              label = "Select counts to plot",
              choices = c(
                "Quantile Normalised" = "exprsQuantNorm",
                "Transformed"         = "exprsTransformed",
                "Scaled Counts"       = "norm",
                "Raw Counts"          = "counts"
              ),
              selected = "exprsQuantNorm"
            ),
            selectInput(
              inputId = "fpColumnToPlot",
              label = "Select a column to plot by",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            selectInput(
              inputId = "fpColumnToSplit",
              label = "Select a column to split by",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            selectInput(
              inputId = "fpContrast",
              label = "Select a contrast for DA clusters",
              choices = NULL, selected = NULL, multiple = FALSE
            ),
            radioButtons(
              inputId = "fpDAFilter",
              label = "Show DA clusters",
              choices = c("None", "All", "Up only", "Down only"),
              selected = "None"
            ),
            # Dynamic plot-type-specific controls (sections keyed off
            # input$featurePlotType). See server-plots.R output$plotTypeControls.
            uiOutput(outputId = "plotTypeControls"),
            # Warning slot
            uiOutput(outputId = "umapFeaturePlotWarningUI")
          ),

          # --- Layout tab: size, colour, plot-by, split-by ---
          tabPanel(
            title = "Layout",
            tags$h5("Colour",
                    style = "margin-top: 0.5rem; margin-bottom: 0.4rem; padding-bottom: 0.2rem; border-bottom: 1px solid #ddd; font-weight: 600; color: #555;"),
            selectInput(
              inputId = "viridisColourFP",
              label = "Colour palette",
              choices = list(
                "viridis"   = c("magma", "inferno", "plasma", "viridis",
                                "cividis", "rocket", "mako", "turbo"),
                "scico"     = c("bam", "berlin", "brocO", "corkO",
                                "lapaz", "lisbon", "romaO", "vikO"),
                "diverging" = c("BrBG", "PiYG", "PRGn", "PuOr",
                                "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
              ),
              selected = "lisbon"
            ),
            checkboxInput(
              inputId = "flipViridisFP",
              label = "Flip colour scale?",
              value = FALSE
            ),
            selectInput(
              inputId = "fpLegendPosition",
              label = "Legend Position",
              choices = c("Right" = "right", "Bottom" = "bottom", "None" = "none"),
              selected = "right"
            ),
            tags$h5("Size",
                    style = "margin-top: 1rem; margin-bottom: 0.4rem; padding-bottom: 0.2rem; border-bottom: 1px solid #ddd; font-weight: 600; color: #555;"),
            splitLayout(
              sliderInput(
                inputId = "textSizeFP", label = "Font Size",
                min = 4, max = 30, value = 14, step = 0.5,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "ncolFPGene", label = "Columns per marker",
                value = 1, min = 1, max = 10, step = 1,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "ncolFPSplit", label = "Columns per split",
                value = 1, min = 1, max = 10, step = 1,
                width = "85%", ticks = FALSE
              )
            ),
            splitLayout(
              sliderInput(
                inputId = "figWidthFP", label = "Figure Width",
                min = 100, max = 2000, value = 650, step = 10,
                width = "85%", ticks = FALSE
              ),
              sliderInput(
                inputId = "figHeightFP", label = "Figure Height",
                min = 100, max = 2000, value = 500, step = 10,
                width = "85%", ticks = FALSE
              )
            ),
            tags$h5("Plot by",
                    style = "margin-top: 1rem; margin-bottom: 0.4rem; padding-bottom: 0.2rem; border-bottom: 1px solid #ddd; font-weight: 600; color: #555;"),
            uiOutput(outputId = "plotByBucket"),
            tags$h5("Split by",
                    style = "margin-top: 1rem; margin-bottom: 0.4rem; padding-bottom: 0.2rem; border-bottom: 1px solid #ddd; font-weight: 600; color: #555;"),
            uiOutput(outputId = "splitByBucket")
          ),

          # --- Subset cells tab ---
          tabPanel(
            title = "Subset",
            radioButtons(
              inputId = "fpSubsetMode",
              label = "Subset mode",
              choices = c("None", "Absolute", "Proportional"),
              selected = "None",
              inline = TRUE
            ),
            uiOutput(outputId = "fpSubsetCellsByColumnUI1"),
            uiOutput(outputId = "fpSubsetCellsByColumnUI2"),
            uiOutput(outputId = "fpSubsetCellsByColumnUI3"),
            hr(style = "border-top: 1px solid #000000;"),
            uiOutput(outputId = "fpSubsetCellsTableUI")
          )

        ) # end tabsetPanel
      ) # end Feature Plot Settings box
    ), # end left column

    column(
      width = 9, offset = 0, style = "padding:5px;",
      box(
        title = "Feature Plot",
        solidHeader = TRUE,
        status = "success",
        collapsible = TRUE,
        width = NULL,
        shinycssloaders::withSpinner(
          plotOutput(outputId = "umapFeaturePlotOutput", inline = TRUE),
          type = 6, color = "#dc2626"
        )
      )
    ) # end right column
  ), # end Row 2

  # ============================================================
  # Row 3: Analysis Plots
  # ============================================================
  fluidRow(
    column(
      width = 3, offset = 0, style = "padding:5px;",
      box(
        title = "Analysis Settings",
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        width = NULL,
        selectInput(
          inputId = "analysisPlotType",
          label = "Select an analysis plot",
          choices = c(
            "Cofactor Histograms",
            "Clustree",
            "Pseudo-bulk MDS",
            "Marker Pair Scatter",
            "Cluster Frequency Boxplots",
            "Cluster Abundances",
            "Marker Boxplots",
            "Marker per Cluster Boxplot",
            "Abundance Barplot"
          ),
          selected = "Cofactor Histograms",
          multiple = FALSE
        ),
        uiOutput(outputId = "analysisSettingsUI")
      )
    ), # end left column

    column(
      width = 9, offset = 0, style = "padding:5px;",
      box(
        title = "Analysis Plot",
        solidHeader = TRUE,
        status = "warning",
        collapsible = TRUE,
        width = NULL,
        shinycssloaders::withSpinner(
          plotOutput(outputId = "analysisPlotOutput", inline = TRUE),
          type = 6, color = "#dc2626"
        )
      )
    ) # end right column
  ), # end Row 3

  # ============================================================
  # Row 4: Data Tables
  # ============================================================
  fluidRow(
    column(
      width = 12, offset = 0, style = "padding:2px;",
      box(
        title = "Data Tables",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        width = 12,
        tabsetPanel(
          tabPanel(
            title = "Metadata",
            DT::dataTableOutput(outputId = "metadataTable"),
            style = "overflow-y: scroll;"
          ),
          tabPanel(
            title = "Marker Table",
            uiOutput(outputId = "posMarkerUI"),
            uiOutput(outputId = "posMarkerUI2"),
            style = "overflow-y: scroll;"
          )
        )
      )
    ) # end full-width column
  ), # end Row 4

  # ============================================================
  # Row 5: DA/DS Results
  # ============================================================
  fluidRow(
    column(
      width = 3, offset = 0, style = "padding:5px;",
      box(
        title = "DA/DS Settings",
        solidHeader = TRUE,
        status = "danger",
        collapsible = TRUE,
        width = NULL,
        selectInput(
          inputId  = "dadsResultType",
          label    = "Result type",
          choices  = c("DA Table", "DA Heatmap", "DS Table", "DS Heatmap"),
          selected = "DA Table"
        ),
        selectInput(
          inputId  = "dadsContrast",
          label    = "Contrast",
          choices  = NULL, selected = NULL
        ),
        conditionalPanel(
          condition = "input.dadsResultType == 'DA Heatmap' || input.dadsResultType == 'DS Heatmap'",
          sliderInput(
            inputId = "dadsTopN",
            label   = "Top N clusters/markers",
            min     = 5, max = 100, value = 20, step = 5,
            ticks   = FALSE
          ),
          splitLayout(
            sliderInput(
              inputId = "dadsFigWidth",
              label   = "Width (px)",
              min     = 400, max = 2000, value = 900, step = 50,
              width   = "85%", ticks = FALSE
            ),
            sliderInput(
              inputId = "dadsFigHeight",
              label   = "Height (px)",
              min     = 400, max = 2000, value = 700, step = 50,
              width   = "85%", ticks = FALSE
            )
          )
        ),
        conditionalPanel(
          condition = "input.dadsResultType == 'DS Heatmap'",
          sliderInput(
            inputId = "dadsFDR",
            label   = "FDR threshold",
            min     = 0.001, max = 0.1, value = 0.05, step = 0.005,
            ticks   = FALSE
          )
        )
      )
    ), # end left column

    column(
      width = 9, offset = 0, style = "padding:5px;",
      box(
        title = "DA/DS Results",
        solidHeader = TRUE,
        status = "danger",
        collapsible = TRUE,
        width = NULL,
        uiOutput(outputId = "dadsOutputUI")
      )
    ) # end right column
  ) # end Row 5

) # end tabItem
