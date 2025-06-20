library(shiny)
library(shinyBS)
library(DT)
library(readxl)
library(openxlsx)
library(shinycssloaders)
library(shinyWidgets)
library(later)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".footer-note { position: fixed; right: 10px; bottom: 10px; opacity: 0.7; font-size: 14px; }"))
  ),
  titlePanel("🦫 MARMOT Pipeline GUI"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("metadataFile", "Upload MARMOT Metadata (.xlsx)", accept = ".xlsx"),
      textInput("dataDir", "FCS file directory", value = "~/Desktop/FGCZ/MARMOT/Data/Org19/"),
      textInput("runName", "Run name", "My MARMOT Analysis"),
      bsButton("runMARMOT", "Run MARMOT", icon = icon("play"), style = "primary")
    ),
    
    mainPanel(
      downloadButton("downloadResults", "Download Results (ZIP)", class = "btn-success", style = "margin-bottom: 10px;"),
      bsCollapse(
        id = "collapsePanels",
        open = c("Pipeline Settings", "Log Output"),
        
        bsCollapsePanel("\U0001F527 Pipeline Settings", uiOutput("pipelineSettingsUI"), style = "info"),
        bsCollapsePanel("\U0001F9EA Study Design", DTOutput("studyDataUI"), style = "primary"),
        bsCollapsePanel("\U0001F4C2 File Metadata", DTOutput("fileDataUI"), style = "primary"),
        bsCollapsePanel("\U0001F680 Log Output", withSpinner(verbatimTextOutput("log"), type = 4), style = "success")
      )
    )
  )
)

server <- function(input, output, session) {
  sheets <- reactiveVal(NULL)
  studyData <- reactiveVal(NULL)
  fileData <- reactiveVal(NULL)
  settingsValues <- reactiveValues()
  
  observeEvent(input$metadataFile, {
    req(input$metadataFile)
    path <- input$metadataFile$datapath
    sheet_names <- excel_sheets(path)
    data <- setNames(lapply(sheet_names, read_excel, path = path), sheet_names)
    sheets(data)
    studyData(data[["Study Data"]])
    fileData(data[["File Data"]])
  })
  
  output$pipelineSettingsUI <- renderUI({
    req(sheets())
    df <- sheets()[["Pipeline Settings"]]
    
    lapply(seq_len(nrow(df)), function(i) {
      var <- df$Variable[i]
      val <- as.character(df$Setting[i])
      help <- df$Info[i]
      inputId <- paste0("setting_", gsub("[^A-Za-z0-9]", "_", var))
      
      observeEvent(input[[inputId]], {
        settingsValues[[var]] <- input[[inputId]]
      }, ignoreNULL = FALSE)
      
      tagList(
        textInput(inputId, label = var, value = val),
        bsPopover(inputId, title = var, content = help, placement = "right")
      )
    })
  })
  
  output$studyDataUI <- renderDT({
    req(studyData())
    datatable(studyData(), editable = TRUE, rownames = FALSE)
  })
  
  output$fileDataUI <- renderDT({
    req(fileData())
    datatable(fileData(), editable = TRUE, rownames = FALSE)
  })
  
  proxy_study <- dataTableProxy("studyDataUI")
  proxy_file <- dataTableProxy("fileDataUI")
  
  observeEvent(input$studyDataUI_cell_edit, {
    info <- input$studyDataUI_cell_edit
    df <- studyData()
    df[info$row, info$col] <- info$value
    studyData(df)
    replaceData(proxy_study, df, resetPaging = FALSE)
  })
  
  observeEvent(input$fileDataUI_cell_edit, {
    info <- input$fileDataUI_cell_edit
    df <- fileData()
    df[info$row, info$col] <- info$value
    fileData(df)
    replaceData(proxy_file, df, resetPaging = FALSE)
  })
  
  output$downloadResults <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", input$runName), "_results.zip"),
    content = function(file) {
      dir_to_zip <- file.path(input$dataDir, gsub("[^A-Za-z0-9]", "_", input$runName))
      if (dir.exists(dir_to_zip)) {
        old_wd <- getwd()
        setwd(dirname(dir_to_zip))
        zip::zip(zipfile = file, files = basename(dir_to_zip))
        setwd(old_wd)
      } else {
        stop("Output directory not found: ", dir_to_zip)
      }
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$runMARMOT, {
    req(sheets(), studyData(), fileData(), input$dataDir)
    
    settings_template <- sheets()[["Pipeline Settings"]]
    settings <- settings_template
    
    settings$Setting <- vapply(settings$Variable, function(v) {
      inputId <- paste0("setting_", gsub("[^A-Za-z0-9]", "_", v))
      val <- input[[inputId]]
      if (is.null(val)) as.character(settings_template$Setting[settings_template$Variable == v]) else as.character(val)
    }, character(1))
    
    tmp_file <- file.path(
      input$dataDir,
      paste0(gsub("[^A-Za-z0-9]", "_", input$runName), "_metadata.xlsx")
    )
    
    wb <- createWorkbook()
    addWorksheet(wb, "Pipeline Settings"); writeData(wb, "Pipeline Settings", settings)
    addWorksheet(wb, "Study Data"); writeData(wb, "Study Data", studyData())
    addWorksheet(wb, "File Data"); writeData(wb, "File Data", fileData())
    addWorksheet(wb, "Options"); writeData(wb, "Options", sheets()[["Options"]])
    saveWorkbook(wb, tmp_file, overwrite = TRUE)
    
    output$log <- renderPrint({ cat("Running MARMOT... please wait.

") })
    
    show_alert(
      title = "Running MARMOT",
      text = "Please be patient. This may take several minutes.",
      type = "info",
      html = FALSE,
      showConfirmButton = FALSE,
      timer = NULL,
      closeOnClickOutside = TRUE
    )
    
    show_alert(
      title = "Running MARMOT",
      text = "Please be patient. This may take several minutes.",
      type = "info",
      html = FALSE,
      showConfirmButton = FALSE,
      timer = NULL,
      closeOnClickOutside = TRUE
    )
    
    later::later(function() {
      output$log <- renderPrint({
        tryCatch({
          capture.output(
            marmot(
              metadata = tmp_file,
              name = input$runName,
              render = TRUE
            )
          )
          
          show_alert(
            title = "✅ MARMOT Completed",
            text = "The HTML report has been successfully generated.",
            type = "success",
            timer = 5000
          )
        }, error = function(e) {
          message("Error in marmot():", e$message)
          print(paste("Error:", e$message))
        })
      })
    }, delay = 0.1)
    
    updateCollapse(session, "collapsePanels", open = "Log Output")
  })
}

shinyApp(
  tagList(
    ui,
    tags$div(class = "footer-note", HTML("🧀🇨🇭🥕 Built in Switzerland by slightly deranged marmots 🦫🥕🇨🇭🧀"))
  ),
  server
)
