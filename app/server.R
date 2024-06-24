# Define server logic
shinyServer(function(input, output, session) {
  # Load Saved .RData ----
  load(file = "data/SSTEM Structure/S-STEM Questions.RData")
  load(file = "data/SSTEM Structure/S-STEM Column Names.RData")
  
  # Dashboard Controlbar =================
  observe(
    updateControlbar(id = "controlBar", session = session)
  ) |> bindEvent(input$toggleControlbar)
  
  # Dashboard Header =====
  ## View S-STEM Data ----
  observe(
    showModal(
      modalDialog(
        title = "S-STEM Survey Data",
        size = "xl",
        withSpinner(rHandsontableOutput(outputId = "viewSSTEMtable"), type = 8)
      )
    )
  ) |> 
    bindEvent(input$viewSSTEMdata_button)
  
  # Dashboard Sidebar ====
  observe(
    updateTabsetPanel(session = session, inputId = "menu_items", selected = "SSTEM_Data_tab")
  ) |> bindEvent(input$sidebarItemExpanded == "SSTEM_Tab",
                 ignoreInit = TRUE,
                 once = TRUE)
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Home Tab ====================================================================================
  
  # S-STEM Tab  ====================================================================================
  ## DATA Tab ----
  ### Inputs ----
  #### Datasets ----
  loadedSSTEM_dataNames <- reactive({
    loadedFiles <- list.files("data")
    
    if(length(loadedFiles) > 0){
      loadedRData <- loadedFiles[str_detect(loadedFiles, pattern = ".RData|.rds")]
      loadedSSTEMfiles <- loadedRData[str_detect(loadedRData, pattern = "S-STEM")]
      loadedSSTEM_dataNames <- c()
      for(i in 1:length(loadedSSTEMfiles)){
        tempName <- load(file = paste0("data/", loadedSSTEMfiles[i]))
        loadedSSTEM_dataNames <- c(loadedSSTEM_dataNames, tempName)
      }
    }else{
      loadedSSTEM_dataNames <- c()
    }
    return(loadedSSTEM_dataNames)
    
    # tagList(
    #   pickerInput(
    #     inputId = "SSTEM_loadedDatasets",
    #     label = "Loaded Datasets",
    #     choices = loadedSSTEM_dataNames,
    #     choicesOpt = list(style = "background: #AED6F1")
    #   )
    # )
  })
  
  output$SSTEM_loadedDatasets_ui <- renderUI({
    tagList(
      pickerInput(
        inputId = "SSTEM_loadedDatasets",
        label = "Loaded Datasets",
        choices = loadedSSTEM_dataNames(),
        choicesOpt = list(style = "background: #AED6F1")
      )
    )
  })
  
  
  SSTEMsurvey_dataNew <- reactive({
    #req(input$SSTEM_fileInput)
    
    inFile <- input$SSTEM_fileInput$datapath
    
    if(is.null(inFile)){
      return()
    }
    
    ext <- tools::file_ext(inFile)
    
    if(ext == "xlsx"){
      SSTEMsurvey_new <- read_excel(path = inFile, na = c("", "NA", NA))
    }else if(ext == "csv"){
      SSTEMsurvey_new <- fread(inFile, na.strings = c("", "NA", NA))
    }
    
    return(SSTEMsurvey_new)
  })
  
  output$newSSTEMdata_ui <- renderDataTable({
    head(SSTEMsurvey_dataNew())
  })
  
  observe({
    # filename <- paste0("data/S-STEM Data ", 
    #                    "New", 
    #                    ".rds")
    filename <- "data/SSTEMdataNew.rds"
    saveRDS(
      data.frame(SSTEMsurvey_dataNew(), check.names = FALSE),
      file = filename)
    readRDS("SSTEMsurvey_dataNew", file = filename)
  }) |> bindEvent(input$saveSSTEMdata)
  
  
  
  
  ## Clean data
  SSTEMsurvey <- reactive({
    validate(
      need(SSTEMsurvey_data(), "Please upload S-STEM survey data in controlbar")
    )
    
    SSTEMsurvey <- SSTEMsurvey_data() |>
      select(
        ResponseId,
        Q2,
        Q3,
        Q21,
        Q22,
        str_which(STEMcolnames, pattern = "Q6"),
        str_which(STEMcolnames, pattern = "Q23"),
        str_which(STEMcolnames, pattern = "Q24")
      ) |>
      rename(
        "School" = Q2,
        "Grade" = Q3,
        "Gender" = Q21,
        "Race" = Q22
      )
    
    ### Rename columns for clarity 
    colnames(SSTEMsurvey) <- str_replace(colnames(SSTEMsurvey), pattern = "Q6_", replacement = "Math_Q")
    colnames(SSTEMsurvey) <- str_replace(colnames(SSTEMsurvey), pattern = "Q23_", replacement = "Science_Q")
    colnames(SSTEMsurvey) <- str_replace(colnames(SSTEMsurvey), pattern = "Q24_", replacement = "EngTech_Q")
    
    return(SSTEMsurvey)
  })
  
  
  ### Render SSTEM View table ----
  SSTEMsurveyView_df <- reactive({
    
    df <- SSTEMsurvey()
    return(df)
  }) |> 
    bindEvent(input$viewSSTEMdata_button)
  
  
  output$viewSSTEMtable <- renderRHandsontable({
    df <- data.frame(SSTEMsurveyView_df())
    df2 <- df |> select(everything())
    rhandsontable(df2, height = 500) |>
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      hot_cols(fixedColumnsLeft = 5)
  }) |> 
    bindEvent(input$viewSSTEMdata_button)
  
  ## CAREER AWARENESS Tab ====================================================================================
  
  
  ## INTEREST Tab ====================================================================================
  
  
  ## IDENTITY Tab ====================================================================================
  
  
  ## SELF EFFICACY Tab ====================================================================================
  ### Clean Data ----
  SSTEMsurveySelfEfficacy <- reactive({
    SSTEMsurvey |>
      rowwise() |>
      mutate(
        MathScore = mean(c_across(str_subset(colnames(SSTEMsurvey), pattern = "Math"))),
        ScienceScore = mean(c_across(str_subset(colnames(SSTEMsurvey), pattern = "Science"))),
        EngTechScore = mean(c_across(str_subset(colnames(SSTEMsurvey), pattern = "EngTech")))
      ) |>
      mutate(
        School = factor(School, levels = c("WEMS", "PSM")),
        Grade = factor(Grade, levels = c("6th", "7th", "8th")),
        Gender = factor(Gender, levels = c("Male", "Female")),
        Race = factor(Race, 
                      levels = c("American Indian/Alaska Native",
                                 "Asian",
                                 "Black/African American",
                                 "Native Hawaiian/Other Pacific Islander",
                                 "White/Caucasian",
                                 "Hispanic/Latino",
                                 "Multracial",
                                 "Other"
                      )
        )
      ) |>
      mutate(
        Race = droplevels(Race)
      )
  })
  
  ### Math ------------------------------------------------------------------------------------------------
  #### Inputs ----
  ##### Model Effects ----
  output$math_modelInteractions_ui <- renderUI({
    req(input$math_demographics)
    
    demographics <- input$math_demographics
    
    if(length(demographics) > 1){
      interactions <- data.frame(t(combn(demographics, 2))) |>
        unite("Interactions", sep = ":") |>
        pull()
      
      tagList(
        virtualSelectInput(
          inputId = "math_modelInteractions", 
          label = "Select interaction effects to include in analysis",
          choices = interactions,
          multiple = TRUE, 
          showValueAsTags = TRUE,
          allowNewOption = TRUE,
          searchPlaceholderText = c("Search or add new...")
        )
      )
    }
  })
  
  output$math_formula <- renderPrint({
    
  })
  
  #### Outputs ----
  ##### Summaries ----
  
  
  # EOG Tab ====================================================================================
})







