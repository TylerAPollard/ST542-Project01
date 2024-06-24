# Define server logic
shinyServer(function(input, output, session) {
  # Load Saved .RData ----
  load(file = "data/S-STEM Questions.RData")
  load(file = "data/S-STEM Column Names.RData")
  
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
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Home Tab ====================================================================================
  
  # S-STEM Tab  ====================================================================================
  ## Read in Data ----
  SSTEMsurvey_data <- reactive({
    #req(input$SSTEM_fileInput)
    
    inFile <- input$SSTEM_fileInput$datapath
    
    if(is.null(inFile)){
      return()
    }
    
    ext <- tools::file_ext(inFile)
    
    if(ext == "xlsx"){
      SSTEMdata <- read_excel(path = inFile, na = c("", "NA", NA))
    }else if(ext == "csv"){
      SSTEMdata <- fread(inFile, na.strings = c("", "NA", NA))
    }
    
    return(SSTEMdata)
  })
  
  # output$SSTEMfileUploaded <- reactive({
  #   return(!is.null(input$SSTEM_fileInput))
  # })
  # outputOptions(output, 'SSTEMfileUploaded', suspendWhenHidden=FALSE)
  
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
  
  
  ## Render SSTEM View table ----
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
        ),
      )
    }
  })
  
  output$math_formula <- renderPrint({
    
  })
  
  #### Outputs ----
  ##### Summaries ----
  
  
  # EOG Tab ====================================================================================
})







