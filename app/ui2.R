#### Property Analysis App
## Tyler Pollard
## 9 Nov 2023

# Libraries ----
## Shiny Dashboard
library(shiny)
library(shinyWidgets)
library(shinybrowser)
library(shinycssloaders)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(bs4Dash)
library(colourpicker)
library(fresh)
library(scales)

## Data Reading 
library(data.table) # Read csv in tibble format
library(readxl) # Read xlsx 

## Data Manipulation
library(tidyr)
library(plyr)
library(stringr)
library(lubridate)
library(hms)

## Data Analysis
# library(survey)
# library(srvyr)
# library(surveydata)
library(likert)
library(lme4)
library(lmtest)
library(car)
library(emmeans)
library(betareg)
library(caret)

## Bayesian Data Analysis
library(DescTools)
library(rstanarm)
library(brms)
library(posterior)
library(bayesplot)
library(BayesFactor)
library(rjags)

## Plotting
library(plotly)
library(RColorBrewer)
library(patchwork)

## Extra packages
library(DT)
library(rhandsontable)
# library(readxl)
# library(readr)
# library(haven)
# library(data.table)
# library(htmltools)
# library(extrafont)
# library(reshape2)
# library(png)
# library(ggpubr)
# library(htmlTable)
# library(tibble)
# library(EnvStats)
# library(xtable)
# library(grid)
# library(rhandsontable)
# library(rvest)
# library(scales)
# library(caret)
# library(knitr)
# library(data.table)
# library(markdown)
# library(gt)
# library(dplyr)

## Load Last
library(tidyverse)


# Set theme and credentials ----
## Theme ----
my_theme <- create_theme(
  theme = "paper",
  bs4dash_sidebar_dark(
    bg = "#000000", submenu_color = "white",
  ),
  bs4dash_sidebar_light(
    bg = "#000000", color = "white", 
  ),
  bs4dash_status(
    primary = "#CC0000", secondary = "#FFD700", info = "#FFD700"
  )
)
tags$style(".buttoncolor.bttn-primary{background-color: #CC0000")

# tags$head(tags$script('
#                     // Define function to set height of "map" and "map_container"
#                     setHeight = function() {
#                       var window_height = $(window).height();
#                       var header_height = $(".main-header").height();
# 
#                       var boxHeight = window_height - header_height - 30;
# 
#                       $("#picks_popularity").height(boxHeight - 400);
#                       $("#pick_popularity_table").height(boxHeight - 400);
# 
#                       $("#picks_output").height(boxHeight - 130);
#                       $("#picks_table").height(boxHeight - 130);
#                     };
# 
#                     // Set input$box_height when the connection is established
#                     $(document).on("shiny:connected", function(event) {
#                       setHeight();
#                     });
# 
#                     // Refresh the box height on every window resize event
#                     $(window).on("resize", function(){
#                       setHeight();
#                     });
#                   '))

# Define UI for application
shinyUI(
  dashboardPage(dark = FALSE, footer = dashboardFooter(left = br()), freshTheme = my_theme,
                shinyjs::useShinyjs(),
                # Dahsboard Header ===============
                header = dashboardHeader(
                  title = dashboardBrand(
                    title = div(
                      #tags$img(src = "", height = "60", width = "175"),
                      "DeSIRE Analysis Dashboard",
                      align = "center"
                    )
                  ), 
                  #div(h1(strong("ST542 Project")), align = "center"),
                  compact = FALSE,
                  
                  ## Right UI ----
                  rightUi = tags$li(
                    class = "dropdown",
                    dropdownMenu(
                      badgeStatus = NULL,
                      type = "notifications",
                      headerText = "DeSIRE Analysis Dashboard",
                      icon = icon("info-circle"),
                      notificationItem(
                        inputId = "info2",
                        text = "Release Date: 23 June 2024",
                        icon = icon("calendar"),
                        status = "info"
                      ),
                      notificationItem(
                        inputId = "info3",
                        text = "Version: 1.0",
                        icon = icon("code"),
                        status = "info"
                      )
                    ) #end dropdown menu
                  ), #end rightUI
                  
                  ## Left UI ----
                  leftUi = tags$li(
                    class = "dropdown",
                    div(
                      style = "display: inline-block; vertical-align: top",
                      ### Toggle Controlbar ----
                      actionBttn(
                        inputId = "toggleControlbar", 
                        label = "Toggle Controlbar", 
                        color = "primary", 
                        style = "fill"
                      ),
                      
                      ### View SSTEM data ----
                      actionBttn(
                        inputId = "viewSSTEMdata_button", 
                        label = "View S-STEM Data", 
                        color = "danger",
                        style = "fill"
                      ),
                      
                      ### View EOG data ----
                      actionBttn(
                        inputId = "viewEOGdata_button", 
                        label = "View EOG Data", 
                        color = "danger",
                        style = "fill", 
                      )
                    )
                  )
                ), # close header
                scrollToTop = TRUE,
                # Dashboard Sidebar =============
                sidebar = dashboardSidebar(
                  skin = "dark",
                  elevation = 5,
                  fixed = FALSE,
                  minified = FALSE,
                  status = "primary",
                  compact = TRUE,
                  ## Sidebar Menu ---------------
                  sidebarMenu(
                    id = "menu_items",
                    menuItem(text = "Home", tabName = "home", icon = icon("home")),
                    menuItem(text = "S-STEM", tabName = "SSTEM_tab", icon = icon("home"),
                             menuSubItem(text = "Career Awareness", tabName = "SSTEM_CareerAwareness_tab", icon = icon("lightbulb")),
                             menuSubItem(text = "Interest", tabName = "SSTEM_Interest_tab", icon = icon("brain")),
                             menuSubItem(text = "Identity", tabName = "SSTEM_Identity_tab", icon = icon("fingerprint")),
                             menuSubItem(text = "Self Efficacy", tabName = "SSTEM_SelfEfficacy_tab", icon = icon("person"))),
                    menuItem(text = "EOG", tabName = "EOG_tab", icon = icon("home"))
                  ) # close sidebar menu
                ), # close dashboard sidebar
                # Dashboard Body ================
                body = dashboardBody(
                  tabItems(
                    ## Home Tab ===============
                    tabItem(
                      tabName = "home", 
                      title = "HOME",
                      fluidPage(
                      ) # end fluid page
                    ), # end Home Tab Item
                    
                    ## S-STEM Tab ===================================================================================
                    
                    ### CAREER AWARENESS Tab ===============
                    tabItem(
                      tabName = "SSTEM_CareerAwareness_tab",
                      h2(strong("S-STEM Survey Career Awareness Analysis")),
                      fluidPage(
                      ) # end fluid page
                    ), # end Home Tab Item
                    
                    ### INTEREST Tab ===============
                    tabItem(
                      tabName = "SSTEM_Interest_tab",
                      h2(strong("S-STEM Survey Interest Analysis")),
                      fluidPage(
                      ) # end fluid page
                    ), # end Home Tab Item
                    
                    ### IDENTITY Tab ===============
                    tabItem(
                      tabName = "SSTEM_Identity_tab",
                      h2(strong("S-STEM Survey Identity Analysis")),
                      fluidPage(
                      ) # end fluid page
                    ), # end Home Tab Item
                    
                    ### SELF EFFICACY Tab ===============
                    tabItem(
                      tabName = "SSTEM_SelfEfficacy_tab",
                      h2(strong("S-STEM Survey Self Efficacy Analysis")),
                      tabsetPanel(
                        #### Data ----
                        tabPanel(
                          title = h4("Math"),
                          value = "SelfEfficacy_Math",
                          fluidPage(
                            hr(),
                            fluidRow(
                              column(
                                width = 3,
                                
                                ##### Inputs ----
                                box(title = div(h4(strong("Inputs"), 
                                                   style = "display: inline-block;
                                                            vertical-align: top")),
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "primary",
                                    width = 12,
                                    
                                    ###### Socio-Demographic Variables ----
                                    h4(strong("Model Variables")),
                                    virtualSelectInput(
                                      inputId = "math_demographics", 
                                      label = "Select demographic variables to include in analysis",
                                      choices = c(
                                        "School",
                                        "Grade",
                                        "Gender",
                                        "Race"
                                      ),
                                      multiple = TRUE, 
                                      autoSelectFirstOption = TRUE,
                                      showValueAsTags = TRUE,
                                      allowNewOption = TRUE,
                                      searchPlaceholderText = c("Search or add new...")
                                    ),
                                    
                                    ###### Model Effects ----
                                    uiOutput(outputId = "math_modelInteractions_ui"),
                                    br(),
                                    
                                    ###### Model Types ----
                                    hr(),
                                    h4(strong("Model Selection")),
                                    virtualSelectInput(
                                      inputId = "math_modelTypes", 
                                      label = "Select regression model to fit", 
                                      choices = c(
                                        "Linear Regression",
                                        "Beta Regression"
                                      ),
                                      multiple = FALSE
                                    ),
                                    
                                    ###### Statistical Framework ----
                                    radioGroupButtons(
                                      inputId = "math_modelFramework",
                                      label = "Choose statistical analysis type",
                                      choices = c("Frequentist", "Bayesian"),
                                      status = "primary",
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon")
                                      )
                                    )
                                )
                              ),
                              column(
                                width = 9,
                                tabBox(
                                  title = "Outputs",
                                  width = 12,
                                  tabPanel(
                                    title = "Summary"
                                  ),
                                  tabPanel(
                                    title = "Model Fit"
                                  )
                                )
                              ) #end column
                            ) # end fluidRow
                          ) # end fluidPage
                        ), # end Math tabPanel
                        #### Science ----
                        tabPanel(
                          title = h4("Science"),
                          value = "SelfEfficacy_Science",
                          fluidPage(
                            
                          )
                        ),
                        #### EngTech ----
                        tabPanel(
                          title = h4("Engineering & Tech"),
                          value = "SelfEfficacy_EngineeringTech",
                          fluidPage(
                            
                          )
                        )
                      )
                    ), # end Home Tab Item
                    
                    ## EOG Tab ===============
                    tabItem(
                      tabName = "EOG_tab",
                      fluidPage(
                      ) # end fluid page
                    ) # end Home Tab Item
                  ) # end Tab items
                ), # end dashboard body 
                
                # Dashboard Control Bar ====================
                controlbar = dashboardControlbar(
                  id = "controlBar", 
                  collapsed = TRUE,  
                  
                  fluidPage(
                    hr(),
                    
                    ## S-STEM File Input ----
                    fileInput(
                      inputId = "SSTEM_fileInput",
                      label = "Upload S-STEM Survey Data",
                      accept = c(".xlsx", ".csv")
                    ),
                    
                    ## EOG File Input ----
                    fileInput(
                      inputId = "EOG_fileInput",
                      label = "Upload EOG Data",
                      accept = c(".xlsx", ".csv")
                    )
                  )
                )
  ) # end dashboard Page
) # end ShinyUI