#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(kableExtra)
library(DTedit)

model_base <- data.table::fread("C:/Users/hu.yang/Projects/SN Time Series/Brand_Awareness_TS_Models_Demo/Brand_Awareness_TS_Models_Demo_V3/brand_awareness_base.csv") %>%
  mutate(date = lubridate::mdy(date))


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
  titlePanel(title=div(img(src="Dynata.png"), "SN Brand Health Time Series Analysis")),
  
  shinyjs::useShinyjs(),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      pickerInput(
        inputId = "varSelect",
        label = "Select Variable for Visualization:", 
        choices = list(
          
          "Dependent Variables" = c("Awareness",
                                    "Consideration",
                                    "Familiarity",
                                    "Favorability",
                                    "Recommend"
          ),
          
          "Exogenous Variables" = c("Brand Digital GCM Impressions", 
                                    "NonBrand Digital GCM Impressions", 
                                    "Print Impressions", 
                                    "TV impressions",
                                    "Events", 
                                    "3rd party content PVs",
                                    "3rd party social impressions",
                                    "3rd Party Social Shares"))
      ),
      
      hr(),
      
      tabsetPanel( 
        
        tabPanel("Imputation/Tranformation",
                 
                 p(""),
                 
                 awesomeRadio(
                   inputId = "missingImputationMethod",
                   label = "Missing Value Imputation by Interpolation:",
                   choices = c("None", "Linear", "Spline", "Stine", "All 0"),
                   selected = "None",
                   inline = TRUE
                 ),
                 
                 p(""),
                 
                 pickerInput(
                   inputId = "varsTransSelect",
                   label = "Independent Variables for Imputation and Transformation:", 
                   choices = c("Brand Digital GCM Impressions", 
                               "NonBrand Digital GCM Impressions", 
                               "Print Impressions", 
                               "TV impressions",
                               "Events",
                               "3rd party content PVs",
                               "3rd party social impressions",
                               "3rd Party Social Shares"),
                   options = list(
                     `actions-box` = TRUE), 
                   
                   multiple = TRUE
                 ),
                 
                 p(""),
                 
                 shinyjs::disabled(awesomeRadio(
                   inputId = "varTransformation",
                   label = "Transformations Options:",
                   choices = c("None", "Log", "Box-Cox after log", "Normalization after log (Min:0, Max:1)", "Standardization after log (Mean:0, SD:1)"),
                   selected = "None"
                 )),
                 
                 fluidRow(
                   actionButton("apply2Data", label = "Apply to Data"),
                   actionButton("resetData", label = "Reset"),
                   progressBar(
                     id = "pb1",
                     value = 0,
                     title = "",
                     display_pct = TRUE
                   )
                 )
        ),
        
        
        
        tabPanel("Trend Line",
                 
                 p(""),
                 
                 awesomeCheckbox(
                   inputId = "trendLine",
                   label = "Trend Line", 
                   value = FALSE
                 ),
                 
                 
                 sliderInput("trendSmoothDegree", "Trend Smooth Degree:",
                             min = 0, max = 2,
                             value = 0, step = 1),
                 
                 awesomeRadio(
                   inputId = "trendTestCL",
                   label = "Trend Test Confidence Level:",
                   choices = c("95%", "90%", "85%", "80%"),
                   selected = "95%",
                   inline = TRUE
                 )
                 
        ),
        
        tabPanel("Anomaly Detection",
                 
                 p(""),
                 
                 sliderInput("anomalyMaxium", "Anomaly Detection (Maximum percent allowed):",
                             min = 0, max = 1,
                             value = 0.2, step = 0.1),
                 
                 awesomeRadio(
                   inputId = "anomalyCL",
                   label = "Anomaly Detection Confidence Level:",
                   choices = c("95%", "90%", "85%", "80%"),
                   selected = "80%",
                   inline = TRUE
                 )
        ),
        
        
        tabPanel("Models Parameters",
                 
                 p(""),
                 
                 pickerInput(
                   inputId = "depVarSelect",
                   label = "Dependent Variable:", 
                   choices = c("Awareness",
                               "Consideration",
                               "Familiarity",
                               "Favorability",
                               "Recommend"
                   )
                 ),
                 
                 pickerInput(
                   inputId = "indepVarSelect",
                   label = "Independent Variable:", 
                   choices = c("Brand Digital GCM Impressions", 
                               "NonBrand Digital GCM Impressions", 
                               "Print Impressions", 
                               "TV impressions",
                               "Events",
                               "3rd party content PVs",
                               "3rd party social impressions",
                               "3rd Party Social Shares"),
                   options = list(
                     `actions-box` = TRUE), 
                   
                   multiple = TRUE
                 ),
                 
                 pickerInput(
                   inputId = "indepVarForced",
                   label = "Forced Independent Variable:", 
                   choices = c(""),
                   options = list(
                     `actions-box` = TRUE), 
                   
                   multiple = TRUE
                 ),
                 
                 actionButton("buildModels", label = "Build Regression Models"),
                 
                 progressBar(
                   id = "pb2",
                   value = 0,
                   title = "",
                   display_pct = TRUE
                 )
                 
        ),
        
        
        tabPanel("Models Selection",
                 
                 p(""),
                 
                 pickerInput(
                   inputId = "modelsSelect",
                   label = "Select Models:", 
                   choices = c("No Model Selected"
                   )
                 ),
                 
                 awesomeCheckboxGroup(
                   inputId = "forecastCI",
                   label = "Forecast Confidence Interval:",
                   choices = c("95%", "90%", "85%", "80%"),
                   selected = c("95%", "80%"),
                   inline = TRUE
                 ),
                 
                 pickerInput(
                   inputId = "refDateSelect",
                   label = "Select Reference Date:", 
                   choices = c("Not Selected", as.character(model_base$date)),
                   selected = "Not Selected"
                 ),
                 
                 
                 pickerInput(
                   inputId = "varsLiftSelect",
                   label = "Independent Variable for Lift Chart:", 
                   choices = c(""),
                   options = list(
                     `actions-box` = TRUE)
                 ),
        )
        
        
      ),
      
      
        
        
      
      
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      
      tabsetPanel(
        
        tabPanel("Visualization & Preprocessing",
                 p(""),
                 
                 plotlyOutput('tsPlot', height = 600) %>% withSpinner(color="#0dc5c1")
                 
        ),
        
        
        tabPanel("Distribution Table/Plots",
                 
                 p(""),
                 DTOutput(outputId = "data_summary_table") %>% withSpinner(color="#0dc5c1"),
                 p("*Skewness Normality Test (D'Agostino test), NH: normal distribution"),
                 
                 tags$br(),
                 tags$br(),
                 
                 plotOutput('tsHistPlots', height = 900) %>% withSpinner(color="#0dc5c1")
                 
        ),
        
        
        tabPanel("Time Series Features",
                 p(""),
                 
                 fluidRow(
                   column(6, plotlyOutput('tsSTLPlot', height = 600) %>% withSpinner(color="#0dc5c1")),
                   column(6, plotlyOutput('tsSeasonalDiagPlot', height = 600) %>% withSpinner(color="#0dc5c1"))
                 ),
                 
                 p(""),
                 p(""),
                 
                 fluidRow(
                   column(6, plotlyOutput('tsAnomalyPlot', height = 600) %>% withSpinner(color="#0dc5c1")),
                   column(6, plotlyOutput('tsACSPACFPlot', height = 600) %>% withSpinner(color="#0dc5c1"))
                 )
        ),
        
        tabPanel("Correlation Plots",
                 
                 p(""),
                 
                 plotOutput('tsCorrPlot', height = 1200) %>% withSpinner(color="#0dc5c1")
                 
        ),
        
        
        tabPanel("Scatter Plots",
                 
                 p(""),
                 
                 plotOutput('tsScatterPlot', height = 1200) %>% withSpinner(color="#0dc5c1")
                 
        ),
        
        tabPanel("Models Table",
                 p(""),
                 
                 fluidRow(
                   column(8, tableOutput(outputId = "models_table")),
                   column(4, 
                          p(""),
                          plotlyOutput('tsModelR2Plot') %>% withSpinner(color="#0dc5c1"),
                          p(""),
                          plotlyOutput('tsModelAICPlot') %>% withSpinner(color="#0dc5c1")
                   )
                 ),
                 
                 tableOutput(outputId = "models_vif_table") %>% withSpinner(color="#0dc5c1"),
        ),
        
        tabPanel("Models Fitting Plots",
                 p(""),
                 
                 plotlyOutput('tsModelsFittingPlot', height = 600) %>% withSpinner(color="#0dc5c1")
        ),
        
        
        tabPanel("Models Sensitivity Analysis",
                 p(""),
                 
                 plotlyOutput('tsModelsSensitivityPlot', height = 600) %>% withSpinner(color="#0dc5c1"),
                 plotlyOutput('tsModelsResidualsPlot', height = 200) %>% withSpinner(color="#0dc5c1"),
                 
                 tags$br(),
                 DTOutput("modelInfo") %>% withSpinner(color="#0dc5c1")
        ),
        
        
        
        tabPanel("Models Lifts Analysis",
                 p(""),
                 plotlyOutput('tsModelsLiftsPlot', height = 600) %>% withSpinner(color="#0dc5c1"),
                 
        ),
        
        tabPanel("Simulation Analysis",
                 p(""),
                 
                 plotlyOutput('tsModelsSimulationPlot', height = 600) %>% withSpinner(color="#0dc5c1"),
                 
                 tags$br(),
                 
                 p(""),
                 p("Simulation Table:"),
                 fluidRow(
                   column(8, uiOutput('simulationTable') %>% withSpinner(color="#0dc5c1")),
                   column(4, htmlOutput('blankTest'))
                 )
                 
        )
        
        
        
        
      )
      
      
    )
  )
  
))
