library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinybusy)
library(ggplot2)
library(readxl)
library(corrplot)
library(DT)
library(car)
library(performance)
library(bslib)
library(rmarkdown)
library(knitr)
library(tinytex) # Required for PDF report generation
library(forecast) # For time series plots and decomposition, and forecasting models
library(tseries) # For ACF/PACF plots
library(plm) # For panel data models (Fixed Effects, Random Effects)
library(lmtest) # For statistical tests like Breusch-Pagan
library(plotly) # For interactive plots
library(survival) # For Cox Regression
library(survminer) # For Cox Regression plots (if needed, not fully implemented in UI yet)
# library(later) # Removed as it was causing persistent issues

# Define the application's user interface
light_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly", # A clean, light theme
  primary = "#007BFF", # A classic, strong blue for key elements
  secondary = "#6C757D", # Standard dark gray secondary color
  success = "#28A745",
  info = "#17A2B8",
  warning = "#FFC107",
  danger = "#DC3545",
  base_font = font_google("Inter"), # Modern sans-serif font for readability
  heading_font = font_google("Inter")
)

ui <- fluidPage(
  theme = light_theme,
  tags$head(
    tags$style(HTML("
      .navbar, .footer { display: none; }

      body {
        background-color: #F8F9FA;
        color: #212529;
        font-family: 'Inter', sans-serif;
      }

      .card {
        background-color: #FFFFFF;
        border-radius: 15px;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
        transition: transform 0.3s ease-in-out, box-shadow 0.3s ease-in-out;
        padding: 30px;
        margin-bottom: 30px;
        border: 1px solid #E0E0E0;
      }
      .card:hover {
        transform: translateY(-7px);
        box-shadow: 0 12px 35px rgba(0, 0, 0, 0.15);
      }

      .main-content-wrapper {
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }

      .btn-primary, .btn-secondary, .btn-info {
        font-weight: bold;
        transition: background-color 0.3s ease, border-color 0.3s ease, transform 0.2s ease, box-shadow 0.2s ease;
        border-radius: 10px;
        padding: 14px 28px;
        font-size: 1.1em;
        letter-spacing: 0.5px;
      }
      .btn-primary:hover {
        background-color: \"#0056B3\";
        border-color: \"#004085\";
        transform: translateY(-4px);
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.15);
      }
      .btn-secondary:hover {
        background-color: \"#5A6268\";
        border-color: \"#545b62\";
        transform: translateY(-4px);
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.15);
      }
      .btn-info:hover {
        background-color: \"#138496\";
        border-color: \"#117a8b\";
        transform: translateY(-4px);
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.15);
      }

      .btn-download-plot {
        background-color: \"#6C757D\";
        border-color: \"#6C757D\";
        color: \"#FFFFFF\";
        padding: 10px 18px;
        font-size: 0.95em;
        border-radius: 8px;
        margin-top: 15px;
        margin-right: 10px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
      }
      .btn-download-plot:hover {
        background-color: \"#5A6268\";
        border-color: \"#545b62\";
        transform: translateY(-2px);
        box-shadow: 0 6px 15px rgba(0, 0, 0, 0.15);
      }

      .shiny-input-container:not(.shiny-input-container-inline) {
        margin-bottom: 30px;
      }
      .form-control, .selectize-input, .picker-input {
        border-radius: 12px;
        padding: 14px 20px;
        background-color: #F8F9FA;
        border: 1px solid #CED4DA;
        color: #212529;
      }
      .selectize-input.focus, .selectize-input.dropdown-active {
        border-color: \"#007BFF\" !important;
        box-shadow: 0 0 0 0.25rem rgba(0, 123, 255, 0.25);
      }
      .selectize-dropdown-content .option {
        background-color: #FFFFFF;
        color: #212529;
      }
      .selectize-dropdown-content .option.active {
        background-color: \"#007BFF\";
        color: #FFFFFF;
      }

      .busy-spinner-text {
        font-size: 1.4em;
        font-weight: bold;
        color: #212529;
      }
      .busy-spinner-container {
        background-color: rgba(255, 255, 255, 0.85);
      }

      .accordion-item {
        border: 1px solid #E0E0E0;
        border-radius: 15px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
      }
      .accordion-button {
        background-color: #E9ECEF;
        color = \"#212529\";
        font-weight: bold;
        font-size: 1.2em;
        padding: 20px 25px;
        border-radius: 15px;
        transition: background-color 0.3s ease, color 0.3s ease;
      }
      .accordion-button:not(.collapsed) {
        background-color: \"#007BFF\";
        color: \"#FFFFFF\";
        border-bottom-left-radius: 0;
        border-bottom-right-radius: 0;
      }
      .accordion-button:focus {
        box-shadow: 0 0 0 0.25rem rgba(0, 123, 255, 0.25);
      }
      .accordion-body {
        background-color: #FFFFFF;
        padding: 30px;
        border-bottom-left-radius: 15px;
        border-bottom-right-radius: 15px;
      }

      h1 {
        font-family: 'Montserrat', sans-serif;
        font-size: 4.5em;
        font-weight: 900;
        margin-bottom: 60px;
        color: \"#007BFF\";
        text-shadow: 3px 3px 8px rgba(0, 123, 255, 0.3);
        letter-spacing: 2px;
        text-align: center;
        text-transform: uppercase;
      }
      h2 { font-size: 2.5em; font-weight: 600; color: \"#343A40\"; }
      h3 { font-size: 2em; font-weight: 600; color: \"#007BFF\"; }
      h4 { font-size: 1.8em; font-weight: 500; margin-bottom: 25px; color = \"#495057\"; }
      h5 { font-size: 1.3em; font-weight: 500; margin-top: 30px; margin-bottom: 12px; color = \"#6C757D\"; }
      p {
        font-family: 'Inter', sans-serif;
        color: #495057;
        line-height: 1.8;
        font-size: 1.1em;
      }
      .form-group label {
        font-size: 1.15em;
        color: #343A40;
        margin-bottom: 10px;
      }

      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_processing,
      .dataTables_wrapper .dataTables_paginate {
        color: #495057 !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #495057 !important;
        border: 1px solid #CED4DA !important;
        background-color: #F0F2F5 !important;
        border-radius: 8px !important;
        margin: 0 5px !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background-color: \"#007BFF\" !important;
        color: \"#FFFFFF\" !important;
        border-color: \"#007BFF\" !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
        color: #ADB5BD !important;
      }
      table.dataTable thead th {
        color: #343A40;
        background-color: #E9ECEF;
        border-bottom: 1px solid #CED4DA !important;
      }
      table.dataTable tbody td {
        color: #495057;
        background-color: #FFFFFF;
        border-top: 1px solid #CED4DA !important;
      }
      table.dataTable.no-footer {
        border-bottom: 1px solid #CED4DA !important;
      }
      pre {
        background-color: #E9ECEF;
        color: #343A40;
        border-radius: 10px;
        padding: 20px;
        border: 1px solid #CED4DA;
        overflow-x: auto;
      }

      .upload-section {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        padding: 40px;
        border: 2px dashed \"#007BFF\";
        border-radius: 20px;
        background-color: #F0F2F5;
        margin: 30px auto;
        max-width: 600px;
        text-align: center;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
      }
      .upload-section .file-input-container {
        margin-bottom: 20px;
      }
      .upload-section .btn {
        margin: 10px;
      }
      .fileinput-button {
        background-color: \"#007BFF\";
        color: \"#FFFFFF\";
        border-color: \"#007BFF\";
        border-radius: 10px;
        padding: 12px 25px;
        font-weight: bold;
        transition: background-color 0.3s ease, border-color 0.3s ease, transform 0.2s ease, box-shadow 0.2s ease;
      }
      .fileinput-button:hover {
        background-color: \"#0056B3\";
        border-color: \"#004085\";
        transform: translateY(-3px);
        box-shadow: 0 6px 15px rgba(0, 0, 0, 0.15);
      }
      .file-caption-name {
        background-color: #E9ECEF !important;
        border: 1px solid #CED4DA !important;
        color: #212529 !important;
        border-radius: 10px !important;
      }

      .welcome-section {
        background-color: #FFFFFF;
        border-radius: 15px;
        padding: 30px;
        margin-bottom: 30px;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
        text-align: center;
      }
      .welcome-section h3, .welcome-section h4 {
        text-align: center;
      }
      .key-capabilities-card {
        min-height: 250px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        text-align: left;
      }
      .key-capabilities-card h5 {
        color: \"#007BFF\";
        font-size: 1.3em;
        margin-bottom: 15px;
      }
      .key-capabilities-card p, .key-capabilities-card ul {
        font-size: 0.95em;
        line-height: 1.6;
        color: #495057;
      }
      .key-capabilities-card ul {
        list-style: none;
        padding-left: 0;
      }
      .key-capabilities-card ul li:before {
        content: '⚡';
        margin-right: 8px;
        color: #FFC107;
      }
      .author-bio-section {
        background-color: #F0F2F5;
        border-radius: 15px;
        padding: 30px;
        margin-top: 30px;
        border: 1px solid #D0D0D0;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.08);
        text-align: left;
      }
    "))
  ),
  use_busy_spinner(spin = "fading-circle", color = "#007BFF"),
  titlePanel(h1("Modulus")),
  div(class = "main-content-wrapper",
      uiOutput("status"),
      div(class = "welcome-section",
          h3("Welcome to the Interactive Data Analysis Dashboard!"),
          uiOutput("welcomeMessage"),
          hr(),
          div(class = "upload-section",
              h4("Ready to Analyze? Upload Your Data!"),
              fileInput("file", "Choose CSV or Excel File", accept = c(".csv", ".xlsx"), width = "100%"),
              actionButton("loadExample", "Or Load Example Data 📊", class = "btn btn-secondary")
          ),
          hr(),
          h4("Quick Data Snapshot 📸"),
          fluidRow(
            column(6, uiOutput("dataSummaryStats")),
            column(6, plotlyOutput("randomPlot1", height = "300px")) # Increased height
          ),
          fluidRow(
            column(12, plotlyOutput("randomPlot2", height = "300px")) # Increased height
          ),
          hr(),
          h4("Key Analytical Capabilities 💡"),
          fluidRow(
            column(4, div(class = "card key-capabilities-card",
                          h5("Explore Distributions"),
                          p("Uncover the hidden patterns and inherent structures within your data. Visualize how variables are spread, where they cluster, and identify outliers that tell unique stories."),
                          tags$ul(tags$li("Histograms"), tags$li("Density Plots"), tags$li("Box & Violin Plots"), tags$li("Categorical Distributions"))
            )),
            column(4, div(class = "card key-capabilities-card",
                          h5("Model Relationships"),
                          p("Decipher the intricate connections between variables. Build predictive models to forecast future outcomes and understand the driving forces behind observed phenomena."),
                          tags$ul(tags$li("Linear Regression"), tags$li("Logit Regression"), tags$li("Cox Regression"))
            )),
            column(4, div(class = "card key-capabilities-card",
                          h5("Analyze Trends & Groups"),
                          p("Track dynamic changes over time and compare distinct groups within your dataset. Pinpoint significant differences and long-term trajectories with specialized tools."),
                          tags$ul(tags$li("Time Series Analysis"), tags$li("Panel Data Models"), tags$li("Hypothesis Tests"))
            ))
          ),
          hr(),
          div(class = "author-bio-section",
              h4("About the Architect of Insights ✍️"),
              uiOutput("authorBio")
          )
      ),
      
      hr(style = "margin: 50px 0; border-top: 2px solid #E0E0E0;"),
      
      bslib::accordion(
        id = "analysisAccordion",
        open = FALSE,
        bslib::accordion_panel(title = span(icon("cogs"), "Data Transformation"), value = "data_transformation",
                               h3("Transform Your Data ⚙️"),
                               p("Here, you can manipulate your dataset to create new variables or modify existing ones, preparing it for deeper analysis."),
                               
                               h4("Categorize Numeric Variable"),
                               p("Categorize a numeric variable into bins."),
                               uiOutput("categorizeVarUI"),
                               actionButton("categorizeData", "Categorize Variable", class = "btn btn-info"),
                               
                               hr(),
                               
                               h4("Transform Existing Variable"),
                               p("Apply mathematical transformations or create interaction terms for numeric variables."),
                               fluidRow(
                                 column(6, uiOutput("transformVarUI")),
                                 column(6, selectInput("transformType", "Transformation Type",
                                                       choices = c("", "Log (natural)", "Square", "Lag", "Interaction"),
                                                       selected = ""))
                               ),
                               conditionalPanel(
                                 condition = "input.transformType == 'Lag'",
                                 numericInput("lagValue", "Lag Value (n)", value = 1, min = 1, step = 1)
                               ),
                               conditionalPanel(
                                 condition = "input.transformType == 'Interaction'",
                                 uiOutput("interactionVar2UI")
                               ),
                               textInput("newTransformedVarName", "New Transformed Variable Name", value = "transformed_var"),
                               actionButton("transformData", "Apply Transformation", class = "btn btn-info"),
                               # Placeholder for dynamic transformation messages (now handled by showNotification)
                               div(id = "transformation_message_placeholder", style = "margin-top: 15px;"),
                               
                               hr(),
                               actionButton("clearData", "Clear Data 🗑️", class = "btn btn-secondary", style = "margin-top: 15px;")
        ),
        bslib::accordion_panel(title = span(icon("chart-pie"), "Descriptive Analysis"), value = "descriptive_analysis",
                               h4("Data Overview"),
                               DTOutput("dataHead"), # No longer showing only first 6 rows
                               h4("Column Data Types"),
                               verbatimTextOutput("dataStructure"),
                               h4("Summary Statistics"),
                               p("Select variables to summarize:"),
                               uiOutput("summaryVarsUI"),
                               actionButton("runSummary", "Generate Summary", class = "btn btn-primary"),
                               DTOutput("summary"), # No longer showing only first 6 rows
                               hr(),
                               h4("Distribution Histograms"),
                               p("Select numeric variables for histograms:"),
                               uiOutput("histVarsUI"),
                               actionButton("runHistograms", "Generate Histograms", class = "btn btn-primary"),
                               uiOutput("histPlots"),
                               conditionalPanel(
                                 condition = "output.histPlots != null",
                                 downloadButton("downloadHistograms", "Download All Histograms", class = "btn-download-plot")
                               ),
                               hr(),
                               h4("Distribution Density Plots"),
                               p("Select numeric variables for density plots:"),
                               uiOutput("densityVarsUI"),
                               actionButton("runDensityPlots", "Generate Density Plots", class = "btn btn-primary"),
                               uiOutput("densityPlots"),
                               conditionalPanel(
                                 condition = "output.densityPlots != null",
                                 downloadButton("downloadDensityPlots", "Download All Density Plots", class = "btn-download-plot")
                               ),
                               hr(),
                               h4("Box and Violin Plots for Group Comparisons"),
                               fluidRow(
                                 column(6, uiOutput("boxViolinNumVarUI")),
                                 column(6, uiOutput("boxViolinCatVarUI"))
                               ),
                               selectInput("boxViolinPlotType", "Select Plot Type", choices = c("Box Plot", "Violin Plot"), selected = "Box Plot"),
                               actionButton("runBoxViolinPlot", "Generate Plot", class = "btn btn-primary"),
                               plotOutput("boxViolinPlot", height = "500px"), # Increased height
                               conditionalPanel(
                                 condition = "output.boxViolinPlot != null",
                                 downloadButton("downloadBoxViolinPlot", "Download Plot", class = "btn-download-plot")
                               ),
                               hr(),
                               h4("Variable Correlation"),
                               p("Select numeric variables for correlation matrix:"),
                               uiOutput("corrVarsUI"),
                               actionButton("runCorrelation", "Generate Correlation Matrix", class = "btn btn-primary"),
                               plotOutput("corrPlot", height = "600px"), # Increased height
                               conditionalPanel(
                                 condition = "output.corrPlot != null",
                                 downloadButton("downloadCorrPlot", "Download Plot", class = "btn-download-plot")
                               ),
                               hr(),
                               h4("Scatter Plots"),
                               p("Select two numeric variables for a scatter plot:"),
                               fluidRow(
                                 column(6, uiOutput("dotPlotXVarUI")),
                                 column(6, uiOutput("dotPlotYVarUI"))
                               ),
                               actionButton("runDotPlot", "Generate Scatter Plot", class = "btn btn-primary"),
                               plotlyOutput("singleDotPlot", height = "500px"), # Increased height
                               hr(),
                               h4("Scatter Plot Matrix (Pairs Plot)"),
                               p("Select multiple numeric variables to view their pairwise relationships:"),
                               uiOutput("pairsPlotVarsUI"),
                               actionButton("runPairsPlot", "Generate Pairs Plot", class = "btn btn-primary"),
                               plotOutput("pairsPlot", height = "600px"), # Increased height
                               conditionalPanel(
                                 condition = "output.pairsPlot != null",
                                 downloadButton("downloadPairsPlot", "Download Plot", class = "btn-download-plot")
                               ),
                               hr(),
                               # Removed Data Heatmap section
                               h4("Categorical Variable Distributions"),
                               p("Select a categorical variable to view its distribution (Bar/Pie Chart):"),
                               uiOutput("catDistVarUI"),
                               selectInput("catDistPlotType", "Select Plot Type", choices = c("Bar Chart", "Pie Chart"), selected = "Bar Chart"),
                               actionButton("runCatDist", "Generate Plot", class = "btn btn-primary"),
                               plotOutput("catDistPlot", height = "500px"), # Increased height
                               conditionalPanel(
                                 condition = "output.catDistPlot != null",
                                 downloadButton("downloadCatDistPlot", "Download Plot", class = "btn-download-plot")
                               )
        ),
        bslib::accordion_panel(title = span(icon("flask"), "Advanced Models"), value = "advanced_models",
                               h3("Linear Regression Model Diagnostics"),
                               p("Select one dependent and one or more independent numeric variables:"),
                               fluidRow(
                                 column(6, uiOutput("depVarRegUI")),
                                 column(6, uiOutput("indepVarsRegUI"))
                               ),
                               actionButton("runRegression", "Run Regression Analysis", class = "btn btn-primary"),
                               uiOutput("regressionMessage"),
                               
                               # Linear Regression Outputs - Enhanced Layout
                               h4("Model Summary"),
                               verbatimTextOutput("regSummary"),
                               
                               h4("Assumptions Check: Residual Plots"),
                               fluidRow(
                                 column(6, 
                                        plotOutput("residualPlot", height = "400px"),
                                        conditionalPanel(
                                          condition = "output.residualPlot != null",
                                          downloadButton("downloadResidualPlot", "Download Residuals vs Fitted Plot", class = "btn-download-plot")
                                        )
                                 ),
                                 column(6, 
                                        plotOutput("qqPlot", height = "400px"),
                                        conditionalPanel(
                                          condition = "output.qqPlot != null",
                                          downloadButton("downloadQQPlot", "Download Q-Q Plot", class = "btn-download-plot")
                                        )
                                 )
                               ),
                               fluidRow( # New row for Scale-Location and Residuals vs Leverage
                                 column(6,
                                        plotOutput("scaleLocationPlot", height = "400px"),
                                        conditionalPanel(
                                          condition = "output.scaleLocationPlot != null",
                                          downloadButton("downloadScaleLocationPlot", "Download Scale-Location Plot", class = "btn-download-plot")
                                        )
                                 ),
                                 column(6,
                                        plotOutput("residualsLeveragePlot", height = "400px"),
                                        conditionalPanel(
                                          condition = "output.residualsLeveragePlot != null",
                                          downloadButton("downloadResidualsLeveragePlot", "Download Residuals vs Leverage Plot", class = "btn-download-plot")
                                        )
                                 )
                               ),
                               fluidRow( # New row for Cook's Distance
                                 column(12,
                                        plotOutput("cooksDistancePlot", height = "400px"),
                                        conditionalPanel(
                                          condition = "output.cooksDistancePlot != null",
                                          downloadButton("downloadCooksDistancePlot", "Download Cook's Distance Plot", class = "btn-download-plot")
                                        )
                                 )
                               ),
                               
                               h4("Residuals vs. Predictors Plots"),
                               uiOutput("residualsVsPredictorsPlots"),
                               conditionalPanel(
                                 condition = "output.residualsVsPredictorsPlots != null",
                                 downloadButton("downloadResidualsVsPredictorsPlots", "Download All Residuals vs Predictors Plots", class = "btn-download-plot")
                               ),
                               
                               h4("Multicollinearity Check: Variance Inflation Factor (VIF)"),
                               verbatimTextOutput("vif"),
                               
                               h4("Heteroskedasticity Test: Breusch-Pagan Test"),
                               verbatimTextOutput("heteroskedasticity"),
                               hr(),
                               
                               h3("Time Series Plots and Diagnostics"),
                               p("Select a numeric variable for the time series and a time index column:"),
                               fluidRow(
                                 column(4, uiOutput("tsVarUI")),
                                 column(4, uiOutput("tsTimeVarUI")),
                                 column(4, numericInput("tsFrequency", "Time Series Frequency (e.g., 12 for monthly)", value = 1, min = 1, step = 1))
                               ),
                               actionButton("runTSAnalysis", "Run Time Series Analysis", class = "btn btn-primary"),
                               uiOutput("tsAnalysisMessage"),
                               
                               # Time Series Outputs - Enhanced Layout
                               h4("Time Series Plot"),
                               plotlyOutput("timeSeriesPlot", height = "500px"),
                               
                               h4("Autocorrelation Plots"),
                               fluidRow(
                                 column(6, plotOutput("acfPlot", height = "400px")),
                                 column(6, plotOutput("pacfPlot", height = "400px"))
                               ),
                               conditionalPanel(
                                 condition = "output.acfPlot != null",
                                 downloadButton("downloadAcfPacfPlots", "Download ACF/PACF Plots", class = "btn-download-plot")
                               ),
                               
                               h4("Time Series Seasonality (if applicable)"),
                               plotOutput("seasonalityPlot", height = "500px"),
                               conditionalPanel(
                                 condition = "output.seasonalityPlot != null",
                                 downloadButton("downloadSeasonalityPlot", "Download Seasonality Plot", class = "btn-download-plot")
                               ),
                               
                               h4("Autocorrelation of Residuals (from ARIMA Model)"),
                               plotOutput("residualAcfPlot", height = "400px"),
                               conditionalPanel(
                                 condition = "output.residualAcfPlot != null",
                                 downloadButton("downloadResidualAcfPlot", "Download Residual ACF Plot", class = "btn-download-plot")
                               ),
                               
                               h4("Time Series Decomposition"),
                               plotOutput("tsDecompositionPlot", height = "600px"),
                               conditionalPanel(
                                 condition = "output.tsDecompositionPlot != null",
                                 downloadButton("downloadDecompositionPlot", "Download Decomposition Plot", class = "btn-download-plot")
                               ),
                               
                               h4("Lag Plot"),
                               plotOutput("lagPlot", height = "500px"),
                               conditionalPanel(
                                 condition = "output.lagPlot != null",
                                 downloadButton("downloadLagPlot", "Download Lag Plot", class = "btn-download-plot")
                               ),
                               hr(),
                               
                               h4("Time Series Forecasting"),
                               fluidRow(
                                 column(4, selectInput("forecastModel", "Select Forecast Model", choices = c("ARIMA", "ETS"), selected = "ARIMA")),
                                 column(4, numericInput("forecastHorizon", "Forecast Horizon (steps)", value = 12, min = 1, step = 1)),
                                 column(4, actionButton("runForecast", "Run Forecast", class = "btn btn-primary", style = "margin-top: 30px;"))
                               ),
                               plotlyOutput("forecastPlot", height = "500px"), # Increased height
                               h5("Forecast Values"),
                               DTOutput("forecastTable"), # All rows with scrolling
                               h5("Forecast Accuracy Metrics"),
                               verbatimTextOutput("forecastAccuracy"),
                               hr(),
                               
                               h3("Time Series Regression (ARIMAX)"),
                               p("Select a numeric dependent variable, a time index, and one or more independent (exogenous) variables for ARIMAX modeling:"),
                               fluidRow(
                                 column(4, uiOutput("arimaxDepVarUI")),
                                 column(4, uiOutput("arimaxTimeVarUI")),
                                 column(4, numericInput("arimaxFrequency", "Time Series Frequency (e.g., 12 for monthly)", value = 1, min = 1, step = 1))
                               ),
                               fluidRow(
                                 column(12, uiOutput("arimaxIndepVarsUI"))
                               ),
                               actionButton("runARIMAX", "Run ARIMAX Model", class = "btn btn-primary"),
                               uiOutput("arimaxMessage"),
                               h4("ARIMAX Model Summary"),
                               verbatimTextOutput("arimaxSummary"),
                               h4("ARIMAX Residuals Diagnostics"),
                               plotOutput("arimaxResidualsPlot", height = "400px"),
                               conditionalPanel(
                                 condition = "output.arimaxResidualsPlot != null",
                                 downloadButton("downloadARIMAXResidualsPlot", "Download ARIMAX Residuals Plot", class = "btn-download-plot")
                               ),
                               hr(),
                               
                               h4("Panel Data Models"),
                               p("Select dependent, independent, grouping, and time variables for panel analysis:"),
                               fluidRow(
                                 column(6, uiOutput("panelDepVarUI")),
                                 column(6, uiOutput("panelIndepVarsUI"))
                               ),
                               fluidRow(
                                 column(6, uiOutput("panelGroupVarUI")),
                                 column(6, uiOutput("panelTimeVarUI"))
                               ),
                               actionButton("runPanelAnalysis", "Run Panel Data Analysis", class = "btn btn-primary"),
                               uiOutput("panelAnalysisMessage"),
                               h5("Pooled OLS Model Summary"),
                               verbatimTextOutput("pooledOLS_summary"),
                               h5("Fixed Effects Model Summary"),
                               verbatimTextOutput("fixedEffects_summary"),
                               h5("Random Effects Model Summary"),
                               verbatimTextOutput("randomEffects_summary"),
                               h5("Hausman Test (Fixed vs. Random Effects)"),
                               verbatimTextOutput("hausmanTest"),
                               h5("F-Test (Pooled OLS vs. Fixed Effects)"),
                               verbatimTextOutput("fTestPooledVsFE"),
                               hr(),
                               h4("Logit Regression Model"),
                               p("Select a binary (0/1 or TRUE/FALSE) dependent variable and independent variables:"),
                               fluidRow(
                                 column(6, uiOutput("logitDepVarUI")),
                                 column(6, uiOutput("logitIndepVarsUI"))
                               ),
                               actionButton("runLogitRegression", "Run Logit Regression", class = "btn btn-primary"),
                               uiOutput("logitRegressionMessage"),
                               h5("Logit Model Summary"),
                               verbatimTextOutput("logitSummary"),
                               h5("Odds Ratios"),
                               verbatimTextOutput("logitOddsRatios"),
                               hr(),
                               h4("Cox Proportional Hazards Model"),
                               p("Select time, event (binary), and independent variables for survival analysis:"),
                               fluidRow(
                                 column(4, uiOutput("coxTimeVarUI")),
                                 column(4, uiOutput("coxEventVarUI")),
                                 column(4, uiOutput("coxIndepVarsUI"))
                               ),
                               actionButton("runCoxRegression", "Run Cox Regression", class = "btn btn-primary"),
                               uiOutput("coxRegressionMessage"),
                               h5("Cox Model Summary"),
                               verbatimTextOutput("coxSummary"),
                               h5("Hazard Ratios"),
                               verbatimTextOutput("coxHazardRatios"),
                               hr(),
                               h4("Perform Statistical Hypothesis Tests"),
                               selectInput("testType", "Choose Test Type:",
                                           choices = c("",
                                                       "One-Sample t-test",
                                                       "Two-Sample t-test",
                                                       "ANOVA",
                                                       "Chi-squared Test")),
                               uiOutput("testInputsUI"),
                               actionButton("runTest", "Run Test", class = "btn btn-primary", icon = icon("play")),
                               hr(),
                               h5("Test Results"),
                               verbatimTextOutput("testResults"),
                               plotOutput("testPlot", height = "500px"), # Increased height
                               conditionalPanel(
                                 condition = "output.testPlot != null",
                                 downloadButton("downloadTestPlot", "Download Plot", class = "btn-download-plot")
                               )
        ),
        bslib::accordion_panel(title = span(icon("toolbox"), "Utilities"), value = "utilities",
                               h4("Filter Data by Time"),
                               p("Select a time-based column to filter the dataset:"),
                               uiOutput("timeFilterVarUI"),
                               uiOutput("timeFilterUI"),
                               DTOutput("filteredTable"), # All rows with scrolling
                               hr(),
                               h4("Generate Full Analysis Report"),
                               p("Click the button below to generate a comprehensive PDF report of all performed analyses."),
                               downloadButton("downloadReport", "Download Full Report as PDF", class = "btn btn-primary")
        ),
        bslib::accordion_panel(title = span(icon("robot"), "Machine Learning (Coming Soon!)"), value = "machine_learning",
                               h3("Machine Learning Capabilities"),
                               p("This section will feature advanced machine learning models for prediction and clustering."),
                               p("Stay tuned for updates! 🚀")
        )
      )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal()
  numVars <- reactiveVal()
  catVars <- reactiveVal()
  allVars <- reactiveVal()
  
  observeEvent(data(), {
    if (!is.null(data())) {
      df <- data()
      nums <- names(df)[sapply(df, is.numeric)]
      cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
      all_cols <- names(df)
      numVars(nums)
      catVars(cats)
      allVars(all_cols)
    } else {
      numVars(NULL)
      catVars(NULL)
      allVars(NULL)
    }
  })
  
  observeEvent(input$file, {
    show_modal_spinner(text = "Unraveling your data's patterns...", color = "#007BFF")
    ext <- tools::file_ext(input$file$name)
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      } else if (ext == "xlsx") {
        read_excel(input$file$datapath)
      } else {
        stop("Unsupported file type. Please upload a CSV or XLSX file.")
      }
    }, error = function(e) {
      showNotification(paste("Error loading file: ", e$message, ". Please ensure the file is a valid CSV or XLSX and not corrupted."), type = "error", duration = 10)
      NULL
    })
    
    if (!is.null(df)) {
      data(df)
      output$status <- renderUI(tags$h5("Status: Data loaded successfully. Let's find some patterns! ✨"))
      
      # Show all rows and columns with scrolling
      output$dataHead <- renderDT({
        req(data()) # Ensure data is present before rendering
        datatable(data(), options = list(dom = 'Bfrtip', scrollX = TRUE, scrollY = "300px", paging = TRUE))
      })
      output$dataStructure <- renderPrint({
        req(data()) # Ensure data is present before rendering
        str(data())
      })
      
    } else {
      data(NULL)
      output$status <- renderUI(tags$h5("Status: Data loading failed. Please try again. ❌"))
      output$dataHead <- renderDT(NULL)
      output$dataStructure <- renderPrint(NULL)
    }
    remove_modal_spinner()
  })
  
  observeEvent(input$loadExample, {
    show_modal_spinner(text = "Loading a classic dataset to spark new insights...", color = "#007BFF")
    example_df <- mtcars
    data(example_df)
    output$status <- renderUI(tags$h5("Status: Example data loaded successfully. Ready to explore! 🚀"))
    
    # Show all rows and columns with scrolling
    output$dataHead <- renderDT({
      req(data()) # Ensure data is present before rendering
      datatable(data(), options = list(dom = 'Bfrtip', scrollX = TRUE, scrollY = "300px", paging = TRUE))
    })
    output$dataStructure <- renderPrint({
      req(data()) # Ensure data is present before rendering
      str(data())
    })
    remove_modal_spinner()
  })
  
  observeEvent(input$clearData, {
    data(NULL)
    numVars(NULL)
    catVars(NULL)
    allVars(NULL)
    output$dataHead <- renderDT(NULL)
    output$dataStructure <- renderPrint(NULL)
    output$summary <- renderDT(NULL)
    output$histPlots <- renderUI(NULL)
    output$densityPlots <- renderUI(NULL)
    output$boxViolinPlot <- renderPlot(NULL)
    output$corrPlot <- renderPlot(NULL)
    output$singleDotPlot <- renderPlotly(NULL)
    output$pairsPlot <- renderPlot(NULL)
    # output$dataHeatmap <- renderPlot(NULL) # Removed
    output$catDistPlot <- renderPlot(NULL)
    output$regressionMessage <- renderUI(NULL)
    output$regSummary <- renderPrint(NULL)
    output$residualPlot <- renderPlot(NULL)
    output$qqPlot <- renderPlot(NULL)
    output$scaleLocationPlot <- renderPlot(NULL)
    output$residualsLeveragePlot <- renderPlot(NULL)
    output$cooksDistancePlot <- renderPlot(NULL)
    output$residualsVsPredictorsPlots <- renderUI(NULL)
    output$vif <- renderPrint(NULL)
    output$heteroskedasticity <- renderPrint(NULL)
    output$tsAnalysisMessage <- renderUI(NULL)
    output$timeSeriesPlot <- renderPlotly(NULL)
    output$acfPlot <- renderPlot(NULL)
    output$pacfPlot <- renderPlot(NULL)
    output$seasonalityPlot <- renderPlot(NULL)
    output$residualAcfPlot <- renderPlot(NULL)
    output$tsDecompositionPlot <- renderPlot(NULL)
    output$lagPlot <- renderPlot(NULL)
    output$forecastPlot <- renderPlotly(NULL)
    output$forecastTable <- renderDT(NULL)
    output$forecastAccuracy <- renderPrint(NULL)
    output$arimaxMessage <- renderUI(NULL) # Clear new ARIMAX outputs
    output$arimaxSummary <- renderPrint(NULL)
    output$arimaxResidualsPlot <- renderPlot(NULL)
    output$panelAnalysisMessage <- renderUI(NULL)
    output$pooledOLS_summary <- renderPrint(NULL)
    output$fixedEffects_summary <- renderPrint(NULL)
    output$randomEffects_summary <- renderPrint(NULL)
    output$hausmanTest <- renderPrint(NULL)
    output$fTestPooledVsFE <- renderPrint(NULL)
    output$logitRegressionMessage <- renderUI(NULL)
    output$logitSummary <- renderPrint(NULL)
    output$logitOddsRatios <- renderPrint(NULL)
    output$coxRegressionMessage <- renderUI(NULL)
    output$coxSummary <- renderPrint(NULL)
    output$coxHazardRatios <- renderPrint(NULL)
    output$timeFilterUI <- renderUI(NULL)
    output$filteredTable <- renderDT(NULL)
    output$testResults <- renderPrint(NULL)
    output$testPlot <- renderPlot(NULL)
    output$dataSummaryStats <- renderUI(NULL)
    output$randomPlot1 <- renderPlotly(NULL)
    output$randomPlot2 <- renderPlotly(NULL)
    output$welcomeMessage <- renderUI(NULL)
    output$authorBio <- renderUI(NULL)
    
    output$status <- renderUI(tags$h5("Status: Data cleared. Ready for a new analytical adventure. 🧹"))
  })
  
  output$status <- renderUI({
    if (is.null(data())) {
      return(NULL)
    } else {
      NULL
    }
  })
  
  output$welcomeMessage <- renderUI({
    if (is.null(data())) {
      tagList(
        p("This dashboard is your gateway to deciphering complex datasets. Upload your CSV or Excel file to begin a journey of discovery, or load our example data to quickly explore the powerful analytical capabilities."),
        p("Once your data is in, the dashboard will offer an immediate snapshot, guiding you towards deeper statistical insights and compelling visualizations. It's designed for those who love to find meaning in numbers and patterns.")
      )
    } else {
      df <- data()
      n_rows <- nrow(df)
      n_cols <- ncol(df)
      n_numeric <- length(numVars())
      n_categorical <- length(catVars())
      
      tip_of_day <- c(
        "Tip: Expand 'Descriptive Analysis' below to understand data distributions. Dive in!",
        "Tip: Transform your numeric data into insightful categories using the 'Data Transformation' accordion.",
        "Tip: Interactive scatter plots allow you to zoom into relationships between any two variables. Try it!",
        "Tip: For time-dependent data, the 'Time Series Analysis' section offers forecasting and decomposition to reveal underlying trends.",
        "Tip: Exploring data across multiple entities over time? The 'Panel Data Models' are built just for that.",
        "Tip: Use 'Hypothesis Tests' to validate your assumptions and make statistically sound conclusions about your data."
      )
      selected_tip <- sample(tip_of_day, 1)
      
      tagList(
        p(selected_tip, style = "font-style: italic; color: #28A745;"),
        p(paste0("Your dataset, a universe of patterns, contains ", n_rows, " observations and ", n_cols, " fascinating variables (", n_numeric, " numeric, ", n_categorical, " categorical). Let's unlock its secrets!"))
      )
    }
  })
  
  output$dataSummaryStats <- renderUI({
    req(data())
    df <- data()
    num_cols <- numVars()
    cat_cols <- catVars()
    
    tagList(
      h5("Dataset Dimensions:"),
      p(paste0("Rows: ", nrow(df))),
      p(paste0("Columns: ", ncol(df))),
      h5("Variable Types:"),
      p(paste0("Numeric: ", length(num_cols))),
      p(paste0("Categorical: ", length(cat_cols)))
    )
  })
  
  output$randomPlot1 <- renderPlotly({
    req(data(), numVars())
    if (length(numVars()) > 0) {
      random_num_var <- sample(numVars(), 1)
      p <- ggplot(data(), aes_string(x = random_num_var)) +
        geom_histogram(fill = "#007BFF", bins = 15, alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Distribution of", random_num_var), x = "", y = "") +
        theme(legend.position = "none", plot.title = element_text(size = 12, hjust = 0.5, color = "#212529"))
      ggplotly(p) %>% layout(margin = list(l=0, r=0, b=0, t=30))
    } else {
      NULL
    }
  })
  
  output$randomPlot2 <- renderPlotly({
    req(data(), catVars())
    if (length(catVars()) > 0) {
      random_cat_var <- sample(catVars(), 1)
      df_plot <- data.frame(Category = data()[[random_cat_var]])
      df_plot$Category <- as.factor(df_plot$Category)
      
      p <- ggplot(df_plot, aes(x = Category, fill = Category)) +
        geom_bar(alpha = 0.8) +
        theme_minimal() +
        labs(title = paste("Count of", random_cat_var), x = "", y = "") +
        theme(legend.position = "none", plot.title = element_text(size = 12, hjust = 0.5, color = "#212529"),
              axis.text.x = element_text(angle = 45, hjust = 1, color = "#495057"))
      ggplotly(p) %>% layout(margin = list(l=0, r=0, b=0, t=30))
    } else {
      NULL
    }
  })
  
  output$authorBio <- renderUI({
    tagList(
      p("Mashhood Raza Khan is an inquisitive mind at the intersection of economics, data science, and digital innovation. A natural problem-solver with a love for patterns, he builds analytical tools and dashboards that simplify complexity and empower smarter decisions. Whether exploring trends or writing books in his early years, Mashhood thrives where curiosity meets computation—bridging intuition with statistical rigor in a way that's both sharp and creative.")
    )
  })
  
  output$categorizeVarUI <- renderUI({
    req(numVars()) # Ensure numeric variables are available
    tagList(
      selectInput("numVarToCategorize", "Numeric Variable to Categorize", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL), # Added selected for consistency
      numericInput("numBins", "Number of Bins", value = 5, min = 2),
      textInput("newCatVarName", "New Categorical Variable Name", value = "new_category")
    )
  })
  
  observeEvent(input$categorizeData, {
    req(data()) # ADDED THIS CRITICAL LINE
    req(input$numVarToCategorize, input$numBins, input$newCatVarName)
    
    df <- data()
    # Ensure the selected column actually exists in the current data
    if (!(input$numVarToCategorize %in% names(df))) {
      showNotification(paste("Error: Selected variable '", input$numVarToCategorize, "' not found in data."), type = "error")
      return()
    }
    
    if (input$newCatVarName == "") {
      showNotification("Please provide a name for the new categorical variable.", type = "warning")
      return()
    }
    if (input$newCatVarName %in% names(df)) { # Use df here, not data() directly
      showNotification("Variable name already exists. Please choose a different name.", type = "warning")
      return()
    }
    
    num_var <- df[[input$numVarToCategorize]]
    
    if (!is.numeric(num_var)) {
      showNotification("Selected variable is not numeric. Cannot categorize.", type = "error")
      return()
    }
    
    show_modal_spinner(text = "Transforming data, finding new dimensions...", color = "#007BFF")
    withProgress(message = 'Categorizing Variable', value = 0, {
      incProgress(1/1, detail = "Creating bins...")
      tryCatch({
        df[[input$newCatVarName]] <- cut(num_var, breaks = input$numBins, include.lowest = TRUE, dig.lab = 4)
        data(df)
        showNotification(paste("Successfully created new categorical variable:", input$newCatVarName), type = "success")
      }, error = function(e) {
        showNotification(paste("Error categorizing variable:", e$message), type = "error")
      })
    })
    remove_modal_spinner()
  })
  
  # UI for Transform Existing Variable
  output$transformVarUI <- renderUI({
    req(numVars()) # Ensure numeric variables are available
    selectInput("varToTransform", "Select Numeric Variable to Transform", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL) # Added selected for consistency
  })
  
  output$interactionVar2UI <- renderUI({
    req(numVars(), input$varToTransform) # Ensure numeric variables and first variable are selected
    # Exclude the first selected variable from choices for the second variable
    choices_for_second_var <- setdiff(numVars(), input$varToTransform)
    if (length(choices_for_second_var) > 0) {
      selectInput("varToTransform2", "Select Second Variable for Interaction", choices = choices_for_second_var, selected = choices_for_second_var[1])
    } else {
      p("No other numeric variables available for interaction.")
    }
  })
  
  observeEvent(input$transformData, {
    print("--- Starting transformData observer ---")
    req(data())
    req(input$varToTransform, input$transformType, input$newTransformedVarName)
    
    df <- data()
    print(paste("Selected variable to transform:", input$varToTransform))
    print(paste("Transformation type:", input$transformType))
    print(paste("New variable name:", input$newTransformedVarName))
    
    # Ensure the selected column actually exists in the current data
    if (!(input$varToTransform %in% names(df))) {
      showNotification(paste("Error: Selected variable '", input$varToTransform, "' not found in data."), type = "error")
      print(paste("Error: Variable", input$varToTransform, "not found."))
      return()
    }
    
    if (input$newTransformedVarName == "") {
      showNotification("Please provide a name for the new transformed variable.", type = "warning")
      print("Warning: New variable name is empty.")
      return()
    }
    if (input$newTransformedVarName %in% names(df)) {
      showNotification("Variable name already exists. Please choose a different name.", type = "warning")
      print(paste("Warning: Variable name", input$newTransformedVarName, "already exists."))
      return()
    }
    if (input$transformType == "") {
      showNotification("Please select a transformation type.", type = "warning")
      print("Warning: No transformation type selected.")
      return()
    }
    
    original_var_data <- df[[input$varToTransform]]
    print(paste("Class of original_var_data:", class(original_var_data)))
    
    if (!is.numeric(original_var_data)) {
      showNotification("Selected variable is not numeric. Cannot apply transformation.", type = "error")
      print("Error: Selected variable is not numeric.")
      return()
    }
    
    show_modal_spinner(text = "Applying transformation...", color = "#007BFF")
    withProgress(message = 'Transforming Variable', value = 0, {
      incProgress(1/1, detail = "Applying transformation...")
      
      transformed_data <- NULL # Initialize to NULL
      
      tryCatch({
        if (input$transformType == "Log (natural)") {
          print("Applying Log transformation...")
          if (length(original_var_data) > 0 && any(original_var_data <= 0, na.rm = TRUE)) {
            showNotification(message = "Warning: Log transformation applied to non-positive values (<= 0). This will result in -Inf or NaN. Consider filtering data or adding a constant.", type = "warning", duration = 10)
            print("Warning: Log transformation on non-positive values detected.")
          }
          transformed_data <- log(original_var_data)
        } else if (input$transformType == "Square") {
          print("Applying Square transformation...")
          transformed_data <- original_var_data^2
        } else if (input$transformType == "Lag") {
          print("Applying Lag transformation...")
          req(input$lagValue)
          lag_value <- as.integer(input$lagValue)
          if (lag_value < 1) stop("Lag value must be at least 1.")
          
          if (length(original_var_data) <= lag_value) {
            transformed_data <- rep(NA, length(original_var_data))
          } else {
            transformed_data <- c(rep(NA, lag_value), original_var_data[1:(length(original_var_data) - lag_value)])
          }
        } else if (input$transformType == "Interaction") {
          print("Applying Interaction transformation...")
          req(input$varToTransform2)
          if (!(input$varToTransform2 %in% names(df))) {
            stop(paste("Error: Second variable for interaction '", input$varToTransform2, "' not found in data."))
          }
          second_var_data <- df[[input$varToTransform2]]
          if (!is.numeric(second_var_data)) {
            stop("Second variable for interaction is not numeric.")
          }
          transformed_data <- original_var_data * second_var_data
        } else {
          stop("Invalid transformation type selected.")
        }
        print(paste("Transformation successful. Class of transformed_data:", class(transformed_data)))
        print(paste("Length of transformed_data:", length(transformed_data)))
        print(paste("First few values of transformed_data:", paste(head(transformed_data), collapse = ", ")))
        
      }, error = function(e) {
        showNotification(message = paste("Error applying transformation:", e$message), type = "error")
        print(paste("Error in transformation tryCatch:", e$message))
        transformed_data <- NULL # Ensure it's NULL on error
      })
      
      if (!is.null(transformed_data)) {
        # Check if the length of transformed_data matches the number of rows in df
        if (length(transformed_data) != nrow(df)) {
          showNotification(message = "Error: Transformed data length does not match original data rows. Transformation aborted.", type = "error")
          print(paste("Error: Transformed data length (", length(transformed_data), ") does not match original data rows (", nrow(df), ").", sep=""))
          transformed_data <- NULL # Prevent assignment if lengths don't match
        } else {
          df[[input$newTransformedVarName]] <- transformed_data
          print("Attempting to update reactive data()...")
          data(df) # This is where the app state changes
          print("Reactive data() updated successfully.")
          
          # Reverted to showNotification with type="message"
          showNotification(paste("Successfully created new variable:", input$newTransformedVarName), type = "message", duration = 3)
        }
      } else {
        print("Transformed data is NULL, not updating data().")
      }
    })
    remove_modal_spinner()
    print("--- Exiting transformData observer ---")
  })
  
  # Reactive expressions for dynamic UI elements
  output$summaryVarsUI <- renderUI({
    req(allVars())
    pickerInput(
      inputId = "summaryVars",
      label = "Select Variables to Summarize",
      choices = allVars(),
      selected = allVars(),
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$histVarsUI <- renderUI({
    req(numVars())
    pickerInput(
      inputId = "histVars",
      label = "Select Numeric Variables for Histograms",
      choices = numVars(),
      selected = numVars(),
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$densityVarsUI <- renderUI({
    req(numVars())
    pickerInput(
      inputId = "densityVars",
      label = "Select Numeric Variables for Density Plots",
      choices = numVars(),
      selected = numVars(),
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$boxViolinNumVarUI <- renderUI({
    req(numVars())
    selectInput("boxViolinNumVar", "Numeric Variable (Y-axis)", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$boxViolinCatVarUI <- renderUI({
    req(catVars())
    selectInput("boxViolinCatVar", "Categorical Variable (X-axis, for grouping)", choices = c("None", catVars()), selected = "None")
  })
  
  output$corrVarsUI <- renderUI({
    req(numVars())
    pickerInput(
      inputId = "corrVars",
      label = "Select Numeric Variables for Correlation Matrix",
      choices = numVars(),
      selected = numVars(),
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$dotPlotXVarUI <- renderUI({
    req(numVars())
    selectInput("dotPlotXVar", "X-axis Variable", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$dotPlotYVarUI <- renderUI({
    req(numVars())
    selectInput("dotPlotYVar", "Y-axis Variable", choices = numVars(), selected = if(length(numVars()) > 1) numVars()[2] else NULL)
  })
  
  output$pairsPlotVarsUI <- renderUI({
    req(numVars())
    pickerInput(
      inputId = "pairsPlotVars",
      label = "Select Numeric Variables for Pairs Plot",
      choices = numVars(),
      selected = numVars()[1:min(5, length(numVars()))], # Select up to 5 by default
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  # Removed heatmapVarsUI
  # output$heatmapVarsUI <- renderUI({
  #   req(numVars())
  #   pickerInput(
  #     inputId = "heatmapVars",
  #     label = "Select Numeric Variables for Heatmap",
  #     choices = numVars(),
  #     selected = numVars()[1:min(5, length(numVars()))], # Select up to 5 by default
  #     options = list(`actions-box` = TRUE, `live-search` = TRUE),
  #     multiple = TRUE
  #   )
  # })
  
  output$catDistVarUI <- renderUI({
    req(catVars())
    selectInput("catDistVar", "Select Categorical Variable", choices = catVars(), selected = if(length(catVars()) > 0) catVars()[1] else NULL)
  })
  
  output$depVarRegUI <- renderUI({
    req(numVars())
    selectInput("depVarReg", "Dependent Variable", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$indepVarsRegUI <- renderUI({
    req(numVars(), input$depVarReg)
    # Exclude dependent variable from independent variable choices
    indep_choices <- setdiff(numVars(), input$depVarReg)
    pickerInput(
      inputId = "indepVarsReg",
      label = "Independent Variables",
      choices = indep_choices,
      selected = indep_choices[1:min(3, length(indep_choices))],
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$tsVarUI <- renderUI({
    req(numVars())
    selectInput("tsVar", "Time Series Variable", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$tsTimeVarUI <- renderUI({
    req(allVars())
    selectInput("tsTimeVar", "Time Index Variable", choices = allVars(), selected = if(length(allVars()) > 0) allVars()[1] else NULL)
  })
  
  # New UI for ARIMAX
  output$arimaxDepVarUI <- renderUI({
    req(numVars())
    selectInput("arimaxDepVar", "Dependent Time Series Variable", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$arimaxTimeVarUI <- renderUI({
    req(allVars())
    selectInput("arimaxTimeVar", "Time Index Variable", choices = allVars(), selected = if(length(allVars()) > 0) allVars()[1] else NULL)
  })
  
  output$arimaxIndepVarsUI <- renderUI({
    req(numVars(), input$arimaxDepVar)
    # Exclude dependent variable and time variable from independent variable choices
    indep_choices <- setdiff(numVars(), c(input$arimaxDepVar, input$arimaxTimeVar))
    pickerInput(
      inputId = "arimaxIndepVars",
      label = "Independent (Exogenous) Variables",
      choices = indep_choices,
      selected = indep_choices[1:min(3, length(indep_choices))],
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  
  output$panelDepVarUI <- renderUI({
    req(numVars())
    selectInput("panelDepVar", "Dependent Variable", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$panelIndepVarsUI <- renderUI({
    req(numVars(), input$panelDepVar)
    indep_choices <- setdiff(numVars(), input$panelDepVar)
    pickerInput(
      inputId = "panelIndepVars",
      label = "Independent Variables",
      choices = indep_choices,
      selected = indep_choices[1:min(3, length(indep_choices))],
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$panelGroupVarUI <- renderUI({
    req(allVars()) # Grouping variable can be numeric or categorical
    selectInput("panelGroupVar", "Grouping Variable (e.g., ID)", choices = allVars(), selected = if(length(allVars()) > 0) allVars()[1] else NULL)
  })
  
  output$panelTimeVarUI <- renderUI({
    req(allVars()) # Time variable can be numeric or date/character
    selectInput("panelTimeVar", "Time Variable (e.g., Year)", choices = allVars(), selected = if(length(allVars()) > 0) allVars()[1] else NULL)
  })
  
  output$logitDepVarUI <- renderUI({
    req(allVars()) # Dependent variable for logit can be numeric (0/1) or factor/character (TRUE/FALSE, Yes/No)
    selectInput("logitDepVar", "Dependent Variable (Binary)", choices = allVars(), selected = if(length(allVars()) > 0) allVars()[1] else NULL)
  })
  
  output$logitIndepVarsUI <- renderUI({
    req(allVars(), input$logitDepVar)
    indep_choices <- setdiff(allVars(), input$logitDepVar)
    pickerInput(
      inputId = "logitIndepVars",
      label = "Independent Variables",
      choices = indep_choices,
      selected = indep_choices[1:min(3, length(indep_choices))],
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$coxTimeVarUI <- renderUI({
    req(numVars())
    selectInput("coxTimeVar", "Time Variable (Numeric, e.g., follow-up time)", choices = numVars(), selected = if(length(numVars()) > 0) numVars()[1] else NULL)
  })
  
  output$coxEventVarUI <- renderUI({
    req(allVars()) # Event variable can be numeric (0/1) or logical
    selectInput("coxEventVar", "Event Variable (Binary, 0=censored, 1=event)", choices = allVars(), selected = if(length(allVars()) > 0) allVars()[1] else NULL)
  })
  
  output$coxIndepVarsUI <- renderUI({
    req(allVars(), input$coxTimeVar, input$coxEventVar)
    indep_choices <- setdiff(allVars(), c(input$coxTimeVar, input$coxEventVar))
    pickerInput(
      inputId = "coxIndepVars",
      label = "Independent Variables",
      choices = indep_choices,
      selected = indep_choices[1:min(3, length(indep_choices))],
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE
    )
  })
  
  output$timeFilterVarUI <- renderUI({
    req(allVars())
    # Identify potential time/date columns (numeric or character that can be converted to date)
    df <- data()
    time_cols <- names(df)[sapply(df, function(x) {
      is.numeric(x) || inherits(x, "Date") || (is.character(x) && !all(is.na(as.Date(x, format = "%Y-%m-%d", tryCatch=TRUE))))
    })]
    selectInput("timeFilterVar", "Select Time-based Column", choices = time_cols, selected = if(length(time_cols) > 0) time_cols[1] else NULL)
  })
  
  output$testInputsUI <- renderUI({
    req(input$testType)
    df <- data()
    num_vars <- numVars()
    cat_vars <- catVars()
    
    if (input$testType == "One-Sample t-test") {
      tagList(
        selectInput("tTestVar", "Variable", choices = num_vars),
        numericInput("tTestMu", "Hypothesized Mean ($\\\\mu_0$)", value = 0)
      )
    } else if (input$testType == "Two-Sample t-test") {
      tagList(
        selectInput("tTestVar1", "Variable 1", choices = num_vars),
        selectInput("tTestVar2", "Variable 2 (or Grouping Variable)", choices = c(num_vars, cat_vars)),
        conditionalPanel(
          condition = "input.tTestVar2 != null && input.tTestVar2.length > 0 && input.tTestVar2 != input.tTestVar1 && (Shiny.inputBindings.bindingNames.indexOf('selectInput') > -1 ? $('#tTestVar2').find('option:selected').data('type') == 'categorical' : false)", # Check if it's a categorical variable
          checkboxInput("tTestPaired", "Paired t-test?", value = FALSE),
          checkboxInput("tTestVarEqual", "Assume equal variances?", value = TRUE)
        )
      )
    } else if (input$testType == "ANOVA") {
      tagList(
        selectInput("anovaDepVar", "Dependent Variable (Numeric)", choices = num_vars),
        selectInput("anovaIndepVar", "Independent Variable (Categorical)", choices = cat_vars)
      )
    } else if (input$testType == "Chi-squared Test") {
      tagList(
        selectInput("chiSqVar1", "Categorical Variable 1", choices = cat_vars),
        selectInput("chiSqVar2", "Categorical Variable 2", choices = cat_vars)
      )
    }
  })
  
  # Event observers for analysis
  observeEvent(input$runSummary, {
    req(data(), input$summaryVars)
    show_modal_spinner(text = "Calculating summary statistics...", color = "#007BFF")
    output$summary <- renderDT({
      df_summary <- data() %>% select(!!!syms(input$summaryVars))
      summary_stats <- as.data.frame(t(sapply(df_summary, function(x) {
        if (is.numeric(x)) {
          c(Mean = mean(x, na.rm = TRUE),
            SD = sd(x, na.rm = TRUE),
            Min = min(x, na.rm = TRUE),
            Median = median(x, na.rm = TRUE),
            Max = max(x, na.rm = TRUE),
            N = sum(!is.na(x)),
            `NAs` = sum(is.na(x)))
        } else if (is.factor(x) || is.character(x)) {
          tbl <- table(x)
          paste(names(tbl), " (", round(prop.table(tbl)*100, 1), "%)", collapse = "; ", sep="")
        } else {
          NA
        }
      })))
      datatable(summary_stats, options = list(dom = 'Bfrtip', scrollX = TRUE, scrollY = "300px", paging = TRUE))
    })
    remove_modal_spinner()
  })
  
  observeEvent(input$runHistograms, {
    req(data(), input$histVars)
    show_modal_spinner(text = "Generating histograms...", color = "#007BFF")
    plot_output_list <- lapply(input$histVars, function(var) {
      plotname <- paste0("hist_", var)
      output[[plotname]] <- renderPlot({
        ggplot(data(), aes_string(x = var)) +
          geom_histogram(bins = 30, fill = "#007BFF", color = "#E9ECEF", alpha = 0.8) +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
      })
      plotOutput(plotname, height = "400px")
    })
    output$histPlots <- renderUI(plot_output_list)
    remove_modal_spinner()
  })
  
  output$downloadHistograms <- downloadHandler(
    filename = function() {
      paste("histograms_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$histVars)
      pdf(file, width = 10, height = 7)
      for (var in input$histVars) {
        p <- ggplot(data(), aes_string(x = var)) +
          geom_histogram(bins = 30, fill = "#007BFF", color = "#E9ECEF", alpha = 0.8) +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
        print(p)
      }
      dev.off()
    }
  )
  
  observeEvent(input$runDensityPlots, {
    req(data(), input$densityVars)
    show_modal_spinner(text = "Generating density plots...", color = "#007BFF")
    plot_output_list <- lapply(input$densityVars, function(var) {
      plotname <- paste0("density_", var)
      output[[plotname]] <- renderPlot({
        ggplot(data(), aes_string(x = var)) +
          geom_density(fill = "#17A2B8", color = "#E9ECEF", alpha = 0.7, bw = "nrd0") +
          labs(title = paste("Density Plot of", var), x = var, y = "Density") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
      })
      plotOutput(plotname, height = "400px")
    })
    output$densityPlots <- renderUI(plot_output_list)
    remove_modal_spinner()
  })
  
  output$downloadDensityPlots <- downloadHandler(
    filename = function() {
      paste("density_plots_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$densityVars)
      pdf(file, width = 10, height = 7)
      for (var in input$densityVars) {
        p <- ggplot(data(), aes_string(x = var)) +
          geom_density(fill = "#17A2B8", color = "#E9ECEF", alpha = 0.7, bw = "nrd0") +
          labs(title = paste("Density Plot of", var), x = var, y = "Density") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
        print(p)
      }
      dev.off()
    }
  )
  
  observeEvent(input$runBoxViolinPlot, {
    req(data(), input$boxViolinNumVar)
    show_modal_spinner(text = "Generating box/violin plot...", color = "#007BFF")
    
    df <- data()
    num_var_data <- df[[input$boxViolinNumVar]]
    
    if (all(is.na(num_var_data)) || var(num_var_data, na.rm = TRUE) == 0) {
      output$boxViolinPlot <- renderPlot({
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Cannot plot: Selected numeric variable has no variance or all NAs.", col="red", cex=1.2)
      })
      showNotification("Error: Selected numeric variable has no variance or all NAs. Cannot generate plot.", type = "error")
      remove_modal_spinner()
      return()
    }
    
    output$boxViolinPlot <- renderPlot({
      p <- ggplot(df, aes_string(y = input$boxViolinNumVar))
      if (input$boxViolinCatVar != "None" && input$boxViolinCatVar %in% names(df)) {
        p <- p + aes_string(x = input$boxViolinCatVar, fill = input$boxViolinCatVar)
      } else {
        p <- p + aes(x = 1, fill = 1) # Dummy aesthetic for consistent fill
      }
      
      if (input$boxViolinPlotType == "Box Plot") {
        p <- p + geom_boxplot(alpha = 0.7, color = "#212529")
      } else { # Violin Plot
        p <- p + geom_violin(alpha = 0.7, color = "#212529") +
          geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) # Add boxplot inside violin
      }
      
      p <- p + labs(title = paste(input$boxViolinPlotType, "of", input$boxViolinNumVar,
                                  if (input$boxViolinCatVar != "None") paste("by", input$boxViolinCatVar) else ""),
                    x = if (input$boxViolinCatVar != "None") input$boxViolinCatVar else "",
                    y = input$boxViolinNumVar) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.position = if (input$boxViolinCatVar != "None") "right" else "none") +
        scale_fill_viridis_d() # Use a colorblind-friendly palette
      
      print(p)
    })
    remove_modal_spinner()
  })
  
  output$downloadBoxViolinPlot <- downloadHandler(
    filename = function() {
      paste(input$boxViolinPlotType, "_", input$boxViolinNumVar, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$boxViolinNumVar)
      pdf(file, width = 10, height = 7)
      df <- data()
      num_var_data <- df[[input$boxViolinNumVar]]
      
      if (all(is.na(num_var_data)) || var(num_var_data, na.rm = TRUE) == 0) {
        # Do not generate plot, show message instead
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Cannot plot: Selected numeric variable has no variance or all NAs.", col="red", cex=1.2)
        dev.off()
        return()
      }
      
      p <- ggplot(df, aes_string(y = input$boxViolinNumVar))
      if (input$boxViolinCatVar != "None" && input$boxViolinCatVar %in% names(df)) {
        p <- p + aes_string(x = input$boxViolinCatVar, fill = input$boxViolinCatVar)
      } else {
        p <- p + aes(x = 1, fill = 1) # Dummy aesthetic for consistent fill
      }
      
      if (input$boxViolinPlotType == "Box Plot") {
        p <- p + geom_boxplot(alpha = 0.7, color = "#212529")
      } else { # Violin Plot
        p <- p + geom_violin(alpha = 0.7, color = "#212529") +
          geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) # Add boxplot inside violin
      }
      
      p <- p + labs(title = paste(input$boxViolinPlotType, "of", input$boxViolinNumVar,
                                  if (input$boxViolinCatVar != "None") paste("by", input$boxViolinCatVar) else ""),
                    x = if (input$boxViolinCatVar != "None") input$boxViolinCatVar else "",
                    y = input$boxViolinNumVar) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.position = if (input$boxViolinCatVar != "None") "right" else "none") +
        scale_fill_viridis_d()
      print(p)
      dev.off()
    }
  )
  
  observeEvent(input$runCorrelation, {
    req(data(), input$corrVars)
    show_modal_spinner(text = "Calculating correlation matrix...", color = "#007BFF")
    output$corrPlot <- renderPlot({
      df_corr <- data() %>% select(!!!syms(input$corrVars))
      M <- cor(df_corr, use = "pairwise.complete.obs") # Handle NAs gracefully
      corrplot(M, method = "circle", type = "upper", order = "hclust",
               tl.col = "black", tl.srt = 45, addCoef.col = "black",
               col = colorRampPalette(c("#007BFF", "white", "#DC3545"))(200),
               title = "Correlation Matrix", mar = c(0,0,1,0)) # Adjusted title margin
    })
    remove_modal_spinner()
  })
  
  output$downloadCorrPlot <- downloadHandler(
    filename = function() {
      paste("correlation_matrix_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$corrVars)
      pdf(file, width = 10, height = 10)
      df_corr <- data() %>% select(!!!syms(input$corrVars))
      M <- cor(df_corr, use = "pairwise.complete.obs")
      corrplot(M, method = "circle", type = "upper", order = "hclust",
               tl.col = "black", tl.srt = 45, addCoef.col = "black",
               col = colorRampPalette(c("#007BFF", "white", "#DC3545"))(200),
               title = "Correlation Matrix", mar = c(0,0,1,0))
      dev.off()
    }
  )
  
  observeEvent(input$runDotPlot, {
    req(data(), input$dotPlotXVar, input$dotPlotYVar)
    show_modal_spinner(text = "Generating scatter plot...", color = "#007BFF")
    output$singleDotPlot <- renderPlotly({
      p <- ggplot(data(), aes_string(x = input$dotPlotXVar, y = input$dotPlotYVar)) +
        geom_point(color = "#007BFF", alpha = 0.7) +
        geom_smooth(method = "lm", color = "#DC3545", se = FALSE) + # Add linear regression line
        labs(title = paste("Scatter Plot of", input$dotPlotYVar, "vs.", input$dotPlotXVar),
             x = input$dotPlotXVar, y = input$dotPlotYVar) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10))
      ggplotly(p)
    })
    remove_modal_spinner()
  })
  
  observeEvent(input$runPairsPlot, {
    req(data(), input$pairsPlotVars)
    show_modal_spinner(text = "Generating pairs plot...", color = "#007BFF")
    
    df_pairs_original <- data() %>% select(!!!syms(input$pairsPlotVars))
    
    # Filter out variables with no variance or all NAs
    valid_vars <- c()
    for (var_name in input$pairsPlotVars) {
      col_data <- df_pairs_original[[var_name]]
      # Check if numeric and has more than 1 unique non-NA value
      if (is.numeric(col_data) && length(unique(na.omit(col_data))) > 1) {
        valid_vars <- c(valid_vars, var_name)
      } else if (!is.numeric(col_data)) {
        # For non-numeric, just ensure there's more than one unique value (if they were selected)
        if (length(unique(na.omit(col_data))) > 1) {
          valid_vars <- c(valid_vars, var_name)
        }
      }
    }
    
    df_valid_pairs <- df_pairs_original %>% select(!!!syms(valid_vars))
    
    if (ncol(df_valid_pairs) < 2) {
      output$pairsPlot <- renderPlot({
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Pairs Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Cannot generate Pairs Plot: At least two numeric variables with variance are required.", col="red", cex=1.2)
      })
      showNotification("Error: Cannot generate Pairs Plot. Please select at least two numeric variables with more than one unique value.", type = "error", duration = 10)
      remove_modal_spinner()
      return()
    }
    
    output$pairsPlot <- renderPlot({
      pairs(df_valid_pairs, # Use the filtered data frame
            panel = function(x, y, ...) {
              points(x, y, col = "#007BFF", pch = 16, cex = 0.8)
              # Only add smoothers if both x and y have variance
              if (length(unique(na.omit(x))) > 1 && length(unique(na.omit(y))) > 1) {
                tryCatch({
                  abline(lm(y ~ x), col = "#DC3545", lwd = 2)
                  lines(lowess(x, y), col = "#28A745", lwd = 2, lty = 2)
                }, error = function(e) {
                  # Suppress errors from lowess if data is problematic, but still plot points
                  message(paste("Warning: Could not add smoothers to a pair in pairs plot:", e$message))
                })
              }
            },
            diag.panel = function(x, ...) {
              usr <- par("usr"); on.exit(par(usr))
              par(usr = c(usr[1:2], 0, 1.5))
              h <- hist(x, plot = FALSE)
              breaks <- h$breaks; nB <- length(breaks)
              y <- h$counts; y <- y/max(y)
              rect(breaks[-nB], 0, breaks[-1], y, col = "#17A2B8", border = "white")
            },
            upper.panel = function(x, y, ...) {
              usr <- par("usr"); on.exit(par(usr))
              # Only show correlation if both x and y are numeric and have variance
              if (is.numeric(x) && is.numeric(y) && length(unique(na.omit(x))) > 1 && length(unique(na.omit(y))) > 1) {
                text(mean(usr[1:2]), mean(usr[3:4]), round(cor(x, y, use = "pairwise.complete.obs"), 2), cex = 2)
              } else {
                text(mean(usr[1:2]), mean(usr[3:4]), "N/A", cex = 1.5, col = "gray")
              }
            },
            main = "Pairs Plot of Selected Variables",
            cex.main = 1.5, font.main = 2, col.main = "#343A40")
    })
    remove_modal_spinner()
  })
  
  output$downloadPairsPlot <- downloadHandler(
    filename = function() {
      paste("pairs_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$pairsPlotVars)
      pdf(file, width = 10, height = 10)
      
      df_pairs_original <- data() %>% select(!!!syms(input$pairsPlotVars))
      valid_vars <- c()
      for (var_name in input$pairsPlotVars) {
        col_data <- df_pairs_original[[var_name]]
        if (is.numeric(col_data) && length(unique(na.omit(col_data))) > 1) {
          valid_vars <- c(valid_vars, var_name)
        } else if (!is.numeric(col_data)) {
          if (length(unique(na.omit(col_data))) > 1) {
            valid_vars <- c(valid_vars, var_name)
          }
        }
      }
      df_valid_pairs <- df_pairs_original %>% select(!!!syms(valid_vars))
      
      if (ncol(df_valid_pairs) < 2) {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Pairs Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Cannot generate Pairs Plot: At least two numeric variables with variance are required.", col="red", cex=1.2)
        dev.off()
        return()
      }
      
      pairs(df_valid_pairs,
            panel = function(x, y, ...) {
              points(x, y, col = "#007BFF", pch = 16, cex = 0.8)
              if (length(unique(na.omit(x))) > 1 && length(unique(na.omit(y))) > 1) {
                tryCatch({
                  abline(lm(y ~ x), col = "#DC3545", lwd = 2)
                  lines(lowess(x, y), col = "#28A745", lwd = 2, lty = 2)
                }, error = function(e) {
                  message(paste("Warning: Could not add smoothers to a pair in pairs plot (PDF):", e$message))
                })
              }
            },
            diag.panel = function(x, ...) {
              usr <- par("usr"); on.exit(par(usr))
              par(usr = c(usr[1:2], 0, 1.5))
              h <- hist(x, plot = FALSE)
              breaks <- h$breaks; nB <- length(breaks)
              y <- h$counts; y <- y/max(y)
              rect(breaks[-nB], 0, breaks[-1], y, col = "#17A2B8", border = "white")
            },
            upper.panel = function(x, y, ...) {
              usr <- par("usr"); on.exit(par(usr))
              if (is.numeric(x) && is.numeric(y) && length(unique(na.omit(x))) > 1 && length(unique(na.omit(y))) > 1) {
                text(mean(usr[1:2]), mean(usr[3:4]), round(cor(x, y, use = "pairwise.complete.obs"), 2), cex = 2)
              } else {
                text(mean(usr[1:2]), mean(usr[3:4]), "N/A", cex = 1.5, col = "gray")
              }
            },
            main = "Pairs Plot of Selected Variables",
            cex.main = 1.5, font.main = 2, col.main = "#343A40")
      dev.off()
    }
  )
  
  # Removed observeEvent for runHeatmap
  # observeEvent(input$runHeatmap, {
  #   req(data(), input$heatmapVars)
  #   show_modal_spinner(text = "Generating data heatmap...", color = "#007BFF")
  #   output$dataHeatmap <- renderPlot({
  #     df_heatmap <- data() %>% select(!!!syms(input$heatmapVars))
  #     # For heatmap, we usually want a matrix, so convert to matrix and handle NAs
  #     mat <- as.matrix(df_heatmap)
  
  #     plot_generated <- FALSE
  #     tryCatch({
  #       if (any(is.na(mat))) {
  #         mat[is.na(mat)] <- mean(mat, na.rm = TRUE) # Replace NAs with mean for visualization
  #         showNotification("NAs found and replaced with column mean for heatmap visualization.", type = "info", duration = 5)
  #       }
  
  #       # Check if there's enough data for a meaningful heatmap (at least 2x2 matrix)
  #       if (nrow(mat) < 2 || ncol(mat) < 2) {
  #         plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Heatmap Error",
  #              xlab="", ylab="", type="n")
  #         text(0.5, 0.5, "Cannot generate Heatmap: Requires at least 2 rows and 2 columns of data.", col="red", cex=1.2)
  #         showNotification("Error: Heatmap requires at least 2 rows and 2 columns of data.", type = "error")
  #         return(NULL)
  #       }
  
  #       heatmap(mat,
  #               Colv = NA, # Do not reorder columns
  #               Rowv = NA, # Do not reorder rows
  #               scale = "column", # Scale by column
  #               col = colorRampPalette(c("#007BFF", "white", "#DC3545"))(256), # Blue to Red color scale
  #               main = "Heatmap of Selected Variables",
  #               cexCol = 1.2, cexRow = 1.2,
  #               margins = c(8, 8))
  #       plot_generated <- TRUE
  #     }, error = function(e) {
  #       output$dataHeatmap <- renderPlot({
  #         plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Heatmap Error",
  #              xlab="", ylab="", type="n")
  #         text(0.5, 0.5, paste("Error generating Heatmap:", e$message), col="red", cex=1.2)
  #       })
  #       showNotification(paste("Error generating Heatmap:", e$message), type = "error")
  #     })
  #     if (!plot_generated) NULL
  #   })
  #   remove_modal_spinner()
  # })
  
  # Removed downloadHandler for downloadHeatmap
  # output$downloadHeatmap <- downloadHandler(
  #   filename = function() {
  #     paste("data_heatmap_", Sys.Date(), ".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     req(data(), input$heatmapVars)
  #     pdf(file, width = 10, height = 10)
  #     df_heatmap <- data() %>% select(!!!syms(input$heatmapVars))
  #     mat <- as.matrix(df_heatmap)
  
  #     tryCatch({
  #       if (any(is.na(mat))) {
  #         mat[is.na(mat)] <- mean(mat, na.rm = TRUE)
  #       }
  
  #       if (nrow(mat) < 2 || ncol(mat) < 2) {
  #         plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Heatmap Error",
  #              xlab="", ylab="", type="n")
  #         text(0.5, 0.5, "Cannot generate Heatmap: Requires at least 2 rows and 2 columns of data.", col="red", cex=1.2)
  #         dev.off()
  #         return()
  #       }
  
  #       heatmap(mat,
  #               Colv = NA,
  #               Rowv = NA,
  #               scale = "column",
  #               col = colorRampPalette(c("#007BFF", "white", "#DC3545"))(256),
  #               main = "Heatmap of Selected Variables",
  #               cexCol = 1.2, cexRow = 1.2,
  #               margins = c(8, 8))
  #     }, error = function(e) {
  #       plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Heatmap Error",
  #            xlab="", ylab="", type="n")
  #       text(0.5, 0.5, paste("Error generating Heatmap for PDF:", e$message), col="red", cex=1.2)
  #     })
  #     dev.off()
  #   }
  # )
  
  observeEvent(input$runCatDist, {
    req(data(), input$catDistVar)
    show_modal_spinner(text = "Generating categorical distribution plot...", color = "#007BFF")
    output$catDistPlot <- renderPlot({
      df <- data()
      if (!(input$catDistVar %in% names(df))) {
        showNotification("Selected categorical variable not found in data.", type = "error")
        return(NULL)
      }
      
      # Ensure the variable is treated as a factor for correct plotting
      df[[input$catDistVar]] <- as.factor(df[[input$catDistVar]])
      
      if (input$catDistPlotType == "Bar Chart") {
        ggplot(df, aes_string(x = input$catDistVar, fill = input$catDistVar)) +
          geom_bar(alpha = 0.8) +
          labs(title = paste("Bar Chart of", input$catDistVar), x = input$catDistVar, y = "Count") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                axis.title = element_text(size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                legend.position = "none") +
          scale_fill_viridis_d()
      } else { # Pie Chart
        # Calculate counts and percentages
        counts <- as.data.frame(table(df[[input$catDistVar]]))
        names(counts) <- c("Category", "Count")
        counts$Percentage <- counts$Count / sum(counts$Count) * 100
        counts$Label <- paste0(counts$Category, "\n(", round(counts$Percentage, 1), "%)")
        
        ggplot(counts, aes(x = "", y = Percentage, fill = Category)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = 0) +
          labs(title = paste("Pie Chart of", input$catDistVar), fill = "Category") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = "bold")) +
          scale_fill_viridis_d()
      }
    })
    remove_modal_spinner()
  })
  
  output$downloadCatDistPlot <- downloadHandler(
    filename = function() {
      paste(input$catDistPlotType, "_", input$catDistVar, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$catDistVar)
      pdf(file, width = 10, height = 7)
      df <- data()
      df[[input$catDistVar]] <- as.factor(df[[input$catDistVar]]) # Ensure factor
      
      if (input$catDistPlotType == "Bar Chart") {
        p <- ggplot(df, aes_string(x = input$catDistVar, fill = input$catDistVar)) +
          geom_bar(alpha = 0.8) +
          labs(title = paste("Bar Chart of", input$catDistVar), x = input$catDistVar, y = "Count") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                axis.title = element_text(size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                legend.position = "none") +
          scale_fill_viridis_d()
      } else { # Pie Chart
        counts <- as.data.frame(table(df[[input$catDistVar]]))
        names(counts) <- c("Category", "Count")
        counts$Percentage <- counts$Count / sum(counts$Count) * 100
        counts$Label <- paste0(counts$Category, "\n(", round(counts$Percentage, 1), "%)")
        
        p <- ggplot(counts, aes(x = "", y = Percentage, fill = Category)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = 0) +
          labs(title = paste("Pie Chart of", input$catDistVar), fill = "Category") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = "bold")) +
          scale_fill_viridis_d()
      }
      print(p)
      dev.off()
    }
  )
  
  # Linear Regression Diagnostics
  observeEvent(input$runRegression, {
    req(data(), input$depVarReg, input$indepVarsReg)
    show_modal_spinner(text = "Running regression analysis...", color = "#007BFF")
    
    df <- data()
    dep_var <- input$depVarReg
    indep_vars <- input$indepVarsReg
    
    if (!is.numeric(df[[dep_var]])) {
      output$regressionMessage <- renderUI(tags$p(style="color:red;", "Dependent variable must be numeric for Linear Regression."))
      remove_modal_spinner()
      return()
    }
    
    # Check if all independent variables are numeric
    non_numeric_indep <- indep_vars[!sapply(df[indep_vars], is.numeric)]
    if (length(non_numeric_indep) > 0) {
      output$regressionMessage <- renderUI(tags$p(style="color:red;", paste("Independent variables must be numeric for Linear Regression. Non-numeric: ", paste(non_numeric_indep, collapse=", "))))
      remove_modal_spinner()
      return()
    }
    
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = "+"))
    
    tryCatch({
      model <- lm(as.formula(formula_str), data = df)
      
      # Removed the regressionMessage UI output that listed assumptions,
      # as the new layout directly shows the outputs with clear headings.
      output$regressionMessage <- renderUI(NULL) # Clear previous messages
      
      output$regSummary <- renderPrint({
        summary(model)
      })
      
      # Residuals vs Fitted Plot
      output$residualPlot <- renderPlot({
        plot(model, which = 1, caption = "Residuals vs Fitted",
             col = "#007BFF", pch = 16, cex = 1.2,
             main = "Residuals vs Fitted Values",
             sub = "", # Remove default sub-title
             cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
        abline(h = 0, col = "#DC3545", lty = 2, lwd = 2)
      })
      
      # Normal Q-Q Plot
      output$qqPlot <- renderPlot({
        plot(model, which = 2, caption = "Normal Q-Q",
             col = "#007BFF", pch = 16, cex = 1.2,
             main = "Normal Q-Q Plot",
             sub = "", # Remove default sub-title
             cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      })
      
      # Scale-Location Plot
      output$scaleLocationPlot <- renderPlot({
        plot(model, which = 3, caption = "Scale-Location",
             col = "#007BFF", pch = 16, cex = 1.2,
             main = "Scale-Location Plot",
             sub = "",
             cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      })
      
      # Residuals vs Leverage Plot
      output$residualsLeveragePlot <- renderPlot({
        plot(model, which = 5, caption = "Residuals vs Leverage",
             col = "#007BFF", pch = 16, cex = 1.2,
             main = "Residuals vs Leverage Plot",
             sub = "",
             cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      })
      
      # Cook's Distance Plot
      output$cooksDistancePlot <- renderPlot({
        plot(model, which = 4, caption = "Cook's distance", # Often plot 4 or 6 for Cook's
             col = "#007BFF", pch = 16, cex = 1.2,
             main = "Cook's Distance Plot",
             sub = "",
             cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      })
      
      # Residuals vs Predictors Plots
      output$residualsVsPredictorsPlots <- renderUI({
        plot_output_list <- lapply(indep_vars, function(predictor) {
          plotname <- paste0("resid_vs_", predictor)
          output[[plotname]] <- renderPlot({
            p <- ggplot(data = data.frame(Residuals = residuals(model), Predictor = df[[predictor]]),
                        aes(x = Predictor, y = Residuals)) +
              geom_point(color = "#007BFF", alpha = 0.7) +
              geom_smooth(method = "loess", color = "#28A745", se = FALSE) +
              geom_hline(yintercept = 0, linetype = "dashed", color = "#DC3545") +
              labs(title = paste("Residuals vs.", predictor), x = predictor, y = "Residuals") +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10))
            print(p)
          })
          fluidRow(column(12, plotOutput(plotname, height = "400px"))) # Each plot in its own row
        })
        do.call(tagList, plot_output_list)
      })
      
      # VIF
      output$vif <- renderPrint({
        if (length(indep_vars) > 1) {
          vif(model)
        } else {
          "VIF is calculated for models with two or more independent variables."
        }
      })
      
      # Breusch-Pagan Test for Heteroskedasticity
      output$heteroskedasticity <- renderPrint({
        bptest(model)
      })
      
    }, error = function(e) {
      output$regressionMessage <- renderUI(tags$p(style="color:red;", paste("Error running regression:", e$message)))
      output$regSummary <- renderPrint(NULL)
      output$residualPlot <- renderPlot(NULL)
      output$qqPlot <- renderPlot(NULL)
      output$scaleLocationPlot <- renderPlot(NULL)
      output$residualsLeveragePlot <- renderPlot(NULL)
      output$cooksDistancePlot <- renderPlot(NULL)
      output$residualsVsPredictorsPlots <- renderUI(NULL)
      output$vif <- renderPrint(NULL)
      output$heteroskedasticity <- renderPrint(NULL)
    })
    remove_modal_spinner()
  })
  
  output$downloadResidualPlot <- downloadHandler(
    filename = function() { paste("residuals_vs_fitted_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$depVarReg, input$indepVarsReg)
      model <- lm(as.formula(paste(input$depVarReg, "~", paste(input$indepVarsReg, collapse = "+"))), data = data())
      pdf(file, width = 10, height = 7)
      plot(model, which = 1, caption = "Residuals vs Fitted",
           col = "#007BFF", pch = 16, cex = 1.2,
           main = "Residuals vs Fitted Values",
           sub = "",
           cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      abline(h = 0, col = "#DC3545", lty = 2, lwd = 2)
      dev.off()
    }
  )
  
  output$downloadQQPlot <- downloadHandler(
    filename = function() { paste("qq_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$depVarReg, input$indepVarsReg)
      model <- lm(as.formula(paste(input$depVarReg, "~", paste(input$indepVarsReg, collapse = "+"))), data = data())
      pdf(file, width = 10, height = 7)
      plot(model, which = 2, caption = "Normal Q-Q",
           col = "#007BFF", pch = 16, cex = 1.2,
           main = "Normal Q-Q Plot",
           sub = "",
           cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      dev.off()
    }
  )
  
  output$downloadScaleLocationPlot <- downloadHandler(
    filename = function() { paste("scale_location_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$depVarReg, input$indepVarsReg)
      model <- lm(as.formula(paste(input$depVarReg, "~", paste(input$indepVarsReg, collapse = "+"))), data = data())
      pdf(file, width = 10, height = 7)
      plot(model, which = 3, caption = "Scale-Location",
           col = "#007BFF", pch = 16, cex = 1.2,
           main = "Scale-Location Plot",
           sub = "",
           cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      dev.off()
    }
  )
  
  output$downloadResidualsLeveragePlot <- downloadHandler(
    filename = function() { paste("residuals_vs_leverage_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$depVarReg, input$indepVarsReg)
      model <- lm(as.formula(paste(input$depVarReg, "~", paste(input$indepVarsReg, collapse = "+"))), data = data())
      pdf(file, width = 10, height = 7)
      plot(model, which = 5, caption = "Residuals vs Leverage",
           col = "#007BFF", pch = 16, cex = 1.2,
           main = "Residuals vs Leverage Plot",
           sub = "",
           cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      dev.off()
    }
  )
  
  output$downloadCooksDistancePlot <- downloadHandler(
    filename = function() { paste("cooks_distance_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$depVarReg, input$indepVarsReg)
      model <- lm(as.formula(paste(input$depVarReg, "~", paste(input$indepVarsReg, collapse = "+"))), data = data())
      pdf(file, width = 10, height = 7)
      plot(model, which = 4, caption = "Cook's distance",
           col = "#007BFF", pch = 16, cex = 1.2,
           main = "Cook's Distance Plot",
           sub = "",
           cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      dev.off()
    }
  )
  
  output$downloadResidualsVsPredictorsPlots <- downloadHandler(
    filename = function() { paste("residuals_vs_predictors_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$depVarReg, input$indepVarsReg)
      model <- lm(as.formula(paste(input$depVarReg, "~", paste(input$indepVarsReg, collapse = "+"))), data = data())
      pdf(file, width = 10, height = 7)
      for (predictor in input$indepVarsReg) {
        p <- ggplot(data = data.frame(Residuals = residuals(model), Predictor = data()[[predictor]]),
                    aes(x = Predictor, y = Residuals)) +
          geom_point(color = "#007BFF", alpha = 0.7) +
          geom_smooth(method = "loess", color = "#28A745", se = FALSE) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#DC3545") +
          labs(title = paste("Residuals vs.", predictor), x = predictor, y = "Residuals") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
        print(p)
      }
      dev.off()
    }
  )
  
  # Time Series Analysis
  observeEvent(input$runTSAnalysis, {
    req(data(), input$tsVar, input$tsTimeVar, input$tsFrequency)
    show_modal_spinner(text = "Analyzing time series data...", color = "#007BFF")
    
    df <- data()
    ts_var <- input$tsVar
    time_var <- input$tsTimeVar
    ts_freq <- as.numeric(input$tsFrequency)
    
    if (!is.numeric(df[[ts_var]])) {
      output$tsAnalysisMessage <- renderUI(tags$p(style="color:red;", "Time series variable must be numeric."))
      remove_modal_spinner()
      return()
    }
    
    # Attempt to convert time variable to numeric or Date if not already
    time_data <- df[[time_var]]
    if (is.character(time_data)) {
      time_data <- tryCatch(as.Date(time_data), error = function(e) as.numeric(time_data))
      if (inherits(time_data, "Date")) {
        df[[time_var]] <- time_data
      } else if (!is.numeric(time_data)) {
        output$tsAnalysisMessage <- renderUI(tags$p(style="color:red;", "Time index variable could not be converted to numeric or date format."))
        remove_modal_spinner()
        return()
      } else {
        df[[time_var]] <- time_data # Successfully converted to numeric
      }
    } else if (!is.numeric(time_data) && !inherits(time_data, "Date")) {
      output$tsAnalysisMessage <- renderUI(tags$p(style="color:red;", "Time index variable must be numeric or a date format."))
      remove_modal_spinner()
      return()
    }
    
    # Sort data by time variable
    df_ts <- df[order(df[[time_var]]), ]
    
    # Create a simple time series object with user-defined frequency
    ts_data <- ts(df_ts[[ts_var]], start = c(1,1), frequency = ts_freq)
    
    output$tsAnalysisMessage <- renderUI(NULL) # Clear previous messages
    
    # Add a simple ARIMA model to get residuals for autocorrelation check
    # This model is just for diagnostic purposes, not for forecasting
    simple_ts_model <- tryCatch({
      auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE, trace = FALSE, approximation = TRUE)
    }, error = function(e) {
      showNotification(paste("Could not fit a simple ARIMA model for residual diagnostics:", e$message), type = "warning", duration = 5)
      NULL
    })
    
    # Time Series Plot (interactive with plotly)
    output$timeSeriesPlot <- renderPlotly({
      p <- ggplot(df_ts, aes_string(x = time_var, y = ts_var)) +
        geom_line(color = "#007BFF") +
        geom_point(color = "#007BFF", size = 1.5, alpha = 0.7) +
        labs(title = paste("Time Series of", ts_var), x = time_var, y = ts_var) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10))
      ggplotly(p)
    })
    
    # ACF Plot
    output$acfPlot <- renderPlot({
      tryCatch({
        acf(ts_data, main = paste("ACF of", ts_var), col = "#007BFF", lwd = 2)
      }, error = function(e) {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="ACF Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, paste("Error generating ACF:", e$message), col="red", cex=1.2)
      })
    })
    
    # PACF Plot
    output$pacfPlot <- renderPlot({
      tryCatch({
        pacf(ts_data, main = paste("PACF of", ts_var), col = "#DC3545", lwd = 2)
      }, error = function(e) {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="PACF Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, paste("Error generating PACF:", e$message), col="red", cex=1.2)
      })
    })
    
    # Seasonality Plot
    output$seasonalityPlot <- renderPlot({
      if (ts_freq > 1 && length(ts_data) >= 2 * ts_freq) { # Ensure enough data points for seasonality
        tryCatch({
          ggseasonplot(ts_data, main = paste("Seasonal Plot of", ts_var)) +
            labs(x = "Season", y = ts_var) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 10))
        }, error = function(e) {
          plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Seasonality Plot Error",
               xlab="", ylab="", type="n")
          text(0.5, 0.5, paste("Error generating seasonality plot:", e$message), col="red", cex=1.2)
          text(0.5, 0.3, "Ensure adequate data points for seasonal frequency.", col="gray", cex=0.9)
        })
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Seasonality Plot",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Seasonality plot requires a seasonal time series (frequency > 1) and enough data points.", col="darkgray", cex=1.2)
      }
    })
    
    # Autocorrelation of Residuals Plot
    output$residualAcfPlot <- renderPlot({
      if (!is.null(simple_ts_model) && !is.null(residuals(simple_ts_model))) {
        tryCatch({
          checkresiduals(simple_ts_model, plot = TRUE, main = paste("ACF of Residuals from ARIMA Model for", ts_var))
        }, error = function(e) {
          plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Residual ACF Plot Error",
               xlab="", ylab="", type="n")
          text(0.5, 0.5, paste("Error generating residual ACF:", e$message), col="red", cex=1.2)
        })
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Residual ACF Plot",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Residual ACF plot requires a fitted time series model.", col="darkgray", cex=1.2)
      }
    })
    
    # Time Series Decomposition (only if enough data points for frequency)
    output$tsDecompositionPlot <- renderPlot({
      if (ts_freq > 1 && length(ts_data) >= 2 * ts_freq) { # Ensure enough data points for decomposition
        tryCatch({
          decomposed_ts <- decompose(ts_data)
          # Removed the 'main' argument from plot() to avoid duplication error
          plot(decomposed_ts)
          # Add title using mtext if a specific title is desired after plotting
          mtext(paste("Decomposition of", ts_var), side = 3, line = 1, cex = 1.5, font = 2)
        }, error = function(e) {
          plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Decomposition Plot Error",
               xlab="", ylab="", type="n")
          text(0.5, 0.5, paste("Error decomposing series:", e$message), col="red", cex=1.2)
          text(0.5, 0.3, "Ensure adequate data points for chosen frequency.", col="gray", cex=0.9)
        })
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Time Series Decomposition",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Time series decomposition requires a seasonal frequency (>1) and enough data points.", col="darkgray", cex=1.2)
      }
    })
    
    # Lag Plot
    output$lagPlot <- renderPlot({
      tryCatch({
        lag.plot(ts_data, lags = 1, do.lines = FALSE, do.points = TRUE,
                 col = "#007BFF", pch = 16, cex = 1.2,
                 main = paste("Lag Plot of", ts_var, "vs. Lag 1"),
                 cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      }, error = function(e) {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Lag Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, paste("Error generating Lag Plot:", e$message), col="red", cex=1.2)
      })
    })
    
    remove_modal_spinner()
  })
  
  output$downloadSeasonalityPlot <- downloadHandler(
    filename = function() { paste("seasonality_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(data(), input$tsVar, input$tsTimeVar, input$tsFrequency)
      df <- data()
      ts_data <- ts(df[[input$tsVar]], start = c(1,1), frequency = as.numeric(input$tsFrequency))
      pdf(file, width = 10, height = 7)
      if (as.numeric(input$tsFrequency) > 1 && length(ts_data) >= 2 * as.numeric(input$tsFrequency)) {
        p <- ggseasonplot(ts_data, main = paste("Seasonal Plot of", input$tsVar)) +
          labs(x = "Season", y = input$tsVar) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
        print(p)
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Seasonality Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Seasonality plot requires a seasonal time series (frequency > 1) and enough data points.", col="red", cex=1.2)
      }
      dev.off()
    }
  )
  
  output$downloadResidualAcfPlot <- downloadHandler(
    filename = function() { paste("residual_acf_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(data(), input$tsVar, input$tsTimeVar, input$tsFrequency)
      df <- data()
      ts_data <- ts(df[[input$tsVar]], start = c(1,1), frequency = as.numeric(input$tsFrequency))
      simple_ts_model <- tryCatch({
        auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE, trace = FALSE, approximation = TRUE)
      }, error = function(e) { NULL })
      
      pdf(file, width = 10, height = 7)
      if (!is.null(simple_ts_model) && !is.null(residuals(simple_ts_model))) {
        checkresiduals(simple_ts_model, plot = TRUE, main = paste("ACF of Residuals from ARIMA Model for", input$tsVar))
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Residual ACF Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Residual ACF plot requires a fitted time series model.", col="red", cex=1.2)
      }
      dev.off()
    }
  )
  
  output$downloadDecompositionPlot <- downloadHandler(
    filename = function() { paste("ts_decomposition_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(data(), input$tsVar, input$tsTimeVar, input$tsFrequency)
      df <- data()
      ts_data <- ts(df[[input$tsVar]], start = c(1,1), frequency = as.numeric(input$tsFrequency))
      pdf(file, width = 10, height = 7)
      if (as.numeric(input$tsFrequency) > 1 && length(ts_data) >= 2 * as.numeric(input$tsFrequency)) {
        decomposed_ts <- decompose(ts_data)
        # Removed the 'main' argument from plot() to avoid duplication error
        plot(decomposed_ts)
        # Add title using mtext if a specific title is desired after plotting
        mtext(paste("Decomposition of", input$tsVar), side = 3, line = 1, cex = 1.5, font = 2)
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Decomposition Plot Error",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Time series decomposition requires a seasonal frequency (>1) and enough data points.", col="red", cex=1.2)
      }
      dev.off()
    }
  )
  
  output$downloadLagPlot <- downloadHandler(
    filename = function() { paste("lag_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(data(), input$tsVar, input$tsTimeVar, input$tsFrequency)
      df <- data()
      ts_data <- ts(df[[input$tsVar]], start = c(1,1), frequency = as.numeric(input$tsFrequency))
      pdf(file, width = 10, height = 7)
      lag.plot(ts_data, lags = 1, do.lines = FALSE, do.points = TRUE,
               col = "#007BFF", pch = 16, cex = 1.2,
               main = paste("Lag Plot of", input$tsVar, "vs. Lag 1"),
               cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
      dev.off()
    }
  )
  
  # Time Series Forecasting
  observeEvent(input$runForecast, {
    req(data(), input$tsVar, input$tsTimeVar, input$forecastModel, input$forecastHorizon, input$tsFrequency)
    show_modal_spinner(text = "Running time series forecast...", color = "#007BFF")
    
    df <- data()
    ts_var <- input$tsVar
    time_var <- input$tsTimeVar
    horizon <- input$forecastHorizon
    ts_freq <- as.numeric(input$tsFrequency)
    
    # Re-prepare time series data, ensuring it's sorted and converted to ts object
    time_data <- df[[time_var]]
    if (is.character(time_data)) {
      time_data <- tryCatch(as.Date(time_data), error = function(e) as.numeric(time_data))
      if (inherits(time_data, "Date")) {
        df[[time_var]] <- time_data
      } else if (!is.numeric(time_data)) {
        showNotification("Time index variable could not be converted to numeric or date format for forecasting.", type = "error")
        remove_modal_spinner()
        return()
      } else {
        df[[time_var]] <- time_data
      }
    } else if (!is.numeric(time_data) && !inherits(time_data, "Date")) {
      showNotification("Time index variable must be numeric or a date format for forecasting.", type = "error")
      remove_modal_spinner()
      return()
    }
    df_ts_sorted <- df[order(df[[time_var]]), ]
    ts_data <- ts(df_ts_sorted[[ts_var]], start = c(1,1), frequency = ts_freq)
    
    forecast_result <- tryCatch({
      if (input$forecastModel == "ARIMA") {
        model <- auto.arima(ts_data)
        forecast(model, h = horizon)
      } else if (input$forecastModel == "ETS") {
        model <- ets(ts_data)
        forecast(model, h = horizon)
      }
    }, error = function(e) {
      showNotification(paste("Error during forecasting:", e$message), type = "error")
      NULL
    })
    
    if (!is.null(forecast_result)) {
      # Plot forecast
      output$forecastPlot <- renderPlotly({
        autoplot(forecast_result, main = paste(input$forecastModel, "Forecast for", ts_var)) +
          labs(x = "Time", y = ts_var) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10))
      })
      
      # Forecast values table
      forecast_df <- as.data.frame(forecast_result)
      output$forecastTable <- renderDT({
        datatable(forecast_df, options = list(dom = 'Bfrtip', scrollX = TRUE, scrollY = "200px", paging = FALSE))
      })
      
      # Accuracy metrics
      output$forecastAccuracy <- renderPrint({
        accuracy(forecast_result)
      })
    } else {
      output$forecastPlot <- renderPlotly(NULL)
      output$forecastTable <- renderDT(NULL)
      output$forecastAccuracy <- renderPrint(NULL)
    }
    remove_modal_spinner()
  })
  
  # ARIMAX Regression
  observeEvent(input$runARIMAX, {
    req(data(), input$arimaxDepVar, input$arimaxTimeVar, input$arimaxIndepVars, input$arimaxFrequency)
    show_modal_spinner(text = "Running ARIMAX model...", color = "#007BFF")
    
    df <- data()
    dep_var <- input$arimaxDepVar
    time_var <- input$arimaxTimeVar
    exog_vars <- input$arimaxIndepVars
    arimax_freq <- as.numeric(input$arimaxFrequency)
    
    if (!is.numeric(df[[dep_var]])) {
      output$arimaxMessage <- renderUI(tags$p(style="color:red;", "Dependent variable must be numeric for ARIMAX Regression."))
      remove_modal_spinner()
      return()
    }
    if (length(exog_vars) == 0) {
      output$arimaxMessage <- renderUI(tags$p(style="color:red;", "Please select at least one independent (exogenous) variable for ARIMAX Regression."))
      remove_modal_spinner()
      return()
    }
    non_numeric_exog <- exog_vars[!sapply(df[exog_vars], is.numeric)]
    if (length(non_numeric_exog) > 0) {
      output$arimaxMessage <- renderUI(tags$p(style="color:red;", paste("Independent (exogenous) variables must be numeric. Non-numeric: ", paste(non_numeric_exog, collapse=", "))))
      remove_modal_spinner()
      return()
    }
    
    # Prepare time series data
    time_data <- df[[time_var]]
    if (is.character(time_data)) {
      time_data <- tryCatch(as.Date(time_data), error = function(e) as.numeric(time_data))
      if (inherits(time_data, "Date")) {
        df[[time_var]] <- time_data
      } else if (!is.numeric(time_data)) {
        output$arimaxMessage <- renderUI(tags$p(style="color:red;", "Time index variable could not be converted to numeric or date format."))
        remove_modal_spinner()
        return()
      } else {
        df[[time_var]] <- time_data
      }
    } else if (!is.numeric(time_data) && !inherits(time_data, "Date")) {
      output$arimaxMessage <- renderUI(tags$p(style="color:red;", "Time index variable must be numeric or a date format."))
      remove_modal_spinner()
      return()
    }
    
    df_arimax_sorted <- df[order(df[[time_var]]), ]
    y_ts <- ts(df_arimax_sorted[[dep_var]], start = c(1,1), frequency = arimax_freq)
    x_reg <- as.matrix(df_arimax_sorted[, exog_vars, drop = FALSE])
    
    # Check for NA values in y_ts or x_reg
    if (any(is.na(y_ts)) || any(is.na(x_reg))) {
      output$arimaxMessage <- renderUI(tags$p(style="color:red;", "ARIMAX model cannot handle NA values. Please clean your data first."))
      remove_modal_spinner()
      return()
    }
    
    arimax_model <- tryCatch({
      auto.arima(y_ts, xreg = x_reg, seasonal = TRUE, stepwise = TRUE, trace = FALSE, approximation = TRUE)
    }, error = function(e) {
      output$arimaxMessage <- renderUI(tags$p(style="color:red;", paste("Error fitting ARIMAX model:", e$message)))
      NULL
    })
    
    if (!is.null(arimax_model)) {
      output$arimaxMessage <- renderUI(NULL)
      output$arimaxSummary <- renderPrint({
        summary(arimax_model)
      })
      output$arimaxResidualsPlot <- renderPlot({
        checkresiduals(arimax_model, plot = TRUE, main = paste("ARIMAX Residuals for", dep_var))
      })
    } else {
      output$arimaxSummary <- renderPrint(NULL)
      output$arimaxResidualsPlot <- renderPlot(NULL)
    }
    remove_modal_spinner()
  })
  
  output$downloadARIMAXResidualsPlot <- downloadHandler(
    filename = function() { paste("arimax_residuals_plot_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(data(), input$arimaxDepVar, input$arimaxTimeVar, input$arimaxIndepVars, input$arimaxFrequency)
      df <- data()
      time_data <- df[[input$arimaxTimeVar]]
      if (is.character(time_data)) {
        time_data <- tryCatch(as.Date(time_data), error = function(e) as.numeric(time_data))
        if (inherits(time_data, "Date")) {
          df[[input$arimaxTimeVar]] <- time_data
        } else if (!is.numeric(time_data)) {
          stop("Time index variable could not be converted to numeric or date format.")
        } else {
          df[[input$arimaxTimeVar]] <- time_data
        }
      } else if (!is.numeric(time_data) && !inherits(time_data, "Date")) {
        stop("Time index variable must be numeric or a date format.")
      }
      df_arimax_sorted <- df[order(df[[input$arimaxTimeVar]]), ]
      y_ts <- ts(df_arimax_sorted[[input$arimaxDepVar]], start = c(1,1), frequency = as.numeric(input$arimaxFrequency))
      x_reg <- as.matrix(df_arimax_sorted[, input$arimaxIndepVars, drop = FALSE])
      
      if (any(is.na(y_ts)) || any(is.na(x_reg))) {
        stop("ARIMAX model cannot handle NA values. Please clean your data first.")
      }
      
      arimax_model <- auto.arima(y_ts, xreg = x_reg, seasonal = TRUE, stepwise = TRUE, trace = FALSE, approximation = TRUE)
      
      pdf(file, width = 10, height = 7)
      checkresiduals(arimax_model, plot = TRUE, main = paste("ARIMAX Residuals for", input$arimaxDepVar))
      dev.off()
    }
  )
  
  
  # Panel Data Models
  observeEvent(input$runPanelAnalysis, {
    req(data(), input$panelDepVar, input$panelIndepVars, input$panelGroupVar, input$panelTimeVar)
    show_modal_spinner(text = "Running panel data analysis...", color = "#007BFF")
    
    df <- data()
    dep_var <- input$panelDepVar
    indep_vars <- input$panelIndepVars
    group_var <- input$panelGroupVar
    time_var <- input$panelTimeVar
    
    # Check variable types
    if (!is.numeric(df[[dep_var]])) {
      output$panelAnalysisMessage <- renderUI(tags$p(style="color:red;", "Dependent variable must be numeric for Panel Data Models."))
      remove_modal_spinner()
      return()
    }
    non_numeric_indep <- indep_vars[!sapply(df[indep_vars], is.numeric)]
    if (length(non_numeric_indep) > 0) {
      output$panelAnalysisMessage <- renderUI(tags$p(style="color:red;", paste("Independent variables must be numeric for Panel Data Models. Non-numeric: ", paste(non_numeric_indep, collapse=", "))))
      remove_modal_spinner()
      return()
    }
    
    # Convert to panel data frame
    pdata <- pdata.frame(df, index = c(group_var, time_var))
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = "+"))
    
    output$panelAnalysisMessage <- renderUI({
      tagList(
        h5("Pooled OLS, Fixed Effects, and Random Effects Model Summaries"),
        h5("Hausman Test: Fixed vs. Random Effects"),
        h5("F-Test: Pooled OLS vs. Fixed Effects")
      )
    })
    
    # Pooled OLS
    tryCatch({
      pooled_ols_model <- plm(as.formula(formula_str), data = pdata, model = "pooling")
      output$pooledOLS_summary <- renderPrint({
        summary(pooled_ols_model)
      })
    }, error = function(e) {
      output$pooledOLS_summary <- renderPrint(paste("Error running Pooled OLS:", e$message))
    })
    
    # Fixed Effects Model
    tryCatch({
      fixed_effects_model <- plm(as.formula(formula_str), data = pdata, model = "within")
      output$fixedEffects_summary <- renderPrint({
        summary(fixed_effects_model)
      })
    }, error = function(e) {
      output$fixedEffects_summary <- renderPrint(paste("Error running Fixed Effects Model:", e$message))
    })
    
    # Random Effects Model
    tryCatch({
      random_effects_model <- plm(as.formula(formula_str), data = pdata, model = "random")
      output$randomEffects_summary <- renderPrint({
        summary(random_effects_model)
      })
    }, error = function(e) {
      output$randomEffects_summary <- renderPrint(paste("Error running Random Effects Model:", e$message))
    })
    
    # Hausman Test
    tryCatch({
      if (exists("fixed_effects_model") && exists("random_effects_model")) {
        hausman_test_result <- phtest(fixed_effects_model, random_effects_model)
        output$hausmanTest <- renderPrint({
          hausman_test_result
        })
      } else {
        output$hausmanTest <- renderPrint("Hausman test requires both Fixed and Random Effects models to run successfully.")
      }
    }, error = function(e) {
      output$hausmanTest <- renderPrint(paste("Error running Hausman Test:", e$message))
    })
    
    # F-Test (Pooled OLS vs. Fixed Effects)
    tryCatch({
      if (exists("pooled_ols_model") && exists("fixed_effects_model")) {
        f_test_result <- pFtest(fixed_effects_model, pooled_ols_model)
        output$fTestPooledVsFE <- renderPrint({
          f_test_result
        })
      } else {
        output$fTestPooledVsFE <- renderPrint("F-test requires both Pooled OLS and Fixed Effects models to run successfully.")
      }
    }, error = function(e) {
      output$fTestPooledVsFE <- renderPrint(paste("Error running F-Test (Pooled OLS vs FE):", e$message))
    })
    
    remove_modal_spinner()
  })
  
  # Logit Regression
  observeEvent(input$runLogitRegression, {
    req(data(), input$logitDepVar, input$logitIndepVars)
    show_modal_spinner(text = "Running Logit regression...", color = "#007BFF")
    
    df <- data()
    dep_var <- input$logitDepVar
    indep_vars <- input$logitIndepVars
    
    # Ensure dependent variable is binary (0/1 or TRUE/FALSE)
    if (!is.numeric(df[[dep_var]]) && !is.logical(df[[dep_var]])) {
      output$logitRegressionMessage <- renderUI(tags$p(style="color:red;", "Dependent variable must be binary (numeric 0/1 or logical TRUE/FALSE) for Logit Regression."))
      remove_modal_spinner()
      return()
    }
    # Convert to factor with 2 levels if not already
    if (is.numeric(df[[dep_var]]) && !all(unique(df[[dep_var]]) %in% c(0, 1, NA))) {
      output$logitRegressionMessage <- renderUI(tags$p(style="color:red;", "Numeric dependent variable for Logit must contain only 0s and 1s."))
      remove_modal_spinner()
      return()
    }
    df[[dep_var]] <- as.factor(df[[dep_var]])
    if (nlevels(df[[dep_var]]) != 2) {
      output$logitRegressionMessage <- renderUI(tags$p(style="color:red;", "Dependent variable must have exactly two unique levels for Logit Regression."))
      remove_modal_spinner()
      return()
    }
    
    formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = "+"))
    
    tryCatch({
      logit_model <- glm(as.formula(formula_str), data = df, family = "binomial")
      output$logitRegressionMessage <- renderUI(h5("Logit Regression Model Summary and Odds Ratios"))
      output$logitSummary <- renderPrint({
        summary(logit_model)
      })
      output$logitOddsRatios <- renderPrint({
        exp(coef(logit_model)) # Odds Ratios
      })
    }, error = function(e) {
      output$logitRegressionMessage <- renderUI(tags$p(style="color:red;", paste("Error running Logit Regression:", e$message)))
      output$logitSummary <- renderPrint(NULL)
      output$logitOddsRatios <- renderPrint(NULL)
    })
    remove_modal_spinner()
  })
  
  # Cox Regression
  observeEvent(input$runCoxRegression, {
    req(data(), input$coxTimeVar, input$coxEventVar, input$coxIndepVars)
    show_modal_spinner(text = "Running Cox regression...", color = "#007BFF")
    
    df <- data()
    time_var <- input$coxTimeVar
    event_var <- input$coxEventVar
    indep_vars <- input$coxIndepVars
    
    # Check variable types
    if (!is.numeric(df[[time_var]])) {
      output$coxRegressionMessage <- renderUI(tags$p(style="color:red;", "Time variable must be numeric for Cox Regression."))
      remove_modal_spinner()
      return()
    }
    if (!is.numeric(df[[event_var]]) && !is.logical(df[[event_var]])) {
      output$coxRegressionMessage <- renderUI(tags$p(style="color:red;", "Event variable must be binary (numeric 0/1 or logical TRUE/FALSE) for Cox Regression."))
      remove_modal_spinner()
      return()
    }
    if (is.numeric(df[[event_var]]) && !all(unique(df[[event_var]]) %in% c(0, 1, NA))) {
      output$coxRegressionMessage <- renderUI(tags$p(style="color:red;", "Numeric event variable for Cox Regression must contain only 0s and 1s."))
      remove_modal_spinner()
      return()
    }
    
    # Ensure event variable is 0/1 numeric for Surv function
    df[[event_var]] <- as.numeric(as.logical(df[[event_var]]))
    
    formula_str <- paste("Surv(", time_var, ",", event_var, ") ~ ", paste(indep_vars, collapse = "+"))
    
    tryCatch({
      cox_model <- coxph(as.formula(formula_str), data = df)
      output$coxRegressionMessage <- renderUI(h5("Cox Proportional Hazards Model Summary and Hazard Ratios"))
      output$coxSummary <- renderPrint({
        summary(cox_model)
      })
      output$coxHazardRatios <- renderPrint({
        exp(coef(cox_model)) # Hazard Ratios
      })
    }, error = function(e) {
      output$coxRegressionMessage <- renderUI(tags$p(style="color:red;", paste("Error running Cox Regression:", e$message)))
      output$coxSummary <- renderPrint(NULL)
      output$coxHazardRatios <- renderPrint(NULL)
    })
    remove_modal_spinner()
  })
  
  # Hypothesis Tests
  observeEvent(input$runTest, {
    req(data(), input$testType)
    show_modal_spinner(text = paste("Running", input$testType, "..."), color = "#007BFF")
    df <- data()
    test_results <- NULL
    test_plot <- NULL
    error_message <- NULL
    
    tryCatch({
      if (input$testType == "One-Sample t-test") {
        req(input$tTestVar, input$tTestMu)
        if (!is.numeric(df[[input$tTestVar]])) stop("Variable must be numeric for t-test.")
        test_results <- t.test(df[[input$tTestVar]], mu = input$tTestMu)
        test_plot <- ggplot(df, aes_string(x = input$tTestVar)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "#007BFF", alpha = 0.7) +
          geom_density(color = "#DC3545", size = 1) +
          geom_vline(xintercept = input$tTestMu, linetype = "dashed", color = "#28A745", size = 1) +
          labs(title = paste("Distribution of", input$tTestVar, "with Hypothesized Mean"),
               x = input$tTestVar, y = "Density") +
          theme_minimal()
      } else if (input$testType == "Two-Sample t-test") {
        req(input$tTestVar1, input$tTestVar2)
        if (!is.numeric(df[[input$tTestVar1]])) stop("Variable 1 must be numeric for t-test.")
        
        if (input$tTestVar2 %in% numVars()) { # Two numeric variables
          if (!is.numeric(df[[input$tTestVar2]])) stop("Variable 2 must be numeric for a two-variable t-test.")
          test_results <- t.test(df[[input$tTestVar1]], df[[input$tTestVar2]],
                                 paired = input$tTestPaired, var.equal = input$tTestVarEqual)
          test_plot <- ggplot(df, aes_string(x = input$tTestVar1, y = input$tTestVar2)) +
            geom_point(color = "#007BFF", alpha = 0.7) +
            labs(title = paste("Scatter Plot of", input$tTestVar1, "vs.", input$tTestVar2),
                 x = input$tTestVar1, y = input$tTestVar2) +
            theme_minimal()
        } else if (input$tTestVar2 %in% catVars()) { # Numeric variable by categorical group
          if (nlevels(as.factor(df[[input$tTestVar2]])) != 2) stop("Grouping variable must have exactly two levels for two-sample t-test.")
          test_results <- t.test(as.formula(paste(input$tTestVar1, "~", input$tTestVar2)),
                                 data = df, var.equal = input$tTestVarEqual)
          test_plot <- ggplot(df, aes_string(x = input$tTestVar2, y = input$tTestVar1, fill = input$tTestVar2)) +
            geom_boxplot(alpha = 0.7) +
            labs(title = paste("Box Plot of", input$tTestVar1, "by", input$tTestVar2),
                 x = input$tTestVar2, y = input$tTestVar1) +
            theme_minimal() + scale_fill_viridis_d()
        } else {
          stop("Invalid selection for Variable 2/Grouping Variable.")
        }
      } else if (input$testType == "ANOVA") {
        req(input$anovaDepVar, input$anovaIndepVar)
        if (!is.numeric(df[[input$anovaDepVar]])) stop("Dependent variable must be numeric for ANOVA.")
        df[[input$anovaIndepVar]] <- as.factor(df[[input$anovaIndepVar]])
        if (nlevels(df[[input$anovaIndepVar]]) < 2) stop("Independent variable must have at least two levels for ANOVA.")
        
        aov_model <- aov(as.formula(paste(input$anovaDepVar, "~", input$anovaIndepVar)), data = df)
        test_results <- summary(aov_model)
        test_plot <- ggplot(df, aes_string(x = input$anovaIndepVar, y = input$anovaDepVar, fill = input$anovaIndepVar)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Box Plot of", input$anovaDepVar, "by", input$anovaIndepVar),
               x = input$anovaIndepVar, y = input$anovaDepVar) +
          theme_minimal() + scale_fill_viridis_d()
      } else if (input$testType == "Chi-squared Test") {
        req(input$chiSqVar1, input$chiSqVar2)
        if (!(input$chiSqVar1 %in% catVars()) || !(input$chiSqVar2 %in% catVars())) stop("Both variables must be categorical for Chi-squared test.")
        
        contingency_table <- table(df[[input$chiSqVar1]], df[[input$chiSqVar2]])
        test_results <- chisq.test(contingency_table)
        
        # Mosaic plot for Chi-squared
        test_plot <- ggplot(as.data.frame(contingency_table), aes(x = Var1, y = Freq, fill = Var2)) +
          geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
          labs(title = paste("Proportional Bar Chart of", input$chiSqVar1, "by", input$chiSqVar2),
               x = input$chiSqVar1, y = "Proportion", fill = input$chiSqVar2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = element_text(size = 10)) +
          scale_fill_viridis_d()
      }
    }, error = function(e) {
      error_message <- paste("Error running test:", e$message)
      test_results <- NULL
      test_plot <- NULL
    })
    
    output$testResults <- renderPrint({
      if (!is.null(error_message)) {
        cat(error_message)
      } else if (!is.null(test_results)) {
        test_results
      } else {
        "No results to display."
      }
    })
    
    output$testPlot <- renderPlot({
      if (!is.null(test_plot)) {
        print(test_plot)
      } else {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main="No Plot Available",
             xlab="", ylab="", type="n")
        text(0.5, 0.5, "Plot could not be generated.", col="darkgray", cex=1.2)
      }
    })
    remove_modal_spinner()
  })
  
  output$downloadTestPlot <- downloadHandler(
    filename = function() {
      paste(input$testType, "_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(data(), input$testType)
      pdf(file, width = 10, height = 7)
      df <- data()
      
      if (input$testType == "One-Sample t-test") {
        req(input$tTestVar, input$tTestMu)
        p <- ggplot(df, aes_string(x = input$tTestVar)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "#007BFF", alpha = 0.7) +
          geom_density(color = "#DC3545", size = 1) +
          geom_vline(xintercept = input$tTestMu, linetype = "dashed", color = "#28A745", size = 1) +
          labs(title = paste("Distribution of", input$tTestVar, "with Hypothesized Mean"),
               x = input$tTestVar, y = "Density") +
          theme_minimal()
        print(p)
      } else if (input$testType == "Two-Sample t-test") {
        req(input$tTestVar1, input$tTestVar2)
        if (input$tTestVar2 %in% numVars()) {
          p <- ggplot(df, aes_string(x = input$tTestVar1, y = input$tTestVar2)) +
            geom_point(color = "#007BFF", alpha = 0.7) +
            labs(title = paste("Scatter Plot of", input$tTestVar1, "vs.", input$tTestVar2),
                 x = input$tTestVar1, y = input$tTestVar2) +
            theme_minimal()
          print(p)
        } else if (input$tTestVar2 %in% catVars()) {
          p <- ggplot(df, aes_string(x = input$tTestVar2, y = input$tTestVar1, fill = input$tTestVar2)) +
            geom_boxplot(alpha = 0.7) +
            labs(title = paste("Box Plot of", input$tTestVar1, "by", input$tTestVar2),
                 x = input$tTestVar2, y = input$tTestVar1) +
            theme_minimal() + scale_fill_viridis_d()
          print(p)
        }
      } else if (input$testType == "ANOVA") {
        req(input$anovaDepVar, input$anovaIndepVar)
        df[[input$anovaIndepVar]] <- as.factor(df[[input$anovaIndepVar]])
        p <- ggplot(df, aes_string(x = input$anovaIndepVar, y = input$anovaDepVar, fill = input$anovaIndepVar)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Box Plot of", input$anovaDepVar, "by", input$anovaIndepVar),
               x = input$anovaIndepVar, y = input$anovaDepVar) +
          theme_minimal() + scale_fill_viridis_d()
        print(p)
      } else if (input$testType == "Chi-squared Test") {
        req(input$chiSqVar1, input$chiSqVar2)
        contingency_table <- table(df[[input$chiSqVar1]], df[[input$chiSqVar2]])
        p <- ggplot(as.data.frame(contingency_table), aes(x = Var1, y = Freq, fill = Var2)) +
          geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
          labs(title = paste("Proportional Bar Chart of", input$chiSqVar1, "by", input$chiSqVar2),
               x = input$chiSqVar1, y = "Proportion", fill = input$chiSqVar2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = element_text(size = 10)) +
          scale_fill_viridis_d()
        print(p)
      }
      dev.off()
    }
  )
  
  # Time Filter
  output$timeFilterUI <- renderUI({
    req(data(), input$timeFilterVar)
    df <- data()
    time_col <- df[[input$timeFilterVar]]
    
    if (inherits(time_col, "Date")) {
      dateRangeInput("dateRangeFilter", "Filter by Date Range:",
                     start = min(time_col, na.rm = TRUE),
                     end = max(time_col, na.rm = TRUE),
                     min = min(time_col, na.rm = TRUE),
                     max = max(time_col, na.rm = TRUE))
    } else if (is.numeric(time_col)) {
      sliderInput("numericRangeFilter", paste("Filter by", input$timeFilterVar, "Range:"),
                  min = min(time_col, na.rm = TRUE),
                  max = max(time_col, na.rm = TRUE),
                  value = c(min(time_col, na.rm = TRUE), max(time_col, na.rm = TRUE)))
    } else {
      tags$p("Selected column is not a recognized date or numeric type for filtering.")
    }
  })
  
  observe({
    req(data(), input$timeFilterVar)
    df <- data()
    time_col_name <- input$timeFilterVar
    filtered_df <- df
    
    if (inherits(df[[time_col_name]], "Date") && !is.null(input$dateRangeFilter)) {
      filtered_df <- df[df[[time_col_name]] >= input$dateRangeFilter[1] & df[[time_col_name]] <= input$dateRangeFilter[2], ]
    } else if (is.numeric(df[[time_col_name]]) && !is.null(input$numericRangeFilter)) {
      filtered_df <- df[df[[time_col_name]] >= input$numericRangeFilter[1] & df[[time_col_name]] <= input$numericRangeFilter[2], ]
    }
    
    output$filteredTable <- renderDT({
      datatable(filtered_df, options = list(dom = 'Bfrtip', scrollX = TRUE, scrollY = "300px", paging = TRUE))
    })
  })
  
  # Report Generation
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Modulus_Analysis_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      show_modal_spinner(text = "Crafting your comprehensive analysis report...", color = "#007BFF")
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE) # Ensure report.Rmd is in the same directory as app.R or provide full path
      
      # Set up parameters to pass to Rmd document
      params <- list(
        data = data(),
        summary_vars = input$summaryVars,
        hist_vars = input$histVars,
        density_vars = input$densityVars,
        box_violin_num_var = input$boxViolinNumVar,
        box_violin_cat_var = input$boxViolinCatVar,
        box_violin_plot_type = input$boxViolinPlotType,
        corr_vars = input$corrVars,
        dot_plot_x_var = input$dotPlotXVar,
        dot_plot_y_var = input$dotPlotYVar,
        pairs_plot_vars = input$pairsPlotVars,
        # heatmap_vars = input$heatmapVars, # Removed
        cat_dist_var = input$catDistVar,
        cat_dist_plot_type = input$catDistPlotType,
        # Regression params
        dep_var_reg = input$depVarReg,
        indep_vars_reg = input$indepVarsReg,
        # Time Series params
        ts_var = input$tsVar,
        ts_time_var = input$tsTimeVar,
        ts_frequency = input$tsFrequency, # Pass frequency to report
        forecast_model = input$forecastModel,
        forecast_horizon = input$forecastHorizon,
        # ARIMAX params
        arimax_dep_var = input$arimaxDepVar,
        arimax_time_var = input$arimaxTimeVar,
        arimax_indep_vars = input$arimaxIndepVars,
        arimax_frequency = input$arimaxFrequency,
        # Panel Data params
        panel_dep_var = input$panelDepVar,
        panel_indep_vars = input$panelIndepVars,
        panel_group_var = input$panelGroupVar,
        panel_time_var = input$panelTimeVar,
        # Logit params
        logit_dep_var = input$logitDepVar,
        logit_indep_vars = input$logitIndepVars,
        # Cox params
        cox_time_var = input$coxTimeVar,
        cox_event_var = input$coxEventVar,
        cox_indep_vars = input$coxIndepVars,
        # Test params
        test_type = input$testType,
        t_test_var = input$tTestVar,
        t_test_mu = input$tTestMu,
        t_test_var1 = input$tTestVar1,
        t_test_var2 = input$tTestVar2,
        t_test_paired = input$tTestPaired,
        t_test_var_equal = input$tTestVarEqual,
        anova_dep_var = input$anovaDepVar,
        anova_indep_var = input$anovaIndepVar,
        chi_sq_var1 = input$chiSqVar1,
        chi_sq_var2 = input$chiSqVar2
      )
      
      # Render the Rmd file to PDF
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      remove_modal_spinner()
    }
  )
}

shinyApp(ui = ui, server = server)
