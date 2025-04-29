# app.R

#Note: This shiny dashboard was made using the help from OpenAI (ChatGPT)

# ───────────────────────────────────────────────────────────────────────
# Loading Libraries
# ───────────────────────────────────────────────────────────────────────

# Shiny App Framework
library(shiny)             # Building interactive web applications
library(shinydashboard)    # Dashboard layouts for Shiny
library(rsconnect)         # For publishing Shiny apps

# Data Import and Cleaning
library(readr)             # Reading CSV and RDS files
library(janitor)           # Cleaning column names, empty rows/columns
library(naniar)            # Handling missing data
library(dplyr)             # Data manipulation (filter, mutate, summarize)
library(tidyr)             # Data tidying (pivoting, reshaping)
library(lubridate)         # Date/time manipulation
library(tidyverse)         # Collection of packages (includes ggplot2, dplyr, etc.)

# Visualization
library(ggplot2)           # Core plotting
library(ggthemes)          # Extra themes for ggplot2
library(ggpubr)            # Publication-ready plots
library(cowplot)           # Combine multiple ggplots
library(corrplot)          # Correlation matrix visualization
library(GGally)            # Pairwise plots (ggpairs)
library(plotly)            # Interactive plots
library(circlize)          # Circular plots
library(PerformanceAnalytics)  # Financial and correlation plots
library(markdown)          # Rendering markdown text

# Machine Learning Framework
library(tidymodels)        # Unified framework for modeling (parsnip, recipes, etc.)
library(finetune)          # Advanced hyperparameter tuning (racing, ANOVA)
library(tune)              # Hyperparameter tuning (grid/random search)
library(rsample)           # Data splitting and resampling
library(embed)             # For step_lencode_glm and categorical embeddings
library(recipes)           # Preprocessing steps for data
library(textrecipes)       # Preprocessing text data
library(hardhat)           # Infrastructure for modeling packages
library(dials)             # Define tuning parameters (e.g., hidden_units, penalty)

# Modeling Engines
library(xgboost)           # XGBoost gradient boosting
library(ranger)            # Fast Random Forests
library(nnet)              # Neural Networks
library(caret)             # Traditional machine learning workflows

# Metrics and Model Evaluation
library(yardstick)         # Evaluation metrics (RMSE, MAE, R²)
library(vip)               # Variable importance plots
library(pROC)              # ROC and AUC curves for classification
library(car)               # VIFs and diagnostics for linear models

# Parallel Processing
library(doParallel)        # For parallel processing
library(future)            # Future package for multicore/distributed processing

# Data Exploration
library(skimr)             # Quick summary of datasets
library(DataExplorer)      # Automated EDA reports



# Show full error traces in console
options(shiny.sanitize.errors = FALSE)

# ───────────────────────────────────────────────────────────────────────────────
# Setup
# ───────────────────────────────────────────────────────────────────────────────

merged = read_csv("merged_factored.csv")
train_data = read_csv("train_data.csv")
test_data = read_csv("test_data.csv")
fitted_xgb_wf = read_rds("xgb_full_data_workflow.rds")

soil_vars = c("soilpH","om_pct","soilk_ppm","soilp_ppm",
              "pHom","pk","soilpH^2","om_pct^2","p+k","norm_pk")
weather_vars = c("prcp_mm_day","srad_w_m_2","swe_kg_m_2",
                 "tmax_deg_c","tmin_deg_c","vp_pa")
other_vars = c("DAP","longitude","latitude")
cat_vars = c("site","hybrid","previous_crop")
all_vars = c(soil_vars, weather_vars, other_vars, cat_vars)

num_vars = merged %>% select(all_of(c(soil_vars, weather_vars, other_vars)))

# ───────────────────────────────────────────────────────────────────────────────
# UI
# ───────────────────────────────────────────────────────────────────────────────

ui = dashboardPage(
  dashboardHeader(title = "Corn Yield Explorer"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",    tabName = "overview",    icon = icon("chart-bar")),
      menuItem("EDA",         tabName = "eda",         icon = icon("search")),
      menuItem("Model VIP",   tabName = "model_vip",   icon = icon("cogs")),
      menuItem("Predictions", tabName = "predictions", icon = icon("sync-alt"))
    ),
    conditionalPanel(
      "input.tabs == 'eda'",
      selectInput("eda_var", "Choose predictor:", choices = all_vars),
      sliderInput("eda_bins", "Bins for yield histogram:", min = 10, max = 100, value = 30)
    ),
    conditionalPanel(
      "input.tabs == 'model_vip'",
      sliderInput("vip_n", "Show top N variables:", min = 5,
                  max = length(soil_vars) + length(weather_vars), value = 10)
    )
  ),
  dashboardBody(
    tabItems(
      # Overview
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Maize Yield Prediction Using Machine Learning (Group 4)",
            width = 12, status = "info", solidHeader = TRUE,
            includeMarkdown("README.md")
          ),
          box(
            title = "Yield Histogram (Train)", status = "primary", solidHeader = TRUE,
            width = 6, plotlyOutput("overview_hist")
          ),
          box(
            title = "Correlation Matrix + Scatter + Hist", status = "primary", solidHeader = TRUE,
            width = 6, plotOutput("corr_matrix", height = 400)
          )
        )
      ),
      # EDA
      tabItem(
        tabName = "eda",
        fluidRow(
          box(
            title = "Yield Histogram", status = "info", solidHeader = TRUE,
            width = 6, plotlyOutput("hist2_yield")
          ),
          box(
            title = "Predictor vs. Yield", status = "info", solidHeader = TRUE,
            width = 6, plotlyOutput("eda_plot")
          )
        )
      ),
      # Model VIP
      tabItem(
        tabName = "model_vip",
        fluidRow(
          box(
            title = "Variable Importance", status = "warning", solidHeader = TRUE,
            width = 8, plotlyOutput("vip_plot")
          )
        )
      ),
      # Predictions
      tabItem(
        tabName = "predictions",
        fluidRow(
          valueBoxOutput("box_r2",   width = 6),
          valueBoxOutput("box_rmse", width = 6)
        ),
        fluidRow(
          box(
            title = "Observed vs. Predicted (Test)", status = "success", solidHeader = TRUE,
            width = 8, plotlyOutput("obs_pred")
          )
        )
      )
    )
  )
)

# ───────────────────────────────────────────────────────────────────────────────
# Server
# ───────────────────────────────────────────────────────────────────────────────

server = function(input, output, session) {
  
  # Overview histogram
  output$overview_hist = renderPlotly({
    p = ggplot(train_data, aes(x = yield_mg_ha)) +
      geom_histogram(bins = 30, fill = "darkred", color = "white") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Correlation matrix
  output$corr_matrix = renderPlot({
    chart.Correlation(
      num_vars,
      histogram = TRUE,
      pch       = 19,
      method    = "pearson"
    )
  })
  
  # EDA: second histogram
  output$hist2_yield = renderPlotly({
    p = ggplot(train_data, aes(x = yield_mg_ha)) +
      geom_histogram(bins = input$eda_bins, fill = "darkorange", color = "white") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  # EDA: predictor vs yield
  output$eda_plot = renderPlotly({
    var = input$eda_var
    if (var %in% c(soil_vars, weather_vars, other_vars)) {
      p = ggplot(train_data, aes_string(x = var, y = "yield_mg_ha")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "darkred") +
        theme_minimal()
    } else {
      p = ggplot(train_data, aes_string(x = var, y = "yield_mg_ha")) +
        geom_boxplot(fill = "skyblue") +
        theme_minimal()
    }
    ggplotly(p)
  })
  
  # Variable importance with explicit fit extraction + validation
  vip_df = reactive({
    req(fitted_xgb_wf)
    fit_obj = extract_fit_parsnip(fitted_xgb_wf)
    vi(fit_obj) %>%
      arrange(desc(Importance)) %>%
      slice_head(n = input$vip_n)
  })
  
  output$vip_plot = renderPlotly({
    df = vip_df()
    validate(
      need(nrow(df) > 0, "No variable‐importance data available.")
    )
    print(str(df))  # inspect structure in console
    p = ggplot(df, aes(x = Importance, y = fct_reorder(Variable, Importance))) +
      geom_col(fill = "tomato") +
      labs(x = "Importance", y = NULL, title = "Top Variable Importance") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Predictions reactive
  preds = reactive({
    req(fitted_xgb_wf, test_data)
    predict(fitted_xgb_wf, new_data = test_data) %>%
      bind_cols(test_data)
  })
  
  # Observed vs Predicted with validation
  output$obs_pred = renderPlotly({
    df = preds()
    validate(
      need(nrow(df) > 0, "No predictions returned."),
      need(".pred" %in% names(df), "Prediction column `.pred` not found.")
    )
    print(names(df))  # inspect column names in console
    p = ggplot(df, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
      labs(
        x = "Observed Yield (Mg/ha)",
        y = "Predicted Yield (Mg/ha)",
        title = "Observed vs. Predicted"
      ) +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # R² & RMSE boxes
  metric_vals = reactive({
    preds() %>%
      metrics(truth = yield_mg_ha, estimate = .pred) %>%
      filter(.metric %in% c("rsq", "rmse"))
  })
  output$box_r2 = renderValueBox({
    r2 = metric_vals() %>% filter(.metric == "rsq") %>% pull(.estimate)
    valueBox(round(r2, 3), "R² (Test)", icon = icon("chart-line"), color = "green")
  })
  output$box_rmse = renderValueBox({
    rm = metric_vals() %>% filter(.metric == "rmse") %>% pull(.estimate)
    valueBox(round(rm, 3), "RMSE (Test)", icon = icon("tachometer-alt"), color = "blue")
  })
}

# ───────────────────────────────────────────────────────────────────────────────
# Launch
# ───────────────────────────────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)

