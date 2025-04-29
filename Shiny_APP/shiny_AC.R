# shiny app.R

# ───────────────────────────────────────────────────────────────────────────────
# Loading Libraries
# ───────────────────────────────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(readr)        # read_csv, read_rds
library(tidyverse)    # dplyr, ggplot2, etc.
library(plotly)       # for interactive plots
library(tidymodels)   # initial_split, recipe, workflows, metrics
library(vip)          # variable importance
library(yardstick)    # rmse, rsq
library(PerformanceAnalytics)  # for chart.Correlation()
library(markdown)

# ───────────────────────────────────────────────────────────────────────────────
# Setup
# ───────────────────────────────────────────────────────────────────────────────

# 0.1 Load data & pre‐fitted workflow (adjust paths accordingly)
merged         <- read_csv("test_data.csv")
fitted_xgb_wf  <- read_rds("xgb_full_data_workflow.rds")

# 0.3 Predictor groups
soil_vars    <- c("soilpH","om_pct","soilk_ppm","soilp_ppm",
                  "pHom","pk","soilpH^2","om_pct^2","p+k","norm_pk")
weather_vars <- c("prcp_mm_day","srad_w_m_2","swe_kg_m_2",
                  "tmax_deg_c","tmin_deg_c","vp_pa")
other_vars   <- c("DAP","longitude","latitude")
cat_vars     <- c("site","hybrid","previous_crop")
all_vars     <- c(soil_vars, weather_vars, other_vars, cat_vars)

# Prepare numeric-only data for correlation matrix
num_vars <- merged %>% select(all_of(c(soil_vars, weather_vars, other_vars)))

# ───────────────────────────────────────────────────────────────────────────────
# UI Interface
# ───────────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  dashboardHeader(title = "Corn Yield Explorer"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
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
      # ─── Overview ───────────────────────────────────────────────────────────
      tabItem("overview",
              fluidRow(
                box(
                  title = "Maize Yield Prediction Using Machine Learning (Group 4)", width = 12, status = "info", solidHeader = TRUE,
                  includeMarkdown("README.md")),
                box(title = "Yield Histogram (Train)", status = "primary", solidHeader = TRUE,
                    width = 6, plotlyOutput("overview_hist")),
                box(title = "Correlation Matrix + Scatter + Hist", status = "primary", solidHeader = TRUE,
                    width = 6, plotOutput("corr_matrix", height = 400))
              )
      ),
      # ─── EDA ───────────────────────────────────────────────────────────────
      tabItem("eda",
              fluidRow(
                box(title = "Yield Histogram", status = "info", solidHeader = TRUE,
                    width = 6, plotlyOutput("hist2_yield")),
                box(title = "Predictor vs. Yield", status = "info", solidHeader = TRUE,
                    width = 6, plotlyOutput("eda_plot"))
              )
      ),
      # ─── Model VIP ─────────────────────────────────────────────────────────
      tabItem("model_vip",
              fluidRow(
                box(title = "Variable Importance", status = "warning", solidHeader = TRUE,
                    width = 8, plotlyOutput("vip_plot"))
              )
      ),
      # ─── Predictions ───────────────────────────────────────────────────────
      tabItem("predictions",
              fluidRow(
                valueBoxOutput("box_r2",   width = 6),
                valueBoxOutput("box_rmse", width = 6)
              ),
              fluidRow(
                box(title = "Observed vs. Predicted (Test)", status = "success", solidHeader = TRUE,
                    width = 8, plotlyOutput("obs_pred"))
              )
      )
    )
  )
)

# ───────────────────────────────────────────────────────────────────────────────
# 2. Server
# ───────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Overview histogram
  output$overview_hist <- renderPlotly({
    p <- ggplot(train_data, aes(x = yield_mg_ha)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Correlation matrix + scatter + histogram
  output$corr_matrix <- renderPlot({
    chart.Correlation(
      num_vars,
      histogram = TRUE,
      pch       = 19,
      method    = "pearson"
    )
  })
  
  # EDA: Yield histogram (second)
  output$hist2_yield <- renderPlotly({
    p <- ggplot(train_data, aes(x = yield_mg_ha)) +
      geom_histogram(bins = input$eda_bins, fill = "darkorange", color = "white") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  # EDA: Predictor vs. Yield
  output$eda_plot <- renderPlotly({
    var <- input$eda_var
    if (var %in% c(soil_vars, weather_vars, other_vars)) {
      p <- ggplot(train_data, aes_string(x = var, y = "yield_mg_ha")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "darkred") +
        theme_minimal()
    } else {
      p <- ggplot(train_data, aes_string(x = var, y = "yield_mg_ha")) +
        geom_boxplot(fill = "skyblue") +
        theme_minimal()
    }
    ggplotly(p)
  })
  
  # Model VIP
  vip_df <- reactive({
    fit_obj <- pull_workflow_fit(fitted_xgb_wf)$fit
    vi(fit_obj) %>%
      arrange(desc(Importance)) %>%
      slice_head(n = input$vip_n)
  })
  output$vip_plot <- renderPlotly({
    df <- vip_df()
    p <- ggplot(df, aes(x = Importance, y = fct_reorder(Variable, Importance))) +
      geom_col(fill = "tomato") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Predictions
  preds <- reactive({
    predict(fitted_xgb_wf, test_data) %>%
      bind_cols(test_data)
  })
  output$obs_pred <- renderPlotly({
    df <- preds()
    p <- ggplot(df, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.6) +
      geom_abline(linetype = "dashed") +
      theme_minimal()
    ggplotly(p)
  })
  
  # R² & RMSE value boxes
  metric_vals <- reactive({
    preds() %>%
      metrics(truth = yield_mg_ha, estimate = .pred) %>%
      filter(.metric %in% c("rsq","rmse"))
  })
  output$box_r2 <- renderValueBox({
    r2 <- metric_vals() %>% filter(.metric=="rsq")   %>% pull(.estimate)
    valueBox(round(r2,3), "R² (Test)", icon = icon("chart-line"), color = "green")
  })
  output$box_rmse <- renderValueBox({
    rm <- metric_vals() %>% filter(.metric=="rmse") %>% pull(.estimate)
    valueBox(round(rm,3), "RMSE (Test)", icon = icon("tachometer-alt"), color = "blue")
  })
}

# ───────────────────────────────────────────────────────────────────────────────
# 3. Run the app
# ───────────────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
