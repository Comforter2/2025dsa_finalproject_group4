
# app.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(tidymodels)
library(vip)
library(yardstick)

# ─── 0. GLOBAL SETUP ─────────────────────────────────────────────────────────────

# 0.1 Load your merged data & model
merged   <- read_csv("merged_factored.csv")    # your 7 392×24 tibble


ml_recipe = recipe(yield_mg_ha ~ ., data = train_data) %>%
  step_rm(site, previous_crop, hybrid) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


xgb_spec = boost_tree(
  trees      = tune(),  
  tree_depth = tune(),  
  min_n      = tune(),  
  learn_rate = tune()   
) %>%
  set_engine("xgboost") %>%  
  set_mode("regression")     



xgb_wf = ml_recipe + 
  
  xgb_wf   <- read_rds("xgb_workflow.rds")       # fitted workflow (recipe + final XGB spec)

# 0.2 Train/Test split (70/30 stratified)
set.seed(1996)
split_obj  <- initial_split(merged, prop = 0.7, strata = yield_mg_ha)
train_data <- training(split_obj)
test_data  <- testing(split_obj)

# 0.3 Predictor groups
soil_vars    <- c("soilpH","om_pct","soilk_ppm","soilp_ppm",
                  "pHom","pk","soilpH^2","om_pct^2","p+k","norm_pk")
weather_vars <- c("prcp_mm_day","srad_w_m_2","swe_kg_m_2",
                  "tmax_deg_c","tmin_deg_c","vp_pa")
other_vars   <- c("DAP","grain_moisture","longitude","latitude")
cat_vars     <- c("site","hybrid","previous_crop")
all_vars     <- c(soil_vars, weather_vars, other_vars, cat_vars)

# ─── 1. UI ─────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  dashboardHeader(title = "Cotton Yield XGBoost"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Overview",    tabName = "overview",    icon = icon("chart-bar")),
                menuItem("EDA",         tabName = "eda",         icon = icon("search")),
                menuItem("Model VIP",   tabName = "model_vip",   icon = icon("cogs")),
                menuItem("Predictions", tabName = "predictions", icon = icon("sync-alt"))
    ),
    
    # EDA controls
    conditionalPanel(
      "input.tabs == 'eda'",
      selectInput("eda_var", "Choose predictor:", choices = all_vars),
      sliderInput("eda_bins", "Bins for yield histogram:", min = 10, max = 100, value = 30)
    ),
    
    # VIP controls
    conditionalPanel(
      "input.tabs == 'model_vip'",
      sliderInput("vip_n", "Show top N variables:", min = 5, max = length(soil_vars)+length(weather_vars), value = 10)
    )
  ),
  dashboardBody(
    tabItems(
      
      # ─ Overview ─────────────────────────────────────────────────────────────────
      tabItem("overview",
              fluidRow(
                box(title = "Yield (Train) Histogram", status = "primary", solidHeader = TRUE,
                    width = 6, plotlyOutput("hist_yield"))
              )
      ),
      
      # ─ EDA ───────────────────────────────────────────────────────────────────────
      tabItem("eda",
              fluidRow(
                box(title = "Yield Histogram", status = "info", solidHeader = TRUE,
                    width = 6, plotlyOutput("hist2_yield")),
                box(title = "Predictor vs. Yield", status = "info", solidHeader = TRUE,
                    width = 6, plotlyOutput("eda_plot"))
              )
      ),
      
      # ─ Model VIP ─────────────────────────────────────────────────────────────────
      tabItem("model_vip",
              fluidRow(
                box(title = "Variable Importance", status = "warning", solidHeader = TRUE,
                    width = 8, plotlyOutput("vip_plot"))
              )
      ),
      
      # ─ Predictions ───────────────────────────────────────────────────────────────
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

# ─── 2. SERVER ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # Overview: Yield histogram
  output$hist_yield <- renderPlotly({
    p <- ggplot(train_data, aes(x = yield_mg_ha)) +
      geom_histogram(bins = input$eda_bins, fill = "steelblue", color = "white") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      theme_minimal()
    ggplotly(p)
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
        labs(x = var, y = "Yield (Mg/ha)") +
        theme_minimal()
    } else {
      p <- ggplot(train_data, aes_string(x = var, y = "yield_mg_ha")) +
        geom_boxplot(fill = "skyblue") +
        labs(x = var, y = "Yield (Mg/ha)") +
        theme_minimal()
    }
    ggplotly(p)
  })
  
  # Model VIP
  vip_df <- reactive({
    fit_obj <- pull_workflow_fit(xgb_wf)$fit
    vi(fit_obj) %>%
      arrange(desc(Importance)) %>%
      slice_head(n = input$vip_n)
  })
  output$vip_plot <- renderPlotly({
    df <- vip_df()
    p <- ggplot(df, aes(x = Importance, y = fct_reorder(Variable, Importance))) +
      geom_col(fill = "tomato") +
      labs(x = "Importance", y = NULL) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Predictions
  preds <- reactive({
    predict(xgb_wf, test_data) %>% bind_cols(test_data)
  })
  
  # Observed vs. Predicted
  output$obs_pred <- renderPlotly({
    df <- preds()
    p <- ggplot(df, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.6) +
      geom_abline(linetype = "dashed") +
      labs(x = "Observed Yield", y = "Predicted Yield") +
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

# ─── 3. RUN APP ───────────────────────────────────────────────────────────────────

shinyApp(ui, server)



