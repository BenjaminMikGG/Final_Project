library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ── Load Data ──────────────────────────────────────────────────────────────────
train      <- read.csv("data_shiny/train_eda.csv",  stringsAsFactors = FALSE)
pred_obs   <- read.csv("data_shiny/pred_obs.csv",   stringsAsFactors = FALSE)
importance <- read.csv("data_shiny/importance.csv", stringsAsFactors = FALSE)

# ── Data Prep ──────────────────────────────────────────────────────────────────
# True full dataset size (plots use 20k sample for performance)
true_n_obs   <- 164477
true_n_sites <- 39
true_n_years <- 10

# Classify importance features
soil_feats      <- c("soilp_h", "soilk_ppm", "soilp_ppm", "om_pct")
importance$Type <- ifelse(importance$Feature %in% soil_feats, "Soil", "Weather")
importance$Gain_pct <- importance$Gain / sum(importance$Gain) * 100

# Fix prev_crop_simple if missing
if (!"prev_crop_simple" %in% names(train)) {
  simplify_crop <- function(x) {
    x <- tolower(trimws(as.character(x)))
    if (grepl("soybean|soybeans", x)) return("Soybean")
    if (grepl("corn",    x)) return("Corn")
    if (grepl("wheat",   x)) return("Wheat")
    if (grepl("peanut",  x)) return("Peanut")
    if (grepl("sorghum", x)) return("Sorghum")
    if (grepl("cotton",  x)) return("Cotton")
    if (grepl("beet",    x)) return("Sugar Beet")
    if (grepl("fallow",  x)) return("Fallow")
    if (grepl("rye",     x)) return("Rye")
    return("Other")
  }
  train$prev_crop_simple <- sapply(train$previous_crop, simplify_crop)
}

# Global metrics from full pred_obs
r2_val   <- round(1 - sum((pred_obs$yield_mg_ha - pred_obs$.pred)^2) /
                      sum((pred_obs$yield_mg_ha - mean(pred_obs$yield_mg_ha))^2), 4)
rmse_val <- round(sqrt(mean((pred_obs$yield_mg_ha - pred_obs$.pred)^2)), 4)

# Year / site lists for dropdowns
all_years <- sort(unique(train$year))
all_sites <- sort(unique(train$site))

# ── Colour Palette ─────────────────────────────────────────────────────────────
pal_soil    <- "#4CAF50"
pal_weather <- "#2196F3"
pal_yield   <- "#FF9800"
pal_bg      <- "#1a1a2e"
pal_card    <- "#16213e"
pal_accent  <- "#e94560"
pal_text    <- "#e0e0e0"
pal_sub     <- "#9e9e9e"

# ── Shared ggplot theme ────────────────────────────────────────────────────────
dark_theme <- theme_minimal(base_family = "sans") +
  theme(
    plot.background   = element_rect(fill = "#16213e", color = NA),
    panel.background  = element_rect(fill = "#16213e", color = NA),
    panel.grid.major  = element_line(color = "#263056", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    axis.text         = element_text(color = "#9e9e9e", size = 9),
    axis.title        = element_text(color = "#c0c0c0", size = 10),
    plot.title        = element_text(color = "#e0e0e0", size = 11, face = "bold"),
    plot.subtitle     = element_text(color = "#9e9e9e", size = 9),
    legend.background = element_rect(fill = "#16213e", color = NA),
    legend.text       = element_text(color = "#9e9e9e"),
    legend.title      = element_text(color = "#c0c0c0"),
    strip.text        = element_text(color = "#c0c0c0")
  )

# ── CSS ────────────────────────────────────────────────────────────────────────
app_css <- paste0("
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
  * { box-sizing: border-box; }
  body {
    background:", pal_bg, ";
    color:", pal_text, ";
    font-family: 'Inter', sans-serif;
    margin: 0; padding: 0;
  }
  .navbar-header { display: none !important; }
  .navbar {
    background: #0f3460 !important;
    border: none !important;
    border-bottom: 2px solid ", pal_accent, " !important;
    margin-bottom: 0 !important;
  }
  .navbar-nav > li > a {
    color:", pal_sub, " !important;
    font-size: 0.9rem; font-weight: 500;
    padding: 15px 22px !important;
    transition: color 0.2s;
  }
  .navbar-nav > li.active > a,
  .navbar-nav > li > a:hover {
    color: #fff !important;
    background: transparent !important;
    border-bottom: 3px solid ", pal_accent, " !important;
  }
  .tab-content {
    padding: 26px 30px !important;
    background:", pal_bg, ";
  }
  /* ── Cards ── */
  .card {
    background:", pal_card, ";
    border-radius: 12px;
    padding: 20px 22px;
    margin-bottom: 18px;
    box-shadow: 0 4px 18px rgba(0,0,0,0.35);
  }
  .card-title {
    font-size: 0.92rem; font-weight: 600;
    color:", pal_text, ";
    margin: 0 0 12px 0;
    padding-bottom: 10px;
    border-bottom: 1px solid #263056;
    display: flex; align-items: center; gap: 8px;
  }
  .dot {
    width: 10px; height: 10px;
    border-radius: 50%;
    display: inline-block;
    flex-shrink: 0;
  }
  /* ── KPI metric boxes ── */
  .metric-row {
    display: flex; gap: 14px;
    margin-bottom: 22px;
    flex-wrap: wrap;
  }
  .metric-box {
    flex: 1; min-width: 160px;
    background:", pal_card, ";
    border-radius: 10px;
    padding: 16px 18px;
    box-shadow: 0 4px 16px rgba(0,0,0,0.3);
    border-left: 4px solid ", pal_accent, ";
  }
  .metric-label {
    font-size: 0.72rem; color:", pal_sub, ";
    text-transform: uppercase; letter-spacing: 1px;
  }
  .metric-value {
    font-size: 1.85rem; font-weight: 700;
    color: #fff; margin-top: 4px; line-height: 1.1;
  }
  .metric-sub {
    font-size: 0.74rem; color:", pal_sub, "; margin-top: 4px;
  }
  /* ── Control panel ── */
  .ctrl-panel {
    background:", pal_card, ";
    border-radius: 12px;
    padding: 18px 18px;
    margin-bottom: 18px;
    box-shadow: 0 4px 16px rgba(0,0,0,0.3);
  }
  .ctrl-panel label {
    color:", pal_sub, " !important;
    font-size: 0.78rem !important;
    text-transform: uppercase !important;
    letter-spacing: 1px !important;
  }
  .section-desc {
    color:", pal_sub, ";
    font-size: 0.81rem;
    margin: 8px 0 0 0;
    line-height: 1.55;
  }
  hr.div {
    border: none;
    border-top: 1px solid #263056;
    margin: 14px 0;
  }
  /* ── Selectize inputs ── */
  .selectize-input, .selectize-dropdown {
    background: #0f3460 !important;
    color:", pal_text, " !important;
    border: 1px solid #263056 !important;
    border-radius: 8px !important;
    font-size: 0.87rem !important;
  }
  .selectize-dropdown-content .option {
    color:", pal_text, " !important;
    padding: 6px 10px !important;
  }
  .selectize-dropdown-content .option:hover,
  .selectize-dropdown-content .option.active {
    background: #1a3a6e !important;
  }
  /* ── Slider ── */
  .irs--shiny .irs-bar {
    background:", pal_accent, " !important;
    border-color:", pal_accent, " !important;
  }
  .irs--shiny .irs-handle {
    background:", pal_accent, " !important;
    border-color:", pal_accent, " !important;
  }
  .irs--shiny .irs-from,
  .irs--shiny .irs-to,
  .irs--shiny .irs-single {
    background:", pal_accent, " !important;
  }
  .irs--shiny .irs-line  { background: #263056 !important; }
  .irs--shiny .irs-grid-text { color:", pal_sub, " !important; }
  /* ── Checkboxes ── */
  .checkbox label { color:", pal_text, " !important; font-size: 0.87rem !important; }
  /* ── Table ── */
  table.shiny-table {
    color:", pal_text, " !important;
    font-size: 0.83rem; width: 100%;
    border-collapse: collapse;
  }
  table.shiny-table th {
    color:", pal_sub, " !important;
    border-bottom: 1px solid #263056 !important;
    font-weight: 600; padding: 6px 6px !important;
    text-align: center !important;
  }
  table.shiny-table td {
    border-bottom: 1px solid #1a2540 !important;
    padding: 7px 6px !important;
    text-align: center !important;
  }
  /* ── Plots ── */
  .shiny-plot-output { border-radius: 8px; overflow: hidden; }
  /* ── Validation messages ── */
  .shiny-output-error-validation {
    color:", pal_sub, " !important;
    font-size: 0.85rem; font-style: italic;
    padding: 20px;
  }
")

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = div(
    style = "display:flex; align-items:center; gap:10px;",
    span(style = "font-size:1.3rem;", "🌽"),
    span(style = "font-weight:700; font-size:1rem; color:#fff;",
         "Corn Yield Prediction – XGBoost"),
    span(style = paste0(
      "background:", pal_accent, "; color:#fff;",
      "padding:3px 10px; border-radius:10px;",
      "font-size:0.7rem; font-weight:600;"),
      paste0("R\u00b2 = ", r2_val)),
    span(style = paste0(
      "background:#4CAF50; color:#fff;",
      "padding:3px 10px; border-radius:10px;",
      "font-size:0.7rem; font-weight:600;"),
      paste0("RMSE = ", rmse_val, " Mg/ha"))
  ),
  id = "main_tabs",
  collapsible = TRUE,
  tags$head(tags$style(HTML(app_css))),
  # ════════════════════════════════════════════════════════════════════════════
  # TAB 0 — Welcome
  # ════════════════════════════════════════════════════════════════════════════
tabPanel("Welcome",

  fluidRow(
    column(12,
      div(class = "card",

        div(class = "card-title",
            span(class = "dot",
                 style = paste0("background:", pal_accent, ";")),
            "About This App"),

        p(style = "font-size:0.95rem; line-height:1.7;",
          "This dashboard presents results from a large corn variety trial dataset ",
          "containing more than 160,000 observations collected between 2014 and 2023 ",
          "across approximately 45 locations in the United States."
        ),

        p(style = "font-size:0.95rem; line-height:1.7;",
          "The app allows users to explore how corn yield varies across environments ",
          "and how different soil properties, weather conditions, and management factors ",
          "influence yield outcomes."
        ),

        p(style = "font-size:0.95rem; line-height:1.7;",
          "Interactive visualizations are provided to examine yield distributions, ",
          "relationships with environmental variables, model prediction accuracy, ",
          "and the most important predictors identified by the machine-learning model."
        )
      )
    )
  )
),
  # TAB 1 — Yield EDA
  # ════════════════════════════════════════════════════════════════════════════
  tabPanel("📊 Yield EDA",

    # KPI row — shows TRUE full-dataset numbers
    div(class = "metric-row",
      div(class = "metric-box",
        div(class = "metric-label", "Training Observations"),
        div(class = "metric-value",
            formatC(true_n_obs, format = "d", big.mark = ",")),
        div(class = "metric-sub",
            paste0(true_n_sites, " sites \u00b7 ", true_n_years,
                   " years \u00b7 plots show 20k sample"))
      ),
      div(class = "metric-box", style = "border-left-color:#4CAF50;",
        div(class = "metric-label", "Median Yield"),
        div(class = "metric-value",
            round(median(train$yield_mg_ha, na.rm = TRUE), 2)),
        div(class = "metric-sub", "Mg/ha")
      ),
      div(class = "metric-box", style = "border-left-color:#2196F3;",
        div(class = "metric-label", "Yield Range"),
        div(class = "metric-value",
            paste0(round(min(train$yield_mg_ha, na.rm = TRUE), 1),
                   " \u2013 ",
                   round(max(train$yield_mg_ha, na.rm = TRUE), 1))),
        div(class = "metric-sub", "Mg/ha")
      ),
      div(class = "metric-box", style = "border-left-color:#9C27B0;",
        div(class = "metric-label", "Std Deviation"),
        div(class = "metric-value",
            round(sd(train$yield_mg_ha, na.rm = TRUE), 2)),
        div(class = "metric-sub", "Mg/ha")
      )
    ),

    fluidRow(
      column(3,
        div(class = "ctrl-panel",
          div(class = "card-title",
              span(class = "dot",
                   style = paste0("background:", pal_accent, ";")),
              "Filter Options"),
          selectInput("eda_year", "Year",
                      choices  = c("All", all_years),
                      selected = "All"),
          selectInput("eda_site", "Site",
                      choices  = c("All", all_sites),
                      selected = "All"),
          hr(class = "div"),
          p(class = "section-desc",
            "Explore corn yield distribution across years and sites.",
            br(),
            "Filters apply to all three plots.")
        )
      ),
      column(9,
        fluidRow(
          column(6,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot",
                       style = paste0("background:", pal_yield, ";")),
                  "Yield Distribution"),
              plotOutput("yield_hist", height = "270px")
            )
          ),
          column(6,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#9C27B0;"),
                  "Yield by Year  (bar = median, whiskers = IQR)"),
              plotOutput("yield_by_year", height = "270px")
            )
          )
        ),
        fluidRow(
          column(12,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#2196F3;"),
                  "Median Yield by Site  \u2014 hover a bar for details"),
              div(style = "position:relative;",
                plotOutput("yield_by_site", height = "240px",
                           hover = hoverOpts("site_hover",
                                             delay     = 80,
                                             delayType = "debounce")),
                uiOutput("site_tooltip")
              )
            )
          )
        )
      )
    )
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 2 — Predictor EDA
  # ════════════════════════════════════════════════════════════════════════════
  tabPanel("🔬 Predictor EDA",

    fluidRow(
      column(3,
        div(class = "ctrl-panel",
          div(class = "card-title",
              span(class = "dot", style = "background:#4CAF50;"),
              "Soil Variables"),
          selectInput("soil_var", "Select Soil Variable",
            choices = c(
              "Soil pH"            = "soilp_h",
              "Potassium (ppm)"    = "soilk_ppm",
              "Phosphorus (ppm)"   = "soilp_ppm",
              "Organic Matter (%)" = "om_pct"
            ),
            selected = "soilp_h"),
          hr(class = "div"),
          div(class = "card-title",
              span(class = "dot", style = "background:#2196F3;"),
              "Weather Variables"),
          selectInput("wx_var", "Select Weather Variable",
            choices = c(
              "Total GDD"                = "total_gdd",
              "Total Precipitation (mm)" = "total_prcp",
              "Avg Temperature (\u00b0C)"  = "avg_t2m",
              "GDD \u2013 Early Season"   = "gdd_early",
              "GDD \u2013 Mid Season"     = "gdd_mid",
              "GDD \u2013 Late Season"    = "gdd_late",
              "Precip \u2013 Early (mm)" = "prcp_early",
              "Precip \u2013 Mid (mm)"   = "prcp_mid",
              "Precip \u2013 Late (mm)"  = "prcp_late"
            ),
            selected = "total_gdd"),
          hr(class = "div"),
          selectInput("pred_year", "Filter by Year",
                      choices  = c("All", all_years),
                      selected = "All"),
          hr(class = "div"),
          p(class = "section-desc",
            "Scatter plots show individual observations with a",
            tags$b("LOESS smooth"), "and 95% confidence band.",
            br(), br(),
            "Soil variables have some missing values — these are",
            "excluded automatically.")
        )
      ),
      column(9,
        fluidRow(
          column(6,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#4CAF50;"),
                  "Soil Variable vs. Yield"),
              plotOutput("soil_scatter", height = "270px")
            )
          ),
          column(6,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#4CAF50;"),
                  "Soil Variable Distribution"),
              plotOutput("soil_hist", height = "270px")
            )
          )
        ),
        fluidRow(
          column(6,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#2196F3;"),
                  "Weather Variable vs. Yield"),
              plotOutput("wx_scatter", height = "270px")
            )
          ),
          column(6,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#2196F3;"),
                  "Previous Crop \u2013 Yield Boxplot"),
              plotOutput("prev_crop_box", height = "270px")
            )
          )
        )
      )
    )
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 3 — Model Performance
  # ════════════════════════════════════════════════════════════════════════════
  tabPanel("🎯 Model Performance",

    div(class = "metric-row",
      div(class = "metric-box",
        div(class = "metric-label", "R\u00b2  (Cross-Validation)"),
        div(class = "metric-value", r2_val),
        div(class = "metric-sub", "Proportion of variance explained")
      ),
      div(class = "metric-box", style = "border-left-color:#4CAF50;",
        div(class = "metric-label", "RMSE"),
        div(class = "metric-value", rmse_val),
        div(class = "metric-sub", "Mg/ha \u2013 root mean squared error")
      ),
      div(class = "metric-box", style = "border-left-color:#2196F3;",
        div(class = "metric-label", "CV Predictions Used"),
        div(class = "metric-value",
            formatC(nrow(pred_obs), format = "d", big.mark = ",")),
        div(class = "metric-sub", "from held-out CV folds")
      ),
      div(class = "metric-box", style = "border-left-color:#9C27B0;",
        div(class = "metric-label", "Best Hyperparameters"),
        div(class = "metric-value",
            style = "font-size:0.88rem; margin-top:6px; line-height:1.7;",
            "depth=7 \u00b7 trees=700"),
        div(class = "metric-sub",
            "lr=0.046 \u00b7 subsample=0.896")
      )
    ),

    fluidRow(
      column(8,
        div(class = "card",
          div(class = "card-title",
              span(class = "dot",
                   style = paste0("background:", pal_accent, ";")),
              "Predicted vs. Observed Yield"),
          p(class = "section-desc",
            paste0("R\u00b2 = ", r2_val,
                   "  \u00b7  RMSE = ", rmse_val, " Mg/ha",
                   "  \u00b7  Green dashed = perfect 1:1 line",
                   "  \u00b7  Blue = fitted regression")),
          sliderInput("n_points",
                      "Number of points to display:",
                      min = 2000, max = 30000,
                      value = 15000, step = 1000,
                      width = "100%"),
          plotOutput("pred_obs_plot", height = "400px")
        )
      ),
      column(4,
        div(class = "card",
          div(class = "card-title",
              span(class = "dot", style = "background:#FF9800;"),
              "Residual Distribution"),
          p(class = "section-desc",
            "Predicted \u2212 Observed. Should be centred on 0."),
          plotOutput("resid_plot", height = "175px")
        ),
        div(class = "card",
          div(class = "card-title",
              span(class = "dot", style = "background:#9C27B0;"),
              "MAE by Yield Bin"),
          p(class = "section-desc",
            "Where does the model struggle most?"),
          plotOutput("error_by_bin", height = "175px")
        )
      )
    )
  ),

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 4 — Variable Importance
  # ════════════════════════════════════════════════════════════════════════════
  tabPanel("⭐ Variable Importance",

    fluidRow(
      column(3,
        div(class = "ctrl-panel",
          div(class = "card-title",
              span(class = "dot",
                   style = paste0("background:", pal_accent, ";")),
              "Controls"),
          sliderInput("top_n", "Top N features:",
                      min   = 5,
                      max   = nrow(importance),
                      value = 20,
                      step  = 1),
          hr(class = "div"),
          checkboxGroupInput("imp_type", "Variable type:",
            choices  = c("Weather" = "Weather", "Soil" = "Soil"),
            selected = c("Weather", "Soil")),
          hr(class = "div"),
          div(class = "section-desc",
            tags$b("Gain"), " = average improvement in loss",
            " brought by a feature across all tree splits.",
            br(), br(),
            tags$span(style = "color:#2196F3; font-weight:600;",
                      "\u25a0 Weather variables"),
            br(),
            tags$span(style = "color:#4CAF50; font-weight:600;",
                      "\u25a0 Soil variables")
          )
        )
      ),
      column(9,
        fluidRow(
          column(8,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot",
                       style = paste0("background:", pal_accent, ";")),
                  "Feature Importance (% of Total Gain)"),
              plotOutput("imp_bar", height = "500px")
            )
          ),
          column(4,
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#2196F3;"),
                  "Soil vs. Weather Share"),
              plotOutput("imp_pie", height = "220px")
            ),
            div(class = "card",
              div(class = "card-title",
                  span(class = "dot", style = "background:#4CAF50;"),
                  "Top 5 Features"),
              tableOutput("imp_table")
            )
          )
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive: filtered data for Tab 1 ──
  filt_train <- reactive({
    d <- train
    if (!is.null(input$eda_year) && input$eda_year != "All")
      d <- d[d$year == as.integer(input$eda_year), ]
    if (!is.null(input$eda_site) && input$eda_site != "All")
      d <- d[d$site == input$eda_site, ]
    d
  })

  # ── Reactive: filtered data for Tab 2 ──
  filt_pred <- reactive({
    d <- train
    if (!is.null(input$pred_year) && input$pred_year != "All")
      d <- d[d$year == as.integer(input$pred_year), ]
    d
  })

  # ── Helper: nice axis label ──
  var_label <- function(v) {
    switch(v,
      soilp_h    = "Soil pH",
      soilk_ppm  = "Potassium (ppm)",
      soilp_ppm  = "Phosphorus (ppm)",
      om_pct     = "Organic Matter (%)",
      total_gdd  = "Total GDD",
      total_prcp = "Total Precipitation (mm)",
      avg_t2m    = "Avg Temperature (\u00b0C)",
      gdd_early  = "GDD \u2013 Early Season",
      gdd_mid    = "GDD \u2013 Mid Season",
      gdd_late   = "GDD \u2013 Late Season",
      prcp_early = "Precip \u2013 Early Season (mm)",
      prcp_mid   = "Precip \u2013 Mid Season (mm)",
      prcp_late  = "Precip \u2013 Late Season (mm)",
      v
    )
  }

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 1 outputs
  # ════════════════════════════════════════════════════════════════════════════

  output$yield_hist <- renderPlot({
    d <- filt_train()
    validate(need(nrow(d) >= 5, "No data for the selected filters."))
    med <- median(d$yield_mg_ha, na.rm = TRUE)
    ggplot(d, aes(x = yield_mg_ha)) +
      geom_histogram(fill = pal_yield, color = NA, bins = 40, alpha = 0.85) +
      geom_vline(xintercept = med, color = pal_accent,
                 linetype = "dashed", linewidth = 1.1) +
      annotate("text",
               x = med + 0.35, y = Inf, vjust = 1.8,
               label = paste0("Median: ", round(med, 2), " Mg/ha"),
               color = pal_accent, size = 3.5, fontface = "bold") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      dark_theme
  }, bg = "#16213e")

  output$yield_by_year <- renderPlot({
    d <- filt_train()
    validate(need(nrow(d) >= 5, "No data for the selected filters."))
    d_yr <- d %>%
      group_by(year) %>%
      summarise(
        med = median(yield_mg_ha, na.rm = TRUE),
        q25 = quantile(yield_mg_ha, 0.25, na.rm = TRUE),
        q75 = quantile(yield_mg_ha, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    ggplot(d_yr, aes(x = factor(year), y = med)) +
      geom_col(fill = "#9C27B0", alpha = 0.85, width = 0.65) +
      geom_errorbar(aes(ymin = q25, ymax = q75),
                    width = 0.3, color = "#e0e0e0", linewidth = 0.7) +
      labs(x = "Year", y = "Yield (Mg/ha)") +
      dark_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, bg = "#16213e")

  site_data <- reactive({
    filt_train() %>%
      group_by(site) %>%
      summarise(
        med_yield = median(yield_mg_ha, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(med_yield) %>%
      mutate(rank = row_number())
  })

  output$yield_by_site <- renderPlot({
    d <- site_data()
    validate(need(nrow(d) >= 1, "No sites to display."))
    ggplot(d, aes(x = reorder(site, med_yield),
                  y = med_yield, fill = med_yield)) +
      geom_col(alpha = 0.88, width = 0.75) +
      scale_fill_gradient(low = pal_accent, high = pal_soil,
                          guide = "none") +
      labs(x = NULL, y = "Median Yield (Mg/ha)") +
      dark_theme +
      theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7.5))
  }, bg = "#16213e")

  output$site_tooltip <- renderUI({
    hover <- input$site_hover
    if (is.null(hover)) return(NULL)
    d <- site_data()
    if (nrow(d) == 0) return(NULL)
    idx <- max(1, min(round(hover$x), nrow(d)))
    row <- d[idx, ]
    div(style = paste0(
      "position:absolute;",
      "top:",  hover$coords_css$y - 68, "px;",
      "left:", hover$coords_css$x + 14, "px;",
      "background:#0f3460;",
      "border:1px solid ", pal_accent, ";",
      "border-radius:8px; padding:8px 13px;",
      "font-size:0.8rem; pointer-events:none; z-index:999;",
      "box-shadow:0 4px 14px rgba(0,0,0,0.55);"),
      tags$strong(row$site), tags$br(),
      paste0("Median: ", round(row$med_yield, 2), " Mg/ha"), tags$br(),
      paste0("N obs: ", formatC(row$n, format = "d", big.mark = ","))
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 2 outputs
  # ════════════════════════════════════════════════════════════════════════════

  output$soil_scatter <- renderPlot({
    d  <- filt_pred()
    xv <- input$soil_var
    xl <- var_label(xv)
    d  <- d[!is.na(d[[xv]]) & !is.na(d$yield_mg_ha), ]
    validate(need(nrow(d) >= 10,
                  paste("Not enough non-NA values for", xl)))
    ggplot(d, aes(x = .data[[xv]], y = yield_mg_ha)) +
      geom_point(color = pal_soil, alpha = 0.15, size = 0.9) +
      geom_smooth(method  = "loess", formula = y ~ x,
                  se      = TRUE,
                  color   = "#ffffff",
                  fill    = "#4CAF5040",
                  linewidth = 1.1) +
      labs(x = xl, y = "Yield (Mg/ha)") +
      dark_theme
  }, bg = "#16213e")

  output$soil_hist <- renderPlot({
    d  <- filt_pred()
    xv <- input$soil_var
    xl <- var_label(xv)
    d  <- d[!is.na(d[[xv]]), ]
    validate(need(nrow(d) >= 10,
                  paste("Not enough non-NA values for", xl)))
    ggplot(d, aes(x = .data[[xv]])) +
      geom_histogram(fill = pal_soil, color = NA,
                     bins = 40, alpha = 0.85) +
      labs(x = xl, y = "Count") +
      dark_theme
  }, bg = "#16213e")

  output$wx_scatter <- renderPlot({
    d  <- filt_pred()
    xv <- input$wx_var
    xl <- var_label(xv)
    d  <- d[!is.na(d[[xv]]) & !is.na(d$yield_mg_ha), ]
    validate(need(nrow(d) >= 10,
                  paste("Not enough non-NA values for", xl)))
    ggplot(d, aes(x = .data[[xv]], y = yield_mg_ha)) +
      geom_point(color = pal_weather, alpha = 0.15, size = 0.9) +
      geom_smooth(method  = "loess", formula = y ~ x,
                  se      = TRUE,
                  color   = "#ffffff",
                  fill    = "#2196F340",
                  linewidth = 1.1) +
      labs(x = xl, y = "Yield (Mg/ha)") +
      dark_theme
  }, bg = "#16213e")

  output$prev_crop_box <- renderPlot({
    d <- filt_pred()
    d <- d[!is.na(d$prev_crop_simple) & !is.na(d$yield_mg_ha), ]
    top8 <- names(
      sort(table(d$prev_crop_simple), decreasing = TRUE)
    )[1:min(8, length(unique(d$prev_crop_simple)))]
    d <- d[d$prev_crop_simple %in% top8, ]
    validate(need(nrow(d) >= 5,
                  "Not enough data for previous crop plot."))
    ggplot(d,
           aes(x    = reorder(prev_crop_simple, yield_mg_ha,
                               FUN = median),
               y    = yield_mg_ha,
               fill = prev_crop_simple)) +
      geom_boxplot(alpha         = 0.8,
                   outlier.size  = 0.5,
                   outlier.alpha = 0.25,
                   color         = "#9e9e9e",
                   linewidth     = 0.5) +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(x = NULL, y = "Yield (Mg/ha)") +
      coord_flip() +
      dark_theme
  }, bg = "#16213e")

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 3 outputs
  # ════════════════════════════════════════════════════════════════════════════

  samp_preds <- reactive({
    n <- min(input$n_points, nrow(pred_obs))
    pred_obs[sample(nrow(pred_obs), n, replace = FALSE), ]
  })

  output$pred_obs_plot <- renderPlot({
    d   <- samp_preds()
    lim <- c(floor(min(c(d$yield_mg_ha, d$.pred), na.rm = TRUE)),
             ceiling(max(c(d$yield_mg_ha, d$.pred), na.rm = TRUE)))
    ggplot(d, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(color = pal_accent, alpha = 0.2, size = 0.8) +
      geom_abline(slope = 1, intercept = 0,
                  color     = pal_soil,
                  linetype  = "dashed",
                  linewidth = 1.2) +
      geom_smooth(method    = "lm",
                  formula   = y ~ x,
                  se        = FALSE,
                  color     = "#2196F3",
                  linewidth = 1.1) +
      annotate("text",
               x = lim[1] + 0.3, y = lim[2] - 0.3,
               hjust = 0, vjust = 1,
               label = paste0("R\u00b2 = ", r2_val,
                              "\nRMSE = ", rmse_val, " Mg/ha"),
               color = pal_soil, size = 4.5, fontface = "bold") +
      scale_x_continuous(limits = lim, expand = c(0.01, 0)) +
      scale_y_continuous(limits = lim, expand = c(0.01, 0)) +
      labs(x = "Observed Yield (Mg/ha)",
           y = "Predicted Yield (Mg/ha)") +
      dark_theme
  }, bg = "#16213e")

  output$resid_plot <- renderPlot({
    d       <- samp_preds()
    d$resid <- d$.pred - d$yield_mg_ha
    ggplot(d, aes(x = resid)) +
      geom_histogram(fill  = "#9C27B0", color = NA,
                     bins  = 50, alpha = 0.85) +
      geom_vline(xintercept = 0, color = pal_accent,
                 linetype = "dashed", linewidth = 0.9) +
      labs(x = "Residual (Mg/ha)", y = "Count") +
      dark_theme
  }, bg = "#16213e")

  output$error_by_bin <- renderPlot({
    d         <- pred_obs
    d$bin     <- cut(d$yield_mg_ha, breaks = 6, dig.lab = 2)
    d$abs_err <- abs(d$.pred - d$yield_mg_ha)
    d2 <- d %>%
      group_by(bin) %>%
      summarise(mae = mean(abs_err, na.rm = TRUE), .groups = "drop")
    ggplot(d2, aes(x = bin, y = mae, fill = mae)) +
      geom_col(alpha = 0.88) +
      scale_fill_gradient(low   = pal_soil,
                          high  = pal_accent,
                          guide = "none") +
      labs(x = "Yield Bin (Mg/ha)", y = "MAE (Mg/ha)") +
      dark_theme +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
  }, bg = "#16213e")

  # ════════════════════════════════════════════════════════════════════════════
  # TAB 4 outputs
  # ════════════════════════════════════════════════════════════════════════════

  filt_imp <- reactive({
    d <- importance
    if (!is.null(input$imp_type) && length(input$imp_type) > 0)
      d <- d[d$Type %in% input$imp_type, ]
    head(d[order(d$Gain, decreasing = TRUE), ], input$top_n)
  })

  output$imp_bar <- renderPlot({
    d <- filt_imp()
    validate(need(nrow(d) >= 1, "Please select at least one variable type."))
    ggplot(d, aes(x = reorder(Feature, Gain),
                  y = Gain_pct,
                  fill = Type)) +
      geom_col(alpha = 0.88, width = 0.72) +
      geom_text(aes(label = paste0(round(Gain_pct, 1), "%")),
                hjust = -0.1, color = "#c0c0c0", size = 3.1) +
      scale_fill_manual(
        values = c("Soil" = pal_soil, "Weather" = pal_weather),
        name   = "Type") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = NULL, y = "Importance (% of Total Gain)") +
      dark_theme +
      theme(legend.position = "bottom",
            axis.text.y    = element_text(size = 9))
  }, bg = "#16213e")

  output$imp_pie <- renderPlot({
    d <- importance %>%
      group_by(Type) %>%
      summarise(total = sum(Gain), .groups = "drop") %>%
      mutate(
        pct   = total / sum(total) * 100,
        label = paste0(Type, "\n", round(pct, 1), "%")
      )
    ggplot(d, aes(x = "", y = total, fill = Type)) +
      geom_col(width = 1, alpha = 0.88,
               color = "#16213e", linewidth = 1.5) +
      coord_polar("y") +
      geom_text(aes(label = label),
                position  = position_stack(vjust = 0.5),
                color     = "white",
                size      = 3.8,
                fontface  = "bold") +
      scale_fill_manual(
        values = c("Soil" = pal_soil, "Weather" = pal_weather)) +
      dark_theme +
      theme(axis.text      = element_blank(),
            axis.title     = element_blank(),
            panel.grid     = element_blank(),
            legend.position = "none")
  }, bg = "#16213e")

  output$imp_table <- renderTable({
    importance %>%
      arrange(desc(Gain)) %>%
      head(5) %>%
      mutate(Rank       = 1:5,
             `Gain (%)` = round(Gain_pct, 2)) %>%
      select(Rank, Feature, Type, `Gain (%)`)
  },
  striped  = FALSE,
  hover    = FALSE,
  bordered = FALSE,
  rownames = FALSE,
  width    = "100%",
  align    = "c")

}

shinyApp(ui = ui, server = server)
