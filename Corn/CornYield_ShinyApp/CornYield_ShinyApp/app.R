library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ── Load Data ──────────────────────────────────────────────────────────────────
train      <- read.csv("data/train_eda.csv",      stringsAsFactors = FALSE)
pred_obs   <- read.csv("data/pred_obs.csv",        stringsAsFactors = FALSE)
importance <- read.csv("data/importance.csv",      stringsAsFactors = FALSE)

# Classify features
soil_feats    <- c("soilp_h","soilk_ppm","soilp_ppm","om_pct")
importance$Type <- ifelse(importance$Feature %in% soil_feats, "Soil", "Weather")
importance$Gain_pct <- importance$Gain / sum(importance$Gain) * 100

# Compute overall R2 & RMSE from full dataset
r2_val   <- round(1 - sum((pred_obs$yield_mg_ha - pred_obs$.pred)^2) /
                      sum((pred_obs$yield_mg_ha - mean(pred_obs$yield_mg_ha))^2), 4)
rmse_val <- round(sqrt(mean((pred_obs$yield_mg_ha - pred_obs$.pred)^2)), 4)

# Colour palette
pal_soil    <- "#4CAF50"
pal_weather <- "#2196F3"
pal_yield   <- "#FF9800"
pal_bg      <- "#1a1a2e"
pal_card    <- "#16213e"
pal_accent  <- "#e94560"
pal_text    <- "#e0e0e0"
pal_sub     <- "#9e9e9e"

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Corn Yield XGBoost Dashboard",

  tags$head(tags$style(HTML(paste0("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
    * { box-sizing: border-box; }
    body {
      background: ", pal_bg, ";
      color: ", pal_text, ";
      font-family: 'Inter', sans-serif;
      margin: 0; padding: 0;
    }
    .navbar-top {
      background: linear-gradient(90deg, #0f3460 0%, #16213e 100%);
      padding: 18px 32px;
      border-bottom: 2px solid ", pal_accent, ";
      display: flex; align-items: center; gap: 16px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.5);
    }
    .navbar-top h1 {
      margin: 0; font-size: 1.5rem; font-weight: 700; color: #fff;
      letter-spacing: 0.5px;
    }
    .navbar-top .badge {
      background: ", pal_accent, "; color: #fff;
      padding: 4px 10px; border-radius: 12px; font-size: 0.72rem; font-weight: 600;
    }
    .tab-header {
      background: #0f3460;
      padding: 0 32px;
      display: flex; gap: 0;
      border-bottom: 1px solid #263056;
    }
    .nav-tabs { border-bottom: none !important; }
    .nav-tabs > li > a {
      color: ", pal_sub, " !important;
      border: none !important; border-radius: 0 !important;
      padding: 14px 22px !important;
      font-size: 0.88rem; font-weight: 500;
      transition: all 0.2s;
      background: transparent !important;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li > a:hover {
      color: #fff !important;
      border-bottom: 3px solid ", pal_accent, " !important;
      background: transparent !important;
    }
    .tab-content { padding: 28px 32px; }
    .card {
      background: ", pal_card, ";
      border-radius: 12px;
      padding: 22px 24px;
      margin-bottom: 20px;
      box-shadow: 0 4px 16px rgba(0,0,0,0.3);
    }
    .card-title {
      font-size: 0.95rem; font-weight: 600;
      color: ", pal_text, "; margin-bottom: 14px;
      padding-bottom: 10px;
      border-bottom: 1px solid #263056;
      display: flex; align-items: center; gap: 8px;
    }
    .card-title .dot {
      width: 10px; height: 10px; border-radius: 50%; display: inline-block;
    }
    .metric-row { display: flex; gap: 16px; margin-bottom: 20px; }
    .metric-box {
      flex: 1;
      background: ", pal_card, ";
      border-radius: 10px; padding: 18px 20px;
      box-shadow: 0 4px 16px rgba(0,0,0,0.3);
      border-left: 4px solid ", pal_accent, ";
    }
    .metric-label { font-size: 0.78rem; color:", pal_sub,"; text-transform: uppercase; letter-spacing: 1px; }
    .metric-value { font-size: 2rem; font-weight: 700; color: #fff; margin-top: 4px; }
    .metric-sub   { font-size: 0.78rem; color:", pal_sub,"; margin-top: 2px; }
    .ctrl-panel {
      background: ", pal_card, ";
      border-radius: 12px; padding: 18px 20px;
      margin-bottom: 20px;
      box-shadow: 0 4px 16px rgba(0,0,0,0.3);
    }
    .ctrl-panel label { color:", pal_sub,"; font-size:0.82rem; text-transform:uppercase; letter-spacing:1px; }
    .form-control, .selectize-input {
      background: #0f3460 !important; color: ", pal_text, " !important;
      border: 1px solid #263056 !important; border-radius: 8px !important;
    }
    .irs-bar, .irs-bar-edge { background: ", pal_accent, " !important; border-color: ", pal_accent, " !important; }
    .irs-handle { background: ", pal_accent, " !important; border-color: ", pal_accent, " !important; }
    .irs-from, .irs-to, .irs-single { background: ", pal_accent, " !important; }
    .irs-grid-text { color: ", pal_sub, " !important; }
    .irs-line { background: #263056 !important; }
    .section-desc { color: ", pal_sub, "; font-size: 0.83rem; margin-bottom: 16px; }
    hr.divider { border-color: #263056; margin: 20px 0; }
    .shiny-plot-output { border-radius: 8px; overflow: hidden; }
  ")))),

  # ── Top Nav ──
  div(class = "navbar-top",
    div(style="font-size:1.6rem;", "🌽"),
    h1("Corn Yield Prediction – XGBoost Dashboard"),
    span(class = "badge", "R² = 0.6536"),
    span(class = "badge", style="background:#4CAF50;", "RMSE = 1.8177 Mg/ha")
  ),

  # ── Tab Nav ──
  navbarPage("", id = "tabs",
    header = NULL,
    windowTitle = "Yield Dashboard",

    # ═══ TAB 1 : YIELD EDA ════════════════════════════════════════════════════
    tabPanel("📊 Yield EDA",
      div(class = "tab-content",

        # KPI row
        div(class = "metric-row",
          div(class = "metric-box",
            div(class = "metric-label", "Training Observations"),
            div(class = "metric-value", formatC(nrow(train), format="d", big.mark=",")),
            div(class = "metric-sub", paste(length(unique(train$site)), "sites ·", length(unique(train$year)), "years"))
          ),
          div(class = "metric-box", style="border-left-color:#4CAF50;",
            div(class = "metric-label", "Median Yield"),
            div(class = "metric-value", paste0(round(median(train$yield_mg_ha),2))),
            div(class = "metric-sub", "Mg/ha")
          ),
          div(class = "metric-box", style="border-left-color:#2196F3;",
            div(class = "metric-label", "Yield Range"),
            div(class = "metric-value", paste0(round(min(train$yield_mg_ha),1), " – ", round(max(train$yield_mg_ha),1))),
            div(class = "metric-sub", "Mg/ha")
          ),
          div(class = "metric-box", style="border-left-color:#9C27B0;",
            div(class = "metric-label", "Std Deviation"),
            div(class = "metric-value", round(sd(train$yield_mg_ha), 2)),
            div(class = "metric-sub", "Mg/ha")
          )
        ),

        fluidRow(
          # Controls
          column(3,
            div(class = "ctrl-panel",
              div(class = "card-title", span(class="dot", style="background:#e94560;"), "Filter Options"),
              selectInput("eda_year", "Year",
                          choices = c("All", sort(unique(train$year))), selected = "All"),
              selectInput("eda_site", "Site",
                          choices = c("All", sort(unique(train$site))), selected = "All"),
              hr(class = "divider"),
              p(class = "section-desc",
                "Explore the distribution of corn yield across years and sites. Use the filters to drill into specific subsets.")
            )
          ),

          column(9,
            fluidRow(
              column(6,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style=paste0("background:",pal_yield,";")),
                      "Yield Distribution"),
                  plotOutput("yield_hist", height = "280px")
                )
              ),
              column(6,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#9C27B0;"),
                      "Yield by Year"),
                  plotOutput("yield_by_year", height = "280px")
                )
              )
            ),
            fluidRow(
              column(12,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#2196F3;"),
                      "Median Yield by Site (Interactive – hover for details)"),
                  plotOutput("yield_by_site", height = "260px",
                             hover = hoverOpts("site_hover", delay=100, delayType="debounce")),
                  uiOutput("site_tooltip")
                )
              )
            )
          )
        )
      )
    ),

    # ═══ TAB 2 : PREDICTOR EDA ════════════════════════════════════════════════
    tabPanel("🔬 Predictor EDA",
      div(class = "tab-content",

        fluidRow(
          column(3,
            div(class = "ctrl-panel",
              div(class = "card-title", span(class="dot", style="background:#4CAF50;"), "Soil Variables"),
              selectInput("soil_var", "Select Soil Variable",
                choices = c(
                  "Soil pH" = "soilp_h",
                  "Potassium (ppm)" = "soilk_ppm",
                  "Phosphorus (ppm)" = "soilp_ppm",
                  "Organic Matter (%)" = "om_pct"
                )
              ),
              hr(class="divider"),
              div(class = "card-title", span(class="dot", style="background:#2196F3;"), "Weather Variables"),
              selectInput("wx_var", "Select Weather Variable",
                choices = c(
                  "Total GDD" = "total_gdd",
                  "Total Precipitation (mm)" = "total_prcp",
                  "Avg Temp (°C)" = "avg_t2m",
                  "GDD – Early Season" = "gdd_early",
                  "GDD – Mid Season" = "gdd_mid",
                  "GDD – Late Season" = "gdd_late",
                  "Precip – Early" = "prcp_early",
                  "Precip – Mid" = "prcp_mid",
                  "Precip – Late" = "prcp_late"
                )
              ),
              hr(class="divider"),
              selectInput("prev_year", "Filter Year",
                          choices = c("All", sort(unique(train$year))), selected = "All")
            )
          ),

          column(9,
            fluidRow(
              column(6,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#4CAF50;"),
                      "Soil Variable vs. Yield"),
                  plotOutput("soil_scatter", height = "280px")
                )
              ),
              column(6,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#4CAF50;"),
                      "Soil Variable Distribution"),
                  plotOutput("soil_hist", height = "280px")
                )
              )
            ),
            fluidRow(
              column(6,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#2196F3;"),
                      "Weather Variable vs. Yield"),
                  plotOutput("wx_scatter", height = "280px")
                )
              ),
              column(6,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#2196F3;"),
                      "Previous Crop – Yield Boxplot"),
                  plotOutput("prev_crop_box", height = "280px")
                )
              )
            )
          )
        )
      )
    ),

    # ═══ TAB 3 : MODEL PERFORMANCE ════════════════════════════════════════════
    tabPanel("🎯 Model Performance",
      div(class = "tab-content",

        # KPI row
        div(class = "metric-row",
          div(class = "metric-box",
            div(class = "metric-label", "R² (Cross-Validation)"),
            div(class = "metric-value", r2_val),
            div(class = "metric-sub", "Proportion of variance explained")
          ),
          div(class = "metric-box", style="border-left-color:#4CAF50;",
            div(class = "metric-label", "RMSE"),
            div(class = "metric-value", rmse_val),
            div(class = "metric-sub", "Mg/ha – root mean squared error")
          ),
          div(class = "metric-box", style="border-left-color:#2196F3;",
            div(class = "metric-label", "Observations"),
            div(class = "metric-value", formatC(nrow(pred_obs), format="d", big.mark=",")),
            div(class = "metric-sub", "CV fold predictions")
          ),
          div(class = "metric-box", style="border-left-color:#9C27B0;",
            div(class = "metric-label", "Best Model"),
            div(class = "metric-value", style="font-size:1.1rem; margin-top:8px;", "XGBoost"),
            div(class = "metric-sub", "depth=7 · trees=700 · lr=0.046")
          )
        ),

        fluidRow(
          column(8,
            div(class = "card",
              div(class = "card-title", span(class="dot", style="background:#e94560;"),
                  "Predicted vs. Observed Yield"),
              div(class = "section-desc",
                  paste0("N = ", formatC(nrow(pred_obs), format="d", big.mark=","),
                         " CV predictions  ·  R² = ", r2_val, "  ·  RMSE = ", rmse_val, " Mg/ha")),
              sliderInput("n_points", "Points to display (sample):",
                          min = 1000, max = min(nrow(pred_obs), 30000),
                          value = 10000, step = 1000, width = "100%"),
              plotOutput("pred_obs_plot", height = "420px")
            )
          ),
          column(4,
            div(class = "card",
              div(class = "card-title", span(class="dot", style="background:#FF9800;"),
                  "Residual Distribution"),
              plotOutput("resid_plot", height = "200px")
            ),
            div(class = "card",
              div(class = "card-title", span(class="dot", style="background:#9C27B0;"),
                  "Error by Yield Bin"),
              plotOutput("error_by_bin", height = "200px")
            )
          )
        )
      )
    ),

    # ═══ TAB 4 : VARIABLE IMPORTANCE ══════════════════════════════════════════
    tabPanel("⭐ Variable Importance",
      div(class = "tab-content",

        fluidRow(
          column(3,
            div(class = "ctrl-panel",
              div(class = "card-title", span(class="dot", style="background:#e94560;"), "Controls"),
              sliderInput("top_n", "Top N Variables:",
                          min = 5, max = 30, value = 20, step = 1),
              hr(class="divider"),
              checkboxGroupInput("imp_type", "Variable Type:",
                choices = c("Weather" = "Weather", "Soil" = "Soil"),
                selected = c("Weather", "Soil")
              ),
              hr(class="divider"),
              div(class="section-desc",
                "Feature importance measured by",
                tags$b("gain"), "– the average reduction in loss brought by a feature across all splits.",
                br(), br(),
                tags$span(style="color:#2196F3;", "■"), " Weather variables",
                br(),
                tags$span(style="color:#4CAF50;", "■"), " Soil variables"
              )
            )
          ),

          column(9,
            fluidRow(
              column(8,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#e94560;"),
                      "Top Predictors by Importance (Gain)"),
                  plotOutput("imp_bar", height = "520px")
                )
              ),
              column(4,
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#2196F3;"),
                      "Importance by Variable Type"),
                  plotOutput("imp_pie", height = "240px")
                ),
                div(class = "card",
                  div(class = "card-title", span(class="dot", style="background:#4CAF50;"),
                      "Top 5 Features"),
                  tableOutput("imp_table")
                )
              )
            )
          )
        )
      )
    )
  )
)


# ── Server ─────────────────────────────────────────────────────────────────────
dark_theme <- theme_minimal(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "#16213e", color = NA),
    panel.background = element_rect(fill = "#16213e", color = NA),
    panel.grid.major = element_line(color = "#263056", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#9e9e9e", size = 9),
    axis.title       = element_text(color = "#c0c0c0", size = 10),
    plot.title       = element_text(color = "#e0e0e0", size = 11, face = "bold"),
    plot.subtitle    = element_text(color = "#9e9e9e", size = 9),
    legend.background = element_rect(fill = "#16213e", color = NA),
    legend.text      = element_text(color = "#9e9e9e"),
    legend.title     = element_text(color = "#c0c0c0"),
    strip.text       = element_text(color = "#c0c0c0")
  )

server <- function(input, output, session) {

  # ── Reactive filtered train data ──
  filt_train <- reactive({
    d <- train
    if (input$eda_year != "All") d <- d[d$year == as.integer(input$eda_year), ]
    if (input$eda_site != "All") d <- d[d$site == input$eda_site, ]
    d
  })

  filt_pred_train <- reactive({
    d <- train
    if (input$prev_year != "All") d <- d[d$year == as.integer(input$prev_year), ]
    d
  })

  # ── TAB 1 ──────────────────────────────────────────────────────────────────
  output$yield_hist <- renderPlot({
    d <- filt_train()
    med <- median(d$yield_mg_ha, na.rm=TRUE)
    ggplot(d, aes(x = yield_mg_ha)) +
      geom_histogram(fill = pal_yield, color = NA, bins = 50, alpha = 0.85) +
      geom_vline(xintercept = med, color = "#e94560", linetype = "dashed", linewidth = 1) +
      annotate("text", x = med + 0.5, y = Inf, vjust = 2,
               label = paste0("Median: ", round(med,2)), color = "#e94560", size = 3.2) +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      dark_theme
  }, bg = "#16213e")

  output$yield_by_year <- renderPlot({
    d <- filt_train()
    d_yr <- d %>%
      group_by(year) %>%
      summarise(med = median(yield_mg_ha, na.rm=TRUE),
                q25 = quantile(yield_mg_ha, 0.25, na.rm=TRUE),
                q75 = quantile(yield_mg_ha, 0.75, na.rm=TRUE))
    ggplot(d_yr, aes(x = factor(year), y = med)) +
      geom_col(fill = "#9C27B0", alpha = 0.8, width = 0.65) +
      geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.3, color = "#e0e0e0", linewidth = 0.7) +
      labs(x = "Year", y = "Yield (Mg/ha)") +
      dark_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, bg = "#16213e")

  # site plot data stored for tooltip
  site_plot_data <- reactive({
    d <- filt_train()
    d %>%
      group_by(site) %>%
      summarise(med_yield = median(yield_mg_ha, na.rm=TRUE),
                n = n()) %>%
      arrange(med_yield)
  })

  output$yield_by_site <- renderPlot({
    d <- site_plot_data()
    ggplot(d, aes(x = reorder(site, med_yield), y = med_yield, fill = med_yield)) +
      geom_col(alpha = 0.88, width = 0.75) +
      scale_fill_gradient(low = "#e94560", high = "#4CAF50", guide = "none") +
      labs(x = "Site", y = "Median Yield (Mg/ha)") +
      dark_theme +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7.5))
  }, bg = "#16213e")

  output$site_tooltip <- renderUI({
    hover <- input$site_hover
    if (is.null(hover)) return(NULL)
    d <- site_plot_data()
    nearest <- d[which.min(abs(seq_len(nrow(d)) - (hover$x))), ]
    if (nrow(nearest) == 0) return(NULL)
    div(style = paste0(
      "position:absolute; top:", hover$coords_css$y - 60, "px;",
      "left:", hover$coords_css$x + 10, "px;",
      "background:#0f3460; border:1px solid #e94560; border-radius:8px;",
      "padding:8px 12px; font-size:0.82rem; pointer-events:none; z-index:999;"),
      strong(nearest$site),
      br(),
      paste0("Median: ", round(nearest$med_yield,2), " Mg/ha"),
      br(),
      paste0("N obs: ", nearest$n)
    )
  })

  # ── TAB 2 ──────────────────────────────────────────────────────────────────
  output$soil_scatter <- renderPlot({
    d <- filt_pred_train()
    xv <- input$soil_var
    xl <- switch(xv,
      soilp_h = "Soil pH", soilk_ppm = "Potassium (ppm)",
      soilp_ppm = "Phosphorus (ppm)", om_pct = "Organic Matter (%)")
    ggplot(d, aes_string(x = xv, y = "yield_mg_ha")) +
      geom_point(color = pal_soil, alpha = 0.12, size = 0.8) +
      geom_smooth(method = "loess", se = TRUE, color = "#fff", fill = "#4CAF5033", linewidth = 1) +
      labs(x = xl, y = "Yield (Mg/ha)") +
      dark_theme
  }, bg = "#16213e")

  output$soil_hist <- renderPlot({
    d <- filt_pred_train()
    xv <- input$soil_var
    xl <- switch(xv,
      soilp_h = "Soil pH", soilk_ppm = "Potassium (ppm)",
      soilp_ppm = "Phosphorus (ppm)", om_pct = "Organic Matter (%)")
    ggplot(d, aes_string(x = xv)) +
      geom_histogram(fill = pal_soil, color = NA, bins = 40, alpha = 0.85) +
      labs(x = xl, y = "Count") +
      dark_theme
  }, bg = "#16213e")

  output$wx_scatter <- renderPlot({
    d <- filt_pred_train()
    xv <- input$wx_var
    xl <- switch(xv,
      total_gdd = "Total GDD", total_prcp = "Total Precip (mm)",
      avg_t2m = "Avg Temp (°C)", gdd_early = "GDD Early",
      gdd_mid = "GDD Mid", gdd_late = "GDD Late",
      prcp_early = "Precip Early", prcp_mid = "Precip Mid", prcp_late = "Precip Late")
    ggplot(d, aes_string(x = xv, y = "yield_mg_ha")) +
      geom_point(color = pal_weather, alpha = 0.12, size = 0.8) +
      geom_smooth(method = "loess", se = TRUE, color = "#fff", fill = "#2196F333", linewidth = 1) +
      labs(x = xl, y = "Yield (Mg/ha)") +
      dark_theme
  }, bg = "#16213e")

  output$prev_crop_box <- renderPlot({
    d <- filt_pred_train()
    # Keep top categories
    top_crops <- names(sort(table(d$prev_crop_simple), decreasing=TRUE)[1:8])
    d2 <- d[d$prev_crop_simple %in% top_crops, ]
    ggplot(d2, aes(x = reorder(prev_crop_simple, yield_mg_ha, median),
                   y = yield_mg_ha, fill = prev_crop_simple)) +
      geom_boxplot(alpha = 0.8, outlier.size = 0.5, outlier.alpha = 0.3, color = "#9e9e9e") +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(x = "Previous Crop", y = "Yield (Mg/ha)") +
      coord_flip() +
      dark_theme
  }, bg = "#16213e")

  # ── TAB 3 ──────────────────────────────────────────────────────────────────
  sampled_preds <- reactive({
    n <- min(input$n_points, nrow(pred_obs))
    pred_obs[sample(nrow(pred_obs), n), ]
  })

  output$pred_obs_plot <- renderPlot({
    d <- sampled_preds()
    lim <- range(c(d$yield_mg_ha, d$.pred), na.rm=TRUE)
    ggplot(d, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(color = pal_accent, alpha = 0.25, size = 0.9) +
      geom_abline(slope = 1, intercept = 0, color = "#4CAF50", linetype = "dashed", linewidth = 1.2) +
      geom_smooth(method = "lm", se = FALSE, color = "#2196F3", linewidth = 1) +
      annotate("text", x = lim[1] + 0.5, y = lim[2] - 0.5, hjust = 0, vjust = 1,
               label = paste0("R² = ", r2_val, "\nRMSE = ", rmse_val, " Mg/ha"),
               color = "#4CAF50", size = 4.2, fontface = "bold") +
      scale_x_continuous(limits = lim) +
      scale_y_continuous(limits = lim) +
      labs(x = "Observed Yield (Mg/ha)", y = "Predicted Yield (Mg/ha)",
           subtitle = paste0("n = ", formatC(input$n_points, format="d", big.mark=","),
                             " sampled  ·  green dashed = perfect prediction  ·  blue = fitted line")) +
      dark_theme +
      theme(plot.subtitle = element_text(size = 8))
  }, bg = "#16213e")

  output$resid_plot <- renderPlot({
    d <- sampled_preds()
    d$resid <- d$.pred - d$yield_mg_ha
    ggplot(d, aes(x = resid)) +
      geom_histogram(fill = "#9C27B0", color = NA, bins = 50, alpha = 0.85) +
      geom_vline(xintercept = 0, color = "#e94560", linetype = "dashed", linewidth = 0.9) +
      labs(x = "Residual (Mg/ha)", y = "Count") +
      dark_theme
  }, bg = "#16213e")

  output$error_by_bin <- renderPlot({
    d <- pred_obs
    d$bin <- cut(d$yield_mg_ha, breaks = 6, dig.lab = 2)
    d$abs_err <- abs(d$.pred - d$yield_mg_ha)
    d2 <- d %>% group_by(bin) %>%
      summarise(mae = mean(abs_err, na.rm=TRUE))
    ggplot(d2, aes(x = bin, y = mae, fill = mae)) +
      geom_col(alpha = 0.85) +
      scale_fill_gradient(low = "#4CAF50", high = "#e94560", guide = "none") +
      labs(x = "Yield Bin (Mg/ha)", y = "MAE (Mg/ha)") +
      dark_theme +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
  }, bg = "#16213e")

  # ── TAB 4 ──────────────────────────────────────────────────────────────────
  filt_imp <- reactive({
    importance %>%
      filter(Type %in% input$imp_type) %>%
      arrange(desc(Gain)) %>%
      head(input$top_n)
  })

  output$imp_bar <- renderPlot({
    d <- filt_imp()
    d$col <- ifelse(d$Type == "Soil", pal_soil, pal_weather)
    ggplot(d, aes(x = reorder(Feature, Gain), y = Gain_pct, fill = Type)) +
      geom_col(alpha = 0.88, width = 0.72) +
      geom_text(aes(label = paste0(round(Gain_pct, 1), "%")),
                hjust = -0.1, color = "#c0c0c0", size = 3) +
      scale_fill_manual(values = c("Soil" = pal_soil, "Weather" = pal_weather),
                        name = "Type") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Importance (% of Total Gain)") +
      dark_theme +
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 9))
  }, bg = "#16213e")

  output$imp_pie <- renderPlot({
    d <- importance %>%
      group_by(Type) %>%
      summarise(total_gain = sum(Gain)) %>%
      mutate(pct = total_gain / sum(total_gain) * 100,
             label = paste0(Type, "\n", round(pct,1), "%"))
    ggplot(d, aes(x = "", y = total_gain, fill = Type)) +
      geom_col(width = 1, alpha = 0.88, color = "#16213e", linewidth = 1.5) +
      coord_polar("y") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5),
                color = "white", size = 3.5, fontface = "bold") +
      scale_fill_manual(values = c("Soil" = pal_soil, "Weather" = pal_weather)) +
      dark_theme +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(), legend.position = "none")
  }, bg = "#16213e")

  output$imp_table <- renderTable({
    importance %>%
      arrange(desc(Gain)) %>%
      head(5) %>%
      mutate(Rank = 1:5,
             `Importance (%)` = round(Gain_pct, 2)) %>%
      select(Rank, Feature, Type, `Importance (%)`)
  },
  striped = FALSE, hover = FALSE, bordered = FALSE,
  rownames = FALSE,
  width = "100%"
  )
}

shinyApp(ui = ui, server = server)
