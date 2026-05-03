# Corn Yield XGBoost Dashboard — Shiny App

## Requirements
Install the following R packages before running:
```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "scales"))
```

## How to Run
```r
shiny::runApp("path/to/CornYield_ShinyApp")
```

## Files
- `app.R`          — full Shiny UI + server code
- `data/train_eda.csv`     — 20k-row sample of training data for EDA plots
- `data/pred_obs.csv`      — 115k cross-validation predicted vs observed values
- `data/importance.csv`    — XGBoost feature importance (gain) for 76 predictors

## Tabs
1. **Yield EDA**         — yield distribution, by-year bar chart, by-site interactive bar
2. **Predictor EDA**     — soil & weather variable scatterplots vs yield, previous crop boxplot
3. **Model Performance** — predicted vs observed (R²=0.6536, RMSE=1.8177), residuals, error by yield bin
4. **Variable Importance** — top-N feature importance bar chart, soil vs weather pie chart
