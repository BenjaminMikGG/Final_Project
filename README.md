# Final_Project

Student 2 successfully cloned repository.

This is the explanation about what we do and how we do on this final project.

# The Procedure of whole process

## Getting open source data

-   weather data
    -   source：NAOO(globally, cover the site in europe)
    -   download based on lat and lon info in meta data

## Data Wrangling

-   format cleaning
-   year and site separation
-   avoid many-to-many merging problem
-   merge soil and meta to trait data

## Feature engineering

-   Using date after plant(pad) to calculate the stage of growth
-   GDD for corn
-   Sum of percipitation,radiation,tmean/min/max
-   Calculate every feature based on stage of growth

## Modeling Strategy

Data engineering approach: GDD-based weather aggregation (mean/sum, stage-based) + target encoding

Number of predictors: 82 (after preprocessing)

Data split proportion: 70/30

Random or stratified: Stratified

If stratified, by which variable: Yield

Pre-processing steps: Dummy encoding, novel level handling, zero variance removal, median imputation

Hyperparameters tuned: mtry, trees, tree_depth, min_n, learn_rate, loss_reduction, sample_size

Search algorithm: ANOVA racing (tune_race_anova)

V value: 10

Metric used: R² (rsq)

Type of best: Overall \## Training Strategy

-   predict by year site hybrid previous soil environment weather(open source)

## Communication with shiny app

<https://igyawali.shinyapps.io/Corn/>

# Workload Splitting

Ming:\
Open source data\
Data wrangling\
Feature engineering

Ishwari: Modeling Strategy Training Strategy Shiny app making
