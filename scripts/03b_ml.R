# Step 3: Estimate logit models
# Purpose:  
# Inputs:   
# Outputs:

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(modelsummary)
library(pROC)

# 2 Load Panel ===============================================================

panel <- read_rds("data/final/panel.rds")

# 3 Set Theme ================================================================

theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "serif", size = 12),
      plot.title = element_blank(),
      axis.text = element_text(size = 12, colour = "black"),
      axis.line = element_line(linewidth = 0.5),
      axis.ticks = element_line(linewidth = 0.5),
      panel.grid = element_line(linetype = "dashed")
    )
)

par(family = "serif")


# 4 Preprocessing ===========================================================

# Explanatory variables
x_vars <- panel |> select(advanced:sprr, - c(nfagdp, cgdpcorp, cgdph, tlcorp_rgrowth, tlh_rgrowth, ycurve, ppgrowth)) |> colnames()

panel_ml <- panel |> 
  # Remove crisis years
  filter(crisis != 1) |> 
  # Remove unnecessary columns
  select(iso3c, year, precrisis3, all_of(x_vars)) |> 
  # Convert precrisis variable into a factor
  mutate(precrisis3 = factor(precrisis3, levels = c(0, 1))) |> 
  # Drop NAs
  drop_na()



# 4 Estimate Machine Learning Models =========================================

train <- panel_ml |>
  filter(year <= 2004)

# 4.1 Random Forests =========================================================

rec_rf <- recipe(precrisis3 ~ ., data = panel_ml) |>
  step_rm(iso3c, year)

mod_rf <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
  ) |>
  set_engine(
    engine = "ranger",
    importance = "permutation"
  ) |>
  set_mode("classification")

wf_rf <- workflow() |>
  add_recipe(rec_rf) |>
  add_model(mod_rf)

# Hyperparameter tuning
cv_periods <- tibble(
  id = paste0("Fold", 1:14),
  analysis_start = 1970,
  analysis_end = 1990:2003,
  assessment_start = 1991:2004,
  assessment_end = 1991:2004
  )

make_split <- function(data, analysis_start, analysis_end, assessment_start, assessment_end) {
  
  make_splits(
    x = list(
      analysis = which(data$year >= analysis_start & data$year <= analysis_end),
      assessment = which(data$year >= assessment_start & data$year <= assessment_end)
      ),
    data = data
  )
  }

folds <- manual_rset(
  splits = pmap(
    list(
      cv_periods$analysis_start,
      cv_periods$analysis_end,
      cv_periods$assessment_start,
      cv_periods$assessment_end
    ),
    ~ make_split(
      train,
      ..1, ..2, ..3, ..4
    )
  ),
  ids = cv_periods$id
)

# Number of predictors
n_predictors <- train |>
  select(-precrisis3, -iso3c, -year) |>
  ncol()

# Create the grid
set.seed(123)

rf_grid <- grid_random(
  mtry(range = c(2L, n_predictors)),
  min_n(range = c(2L, 30L)),
  size = 30
)

# Tune Hyperparameters
rf_tuned <- tune_grid(
  wf_rf,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc),
  control = control_grid(save_pred = TRUE, verbose = TRUE)
)

show_best(rf_tuned, metric = "roc_auc")

best_rf <- select_best(rf_tuned, metric = "roc_auc")

# Actual pseudo oos forecasting

final_wf <- finalize_workflow(wf_rf, best_rf)

forecast_years <- 2005:2022

rf_oos_results <- map_dfr(
  forecast_years,
  function(y) {
    
    train_data <- panel_ml |>
      filter(.data$year >= 1970, .data$year <= .env$y - 1)
    
    test_data <- panel_ml |>
      filter(.data$year == .env$y)
    
    fit <- final_wf |>
      fit(data = train_data)
    
    predictions <- predict(
      fit,
      new_data = test_data,
      type = "prob"
    )
    
    test_data |>
      select(iso3c, year, precrisis3) |>
      bind_cols(predictions)
  }
)


# Evaluate
roc_auc(
  rf_oos_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
)

rf_oos_results |>
  mutate(
    precrisis3 = factor(precrisis3, levels = c(0, 1))
  ) |>
  roc_curve(
    truth = precrisis3,
    .pred_1,
    event_level = "second"
  ) |>
  autoplot()


# 4.2 Support Vector Machine ================================================

# SVM recipe

rec_svm <- recipe(precrisis3 ~ ., data = panel_ml) |>
  step_rm(iso3c, year) |>
  step_normalize(all_numeric_predictors())

# SVM model

mod_svm <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
  ) |>
  set_engine("kernlab") |>
  set_mode("classification")


# Workflow

wf_svm <- workflow() |>
  add_recipe(rec_svm) |>
  add_model(mod_svm)


# Hyperparameter grid

set.seed(123)

svm_grid <- grid_regular(
  cost(range = c(-3, 3)),
  rbf_sigma(range = c(-3, 0)),
  levels = 7
  )


# Tune

svm_tuned <- tune_grid(
  wf_svm,
  resamples = folds,
  grid = svm_grid,
  metrics = metric_set(roc_auc),
  control = control_grid(save_pred = T, verbose = T)
  )


# Results

show_best(
  svm_tuned,
  metric = "roc_auc"
  )

# Select best model

best_svm <- select_best(
  svm_tuned,
  metric = "roc_auc"
  )


# Final model

final_svm_wf <- finalize_workflow(
  wf_svm,
  best_svm
  )

# Out-of-sample predictions

svm_oos_results <- map_dfr(
  forecast_years,
  function(y) {
    
    train_data <- panel_ml |>
      filter(.data$year >= 1970, .data$year <= .env$y - 1)
    
    test_data <- panel_ml |>
      filter(.data$year == .env$y)
    
    fit <- final_svm_wf |>
      fit(data = train_data)
    
    predictions <- predict(
      fit,
      new_data = test_data,
      type = "prob"
    )
    
    test_data |>
      select(iso3c, year, precrisis3) |>
      bind_cols(predictions)
  }
)


# Evaluation

roc_auc(
  svm_oos_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
  )

# ROC curve

svm_oos_results |>
  mutate(
    precrisis3 = factor(
      precrisis3,
      levels = c(0, 1)
    )
  ) |>
  roc_curve(
    truth = precrisis3,
    .pred_1,
    event_level = "second"
  ) |>
  autoplot()






prep_svm <- prep(rec_svm)

train_processed <- bake(
  prep_svm,
  new_data = NULL
)

summary(train_processed)






