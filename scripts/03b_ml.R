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
x_vars <- colnames(panel)[-(1:8)]

panel_ml <- panel |> 
  # Remove crisis years
  filter(crisis != 1) |> 
  # Remove unnecessary columns
  select(iso3c, year, precrisis3, all_of(x_vars)) |> 
  # Convert precrisis variable into a factor
  mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))



# 4 Estimate Machine Learning Models =========================================

# 4.1 Random Forests =========================================================

# Train / test split

train <- panel_ml |>
  filter(year <= 2006)

test <- panel_ml |>
  filter(year >= 2007, year <= 2022)


# Random Forest recipe

rec_rf <- recipe(precrisis3 ~ ., data = train) |>
  step_rm(iso3c, year) |>
  step_impute_median(all_numeric_predictors()) |>
  step_zv(all_predictors())

# Random Forest model

mod_rf <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
  ) |>
  set_engine(
    "ranger",
    importance = "permutation"
  ) |>
  set_mode("classification")

# Workflow

wf_rf <- workflow() |>
  add_recipe(rec_rf) |>
  add_model(mod_rf)


# Define expanding / rolling window

cv_periods <- tibble(
  id = paste0("Fold", 1:4),
  analysis_start = c(1970, 1970, 1970, 1970),
  analysis_end   = c(1989, 1994, 1999, 2001),
  assessment_start = c(1990, 1995, 2000, 2002),
  assessment_end   = c(1994, 1999, 2004, 2006)
)

make_split <- function(data, analysis_start, analysis_end, assessment_start, assessment_end) {
  
  analysis <- data |>
    filter(
      year >= analysis_start,
      year <= analysis_end
    )
  
  assessment <- data |>
    filter(
      year >= assessment_start,
      year <= assessment_end
    )
  
  make_splits(
    x = list(
      analysis = which(
        data$year >= analysis_start &
          data$year <= analysis_end
      ),
      assessment = which(
        data$year >= assessment_start &
          data$year <= assessment_end
      )
    ),
    data = data
  )
}

folds <- manual_rset(
  splits = cv_periods |>
    mutate(
      splits = pmap(
        list(
          analysis_start,
          analysis_end,
          assessment_start,
          assessment_end
        ),
        ~ make_split(
          train,
          ..1, ..2, ..3, ..4
        )
      )
    ) |>
    pull(splits),
  
  ids = cv_periods$id
)


# Verify

analysis(folds$splits[[1]]) |>
  summarise(
    min_year = min(year),
    max_year = max(year),
    n = n()
  )

assessment(folds$splits[[1]]) |>
  summarise(
    min_year = min(year),
    max_year = max(year),
    n = n()
  )


n_predictors <- train |>
  select(-precrisis3, -iso3c, -year) |>
  ncol()

set.seed(123)

rf_grid <- grid_random(
  mtry(range = c(2L, n_predictors)),
  min_n(range = c(2L, 30L)),
  size = 30
)


rf_tuned <- tune_grid(
  wf_rf,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc),
  control = control_grid(
    save_pred = T,
    verbose = T
  )
)


collect_metrics(rf_tuned)

show_best(
  rf_tuned,
  metric = "roc_auc",
  n = 10
)

best_rf <- select_best(
  rf_tuned,
  metric = "roc_auc"
)


final_wf <- finalize_workflow(wf_rf, best_rf)

final_rf_fit <- fit(
  final_wf,
  data = train
)

rf_test_pred <- predict(
  final_rf_fit,
  test,
  type = "prob"
)

rf_results <- test |>
  select(iso3c, year, precrisis3) |>
  bind_cols(rf_test_pred)

roc_auc(
  rf_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
)


# Plot curves

roc_curve(
  rf_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
) |>
  autoplot()



# 4.2 Support Vector Machine ================================================

# SVM recipe

rec_svm <- recipe(precrisis3 ~ ., data = train) |>
  step_rm(iso3c, year) |>
  step_impute_median(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
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

svm_grid <- grid_random(
  cost(range = c(-3, 3)),
  rbf_sigma(range = c(-4, 1)),
  size = 30
)


# Tune

set.seed(123)

svm_tuned <- tune_grid(
  wf_svm,
  resamples = folds,
  grid = svm_grid,
  metrics = metric_set(roc_auc),
  control = control_grid(save_pred = T, verbose = T)
  )


# Results

collect_metrics(svm_tuned)

show_best(
  svm_tuned,
  metric = "roc_auc",
  n = 10
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

final_svm_fit <- fit(
  final_svm_wf,
  data = train
)


# Out-of-sample predictions

svm_test_pred <- predict(
  final_svm_fit,
  test,
  type = "prob"
)

svm_results <- test |>
  select(iso3c, year, precrisis3) |>
  bind_cols(svm_test_pred)


# Evaluation

roc_auc(
  svm_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
)


# ROC curve

roc_curve(
  svm_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
) |>
  autoplot()




# Random Forests without NAs ==================================================


# Train / test split

train <- panel_ml |>
  filter(year <= 2006)

test <- panel_ml |>
  filter(year >= 2007, year <= 2022)


# Random Forest recipe

rec_rf <- recipe(precrisis3 ~ ., data = train) |>
  step_rm(iso3c, year) |>
  step_naomit(all_numeric_predictors()) |> 
  step_zv(all_predictors())

# Random Forest model

mod_rf <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
) |>
  set_engine(
    "ranger",
    importance = "permutation"
  ) |>
  set_mode("classification")

# Workflow

wf_rf <- workflow() |>
  add_recipe(rec_rf) |>
  add_model(mod_rf)


# Define expanding / rolling window

cv_periods <- tibble(
  id = paste0("Fold", 1:4),
  analysis_start = c(1970, 1970, 1970, 1970),
  analysis_end   = c(1989, 1994, 1999, 2001),
  assessment_start = c(1990, 1995, 2000, 2002),
  assessment_end   = c(1994, 1999, 2004, 2006)
)

make_split <- function(data, analysis_start, analysis_end, assessment_start, assessment_end) {
  
  analysis <- data |>
    filter(
      year >= analysis_start,
      year <= analysis_end
    )
  
  assessment <- data |>
    filter(
      year >= assessment_start,
      year <= assessment_end
    )
  
  make_splits(
    x = list(
      analysis = which(
        data$year >= analysis_start &
          data$year <= analysis_end
      ),
      assessment = which(
        data$year >= assessment_start &
          data$year <= assessment_end
      )
    ),
    data = data
  )
}

folds <- manual_rset(
  splits = cv_periods |>
    mutate(
      splits = pmap(
        list(
          analysis_start,
          analysis_end,
          assessment_start,
          assessment_end
        ),
        ~ make_split(
          train,
          ..1, ..2, ..3, ..4
        )
      )
    ) |>
    pull(splits),
  
  ids = cv_periods$id
)


# Verify

# analysis(folds$splits[[1]]) |>
#   summarise(
#     min_year = min(year),
#     max_year = max(year),
#     n = n()
#   )
# 
# assessment(folds$splits[[1]]) |>
#   summarise(
#     min_year = min(year),
#     max_year = max(year),
#     n = n()
#   )


n_predictors <- train |>
  select(-precrisis3, -iso3c, -year) |>
  ncol()

set.seed(123)

rf_grid <- grid_random(
  mtry(range = c(2L, n_predictors)),
  min_n(range = c(2L, 30L)),
  size = 30
)


rf_tuned <- tune_grid(
  wf_rf,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc),
  control = control_grid(
    save_pred = T,
    verbose = T
  )
)


collect_metrics(rf_tuned)

show_best(
  rf_tuned,
  metric = "roc_auc",
  n = 10
)

best_rf <- select_best(
  rf_tuned,
  metric = "roc_auc"
)


final_wf <- finalize_workflow(wf_rf, best_rf)

final_rf_fit <- fit(
  final_wf,
  data = train
)

rf_test_pred <- predict(
  final_rf_fit,
  test,
  type = "prob"
)

rf_results <- test |>
  select(iso3c, year, precrisis3) |>
  bind_cols(rf_test_pred)

roc_auc(
  rf_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
)


# Plot curves

roc_curve(
  rf_results,
  truth = precrisis3,
  .pred_1,
  event_level = "second"
) |>
  autoplot()





# WICHTIG ==================

prep_svm <- prep(rec_svm)

train_processed <- bake(
  prep_svm,
  new_data = NULL
)

summary(train_processed)


prep_rf <- prep(rec_rf)

train_processed <- bake(
  prep_rf,
  new_data = NULL
)

summary(train_processed)






# Mülleimer ===============================================================

# # Transform panel
# ml_panel <- panel |>
#   arrange(country, year) |>
#   group_by(country) |>
#   
#   # Create lagged variables
#   mutate(
#     across(
#       all_of(x_vars),
#       list(
#         lag1 = ~lag(.x, 1),
#         lag2 = ~lag(.x, 2),
#         lag3 = ~lag(.x, 3)
#       ),
#       .names = "{.col}_{.fn}"
#     )
#   ) |>
#   ungroup() |>
#   
#   # Remove crisis years
#   filter(crisis != 1)
# 
# lag_vars <- names(ml_panel)[
#   grepl("_lag[123]$", names(ml_panel))
# ]
# 
# # Remove unnecessary columns
# ml_panel <- ml_panel |>
#   select(country, year, precrisis3, all_of(x_vars), all_of(lag_vars))
# 
# # Convert precrisis variable into a factor
# ml_panel <- ml_panel |>
#   mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))

