# Step 3: Estimate logit models
# Purpose:  
# Inputs:   
# Outputs:

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(modelsummary)
library(fixest)
library(lmtest)
library(sandwich)
library(marginaleffects)
library(kernlab)

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
x_vars <- colnames(panel)[-(1:9)]

# Transform panel
ml_panel <- panel |>
  arrange(country, year) |>
  group_by(country) |>
  
  # Create lagged variables
  mutate(
    across(
      all_of(x_vars),
      list(
        lag1 = ~lag(.x, 1),
        lag2 = ~lag(.x, 2),
        lag3 = ~lag(.x, 3)
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  ungroup() |>
  
  # Remove crisis years
  filter(crisis != 1)

lag_vars <- names(ml_panel)[
  grepl("_lag[123]$", names(ml_panel))
]

# Remove unnecessary columns
ml_panel <- ml_panel |>
  select(country, year, precrisis3, all_of(x_vars), all_of(lag_vars))

# Convert precrisis variable into a factor
ml_panel <- ml_panel |>
  mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))


# 4 Estimate Machine Learning Models =========================================

# 4.1 Random Forests =========================================================


### ChatGPT approach

# Train / test split

train <- ml_panel |>
  filter(year <= 2009)

test <- ml_panel |>
  filter(year >= 2010, year <= 2022)


# Random Forest recipe

rec_rf <- recipe(precrisis3 ~ ., data = train) |>
  step_rm(country, year) |>
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
  analysis_end   = c(1989, 1994, 1999, 2004),
  assessment_start = c(1990, 1995, 2000, 2005),
  assessment_end   = c(1994, 1999, 2004, 2009)
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
  select(-precrisis3, -country, -year) |>
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
  select(country, year, precrisis3) |>
  bind_cols(rf_test_pred)

roc_auc(
  rf_results,
  truth = precrisis3,
  .pred_1
)

pr_auc(
  rf_results,
  truth = precrisis3,
  .pred_1
)

# Plot curves

roc_curve(
  rf_results,
  truth = precrisis3,
  .pred_1
) |>
  autoplot()


pr_curve(
  rf_results,
  truth = precrisis3,
  .pred_1
) |>
  autoplot()











rec_rf <- recipe(precrisis3 ~ ., data = train)

mod_rf <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

wf <- workflow() |>
  add_recipe(rec_rf) |>
  add_model(mod_rf)


set.seed(123)

# Später umändern
folds <- vfold_cv(train, v = 5)

rf_grid <-
  grid_regular(
    mtry(range = c(2, 10)),
    min_n(range = c(2, 20)),
    levels = 5
  )

rf_tuned <-
  tune_grid(
    wf,
    resamples = folds,
    grid = rf_grid,
    metrics = metric_set(roc_auc),
    control = control_grid(save_pred = T, verbose = T)
  )

collect_metrics(rf_tuned)

show_best(rf_tuned, metric = "roc_auc")

best_rf <- select_best(rf_tuned, metric = "roc_auc")

final_wf <-
  finalize_workflow(
    wf,
    best_rf
  )

rf_fit <-
  fit(
    final_wf,
    data = train
  )

pred <-
  predict(
    rf_fit,
    test,
    type = "prob"
  )


# Approach from HW

rf_preds <- rf_tuned |> 
  collect_predictions() |> 
  inner_join(best_rf, by = c("mtry", "min_n"))


