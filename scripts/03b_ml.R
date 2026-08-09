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

# 4 Estimate Machine Learning Models =========================================

# 4.1 Random Forests =========================================================

# Remove crisis years

x_vars <- colnames(panel)[-(1:9)]

ml_panel <- panel |>
  arrange(country, year) |>
  group_by(country) |>
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
  filter(crisis != 1)

lag_vars <- names(ml_panel)[
  grepl("_lag[123]$", names(ml_panel))
]

ml_panel <- ml_panel |>
  select(country, year, precrisis3, all_of(lag_vars))

ml_panel <- ml_panel |>
  mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))

# Set train, validation and test samples
train <- panel_mod |> 
  filter(year <= 2004) |> 
  mutate(precrisis3 = factor(precrisis3))

validation <- panel_mod |> 
  filter(year >= 2005,
         year <= 2014)

test <- panel_mod |> 
  filter(year >= 2015) |> 
  mutate(precrisis3 = factor(precrisis3))



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


