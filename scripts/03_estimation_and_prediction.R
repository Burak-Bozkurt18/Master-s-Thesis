# Step 3: Estimate logit models
# Purpose:  
# Inputs:   
# Outputs:

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(modelsummary)
library(pROC)
library(foreach)
library(doParallel)
# library(kernelshap)
library(shapviz)
library(themis)
library(treeshap)
library(patchwork)

# 2 Load Panel ===============================================================

panel <- read_rds("data/final/panel.rds")

# 3 Set Theme ================================================================

theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "serif", size = 12),
      axis.text = element_text(size = 12, colour = "black"),
      axis.line = element_line(linewidth = 0.5),
      axis.ticks = element_line(linewidth = 0.5),
      panel.grid = element_line(linetype = "dashed")
    )
)

par(family = "serif")



# 4. Preprocessing ============================================================

# Logit Panel

panel_logit <- panel |> 
  filter(crisis != 1) |> 
  mutate(
    advanced = factor(advanced),
    precrisis1 = factor(precrisis1),
    precrisis2 = factor(precrisis2),
    precrisis3 = factor(precrisis3),
    precrisis4 = factor(precrisis4)
    )



# Define model specifications
specs <- list(
  baseline = c("advanced", "cgdppriv_growth"),
  broad    = c("advanced", "cgdppriv_growth", "govcgdp_growth",
               "rgdpgrowth", "inflation"),
  full     = c("advanced", "cgdppriv_growth", "govcgdp_growth",
               "rgdpgrowth", "inflation", "ltd", "bcagdp",
               "bmgdpgrowth", "bmtr", "sprr")
)

outcomes <- c("precrisis1", "precrisis2", "precrisis3", "precrisis4")



# 5 Estimate Logit Models =====================================================

## 5.1 In-Sample ==============================================================

# Custom model summary
gof_custom <- function(model) {
  
  # Model data
  mf <- model.frame(model)
  
  # Identify observations used by the model
  rows <- as.integer(rownames(mf))
  
  # Countries in estimation sample
  countries <- unique(panel_logit$iso3c[rows])
  
  # Number of countries
  n_countries <- length(countries)
  
  # Number of crisis observations for these countries
  n_crises <- panel |>
    filter(
      iso3c %in% countries,
      crisis_start == 1
    ) |>
    nrow()
  
  # Predicted probabilities
  p <- predict(model, type = "response")
  
  # Actual outcome
  y <- model.response(mf)
  
  # AUC
  auc_value <- as.numeric(pROC::auc(y, p))
  
  # McFadden R²
  ll_model <- as.numeric(logLik(model))
  
  null_model <- glm(
    y ~ 1,
    family = binomial()
  )
  
  ll_null <- as.numeric(logLik(null_model))
  
  mcfadden_r2 <- 1 - ll_model / ll_null
  
  data.frame(
    `Num. Countries` = n_countries,
    `Num. Crises` = n_crises,
    `AUC` = round(auc_value, 3),
    `McFadden R²` = round(mcfadden_r2, 3)
  )
}

estimate_logit <- function(data, outcome) {
  map(specs, \(rhs) glm(reformulate(rhs, response = outcome),
                           data = data, family = binomial()))
}

models_all <- setNames(
  map(
    outcomes,
    \(outcome) estimate_logit(panel_logit, outcome)
  ),
  outcomes
)

# Summarize results
modelsummary(
  models_all[c("precrisis1", "precrisis2")],
  title = "Early-warning logits, 1-2 year horizons",
  gof_function = gof_custom,
  gof_omit = "RMSE|Log.Lik.|Std.Errors|AUC",
  coef_omit = "(Intercept)",
  stars = TRUE,
  vcov = ~iso3c,
  shape = "cbind"
)

modelsummary(
  models_all[c("precrisis3", "precrisis4")],
  title = "Early-warning logits, 3-4 year horizons",
  gof_function = gof_custom,
  gof_omit = "RMSE|Log.Lik.|Std.Errors|AUC",
  coef_omit = "(Intercept)",
  stars = TRUE,
  vcov = ~iso3c,
  shape = "cbind"
)

## 5.2 Out-of-sample ==========================================================

last_known_crisis_year <- 2025

horizon_of <- function(outcome) as.integer(sub("precrisis", "", outcome))

max_test_year <- function(outcome) last_known_crisis_year - horizon_of(outcome)

forecast_logit <- function(formula, data, forecast_years, horizon) {
  
  predictions <- map(forecast_years, function(test_year) {
    
    # Expanding training window
    train <- data |>
      filter(year < test_year - horizon)
    
    # One-year test set
    test <- data |>
      filter(year == test_year)
    
    # Estimate model
    model <- glm(formula = formula, data = train, family = binomial())
    
    # Predict test year
    test |>
      mutate(.pred_1 = predict(model, newdata = test, type = "response"))
    
  })
  
  bind_rows(predictions)
}

logit_oos_results <- expand_grid(
  outcome       = outcomes,
  specification = names(specs)
) |>
  mutate(
    preds = map2(outcome, specification, \(o, s) {
      forecast_logit(
        formula        = reformulate(specs[[s]], response = o),
        data           = panel_logit,
        forecast_years = 2005:max_test_year(o),
        horizon = horizon_of(o)
      )
    })
  )

evaluate <- function(results, outcome) {
  
  results <- results |>
    filter(!is.na(.data[[outcome]]), !is.na(.pred_1))
  
  bind_rows(
    roc_auc(results, truth = !!sym(outcome), .pred_1, event_level = "second"),
    pr_auc( results, truth = !!sym(outcome), .pred_1, event_level = "second")
  )
}

logit_auc <- logit_oos_results |>
  mutate(metrics = map2(preds, outcome, evaluate)) |>
  select(outcome, specification, metrics) |>
  unnest(metrics)

# 4. Cross-validation =========================================================

# +++ All precisis variables +++ =============================================

# Outcome / horizon bookkeeping ------------------------------------------

outcomes <- c("precrisis1", "precrisis2", "precrisis3", "precrisis4")

horizon_of <- function(outcome) as.integer(sub("precrisis", "", outcome))

last_known_crisis_year <- 2025
max_test_year <- function(outcome) last_known_crisis_year - horizon_of(outcome)

# ML panels, now nested outcome -> spec -----------------------------------

panel_temp <- panel |>
  filter(crisis != 1) |>
  mutate(across(all_of(outcomes), factor))

make_panels_ml <- function(outcome) {
  map(specs, \(vars) panel_temp |>
        select(year, iso3c, all_of(outcome), all_of(vars)) |>
        drop_na())
}

panels_ml_all <- map(outcomes, make_panels_ml) |> setNames(outcomes)

train_all <- map(
  panels_ml_all,
  \(spec_list) map(spec_list, \(data) filter(data, year <= 2004))
)

# CV periods, now a function of horizon -----------------------------------

make_cv_periods <- function(horizon) {
  tibble(
    id = paste0("Fold", 1:10),
    analysis_start = 1970,
    assessment_start = 1995:2004,
    assessment_end = 1995:2004,
    analysis_end = assessment_start - 1 - horizon
  )
}

make_split <- function(data, analysis_start, analysis_end, assessment_start, assessment_end) {
  make_splits(
    x = list(
      analysis = which(data$year >= analysis_start & data$year <= analysis_end),
      assessment = which(data$year >= assessment_start & data$year <= assessment_end)
    ),
    data = data
  )
}

make_folds <- function(data, cv_periods) {
  manual_rset(
    splits = pmap(
      cv_periods[, c("analysis_start", "analysis_end", "assessment_start", "assessment_end")],
      \(analysis_start, analysis_end, assessment_start, assessment_end) {
        make_split(data, analysis_start, analysis_end, assessment_start, assessment_end)
      }
    ),
    ids = cv_periods$id
  )
}

folds_all <- map(outcomes, \(o) {
  cvp <- make_cv_periods(horizon_of(o))
  map(train_all[[o]], \(d) make_folds(d, cvp))
}) |> setNames(outcomes)

# Random forest workflow / grid, now parameterized by outcome -------------

make_rf_workflow <- function(data, outcome) {
  recipe(reformulate(".", response = outcome), data = data) |>
    step_rm(iso3c, year) |>
    workflow() |>
    add_model(
      rand_forest(trees = 500, mtry = tune(), min_n = tune()) |>
        set_engine("ranger", importance = "permutation") |>
        set_mode("classification")
    )
}

make_rf_grid <- function(data, outcome, size = 30) {
  n_predictors <- data |> select(-all_of(outcome), -iso3c, -year) |> ncol()
  grid_random(
    mtry(range = c(2L, n_predictors)),
    min_n(range = c(2L, 30L)),
    size = size
  )
}

tune_model <- function(data, folds, outcome, model_type = c("rf", "svm", "mlp"), grid_size = 30) {
  model_type <- match.arg(model_type)
  workflow_fn <- match.fun(paste0("make_", model_type, "_workflow"))
  grid_fn     <- match.fun(paste0("make_", model_type, "_grid"))
  
  workflow <- workflow_fn(data, outcome)
  grid     <- grid_fn(data, outcome, grid_size)
  
  tune_grid(
    workflow,
    resamples = folds,
    grid = grid,
    metrics = metric_set(pr_auc),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )
}

set.seed(123)
n_cores <- detectCores() - 1
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

rf_tuned_all <- map(outcomes, \(o) {
  map2(train_all[[o]], folds_all[[o]], \(d, f) tune_model(d, f, outcome = o, model_type = "rf"))
}) |> setNames(outcomes)

best_rf_all <- map(
  rf_tuned_all,
  \(spec_list) map(spec_list, \(x) select_best(x, metric = "pr_auc"))
)

# Pseudo-OOS forecasting, now taking outcome + horizon ---------------------

forecast_model <- function(data, best_params, forecast_years, horizon, outcome,
                           model_type = c("rf", "svm", "mlp")) {
  model_type <- match.arg(model_type)
  workflow_fn <- match.fun(paste0("make_", model_type, "_workflow"))
  
  final_workflow <- workflow_fn(data, outcome) |>
    finalize_workflow(best_params)
  
  map(
    forecast_years,
    function(y) {
      message("Forecasting ", outcome, ", year: ", y)
      
      train_data <- data |> filter(year >= 1970, year <= y - 1 - horizon)
      test_data  <- data |> filter(year == y)
      
      fit <- final_workflow |> fit(data = train_data)
      predictions <- predict(fit, new_data = test_data, type = "prob")
      
      test_data |>
        select(iso3c, year, all_of(outcome)) |>
        bind_cols(predictions)
    }
  ) |>
    list_rbind()
}

set.seed(123)

rf_oos_results_all <- map(outcomes, \(o) {
  map2(
    panels_ml_all[[o]], best_rf_all[[o]],
    \(data, params) forecast_model(
      data, params,
      forecast_years = 2005:max_test_year(o),
      horizon = horizon_of(o),
      outcome = o,
      model_type = "rf"
    )
  )
}) |> setNames(outcomes)

# Evaluation, now outcome-aware --------------------------------------------

evaluate <- function(results, outcome) {
  results <- results |> filter(!is.na(.data[[outcome]]), !is.na(.pred_1))
  bind_rows(
    roc_auc(results, truth = !!sym(outcome), .pred_1, event_level = "second"),
    pr_auc( results, truth = !!sym(outcome), .pred_1, event_level = "second")
  )
}

rf_auc <- map(outcomes, \(o) {
  map(rf_oos_results_all[[o]], \(res) evaluate(res, o)) |>
    list_rbind(names_to = "specification")
}) |>
  setNames(outcomes) |>
  list_rbind(names_to = "outcome")


# 5. Support Vector Machines ==================================================

# Generic SVM workflow, now outcome-aware
make_svm_workflow <- function(data, outcome) {
  
  recipe(reformulate(".", response = outcome), data = data) |>
    step_rm(iso3c, year) |>
    step_dummy(all_nominal_predictors()) |>
    step_normalize(all_predictors()) |>
    step_upsample(
      all_of(outcome),
      over_ratio = 0.5
    ) |>
    workflow() |>
    add_model(
      svm_rbf(
        cost = tune(),
        rbf_sigma = tune()
      ) |>
        set_engine("kernlab") |>
        set_mode("classification")
    )
}


# Hyperparameter grid (data/outcome args unused here, but kept for
# interface consistency with make_rf_grid via match.fun() dispatch)
make_svm_grid <- function(data, outcome, size = 30) {
  
  grid_space_filling(
    cost(),
    rbf_sigma(),
    size = size
  )
}


# Tune all specifications, for every outcome ==================================

set.seed(123)

svm_tuned_all <- map(outcomes, \(o) {
  map2(train_all[[o]], folds_all[[o]], \(d, f) tune_model(d, f, outcome = o, model_type = "svm"))
}) |> setNames(outcomes)


# Best hyperparameters =========================================================

best_svm_all <- map(
  svm_tuned_all,
  \(spec_list) map(spec_list, \(x) select_best(x, metric = "pr_auc"))
)

best_svm_all


# Forecast all outcomes x specifications, embargoed ============================

set.seed(222)

svm_oos_results_all <- map(outcomes, \(o) {
  map2(
    panels_ml_all[[o]], best_svm_all[[o]],
    \(data, params) forecast_model(
      data, params,
      forecast_years = 2005:max_test_year(o),
      horizon = horizon_of(o),
      outcome = o,
      model_type = "svm"
    )
  )
}) |> setNames(outcomes)


# Evaluation ====================================================================

svm_auc <- map(outcomes, \(o) {
  map(svm_oos_results_all[[o]], \(res) evaluate(res, o)) |>
    list_rbind(names_to = "specification")
}) |>
  setNames(outcomes) |>
  list_rbind(names_to = "outcome")


# 6. Multilayer Perceptrons ===================================================

# +++ All Precrisis variables +++ =============================================

# 6. Multilayer Perceptrons ===================================================

make_mlp_workflow <- function(data, outcome) {
  
  recipe(reformulate(".", response = outcome), data = data) |>
    step_rm(iso3c, year) |>
    step_dummy(all_nominal_predictors()) |>
    step_normalize(all_predictors()) |>
    step_upsample(
      all_of(outcome),
      over_ratio = 0.5
    ) |>
    workflow() |>
    add_model(
      mlp(
        hidden_units = tune(),
        penalty = tune(),
        epochs = 50,
        activation = "relu"
      ) |>
        set_engine("brulee", stop_iter = 5) |>
        set_mode("classification")
    )
}

make_mlp_grid <- function(data, outcome, size = 15) {
  
  grid_space_filling(
    hidden_units(),
    penalty(),
    size = size
  )
}

# Tune all MLP specifications, for every outcome ==============================

set.seed(123)

mlp_tuned_all <- map(outcomes, \(o) {
  map2(train_all[[o]], folds_all[[o]], \(d, f) tune_model(d, f, outcome = o, model_type = "mlp", grid_size = 15))
}) |> setNames(outcomes)

# Select best hyperparameters ==================================================

best_mlp_all <- map(
  mlp_tuned_all,
  \(spec_list) map(spec_list, \(x) select_best(x, metric = "pr_auc"))
)

best_mlp_all

# Out-of-sample predictions, embargoed and per outcome =========================

set.seed(123)

mlp_oos_results_all <- map(outcomes, \(o) {
  map2(
    panels_ml_all[[o]], best_mlp_all[[o]],
    \(data, params) forecast_model(
      data, params,
      forecast_years = 2005:max_test_year(o),
      horizon = horizon_of(o),
      outcome = o,
      model_type = "mlp"
    )
  )
}) |> setNames(outcomes)

# Evaluation ====================================================================

mlp_auc <- map(outcomes, \(o) {
  map(mlp_oos_results_all[[o]], \(res) evaluate(res, o)) |>
    list_rbind(names_to = "specification")
}) |>
  setNames(outcomes) |>
  list_rbind(names_to = "outcome")

# Evaluate all ML results ===================================================
model_auc <- bind_rows(
  
  logit_auc |> 
    mutate(model = "Logit"),
  
  rf_auc |>
    mutate(model = "RF"),
  
  svm_auc |>
    mutate(model = "SVM"),
  
  mlp_auc |>
    mutate(model = "MLP")
  
  ) |>
  select(
    outcome,
    specification,
    model,
    .metric,
    .estimate
  ) |> 
  pivot_wider(
    names_from = "model",
    values_from = ".estimate"
  ) |> 
  arrange(.metric)

model_auc |> 
  filter(.metric == "pr_auc", outcome == "precrisis1")

datasummary_df(model_auc)

# ROC and PR Curves =================================================================

oos_results <- list(
  Logit = logit_oos_results,
  MLP   = mlp_oos_results_all,
  SVM   = svm_oos_results_all,
  RF    = rf_oos_results_all
)

create_roc <- function(model_results, specification, model_name) {
  
  model_results[[specification]] |>
    roc_curve(
      truth = precrisis3,
      .pred_1,
      event_level = "second"
    ) |>
    mutate(
      model = model_name,
      specification = specification
    )
}

create_pr <- function(model_results, specification, model_name) {
  
  model_results[[specification]] |>
    pr_curve(
      truth = precrisis3,
      .pred_1,
      event_level = "second"
    ) |>
    mutate(
      model = model_name,
      specification = specification
    )
}

create_curves <- function(curve_function) {
  
  map(
    names(specs),
    function(spec) {
      
      imap(
        oos_results,
        ~ curve_function(
          .x,
          specification = spec,
          model_name = .y
        )
      ) |>
        list_rbind()
    }
  ) |>
    list_rbind()
}

roc_all <- create_curves(create_roc)
pr_all  <- create_curves(create_pr)

p1 <- ggplot(
  roc_all,
  aes(
    x = 1 - specificity,
    y = sensitivity,
    color = model,
    linetype = model
  )
) +
  geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed") +
  coord_equal() +
  facet_wrap(~ specification) +
  labs(
    title = "ROC Curves",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(
  pr_all,
  aes(
    x = recall,
    y = precision,
    color = model,
    linetype = model
    )
  ) +
  geom_line(linewidth = 1) +
  coord_equal() +
  facet_wrap(~ specification) +
  labs(
    title = "PR Curves",
    x = "Recall",
    y = "Precision"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p1 / p2 +
  plot_layout(
    guides = "collect" # Eine gemeinsame Legende
  ) &
  theme(
    legend.position='bottom',
    legend.title = element_blank()
    )

# Shapley Values =============================================================
set.seed(123)

train_data <- panels_ml$full |>
  filter(year >= 1970, year <= 2006)

test_data <- panels_ml$full |>
  filter(year == 2007)

final_workflow <- make_rf_workflow(panels_ml$full) |>
  finalize_workflow(best_rf$full)

fit <- final_workflow |>
  fit(data = train_data)

# Extract ranger model
rf_model <- extract_fit_engine(fit)

X_train <- train_data |>
  select(-iso3c, -year, -precrisis3)

X_test <- test_data |>
  select(-iso3c, -year, -precrisis3)

unified_rf <- ranger.unify(rf_model, X_train)

shap_2007 <- treeshap(unified_rf, X_test)

pred <- predict(
  fit,
  new_data = test_data,
  type = "prob"
)

pred

plot_feature_importance(shap_2007)

plot_contribution(shap_2007, obs = 48)


# Robustness Checks ==========================================================

## RC1: estimate baseline & broad on the SAME sample as the full specification ====

specs_rc1 <- specs[c("baseline", "broad")]

# Generic logit pipeline; `data` can be one shared data frame or a named
# list of per-outcome data frames (needed when the sample differs by outcome)
run_logit_pipeline <- function(data, specs_subset, outcomes) {
  
  get_data <- function(o) if (is.data.frame(data)) data else data[[o]]
  
  oos <- expand_grid(outcome = outcomes, specification = names(specs_subset)) |>
    mutate(
      preds = map2(outcome, specification, \(o, s) {
        forecast_logit(
          formula        = reformulate(specs_subset[[s]], response = o),
          data           = get_data(o),
          forecast_years = 2005:max_test_year(o),
          horizon        = horizon_of(o)
        )
      })
    )
  
  auc <- oos |>
    mutate(metrics = map2(preds, outcome, evaluate)) |>
    select(outcome, specification, metrics) |>
    unnest(metrics)
  
  list(oos = oos, auc = auc)
}

# Logit: restrict panel_logit to rows non-missing on full-spec vars
panel_logit_rc1 <- map(outcomes, \(o) {
  panel_logit |>
    select(year, iso3c, all_of(o), all_of(specs$full)) |>
    drop_na()
}) |> setNames(outcomes)

logit_rc1 <- run_logit_pipeline(panel_logit_rc1, specs_rc1, outcomes)

# ML: reuse the already-cleaned full-spec panels, just select fewer cols
panels_ml_rc1 <- map(outcomes, \(o) {
  full_data <- panels_ml_all[[o]][["full"]]
  map(specs_rc1, \(vars) full_data |> select(year, iso3c, all_of(o), all_of(vars)))
}) |> setNames(outcomes)

train_rc1 <- map(panels_ml_rc1, \(spec_list) map(spec_list, \(d) filter(d, year <= 2004)))

# Shared CV-fold builder (reuses make_cv_periods / make_folds you already have)
make_folds_for <- function(train_list, outcomes) {
  map(outcomes, \(o) {
    cvp <- make_cv_periods(horizon_of(o))
    map(train_list[[o]], \(d) make_folds(d, cvp))
  }) |> setNames(outcomes)
}

folds_rc1 <- make_folds_for(train_rc1, outcomes)


# Generic ML pipeline: tune -> select best -> forecast -> evaluate, per outcome
run_ml_pipeline <- function(panels_ml, train, folds, outcomes, model_type, grid_size = 30) {
  
  set.seed(123)
  tuned <- map(outcomes, \(o) {
    map2(train[[o]], folds[[o]],
         \(d, f) tune_model(d, f, outcome = o, model_type = model_type, grid_size = grid_size))
  }) |> setNames(outcomes)
  
  best <- map(tuned, \(spec_list) map(spec_list, \(x) select_best(x, metric = "pr_auc")))
  
  set.seed(123)
  oos <- map(outcomes, \(o) {
    map2(panels_ml[[o]], best[[o]], \(data, params) forecast_model(
      data, params,
      forecast_years = 2005:max_test_year(o),
      horizon = horizon_of(o), outcome = o, model_type = model_type
    ))
  }) |> setNames(outcomes)
  
  auc <- map(outcomes, \(o) {
    map(oos[[o]], \(res) evaluate(res, o)) |> list_rbind(names_to = "specification")
  }) |> setNames(outcomes) |> list_rbind(names_to = "outcome")
  
  list(tuned = tuned, best = best, oos = oos, auc = auc)
}

rf_rc1  <- run_ml_pipeline(panels_ml_rc1, train_rc1, folds_rc1, outcomes, "rf")
svm_rc1 <- run_ml_pipeline(panels_ml_rc1, train_rc1, folds_rc1, outcomes, "svm")
mlp_rc1 <- run_ml_pipeline(panels_ml_rc1, train_rc1, folds_rc1, outcomes, "mlp", grid_size = 15)

rc1_auc <- bind_rows(
  logit_rc1$auc |> mutate(model = "Logit"),
  rf_rc1$auc    |> mutate(model = "RF"),
  svm_rc1$auc   |> mutate(model = "SVM"),
  mlp_rc1$auc   |> mutate(model = "MLP")
) |> mutate(check = "RC1: reduced sample")

rc1_auc |>
  select(outcome, specification, .metric, .estimate, model) |> 
  pivot_wider(names_from = model, values_from = .estimate) |> 
  arrange(.metric)

## RC2: cgdppriv_growth -> tlpriv_rgrowth, bmgdpgrowth -> bm_rgrowth =========

full_vars_rc2 <- c("advanced", "tlpriv_rgrowth", "govcgdp_growth",
                   "rgdpgrowth", "inflation", "ltd", "bcagdp",
                   "bm_rgrowth", "bmtr", "sprr")

specs_rc2 <- list(full = full_vars_rc2)

# Logit: panel_logit already has advanced as factor; vars just swapped
logit_rc2 <- run_logit_pipeline(panel_logit, specs_rc2, outcomes)

# ML
panels_ml_rc2 <- map(outcomes, \(o) {
  map(specs_rc2, \(vars) panel_temp |>
        select(year, iso3c, all_of(o), all_of(vars)) |>
        drop_na())
}) |> setNames(outcomes)

train_rc2 <- map(panels_ml_rc2, \(spec_list) map(spec_list, \(d) filter(d, year <= 2004)))
folds_rc2 <- make_folds_for(train_rc2, outcomes)

rf_rc2  <- run_ml_pipeline(panels_ml_rc2, train_rc2, folds_rc2, outcomes, "rf")
svm_rc2 <- run_ml_pipeline(panels_ml_rc2, train_rc2, folds_rc2, outcomes, "svm")
mlp_rc2 <- run_ml_pipeline(panels_ml_rc2, train_rc2, folds_rc2, outcomes, "mlp", grid_size = 15)

rc2_auc <- bind_rows(
  logit_rc2$auc |> mutate(model = "Logit"),
  rf_rc2$auc    |> mutate(model = "RF"),
  svm_rc2$auc   |> mutate(model = "SVM"),
  mlp_rc2$auc   |> mutate(model = "MLP")
) |> mutate(check = "RC2: real growth vars")

rc2_auc |> 
  select(outcome, specification, .metric, .estimate, model) |> 
  pivot_wider(names_from = model, values_from = .estimate) |> 
  arrange(.metric)

## RC3: advanced -> region =====================================================

full_vars_rc3 <- c("region", "cgdppriv_growth", "govcgdp_growth",
                   "rgdpgrowth", "inflation", "ltd", "bcagdp",
                   "bmgdpgrowth", "bmtr", "sprr")

specs_rc3 <- list(full = full_vars_rc3)

# Logit: need a region-aware version of panel_logit; glm dummy-codes
# factors automatically via the model formula, so no step_dummy needed
panel_logit_rc3 <- panel |>
  filter(crisis != 1) |>
  mutate(
    region = factor(region),
    across(all_of(outcomes), factor)
  )

logit_rc3 <- run_logit_pipeline(panel_logit_rc3, specs_rc3, outcomes)

# ML: region as factor; step_dummy() in the updated SVM/MLP workflows
# handles the encoding, ranger (RF) uses the raw factor natively
panel_temp_rc3 <- panel |>
  filter(crisis != 1) |>
  mutate(across(all_of(outcomes), factor), region = factor(region))

panels_ml_rc3 <- map(outcomes, \(o) {
  map(specs_rc3, \(vars) panel_temp_rc3 |>
        select(year, iso3c, all_of(o), all_of(vars)) |>
        drop_na())
}) |> setNames(outcomes)

train_rc3 <- map(panels_ml_rc3, \(spec_list) map(spec_list, \(d) filter(d, year <= 2004)))
folds_rc3 <- make_folds_for(train_rc3, outcomes)

rf_rc3  <- run_ml_pipeline(panels_ml_rc3, train_rc3, folds_rc3, outcomes, "rf")
svm_rc3 <- run_ml_pipeline(panels_ml_rc3, train_rc3, folds_rc3, outcomes, "svm")
mlp_rc3 <- run_ml_pipeline(panels_ml_rc3, train_rc3, folds_rc3, outcomes, "mlp", grid_size = 15)

rc3_auc <- bind_rows(
  logit_rc3$auc |> mutate(model = "Logit"),
  rf_rc3$auc    |> mutate(model = "RF"),
  svm_rc3$auc   |> mutate(model = "SVM"),
  mlp_rc3$auc   |> mutate(model = "MLP")
) |> mutate(check = "RC3: region")

rc3_auc |> 
  select(outcome, specification, .metric, .estimate, model) |> 
  pivot_wider(names_from = model, values_from = .estimate) |> 
  arrange(.metric)

## No years after COVID =======================================================

# Modify the "evaluate" function such that it only considers predictions up to 2019
rob2_evaluate <- function(results, outcome) {
  results <- results |> filter(!is.na(.data[[outcome]]), !is.na(.pred_1), year <= 2019)
  bind_rows(
    roc_auc(results, truth = !!sym(outcome), .pred_1, event_level = "second"),
    pr_auc( results, truth = !!sym(outcome), .pred_1, event_level = "second")
  )
}

# Logit
rob2_logit_auc <- logit_oos_results |>
  mutate(metrics = map2(preds, outcome, rob2_evaluate)) |>
  select(outcome, specification, metrics) |>
  unnest(metrics)

# RF
rob2_rf_auc <- map(outcomes, \(o) {
  map(rf_oos_results_all[[o]], \(res) rob2_evaluate(res, o)) |>
    list_rbind(names_to = "specification")
}) |>
  setNames(outcomes) |>
  list_rbind(names_to = "outcome")

# SVM
rob2_svm_auc <- map(outcomes, \(o) {
  map(svm_oos_results_all[[o]], \(res) rob2_evaluate(res, o)) |>
    list_rbind(names_to = "specification")
}) |>
  setNames(outcomes) |>
  list_rbind(names_to = "outcome")

# MLP
rob2_mlp_auc <- map(outcomes, \(o) {
  map(mlp_oos_results_all[[o]], \(res) rob2_evaluate(res, o)) |>
    list_rbind(names_to = "specification")
}) |>
  setNames(outcomes) |>
  list_rbind(names_to = "outcome")

# Evaluate
rob2_model_auc <- bind_rows(
  
  rob2_logit_auc |> 
    mutate(model = "Logit"),
  
  rob2_rf_auc |>
    mutate(model = "Random Forest"),
  
  rob2_svm_auc |>
    mutate(model = "SVM"),
  
  rob2_mlp_auc |>
    mutate(model = "MLP")
  
) |>
  select(
    outcome,
    specification,
    model,
    .metric,
    .estimate
  ) |> 
  pivot_wider(
    names_from = "outcome",
    values_from = ".estimate"
  ) |> 
  arrange(.metric)


datasummary_df(rob2_model_auc)




# tlpriv_rgrowth and bm_rgrowth
# Regions instead of Advanced indicator


stopCluster(cl)

# Mülleimer ================================================================

# forecast_rf_treeshap <- function(data, best_params, forecast_years) {
#   
#   final_workflow <- make_rf_workflow(data) |>
#     finalize_workflow(best_params)
#   
#   map(
#     forecast_years,
#     function(y) {
#       
#       message("Processing year: ", y)
#       
#       # Expanding training sample
#       train_data <- data |>
#         filter(year >= 1970, year <= y - 1)
#       
#       # OOS test sample
#       test_data <- data |>
#         filter(year == y)
#       
#       # Fit RF
#       fit <- final_workflow |>
#         fit(data = train_data)
#       
#       # Extract ranger model
#       rf_model <- extract_fit_engine(fit)
#       
#       # Predictors used by the RF
#       X_train <- train_data |>
#         select(-iso3c, -year, -precrisis3)
#       
#       X_test <- test_data |>
#         select(-iso3c, -year, -precrisis3)
#       
#       # Convert ranger model to treeshap format
#       unified_rf <- ranger.unify(
#         rf_model,
#         X_train
#       )
#       
#       # Calculate TreeSHAP values
#       shap <- treeshap(
#         unified_rf,
#         X_test
#       )
#       
#       list(
#         year = y,
#         shap = shap,
#         data = test_data
#       )
#     }
#   )
# }
# 
# rf_full_shap <- forecast_rf_treeshap(
#   data = panels_ml$full,
#   best_params = best_rf$full,
#   forecast_years = 2005:2022
# )
# 
# shap_full <- map(
#   rf_full_shap,
#   function(x) {
#     
#     x$shap$shaps |>
#       as.data.frame() |>
#       mutate(
#         iso3c = x$data$iso3c,
#         year = x$data$year,
#         precrisis3 = x$data$precrisis3
#       )
#   }
# ) |> 
#   list_rbind()
# 
# shap_importance <- shap_full |>
#   select(-c(iso3c, year, precrisis3)) |>
#   summarise(
#     across(
#       everything(),
#       ~ mean(abs(.x), na.rm = TRUE)
#     )
#   ) |>
#   pivot_longer(
#     everything(),
#     names_to = "variable",
#     values_to = "mean_abs_shap"
#   ) |>
#   arrange(desc(mean_abs_shap))
# 
# ggplot(
#   shap_importance,
#   aes(
#     x = reorder(variable, mean_abs_shap),
#     y = mean_abs_shap
#   )
# ) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     x = NULL,
#     y = "Mean absolute SHAP value"
#   )