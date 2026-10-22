# Step 3: Estimate logit models
# Purpose:  
# Inputs:   
# Outputs:

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(modelsummary)
library(kableExtra)
library(pROC)
library(foreach)
library(doParallel)
# library(kernelshap)
# library(shapviz)
library(themis)
# library(treeshap)
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

# Horizon / embargo helpers
last_known_crisis_year <- 2025
horizon_of <- function(outcome) as.integer(sub("precrisis", "", outcome))
max_test_year <- function(outcome) last_known_crisis_year - horizon_of(outcome)

first_test_year <- 2005
first_year      <- 1970

# Format a vector of years as "1970--2003" (en dash in LaTeX), "2004" or "--"
fmt_range <- function(x) {
  if (length(x) == 0) return("--")
  r <- range(x)
  if (r[1] == r[2]) as.character(r[1]) else paste0(r[1], "--", r[2])
}

create_sample_summary <- function(data, variables, outcome) {
  
  h         <- horizon_of(outcome)
  train_end <- first_test_year - 1 - h      # last training year for the first forecast
  test_end  <- max_test_year(outcome)
  
  sample <- data |>
    select(iso3c, year, advanced, all_of(outcome), all_of(variables)) |>
    drop_na() |>
    mutate(y = .data[[outcome]] == 1)
  
  train    <- sample |> filter(year >= first_year, year <= train_end)
  embargo  <- sample |> filter(year > train_end, year < first_test_year)
  test     <- sample |> filter(year >= first_test_year, year <= test_end)
  
  # Sanity check: the three groups should cover the full sample
  if (nrow(train) + nrow(embargo) + nrow(test) != nrow(sample)) {
    warning("Train + embargo + test != full sample for ", outcome)
  }
  
  tibble(
    observations   = nrow(sample),
    precrisisK     = sum(sample$y),
    countries      = n_distinct(sample$iso3c),
    advanced_share = (mean(as.integer(sample$advanced)) - 1) * 100,
    train_years    = fmt_range(train$year),
    training_obs   = nrow(train),
    training_share = mean(train$y) * 100,
    embargo_obs    = nrow(embargo),
    test_years     = fmt_range(test$year),
    testing_obs    = nrow(test),
    testing_share  = mean(test$y) * 100
  )
}

summary_table <- map_dfr(
  names(specs),
  \(spec) {
    map_dfr(
      outcomes,
      \(outcome) {
        create_sample_summary(
          data      = panel_logit,
          variables = specs[[spec]],
          outcome   = outcome
        ) |>
          mutate(specification = spec, outcome = outcome, .before = 1)
      }
    )
  }
)

tab_df <- summary_table |>
  mutate(
    specification = factor(specification, levels = names(specs)),
    Outcome = str_replace(outcome, "precrisis", "K = ")
  ) |>
  arrange(specification, outcome)

panel_index <- tab_df |>
  count(specification) |>
  mutate(label = str_to_title(as.character(specification))) %>%
  { setNames(.$n, .$label) }

tab_fmt <- tab_df |>
  transmute(
    Outcome,
    obs        = prettyNum(observations, big.mark = ","),
    pre_obs    = prettyNum(precrisisK,   big.mark = ","),
    countries  = as.character(countries),
    advanced   = formatC(advanced_share, format = "f", digits = 1),
    train_yrs  = train_years,
    train_obs  = prettyNum(training_obs, big.mark = ","),
    train_pre  = formatC(training_share, format = "f", digits = 1),
    emb_obs    = prettyNum(embargo_obs,  big.mark = ","),
    test_yrs   = test_years,
    test_obs   = prettyNum(testing_obs,  big.mark = ","),
    test_pre   = formatC(testing_share,  format = "f", digits = 1)
  )

tab_fmt |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    align     = paste0("l", strrep("r", 11)),
    escape    = FALSE,
    linesep   = "",
    col.names = c("Outcome",
                  "Obs.", "PreCrisis obs.", "Countries", "Advanced (\\%)",
                  "Years", "Obs.", "PreCrisis (\\%)",
                  "Obs.",
                  "Years", "Obs.", "PreCrisis (\\%)"),
    caption   = "Sample composition by model specification and crisis horizon",
    label     = "sample_summary"
  ) |>
  pack_rows(index = panel_index, bold = TRUE, italic = FALSE,
            latex_gap_space = "0.4em") |>
  add_header_above(c(" " = 1, "Full sample" = 4, "Training" = 3,
                     "Embargo" = 1, "Testing" = 3)) |>
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "center")


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
  
  # Predicted probabilities
  p <- predict(model, type = "response")
  
  # Actual outcome
  y <- model.response(mf)
  
  # McFadden R²
  ll_model <- as.numeric(logLik(model))
  
  null_model <- glm(y ~ 1, family = binomial())
  
  ll_null <- as.numeric(logLik(null_model))
  
  mcfadden_r2 <- 1 - ll_model / ll_null
  
  data.frame(
    `Num. Countries` = n_countries,
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
  gof_omit = "RMSE|Log.Lik.|Std.Errors|AIC",
  coef_omit = "(Intercept)",
  stars = TRUE,
  vcov = ~iso3c,
  shape = "cbind",
  output = "latex"
)

modelsummary(
  models_all[c("precrisis3", "precrisis4")],
  title = "Early-warning logits, 3-4 year horizons",
  gof_function = gof_custom,
  gof_omit = "RMSE|Log.Lik.|Std.Errors|AIC",
  coef_omit = "(Intercept)",
  stars = TRUE,
  vcov = ~iso3c,
  shape = "cbind",
  output = "latex"
)

## 5.2 Out-of-sample ==========================================================

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
  pivot_wider(
    names_from  = .metric,
    values_from = c(Logit, RF, SVM, MLP),
    names_glue  = "{.metric}_{.value}"
  ) |>
  select(outcome, specification,
         starts_with("pr_auc"), starts_with("roc_auc")) |>
  select(outcome, specification,
         pr_auc_Logit, pr_auc_RF, pr_auc_SVM, pr_auc_MLP,
         roc_auc_Logit, roc_auc_RF, roc_auc_SVM, roc_auc_MLP) |>
  kbl(format = "latex", 
      booktabs = TRUE, 
      digits = 2, 
      linesep = "",
      align = "llcccccccc",
      col.names = c("Outcome", "Specification", rep(c("Logit","RF","SVM","MLP"), 2)),
      caption = "Out-of-sample performance by outcome, specification and model") |>
  add_header_above(c(" " = 2, "PR-AUC" = 4, "ROC-AUC" = 4)) |>
  collapse_rows(columns = 1, latex_hline = "major", valign = "top")

datasummary_df(model_auc, output = "latex")





# Evaluation with cluster Bootstrap ===========================================

# Cluster bootstrap for AUC inference (resamples whole countries, not rows)

cluster_bootstrap_auc <- function(results, outcome, cluster_var = "iso3c",
                                  metric = c("roc_auc", "pr_auc"),
                                  null_value = NULL,
                                  n_boot = 1000, seed = 123) {
  
  metric <- match.arg(metric)
  results <- results |> filter(!is.na(.data[[outcome]]), !is.na(.pred_1))
  
  auc_fn <- function(df) {
    tryCatch({
      if (metric == "roc_auc") {
        as.numeric(pROC::auc(df[[outcome]], df$.pred_1, levels = c("0","1"), direction = "<"))
      } else {
        yardstick::pr_auc_vec(truth = df[[outcome]], estimate = df$.pred_1, event_level = "second")
      }
    }, error = function(e) NA_real_)
  }
  
  observed <- auc_fn(results)
  
  # Default null: 0.5 for ROC-AUC, prevalence for PR-AUC
  if (is.null(null_value)) {
    null_value <- if (metric == "roc_auc") 0.5 else {
      mean(as.numeric(as.character(results[[outcome]])), na.rm = TRUE)
    }
  }
  
  set.seed(seed)
  countries <- unique(results[[cluster_var]])
  n_countries <- length(countries)
  by_country <- split(results, results[[cluster_var]])
  
  boot_values <- map_dbl(seq_len(n_boot), function(i) {
    sampled <- sample(countries, n_countries, replace = TRUE)
    auc_fn(list_rbind(by_country[sampled]))
  })
  boot_values <- boot_values[!is.na(boot_values)]
  
  # One-sample bootstrap p-value: shift the distribution to be centered
  # under the null, then see how extreme the observed value is relative to it
  centered <- boot_values - mean(boot_values) + null_value
  p_value <- mean(abs(centered - null_value) >= abs(observed - null_value))
  
  tibble(
    metric       = metric,
    estimate     = observed,
    std.error    = sd(boot_values),
    null_value   = null_value,
    ci_lower     = quantile(boot_values, 0.025, names = FALSE),
    ci_upper     = quantile(boot_values, 0.975, names = FALSE),
    p.value      = p_value,
    n_valid_boot = length(boot_values)
  )
}


# Gather all OOS predictions into one long structure, keyed by model/outcome/spec
all_oos_results <- bind_rows(
  logit_oos_results |> select(outcome, specification, preds) |> mutate(model = "Logit"),
  map(outcomes, \(o) tibble(outcome = o, specification = names(rf_oos_results_all[[o]]),
                            preds = rf_oos_results_all[[o]])) |> list_rbind() |> mutate(model = "RF"),
  map(outcomes, \(o) tibble(outcome = o, specification = names(svm_oos_results_all[[o]]),
                            preds = svm_oos_results_all[[o]])) |> list_rbind() |> mutate(model = "SVM"),
  map(outcomes, \(o) tibble(outcome = o, specification = names(mlp_oos_results_all[[o]]),
                            preds = mlp_oos_results_all[[o]])) |> list_rbind() |> mutate(model = "MLP")
)

set.seed(123)

vs_random <- all_oos_results |>
  mutate(
    roc = map2(preds, outcome, \(p, o) cluster_bootstrap_auc(p, o, metric = "roc_auc", n_boot = 1000)),
    pr  = map2(preds, outcome, \(p, o) cluster_bootstrap_auc(p, o, metric = "pr_auc",  n_boot = 1000))
  ) |>
  select(model, outcome, specification, roc, pr) |>
  pivot_longer(c(roc, pr), names_to = "which", values_to = "result") |>
  unnest(result) |>
  select(model, outcome, specification, metric, estimate, std.error, p.value)


build_modelsummary_vs_random <- function(vs_random, metric_choice) {
  vs_random |>
    filter(metric == metric_choice) |>
    mutate(
      group = outcome,                      # becomes the spanning row label
      term  = str_to_title(specification)   # becomes the within-group row label
    ) |>
    select(model, group, term, estimate, std.error, p.value) |>
    split(~model) |>
    map(\(df) select(df, group, term, estimate, std.error, p.value)) |>
    map(as_modelsummary_model)
}


# Wrap a tidy data.frame as a modelsummary-recognized "model"
as_modelsummary_model <- function(tidy_df, n_obs = NA) {
  mod <- list(
    tidy   = tidy_df,
    glance = data.frame(nobs = n_obs)
  )
  class(mod) <- "modelsummary_list"
  mod
}


models_roc_random <- build_modelsummary_vs_random(vs_random, "roc_auc")[c("Logit", "RF", "SVM", "MLP")]
models_pr_random  <- build_modelsummary_vs_random(vs_random, "pr_auc")[c("Logit", "RF", "SVM", "MLP")]

modelsummary(
  models_roc_random,
  title = "Out-of-sample ROC-AUC vs. random benchmark (AUC = 0.5)",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.",
    "Stars denote significance of the cluster-bootstrap test against AUC = 0.5."
  ),
  output = "latex"
)

modelsummary(
  models_pr_random,
  title = "Out-of-sample PR-AUC vs. prevalence benchmark",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.",
    "Null for PR-AUC is each outcome's prevalence, not a fixed constant across outcomes.",
    "Stars denote significance of the cluster-bootstrap test against that prevalence."
  ),
  output = "latex"
)




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



# Robchecks with bootstrap ==================================================

# Normalize either pipeline's OOS output into one long tibble:
# columns = model, outcome, specification, preds
normalize_oos <- function(pipeline_result, model_name, is_logit = FALSE) {
  
  if (is_logit) {
    pipeline_result$oos |>
      select(outcome, specification, preds) |>
      mutate(model = model_name)
  } else {
    map(names(pipeline_result$oos), \(o) {
      spec_list <- pipeline_result$oos[[o]]
      tibble(
        outcome       = o,
        specification = names(spec_list),
        preds         = spec_list
      )
    }) |>
      list_rbind() |>
      mutate(model = model_name)
  }
}


# Build the vs-random bootstrap table for one robustness check,
# given its logit + rf + svm + mlp pipeline results.
# year_filter: optional max year to restrict predictions to (for RC4)
build_vs_random_check <- function(logit_result, rf_result, svm_result, mlp_result,
                                  check_label, year_filter = NULL, n_boot = 1000) {
  
  all_results <- bind_rows(
    normalize_oos(logit_result, "Logit", is_logit = TRUE),
    normalize_oos(rf_result,  "RF"),
    normalize_oos(svm_result, "SVM"),
    normalize_oos(mlp_result, "MLP")
  )
  
  if (!is.null(year_filter)) {
    all_results <- all_results |>
      mutate(preds = map(preds, \(df) filter(df, year <= year_filter)))
  }
  
  set.seed(123)
  
  all_results |>
    mutate(
      roc = map2(preds, outcome, \(p, o) cluster_bootstrap_auc(p, o, metric = "roc_auc", n_boot = n_boot)),
      pr  = map2(preds, outcome, \(p, o) cluster_bootstrap_auc(p, o, metric = "pr_auc",  n_boot = n_boot))
    ) |>
    select(model, outcome, specification, roc, pr) |>
    pivot_longer(c(roc, pr), names_to = "which", values_to = "result") |>
    unnest(result) |>
    select(model, outcome, specification, metric, estimate, std.error, p.value) |>
    mutate(check = check_label)
}

vs_random_rc1 <- build_vs_random_check(logit_rc1, rf_rc1, svm_rc1, mlp_rc1, "RC1: reduced sample")
vs_random_rc2 <- build_vs_random_check(logit_rc2, rf_rc2, svm_rc2, mlp_rc2, "RC2: real growth vars")
vs_random_rc3 <- build_vs_random_check(logit_rc3, rf_rc3, svm_rc3, mlp_rc3, "RC3: region")

# Build "pipeline-shaped" wrappers around your main-analysis results so
# normalize_oos() can consume them the same way as RC1-3
main_logit_as_pipeline <- list(oos = logit_oos_results)
main_rf_as_pipeline    <- list(oos = rf_oos_results_all)
main_svm_as_pipeline   <- list(oos = svm_oos_results_all)
main_mlp_as_pipeline   <- list(oos = mlp_oos_results_all)

vs_random_rc4 <- build_vs_random_check(
  main_logit_as_pipeline, main_rf_as_pipeline, main_svm_as_pipeline, main_mlp_as_pipeline,
  check_label = "RC4: excluding COVID years",
  year_filter = 2019
)

vs_random_all_rc <- bind_rows(vs_random_rc1, vs_random_rc2, vs_random_rc3, vs_random_rc4)

build_modelsummary_rc <- function(vs_random_all_rc, check_label, metric_choice) {
  vs_random_all_rc |>
    filter(check == check_label, metric == metric_choice) |>
    mutate(group = outcome, term = str_to_title(specification)) |>
    select(model, group, term, estimate, std.error, p.value) |>
    split(~model) |>
    map(\(df) select(df, group, term, estimate, std.error, p.value)) |>
    map(as_modelsummary_model)
}



rc_labels <- c("RC1: reduced sample", "RC2: real growth vars", "RC3: region", "RC4: excluding COVID years")

rc_tables_roc <- map(rc_labels, \(lbl) {
  build_modelsummary_rc(vs_random_all_rc, lbl, "roc_auc")[c("Logit","RF","SVM","MLP")]
}) |> setNames(rc_labels)

rc_tables_pr <- map(rc_labels, \(lbl) {
  build_modelsummary_rc(vs_random_all_rc, lbl, "pr_auc")[c("Logit","RF","SVM","MLP")]
}) |> setNames(rc_labels)

modelsummary(
  rc_tables_roc[["RC1: reduced sample"]],
  title = "RC1 (reduced sample): ROC-AUC vs. random benchmark (AUC = 0.5)",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses. 
    Stars denote significance of the cluster-bootstrap test against AUC = 0.5."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_pr[["RC1: reduced sample"]],
  title = "RC1 (reduced sample): PR-AUC vs. prevalence benchmark",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Null for PR-AUC is each outcome's prevalence, not a fixed constant across outcomes.
    Stars denote significance of the cluster-bootstrap test against that prevalence."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_roc[["RC2: real growth vars"]],
  title = "RC2 (real growth variables): ROC-AUC vs. random benchmark (AUC = 0.5)",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Stars denote significance of the cluster-bootstrap test against AUC = 0.5."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_pr[["RC2: real growth vars"]],
  title = "RC2 (real growth variables): PR-AUC vs. prevalence benchmark",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Null for PR-AUC is each outcome's prevalence, not a fixed constant across outcomes.
    Stars denote significance of the cluster-bootstrap test against that prevalence."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_roc[["RC3: region"]],
  title = "RC3 (region): ROC-AUC vs. random benchmark (AUC = 0.5)",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Stars denote significance of the cluster-bootstrap test against AUC = 0.5."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_pr[["RC3: region"]],
  title = "RC3 (region): PR-AUC vs. prevalence benchmark",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Null for PR-AUC is each outcome's prevalence, not a fixed constant across outcomes.
    Stars denote significance of the cluster-bootstrap test against that prevalence."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_roc[["RC4: excluding COVID years"]],
  title = "RC4 (excluding COVID years): ROC-AUC vs. random benchmark (AUC = 0.5)",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Stars denote significance of the cluster-bootstrap test against AUC = 0.5."
  ),
  output = "latex"
)

modelsummary(
  rc_tables_pr[["RC4: excluding COVID years"]],
  title = "RC4 (excluding COVID years): PR-AUC vs. prevalence benchmark",
  shape = group + term ~ model,
  stars = TRUE,
  gof_omit = ".*",
  notes = list(
    "Standard errors (cluster bootstrap over countries, 1000 replicates) in parentheses.
    Null for PR-AUC is each outcome's prevalence, not a fixed constant across outcomes.
    Stars denote significance of the cluster-bootstrap test against that prevalence."
  ),
  output = "latex"
)

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