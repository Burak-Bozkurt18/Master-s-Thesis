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
library(kernelshap)
library(shapviz)
library(themis)

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



# 4. Preprocessing ============================================================

# Logit Panel

panel_logit <- panel |> 
  filter(crisis != 1) |> 
  mutate(
    advanced = factor(advanced),
    precrisis3 = factor(precrisis3)
    )

# ML Panels

panel_temp <- panel |>
  filter(crisis != 1) |>
  mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))


# Define model specifications
specs <- list(
  
  baseline = c(
    "iso3c", "year", "precrisis3",
    "advanced", "cgdppriv", "tlpriv_rgrowth", "govcgdp"
  ),
  
  broad = c(
    "iso3c", "year", "precrisis3",
    "advanced", "cgdppriv", "tlpriv_rgrowth", "govcgdp",
    "inflation", "rgdpgrowth", "ltd", "bcagdp"
  ),
  
  full = c(
    "iso3c", "year", "precrisis3",
    "advanced", "cgdppriv", "tlpriv_rgrowth", "govcgdp",
    "inflation", "rgdpgrowth", "ltd", "bcagdp",
    "bm_rgrowth", "bmgdp", "bmtr", "sprr"
  )
)


# Create model datasets
panels_ml <- map(
  specs,
  \(vars) panel_temp |>
    select(all_of(vars)) |>
    drop_na()
  )

# Training samples
train <- map(
  panels_ml,
  \(data) filter(data, year <= 2004)
  )


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

# Fitting models in-sample
models <- list()

formulas <- list(
  baseline = precrisis3 ~ advanced + cgdppriv + govcgdp + tlpriv_rgrowth,
  broad = precrisis3 ~ advanced + cgdppriv + govcgdp + tlpriv_rgrowth + rgdpgrowth + inflation + ltd  + bcagdp,
  full = precrisis3 ~ advanced + cgdppriv + govcgdp + tlpriv_rgrowth + rgdpgrowth + inflation + ltd  + bcagdp + bm_rgrowth + bmgdp + bmtr + sprr
  # Model8 = precrisis3 ~ advanced + cgdpcorp + cgdph + tlcorp_rgrowth + tlh_rgrowth + rgdpgrowth + inflation + ltd + govcgdp + bcagdp + bm_rgrowth + bmgdp + bmtr + ycurve + sprr + ppgrowth
)

# Fit models
models <- lapply(
  formulas,
  \(f) glm(
    formula = f,
    data = panel_logit,
    family = binomial()
  )
)

# Summarize results
modelsummary(
  models,
  title = "Title",
  gof_function = gof_custom,
  gof_omit = "RMSE|Log.Lik.|Std.Errors|AUC",
  coef_omit = "(Intercept)",
  stars = TRUE,
  vcov = ~iso3c
)

## 5.2 Out-of-sample ==========================================================

forecast_logit <- function(formula, data, forecast_years) {
  
  predictions <- lapply(forecast_years, function(test_year) {
    
    # Expanding training window
    train <- data |>
      filter(year < test_year)
    
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

forecast_years <- 2005:2022

logit_oos_results <- lapply(
  X = formulas,
  FUN = forecast_logit,
  data = panel_logit,
  forecast_years = forecast_years
)

names(logit_oos_results) <- names(formulas)

evaluate <- function(results) {
  
  roc <- roc_auc(
    results,
    truth = precrisis3,
    .pred_1,
    event_level = "second"
  )
  
  pr <- pr_auc(
    results,
    truth = precrisis3,
    .pred_1,
    event_level = "second"
  )
  
  rbind(roc, pr)
}

# Compute AUC

logit_auc <- map(logit_oos_results, evaluate) |> list_rbind(names_to = "specification")

# 4. Cross-validation =========================================================

cv_periods <- tibble(
  id = paste0("Fold", 1:10),
  analysis_start = 1970,
  analysis_end = 1994:2003,
  assessment_start = 1995:2004,
  assessment_end = 1995:2004
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


make_folds <- function(data) {
  
  manual_rset(
    splits = pmap(
      cv_periods[, c(
        "analysis_start",
        "analysis_end",
        "assessment_start",
        "assessment_end"
      )],
      \(analysis_start, analysis_end,
        assessment_start, assessment_end) {
        
        make_split(
          data,
          analysis_start,
          analysis_end,
          assessment_start,
          assessment_end
        )
      }
    ),
    ids = cv_periods$id
  )
}


folds <- map(train, make_folds)


# 4.1 Random Forests ===========================================================

# Generic random forest specification
make_rf_workflow <- function(data) {
  
  recipe(precrisis3 ~ ., data = data) |>
    step_rm(iso3c, year) |>
    workflow() |>
    add_model(
      rand_forest(
        trees = 500,
        mtry = tune(),
        min_n = tune()
      ) |>
        set_engine(
          "ranger",
          importance = "permutation"
        ) |>
        set_mode("classification")
    )
  }


# Hyperparameter grid
make_rf_grid <- function(data, size = 30) {
  
  n_predictors <- data |>
    select(-precrisis3, -iso3c, -year) |>
    ncol()
  
  grid_random(
    mtry(range = c(2L, n_predictors)),
    min_n(range = c(2L, 30L)),
    size = size
  )
}


# Tune one model
tune_model <- function(data, folds, model_type = c("rf", "svm", "mlp"), grid_size = 30) {
  
  model_type <- match.arg(model_type)
  
  workflow_fn <- match.fun(paste0("make_", model_type, "_workflow"))
  grid_fn     <- match.fun(paste0("make_", model_type, "_grid"))
  
  workflow <- workflow_fn(data)
  grid     <- grid_fn(data, grid_size)
  
  tune_grid(
    workflow,
    resamples = folds,
    grid = grid,
    metrics = metric_set(pr_auc),
    control = control_grid(save_pred = T, verbose = T)
  )
}

# Tune all specifications
set.seed(123)

n_cores <- detectCores() - 1

cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

rf_tuned <- map2(train, folds, \(d, f) tune_model(d, f, model_type = "rf"))

stopCluster(cl)


# Best hyperparameters
best_rf <- map(
  rf_tuned,
  \(x) select_best(x, metric = "pr_auc")
)


# Display best hyperparameters
map(best_rf, \(x) x)


# 4.2 Pseudo-OOS forecasting ================================================

forecast_years <- 2005:2022

forecast_model <- function(data, best_params, forecast_years, model_type = c("rf", "svm", "mlp")) {
  
  model_type <- match.arg(model_type)
  
  workflow_fn <- match.fun(paste0("make_", model_type, "_workflow"))
  
  final_workflow <- workflow_fn(data) |>
    finalize_workflow(best_params)
  
  map(
    forecast_years,
    function(y) {
      
      message("Forecasting year: ", y)
      
      train_data <- data |>
        filter(year >= 1970, year <= y - 1)
      
      test_data <- data |>
        filter(year == y)
      
      fit <- final_workflow |>
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
  ) |>
    list_rbind()
}


# Forecast all three specifications

rf_oos_results <- map2(
  panels_ml,
  best_rf,
  \(data, params) forecast_model(data, params, forecast_years, model_type = "rf")
)

# 4.3 Evaluation =============================================================


rf_auc <- map(rf_oos_results, evaluate) |> list_rbind(names_to = "specification")

# 5. Support Vector Machines ==================================================

# Generic SVM workflow
make_svm_workflow <- function(data) {
  
  recipe(precrisis3 ~ ., data = data) |>
    step_rm(iso3c, year) |>
    step_normalize(all_predictors()) |>
    step_upsample(
      precrisis3,
      over_ratio = 1
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


# Hyperparameter grid
make_svm_grid <- function(data, size = 30) {
  
  grid_space_filling(
    cost(),
    rbf_sigma(),
    size = size
  )
}

# Tune all specifications =====================================================

set.seed(123)

cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

svm_tuned <- map2(train, folds, \(d, f) tune_model(d, f, model_type = "svm"))

stopCluster(cl)


# Best hyperparameters ========================================================

best_svm <- map(
  svm_tuned,
  \(x) select_best(x, metric = "pr_auc")
)


best_svm

# Forecast all three specifications ===========================================

svm_oos_results <- map2(
  panels_ml,
  best_svm,
  \(data, params) forecast_model(data, params, forecast_years, model_type = "svm")
)

# Evaluate SVMs ===============================================================

svm_auc <- map(svm_oos_results, evaluate) |> list_rbind(names_to = "specification")

# 6. Multilayer Perceptrons ===================================================

make_mlp_workflow <- function(data) {
  
  recipe(precrisis3 ~ ., data = data) |>
    step_rm(iso3c, year) |>
    step_normalize(all_predictors()) |>
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

make_mlp_grid <- function(data, size = 10) {
  
  grid_space_filling(
    hidden_units(range = c(1L, 5L)),
    penalty(range = c(-5, 0)),
    size = size
  )
}

# Tune all MLP specifications ================================================

set.seed(123)

cl <- makePSOCKcluster(n_cores)

registerDoParallel(cl)

mlp_tuned <- map2(train, folds, \(d, f) tune_model(d, f, model_type = "mlp"))

stopCluster(cl)



best_mlp <- map(
  mlp_tuned,
  \(x) select_best(x, metric = "roc_auc")
)

best_mlp

mlp_oos_results <- map2(
  panels_ml,
  best_mlp,
  \(data, params) forecast_model(data, params, forecast_years, model_type = "mlp")
)


mlp_auc <- map(mlp_oos_results, evaluate) |> list_rbind(names_to = "specification")


# Evaluate all ML results ===================================================
model_auc <- bind_rows(
  
  logit_auc |> 
    mutate(model = "Logit"),
  
  rf_auc |>
    mutate(model = "Random Forest"),
  
  svm_auc |>
    mutate(model = "SVM"),
  
  mlp_auc |>
    mutate(model = "MLP")
  
  ) |>
  select(
    specification,
    model,
    .metric,
    .estimate
  )

model_auc |> 
  pivot_wider(
    names_from = "specification",
    values_from = ".estimate"
  ) |> 
  arrange(.metric)


# ROC and PR Curves =================================================================

oos_results <- list(
  Logit = logit_oos_results,
  MLP   = mlp_oos_results,
  SVM   = svm_oos_results,
  RF    = rf_oos_results
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

ggplot(
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
    x = "False Positive Rate",
    y = "True Positive Rate"
  )

ggplot(
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
    x = "Recall",
    y = "Precision"
  )


# Shapley Values =============================================================


forecast_rf_shap <- function(data, best_params, forecast_years) {
  
  final_workflow <- make_rf_workflow(data) |>
    finalize_workflow(best_params)
  
  map(
    forecast_years,
    function(y) {
      
      train_data <- data |>
        filter(year >= 1970, year <= y - 1)
      
      test_data <- data |>
        filter(year == y)
      
      # Fit model
      fit <- final_workflow |>
        fit(data = train_data)
      
      # IMPORTANT:
      # Keep iso3c and year because the workflow expects them.
      X_test <- test_data |>
        select(-precrisis3)
      
      X_background <- train_data |>
        select(-precrisis3)
      
      # Calculate SHAP values
      shap <- permshap(
        fit,
        X = X_test,
        bg_X = X_background,
        type = "prob",
        seed = 123
      )
      
      list(
        year = y,
        shap = shap,
        data = test_data
      )
    }
  )
}


rf_full_shap <- forecast_rf_shap(
  data = panels_ml$full,
  best_params = best_rf$full,
  forecast_years = 2005:2022
)


# Robustness Checks ==========================================================




# Mülleimer ==================================================================

# 4 Preprocessing ===========================================================

# panel_temp <- panel |> 
#   # Remove crisis years
#   filter(crisis != 1) |> 
#   # Convert precrisis variable into a factor
#   mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))
# 
# # Baseline model panel
# panel_ml1 <- panel_temp |> 
#   # Select predictors and important columns
#   select(iso3c, year, precrisis3, advanced, cgdppriv, tlpriv_rgrowth, govcgdp) |> 
#   # Drop NAs
#   drop_na()
# 
# # Broad model panel
# panel_ml2 <- panel_temp |> 
#   # Select predictors and important columns
#   select(iso3c, year, precrisis3, advanced, cgdppriv, tlpriv_rgrowth, govcgdp, inflation, rgdpgrowth, ltd, bcagdp) |> 
#   # Drop NAs
#   drop_na()
# 
# # Full model panel
# panel_ml3 <- panel_temp |> 
#   # Select predictors and important columns
#   select(iso3c, year, precrisis3, advanced, cgdppriv, tlpriv_rgrowth, govcgdp, inflation, rgdpgrowth, ltd, bcagdp, bm_rgrowth, bmgdp, bmtr, sprr) |> 
#   # Drop NAs
#   drop_na()
# 
# # 4 Estimate Machine Learning Models =========================================
# 
# # Create training sets
# train1 <- panel_ml1 |>
#   filter(year <= 2004)
# 
# train2 <- panel_ml2 |>
#   filter(year <= 2004)
# 
# train3 <- panel_ml3 |>
#   filter(year <= 2004)
# 
# # Create cross-validation folds
# cv_periods <- tibble(
#   id = paste0("Fold", 1:14),
#   analysis_start = 1970,
#   analysis_end = 1990:2003,
#   assessment_start = 1991:2004,
#   assessment_end = 1991:2004
# )
# 
# make_split <- function(data, analysis_start, analysis_end, assessment_start, assessment_end) {
#   
#   make_splits(
#     x = list(
#       analysis = which(data$year >= analysis_start & data$year <= analysis_end),
#       assessment = which(data$year >= assessment_start & data$year <= assessment_end)
#     ),
#     data = data
#   )
# }
# 
# folds1 <- manual_rset(
#   splits = pmap(
#     list(
#       cv_periods$analysis_start,
#       cv_periods$analysis_end,
#       cv_periods$assessment_start,
#       cv_periods$assessment_end
#     ),
#     ~ make_split(
#       train1,
#       ..1, ..2, ..3, ..4
#     )
#   ),
#   ids = cv_periods$id
# )
# 
# folds2 <- manual_rset(
#   splits = pmap(
#     list(
#       cv_periods$analysis_start,
#       cv_periods$analysis_end,
#       cv_periods$assessment_start,
#       cv_periods$assessment_end
#     ),
#     ~ make_split(
#       train2,
#       ..1, ..2, ..3, ..4
#     )
#   ),
#   ids = cv_periods$id
# )
# 
# folds3 <- manual_rset(
#   splits = pmap(
#     list(
#       cv_periods$analysis_start,
#       cv_periods$analysis_end,
#       cv_periods$assessment_start,
#       cv_periods$assessment_end
#     ),
#     ~ make_split(
#       train3,
#       ..1, ..2, ..3, ..4
#     )
#   ),
#   ids = cv_periods$id
# )
# 
# forecast_years <- 2005:2022
# 
# # 4.1 Random Forests =========================================================
# 
# # Baseline Model
# 
# rec_rf1 <- recipe(precrisis3 ~ ., data = panel_ml1) |>
#   step_rm(iso3c, year)
# 
# mod_rf1 <- rand_forest(
#   trees = 500,
#   mtry = tune(),
#   min_n = tune()
#   ) |>
#   set_engine(
#     engine = "ranger",
#     importance = "permutation"
#   ) |>
#   set_mode("classification")
# 
# wf_rf1 <- workflow() |>
#   add_recipe(rec_rf1) |>
#   add_model(mod_rf1)
# 
# # Number of predictors
# n_predictors1 <- train1 |>
#   select(-precrisis3, -iso3c, -year) |>
#   ncol()
# 
# # Create the grid
# set.seed(123)
# 
# rf_grid1 <- grid_random(
#   mtry(range = c(2L, n_predictors1)),
#   min_n(range = c(2L, 30L)),
#   size = 30
# )
# 
# 
# # Parallel processing
# n_cores <- detectCores()
# n_cores
# 
# cl <- makePSOCKcluster(n_cores - 1)
# registerDoParallel(cl)
# 
# 
# # Tune Hyperparameters
# rf_tuned1 <- tune_grid(
#   wf_rf1,
#   resamples = folds1,
#   grid = rf_grid1,
#   metrics = metric_set(roc_auc),
#   control = control_grid(save_pred = TRUE, verbose = TRUE)
# )
# 
# stopCluster(cl)
# 
# show_best(rf_tuned1, metric = "roc_auc")
# 
# best_rf1 <- select_best(rf_tuned1, metric = "roc_auc")
# 
# # Actual pseudo oos forecasting
# 
# final_wf1 <- finalize_workflow(wf_rf1, best_rf1)
# 
# rf_oos_results1 <- map_dfr(
#   forecast_years,
#   function(y) {
#     
#     train_data <- panel_ml1 |>
#       filter(.data$year >= 1970, .data$year <= .env$y - 1)
#     
#     test_data <- panel_ml1 |>
#       filter(.data$year == .env$y)
#     
#     fit <- final_wf1 |>
#       fit(data = train_data)
#     
#     predictions <- predict(
#       fit,
#       new_data = test_data,
#       type = "prob"
#     )
#     
#     test_data |>
#       select(iso3c, year, precrisis3) |>
#       bind_cols(predictions)
#   }
# )
# 
# 
# # Evaluate
# roc_auc(
#   rf_oos_results1,
#   truth = precrisis3,
#   .pred_1,
#   event_level = "second"
# )
# 
# rf_oos_results1 |>
#   roc_curve(
#     truth = precrisis3,
#     .pred_1,
#     event_level = "second"
#   ) |>
#   autoplot()
# 
# # Broad model
# 
# 
# rec_rf2 <- recipe(precrisis3 ~ ., data = panel_ml2) |>
#   step_rm(iso3c, year)
# 
# mod_rf2 <- rand_forest(
#   trees = 500,
#   mtry = tune(),
#   min_n = tune()
# ) |>
#   set_engine(
#     engine = "ranger",
#     importance = "permutation"
#   ) |>
#   set_mode("classification")
# 
# wf_rf2 <- workflow() |>
#   add_recipe(rec_rf2) |>
#   add_model(mod_rf2)
# 
# # Number of predictors
# n_predictors2 <- train2 |>
#   select(-precrisis3, -iso3c, -year) |>
#   ncol()
# 
# # Create the grid
# set.seed(123)
# 
# rf_grid2 <- grid_random(
#   mtry(range = c(2L, n_predictors2)),
#   min_n(range = c(2L, 30L)),
#   size = 30
# )
# 
# 
# # Parallel processing
# n_cores <- detectCores()
# n_cores
# 
# cl <- makePSOCKcluster(n_cores - 1)
# registerDoParallel(cl)
# 
# 
# # Tune Hyperparameters
# rf_tuned2 <- tune_grid(
#   wf_rf2,
#   resamples = folds2,
#   grid = rf_grid2,
#   metrics = metric_set(roc_auc),
#   control = control_grid(save_pred = TRUE, verbose = TRUE)
# )
# 
# stopCluster(cl)
# 
# show_best(rf_tuned2, metric = "roc_auc")
# 
# best_rf2 <- select_best(rf_tuned2, metric = "roc_auc")
# 
# # Actual pseudo oos forecasting
# 
# final_wf2 <- finalize_workflow(wf_rf2, best_rf2)
# 
# rf_oos_results2 <- map_dfr(
#   forecast_years,
#   function(y) {
#     
#     train_data <- panel_ml2 |>
#       filter(.data$year >= 1970, .data$year <= .env$y - 1)
#     
#     test_data <- panel_ml2 |>
#       filter(.data$year == .env$y)
#     
#     fit <- final_wf2 |>
#       fit(data = train_data)
#     
#     predictions <- predict(
#       fit,
#       new_data = test_data,
#       type = "prob"
#     )
#     
#     test_data |>
#       select(iso3c, year, precrisis3) |>
#       bind_cols(predictions)
#   }
# )
# 
# 
# # Evaluate
# roc_auc(
#   rf_oos_results2,
#   truth = precrisis3,
#   .pred_1,
#   event_level = "second"
# )
# 
# rf_oos_results2 |>
#   roc_curve(
#     truth = precrisis3,
#     .pred_1,
#     event_level = "second"
#   ) |>
#   autoplot()
# 
# 
# # Full model
# 
# rec_rf3 <- recipe(precrisis3 ~ ., data = panel_ml3) |>
#   step_rm(iso3c, year)
# 
# mod_rf3 <- rand_forest(
#   trees = 500,
#   mtry = tune(),
#   min_n = tune()
# ) |>
#   set_engine(
#     engine = "ranger",
#     importance = "permutation"
#   ) |>
#   set_mode("classification")
# 
# wf_rf3 <- workflow() |>
#   add_recipe(rec_rf3) |>
#   add_model(mod_rf3)
# 
# # Number of predictors
# n_predictors3 <- train3 |>
#   select(-precrisis3, -iso3c, -year) |>
#   ncol()
# 
# # Create the grid
# set.seed(123)
# 
# rf_grid3 <- grid_random(
#   mtry(range = c(2L, n_predictors3)),
#   min_n(range = c(2L, 30L)),
#   size = 30
# )
# 
# 
# # Parallel processing
# n_cores <- detectCores()
# n_cores
# 
# cl <- makePSOCKcluster(n_cores - 1)
# registerDoParallel(cl)
# 
# 
# # Tune Hyperparameters
# rf_tuned3 <- tune_grid(
#   wf_rf3,
#   resamples = folds3,
#   grid = rf_grid3,
#   metrics = metric_set(roc_auc),
#   control = control_grid(save_pred = TRUE, verbose = TRUE)
# )
# 
# stopCluster(cl)
# 
# show_best(rf_tuned3, metric = "roc_auc")
# 
# best_rf3 <- select_best(rf_tuned3, metric = "roc_auc")
# 
# # Actual pseudo oos forecasting
# 
# final_wf3 <- finalize_workflow(wf_rf3, best_rf3)
# 
# rf_oos_results3 <- map_dfr(
#   forecast_years,
#   function(y) {
#     
#     train_data <- panel_ml3 |>
#       filter(.data$year >= 1970, .data$year <= .env$y - 1)
#     
#     test_data <- panel_ml3 |>
#       filter(.data$year == .env$y)
#     
#     fit <- final_wf3 |>
#       fit(data = train_data)
#     
#     predictions <- predict(
#       fit,
#       new_data = test_data,
#       type = "prob"
#     )
#     
#     test_data |>
#       select(iso3c, year, precrisis3) |>
#       bind_cols(predictions)
#   }
# )
# 
# 
# # Evaluate
# roc_auc(
#   rf_oos_results3,
#   truth = precrisis3,
#   .pred_1,
#   event_level = "second"
# )
# 
# rf_oos_results3 |>
#   roc_curve(
#     truth = precrisis3,
#     .pred_1,
#     event_level = "second"
#   ) |>
#   autoplot()






















# 4.2 Support Vector Machine ================================================

# # SVM recipe
# 
# rec_svm <- recipe(precrisis3 ~ ., data = panel_ml) |>
#   step_rm(iso3c, year) |>
#   step_normalize(all_numeric_predictors())
# 
# # SVM model
# 
# mod_svm <- svm_rbf(
#   cost = tune(),
#   rbf_sigma = tune()
# ) |>
#   set_engine("kernlab") |>
#   set_mode("classification")
# 
# 
# # Workflow
# 
# wf_svm <- workflow() |>
#   add_recipe(rec_svm) |>
#   add_model(mod_svm)
# 
# 
# # Hyperparameter grid
# 
# set.seed(123)
# 
# svm_grid <- grid_regular(
#   cost(range = c(-3, 3)),
#   rbf_sigma(range = c(-3, 0)),
#   levels = 7
# )
# 
# 
# # Tune
# 
# svm_tuned <- tune_grid(
#   wf_svm,
#   resamples = folds,
#   grid = svm_grid,
#   metrics = metric_set(roc_auc),
#   control = control_grid(save_pred = T, verbose = T)
# )
# 
# 
# # Results
# 
# show_best(
#   svm_tuned,
#   metric = "roc_auc"
# )
# 
# # Select best model
# 
# best_svm <- select_best(
#   svm_tuned,
#   metric = "roc_auc"
# )
# 
# 
# # Final model
# 
# final_svm_wf <- finalize_workflow(
#   wf_svm,
#   best_svm
# )
# 
# # Out-of-sample predictions
# 
# svm_oos_results <- map_dfr(
#   forecast_years,
#   function(y) {
#     
#     train_data <- panel_ml |>
#       filter(.data$year >= 1970, .data$year <= .env$y - 1)
#     
#     test_data <- panel_ml |>
#       filter(.data$year == .env$y)
#     
#     fit <- final_svm_wf |>
#       fit(data = train_data)
#     
#     predictions <- predict(
#       fit,
#       new_data = test_data,
#       type = "prob"
#     )
#     
#     test_data |>
#       select(iso3c, year, precrisis3) |>
#       bind_cols(predictions)
#   }
# )
# 
# 
# # Evaluation
# 
# roc_auc(
#   svm_oos_results,
#   truth = precrisis3,
#   .pred_1,
#   event_level = "second"
# )
# 
# # ROC curve
# 
# svm_oos_results |>
#   mutate(
#     precrisis3 = factor(
#       precrisis3,
#       levels = c(0, 1)
#     )
#   ) |>
#   roc_curve(
#     truth = precrisis3,
#     .pred_1,
#     event_level = "second"
#   ) |>
#   autoplot()






# prep_svm <- prep(rec_svm)
# 
# train_processed <- bake(
#   prep_svm,
#   new_data = NULL
# )
# 
# summary(train_processed)






## 4.3 Multilayer Perceptron ==================================================

# rec_mlp <- recipe(precrisis3 ~ ., data = panel_ml) |>
#   step_rm(iso3c, year) |>
#   step_normalize(all_numeric_predictors())
# 
# mod_mlp <- mlp(
#   hidden_units = tune(),
#   penalty = tune(),
#   epochs = 100,
#   activation = "relu"
# ) |>
#   set_engine("brulee") |>
#   set_mode("classification")
# 
# wf_mlp <- workflow() |>
#   add_recipe(rec_mlp) |>
#   add_model(mod_mlp)
# 
# # Tune Hyperparameters
# set.seed(123)
# 
# mlp_grid <- grid_regular(
#   hidden_units(range = c(2L, 20L)),
#   penalty(range = c(-6, 1)),
#   levels = c(5,5)
# )
# 
# mlp_tuned <- tune_grid(
#   wf_mlp,
#   resamples = folds,
#   grid = mlp_grid,
#   metrics = metric_set(roc_auc),
#   control = control_grid(save_pred = T, verbose = T)
# )
# 
# show_best(
#   mlp_tuned,
#   metric = "roc_auc"
# )
# 
# best_mlp <- select_best(
#   mlp_tuned,
#   metric = "roc_auc"
# )
# 
# final_wf_mlp <- finalize_workflow(
#   wf_mlp,
#   best_mlp
# )
# 
# mlp_oos_results <- map_dfr(
#   forecast_years,
#   function(y) {
#     
#     train_data <- panel_ml |>
#       filter(.data$year >= 1970, .data$year <= .env$y - 1)
#     
#     test_data <- panel_ml |>
#       filter(.data$year == .env$y)
#     
#     fit <- final_wf_mlp |>
#       fit(data = train_data)
#     
#     predictions <- predict(
#       fit,
#       new_data = test_data,
#       type = "prob"
#     )
#     
#     test_data |>
#       select(iso3c, year, precrisis3) |>
#       bind_cols(predictions)
#   }
# )
# 
# roc_auc(
#   mlp_oos_results,
#   truth = precrisis3,
#   .pred_1,
#   event_level = "second"
# )
# 
# mlp_oos_results |>
#   roc_curve(
#     truth = precrisis3,
#     .pred_1,
#     event_level = "second"
#   ) |>
#   autoplot()

























