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


# 4 Preprocessing ============================================================

# Remove crisis years
panel_logit <- panel |> 
  filter(crisis != 1) |> 
  mutate(advanced = factor(advanced))

# Check data coverage per variable

coverage <- panel_logit |>
  group_by(advanced) |> 
  summarise(
    across(
      rgdpgrowth:sprr,
      ~ mean(!is.na(.)) * 100
    )
  ) |>
  pivot_longer(
    cols = !advanced,
    names_to = "variable",
    values_to = "coverage_pct"
  ) |>
  pivot_wider(
    names_from = advanced,
    values_from = coverage_pct
  ) |> 
  arrange(desc(`0`))

coverage


# 5 Estimate Logit Models =====================================================

## 5.1 In-Sample ==============================================================


# Correlation between predictors
panel |> select(advanced:sprr) |> cor(use = "pairwise.complete.obs")

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
  Baseline = precrisis3 ~ advanced + cgdppriv + govcgdp + tlpriv_rgrowth,
  Broad = precrisis3 ~ advanced + cgdppriv + govcgdp + tlpriv_rgrowth + rgdpgrowth + inflation + ltd  + bcagdp,
  Full = precrisis3 ~ advanced + cgdppriv + govcgdp + tlpriv_rgrowth + rgdpgrowth + inflation + ltd  + bcagdp + bm_rgrowth + bmgdp + bmtr + sprr
  # Model8 = precrisis3 ~ advanced + cgdpcorp + cgdph + tlcorp_rgrowth + tlh_rgrowth + rgdpgrowth + inflation + ltd + govcgdp + bcagdp + bm_rgrowth + bmgdp + bmtr + ycurve + sprr + ppgrowth
)


models <- lapply(
  formulas,
  \(f) glm(
    formula = f,
    data = panel_logit,
    family = binomial()
  )
)


modelsummary(
  models,
  title = "Title",
  gof_function = gof_custom,
  gof_omit = "RMSE|Log.Lik.|Std.Errors|AUC",
  coef_omit = "(Intercept)",
  stars = TRUE,
  vcov = ~iso3c
)



# panel_logit |> select(-nfagdp) |> mutate(precrisis3 = factor(precrisis3)) |>  drop_na() |> group_by(year, precrisis3) |> summarize(n = n()) |> ggplot(aes(x = year, y = n, fill = precrisis3)) + geom_col()
# panel_logit |> select(-c(nfagdp, ppgrowth, ycurve)) |> mutate(precrisis3 = factor(precrisis3)) |>  drop_na() |> group_by(year, precrisis3) |> summarize(n = n()) |> ggplot(aes(x = year, y = n, fill = precrisis3)) + geom_col()
# panel_logit |> select(-c(nfagdp, ycurve, ppgrowth, tlcorp_rgrowth, tlh_rgrowth, cgdpcorp, cgdph)) |> mutate(precrisis3 = factor(precrisis3)) |>  drop_na() |> group_by(year, precrisis3) |> summarize(n = n()) |> ggplot(aes(x = year, y = n, fill = precrisis3)) + geom_col()
# panel_logit |> select(year, precrisis3, advanced, cgdppriv, tlpriv_rgrowth) |> mutate(precrisis3 = factor(precrisis3)) |>  drop_na() |> group_by(year, precrisis3) |> summarize(n = n()) |> ggplot(aes(x = year, y = n, fill = precrisis3)) + geom_col()



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
      mutate(pred_prob = predict(model, newdata = test, type = "response"))
    
  })
  
  bind_rows(predictions)
}

forecast_years <- 2005:2022

oos_predictions <- lapply(
  X = formulas,
  FUN = forecast_logit,
  data = panel_logit,
  forecast_years = forecast_years
)

names(oos_predictions) <- names(formulas)

oos_auc <- sapply(
  oos_predictions,
  \(x) as.numeric(auc(x$precrisis3, x$pred_prob))
)

round(oos_auc, 3)

roc_curves <- map(
  oos_predictions,
  \(x) {
    x |>
      mutate(
        precrisis3 = factor(
          precrisis3,
          levels = c(0, 1)
        )
      ) |>
      roc_curve(
        truth = precrisis3,
        pred_prob,
        event_level = "second"
      )
  }
)

sapply(roc_curves, autoplot)















# Mülleimer =================================

# # Explanatory variables
# x_vars <- colnames(panel)[-(1:9)]
# 
# # Transform panel
# logit_panel <- panel |>
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
# lag_vars <- names(logit_panel)[
#   grepl("_lag[123]$", names(logit_panel))
# ]
# 
# # Remove unnecessary columns
# logit_panel <- logit_panel |>
#   select(iso3c, year, precrisis3, all_of(x_vars), all_of(lag_vars))
# 
# # Standardize lagged explanatory variables
# logit_panel <- logit_panel |>
#   mutate(
#     across(
#       all_of(c(x_vars, lag_vars)),
#       ~ as.numeric(scale(.x)),
#       .names = "z_{.col}"
#     )
#   )
# 
# # Convert precrisis variable into a factor
# logit_panel <- logit_panel |>
#   mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))




# logit_panel_fe1 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(cgdppriv)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe2 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(cgdpcorp), !is.na(cgdph)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe3 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(tlpriv_rgrowth)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe4 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(tlcorp_rgrowth), !is.na(tlh_rgrowth)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe5 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(cgdppriv), !is.na(tlpriv_rgrowth)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe6 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(cgdpcorp), !is.na(cgdph), !is.na(tlcorp_rgrowth), !is.na(tlh_rgrowth)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe7 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(bcgdppriv)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# logit_panel_fe8 <- logit_panel |> 
#   group_by(iso3c) |> 
#   filter(!is.na(blpriv_rgrowth)) |> 
#   filter(any(precrisis3 == 1)) |> 
#   ungroup()
# 
# 
# 
# 
# models <- list()
# 
# models[["Logit1"]] <- logit_panel_fe1 |> 
#   mutate(iso3c = factor(iso3c)) |> 
#   glm(formula = precrisis3 ~ iso3c + cgdppriv, family = binomial())
# 
# models[["Logit2"]] <- logit_panel_fe2 |> 
#   mutate(iso3c = factor(iso3c)) |>
#   glm(formula = precrisis3 ~ iso3c + cgdpcorp + cgdph, family = binomial())
# 
# models[["Logit3"]] <- logit_panel_fe3 |> 
#   mutate(iso3c = factor(iso3c)) |> 
#   glm(formula = precrisis3 ~ iso3c + tlpriv_rgrowth, family = binomial())
# 
# models[["Logit4"]] <- logit_panel_fe4 |> 
#   mutate(iso3c = factor(iso3c)) |>
#   glm(formula = precrisis3 ~ iso3c + tlcorp_rgrowth + tlh_rgrowth, family = binomial())
# 
# models[["Logit5"]] <- logit_panel_fe5 |> 
#   mutate(iso3c = factor(iso3c)) |>
#   glm(formula = precrisis3 ~ iso3c + cgdppriv + tlpriv_rgrowth, family = binomial())
# 
# models[["Logit6"]] <- logit_panel_fe6 |> 
#   mutate(iso3c = factor(iso3c)) |>
#   glm(formula = precrisis3 ~ iso3c + cgdpcorp + cgdph + tlcorp_rgrowth + tlh_rgrowth, family = binomial())
# 
# models[["Logit7"]] <- logit_panel_fe7 |> 
#   mutate(iso3c = factor(iso3c)) |>
#   glm(formula = precrisis3 ~ iso3c + bcgdppriv, family = binomial())
# 
# models[["Logit8"]] <- logit_panel_fe8 |> 
#   mutate(iso3c = factor(iso3c)) |>
#   glm(formula = precrisis3 ~ iso3c + blpriv_rgrowth, family = binomial())
# 
# 
# 
# 
# gof_custom <- function(model) {
#   
#   # Predicted probabilities
#   p <- predict(model, type = "response")
#   
#   # Actual dependent variable
#   y <- model$model[[1]]
#   
#   # Number of countries
#   n_countries <- length(unique(model$model$iso3c))
#   
#   # AUC
#   auc_value <- round(as.numeric(auc(y, p)), digits = 3)
#   
#   # McFadden pseudo-R2
#   pseudo_r2 <- round(1 - as.numeric(logLik(model)) / as.numeric(logLik(update(model, . ~ 1))), digits = 3)
#   
#   data.frame(
#     "Num. Countries" = n_countries,
#     "AUC" = auc_value,
#     "McFadden R^2" = pseudo_r2
#   )
# }
# 
# 
# modelsummary(
#   models,
#   title = "Title",
#   gof_function = gof_custom,
#   gof_omit = "RMSE|Log.Lik.|Std.Errors|AUC",
#   coef_omit = "^iso3c|(Intercept)", 
#   stars = T, 
#   vcov = ~iso3c
# )


# # Calculate clustered standard errors and test coefficients
# coeftest(models$Logit2, vcov = vcovCL, cluster = ~ iso3c)


# # More efficient way
# 
# model_specs_broad <- list(
#   Logit1 = c("cgdppriv", "tlpriv_rgrowth"),
#   # Logit2 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth"),
#   Logit2 = c("bcgdppriv", "blpriv_rgrowth"),
#   Log1 = c("cgdppriv", "tlpriv_rgrowth", "inflation", "ltd", "govcgdp", "bcagdp", "nfagdp", "bm_rgrowth", "bmgdp", "bmtr"),
#   Log2 = c("bcgdppriv", "blpriv_rgrowth", "inflation", "ltd", "govcgdp", "bcagdp", "nfagdp", "bm_rgrowth", "bmgdp", "bmtr")
# )
# 
# model_specs_narrow <- list(
#   Model1 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth"),
#   Model2 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth", "ycurve"),
#   Model3 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth", "ppgrowth"),
#   Model4 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth", "sprr"),
#   Model5 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth", "ycurve", "ppgrowth", "sprr", "inflation", "ltd", "govcgdp", "bcagdp", "nfagdp", "bm_rgrowth", "bmgdp", "bmtr")
# )
# 
# models_broad <- lapply(model_specs_broad, function(vars) {
#   
#   data <- logit_panel |>
#     group_by(iso3c) |>
#     filter(
#       if_all(all_of(vars), ~ !is.na(.x))
#     ) |>
#     filter(any(precrisis3 == 1)) |> 
#     ungroup() |>
#     mutate(iso3c = factor(iso3c))
#   
#   formula <- reformulate(
#     c("iso3c", vars),
#     response = "precrisis3"
#   )
#   
#   glm(
#     formula = formula,
#     data = data,
#     family = binomial()
#   )
# })
# models_narrow <- lapply(model_specs_narrow, function(vars) {
#   
#   data <- logit_panel |>
#     group_by(iso3c) |>
#     filter(
#       if_all(all_of(vars), ~ !is.na(.x))
#     ) |>
#     filter(any(precrisis3 == 1)) |> 
#     ungroup() |>
#     mutate(iso3c = factor(iso3c))
#   
#   formula <- reformulate(
#     c("iso3c", vars),
#     response = "precrisis3"
#   )
#   
#   glm(
#     formula = formula,
#     data = data,
#     family = binomial()
#   )
# })
# 
# # Customized Modelsummary
# 
# gof_custom <- function(model) {
#   
#   # Predicted probabilities
#   p <- predict(model, type = "response")
#   
#   # Actual dependent variable
#   y <- model$model[[1]]
#   
#   # Number of countries
#   n_countries <- length(unique(model$model$iso3c))
#   
#   # AUC
#   auc_value <- as.numeric(pROC::auc(y, p))
#   
#   # Full model log-likelihood
#   ll_full <- as.numeric(logLik(model))
#   
#   # Data used to estimate the model
#   model_data <- model.frame(model)
#   
#   # FE-only model
#   fe_model <- glm(
#     precrisis3 ~ iso3c,
#     data = model_data,
#     family = binomial()
#   )
#   
#   # FE-only log-likelihood
#   ll_fe <- as.numeric(logLik(fe_model))
#   
#   # McFadden pseudo-R2 relative to FE-only model
#   pseudo_r2 <- 1 - ll_full / ll_fe
#   
#   data.frame(
#     `Num. Countries` = n_countries,
#     `AUC` = round(auc_value, 3),
#     `McFadden R²` = round(pseudo_r2, 3)
#   )
# }
# 
# modelsummary(
#   models_broad,
#   title = "Title",
#   gof_function = gof_custom,
#   gof_omit = "RMSE|Log.Lik.|Std.Errors",
#   coef_omit = "^iso3c|(Intercept)",
#   stars = TRUE,
#   vcov = ~iso3c
# )
# 
# modelsummary(
#   models_narrow,
#   title = "Title",
#   gof_function = gof_custom,
#   gof_omit = "RMSE|Log.Lik.|Std.Errors",
#   coef_omit = "^iso3c|(Intercept)",
#   stars = TRUE,
#   vcov = ~iso3c
# )






# # Interaction effects
# models_int <- list()
# 
# formulas_int <- list(
#   Model1 = precrisis3 ~ advanced + cgdppriv + tlpriv_rgrowth + sprr + ppgrowth + 
#     tlpriv_rgrowth * sprr + tlpriv_rgrowth * ppgrowth,
#   Model2 = precrisis3 ~ advanced + bcgdppriv + blpriv_rgrowth + sprr + ppgrowth +
#     blpriv_rgrowth * sprr + blpriv_rgrowth * ppgrowth,
#   Model3 = precrisis3 ~ advanced + cgdpcorp + cgdph + tlcorp_rgrowth + tlh_rgrowth + 
#     ppgrowth + sprr + tlcorp_rgrowth * ppgrowth + tlh_rgrowth * ppgrowth +
#     tlcorp_rgrowth * sprr + tlh_rgrowth * sprr,
#   Model4 = precrisis3 ~ advanced + cgdppriv + tlpriv_rgrowth + sprr + ppgrowth + 
#     rgdpgrowth + inflation + ltd + govcgdp + bcagdp + bm_rgrowth + bmgdp + bmtr +
#     tlpriv_rgrowth * sprr + tlpriv_rgrowth * ppgrowth
# )
# 
# 
# models_int <- lapply(
#   formulas_int,
#   \(f) glm(
#     formula = f,
#     data = panel_logit,
#     family = binomial()
#   )
# )
# 
# 
# modelsummary(
#   models_int,
#   title = "Title",
#   gof_function = gof_custom,
#   gof_omit = "RMSE|Log.Lik.|Std.Errors|AUC",
#   coef_omit = "(Intercept)",
#   stars = TRUE,
#   vcov = ~iso3c
# )


