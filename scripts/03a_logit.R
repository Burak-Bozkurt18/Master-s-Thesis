# Step 3: Estimate logit models
# Purpose:  
# Inputs:   
# Outputs:

# 1 Load Packages =============================================================

library(tidyverse)
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

# Explanatory variables
x_vars <- colnames(panel)[-(1:9)]

# Transform panel
logit_panel <- panel |>
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

lag_vars <- names(logit_panel)[
  grepl("_lag[123]$", names(logit_panel))
]

# Remove unnecessary columns
logit_panel <- logit_panel |>
  select(iso3c, year, precrisis3, all_of(x_vars), all_of(lag_vars))

# Standardize lagged explanatory variables
logit_panel <- logit_panel |>
  mutate(
    across(
      all_of(c(x_vars, lag_vars)),
      ~ as.numeric(scale(.x)),
      .names = "z_{.col}"
    )
  )

# Convert precrisis variable into a factor
logit_panel <- logit_panel |>
  mutate(precrisis3 = factor(precrisis3, levels = c(0, 1)))





# 5 Estimate Logit Models =====================================================

## 5.1 Logit ================================================================


# Correlation between predictors
panel |> select(rgdpgrowth:sprr) |> cor(use = "pairwise.complete.obs")



# More efficient way

model_specs <- list(
  Logit1 = c("cgdppriv"),
  Logit2 = c("cgdpcorp", "cgdph"),
  Logit3 = c("tlpriv_rgrowth"),
  Logit4 = c("tlcorp_rgrowth", "tlh_rgrowth"),
  Logit5 = c("cgdppriv", "tlpriv_rgrowth"),
  Logit6 = c("cgdpcorp", "cgdph", "tlcorp_rgrowth", "tlh_rgrowth"),
  Logit7 = c("bcgdppriv"),
  Logit8 = c("blpriv_rgrowth")
)



models <- lapply(model_specs, function(vars) {
  
  data <- logit_panel |>
    group_by(iso3c) |>
    filter(
      if_all(all_of(vars), ~ !is.na(.x))
    ) |>
    filter(any(precrisis3 == 1)) |> 
    ungroup() |>
    mutate(iso3c = factor(iso3c))
  
  formula <- reformulate(
    c("iso3c", vars),
    response = "precrisis3"
  )
  
  glm(
    formula = formula,
    data = data,
    family = binomial()
  )
})

# Customized Modelsummary

gof_custom <- function(model) {
  
  # Predicted probabilities
  p <- predict(model, type = "response")
  
  # Actual dependent variable
  y <- model$model[[1]]
  
  # Number of countries
  n_countries <- length(unique(model$model$iso3c))
  
  # AUC
  auc_value <- as.numeric(pROC::auc(y, p))
  
  # Full model log-likelihood
  ll_full <- as.numeric(logLik(model))
  
  # Data used to estimate the model
  model_data <- model.frame(model)
  
  # FE-only model
  fe_model <- glm(
    precrisis3 ~ iso3c,
    data = model_data,
    family = binomial()
  )
  
  # FE-only log-likelihood
  ll_fe <- as.numeric(logLik(fe_model))
  
  # McFadden pseudo-R2 relative to FE-only model
  pseudo_r2 <- 1 - ll_full / ll_fe
  
  data.frame(
    `Num. Countries` = n_countries,
    `AUC` = round(auc_value, 3),
    `McFadden R²` = round(pseudo_r2, 3)
  )
}

modelsummary(
  models,
  title = "Title",
  gof_function = gof_custom,
  gof_omit = "RMSE|Log.Lik.|Std.Errors",
  coef_omit = "^iso3c|(Intercept)",
  stars = TRUE,
  vcov = ~iso3c
)








# Mülleimer =================================

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



## Calculate AMEs
# ame1 <- avg_slopes(
#   models$Logit1, 
#   variables = "cgdppriv"
# )
# 
# ame2 <- avg_slopes(
#   models$Logit2, 
#   variables = c("cgdppriv", "cgdph", "cgdpcorp")
#   )
# 
# modelsummary(
#   list(
#     "Model 1" = ame1,
#     "Model 2" = ame2
#   ),
#   stars = T
# )
