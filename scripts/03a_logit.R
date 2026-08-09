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

# 4 Estimate Models ==========================================================

## 4.1 Logit ================================================================

panel_fe <- panel |> 
  group_by(country) |> 
  filter(crisis != 1, !is.na(cgdppriv)) |> 
  filter(any(precrisis3 == 1)) |> 
  ungroup()

panel_fe2 <- panel |> 
  group_by(country) |> 
  filter(crisis != 1, !is.na(cgdppriv), !is.na(cgdph), !is.na(cgdpcorp)) |> 
  filter(any(precrisis3 == 1)) |> 
  ungroup()

models <- list()

models[["Logit1"]] <- panel_fe |> 
  mutate(iso3c = factor(iso3c)) |> 
  glm(formula = precrisis3 ~ iso3c + cgdppriv, family = binomial())

models[["Logit2"]] <- panel_fe2 |> 
  glm(formula = precrisis3 ~ factor(iso3c) + cgdppriv + cgdph + cgdpcorp, family = binomial())

modelsummary(models, coef_omit = "^factor", stars = T, vcov = ~iso3c)

# 2. Calculate clustered standard errors and test coefficients
coeftest(models$Logit2, vcov = vcovCL, cluster = ~ iso3c)


ame <- avg_slopes(models$Logit1)



model <- panel_fe |> 
  mutate(iso3c = factor(iso3c)) |> 
  feglm(
    precrisis3 ~ cgdppriv | iso3c,
    family = "binomial"
    )

ame <- avg_slopes(model)



panel_fe_all <- panel |> 
  drop_na() |> 
  group_by(country) |> 
  filter(crisis != 1) |> 
  filter(any(precrisis3 == 1)) |> 
  ungroup()

model_all <- panel_fe_all |> 
  select(iso3c, precrisis3, rgdpgrowth:sprr) |> 
  glm(formula =  precrisis3 ~ factor(iso3c) + . -iso3c, family = binomial())

summary(model_all)
