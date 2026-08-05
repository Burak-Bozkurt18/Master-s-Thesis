# Step 3: Estimate logit models
# Purpose:  
# Inputs:   
# Outputs:

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(modelsummary)
library(fixest)

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

model1 <- panel_fe |> 
  glm(formula = precrisis3 ~ factor(iso3c) + cgdppriv, family = binomial())

summary(model1)
modelsummary(model1)
# Driscoll-Kraay Standard Errors
se_felogit_DK = se(model1, vcov = "DK")
