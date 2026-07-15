# Step 2: Descriptive Analysis
# Purpose:  
# Inputs:   All files in data/interim/indicators
# Outputs:  data/final

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(stargazer)

# 2 Load Panel ===============================================================

panel <- read_rds("data/final/panel.rds")

# 3 Descriptive Analysis =====================================================

panel |> 
  ggplot(aes(x = year, y = crisis_start, fill = factor(advanced))) +
  geom_col() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

stargazer(
  as.data.frame(panel) |> select(!(year:ngdpbil)), 
  type = "text", 
  title="Descriptive statistics", 
  digits = 1,
  median = T
)


# 4 Estimate Models ==========================================================

## 4.1 Logit ================================================================

model1 <- panel |> 
  filter(Crisis != 1) |> 
  glm(formula = PreCrisis3 ~ factor(iso3c) + cgdppriv, family = binomial())

summary(model1)

log_spec <- logistic_reg() |>
  set_engine("glm")

log_fit <- logistic_reg() |> 
  set_engine("glm") |> 
  fit(factor(PreCrisis3) ~ factor(iso3c) + cgdppriv, data = filter(panel, Crisis != 1))

log_fit$fit$coefficients