# Step 2: Descriptive Analysis
# Purpose:  
# Inputs:   All files in data/interim/indicators
# Outputs:  data/final

# 1 Load Packages =============================================================

library(tidyverse)
library(tidymodels)
library(modelsummary)

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

datasummary_skim(panel |> select(!(year:advanced)))

datasummary(~  N * advanced * precrisis3,
            data = panel |> mutate(advanced = factor(advanced), precrisis3 = factor(precrisis3)))

panel |> datasummary(formula = All(panel |> select(!(year:advanced))) ~ N + Mean + SD + Min + Median + Max)

panel_expl <- panel |> 
  rename(
    "Real GDP growth (\\%)" = rgdpgrowth,
    "Inflation (\\%)" = inflation,
    "Total Private Credit (\\% of GDP)" = cgdppriv,
    "Bank Private Credit (\\% of GDP)" = bcgdppriv,
    "Corporate Credit (\\% of GDP)" = cgdpcorp,
    "Household Credit (\\% of GDP)" = cgdph,
    "Total Private Credit Growth (\\%)" = tlpriv_rgrowth,
    "Bank Private Credit Growth (\\%)" = blpriv_rgrowth,
    "Corporate Credit Growth (\\%)" = tlcorp_rgrowth,
    "Household Credit Growth (\\%)" = tlh_rgrowth,
    "Public Debt (\\% of GDP)" = govcgdp,
    "Current Account Balance (\\% of GDP)" = bcagdp,
    "Real Property Price growth (\\%)" = ppgrowth,
    "Net Foreign Assets (\\% of GDP)" = nfagdp,
    "Yield Curve" = ycurve,
    "Broad Money (\\% of Total Reserves)" = bmtr,
    "Broad Money growth (\\%)" = bm_rgrowth,
    "Broad Money (\\% of GDP)" = bmgdp,
    "Loans to Deposit (\\%)" = ltd,
    "Stock Price Returns (\\%)" = sprr
  )

panel_expl |> 
  select(!(country:advanced)) |> 
  datasummary_skim(
    fun_numeric = list(N = N, Mean = Mean, SD = SD, Min = Min, Median = Median, Max = Max),
    output = "latex")

panel_expl |> 
  filter(advanced == 1) |> 
  select(!c((country:precrisis3), advanced)) |> 
  datasummary_balance(formula = ~ precrisis4, output = "latex")

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