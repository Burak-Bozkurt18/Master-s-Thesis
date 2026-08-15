# Step 2: Descriptive Analysis
# Purpose:  
# Inputs:   All files in data/interim/indicators
# Outputs:  data/final

# 1 Load Packages =============================================================

library(tidyverse)
library(modelsummary)

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

# 4 Descriptive Analysis =====================================================

panel |> 
  ggplot(aes(x = year, y = crisis, fill = factor(advanced))) +
  geom_col() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(labels = c("Emerging and Developing Economies", "Advanced Economies"), values = c("#F8766D", "#00BFC4")) +
  labs(y = "Number of Crises")

datasummary(~  N * advanced * precrisis3,
            data = panel |> mutate(advanced = factor(advanced), precrisis3 = factor(precrisis3)))


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
  ) |> 
  mutate(advanced = factor(advanced))

panel_expl |> 
  datasummary(formula = All(panel_expl |> select(!(year:advanced))) ~ N + Mean + SD + Min + Median + Max)

panel_expl |> 
  datasummary(formula = All(panel_expl |> select(!(year:advanced))) ~ advanced * (N + Mean))


panel_expl |> 
  select(!(country:advanced)) |> 
  datasummary_skim(
    fun_numeric = list(N = N, Mean = Mean, SD = SD, Min = Min, Median = Median, Max = Max),
    output = "latex")

panel_expl |> 
  filter(advanced == 1) |> 
  select(!c((country:precrisis3), advanced)) |> 
  datasummary_balance(formula = ~ precrisis4, output = "latex")







## Restricted Sample

x_vars <- colnames(panel)[10:29]

# Percentage coverage of observations per variable
coverage <- panel_restricted |>
  group_by(advanced) |> 
  summarise(
    across(
      all_of(x_vars),
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

# Coverage per row in %
panel_mod <- panel |>
  mutate(
    n_available = rowSums(!is.na(across(all_of(x_vars)))),
    coverage = n_available / length(x_vars)
  )

# Drop rows with variable coverage less than 50%
panel_restricted <- panel_mod |>
  filter(coverage >= 0.50)
