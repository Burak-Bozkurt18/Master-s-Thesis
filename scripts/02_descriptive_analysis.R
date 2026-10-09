# Step 2: Descriptive Analysis
# Purpose:  
# Inputs:   All files in data/interim/indicators
# Outputs:  data/final

# 1 Load Packages =============================================================

library(tidyverse)
library(modelsummary)
library(patchwork)

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

p1_1 <- panel |> 
  ggplot(aes(x = year, y = crisis_start, fill = factor(advanced))) +
  geom_col() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(labels = c("Emerging and Developing Economies", "Advanced Economies"), values = c("#F8766D", "#00BFC4")) +
  labs(y = "Number of Crisis Starts") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

p1_2 <- panel |> 
  ggplot(aes(x = year, y = crisis_start, fill = factor(region))) +
  geom_col() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(y = "Number of Crisis Starts") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set2")

p1_1 / p1_2

crisis_starts <- panel |>
  filter(crisis_start == 1) |>
  select(iso3c, crisis_year = year)

panel_event <- panel |>
  inner_join(crisis_starts, by = "iso3c") |>
  mutate(
    event_time = year - crisis_year
  ) |>
  filter(event_time >= -5, event_time <= 5)

# Variables to plot
event_vars <- c(
  "cgdppriv_growth",
  "tlpriv_rgrowth",
  "govcgdp_growth",
  "rgdpgrowth",
  "inflation",
  "ltd",
  "bcagdp",
  "bmgdpgrowth",
  "bm_rgrowth",
  "bmtr",
  "sprr"
)

# Calculate summary statistics for each event time and variable
event_summary <- panel_event |>
  select(event_time, all_of(event_vars)) |>
  pivot_longer(
    cols = all_of(event_vars),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(event_time, variable) |>
  summarise(
    median = median(value, na.rm = TRUE),
    p25    = quantile(value, 0.25, na.rm = TRUE),
    p75    = quantile(value, 0.75, na.rm = TRUE),
    n      = sum(!is.na(value)),
    .groups = "drop"
  )

var_labs <- c(
  cgdppriv_growth = "Private credit-to-GDP growth",
  tlpriv_rgrowth   = "Private credit growth (real)",
  govcgdp_growth   = "Gov. credit-to-GDP growth",
  rgdpgrowth       = "Real GDP growth",
  inflation        = "Inflation",
  ltd              = "Loans-to-deposits ratio",
  bcagdp           = "Current account balance / GDP",
  bmgdpgrowth      = "Broad-money-to-GDP growth",
  bm_rgrowth       = "Broad money growth (real)",
  bmtr             = "Broad money / total reserves",
  sprr             = "Stock price returns (real)"
)

event_summary <- event_summary |>
  mutate(
    variable = recode(variable, !!!var_labs)
  )

ggplot(event_summary, aes(x = event_time)) +
  
  # Interquartile range
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2) +
  
  # Median
  geom_line(aes(y = median), linewidth = 0.8) +
  
  # Crisis onset
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  
  labs(
    x = "Years relative to crisis onset",
    y = NULL
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(-5,5,1))




panel_expl <- panel |> 
  mutate(
    `Real GDP growth (%)` = rgdpgrowth,
    `Inflation (%)` = inflation,
    `Total private credit-to-GDP growth (%)` = cgdppriv_growth,
    `Real total private credit growth (%)` = tlpriv_rgrowth,
    `Government credit-to-GDP growth (%)` = govcgdp_growth,
    `Current account balance (% of GDP)` = bcagdp,
    `Broad money (% of total reserves)` = bmtr,
    `Real broad money growth (%)` = bm_rgrowth,
    `Broad-money-to-GDP growth (%)` = bmgdpgrowth,
    `Bank loans-to-deposit ratio (%)` = ltd,
    `Real stock price returns (%)` = sprr,
    .keep = "none"
  )

panel_expl |> 
  datasummary(formula = All(panel_expl) ~ N + Mean + SD + Min + Median + Max)

panel_expl |> 
  datasummary(
    formula = All(panel_expl) ~ N + Mean + SD + Min + Median + Max,
    output = "latex")


