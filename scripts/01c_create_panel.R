# Step 1c: Create final panel
# Purpose:  
# Inputs:   All files in data/interim/indicators
# Outputs:  data/final

# 1 Load Packages ==========================================================
library(tidyverse)
library(countrycode)

# 2 Load indicators ========================================================

ind.files <- list.files("data/interim/indicators", pattern = "\\.rds$", full.names = TRUE)

indicators <- ind.files |>
  set_names(tools::file_path_sans_ext(basename(ind.files))) |>
  map(read_rds)

crisis_start <- read_rds("data/interim/cleaned_datasets/crisis_start.rds")
crisis_years <- read_rds("data/interim/cleaned_datasets/crisis_years.rds")
crises_merged <- read_rds("data/interim/cleaned_datasets/crises_merged.rds")

# 3 Create country-year panel ==============================================

# Country-Year Panel
panel <- expand_grid(
  country = unique(crises_merged$country),
  year = 1970:2025
)

# Add country codes
panel <- panel |> 
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

panel <- panel |> 
  left_join(crisis_years, by = c("country", "year")) |> 
  mutate(crisis = replace_na(crisis, 0))

panel <- panel |> 
  left_join(crisis_start, by = c("country", "year")) |> 
  mutate(crisis_start = replace_na(crisis_start, 0))

# Pre-crisis indicator
panel <- panel |>
  group_by(country) |>
  arrange(year) |>
  mutate(
    precrisis1 = as.integer(lead(crisis_start, 1, default = 0)),
    precrisis2 = as.integer(
      lead(crisis_start, 1, default = 0) +
        lead(crisis_start, 2, default = 0) > 0
    ),
    precrisis3 = as.integer(
      lead(crisis_start, 1, default = 0) +
        lead(crisis_start, 2, default = 0) +
        lead(crisis_start, 3, default = 0) > 0
    ),
    precrisis4 = as.integer(
      lead(crisis_start, 1, default = 0) +
        lead(crisis_start, 2, default = 0) +
        lead(crisis_start, 3, default = 0) + 
        lead(crisis_start, 4, default = 0) > 0
    )
  ) |>
  ungroup() |> 
  arrange(country)

# Categorize countries in Advanced Economies and Emerging/Developing Economies
advanced <- c(
  "AUS", "AUT", "BEL", "CAN", "CHE", "CYP", "CZE",
  "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR",
  "GRC", "HRV", "IRL", "ISL", "ISR", "ITA", "JPN",
  "KOR", "LTU", "LUX", "LVA", "MLT", "NLD", "NOR",
  "NZL", "PRT", "SGP", "SVK", "SVN", "SWE", "USA"
)

panel <- panel |>
  mutate(advanced = if_else(iso3c %in% advanced, 1, 0))


# 4 Add all indicators to the panel ==========================================

# Real GDP growth
panel <- left_join(panel, indicators$rgdp_final |> select(iso3c, year, rgdpgrowth), by = c("iso3c", "year"))

# Inflation
panel <- left_join(panel, indicators$infl_final |> select(iso3c, year, inflation), by = c("iso3c", "year"))

# Total Private Credit-to-GDP ratio
panel <- left_join(panel, indicators$cgdppriv_final |> select(iso3c, year, cgdppriv, cgdppriv_growth), by = c("iso3c", "year"))

# Corporate and household Credit-to-GDP ratio
panel <- left_join(panel, indicators$cgdpprivsplit, by = c("iso3c", "year"))

# Real credit growth
panel <- left_join(panel, indicators$tlpriv_final |>  select(year, iso3c, tlpriv_rgrowth), by = c("iso3c", "year"))
panel <- left_join(panel, indicators$tlcorp_final |>  select(year, iso3c, tlcorp_rgrowth), by = c("iso3c", "year"))
panel <- left_join(panel, indicators$tlh_final |>  select(year, iso3c, tlh_rgrowth), by = c("iso3c", "year"))

# Government Credit-to-GDP ratio
panel <- left_join(panel, indicators$govcgdp_final |> select(iso3c, year, govcgdp, govcgdp_growth), by = c("iso3c", "year"))

# Current Account Balance to GDP ratio
panel <- left_join(panel, indicators$bca_final |> select(iso3c, year, bcagdp), by = c("iso3c", "year"))

# Real property price growth
panel <- left_join(panel, indicators$pp_final |> select(iso3c, year, ppgrowth), by = c("iso3c", "year"))

# Net foreign assets to GDP
panel <- left_join(panel, indicators$nfa_final |> select(year, iso3c, nfagdp), by = c("iso3c", "year"))

# Yield Curve
panel <- left_join(panel, indicators$ycurve_final |> select(iso3c, year, ycurve), by = c("iso3c", "year"))

# Broad money to total reserves, real broad money growth and broad money to GDP
panel <- left_join(panel, indicators$bm_final |> select(year, iso3c, bm_rgrowth), by = c("iso3c", "year"))
panel <- left_join(panel, indicators$bmgdp_final |> select(year, iso3c, bmgdp, bmgdpgrowth), by = c("iso3c", "year"))
panel <- left_join(panel, indicators$bmtr_final |> select(year, iso3c, bmtr), by = c("iso3c", "year"))

# Loans-to-Deposit ratio
panel <- left_join(panel, indicators$ltd_final |> select(iso3c, year, ltd, ltd_growth), by = c("iso3c", "year"))

# Real Stock Price returns
panel <- left_join(panel, indicators$sp_final |> select(iso3c, year, sprr), by = c("iso3c", "year"))

# 5 Save Panel =================================================================
write_rds(panel, "data/final/panel.rds")

message("Step 1c: Panel saved to data/interim/final")


# Appendix =====================================================================


# # Identify outliers and structural breaks
# 
# find_large_changes <- function(data) {
# 
#   vars <- data |>
#     select(where(is.numeric), -year) |>
#     names()
# 
#   map_dfr(vars, function(var) {
# 
#     data |>
#       arrange(iso3c, year) |>
#       group_by(iso3c) |>
#       mutate(
#         change = abs(.data[[var]] - lag(.data[[var]]))
#       ) |>
#       ungroup() |>
#       slice_max(change, n = 20, with_ties = FALSE) |>
#       transmute(
#         variable = var,
#         iso3c,
#         year,
#         value = .data[[var]],
#         change
#       )
#   })
# }
# large_changes <- find_large_changes(panel)
# 
# large_changes |>
#   filter(!(variable %in% c("crisis", "crisis_start", "precrisis1", "precrisis2", 
#                            "precrisis3", "precrisis4", "advanced", "inflation"))) |>
#   View()
# 
# panel |> filter(iso3c %in% c("STP", "ZMB", "LBN", "LBR")) |> ggplot(aes(x = year, y = govcgdp, col = iso3c)) + geom_line( linewidth = 1)
# 
