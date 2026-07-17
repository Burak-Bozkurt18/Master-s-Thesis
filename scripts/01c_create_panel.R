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
  left_join(crisis_years,
            by = c("country", "year")) |> 
  mutate(
    crisis = replace_na(crisis, 0)
  )

panel <- panel |> 
  left_join(crisis_start,
            by = c("country", "year")) |> 
  mutate(
    crisis_start = replace_na(crisis_start, 0)
  )

# Pre-crisis indicator
panel <- panel |>
  group_by(country) |>
  arrange(year) |>
  mutate(
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

# Nominal GDP (in units, millions and billions)
panel <- left_join(panel, indicators$ngdp |> select(iso3c, year, ngdp), by = c("iso3c", "year"))
panel <- left_join(panel, indicators$ngdpmil |> select(iso3c, year, ngdpmil), by = c("iso3c", "year"))
panel <- left_join(panel, indicators$ngdpbil |> select(iso3c, year, ngdpbil), by = c("iso3c", "year"))

# Real GDP growth
panel <- left_join(panel, indicators$rgdp_comb |> select(iso3c, year, rgdpgrowth), by = c("iso3c", "year"))

# Inflation
panel <- left_join(panel, indicators$infl_comb |> select(iso3c, year, inflation), by = c("iso3c", "year"))

# Private Credit-to-GDP ratio
panel <- left_join(panel, indicators$cgdppriv_comb |> select(iso3c, year, cgdppriv), by = c("iso3c", "year"))

# Corporate and household Credit-to-GDP ratio
panel <- left_join(panel, indicators$cgdpprivsplit, by = c("iso3c", "year"))

# Real credit growth
panel <- left_join(panel, indicators$credit_comb |>  select(year, iso3c, ends_with("rgrowth")), by = c("iso3c", "year"))

# Government Credit-to-GDP ratio
panel <- left_join(panel, indicators$govcgdp_comb |> select(iso3c, year, govcgdp), by = c("iso3c", "year"))

# Current Account Balance to GDP ratio
panel <- left_join(panel, indicators$bca_comb |> select(iso3c, year, bcagdp), by = c("iso3c", "year"))

# Real property price growth
panel <- left_join(panel, indicators$pp_comb |> select(iso3c, year, ppgrowth), by = c("iso3c", "year"))

# Net foreign assets to GDP
panel <- left_join(panel, indicators$nfa_comb |> select(year, iso3c, nfagdp), by = c("iso3c", "year"))

# Yield Curve
panel <- left_join(panel, indicators$ir_comb |> select(iso3c, year, ycurve), by = c("iso3c", "year"))

# Broad money to total reserves, real broad money growth and broad money to GDP
panel <- left_join(panel, indicators$bmoney |> select(year, iso3c, bmtr, bm_rgrowth),
                   by = c("iso3c", "year"))
panel <- left_join(panel, indicators$bmgdp |> select(year, iso3c, bmgdp), by = c("iso3c", "year"))

# Loans-to-Deposit ratio
panel <- left_join(panel, indicators$ltd_comb |> select(iso3c, year, ltd), by = c("iso3c", "year"))

# Real Stock Price returns
panel <- left_join(panel, indicators$sp_comb |> select(iso3c, year, sprr), by = c("iso3c", "year"))

# 5 Save Panel =================================================================
write_rds(panel, "data/final/panel.rds")

message("Step 1c: Panel saved to data/interim/final")


# Appendix =====================================================================


# Check how many observations each country has for each indicator

# check <- panel |>
#   group_by(country) |>
#   summarise(
#     across(
#       - (year:advanced),
#       ~ sum(!is.na(.x)),
#       .names = "n_{.col}"
#       )
# )


# Identify outliers and structural breaks

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
#   filter(!(variable %in% c("crisis", "crisis_start", "precrisis2", "precrisis3", "precrisis4", "advanced", "ngdpmil", "ngdpbil", "inflation"))) |> 
#   View()
# 
# panel |> filter(iso3c %in% c("MKD", "MNG", "TUR", "JAM", "ISL", "BGD", "UKR")) |> ggplot(aes(x = year, y = sprr, col = iso3c)) + geom_line( linewidth = 1)
# 
