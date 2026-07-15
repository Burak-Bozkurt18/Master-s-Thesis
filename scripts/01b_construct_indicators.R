# Step 1b: Create final panel
# Purpose:  
# Inputs:   All files in data/interim
# Outputs:  data/interim/

# 0 Load Packages ==========================================================
library(tidyverse)
library(countrycode)

# 1 Define Functions =======================================================

combine_longest_series <- function(data, indicator, sources) {
  
  counts <- data |>
    filter(year >= 1970, year <= 2025) |>
    group_by(iso3c) |>
    summarise(
      across(all_of(sources), ~sum(!is.na(.))),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(source = sources[which.max(c_across(all_of(sources)))]) |>
    ungroup() |>
    select(iso3c, source)
  
  data |>
    left_join(counts, by = "iso3c") |>
    rowwise() |>
    mutate(
      !!indicator := get(source)
    ) |>
    ungroup()
}

# 2 Load Datasets ==========================================================

files <- list.files("data/interim/cleaned_datasets", pattern = "\\.rds$", full.names = TRUE)

clean_data <- files |>
  set_names(tools::file_path_sans_ext(basename(files))) |>
  map(read_rds)

# 3 Create country-year panel ============================================

# Country-Year Panel
panel <- expand_grid(
  country = unique(clean_data$crises_merged$country),
  year = 1970:2025
)

# Add country codes
panel <- panel |> 
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"))

panel <- panel |> 
  left_join(clean_data$crisis_years,
            by = c("country", "year")) |> 
  mutate(
    crisis = replace_na(crisis, 0)
  )

panel <- panel |> 
  left_join(clean_data$crisis_start,
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


# 4 Create indicators ==================================================

## 2.4 GDP =============================================================

### 2.4.1 Nominal ======================================================

ngdp_nea <- clean_data$nea_clean |> select(iso3c, year, ngdp, ngdpmil, ngdpbil)
ngdp_weo <- clean_data$weo_clean |> select(iso3c, year, ngdp, ngdpmil, ngdpbil)
ngdp_wdi <- clean_data$wdi2_clean |> select(iso3c, year, ngdp, ngdpmil, ngdpbil)


# Combine datasets
ngdp_comb <- ngdp_nea |> 
  full_join(ngdp_weo, by = c("iso3c", "year"), suffix = c("_nea", "_weo")) |> 
  full_join(ngdp_wdi, by = c("iso3c", "year")) |> 
  rename(
    "ngdp_wdi" = ngdp,
    "ngdpmil_wdi" = ngdpmil,
    "ngdpbil_wdi" = ngdpbil
  )

# Choose the longest series per country
ngdp <- combine_longest_series(
  ngdp_comb,
  "ngdp",
  c("ngdp_nea", "ngdp_weo", "ngdp_wdi")
)

ngdpmil <- combine_longest_series(
  ngdp_comb,
  "ngdpmil",
  c("ngdpmil_nea", "ngdpmil_weo", "ngdpmil_wdi")
)

ngdpbil <- combine_longest_series(
  ngdp_comb,
  "ngdpbil",
  c("ngdpbil_nea", "ngdpbil_weo", "ngdpbil_wdi")
)

### 2.4.2 Real Growth ==================================================

rgdp_nea <- clean_data$nea_clean |> select(iso3c, year, rgdpgrowth)
rgdp_pfmh <- clean_data$pfmh_clean |> select(iso3c, year, rgdpgrowth)
rgdp_afrreo <- clean_data$afrreo_clean |> select(iso3c, year, rgdpgrowth)

# Combine datasets

rgdp_comb <- rgdp_nea |>
  full_join(rgdp_pfmh, by = c("iso3c", "year"), suffix = c("_nea", "_pfmh")) |>
  full_join(rgdp_afrreo, by = c("iso3c", "year")) |>
  rename("rgdpgrowth_afrreo" = "rgdpgrowth")

# Choose the longest series per country
rgdp_comb <- combine_longest_series(
  rgdp_comb,
  "rgdpgrowth",
  c("rgdpgrowth_nea", "rgdpgrowth_pfmh", "rgdpgrowth_afrreo")
)

## 2.6 Inflation Data =================================================

inflation_wdi <- clean_data$wdi2_clean |> select(year, iso3c, inflation)
inflation_weo <- clean_data$weo_clean |> select(year, iso3c, inflation)

# Combine BIS and WEO

infl_comb <- clean_data$bis_cpi_clean |> 
  full_join(inflation_weo, by = c("iso3c", "year"), suffix = c("_bis", "_weo")) |> 
  full_join(inflation_wdi, by = c("iso3c", "year")) |> 
  rename("inflation_wdi" = inflation)

# Choose the longest series per country

infl_comb <- combine_longest_series(infl_comb, "inflation", c("inflation_bis", "inflation_weo", "inflation_wdi"))

## 2.5 Debt Variables ===============================================

### 2.5.1 Private Debt ===============================================

# Credit to GDP ratio

# Combine everything

cgdppriv_gdd <- clean_data$gdd_clean |> select(iso3c, year, cgdppriv)
cgdppriv_afrreo <- clean_data$afrreo_clean |> select(iso3c, year, cgdppriv)
cgdppriv_wdi <- clean_data$wdi2_clean |> select(iso3c, year, cgdppriv)

cgdppriv_comb <- clean_data$cgdp_bis_clean |> 
  full_join(cgdppriv_gdd, by = c("iso3c", "year"), suffix = c("_bis", "_gdd")) |> 
  full_join(cgdppriv_afrreo, by = c("iso3c", "year")) |> 
  full_join(cgdppriv_wdi, by = c("iso3c", "year"), suffix = c("_afrreo", "_wdi"))


# Choose the longest series per country

cgdppriv_comb <- combine_longest_series(
  data = cgdppriv_comb,
  indicator = "cgdppriv",
  sources = c("cgdppriv_bis", "cgdppriv_gdd", "cgdppriv_afrreo", "cgdppriv_wdi")
)

# Corporate and household credit-to-GDP ratio
cgdpprivsplit <- clean_data$gdd_clean |> select(year, iso3c, cgdpcorp, cgdph)

# Create approximated credit column
credit_approx <- clean_data$gdd_clean |> 
  select(iso3c, year, cgdppriv, cgdpcorp, cgdph) |> 
  left_join(ngdpbil, by = c("iso3c", "year")) |> 
  mutate(
    tlpriv_approx = cgdppriv / 100 * ngdpbil,
    tlcorp_approx = cgdpcorp / 100 * ngdpbil,
    tlh_approx = cgdph / 100 * ngdpbil,
    year,
    iso3c,
    .keep = "none"
  ) 

# Combine actual credit dataset with approximated values
credit_comb <- clean_data$credit_bis_clean |> 
  full_join(credit_approx, by = c("iso3c", "year"))

# Fill in missing values in tloanspriv with the approximated values
credit_comb <- credit_comb |> 
  mutate(
    tlpriv = coalesce(tloanspriv, tlpriv_approx),
    tlcorp = coalesce(tloanscorp, tlcorp_approx),
    tlh = coalesce(tloansh, tlh_approx)
  )

# Calculate credit growth
credit_comb <- credit_comb |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(
    tlpriv_growth = (log(tlpriv) - lag(log(tlpriv))) * 100,
    tlcorp_growth = (log(tlcorp) - lag(log(tlcorp))) * 100,
    tlh_growth = (log(tlh) - lag(log(tlh))) * 100,
    blpriv_growth = (log(bloanspriv) - lag(log(bloanspriv))) * 100
  )

# Calculate real credit growth
credit_comb <- credit_comb |> 
  left_join(infl_comb |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(
    tlpriv_rgrowth = tlpriv_growth - inflation,
    tlcorp_rgrowth = tlcorp_growth - inflation,
    tlh_rgrowth = tlh_growth - inflation,
    blpriv_rgrowth = blpriv_growth - inflation
  ) |> 
  ungroup()



### 2.5.2 Public Debt =================================================

# Combine Datasets

govcgdp_gdd <- clean_data$gdd_clean |> select(iso3c, year, govcgdp)
govcgdp_pfmh <- clean_data$pfmh_clean |> select(iso3c, year, govcgdp_pfmh)
govcgdp_weo <- clean_data$weo_clean |> select(iso3c, year, govcgdp)

govcgdp_comb <- govcgdp_gdd |> 
  full_join(govcgdp_weo, by = c("iso3c", "year"), suffix = c("_gdd", "_weo")) |> 
  full_join(govcgdp_pfmh, by = c("iso3c", "year"))

# Choose the longest series per country

govcgdp_comb <- combine_longest_series(
  data = govcgdp_comb,
  indicator = "govcgdp",
  sources = c("govcgdp_weo", "govcgdp_pfmh", "govcgdp_gdd")
)

## 2.7 Current account balance (% of GDP) =============================

bca_wdi <- clean_data$wdi2_clean |> select(year, iso3c, bcagdp_wdi) 
bca_weo <- clean_data$weo_clean |> select(year, iso3c, bcagdp)

# Combine datasets
bca_comb <- bca_wdi |> 
  full_join(bca_weo, by = c("iso3c", "year")) |> 
  rename("bcagdp_weo" = bcagdp)

# Choose the longest series per country

bca_comb <- combine_longest_series(
  data = bca_comb,
  indicator = "bcagdp",
  sources = c("bcagdp_wdi", "bcagdp_weo")
)

## 2.8 Property Prices =======================================

# Combine datasets
pp_comb <- clean_data$bis_propprices_clean |> 
  full_join(clean_data$pp_oecd_clean |> select(-pp), by = c("iso3c", "year"), suffix = c("_bis", "_oecd"))

# Choose longest series
pp_comb <- combine_longest_series(
  data = pp_comb,
  indicator = "ppgrowth",
  sources = c("ppgrowth_bis", "ppgrowth_oecd")
)

## 2.9 Net foreign assets ===================================================

# Combine datasets

nfa_comb <- full_join(
  clean_data$nfa_mfs_clean,
  clean_data$wdi2_clean |> select(iso3c, year, nfa_wdi),
  by = c("iso3c", "year")
)


# Choose the longest series per country

nfa_comb <- combine_longest_series(
  data = nfa_comb,
  indicator = "nfa",
  sources = c("nfa_mfs", "nfa_wdi")
)

# Compute NFA-to-GDP ratio
nfa_comb <- nfa_comb |> 
  left_join(ngdpmil |> select(year, iso3c, ngdpmil), by = c("iso3c", "year")) |> 
  mutate(nfagdp = (nfa / ngdpmil) * 100)

## 2.10 Yield curve ===================================

# Merge all datasets

ltr_pfmh <- clean_data$pfmh_clean |> select(iso3c, year, rltir)
ir_jst <- clean_data$jst_clean |> select(iso3c, year, str_jst, ltr_jst)
mfs_str_clean <- clean_data$mfs_str_clean |> select(iso3c, year, str_mfs)

ir_comb <- clean_data$ir_oecd_clean |> 
  full_join(mfs_str_clean, by = c("iso3c", "year")) |> 
  full_join(clean_data$mfs_ltr_clean, by = c("iso3c", "year")) |> 
  full_join(clean_data$str_eurostat_clean, by = c("iso3c", "year")) |> 
  full_join(ltr_pfmh, by = c("iso3c", "year")) |> 
  full_join(ir_jst, by = c("iso3c", "year")) |> 
  # Inflation for approximating long term nominal interest rate
  left_join(infl_comb |> select(iso3c, year, inflation), by = c("iso3c", "year")) |> 
  mutate(
    str = coalesce(str_oecd, str_mfs, str_eurostat, str_jst),
    ltr_approx = rltir + inflation,
    ltr = coalesce(ltr_oecd, mfs_ltr, ltr_approx, ltr_jst),
    ycurve = ltr - str
  )

## 2.11 Broad Money ========================================================

bmgdp_gfd <- clean_data$gfd_clean |> select(year, iso3c, bmgdp)

# Combine datasets
bmoney_comb <- clean_data$bmoney_mfs_clean |> 
  full_join(clean_data$wdi1_clean, by = c("iso3c", "year"), suffix = c("_mfs", "_wdi")) |>
  full_join(bmgdp_gfd, by = c("iso3c", "year"), suffix = c("_wdi", "_gfd")) |> 
  # Nominal GDP for approximating broad money
  left_join(ngdpmil |> select(iso3c, year, ngdpmil), by = c("iso3c", "year")) |> 
  # full_join(bmoney_jst, by = c("iso3c", "year")) |> 
  rename(
    "bmoney_mfs" = broad_money,
    "bmoney_wdi" = bm
    # "bmoney_jst" = money
  )

# Choose the longest series per country
bmgdp <- combine_longest_series(
  data = bmoney_comb,
  indicator = "bmgdp",
  sources = c("bmgdp_wdi", "bmgdp_gfd")
)

# Approximate broad money
bmoney_comb <- bmoney_comb |> 
  full_join(bmgdp |> select(year, iso3c, bmgdp), by = c("year", "iso3c")) |>  
  mutate(bmoney_approx = bmgdp / 100 * ngdpmil)

bmoney <- combine_longest_series(
  data = bmoney_comb,
  indicator = "bmoney",
  sources = c("bmoney_mfs", "bmoney_wdi", "bmoney_approx")
)

# # Rescale the JST broad money values for CAN, FRA, DEU and ITA from billions to millions 
# bmoney <- bmoney |>
#   mutate(
#     bmoney = if_else(
#       source == "bmoney_jst" &
#         iso3c %in% c("CAN", "FRA", "DEU", "ITA"),
#       bmoney * 1000,
#       bmoney
#     )
#   )


# Calculate broad money growth rate

bmoney <- bmoney |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    bmgrowth = (log(bmoney) - lag(log(bmoney))) * 100
  )

# Calculate real broad money growth
bmoney <- bmoney |>
  left_join(infl_comb |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    bm_rgrowth = bmgrowth - inflation
  )

## 2.12 Loans-to-deposit ratio ===============================================

ltd_gfd <- clean_data$gfd_clean |> select(year, iso3c, ltd)
ltd_jst <- clean_data$jst_clean |> select(year, iso3c, ltd)

# Combine datasets
ltd_comb <- clean_data$ltd_mfs_clean |> 
  full_join(ltd_gfd, by = c("iso3c", "year"), suffix = c("_mfs", "_gfd")) |> 
  full_join(ltd_jst, by = c("iso3c", "year")) |> 
  rename("ltd_jst" = ltd)

# Choose the longest series per country
ltd_comb <- combine_longest_series(
  data = ltd_comb,
  indicator = "ltd",
  sources = c("ltd_mfs", "ltd_gfd", "ltd_jst")
)

## 2.13 Share prices =========================================================

# GFD
spr_gfd <- clean_data$gfd_clean |> select(year, iso3c, spr) |> rename("spr_gfd" = spr)
sp_mfs <- clean_data$sp_mfs_clean |> select(year, iso3c, sp)
  
# Combine datasets
sp_comb <- clean_data$sp_oecd_clean |> 
  full_join(sp_mfs, by = c("iso3c", "year"), suffix = c("_oecd", "_mfs")) |> 
  full_join(spr_gfd, by = c("iso3c", "year")) |> 
  # Convert values of 0 into NA
  mutate(
    sp_oecd = na_if(sp_oecd, 0),
    sp_mfs = na_if(sp_mfs, 0),
    spr_gfd = na_if(spr_gfd, 0)
  )

# Calculate returns
sp_comb <- sp_comb |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(
    spr_oecd = (log(sp_oecd) - lag(log(sp_oecd))) * 100,
    spr_mfs = (log(sp_mfs) - lag(log(sp_mfs))) * 100
  )

# Choose the longest series

sp_comb <- combine_longest_series(
  data = sp_comb,
  indicator = "spr",
  sources = c("spr_oecd", "spr_mfs", "spr_gfd")
)

# Compute real stock market return

sp_comb <- sp_comb |> 
  left_join(infl_comb |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(sprr = spr - inflation) |> 
  ungroup()

# 5 Save the constructed indicators ==========================================
indicators <- ls(pattern = "_comb$")
indicators <- indicators[!indicators %in% c("ngdp_comb", "bmoney_comb")]
indicators <- c(indicators, "ngdp", "ngdpmil", "ngdpbil", "bmoney", "bmgdp")

walk(indicators, ~ write_rds(get(.x), file.path("data/interim/indicators", paste0(.x, ".rds"))))

message("Step 1b: Constructed indicators saved to data/interim/indicators")

# 5 Add all indicators to the panel ==========================================

# Add to panel
panel <- left_join(panel, ngdp |> select(iso3c, year, ngdp), by = c("iso3c", "year"))
panel <- left_join(panel, ngdpmil |> select(iso3c, year, ngdpmil), by = c("iso3c", "year"))
panel <- left_join(panel, ngdpbil |> select(iso3c, year, ngdpbil), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, rgdp_comb |> select(iso3c, year, rgdpgrowth), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, infl_comb |> select(iso3c, year, inflation), by = c("iso3c", "year"))

# Add data to panel
panel <- left_join(panel, cgdppriv_comb |> select(iso3c, year, cgdppriv), by = c("iso3c", "year"))

# Corporate and household credit to GDP
panel <- left_join(panel, cgdpprivsplit, by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, credit_comb |>  select(year, iso3c, ends_with("rgrowth")), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, govcgdp_comb |> select(iso3c, year, govcgdp), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, bca_comb |> select(iso3c, year, bcagdp), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, pp_comb |> select(iso3c, year, ppgrowth), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, nfa_comb |> select(year, iso3c, nfagdp), by = c("iso3c", "year"))

# Add variables to panel
panel <- left_join(panel, ir_comb |> select(iso3c, year, ycurve), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, bmoney |> select(year, iso3c, bmtr, bm_rgrowth),
                   by = c("iso3c", "year"))
panel <- left_join(panel, bmgdp |> select(year, iso3c, bmgdp), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, ltd_comb |> select(iso3c, year, ltd), by = c("iso3c", "year"))

# Add to panel
panel <- left_join(panel, sp_comb |> select(iso3c, year, sprr), by = c("iso3c", "year"))



## 2.14 Check how many obs ===================================================

check <- panel |>
  group_by(country) |>
  summarise(
    across(
      - (year:precrisis4),
      ~ sum(!is.na(.x)),
      .names = "n_{.col}"
    )
  )

sort(colSums(check[,-1]), decreasing = T)

panel |> 
  select(cgdppriv, rgdpgrowth, inflation, govcgdp, bcagdp, bmgdp, ltd, nfagdp) |> 
  complete.cases() |> 
  sum()

predictors <- c(
  "cgdppriv", "rgdpgrowth", "inflation", "govcgdp", "bcagdp", "bmgdp", "ltd", "nfagdp"
)

panel_complete <- panel |> 
  filter(if_all(all_of(predictors), ~ !is.na(.)), crisis != 1) 

panel_complete |> 
  summarize(
    n_precrisis2 = sum(precrisis2),
    n_precrisis3 = sum(precrisis3),
    n_precrisis4 = sum(precrisis4)
  )
