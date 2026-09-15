# Step 1b: Construct indicators
# Purpose:  
# Inputs:   All files in data/interim/cleaned_datasets
# Outputs:  data/interim/indicators

# 0 Load Packages ==========================================================
library(tidyverse)
library(countrycode)

# 1 Define Functions =======================================================

check_compatibility <- function(data, sources, min_overlap = 8, min_cor = 0.8) {
  
  # Create all possible pairs from the list of sources
  pairs <- combn(sources, 2, simplify = FALSE)
  
  # For every country...
  map(unique(data$iso3c), function(country) {
    
    # For every source in a pair...
    map(pairs, function(pair) {
      
      # Create a temporary dataframe which only contains observations from the
      # sources of the considered pair
      xy <- data |>
        filter(iso3c == country) |>
        select(year, x = all_of(pair[1]), y = all_of(pair[2])) |>
        drop_na()
      
      # If the overlap of the two sources is not big enough,
      # then there is no compatibility
      if (nrow(xy) < min_overlap) {
        return(tibble(
          iso3c = country,
          source1 = pair[1],
          source2 = pair[2],
          n = nrow(xy),
          correlation = NA_real_,
          adjustment_factor = NA_real_,
          compatible = FALSE
        ))
      }
      
      # Compute correlation between the two sources
      correlation <- cor(xy$x, xy$y)
      
      # Compute the adjustment factor
      adjustment_factor <- median(xy$x / xy$y, na.rm = T)
      
      tibble(
        iso3c = country,
        source1 = pair[1],
        source2 = pair[2],
        n = nrow(xy),
        correlation = correlation,
        adjustment_factor = adjustment_factor,
        compatible = correlation >= min_cor
      )
    }) |> 
      list_rbind()
  }) |> 
    list_rbind()
}

choose_longest_source <- function(data, sources) {
  
  data |>
    filter(year >= 1970, year <= 2025) |>
    group_by(iso3c) |>
    # Calculate the number of non-missing observations across all sources
    summarise(
      across(all_of(sources), ~ sum(!is.na(.))),
      .groups = "drop"
    ) |>
    # Check for every row (that is for every country) which source has the
    # most observations
    rowwise() |>
    mutate(primary_source = sources[which.max(c_across(all_of(sources)))]) |>
    ungroup() |>
    select(iso3c, primary_source)
}

apply_adjustments <- function(data, adjustment_factors, sources, adjust = TRUE) {
  
  result <- data
  
  for (source in sources) {
    
    factors <- adjustment_factors |>
      filter(secondary_source == source) |>
      select(iso3c, adjustment_factor) |>
      mutate(compatible_flag = TRUE)
    
    result <- result |>
      left_join(factors, by = "iso3c") |>
      mutate(
        "{source}_adj" := if (adjust) {
          .data[[source]] * adjustment_factor
        } else {
          if_else(!is.na(compatible_flag), .data[[source]], NA_real_)
        }
      ) |>
      select(-adjustment_factor, -compatible_flag)
  }
  
  result
}

combine_sources <- function(data, sources, var_name,
                            adjust_levels = TRUE,
                            min_overlap = 8, min_cor = 0.8) {
  
  primary <- choose_longest_source(data, sources)
  compatibility <- check_compatibility(data, sources, min_overlap, min_cor)
  
  primary_compatibility <- primary |>
    left_join(compatibility, by = "iso3c") |>
    filter(source1 == primary_source | source2 == primary_source) |>
    mutate(secondary_source = if_else(source1 == primary_source, source2, source1)) |>
    mutate(
      adjustment_factor = case_when(
        source1 == primary_source ~ adjustment_factor,
        source2 == primary_source ~ 1 / adjustment_factor
      )
    )
  
  adjustment_factors <- primary_compatibility |>
    filter(compatible) |>
    select(iso3c, primary_source, secondary_source, adjustment_factor)
  
  adjusted <- apply_adjustments(data, adjustment_factors, sources, adjust = adjust_levels)
  
  adj_cols <- paste0(sources, "_adj")
  
  adjusted |>
    left_join(primary, by = "iso3c") |>
    mutate(
      value_primary = as.matrix(pick(all_of(sources)))[
        cbind(row_number(), match(primary_source, sources))
      ],
      value_secondary = do.call(coalesce, as.list(pick(all_of(adj_cols)))),
      "{var_name}" := coalesce(value_primary, value_secondary)
    ) |>
    select(-value_primary, -value_secondary)
}

# 2 Load Datasets ==========================================================

files <- list.files("data/interim/cleaned_datasets", pattern = "\\.rds$", full.names = TRUE)

clean_data <- files |>
  set_names(tools::file_path_sans_ext(basename(files))) |>
  map(read_rds)

# 4 Create indicators ==================================================

## 2.4 GDP =============================================================

### 2.4.1 Nominal ======================================================

ngdp_nea <- clean_data$nea_clean |> select(iso3c, year, ngdp)
ngdp_weo <- clean_data$weo_clean |> select(iso3c, year, ngdp)
ngdp_wdi <- clean_data$wdi2_clean |> select(iso3c, year, ngdp)

# Combine datasets
ngdp_comb <- ngdp_nea |> 
  full_join(ngdp_weo, by = c("iso3c", "year"), suffix = c("_nea", "_weo")) |> 
  full_join(ngdp_wdi, by = c("iso3c", "year")) |> 
  rename("ngdp_wdi" = ngdp)


ngdp_final <- combine_sources(
  data = ngdp_comb,
  sources = c("ngdp_nea", "ngdp_weo", "ngdp_wdi"),
  var_name = "ngdp",
  adjust_levels = TRUE
)

### 2.4.2 Real Growth ==================================================

rgdp_nea <- clean_data$nea_clean |> select(iso3c, year, rgdpgrowth)
rgdp_pfmh <- clean_data$pfmh_clean |> select(iso3c, year, rgdpgrowth)
rgdp_afrreo <- clean_data$afrreo_clean |> select(iso3c, year, rgdpgrowth)
rgdp_wdi <- clean_data$wdi2_clean |> select(iso3c, year, rgdpgrowth)

# Combine datasets

rgdp_comb <- rgdp_nea |>
  full_join(rgdp_pfmh, by = c("iso3c", "year"), suffix = c("_nea", "_pfmh")) |>
  full_join(rgdp_afrreo, by = c("iso3c", "year")) |>
  full_join(rgdp_wdi, by = c("iso3c", "year"), suffix = c("_afrreo", "_wdi"))

rgdp_final <- combine_sources(
  data = rgdp_comb,
  sources = c("rgdpgrowth_nea", "rgdpgrowth_pfmh", "rgdpgrowth_afrreo", "rgdpgrowth_wdi"),
  var_name = "rgdpgrowth",
  adjust_levels = FALSE
)

# Manual corrections
rgdp_final <- rgdp_final |> 
  # Replace implausible observation for Belarus in 2018 and 2022 by the more plausible values from WDI
  mutate(rgdpgrowth = if_else(iso3c %in% "BLR" & (year == 2018 | year == 2022), rgdpgrowth_wdi, rgdpgrowth))


## 2.6 Inflation Data =================================================


inflation_wdi <- clean_data$wdi2_clean |> select(year, iso3c, inflation)
inflation_weo <- clean_data$weo_clean |> select(year, iso3c, inflation)

# Combine BIS and WEO

infl_comb <- clean_data$bis_cpi_clean |> 
  full_join(inflation_weo, by = c("iso3c", "year"), suffix = c("_bis", "_weo")) |> 
  full_join(inflation_wdi, by = c("iso3c", "year")) |> 
  rename("inflation_wdi" = inflation)

infl_final <- combine_sources(
  data = infl_comb,
  sources = c("inflation_bis", "inflation_weo", "inflation_wdi"),
  var_name = "inflation",
  adjust_levels = FALSE
)

## 2.5 Debt Variables ===============================================

### 2.5.1 Private Debt ===============================================

# Total Credit to GDP ratio

cgdppriv_gdd <- clean_data$gdd_clean |> select(iso3c, year, cgdppriv)
cgdppriv_afrreo <- clean_data$afrreo_clean |> select(iso3c, year, cgdppriv)
cgdppriv_wdi <- clean_data$wdi2_clean |> select(iso3c, year, cgdppriv)
# Bank credit as complementary data
bcgdppriv <- clean_data$wdi2_clean |> select(iso3c, year, bcgdppriv) 
blpriv <- clean_data$credit_bis_clean |> select(iso3c, year, bloanspriv)

bl <- bcgdppriv |> 
  full_join(blpriv, by = c("iso3c", "year")) |> 
  full_join(ngdp_final |> select(iso3c, year, ngdp), by = c("iso3c", "year")) |> 
  mutate(
    # Construct Bank Loans to GDP ratio
    bcgdppriv_constr = bloanspriv / (ngdp / 1000000000) * 100
  ) |> 
  rename(
    "bcgdppriv_wdi" = bcgdppriv,
    "blpriv_bis" = bloanspriv
  )

cgdppriv_comb <- clean_data$cgdp_bis_clean |> 
  full_join(cgdppriv_gdd, by = c("iso3c", "year"), suffix = c("_bis", "_gdd")) |> 
  full_join(cgdppriv_afrreo, by = c("iso3c", "year")) |> 
  full_join(cgdppriv_wdi, by = c("iso3c", "year"), suffix = c("_afrreo", "_wdi")) |> 
  full_join(bcgdppriv, by = c("iso3c", "year")) |> 
  full_join(blpriv, by = c("iso3c", "year")) |> 
  # Add nominal GDP in order to construct Bank Credit-to-GDP ratio
  full_join(ngdp_final |> select(iso3c, year, ngdp), by = c("iso3c", "year")) |> 
  mutate(
    # Construct Bank Loans to GDP ratio
    bcgdppriv_constr = bloanspriv / (ngdp / 1000000000) * 100
  ) |> 
  rename(
    "bcgdppriv_wdi" = bcgdppriv,
    "blpriv_bis" = bloanspriv
  )

cgdppriv_final <- combine_sources(
  data = cgdppriv_comb,
  sources = c("cgdppriv_bis", "cgdppriv_gdd", "cgdppriv_afrreo", "cgdppriv_wdi", "bcgdppriv_constr", "bcgdppriv_wdi"),
  var_name = "cgdppriv",
  adjust_levels = TRUE
)

# Calculate log difference
cgdppriv_final <- cgdppriv_final |> 
  arrange(iso3c, year) |>
  group_by(iso3c) |> 
  mutate(cgdppriv_growth = (log(cgdppriv) - lag(log(cgdppriv))) * 100)

# Corporate and household credit-to-GDP ratio
cgdpprivsplit <- clean_data$gdd_clean |> select(year, iso3c, cgdpcorp, cgdph)

# Manually remove implausible values
cgdpprivsplit <- cgdpprivsplit |> 
  mutate(cgdph = if_else(iso3c %in% "IND" & year >= 1998 & year <= 2006, NA_real_, cgdph))

# Create approximated credit column
credit_approx <- cgdppriv_final |> 
  select(iso3c, year, cgdppriv) |> 
  left_join(clean_data$gdd_clean |> select(iso3c, year, cgdpcorp, cgdph), by = c("iso3c", "year")) |> 
  left_join(ngdp_final, by = c("iso3c", "year")) |> 
  mutate(
    tlpriv_approx = cgdppriv / 100 * (ngdp / 1000000000),
    tlcorp_approx = cgdpcorp / 100 * (ngdp / 1000000000),
    tlh_approx = cgdph / 100 * (ngdp / 1000000000),
    year,
    iso3c,
    .keep = "none"
  ) 


# Combine actual credit dataset with approximated values
credit_comb <- clean_data$credit_bis_clean |>
  full_join(credit_approx, by = c("iso3c", "year"))

tlpriv_final <- combine_sources(
  data = credit_comb,
  sources = c("tloanspriv", "tlpriv_approx", "bloanspriv"),
  var_name = "tlpriv",
  adjust_levels = TRUE
)

tlcorp_final <- combine_sources(
  data = credit_comb,
  sources = c("tloanscorp", "tlcorp_approx"),
  var_name = "tlcorp",
  adjust_levels = TRUE
)

tlh_final <- combine_sources(
  data = credit_comb,
  sources = c("tloansh", "tlh_approx"),
  var_name = "tlh",
  adjust_levels = TRUE
)

# Calculate credit growth
tlpriv_final <- tlpriv_final |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(tlpriv_growth = (log(tlpriv) - lag(log(tlpriv))) * 100)

tlcorp_final <- tlcorp_final |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(tlcorp_growth = (log(tlcorp) - lag(log(tlcorp))) * 100)

tlh_final <- tlh_final |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(tlh_growth = (log(tlh) - lag(log(tlh))) * 100)

# Calculate real credit growth

tlpriv_final <- tlpriv_final |> 
  left_join(infl_final |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(tlpriv_rgrowth = ((1 + tlpriv_growth/100) / (1 + inflation/100) - 1) * 100) |> 
  ungroup()

tlcorp_final <- tlcorp_final |> 
  left_join(infl_final |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(tlcorp_rgrowth = ((1 + tlcorp_growth/100) / (1 + inflation/100) - 1) * 100) |> 
  ungroup()

tlh_final <- tlh_final |> 
  left_join(infl_final |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(tlh_rgrowth = ((1 + tlh_growth/100) / (1 + inflation/100) - 1) * 100) |> 
  ungroup()

### 2.5.2 Public Debt =================================================

# Combine Datasets

govcgdp_gdd <- clean_data$gdd_clean |> select(iso3c, year, govcgdp, ggovdebt) |> rename("ggovdebt_gdd" = ggovdebt)
govcgdp_pfmh <- clean_data$pfmh_clean |> select(iso3c, year, govcgdp_pfmh)
govcgdp_weo <- clean_data$weo_clean |> select(iso3c, year, govcgdp)

govcgdp_comb <- govcgdp_gdd |> 
  full_join(govcgdp_weo, by = c("iso3c", "year"), suffix = c("_gdd", "_weo")) |> 
  full_join(govcgdp_pfmh, by = c("iso3c", "year"))

govcgdp_final <- combine_sources(
  data = govcgdp_comb,
  sources = c("govcgdp_weo", "govcgdp_pfmh", "govcgdp_gdd", "ggovdebt_gdd"),
  var_name = "govcgdp",
  adjust_levels = TRUE
)

# Compute log differences
govcgdp_final <- govcgdp_final |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    govcgdp_growth = (log(govcgdp) - lag(log(govcgdp))) * 100
  )
  

## 2.7 Current account balance (% of GDP) =============================

bca_wdi <- clean_data$wdi2_clean |> select(year, iso3c, bcagdp_wdi) 
bca_weo <- clean_data$weo_clean |> select(year, iso3c, bcagdp)

# Combine datasets
bca_comb <- bca_wdi |> 
  full_join(bca_weo, by = c("iso3c", "year")) |> 
  rename("bcagdp_weo" = bcagdp)

bca_final <- combine_sources(
  data = bca_comb,
  sources = c("bcagdp_weo", "bcagdp_wdi"),
  var_name = "bcagdp",
  adjust_levels = TRUE
)

## 2.8 Property Prices =======================================

# Combine datasets
pp_comb <- clean_data$bis_propprices_clean |> 
  full_join(clean_data$pp_oecd_clean |> select(-pp), by = c("iso3c", "year"), suffix = c("_bis", "_oecd"))

pp_final <- combine_sources(
  data = pp_comb,
  sources = c("ppgrowth_bis", "ppgrowth_oecd"),
  var_name = "ppgrowth",
  adjust_levels = FALSE
)


## 2.9 Net foreign assets ===================================================

# Combine datasets

nfa_comb <- full_join(
  clean_data$nfa_mfs_clean,
  clean_data$wdi2_clean |> select(iso3c, year, nfa_wdi),
  by = c("iso3c", "year")
)

nfa_final <- combine_sources(
  data = nfa_comb,
  sources = c("nfa_mfs", "nfa_wdi"),
  var_name = "nfa",
  adjust_levels = TRUE
)

# Compute NFA-to-GDP ratio
nfa_final <- nfa_final |> 
  left_join(ngdp_final |> select(year, iso3c, ngdp), by = c("iso3c", "year")) |> 
  mutate(nfagdp = (nfa / (ngdp / 1000000)) * 100)

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
  left_join(infl_final |> select(iso3c, year, inflation), by = c("iso3c", "year")) |> 
  mutate(ltr_approx = rltir + inflation)

str <- combine_sources(
  data = ir_comb,
  sources = c("str_oecd", "str_mfs", "str_eurostat", "str_jst"),
  var_name = "str",
  adjust_levels = TRUE
)

ltr <- combine_sources(
  data = ir_comb,
  sources = c("ltr_oecd", "mfs_ltr", "ltr_approx", "ltr_jst"),
  var_name = "ltr",
  adjust_levels = TRUE
)

ycurve_final <- str |> 
  full_join(ltr, by = c("iso3c", "year")) |> 
  mutate(ycurve = ltr - str) |> 
  select(iso3c, year, ycurve)

## 2.11 Broad Money ========================================================

bmgdp_gfd <- clean_data$gfd_clean |> select(year, iso3c, bmgdp)
bmgdp_wdi <- clean_data$wdi1_clean |> select(iso3c, year, bmgdp)
bm_mfs <- clean_data$bmoney_mfs_clean |> rename("bm" = broad_money)
bm_wdi <- clean_data$wdi1 |> select(iso3c, year, bm)

bm_jst <- clean_data$jst_clean |> 
  select(iso3c, year, money, gdp) |> 
  mutate(bmgdp_jst = money / gdp * 100) |> 
  select(-gdp)

# Broad Money to GDP

bmgdp_comb <- bmgdp_gfd |> 
  full_join(bmgdp_wdi, by = c("iso3c", "year"), suffix = c("_gfd", "_wdi")) |> 
  full_join(bm_jst, by = c("iso3c", "year")) |> 
  full_join(bm_mfs, by = c("iso3c", "year")) |> 
  # Nominal GDP for constructing broad money to GDP ratio
  left_join(ngdp_final |> select(iso3c, year, ngdp), by = c("iso3c", "year")) |> 
  # Construct broad money to GDP ratio
  mutate(
    bmgdp_mfs = bm / (ngdp / 1000000) * 100
  )

bmgdp_final <- combine_sources(
  data = bmgdp_comb,
  sources = c("bmgdp_gfd", "bmgdp_mfs", "bmgdp_wdi", "bmgdp_jst"),
  var_name = "bmgdp",
  adjust_levels = TRUE
)

# Manually correct breaks and errors
bmgdp_final <- bmgdp_final |> 
  # Replace the WDI bmgdp series by the GFD series for Sierra Leone
  mutate(bmgdp = if_else(iso3c %in% "SLE", bmgdp_gfd, bmgdp)) |> 
  # Set implausible bmgdp value for Luxembourg in 1993 to NA
  mutate(bmgdp = if_else(iso3c %in% "LUX" & year == 1993, NA_real_, bmgdp)) |> 
  # Multiply implausibly low bmgdp values for Spain from 1997 to 2000 by 10
  mutate(bmgdp = if_else(iso3c %in% "ESP" & year >= 1997 & year <= 2000, bmgdp * 10, bmgdp)) |> 
  # Replace implausibly low values for Zimbabwe in 2024 and 2025 by the unadjusted values from MFS
  mutate(bmgdp = if_else(iso3c %in% "ZWE" & year >= 2024 & year <= 2025, bmgdp_mfs, bmgdp))

# Compute log differences
bmgdp_final <- bmgdp_final |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    bmgdpgrowth = (log(bmgdp) - lag(log(bmgdp))) * 100
  )
  

# Broad Money

# Combine datasets
bm_comb <- bm_mfs |> 
  full_join(bm_wdi, by = c("iso3c", "year"), suffix = c("_mfs", "_wdi")) |> 
  full_join(bm_jst |> select(iso3c, year, money), by = c("iso3c", "year")) |> 
  full_join(bmgdp_final |> select(year, iso3c, bmgdp), by = c("year", "iso3c")) |> 
  # Add nominal GDP for approximating broad money
  left_join(ngdp_final |> select(iso3c, year, ngdp), by = c("iso3c", "year")) |> 
  # Approximate broad money
  mutate(bm_approx = bmgdp / 100 * (ngdp / 1000000)) |> 
  select(-ngdp) |> 
  rename("bm_jst" = money)

bm_final <- combine_sources(
  data = bm_comb,
  sources = c("bm_mfs", "bm_wdi", "bm_jst", "bm_approx"),
  var_name = "bm",
  adjust_levels = TRUE
)

# Manual corrections
bm_final <- bm_final |> 
  # Replace implausible bmoney values for Sierra Leone by the approximation
  mutate(bm = if_else(iso3c %in% "SLE", bm_approx, bm)) |> 
  # Set broad money value for Liberia in 2024 as missing since it creates an artificial jump
  mutate(bm = if_else(iso3c %in% "LBR" & year == 2024, NA_real_, bm))

# Calculate broad money log difference
bm_final <- bm_final |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    bmgrowth = (log(bm) - lag(log(bm))) * 100
  )

# Deflate broad money log difference
bm_final <- bm_final |>
  left_join(infl_final |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    bm_rgrowth = ((1 + bmgrowth/100) / (1 + inflation/100) - 1) * 100
  )

# Broad money to total reserves

bmtr <- clean_data$wdi1_clean |> select(iso3c, year, bmtr) |> rename("bmtr_wdi" = bmtr)
tr <- clean_data$wdi2_clean |> select(iso3c, year, trd)

# Compute Broad money to total reserves ratio for remaining countries (especially Euro Countries)
bmtr_comb <- bmtr |> 
  full_join(tr, by = c("iso3c", "year")) |> 
  full_join(bm_final |> select(year, iso3c, bm), by = c("iso3c", "year")) |> 
  # Exchange Rate Domestic Currency per USD
  left_join(clean_data$er_oecd_clean, by = c("iso3c", "year")) |> 
  mutate(
    # Convert total reserves to local currency
    tr = trd * er_lc_usd, 
    # Calculate Broad Money to total reserves ratio
    bmtr_constr = bm / tr
  ) 

# Choose longest series
bmtr_final <- combine_sources(
  data = bmtr_comb,
  sources = c("bmtr_wdi", "bmtr_constr"),
  var_name = "bmtr",
  adjust_levels = TRUE
)

# Manual Corrections
bmtr_final <- bmtr_final |> 
  mutate(
    # Divide implausibly high bmtr values for Sierra Leone by 1000 (then they become more plausible)
    bmtr = if_else(iso3c %in% "SLE" & year >= 2001 & year <= 2014, bmtr / 1000, bmtr) 
  )

## 2.12 Loans-to-deposit ratio ===============================================

ltd_gfd <- clean_data$gfd_clean |> select(year, iso3c, ltd)
ltd_jst <- clean_data$jst_clean |> select(year, iso3c, ltd)

# Combine datasets
ltd_comb <- clean_data$ltd_mfs_clean |> 
  full_join(ltd_gfd, by = c("iso3c", "year"), suffix = c("_mfs", "_gfd")) |> 
  full_join(ltd_jst, by = c("iso3c", "year")) |> 
  rename("ltd_jst" = ltd)

ltd_final <- combine_sources(
  data = ltd_comb,
  sources = c("ltd_mfs", "ltd_gfd", "ltd_jst"),
  var_name = "ltd",
  adjust_levels = TRUE
)

# Compute log differences
ltd_final <- ltd_final |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    ltd_growth = (log(ltd) - lag(log(ltd))) * 100
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
    spr_oecd = (sp_oecd - lag(sp_oecd)) / lag(sp_oecd) * 100,
    spr_mfs = (sp_mfs - lag(sp_mfs)) / lag(sp_mfs) * 100
  )

sp_final <- combine_sources(
  data = sp_comb,
  sources = c("spr_oecd", "spr_mfs", "spr_gfd"),
  var_name = "spr",
  adjust_levels = FALSE
)

# Compute real stock market return

sp_final <- sp_final |> 
  left_join(infl_final |> select(year, iso3c, inflation), by = c("iso3c", "year")) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(sprr = ((1 + spr/100) / (1 + inflation/100) - 1) * 100) |> 
  ungroup()

# 5 Save the constructed indicators ==========================================
indicators <- ls(pattern = "_final$")
indicators <- c(indicators, "cgdpprivsplit")

walk(indicators, ~ write_rds(get(.x), file.path("data/interim/indicators", paste0(.x, ".rds"))))

message("Step 1b: Constructed indicators saved to data/interim/indicators")

