# Step 1a: Data Transformation & Cleaning
# Purpose:  Convert all datasets into a long format,
#           add country codes
#           Standardise column names, derive log-wage and a
#           readable gender variable, and subset to the
#           variables needed for analysis.
# Inputs:   All files in data/raw
# Outputs:  data/interim/


# 0 Load Packages ===========================================================
library(readxl)
library(tidyverse)
library(countrycode)
library(janitor)

# 1 Create functions =======================================================

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

clean_data <- function(data, indicator_col = NULL, country_col = country) {
  
  data <- data |>
    clean_names() |>
    pivot_longer(
      cols = starts_with("x"),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      iso3c = countrycode({{ country_col }}, origin = "country.name", destination = "iso3c"),
      year = as.integer(str_extract(year, "\\d{4}"))
    ) |>
    filter(!is.na(iso3c))
  
  if (!is.null(indicator_col)) {
    data <- data |>
      select(iso3c, year, value, {{ indicator_col }}) |>
      pivot_wider(
        names_from = {{ indicator_col }},
        values_from = value
      ) |>
      clean_names()
  } else {
    data <- data |>
      select(iso3c, year, value)
  }
  
  data
}

# 2 Read and Clean raw datasets ================================================

## 2.1 Laeven & Valencia Banking Crisis Dataset ============================

crises <- read_xlsx(
  path = "data/raw/SYSTEMIC_BANKING_CRISES_DATABASE_2026.xlsx",
  sheet = 2
)

# Select important columns
crises <- crises |> 
  clean_names() |> 
  select(country, start, end)

crises <- crises |> 
  
  # Remove rows that contain NA in column "start"
  filter(!is.na(start)) |> 
  
  # Extract annotation symbols (e.g. "6/")
  mutate(
    annotation_country = str_extract(country, "\\d+/"),
    annotation_year = str_extract(end, "\\d+/"),
    
    # Remove annotations from country and end
    country = str_trim(str_remove(country, "\\s*\\d+/")),
    end = str_trim(str_remove(end, "\\s*\\d+/"))
  ) |> 
  
  # Replace "..." in end with 2025
  mutate(
    end = ifelse(end == "…", "2025", end)
  ) |> 
  
  # Convert end to numeric
  mutate(
    end = as.numeric(end)
  )

# Remove duplicates

crises_merged <- crises |> 
  arrange(country, start) |> 
  group_by(country) |> 
  
  # start a new crisis group whenever there is a gap
  mutate(
    new_group = if_else(
      start > lag(end, default = first(start) - 2) + 1,
      1L,
      0L
    ),
    crisis_group = cumsum(new_group)
  ) |> 
  
  group_by(country, crisis_group) |> 
  summarise(
    start = min(start),
    end   = max(end),
    .groups = "drop"
  )

# crisis years
crisis_years <- crises_merged |> 
  rowwise() |> 
  mutate(year = list(start:end)) |> 
  unnest(year) |> 
  select(country, year) |> 
  mutate(crisis = 1)

crisis_start <- crises_merged |> 
  mutate(
    country,
    year = start,
    crisis_start = 1,
    .keep = "none"
  )


## 2.2 BIS ===================================================================

# Loans
credit_bis <- read.csv("data/raw/WS_TC_csv_col.csv")

credit_bis_clean <- credit_bis |> 
  clean_names() |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "time",
    values_to = "value" 
  ) |> 
  filter(
    unit_type_2 == "Domestic currency (incl. conv. to current ccy made using a fix parity)", 
    valuation_method == "Market value",
    adjustment == "Adjusted for breaks",
    borrowing_sector %in% c("Private non-financial sector", "Households & NPISHs", "Non-financial corporations"),
    # Year end value
    str_detect(time, "q4")
  ) |> 
  mutate(
    year = as.integer(str_extract(time, "\\d{4}")),
    # Add country codes
    iso3c = countrycode(borrowers_country, origin = "country.name", destination = "iso3c")
  ) |> 
  select(c(borrowing_sector, lending_sector, year, value, iso3c)) |> 
  pivot_wider( 
    names_from = lending_sector,
    values_from = value
  ) |> 
  pivot_wider(
    names_from = borrowing_sector,
    values_from = c(`All sectors`, `Banks, domestic`)
  ) |> 
  mutate(
    year,
    iso3c,
    tloanscorp = `All sectors_Non-financial corporations`,
    tloanspriv = `All sectors_Private non-financial sector`,
    tloansh = `All sectors_Households & NPISHs`,
    bloanspriv = `Banks, domestic_Private non-financial sector`,
    .keep = "none"
  )

# Credit-to-GDP ratio
cgdp_bis <- read_csv("data/raw/WS_CREDIT_GAP_csv_col.csv")

cgdp_bis_clean <- cgdp_bis |> 
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Time",
    values_to = "cgdppriv"
  ) |> 
  # Year end value
  filter(
    str_detect(Time, "Q4"),
    `Credit gap data type` == "Credit-to-GDP ratios (actual data)"
  ) |> 
  mutate(
    year = as.integer(str_extract(Time, "\\d{4}")),
    iso3c = countrycode(`Borrowers' country`, origin = "country.name", destination = "iso3c")
  ) |> 
  select(c(iso3c, year, cgdppriv))

# Consumer Price Index / Inflation 
bis_cpi <- read_csv("data/raw/WS_LONG_CPI_csv_col.csv")

bis_cpi_clean <- bis_cpi |> 
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Year",
    values_to = "inflation"
  ) |> 
  filter(
    grepl("^\\d{4}$", Year),
    `Unit of measure` == "Year-on-year changes, in per cent",
    Frequency == "Annual"
  ) |> 
  select(c(`Reference area`, inflation, Year)) |> 
  mutate(
    year = as.integer(Year),
    iso3c = countrycode(`Reference area`, origin = "country.name", destination = "iso3c"),
    inflation,
    .keep = "none"
  )

# Property Prices
bis_propprices <- read_csv("data/raw/WS_SPP_csv_col.csv")

bis_propprices_clean <- bis_propprices |>
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Time",
    values_to = "Price"
  ) |> 
  mutate(year = as.integer(str_extract(Time, "\\d{4}"))) |>
  filter(
    `Unit of measure` == "Year-on-year changes, in per cent",
    Value == "Real",
    year >= "1970",
    !(`Reference area` %in% c("Advanced economies", "Euro area", "World", "Emerging market economies (aggregate)"))
  ) |> 
  # annualize quarterly data
  summarize(ppgrowth = mean(Price, na.rm = TRUE), .by = c(REF_AREA, year, Value)) |> 
  mutate(iso3c = countrycode(REF_AREA, origin = "iso2c", destination = "iso3c")) |> 
  select(- c(REF_AREA, Value))
  

## 2.3 Eurostat ===============================================================
str_eurostat <- read_xlsx("data/raw/str_eurostat.xlsx", sheet = 2, skip = 7, na = c("", "NA", ":"))

str_eurostat_clean <- str_eurostat |> 
  select(-starts_with("..")) |> 
  slice(2:20) |> 
  mutate(`1970` = as.numeric(`1970`)) |> 
  pivot_longer(
    cols = !TIME,
    names_to = "year",
    values_to = "str_eurostat"
  ) |> 
  mutate(
    iso3c = countrycode(TIME, origin = "country.name", destination = "iso3c"),
    year = as.integer(year),
    str_eurostat,
    .keep = "none"
  )

## 2.4 IMF ====================================================================

### 2.4.1 Global Debt Database (GDD) ========================================
gdd <- read_csv("data/raw/GDD.csv")

gdd_clean <- clean_data(gdd, indicator_col = "indicator") |> 
  rename(
    "ngdpdc" = gross_domestic_product_gdp_current_prices_domestic_currency,
    "cgdppriv" = debt_securities_and_loans_private_sector_percent_of_gdp,
    "cgdpcorp" = debt_securities_and_loans_non_financial_corporations_percent_of_gdp,
    "cgdph" = debt_securities_and_loans_households_percent_of_gdp,
    "pubdebt" = debt_instruments_public_sector_percent_of_gdp,
    "pubnfdebt" = debt_instruments_public_non_financial_sector_percent_of_gdp,
    "privdebt" = debt_instruments_private_sector_percent_of_gdp,
    "corpdebt" = debt_instruments_non_financial_corporations_percent_of_gdp,
    "hdebt" = debt_instruments_households_percent_of_gdp,
    "ggovdebt" = debt_instruments_general_government_percent_of_gdp,
    "govcgdp" = debt_instruments_central_government_percent_of_gdp
  ) |> 
  # Set Credit-to-GDP values of 0 as missing
  mutate(
    cgdppriv = na_if(cgdppriv, 0),
    cgdpcorp = na_if(cgdpcorp, 0),
    cgdph = na_if(cgdph, 0)
  )

### 2.4.2 Africa Regional Economic Outlook (AFRREO) ===========================
afrreo <- read_csv("data/raw/AFRREO.csv")

afrreo_clean <- afrreo |> 
  clean_names() |> 
  filter(
    indicator %in% c(
      "All Items, Consumer price index (CPI), Period average, Year-over-year (YOY) percent change",
      "Broad money, Percent change",
      "Broad money, Percent of GDP",
      "Credit to the private sector, Percent change",
      "Credit to the private sector, Percent of GDP",
      "Current account balance (credit less debit), Percent of GDP",
      "Gross domestic product (GDP), Constant prices, Percent change"
    ),
    !(country %in% c(
      "Sub-Saharan Africa excluding Nigeria and South Africa", 
      "SSA oil-exporting countries excluding South Africa",
      "SSA oil-exporting countries excluding Nigeria",
      "SSA oil-importing countries excluding South Africa",
      "SSA middle-income countries excluding South Africa and Nigeria"
    ))
  ) |> 
  clean_data(indicator_col = "indicator") |> 
  rename(
    "bmoneygr" = broad_money_percent_change,
    "bmoneygdp" = broad_money_percent_of_gdp,
    "inflation" = all_items_consumer_price_index_cpi_period_average_year_over_year_yoy_percent_change,
    "bcagdp" = current_account_balance_credit_less_debit_percent_of_gdp,
    "privloansgr" = credit_to_the_private_sector_percent_change,
    "cgdppriv" = credit_to_the_private_sector_percent_of_gdp,
    "rgdpgrowth" = gross_domestic_product_gdp_constant_prices_percent_change
  )

### 2.4.3 World Economic Outlook (WEO) ========================================
weo <- read_csv("data/raw/WEO.csv")

weo_clean <- clean_data(weo, indicator_col = "indicator") |> 
  rename(
    "ngdpbil" = gross_domestic_product_gdp_current_prices_domestic_currency,
    "inflation" = all_items_consumer_price_index_cpi_period_average_percent_change,
    "govcgdp" = gross_debt_general_government_percent_of_gdp,
    "bcagdp" = current_account_balance_credit_less_debit_percent_of_gdp
  ) |> 
  mutate(
    ngdpmil = ngdpbil * 1000,
    ngdp = ngdpbil * 1000000000
  )

### 2.4.4 Public Finances in Modern History (PFMH) ==============================
pfmh <- read_xlsx("data/raw/PFMH.xlsx")

pfmh_clean <- pfmh |> 
  rename(
    "iso3c" = isocode,
    "rgdpgrowth" = rgc,
    "govcgdp_pfmh" = d
  )

### 2.4.5 Monetary Financial Statistics (MFS) ===================================

# Net Foreign Assets
nfa_mfs <- read_csv("data/raw/NetAssets_IMF_MFS.csv")

nfa_mfs <- nfa_mfs |> 
  clean_names() |> 
  mutate(
    priority = case_when(
      type_of_transformation == "Domestic currency" ~ 1,
      type_of_transformation == "Euro" ~ 2,
      type_of_transformation == "US dollar" ~ 3,
      TRUE ~ 99
    )
  ) |>
  arrange(country, priority) |>
  group_by(country) |>
  slice(1) |>
  ungroup()

nfa_mfs_clean <- clean_data(nfa_mfs) |>
  rename(nfa_mfs = value)


# Interest rates
mfs_str <- read_csv("data/raw/IMF_MFS_STR.csv")
mfs_ltr <- read_csv("data/raw/IMF_MFS_LTR.csv")

mfs_str_clean <- clean_data(mfs_str, indicator_col = "indicator") |> 
  rename(
    "tbyield" = government_securities_treasury_bills_yields_rate_percent_per_annum,
    "mmrate" = money_market_rate_percent_per_annum) |> 
  mutate(
    str_mfs = coalesce(mmrate, tbyield)
  )

mfs_ltr_clean <- clean_data(mfs_ltr) |>
  rename(mfs_ltr = value)

# Broad Money
bmoney_mfs <- read_csv("data/raw/IMF_MFS_BroadMoney.csv")

bmoney_mfs_clean <- bmoney_mfs |> 
  clean_names() |> 
  filter(!(indicator %in% c("Broad Money, Seasonally adjusted (SA)", "Broad Money, M5"))) |> 
  clean_data(indicator_col = "indicator")


# Loans-to-Deposit Ratio
ltd_mfs <- read_csv("data/raw/loans_to_deposit_mfs.csv")

ltd_mfs_clean <- clean_data(ltd_mfs, indicator_col = "indicator") |> 
  rename(
    "loans" = assets_claims_on_private_sector,
    "transdep" = liabilities_transferable_deposits_included_in_broad_money,
    "othdep" = liabilities_other_deposits_included_in_broad_money
  ) |> 
  mutate(ltd = (loans / (transdep + othdep)) * 100)


# Share prices
sp_mfs <- read_csv("data/raw/sp_mfs.csv")

sp_mfs_clean <- clean_data(sp_mfs, indicator_col = "type_of_transformation") |> 
  rename(
    "sppa" = period_average_index,
    "speop" = end_of_period_eo_p_index
  ) |> 
  # Choose the longest mfs share price series (period avrg. vs end-of-period)
  combine_longest_series("sp", c("sppa", "speop"))

### 2.4.6 National Economic Accounts (NEA) ================================
nea <- read_csv("data/raw/nea.csv")

nea_clean <- clean_data(nea, indicator_col = "price_type") |> 
  rename(
    "ngdp" = current_prices,
    "rgdp" = constant_prices
  ) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(
    ngdpmil = ngdp / 1000000,
    ngdpbil = ngdp / 1000000000,
    rgdpgrowth = (log(rgdp) - lag(log(rgdp))) * 100
  )

## 2.5 OECD =========================================================

# Share Price Indices
sp_oecd <- read_xlsx("data/raw/sp_oecd.xlsx", skip = 5)

sp_oecd_clean <- sp_oecd |> 
  clean_names() |> 
  slice_head(n = -2) |> 
  slice_tail(n = -1) |> 
  select(-c(time_period_2, last_col())) |> 
  rename("country" = time_period_1) |> 
  clean_data() |> 
  rename("sp" = value)

# Interest Rates
ir_oecd <- read_xlsx("data/raw/ir_oecd.xlsx", skip = 4)

ir_oecd_clean <- ir_oecd |> 
  clean_names() |> 
  slice_head(n = -2) |> 
  slice_tail(n = -1) |> 
  select(-c(time_period_3, last_col())) |> 
  rename(
    "indicator" = time_period_1,
    "country" = time_period_2
  ) |> 
  clean_data(indicator_col = "indicator") |> 
  rename(
    "ltr_oecd" = long_term_interest_rates,
    "str_oecd" = short_term_interest_rates
  )

# Property Prices
pp_oecd <- read_xlsx("data/raw/pp_oecd.xlsx", skip = 5)

pp_oecd_clean <- pp_oecd |> 
  clean_names() |> 
  slice_head(n = -2) |> 
  slice_tail(n = -1) |> 
  select(-c(time_period_2, last_col())) |> 
  rename("country" = time_period_1) |> 
  clean_data() |> 
  rename("pp" = value)

# Calculate growth
pp_oecd_clean <- pp_oecd_clean |> 
  group_by(iso3c) |> 
  mutate(ppgrowth = (log(pp) - lag(log(pp))) * 100)

## 2.6 World Bank ==================================================

### 2.6.1 World Development Indicators (WDI) =======================
wdi1 <- read_csv("data/raw/wdi1.csv", na = c("", "NA", ".."))
wdi2 <- read_csv("data/raw/wdi2.csv", na = c("", "NA", ".."))

wdi1_clean <- wdi1 |> 
  slice_head(n = -5) |> 
  clean_data(indicator_col = "series_name", country_col = country_name) |> 
  rename(
    "bm" = broad_money_current_lcu,
    "bmtr" = broad_money_to_total_reserves_ratio,
    "bmgrowth" = broad_money_growth_annual_percent,
    "bmgdp" = broad_money_percent_of_gdp
  ) |> 
  mutate(
    bm = bm / 1000000
  )

wdi2_clean <- wdi2 |> 
  slice_head(n = -5) |> 
  clean_data(indicator_col = "series_name", country_col = country_name) |> 
  rename(
    "nfa" = net_foreign_assets_current_lcu,
    "bcagdp_wdi" = current_account_balance_percent_of_gdp,
    "ngdp" = gdp_current_lcu,
    "cgdppriv" = domestic_credit_to_private_sector_percent_of_gdp,
    "bcgdpriv" = domestic_credit_to_private_sector_by_banks_percent_of_gdp,
    "trd" = total_reserves_includes_gold_current_us,
    "inflation" = inflation_consumer_prices_annual_percent
  ) |> 
  mutate(
    ngdpmil = ngdp / 1000000,
    ngdpbil = ngdp / 1000000000,
    nfa_wdi = nfa / 1000000
  ) |> 
  # Manually remove data errors (discovered after descriptive analysis)
  mutate(nfa_wdi = if_else(iso3c %in% c("NLD", "ITA") & year == 2024, NA_real_, nfa_wdi))

### 2.6.2 Global Financial Development (GFD) =============================
gfd <- read_csv("data/raw/gfd.csv", na = c("", "NA", ".."), locale = locale(encoding = "Latin1"))

gfd_clean <- gfd |> 
  slice_head(n = -5) |> 
  clean_data(indicator_col = "series_name", country_col = country_name) |> 
  rename(
    "spr" = stock_market_return_percent_year_on_year,
    "ltd" = bank_credit_to_bank_deposits_percent,
    "bmgdp" = liquid_liabilities_to_gdp_percent
  )

## 2.7 Jordà-Schularick-Taylor Macrohistory Database (JST) ====================
jst <- read_xlsx("data/raw/JSTdatasetR6.xlsx")

jst_clean <- jst |> 
  rename(
    "iso3c" = iso,
    "str_jst" = stir,
    "ltr_jst" = ltrate
  )

# 3 Save cleaned datasets =====================================================

clean_names <- ls(pattern = "_clean$")

walk(clean_names, ~ write_rds(get(.x), file.path("data/interim/cleaned_datasets", paste0(.x, ".rds"))))

write_rds(x = crises_merged, file = "data/interim/cleaned_datasets/crises_merged.rds")
write_rds(x = crisis_start, file = "data/interim/cleaned_datasets/crisis_start.rds")
write_rds(x = crisis_years, file = "data/interim/cleaned_datasets/crisis_years.rds")

message("Step 1a: Cleaned data saved to data/interim/cleaned_datasets")
